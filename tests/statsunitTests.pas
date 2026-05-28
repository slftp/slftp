unit statsunitTests;

interface

uses
  {$IFDEF FPC}
    TestFramework,
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility,
  {$ENDIF}
  SysUtils, statsunit, mormot.orm.core;

type
  TTestStatsUnit = class(TTestCase)
  private
    fStatsThread: TWriteStatsToDBThread;
    procedure WaitForQueue;
    function CountStatsRecords: integer;
    function CountSiteRecords: integer;
    function CountFileInfoRecords: integer;
    function CountSectionRecords: integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInitAndActive;
    procedure TestProcessRace;
    procedure TestProcessRaceDuplicate;
    procedure TestProcessRaceSmallFileIgnored;
    procedure TestRemoveStats;
    procedure TestRemoveStatsOrphanedFileInfo;
    procedure TestBackup;
  end;

implementation

uses
  configunit, dbhandler, mormot.core.base, slcriticalsection2;

const
  CTEST_DB_NAME = 'test_stats.db';

{ TTestStatsUnit }

function TTestStatsUnit.CountStatsRecords: integer;
begin
  Result := glStatsDb.TableRowCount(TSQLStatsRecord);
end;

function TTestStatsUnit.CountSiteRecords: integer;
begin
  Result := glStatsDb.TableRowCount(TSQLSitesRecord);
end;

function TTestStatsUnit.CountFileInfoRecords: integer;
begin
  Result := glStatsDb.TableRowCount(TSQLFileInfoRecord);
end;

function TTestStatsUnit.CountSectionRecords: integer;
begin
  Result := glStatsDb.TableRowCount(TSQLSectionRecord);
end;

procedure TTestStatsUnit.SetUp;
begin
  inherited SetUp;
  if not IsStatsDatabaseActive then
  begin
    glDeleteAfterDays := 0;
    glStatsModel := TSQLModel.Create([TSQLStatsRecord, TSQLSitesRecord, TSQLSectionRecord, TSQLFileInfoRecord]);
    glStatsDb := CreateORMSQLite3DB(glStatsModel, CTEST_DB_NAME, '');

    glStatRaceQueue := TQueue<TStatRaceRecord>.Create;
    glStatRaceLock := TSlCriticalSection2.Create('glStatRaceLock');
    fStatsThread := TWriteStatsToDBThread.Create;
  end;
end;

procedure TTestStatsUnit.TearDown;
begin
  if Assigned(fStatsThread) then
  begin
    glWriteStatsThreadShouldStop := True;
    while glTWriteStatsThreadRunning do
      Sleep(100);
    fStatsThread.WaitFor;
    fStatsThread.Free;
    fStatsThread := nil;
  end;

  if Assigned(glStatsDb) then
    FreeAndNil(glStatsDb);
  if Assigned(glStatsModel) then
    FreeAndNil(glStatsModel);
  if Assigned(glStatRaceLock) then
    FreeAndNil(glStatRaceLock);
  if Assigned(glStatRaceQueue) then
    FreeAndNil(glStatRaceQueue);

  glWriteStatsThreadShouldStop := False;

  if FileExists(CTEST_DB_NAME) then
    DeleteFile(CTEST_DB_NAME);

  inherited TearDown;
end;

procedure TTestStatsUnit.WaitForQueue;
var
  i: integer;
begin
  for i := 1 to 30 do
  begin
    if glStatRaceQueue.Count = 0 then
      Break;
    Sleep(100);
  end;
end;

procedure TTestStatsUnit.TestInitAndActive;
begin
  CheckTrue(IsStatsDatabaseActive, 'Stats database should be active after init');
  CheckNotNull(glStatsDb, 'glStatsDb should not be nil');
  CheckNotNull(glStatsModel, 'glStatsModel should not be nil');
end;

procedure TTestStatsUnit.TestProcessRace;
var
  fCountBefore, fCountAfter: integer;
  fSitesBefore, fSectionsBefore, fFileInfoBefore: integer;
begin
  fCountBefore := CountStatsRecords;
  fSitesBefore := CountSiteRecords;
  fSectionsBefore := CountSectionRecords;
  fFileInfoBefore := CountFileInfoRecords;

  statsProcessRace('SRC_SITE', 'DST_SITE', 'SECTION', 'RELEASE.NAME', 'file.rar', 500000);
  WaitForQueue;

  fCountAfter := CountStatsRecords;
  CheckTrue(fCountAfter > fCountBefore, 'Stats record count should have increased');
  CheckTrue(CountSiteRecords > fSitesBefore, 'Site record count should have increased');
  CheckTrue(CountSectionRecords > fSectionsBefore, 'Section record count should have increased');
  CheckTrue(CountFileInfoRecords > fFileInfoBefore, 'FileInfo record count should have increased');
end;

procedure TTestStatsUnit.TestProcessRaceDuplicate;
var
  fCountBefore, fCountAfter: integer;
begin
  statsProcessRace('DUP_SRC', 'DUP_DST', 'DUP_SEC', 'DUP.RLS', 'file.rar', 500000);
  WaitForQueue;
  fCountBefore := CountStatsRecords;

  // Same race again should be ignored
  statsProcessRace('DUP_SRC', 'DUP_DST', 'DUP_SEC', 'DUP.RLS', 'file.rar', 500000);
  WaitForQueue;
  fCountAfter := CountStatsRecords;

  CheckEquals(fCountBefore, fCountAfter, 'Duplicate race should not create another stats record');
end;

procedure TTestStatsUnit.TestProcessRaceSmallFileIgnored;
var
  fCountBefore, fCountAfter: integer;
begin
  fCountBefore := CountStatsRecords;
  statsProcessRace('SRC_SITE', 'DST_SITE', 'SECTION', 'RELEASE.NAME', 'small.txt', 1);
  WaitForQueue;
  fCountAfter := CountStatsRecords;
  CheckEquals(fCountBefore, fCountAfter, 'Small file should be ignored due to min_filesize');
end;

procedure TTestStatsUnit.TestRemoveStats;
var
  fSiteCountBefore, fSiteCountAfter: integer;
begin
  statsProcessRace('DELETE_ME_SRC', 'DELETE_ME_DST', 'SEC', 'RLS', 'file.rar', 600000);
  WaitForQueue;

  fSiteCountBefore := CountSiteRecords;
  CheckTrue(RemoveStats('DELETE_ME_SRC'), 'RemoveStats should return true');
  fSiteCountAfter := CountSiteRecords;
  CheckTrue(fSiteCountAfter < fSiteCountBefore, 'Site record count should have decreased');
end;

procedure TTestStatsUnit.TestRemoveStatsOrphanedFileInfo;
var
  fFileInfoCountBefore, fFileInfoCountAfter: integer;
  fStatsCountBefore, fStatsCountAfter: integer;
begin
  // Single race with unique file info
  statsProcessRace('ORPH_SRC', 'ORPH_DST', 'SEC', 'ORPH.RLS', 'orphan.rar', 600000);
  WaitForQueue;

  fFileInfoCountBefore := CountFileInfoRecords;
  fStatsCountBefore := CountStatsRecords;

  // Remove source site -> stats entry still referenced by dst
  CheckTrue(RemoveStats('ORPH_SRC'), 'RemoveStats should return true for source site');

  // Remove destination site -> stats entry now fully orphaned and should be cleaned up
  CheckTrue(RemoveStats('ORPH_DST'), 'RemoveStats should return true for destination site');

  fFileInfoCountAfter := CountFileInfoRecords;
  fStatsCountAfter := CountStatsRecords;

  CheckTrue(fFileInfoCountAfter < fFileInfoCountBefore, 'Orphaned FileInfo should be deleted');
  CheckTrue(fStatsCountAfter < fStatsCountBefore, 'Orphaned Stats record should be deleted');
end;

procedure TTestStatsUnit.TestBackup;
var
  fBackupPath: String;
begin
  fBackupPath := 'test_stats_backup.db';
  if FileExists(fBackupPath) then
    DeleteFile(fBackupPath);

  doStatsBackup('', fBackupPath);
  // Backup is async, give it a moment
  Sleep(500);
  CheckTrue(FileExists(fBackupPath), 'Backup file should exist after doStatsBackup');

  if FileExists(fBackupPath) then
    DeleteFile(fBackupPath);
end;

initialization
  {$IFDEF FPC}
    RegisterTest('statsunit', TTestStatsUnit.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestStatsUnit);
  {$ENDIF}

end.
