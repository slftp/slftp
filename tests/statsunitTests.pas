unit statsunitTests;

interface

uses
  {$IFDEF FPC}
    TestFramework,
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility,
  {$ENDIF}
  SysUtils, statsunit, mormot.orm.core, mormot.core.base;

type
  TTestStatsUnit = class(TTestCase)
  private
    procedure WaitForStatsCount(const aMinCount: integer);
    procedure DeleteTestDb;
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
    procedure TestStatsRecordReferencesRealIDs;
    procedure TestRemoveStats;
    procedure TestRemoveStatsOrphanedFileInfo;
    procedure TestBackup;
  end;

implementation

uses
  globals;

const
  CTEST_DB_NAME = 'test_stats.db';

{ TTestStatsUnit }

function TTestStatsUnit.CountStatsRecords: integer;
begin
  Result := GlStatsDb.TableRowCount(TSQLStatsRecord);
end;

function TTestStatsUnit.CountSiteRecords: integer;
begin
  Result := GlStatsDb.TableRowCount(TSQLSitesRecord);
end;

function TTestStatsUnit.CountFileInfoRecords: integer;
begin
  Result := GlStatsDb.TableRowCount(TSQLFileInfoRecord);
end;

function TTestStatsUnit.CountSectionRecords: integer;
begin
  Result := GlStatsDb.TableRowCount(TSQLSectionRecord);
end;

procedure TTestStatsUnit.DeleteTestDb;
var
  fDbPath: String;
begin
  // CreateORMSQLite3DB (dbhandler) always puts the db file into the
  // databases folder next to the binary, not into the current directory
  fDbPath := ExtractFilePath(ParamStr(0)) + DATABASEFOLDERNAME + PathDelim + CTEST_DB_NAME;
  DeleteFile(fDbPath);
  // WAL mode files
  DeleteFile(fDbPath + '-wal');
  DeleteFile(fDbPath + '-shm');
end;

procedure TTestStatsUnit.SetUp;
begin
  inherited SetUp;
  DeleteTestDb; // remove leftovers of a previously killed run
  // dbhandler creates the folder relative to the CWD, but opens the db next
  // to the binary - make sure the real target folder exists
  ForceDirectories(ExtractFilePath(ParamStr(0)) + DATABASEFOLDERNAME);
  GlDeleteAfterDays := 0;
  statsInit(CTEST_DB_NAME);
end;

procedure TTestStatsUnit.TearDown;
begin
  // statsUninit stops the writer thread (it frees itself via FreeOnTerminate)
  statsUninit;
  DeleteTestDb;

  inherited TearDown;
end;

procedure TTestStatsUnit.WaitForStatsCount(const aMinCount: integer);
var
  i: integer;
begin
  // poll the DB instead of the queue: the writer thread swaps the queue,
  // so an empty queue does not mean the records are written yet
  for i := 1 to 60 do
  begin
    if CountStatsRecords >= aMinCount then
      Break;
    Sleep(50);
  end;
end;

procedure TTestStatsUnit.TestInitAndActive;
begin
  CheckTrue(IsStatsDatabaseActive, 'Stats database should be active after init');
  CheckNotNull(GlStatsDb, 'GlStatsDb should not be nil');
  CheckNotNull(GlStatsModel, 'GlStatsModel should not be nil');
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
  WaitForStatsCount(fCountBefore + 1);

  fCountAfter := CountStatsRecords;
  CheckTrue(fCountAfter > fCountBefore, 'Stats record count should have increased');
  CheckTrue(CountSiteRecords > fSitesBefore, 'Site record count should have increased');
  CheckTrue(CountSectionRecords > fSectionsBefore, 'Section record count should have increased');
  CheckTrue(CountFileInfoRecords > fFileInfoBefore, 'FileInfo record count should have increased');
end;

procedure TTestStatsUnit.TestProcessRaceDuplicate;
begin
  statsProcessRace('DUP_SRC', 'DUP_DST', 'DUP_SEC', 'DUP.RLS', 'file.rar', 500000);
  WaitForStatsCount(1);

  // Same race again should be ignored
  statsProcessRace('DUP_SRC', 'DUP_DST', 'DUP_SEC', 'DUP.RLS', 'file.rar', 500000);
  // the duplicate will not create a new record, so we cannot poll for it;
  // wait long enough for the writer thread (1s interval) to have processed it
  Sleep(1500);

  CheckEquals(1, CountStatsRecords, 'Duplicate race should not create another stats record');
end;

procedure TTestStatsUnit.TestProcessRaceSmallFileIgnored;
begin
  statsProcessRace('SRC_SITE', 'DST_SITE', 'SECTION', 'RELEASE.NAME', 'small.txt', 1);
  // small files are filtered out synchronously in statsProcessRace (min_filesize)
  CheckEquals(0, CountStatsRecords, 'Small file should be ignored due to min_filesize');
end;

procedure TTestStatsUnit.TestStatsRecordReferencesRealIDs;
var
  fSiteRec: TSQLSitesRecord;
  fSiteID: TID;
  fStatsRec: TSQLStatsRecord;
begin
  statsProcessRace('FK_SRC', 'FK_DST', 'FK_SEC', 'FK.RLS', 'fk.rar', 500000);
  WaitForStatsCount(1);

  fSiteRec := TSQLSitesRecord.CreateAndFillPrepare(GlStatsDb.Client, 'Name = ?', ['FK_SRC'], 'ID');
  try
    CheckTrue(fSiteRec.FillOne, 'Source site record should exist');
    fSiteID := fSiteRec.ID;
  finally
    fSiteRec.Free;
  end;

  // oftID fields must store pointer(ID); storing the object pointer instead
  // would write a heap address as foreign key and this query would not match
  fStatsRec := TSQLStatsRecord.CreateAndFillPrepare(GlStatsDb.Client, 'SrcSiteRec = ?', [fSiteID]);
  try
    CheckTrue(fStatsRec.FillOne, 'Stats record should reference the real site ID, not a pointer');
  finally
    fStatsRec.Free;
  end;
end;

procedure TTestStatsUnit.TestRemoveStats;
var
  fSiteCountBefore, fSiteCountAfter: integer;
begin
  statsProcessRace('DELETE_ME_SRC', 'DELETE_ME_DST', 'SEC', 'RLS', 'file.rar', 600000);
  WaitForStatsCount(1);

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
  WaitForStatsCount(1);

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
  fBackupDir, fBackupFile: String;
begin
  fBackupDir := ExtractFilePath(ParamStr(0));
  fBackupFile := 'test_stats_backup.db';
  DeleteFile(fBackupDir + fBackupFile);

  try
    doStatsBackup(fBackupDir, fBackupFile);
    // doStatsBackup waits via BackupBackgroundWaitUntilFinished
    CheckTrue(FileExists(fBackupDir + fBackupFile), 'Backup file should exist after doStatsBackup');
  finally
    DeleteFile(fBackupDir + fBackupFile);
  end;
end;

initialization
  {$IFDEF FPC}
    RegisterTest('statsunit', TTestStatsUnit.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestStatsUnit);
  {$ENDIF}

end.
