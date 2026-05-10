unit statsunitTests;

interface

uses
  {$IFDEF FPC}
    TestFramework,
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility,
  {$ENDIF}
  SysUtils, statsunit, mormot.rest.sqlite3, mormot.orm.core, Generics.Collections;

type
  TTestStatsUnit = class(TTestCase)
  private
    fStatsThread: TWriteStatsToDBThread;
    procedure WaitForQueue;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInitAndActive;
    procedure TestProcessRace;
    procedure TestRemoveStats;
  end;

implementation

uses
  configunit, sitesunit, mormot.core.base, slcriticalsection2;

const
  CTEST_DB_NAME = 'test_stats.db';

{ TTestStatsUnit }

procedure TTestStatsUnit.SetUp;
begin
  inherited SetUp;
  if not IsStatsDatabaseActive then
  begin
    // Force initialization with a test database name
    glDeleteAfterDays := 0;
    glStatsModel := TSQLModel.Create([TSQLStatsRecord, TSQLSitesRecord, TSQLSectionRecord, TSQLFileInfoRecord]);
    glStatsDb := TSQLRestClientDB.Create(glStatsModel, nil, CTEST_DB_NAME, TSQLRestServerDB);
    TSQLRestServerDB(glStatsDb.Server).CreateMissingTables;

    glStatRaceQueue := TQueue<TStatRaceRecord>.Create;
    glStatRaceLock := TSlCriticalSection2.Create('glStatRaceLock');
    fStatsThread := TWriteStatsToDBThread.Create;
  end;
end;

procedure TTestStatsUnit.TearDown;
begin
  // Signal the background thread to stop and wait for it
  if Assigned(fStatsThread) then
  begin
    glWriteStatsThreadShouldStop := True;
    // Wait for thread to finish (max ~3 seconds)
    while glTWriteStatsThreadRunning do
      Sleep(100);
    fStatsThread.WaitFor;
    fStatsThread.Free;
    fStatsThread := nil;
  end;

  // Clean up ORM and queue resources
  if Assigned(glStatsDb) then
    FreeAndNil(glStatsDb);
  if Assigned(glStatsModel) then
    FreeAndNil(glStatsModel);
  if Assigned(glStatRaceLock) then
    FreeAndNil(glStatRaceLock);
  if Assigned(glStatRaceQueue) then
    FreeAndNil(glStatRaceQueue);

  // Reset global flags so next test run can reinitialize
  glWriteStatsThreadShouldStop := False;

  // Remove test database file
  if FileExists(CTEST_DB_NAME) then
    DeleteFile(CTEST_DB_NAME);

  inherited TearDown;
end;

procedure TTestStatsUnit.WaitForQueue;
var
  i: integer;
begin
  // Wait up to 2 seconds for the background thread to drain the queue
  for i := 1 to 20 do
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
begin
  fCountBefore := glStatsDb.TableRowCount(TSQLStatsRecord);

  statsProcessRace('SRC_SITE', 'DST_SITE', 'SECTION', 'RELEASE.NAME', 'file.rar', 500000);

  WaitForQueue;

  fCountAfter := glStatsDb.TableRowCount(TSQLStatsRecord);
  CheckTrue(fCountAfter > fCountBefore, 'Stats record count should have increased');
end;

procedure TTestStatsUnit.TestRemoveStats;
var
  fSiteCountBefore, fSiteCountAfter: integer;
begin
  // Add a site entry first by processing a race
  statsProcessRace('DELETE_ME_SRC', 'DELETE_ME_DST', 'SEC', 'RLS', 'file.rar', 600000);
  WaitForQueue;

  fSiteCountBefore := glStatsDb.TableRowCount(TSQLSitesRecord);

  CheckTrue(RemoveStats('DELETE_ME_SRC'), 'RemoveStats should return true');

  fSiteCountAfter := glStatsDb.TableRowCount(TSQLSitesRecord);
  CheckTrue(fSiteCountAfter < fSiteCountBefore, 'Site record count should have decreased');
end;

initialization
  {$IFDEF FPC}
    RegisterTest('statsunit', TTestStatsUnit.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestStatsUnit);
  {$ENDIF}
end.
