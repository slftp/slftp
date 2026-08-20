unit dbaddpreTests;

interface

uses
  {$IFDEF FPC}
    TestFramework,
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility,
  {$ENDIF}
  dbaddpre;

type
  { @abstract(Tests for the mORMot2 ORM persistence of dbaddpre (insert, dupe handling, pretime lookup and legacy table migration)) }
  TTestDbAddPre = class(TTestCase)
  private
    fOldMode: integer;
    fOldDbFile: String;
    fOldAutoUpdate: Boolean;
    { deletes the test database incl. WAL/SHM files }
    procedure DeleteTestDb;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInitAndAlive;
    procedure TestInsertAndReadPretime;
    procedure TestInsertDuplicateIsIgnored;
    procedure TestGetCount;
    procedure TestCleanupPathRunsInMemoryMode;
    procedure TestLegacyTableMigration;
  end;

implementation

uses
  SysUtils, configunit, globals, mormot.core.base, mormot.core.unicode,
  mormot.db.raw.sqlite3;

const
  CTEST_DB_NAME = 'test_addpre.db'; //< database file name used by the ORM persistence tests

procedure TTestDbAddPre.DeleteTestDb;
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

procedure TTestDbAddPre.SetUp;
begin
  inherited SetUp;
  DeleteTestDb;
  // workaround for the known _CreateDatabaseFolder CWD bug in dbhandler.pas
  ForceDirectories(ExtractFilePath(ParamStr(0)) + DATABASEFOLDERNAME);

  // switch config to a file-based sqlite addpre db for the tests (in-memory only, never written to disk)
  fOldMode := config.ReadInteger('dbaddpre', 'mode', 3);
  fOldDbFile := config.ReadString('dbaddpre', 'db_file', 'db_addpre.db');
  fOldAutoUpdate := config.AutoUpdate;
  config.AutoUpdate := False;
  config.WriteInteger('dbaddpre', 'mode', Integer(apmSQLITE));
  config.WriteString('dbaddpre', 'db_file', CTEST_DB_NAME);

  dbaddpreStart;
end;

procedure TTestDbAddPre.TearDown;
begin
  dbaddpreUninit;
  config.WriteInteger('dbaddpre', 'mode', fOldMode);
  config.WriteString('dbaddpre', 'db_file', fOldDbFile);
  config.AutoUpdate := fOldAutoUpdate;
  DeleteTestDb;
  inherited TearDown;
end;

procedure TTestDbAddPre.TestInitAndAlive;
begin
  CheckTrue(AddPreDbAlive, 'addpre db should be alive after dbaddpreStart in sqlite mode');
end;

procedure TTestDbAddPre.TestInsertAndReadPretime;
begin
  CheckTrue(dbaddpre_InsertRlz('Test-Release-Group', 'TV', 'test-source'), 'first insert should return true');
  CheckTrue(ReadPretime('Test-Release-Group', plmSQLITE) > 0, 'pretime of an inserted release should be found');
end;

procedure TTestDbAddPre.TestInsertDuplicateIsIgnored;
begin
  CheckTrue(dbaddpre_InsertRlz('Test-Dupe-Group', 'TV', 'test-source'), 'first insert should return true');
  CheckFalse(dbaddpre_InsertRlz('Test-Dupe-Group', 'TV', 'test-source'), 'duplicate insert must be ignored and return false');
  CheckEquals(1, dbaddpre_GetCount, 'duplicate insert must not add another row');
end;

procedure TTestDbAddPre.TestGetCount;
begin
  CheckEquals(0, dbaddpre_GetCount, 'fresh db should be empty');
  dbaddpre_InsertRlz('Test-Count-One-Group', 'TV', 'test-source');
  dbaddpre_InsertRlz('Test-Count-Two-Group', '0DAY', 'test-source');
  CheckEquals(2, dbaddpre_GetCount, 'two distinct releases expected');
end;

procedure TTestDbAddPre.TestCleanupPathRunsInMemoryMode;
var
  i: integer;
begin
  // re-init in memory mode (db cleanup only runs there)
  dbaddpreUninit;
  config.WriteInteger('dbaddpre', 'mode', Integer(apmMemory));
  dbaddpreStart;

  // 55 inserts push the cleanup counter past DBCLEANUP_INTERVAL (50), so the
  // cleanup code path runs once. All entries share (nearly) the same ts, so the
  // delete-by-ts affects nothing - this is a smoke test that the path executes
  // without errors, the exact row counting is covered by sqlite itself.
  for i := 1 to 55 do
    CheckTrue(dbaddpre_InsertRlz('Cleanup-Test-' + IntToStr(i) + '-Group', 'TV', 'test-source'), 'insert should return true');
  CheckEquals(55, dbaddpre_GetCount, 'all inserted releases should be present');

  // restore started state in sqlite mode for TearDown
  dbaddpreUninit;
  config.WriteInteger('dbaddpre', 'mode', Integer(apmSQLITE));
  dbaddpreStart;
end;

procedure TTestDbAddPre.TestLegacyTableMigration;
var
  fLegacyDb: TSQLDataBase;
  fDb: TSQLDataBase;
  fTables: TRawUTF8DynArray;
  fTableName: RawUTF8;
  fHasLegacyTable, fHasOrmTable: boolean;
  fDbPath: String;
begin
  // build a realistic legacy database from scratch (as created by the old Zeos code)
  dbaddpreUninit;
  DeleteTestDb;

  fDbPath := ExtractFilePath(ParamStr(0)) + DATABASEFOLDERNAME + PathDelim + CTEST_DB_NAME;
  fLegacyDb := TSQLDataBase.Create(StringToUTF8(fDbPath));
  try
    fLegacyDb.Execute('CREATE TABLE addpre (rlz VARCHAR(255) NOT NULL, section VARCHAR(25) NOT NULL, ts INT(12) NOT NULL, source VARCHAR(255) NOT NULL)');
    fLegacyDb.Execute('INSERT INTO addpre (rlz, section, ts, source) VALUES (''Legacy-Release-Group'', ''TV'', 1700000000, ''test-legacy'')');
    fLegacyDb.Execute('CREATE UNIQUE INDEX IF NOT EXISTS addpre_index ON addpre (rlz)');
  finally
    fLegacyDb.Free;
  end;

  dbaddpreStart;

  CheckEquals(1, dbaddpre_GetCount, 'legacy entry should be migrated into the ORM table');
  CheckEquals(1700000000, ReadPretime('Legacy-Release-Group', plmSQLITE), 'pretime of the migrated release should be readable');

  // the legacy table must be gone after migration (close the ORM db first, it holds an exclusive lock)
  dbaddpreUninit;
  fDb := TSQLDataBase.Create(StringToUTF8(fDbPath));
  try
    fHasLegacyTable := False;
    fHasOrmTable := False;
    fDb.GetTableNames(fTables);
    for fTableName in fTables do
    begin
      if SameText(fTableName, 'addpre') then
        fHasLegacyTable := True;
      if SameText(fTableName, 'AddPreRecord') then
        fHasOrmTable := True;
    end;
    CheckFalse(fHasLegacyTable, 'legacy addpre table should be dropped after migration');
    CheckTrue(fHasOrmTable, 'ORM table AddPreRecord should exist after migration');
  finally
    fDb.Free;
  end;

  // restore started state for TearDown
  dbaddpreStart;
end;

initialization
  {$IFDEF FPC}
    RegisterTest('dbaddpre', TTestDbAddPre.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestDbAddPre);
  {$ENDIF}
end.
