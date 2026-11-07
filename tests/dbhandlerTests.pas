unit dbhandlerTests;

interface

uses
  {$IFDEF FPC}
    fpcunit, testregistry,
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility,
  {$ENDIF}
  mormot.core.base, mormot.db.raw.sqlite3, mormot.orm.core;

type
  TTestMemoryOrmRecord = class(TOrm)
  private
    FValue: RawUtf8;
  published
    property Value: RawUtf8 read FValue write FValue;
  end;

  TTestSQLite3RollbackIfActive = class(TTestCase)
  private
    fDB: TSqlDataBase;
    { Executes aSql and fails the test if it does not raise an exception }
    procedure ExecuteFailing(const aSql: RawUtf8);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure RollbackOpenTransaction;
    procedure NoTransaction;
    procedure TransactionEndedBySQLite;
    procedure TransactionOpenAfterFailedCommit;
  end;

  TTestORMSQLite3DB = class(TTestCase)
  published
    procedure InMemoryDatabaseStoresRecordsWithoutCreatingFile;
  end;

implementation

uses
  SysUtils, dbhandler, globals, mormot.rest.sqlite3;

procedure TTestORMSQLite3DB.InMemoryDatabaseStoresRecordsWithoutCreatingFile;
var
  fGUID: TGUID;
  fDatabaseName, fDatabasePath: String;
  fModel: TSQLModel;
  fDB: TSQLRestClientDB;
  fRecord, fRetrieved: TTestMemoryOrmRecord;
  fID: TID;
begin
  CreateGUID(fGUID);
  fDatabaseName := 'dbhandler-memory-' + GUIDToString(fGUID) + '.db';
  fDatabasePath := ExtractFilePath(ParamStr(0)) + DATABASEFOLDERNAME + PathDelim + fDatabaseName;
  fModel := TSQLModel.Create([TTestMemoryOrmRecord]);
  fDB := nil;
  try
    fDB := CreateORMSQLite3DB(fModel, fDatabaseName, '', True);
    CheckTrue(Assigned(fDB), 'in-memory ORM database should initialize');
    fRecord := TTestMemoryOrmRecord.Create;
    try
      fRecord.Value := 'memory-only';
      fID := fDB.Add(fRecord, True);
    finally
      fRecord.Free;
    end;

    fRetrieved := TTestMemoryOrmRecord.Create;
    try
      CheckTrue(fDB.Retrieve(fID, fRetrieved), 'record should be retrievable');
      CheckEquals('memory-only', String(fRetrieved.Value));
    finally
      fRetrieved.Free;
    end;
    CheckFalse(FileExists(fDatabasePath), 'in-memory database must not create a file');
  finally
    fDB.Free;
    fModel.Free;
  end;
  CheckFalse(FileExists(fDatabasePath), 'in-memory database must not persist after close');
end;

{ TTestSQLite3RollbackIfActive }

procedure TTestSQLite3RollbackIfActive.SetUp;
begin
  fDB := TSqlDataBase.Create(SQLITE_MEMORY_DATABASE_NAME);
  fDB.Execute('CREATE TABLE t (v TEXT NOT NULL)');
  // RAISE(ROLLBACK) ends the whole transaction, like SQLite does on e.g. SQLITE_FULL
  fDB.Execute('CREATE TRIGGER t_rollback BEFORE INSERT ON t WHEN NEW.v = ''rollback'' ' +
    'BEGIN SELECT RAISE(ROLLBACK, ''test''); END');
end;

procedure TTestSQLite3RollbackIfActive.TearDown;
begin
  fDB.Free;
end;

procedure TTestSQLite3RollbackIfActive.ExecuteFailing(const aSql: RawUtf8);
var
  fRaised: Boolean;
begin
  fRaised := False;
  try
    fDB.Execute(aSql);
  except
    on ESqlite3Exception do
      fRaised := True;
  end;
  CheckTrue(fRaised, 'no exception for: ' + String(aSql));
end;

procedure TTestSQLite3RollbackIfActive.RollbackOpenTransaction;
begin
  fDB.Execute('BEGIN TRANSACTION;');
  fDB.Execute('INSERT INTO t VALUES (''a'')');

  SQLite3RollbackIfActive(fDB);

  CheckEquals(1, sqlite3.get_autocommit(fDB.DB), 'transaction must be closed');
  CheckEquals(0, fDB.ExecuteNoExceptionInt64('SELECT COUNT(*) FROM t'));
end;

procedure TTestSQLite3RollbackIfActive.NoTransaction;
begin
  // a plain ROLLBACK raises 'cannot rollback - no transaction is active'
  SQLite3RollbackIfActive(fDB);

  CheckEquals(1, sqlite3.get_autocommit(fDB.DB));
end;

procedure TTestSQLite3RollbackIfActive.TransactionEndedBySQLite;
begin
  fDB.Execute('BEGIN TRANSACTION;');
  fDB.Execute('INSERT INTO t VALUES (''a'')');
  ExecuteFailing('INSERT INTO t VALUES (''rollback'')');

  SQLite3RollbackIfActive(fDB);

  // a new transaction must work
  fDB.Execute('BEGIN TRANSACTION;');
  fDB.Execute('INSERT INTO t VALUES (''b'')');
  fDB.Execute('COMMIT TRANSACTION;');
  CheckEquals(1, fDB.ExecuteNoExceptionInt64('SELECT COUNT(*) FROM t'));
end;

procedure TTestSQLite3RollbackIfActive.TransactionOpenAfterFailedCommit;
begin
  // a deferred foreign key violation makes COMMIT fail and keeps the transaction open
  fDB.Execute('PRAGMA foreign_keys = ON');
  fDB.Execute('CREATE TABLE parent (id INTEGER PRIMARY KEY)');
  fDB.Execute('CREATE TABLE child (pid INTEGER REFERENCES parent (id) DEFERRABLE INITIALLY DEFERRED)');
  fDB.Execute('BEGIN TRANSACTION;');
  fDB.Execute('INSERT INTO child VALUES (1)');
  ExecuteFailing('COMMIT TRANSACTION;');
  CheckEquals(0, sqlite3.get_autocommit(fDB.DB), 'test setup: transaction must still be open');

  SQLite3RollbackIfActive(fDB);

  CheckEquals(1, sqlite3.get_autocommit(fDB.DB), 'transaction must be closed');
  CheckEquals(0, fDB.ExecuteNoExceptionInt64('SELECT COUNT(*) FROM child'));
end;

initialization
  {$IFDEF FPC}
    RegisterTest('TTestSQLite3RollbackIfActive', TTestSQLite3RollbackIfActive.Suite);
    RegisterTest('TTestORMSQLite3DB', TTestORMSQLite3DB.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestSQLite3RollbackIfActive);
    TDUnitX.RegisterTestFixture(TTestORMSQLite3DB);
  {$ENDIF}
end.
