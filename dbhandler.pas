unit dbhandler;

interface

uses
  mormot.db.sql.zeos, mormot.db.sql.sqlite3, mormot.rest.sqlite3, mormot.db.core, mormot.db.sql, mormot.orm.core, mormot.rest.server, mormot.rest.client;

{ Creates an initialized instance of TSQLDBSQLite3ConnectionProperties for further use of given SQLite3 database
  @param(aDatabaseName name of the database file on local storage, must include filename extension)
  @param(aPassword password which is used for encryption/decryption of the database (FOR FUTURE USE! [TODO])
  @param(aIsInMemory Set to true if this SQLite DB should be an in-memory DB)
  @returns(Initialized TSQLDBSQLite3ConnectionProperties instance, returns exception and nil on failure) }
function CreateSQLite3DbConn(const aDatabaseName: String; const aPassword: String; const aIsInMemory: Boolean = False): TSQLDBSQLite3ConnectionProperties;

{ Initialize an ORM instance of TRestClientDB with default settings and create missing tables
  @param(aORMSQLModel SQL ORM model for database (object must remain for complete runtime))
  @param(aDatabaseName name of the database file on local storage, must include filename extension)
  @param(aPassword password which is used for encryption/decryption of the database (FOR FUTURE USE! [TODO])
  @returns(Initialized TSQLRestClientDB instance, returns exception and nil on failure) }
function CreateORMSQLite3DB(const aORMSQLModel: TSQLModel; const aDatabaseName: String; const aPassword: String; const aIsInMemory: Boolean = False): TRestClientDb;
{ Initialize an ORM instance of TRestClientDB with default settings and create missing tables
  @param(aORMSQLModel SQL ORM model for database (object must remain for complete runtime))
  @param(aDatabaseName name of the database file on local storage, must include filename extension)
  @param(aLibName name of the library which is used for the connection)
  @param(aHostName hostname used for the connection)
  @param(aUserName UserName used for the connection)
  @param(aPassword Password used for the connection)
  @param(aPort Port used for the connection)
  @returns(Initialized TRestClientDB instance, returns exception and nil on failure) }
function CreateORMMysqlConnection(const aORMSQLModel: TSQLModel; const aDatabaseName, fLibName, aHostName, aUserName, aPassword, aPort: String): TRestClientDb;

var
  MySQLCon: TSQLDBZEOSConnectionProperties = nil; //< global connection to a MySQL/MariaDb server

implementation

uses
  SysUtils, debugunit, globals, mormot.core.unicode, mormot.db.raw.sqlite3, mormot.rest.core, mormot.orm.sql, dbaddpre, mormot.core.base, mormot.orm.base;

const
  section = 'dbhandler';

procedure _CreateDatabaseFolder;
begin
  if not DirectoryExists(DATABASEFOLDERNAME) then
    Mkdir(DATABASEFOLDERNAME);
end;

function _GetDatabasePath: String;
begin
  Result := ExtractFilePath(ParamStr(0)) + DATABASEFOLDERNAME + PathDelim;
end;

function CreateSQLite3DbConn(const aDatabaseName: String; const aPassword: String; const aIsInMemory: Boolean = False): TSQLDBSQLite3ConnectionProperties;
begin
  Result := nil;

  _CreateDatabaseFolder;

  try
    if aIsInMemory then
      Result := TSQLDBSQLite3ConnectionProperties.Create(':memory:', aDatabaseName, '', '')
    else
    begin
      Result := TSQLDBSQLite3ConnectionProperties.Create(StringToUTF8(_GetDatabasePath + aDatabaseName), '', '', '');
      // locks the database file for exclusive use during the whole session, read/write will be much faster
      Result.MainSQLite3DB.LockingMode := lmExclusive;
      // enable Write-Ahead Logging mode a which is slightly faster
      Result.MainSQLite3DB.WALMode := True;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] CreateSQLite3DbConn: %s - DatabaseName: %s, Password: %s', [e.Message, aDatabaseName, aPassword]));
      exit;
    end;
  end;
end;

function CreateORMSQLite3DB(const aORMSQLModel: TSQLModel; const aDatabaseName: String; const aPassword: String; const aIsInMemory: Boolean = False): TRestClientDb;
begin
  Result := nil;

  _CreateDatabaseFolder;

  try
    if aIsInMemory then
      //Result := TSQLRestServerDB(TSQLRestClientDB.Create(aORMSQLModel, nil, ':memory:', TSQLRestServerDB, False, StringToUTF8(aPassword))).Server
      Result := TSQLRestClientDB.Create(aORMSQLModel, nil, ':memory:', TSQLRestServerDB, False, StringToUTF8(aPassword))
    else
      Result := TSQLRestClientDB.Create(aORMSQLModel, nil, _GetDatabasePath + aDatabaseName, TSQLRestServerDB, False, StringToUTF8(aPassword));
    // locks the database file for exclusive use during the whole session, read/write will be much faster
    Result.DB.LockingMode := lmExclusive;
    // enable Write-Ahead Logging mode a which is slightly faster
    Result.DB.WALMode := True;
    Result.DB.Synchronous := smNormal;

    // create missing sql tables
    Result.Server.CreateMissingTables;
    //Result := TRestServerDB(ORMDB.Server).Server;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] CreateORMSQLite3DB: %s - DatabaseName: %s, Password: %s', [e.Message, aDatabaseName, aPassword]));
      exit;
    end;
  end;
end;

function CreateORMMysqlConnection(const aORMSQLModel: TSQLModel; const aDatabaseName, fLibName, aHostName, aUserName, aPassword, aPort: String): TRestClientDb;
var fOrmDb: TSQLRestServerDB;
    fOrmClientDb: TRestClientDb;
    Model: TOrmModel;
begin
  // create connection
  try
    MySQLCon := TSQLDBZEOSConnectionProperties.Create(TSQLDBZEOSConnectionProperties.URI(dMySQL, aHostName + ':' + aPort, fLibName), aDatabaseName, aUserName, aPassword);
    MySQLCon.ThreadSafeConnection.Connect;
    Debug(dpSpam, section, Format('Database Connection Established: %s', [MySQLCon.ThreadSafeConnection.Connected.ToString()]));

    Model := TOrmModel.Create([TSQLAddPreRecord]);

    OrmMapExternal(Model, [TSQLAddPreRecord], MySQLCon);
    // 3. Create the REST server, mapped to the external DB
    fOrmClientDb := TRestClientDB.Create(Model, nil, ':memory:', TRestServerDB, False, 'password');
    TRestServerDB(fOrmClientDb.Server).Server.CreateMissingTables(0, [itoNoAutoCreateUsers]);
  except
  on e: Exception do
    begin
      Debug(dpError, section, Format('Failed to load MySQL/MariaDB: %s', [e.Message]));
      exit;
    end;
  end;

  if not Assigned(MySQLCon) then
  begin
    Debug(dpError, section, Format('Failed to load MySQL/MariaDB: %s', [fLibName]));
    exit;
  end;

  Result := fOrmClientDb;
  Debug(dpSpam, section, 'MySQL/MariaDB library initialised.');
end;
end.
