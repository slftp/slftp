unit dbhandler;

interface

uses
  SynDBSQLite3, SynDBZeos, mORMot, mORMotSQLite3;

{ Creates an initialized instance of TSQLDBSQLite3ConnectionProperties for further use of given SQLite3 database
  @param(aDatabaseName name of the database file on local storage, must include filename extension)
  @param(aPassword password which is used for encryption/decryption of the database (FOR FUTURE USE! [TODO])
  @returns(Initialized TSQLDBSQLite3ConnectionProperties instance, returns exception and nil on failure) }
function CreateSQLite3DbConn(const aDatabaseName: String; const aPassword: String): TSQLDBSQLite3ConnectionProperties;

{ Initialize an ORM instance of TSQLRestClientDB with default settings and create missing tables
  @param(aORMSQLModel SQL ORM model for database (object must remain for complete runtime))
  @param(aDatabaseName name of the database file on local storage, must include filename extension)
  @param(aPassword password which is used for encryption/decryption of the database (FOR FUTURE USE! [TODO])
  @returns(Initialized TSQLRestClientDB instance, returns exception and nil on failure) }
function CreateORMSQLite3DB(const aORMSQLModel: TSQLModel; const aDatabaseName: String; const aPassword: String): TSQLRestClientDB;

var
  MySQLCon: TSQLDBZEOSConnectionProperties = nil; //< global connection to a MySQL/MariaDb server

implementation

uses
  SysUtils, debugunit, globals, SynCommons, SynSQLite3;

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

function CreateSQLite3DbConn(const aDatabaseName: String; const aPassword: String): TSQLDBSQLite3ConnectionProperties;
begin
  Result := nil;

  _CreateDatabaseFolder;

  try
    Result := TSQLDBSQLite3ConnectionProperties.Create(StringToUTF8(_GetDatabasePath + aDatabaseName), '', '', '');
    // locks the database file for exclusive use during the whole session, read/write will be much faster
    Result.MainSQLite3DB.LockingMode := lmExclusive;
    // enable Write-Ahead Logging mode a which is slightly faster
    Result.MainSQLite3DB.WALMode := True;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] CreateSQLite3DbConn: %s - DatabaseName: %s, Password: %s', [e.Message, aDatabaseName, aPassword]));
      exit;
    end;
  end;
end;

function CreateORMSQLite3DB(const aORMSQLModel: TSQLModel; const aDatabaseName: String; const aPassword: String): TSQLRestClientDB;
begin
  Result := nil;

  _CreateDatabaseFolder;

  try
    Result := TSQLRestClientDB.Create(aORMSQLModel, nil, _GetDatabasePath + aDatabaseName, TSQLRestServerDB, False, StringToUTF8(aPassword));
    // locks the database file for exclusive use during the whole session, read/write will be much faster
    Result.DB.LockingMode := lmExclusive;
    // enable Write-Ahead Logging mode a which is slightly faster
    Result.DB.WALMode := True;
    // create missing sql tables
    Result.Server.CreateMissingTables;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] CreateORMSQLite3DB: %s - DatabaseName: %s, Password: %s', [e.Message, aDatabaseName, aPassword]));
      exit;
    end;
  end;
end;

end.
