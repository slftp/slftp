unit dbhandler;

interface

uses
  SynDBSQLite3, SynDBZeos;

{ Creates an initialized instance of TSQLDBSQLite3ConnectionProperties for further use of given SQLite3 database
  @param(aDatabaseName name of the database file on local storage, must include filename extension)
  @param(aPassword password which is used for encryption/decryption of the database (FOR FUTURE USE! [TODO])
  @returns(Initialized TSQLDBSQLite3ConnectionProperties instance, returns exception and nil on failure) }
function CreateSQLite3DbConn(const aDatabaseName: String; const aPassword: String): TSQLDBSQLite3ConnectionProperties;

var
  MySQLCon: TSQLDBZEOSConnectionProperties = nil; //< global connection to a MySQL/MariaDb server

implementation

uses
  SysUtils, debugunit, SynSQLite3;

const
  section = 'dbhandler';

function CreateSQLite3DbConn(const aDatabaseName: String; const aPassword: String): TSQLDBSQLite3ConnectionProperties;
const
  DATABASEFOLDERNAME = 'databases';
var
  fDatabasePath: String;
begin
  Result := nil;

  if not DirectoryExists(DATABASEFOLDERNAME) then
    Mkdir(DATABASEFOLDERNAME);

  fDatabasePath := ExtractFilePath(ParamStr(0)) + DATABASEFOLDERNAME + PathDelim;

  try
    Result := TSQLDBSQLite3ConnectionProperties.Create(fDatabasePath + aDatabaseName, '', '', '');
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

end.
