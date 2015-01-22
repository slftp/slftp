unit mysqlutilunit;

interface

uses slmysql2, SysUtils, Classes, syncobjs, kb, regexpr;

type
  TMySQLThread = class(TThread)
  protected
  (* place holder for future mods like imdb over mysql....
  host:string;
  username:string;
  passw:string;
  dbname:string;
  tablename:string;
  port: integer;
  ping: integer;
  *)
  procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  mysqldb:    PMYSQL;
  mysql_lock: TCriticalSection = nil;

procedure MySQLInit;
procedure MySQLUninit;
procedure MySQLStart;

function MySQLConnected: boolean;
function MySQLInsertQuery(sql: string; args: array of const): boolean;

implementation

uses configunit, mainthread, debugunit
  , irc;

const
  section = 'mysql';

var
  mysqlth:    TMySQLThread;
  mysqlevent: TEvent = nil;

  mysql_host, mysql_user, mysql_pass, mysql_dbname: string;
  mysql_port, mysql_ping: integer;

procedure MySQLInit;
begin
  if slmysql_LoadedLibrary = '' then
    exit;

  // config
  mysql_host   := config.ReadString(section, 'host', '0');
  mysql_port   := config.ReadInteger(section, 'port', 0);
  mysql_user   := config.ReadString(section, 'user', '');
  mysql_pass   := config.ReadString(section, 'pass', '');
  mysql_dbname := config.ReadString(section, 'dbname', '');

  mysql_ping := config.ReadInteger(section, 'ping', 0);

  if mysql_host = '0' then
    exit; //we dont use mysql...


  // lock
  mysql_lock := TCriticalSection.Create;

  // mysql
  mysqldb := mysql_init(nil);
  if mysqldb = nil then
  begin
    Debug(dpError, section, '[ERROR] mysql_init', []);
    exit;
  end;
  if slmysql2.mysql_real_connect(mysqldb, PChar(mysql_host),
    PChar(mysql_user), PChar(mysql_pass), PChar(mysql_dbname),
    mysql_port, nil, CLIENT_MULTI_STATEMENTS) = nil then
  begin
    Debug(dpError, section, '[ERROR] mysql_real_connect: %s', [mysql_error(mysqldb)]);
  end
  else
  begin
    slmysql2.mysql_options(mysqldb, MYSQL_OPT_RECONNECT, 'true');
    slmysql2.mysql_options(mysqldb, MYSQL_OPT_COMPRESS, 0);
  end;

  // event
  mysqlevent := TEvent.Create(nil, False, False, 'mysql');

  // thread
  mysqlth := TMySQLThread.Create;
  mysqlth.FreeOnTerminate := True;
end;

procedure MySQLUninit;
begin
  if mysqlevent <> nil then
  begin
    mysqlevent.Free;
    mysqlevent := nil;
  end;

  if mysql_lock <> nil then
  begin
    mysql_lock.Free;
    mysql_lock := nil;

  end;

end;

procedure MySQLStart;
begin

end;

function MySQLConnected: boolean;
begin
  Result := False;
  try
    if slmysql2.mysql_ping(mysqldb) = 0 then
      Result := True
    else
      Result := False;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] MySQLConnected : %s', [e.Message]));
      exit;
    end;
  end;
end;

function MySQLInsertQuery(sql: string; args: array of const): boolean;
var
  mysql_err: string;
begin
  Result := False;
  try
    mysql_lock.Enter;
    try
      gc(mysqldb, sql, args);
    finally
      mysql_lock.Leave;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] MySQLInsertQuery : %s %s',
        [e.Message, sql]));
      exit;
    end;
  end;
end;


constructor TMySQLThread.Create;
begin
  inherited Create(False);
end;

destructor TMySQLThread.Destroy;
begin
  inherited;
end;

procedure TMySQLThread.Execute;
begin
  if mysql_ping <> 0 then
  begin
    while ((not slshutdown) and (not Terminated)) do
    begin
      try
        mysql_lock.Enter;
        try
          if slmysql2.mysql_ping(mysqldb) <> 0 then
          begin
            irc_Adderror('TMySQLThread.Execute: <c2>PiNG</c>: MySQL did not ping, restarting');
            if mysqldb <> nil then
            begin
              mysql_close(mysqldb);
              mysqldb := nil;
            end;
            mysqldb := mysql_init(nil);
            if mysqldb <> nil then
            begin
              if slmysql2.mysql_real_connect(mysqldb, PChar(mysql_host),
                PChar(mysql_user), PChar(mysql_pass), PChar(mysql_dbname),
                mysql_port, nil, CLIENT_MULTI_STATEMENTS) = nil then
              begin
                Debug(dpError, section, '[ERROR] mysql_real_connect: %s',
                  [mysql_error(mysqldb)]);
              end
              else
              begin
                slmysql2.mysql_options(mysqldb, MYSQL_OPT_RECONNECT, 'true');
                slmysql2.mysql_options(mysqldb, MYSQL_OPT_COMPRESS, nil);
              end;
            end;
          end;
        finally
          mysql_lock.Leave;
        end;
      except
        on e: Exception do
        begin
          Debug(dpError, section, '[EXCEPTION] TMySQLThread.Execute: %s', [e.Message]);
        end;
      end;
      mysqlevent.WaitFor(mysql_ping * 1000);
    end;
  end;
end;

end.

