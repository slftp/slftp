unit mysqlutilunit;

interface

uses slmysql2,sysutils,classes,syncobjs,kb, regexpr;

type
  TMySQLThread = class(TThread)
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
  end;

var
  mysqldb: PMYSQL;
  mysql_lock: TCriticalSection;

procedure MySQLInit;
procedure MySQLUninit;
procedure MySQLStart;

function MySQLConnected:boolean;
function MySQLInsertQuery(sql: string; args: array of const):boolean;

implementation

uses configunit, mainthread, debugunit
     , irc;

const section = 'mysql';

var
  mysqlth: TMySQLThread;
  mysqlevent: TEvent;

  mysql_host, mysql_user, mysql_pass, mysql_dbname: String;
  mysql_port, mysql_ping: Integer;

procedure MySQLInit;
begin
  if slmysql_LoadedLibrary = '' then
    exit;

  // config
  mysql_host:= config.ReadString(section,'host','');
  mysql_port:= config.ReadInteger(section,'port',0);
  mysql_user:= config.ReadString(section,'user','');
  mysql_pass:= config.ReadString(section,'pass','');
  mysql_dbname:= config.ReadString(section,'dbname','');

  mysql_ping:= config.ReadInteger(section,'ping',0);

  // lock
  mysql_lock:= TCriticalSection.Create;

  // mysql
  mysqldb := mysql_init(nil);
  if mysqldb = nil then
  begin
    Debug(dpError, section, '[ERROR] mysql_init', []);
    exit;
  end;
  if slmysql2.mysql_real_connect(mysqldb, pChar(mysql_host), pChar(mysql_user), pChar(mysql_pass), pChar(mysql_dbname), mysql_port, nil, CLIENT_MULTI_STATEMENTS) = nil then
  begin
    Debug(dpError, section, '[ERROR] mysql_real_connect: %s', [mysql_error(mysqldb)]);
  end else begin
    slmysql2.mysql_options(mysqldb, MYSQL_OPT_RECONNECT, 'true');
    slmysql2.mysql_options(mysqldb, MYSQL_OPT_COMPRESS, 0);
  end;

  // event
  mysqlevent:= TEvent.Create(nil, False, False, 'mysql');
  
  // thread
  mysqlth:= TMySQLThread.Create;
  mysqlth.FreeOnTerminate:= True;
end;

procedure MySQLUninit;
begin
  mysqlevent.Free;

  mysql_lock.Free;
end;

procedure MySQLStart;
begin

end;

function MySQLConnected:boolean;
begin
  Result:= False;
  try
    if slmysql2.mysql_ping(mysqldb) = 0 then Result:= True else Result:= False;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] MySQLConnected : %s', [e.Message]));
      exit;
    end;
  end;
end;

function MySQLInsertQuery(sql: string; args: array of const): boolean;
var mysql_err: string;
begin
  Result:= False;
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
      Debug(dpError, section, Format('[EXCEPTION] MySQLInsertQuery : %s %s', [e.Message, sql]));
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
              if slmysql2.mysql_real_connect(mysqldb, pChar(mysql_host), pChar(mysql_user), pChar(mysql_pass), pChar(mysql_dbname), mysql_port, nil, CLIENT_MULTI_STATEMENTS) = nil then
              begin
                Debug(dpError, section, '[ERROR] mysql_real_connect: %s', [mysql_error(mysqldb)]);
              end else begin
                slmysql2.mysql_options(mysqldb, MYSQL_OPT_RECONNECT, 'true');
                slmysql2.mysql_options(mysqldb, MYSQL_OPT_COMPRESS, 0);
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
