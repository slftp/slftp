unit dbaddpre;

interface

uses Classes, IniFiles, irc, kb;

type
TPretimeResult = record
pretime:TDateTime;
mode:string;
end;


  TPretimeLookupMOde = (plmNone, plmHTTP, plmMYSQL, plmSQLITE);
  
  TDbAddPre = class
    rls: String;
    pretime: TDateTime;
    constructor Create(rls: string; pretime: TDateTime);
    destructor Destroy; override;
  end;

function dbaddpre_ADDPRE(netname, channel, nickname: string; params: string; event: string): boolean;
function dbaddpre_GetRlz(rls: string): TDateTime;
function dbaddpre_InsertRlz(rls, rls_section, source: string): boolean;
function dbaddpre_GetCount: Integer;
function dbaddpre_GetPreduration(rlz_pretime: TDateTime): string;
function dbaddpre_Status: string;

function dbaddpre_Process(net, chan, nick, msg: string): Boolean;
function dbsitepre_Process(net, chan, nick, msg: string): Boolean;

procedure dbaddpreInit;
procedure dbaddpreStart;
procedure dbaddpreUnInit;


function getPretime(rlz: string): TPretimeResult;

function ReadPretime(rlz: string): TDateTime;
function ReadPretimeOverHTTP(rls:string):TDateTime;
function ReadPretimeOverMYSQL(rls:string):TDateTime;
function ReadPretimeOverSQLITE(rls:string):TDateTime;

function GetPretimeMode: TPretimeLookupMOde;
function pretimeModeToString(mode:TPretimeLookupMOde):string;

implementation

uses DateUtils, SysUtils, Math, configunit, mystrings, irccommandsunit, console,
  sitesunit, queueunit, slhttp, regexpr, debugunit, pazo, taskrace,
  precatcher, SyncObjs, DateUnit, taskpretime,
  slsqlite, mysqlutilunit, slmysql2;

const
  section = 'dbaddpre';

var
  addpreDB: TslSqliteDB = nil;
  sql_addrlz: Psqlite3_stmt = nil;
  sql_countrlz: Psqlite3_stmt = nil;
  sql_gettime: Psqlite3_stmt = nil;

  addprecmd: TStringlist;
  siteprecmd: string;
  kbadd_addpre: Boolean;
  kbadd_sitepre: Boolean;

  last_addpre: THashedStringList;
  last_addpre_lock : TCriticalSection;

  dbaddpre_mode: Integer = 0;
  dbaddpre_plm1: TPretimeLookupMOde;
  dbaddpre_plm2: TPretimeLookupMOde;

  config_taskpretime_url: String;

function GetPretimeMode: TPretimeLookupMOde;
begin
  Result:= dbaddpre_plm1;
end;

function pretimeModeToString(mode:TPretimeLookupMOde):string;
begin
case mode of
plmNone:result:='None';
plmHTTP:result:='HTTP';
plmMYSQL:result:='MYSQL';
plmSQLITE:result:='SQLite';
end;
end;

{ TDbAddPre }
constructor TDbAddPre.Create(rls: string; pretime: TDateTime);
begin
  self.rls := rls;
  self.pretime := pretime;
end;

destructor TDbAddPre.Destroy;
begin
  inherited;
end;

function GetPretimeURL:string;
begin
  result:=config.readString(section,'url','');
end;

function ReadPretimeOverHTTP(rls:string):TDateTime;
var
  response: TstringList;
  prex:TRegexpr;
  vctime:int64;
  url:string;
  i:integer;
  read_count: Integer;
  sql: string;
begin
  Result := UnixToDateTime(0);
  if rls = '' then irc_adderror('No Releasename as parameter!');

  vctime:= 0;

  url:= config_taskpretime_url;
  if url = '' then
  begin
    debug(dpSpam, section, 'URL value is empty');
    exit;
  end;
  response:= TstringList.Create;
  response.text:= slUrlGet(Format(url, [rls]));
  debug(dpSpam, section, 'Pretime results for %s'+#13#10+'%s', [rls, response.text]);
  prex:=TRegexpr.Create;
  prex.ModifierM:=True;
  prex.Expression:='(\S+) (\S+) (\S+) (\S+) (\S+)$';
  read_count:= 0;
  for i := 0 to response.Count - 1 do
  begin
    inc(read_count);
    if read_count > 500 then
    begin
    irc_addtext('CONSOLE','ADMIN','Read count higher then 500');
      Result := UnixToDateTime(0);
      break;
    end;
    if prex.Exec(response.strings[i]) then
    begin
      Debug(dpMessage, section, 'ReadPretimeOverHTTP : %s', [response.DelimitedText]);

      if (StrToIntDef(prex.Match[2], 0) <> 0) and (StrToIntDef(prex.Match[2], 0) <> 1295645417) then
      begin
        vctime:= StrToIntDef(prex.Match[2], 0);
        result:=UnixToDateTime(vctime);




        if ((DaysBetween(Now(), Result) > 30) and config.ReadBool('kb','skip_rip_older_then_one_month',False)) then begin
//        irc_addtext('CONSOLE','ADMIN','Days higher then 30 days');
          Result := UnixToDateTime(0);
        end;
      end else begin
//      irc_addtext('CONSOLE','ADMIN','regex dosnot match');
        Result := UnixToDateTime(0);
      end;
    end;
  end;
  prex.free;
  response.free;
end;


function ReadPretimeOverSQLITE(rls:string):TDateTime;
var //time:int64;
//    rlz_timestamp: String;
    i: Integer;
begin
  Result := UnixToDateTime(0);
  if rls = '' then irc_adderror('No Releasename as parameter!');

 if addpreDB = nil then exit;
 if sql_gettime = nil then exit;

 try
   i:= 0;
   addpreDB.Open(sql_gettime, [rls]);
   while addpreDB.Step(sql_gettime) do
   begin
     inc(i); if i > 10 then begin Result := UnixToDateTime(0); exit; end;
     Result := UnixToDateTime(addpreDB.column_int64(sql_gettime, 0));
   end;
 except
   Result := UnixToDateTime(0);
 end;
end;

function ReadPretimeOverMYSQL(rls:string):TDateTime;
var time:int64;
    rlz_timestamp: String;
    i_rows: Integer;
    q, mysql_err: string;
    myRES : PMYSQL_RES;
    myROW : MYSQL_ROW;
    rows: integer;
begin
  try
    mysql_lock.Enter;
    try
      q:= 'SELECT ts FROM addpre WHERE rls=''%s'';';
      if StrToIntDef(gc(mysqldb, q, [rls]), 0) <> 0 then
        Result := UnixUTCToDateTime(StrToIntDef(gc(mysqldb, q, [rls]), 0));
    finally
      mysql_lock.Leave;
    end;
  except
    on e: Exception do
    begin
      Result := UnixToDateTime(0);
      exit;
    end;
  end;
end;



function getPretime(rlz: string): TPretimeResult;
begin
  Result.pretime:= UnixToDateTime(0);
  Result.mode:='None';
  if rlz = '' then irc_adderror('GETPRETIME --> No RLZ value!');

  case dbaddpre_plm1 of
    plmNone: Exit;
    plmHTTP: Result.pretime:= ReadPretimeOverHTTP(rlz);
    plmMYSQL: Result.pretime:= ReadPretimeOverMYSQL(rlz);
    plmSQLITE: Result.pretime:= ReadPretimeOverSQLITE(rlz);
    else begin
      Debug(dpMessage, section, 'GetPretime unknown pretime mode : %d', [config.ReadInteger('taskpretime','mode',0)]);
     Result.pretime:= UnixToDateTime(0);
    end;
  end;

  if (result.pretime <> UnixToDateTime(0)) then
  begin
    result.mode:=pretimeModeToString(dbaddpre_plm1);
    result.pretime:=UnixToDateTime(PrepareTimestamp(DateTimeToUnix(result.pretime)));
    exit;
  end;

  case dbaddpre_plm2 of
    plmNone: Exit;
    plmHTTP: result.pretime:= ReadPretimeOverHTTP(rlz);
    plmMYSQL: result.pretime:= ReadPretimeOverMYSQL(rlz);
    plmSQLITE: result.pretime:= ReadPretimeOverSQLITE(rlz);
    else begin
      Debug(dpMessage, section, 'GetPretime unknown pretime mode_2 : %d', [config.ReadInteger('taskpretime','mode_2',0)]);
      Result.pretime := UnixToDateTime(0);
    end;
  end;

  if (result.pretime <> UnixToDateTime(0)) then
  begin
    result.mode:=pretimeModeToString(dbaddpre_plm2);
    result.pretime:=UnixToDateTime(PrepareTimestamp(DateTimeToUnix(result.pretime)));
  end;

end;

function ReadPretime(rlz: string): TDateTime;
begin
  Result := UnixToDateTime(0);

  if rlz = '' then irc_adderror('GETPRETIME --> No RLZ value!');


  case dbaddpre_plm1 of
    plmNone: Exit;
    plmHTTP: result:= ReadPretimeOverHTTP(rlz);
    plmMYSQL: result:= ReadPretimeOverMYSQL(rlz);
    plmSQLITE: result:= ReadPretimeOverSQLITE(rlz);
    else begin
      Debug(dpMessage, section, 'GetPretime unknown pretime mode : %d', [config.ReadInteger('taskpretime','mode',0)]);
      Result := UnixToDateTime(0);
    end;
  end;

  if ((Result = UnixToDateTime(0)) and (dbaddpre_plm2 <> plmNone)) then
  begin
    case dbaddpre_plm2 of
      plmNone: Exit;
      plmHTTP: result:= ReadPretimeOverHTTP(rlz);
      plmMYSQL: result:= ReadPretimeOverMYSQL(rlz);
      plmSQLITE: result:= ReadPretimeOverSQLITE(rlz);
      else begin
        Debug(dpMessage, section, 'GetPretime unknown pretime mode : %d', [config.ReadInteger('taskpretime','mode_2',0)]);
        Result := UnixToDateTime(0);
      end;
    end;
   end;

   if (result <> UnixToDateTime(0)) then
   begin
        result:=UnixToDateTime(PrepareTimestamp(DateTimeToUnix(result)));
   end;
end;

function kb_Add_addpre(rls, section: string; event: string): Integer;
var rls_section: String;
begin
  Result:= -1;
  
  section:= ProcessDoReplace(section);
  rls_section:= '';
  rls_section:= KibontasSection(' '+section+' ', '');
  rls_section:= PrecatcherSectionMapping(rls, rls_section);
  if (rls_section = 'TRASH') then
  begin
    exit;
  end;
  
  if (rls_section = '') then
  begin
    irc_Addstats(Format('<c7>[ADDPRE]</c> %s %s (%s) : <b>No Sites</b>', [rls, rls_section, section]));
    exit;
  end;

  kb_Add('', '', config.ReadString('sites', 'admin_sitename', 'SLFTP'), rls_section, '', event, rls, '');
end;

function dbaddpre_ADDPRE(netname, channel, nickname: string; params: string; event: string): boolean;
var
  rls: string;
  rls_section: string;
begin
  Result := False;

  rls := '';
  rls := SubString(params, ' ', 1);
  rls_section := '';
  rls_section := UpperCase(SubString(params, ' ', 2));
  if ((rls <> '') and (rls_section <> '') and (length(rls) > minimum_rlsname)) then
  begin
    dbaddpre_InsertRlz(rls, rls_section, netname+'-'+channel+'-'+nickname);
    if (event = 'ADDPRE') and (kbadd_addpre) then
    begin
      kb_Add_addpre(rls, rls_section, event);
    end;
    if (event = 'SITEPRE') and (kbadd_sitepre) then
    begin
      kb_Add_addpre(rls, rls_section, event);
    end;
  end;

  Result := True;
end;

function dbaddpre_GetRlz(rls: string): TDateTime;
var i: Integer;
    addpredata: TDbAddPre;
    q, mysql_err, mysql_result: string;
    myRES : PMYSQL_RES;
    myROW : MYSQL_ROW;
    rows, i_rows: integer;
begin
  Result := UnixToDateTime(0);
  // stor in memory
  if (dbaddpre_mode = 0) then
  begin
    try
      last_addpre_lock.Enter;
      try
        i:= last_addpre.IndexOf(rls);
        if i = -1 then
        begin
          Result := UnixToDateTime(0);
          exit;
        end;
        addpredata:= TDbAddPre(last_addpre.Objects[i]);
      finally
        last_addpre_lock.Leave;
      end;

      Result:= addpredata.pretime;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] dbaddpre_GetRlz (memory): %s', [e.Message]));
        Result := UnixToDateTime(0);
        exit;
      end;
    end;
  end;

  // stor in sqlite
  if dbaddpre_mode = 1 then
  begin
    try
      i:= 0;
      addpreDB.Open(sql_gettime, [rls]);
      while addpreDB.Step(sql_gettime) do
      begin
        inc(i); if i > 10 then begin Result := UnixToDateTime(0); exit; end;
        Result := UnixToDateTime(addpreDB.column_int64(sql_gettime, 0));
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] dbaddpre_GetRlz (sqlite): %s', [e.Message]));
        Result := UnixToDateTime(0);
        exit;
      end;
    end;
  end;

  // stor in mysql
  if dbaddpre_mode = 2 then
  begin
    try
      mysql_lock.Enter;
      try
        q:= 'SELECT ts FROM addpre WHERE rls=''%s'';';
        mysql_result:= gc(mysqldb, q, [rls]);
        if mysql_result <> '' then
          Result := UnixUTCToDateTime(StrToIntDef(mysql_result, 0));
      finally
        mysql_lock.Leave;
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] dbaddpre_GetRlz (mysql): %s', [e.Message]));
        Result := UnixToDateTime(0);
        exit;
      end;
    end;
  end;
end;

function dbaddpre_InsertRlz(rls, rls_section, source: string): Boolean;
var i: Integer;
    sql: string;
    pretime: TDateTime;
    addpredata: TDbAddPre;
begin
  Result:= False;

  pretime:= dbaddpre_GetRlz(rls);
  if pretime <> UnixToDateTime(0) then exit;

  if (dbaddpre_mode = 0) then
  begin
    try
      last_addpre_lock.Enter;
      try
        addpredata:= TDbAddPre.Create(rls, Now());
        last_addpre.AddObject(rls, addpredata);

        i:= last_addpre.Count;
        if i > 150 then
        begin
          while i > 100 do
          begin
            last_addpre.Delete(0);
            i:= last_addpre.Count - 1;
          end;
        end;
      finally
        last_addpre_lock.Leave;
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] dbaddpre_InsertRlz (memory): %s', [e.Message]));
        Result := False;
        exit;
      end;
    end;
  end;

  if dbaddpre_mode = 1 then
  begin
    try
      pretime:= dbaddpre_GetRlz(rls);
      if pretime <> UnixToDateTime(0) then exit;

      addpreDB.ExecSQL( sql_addrlz, [rls, rls_section, DateTimeToUnix(Now()), source]);
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] dbaddpre_InsertRlz (sqlite): %s', [e.Message]));
        Result := False;
        exit;
      end;
    end;
  end;

  if dbaddpre_mode = 2 then
  begin
    try
      sql := 'INSERT IGNORE INTO addpre(rls, section, ts, source) VALUES (''%s'',''%s'', UNIX_TIMESTAMP(NOW()), ''%s'');';
      MySQLInsertQuery(sql, [rls, rls_section, source]);
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] dbaddpre_InsertRlz (mysql): %s', [e.Message]));
        Result := False;
        exit;
      end;
    end;
  end;


  Result:= true;
end;

function dbaddpre_GetCount: Integer;
var i: Integer;
    q, res: string;
begin
  Result:= 0;

  if dbaddpre_mode = 0 then
  begin
    Result:= last_addpre.Count;
    exit;
  end;

  if dbaddpre_mode = 1 then
  begin
    i:= 0;
    addpreDB.Open(sql_countrlz);
    while addpreDB.Step(sql_countrlz) do
    begin
      inc(i); if i > 10 then begin Result:= 0; exit; end;
      Result := addpreDB.column_int(sql_countrlz, 0);
    end;
    exit;
  end;

  if dbaddpre_mode = 2 then
  begin
    mysql_lock.Enter;
    try
      q:= 'SELECT count(*) as count FROM addpre;';
      res:= gc(mysqldb, q, []);
      Result:= StrToIntDef(res, 0);
    finally
      mysql_lock.Leave;
    end;
    exit;
  end;
end;

function dbaddpre_GetPreduration(rlz_pretime: TDateTime): string;
var
  preage: int64;
begin
  preage := DateTimeToUnix(Now) - DateTimeToUnix(rlz_pretime);
  if preage >= 604800 then
    Result := Format('%2.2d Weeks %1.1d Days %2.2d Hour %2.2d Min %2.2d Sec', [preage div 604800, (preage div 86400) mod
      7, (preage div 3600) mod 24, (preage div 60) mod 60, preage mod 60])
  else
  if preage >= 86400 then
    Result := Format('%1.1d Days %2.2d Hour %2.2d Min %2.2d Sec', [preage div 86400, (preage div 3600) mod 24, (preage div 60) mod 60, preage mod 60])
  else
  if preage >= 3600 then
    Result := Format('%2.2d Hour %2.2d Min %2.2d Sec', [preage div 3600, (preage div 60) mod 60, preage mod 60])
  else
  if preage >= 60 then
    Result := Format('%2.2d Min %2.2d Sec', [(preage div 60) mod 60, preage mod 60])
  else
    Result := Format('%2.2d Sec', [preage mod 60]);
end;


function dbaddpre_Process(net, chan, nick, msg: string): Boolean;
var
ii:integer;
begin
  Result := False;
 ii:=-1;
  try
  ii:=addprecmd.IndexOf(substring(msg,' ',1));
  except
  on e: Exception do
  Debug(dpError, section, Format('[EXCEPTION] dbaddpre_Process: %s ', [e.Message]));
  end;


  if ii > -1 then
//  if (1 = Pos(addprecmd, msg)) then
  begin
    Result := True;
    msg := Copy(msg, length(addprecmd.Strings[ii] + ' ') + 1, 1000);
    try
      last_addpre_lock.Enter;
      try
        dbaddpre_ADDPRE(net, chan, nick, msg, 'ADDPRE');
      finally
        last_addpre_lock.Leave;
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] dbaddpre_Process: %s ', [e.Message]));
        dbaddpreUninit;
        dbaddpreinit;
      end;
    end;
  end;
end;

function dbsitepre_Process(net, chan, nick, msg: string): Boolean;
var rls, rls_section: String;
begin
  Result := False;
  if (1 = Pos(siteprecmd, msg)) then
  begin
    Result := True;
    msg := Copy(msg, length(siteprecmd + ' ') + 1, MaxInt);
    rls := '';
    rls := SubString(msg, ' ', 1);
    rls_section := '';
    rls_section := UpperCase(SubString(msg, ' ', 2));
    try
      dbaddpre_ADDPRE(net, chan, nick, msg, 'SITEPRE');
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] dbsitepre_Process: %s ', [e.Message]));
      end;
    end;
  end;
end;

function dbaddpre_Status: string;
begin
  Result := '';
  Result:= Format('<b>DB addpre</b>: %d (%d) rls',[dbaddpre_GetCount, last_addpre.Count]);
end;

procedure dbaddpreInit;
begin
  last_addpre_lock:= TCriticalSection.Create;
  addprecmd:=TStringlist.Create;
  last_addpre:= THashedStringList.Create;
  last_addpre.Duplicates:= dupIgnore;
  last_addpre.CaseSensitive:= false;
  last_addpre.Sorted:= true;
end;

procedure dbaddpreStart;
var
  db_pre_name: string;
begin

  addprecmd.CommaText := config.ReadString(section, 'addprecmd', '!addpre');
  siteprecmd := config.ReadString(section, 'siteprecmd', '!sitepre');
  kbadd_addpre := config.ReadBool(section, 'kbadd_addpre', False);
  kbadd_sitepre := config.ReadBool(section, 'kbadd_sitepre', False);

  dbaddpre_mode:= config.ReadInteger(section, 'mode', 0);
  dbaddpre_plm1:= TPretimeLookupMOde(config.ReadInteger('taskpretime','mode',0));
  dbaddpre_plm2:= TPretimeLookupMOde(config.ReadInteger('taskpretime','mode_2',0));

  config_taskpretime_url:= config.readString('taskpretime','url','');

  if slsqlite_inited and (dbaddpre_mode = 1) then
  begin
    db_pre_name := Trim(config.ReadString(section, 'db_file', 'db_addpre.db'));

    // Addpre DB
    addpreDB := TslSqliteDB.Create(db_pre_name, config.ReadString(section, 'pragma', ''));
    addpreDB.ExecSQL(
      'CREATE TABLE IF NOT EXISTS addpre (rlz VARCHAR(255) NOT NULL, section VARCHAR(25) NOT NULL, ts INT(12) NOT NULL, source VARCHAR(255) NOT NULL)'
      );
    addpreDB.ExecSQL(
      'CREATE UNIQUE INDEX IF NOT EXISTS addpre_index ON addpre (rlz)'
      );

    sql_addrlz := addpreDB.Open('INSERT OR IGNORE INTO addpre (rlz, section, ts, source) VALUES (?, ?, ?, ?)');
    sql_countrlz := addpreDB.Open('SELECT count(*) FROM addpre');
    sql_gettime := addpreDB.Open('SELECT ts FROM addpre WHERE rlz=?');
  end;

  case dbaddpre_mode of
    0: Console_Addline('', 'Local addpre DB Started...');
    1: Console_Addline('', 'Local SQLITE addpre DB Started...');
    2: Console_Addline('', 'Local MYSQL addpre DB Started...');
    else begin
      Console_Addline('', 'Local addpre DB NOT Started.');
    end;
  end;
end;

procedure dbaddpreUninit;
begin
  addprecmd.free;
  last_addpre_lock.Free;
  last_addpre.Free;
  if addpreDB <> nil then
  begin
    addpreDB.Free;
    addpreDB := nil;
  end;
end;

end.

