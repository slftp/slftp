unit dbaddpre;

interface

uses Classes, IniFiles, irc, kb;

type
  TPretimeResult = record
    pretime: TDateTime;
    mode: AnsiString;
  end;

  TPretimeLookupMOde = (plmNone, plmHTTP, plmMYSQL, plmSQLITE);
  TAddPreMode = (apmMem, apmSQLITE, apmMYSQL, apmNone);

  TDbAddPre = class
    rls: AnsiString;
    pretime: TDateTime;
    constructor Create(rls: AnsiString; pretime: TDateTime);
    destructor Destroy; override;
  end;

function dbaddpre_ADDPRE(netname, channel, nickname: AnsiString; params: AnsiString;
  event: AnsiString): boolean;
function dbaddpre_GetRlz(rls: AnsiString): TDateTime;
function dbaddpre_InsertRlz(rls, rls_section, Source: AnsiString): boolean;
function dbaddpre_GetCount: integer;
function dbaddpre_GetPreduration(rlz_pretime: TDateTime): AnsiString;
function dbaddpre_Status: AnsiString;

function dbaddpre_Process(net, chan, nick, msg: AnsiString): boolean;

procedure dbaddpreInit;
procedure dbaddpreStart;
procedure dbaddpreUnInit;

function getPretime(rlz: AnsiString): TPretimeResult;

//function ReadPretime(rlz: string): TDateTime;
function ReadPretimeOverHTTP(rls: AnsiString): TDateTime;
function ReadPretimeOverMYSQL(rls: AnsiString): TDateTime;
function ReadPretimeOverSQLITE(rls: AnsiString): TDateTime;

function GetPretimeMode: TPretimeLookupMOde;
function pretimeModeToString(mode: TPretimeLookupMOde): AnsiString;
function addPreModeToString(mode: TAddPreMode): AnsiString;

procedure setPretimeMode_One(mode: TPretimeLookupMOde);
procedure setPretimeMode_Two(mode: TPretimeLookupMOde);

procedure setAddPretimeMode(mode: TAddPreMode);

function AddPreDbAlive: boolean;

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

  addprecmd: TStringList;
  siteprecmd: AnsiString;
  kbadd_addpre: boolean;
  kbadd_sitepre: boolean;

  last_addpre: THashedStringList;
  last_addpre_lock: TCriticalSection;

  dbaddpre_mode: TAddPreMode = TAddPreMode(3);
  //  integer = 1;
  dbaddpre_plm1: TPretimeLookupMOde;
  dbaddpre_plm2: TPretimeLookupMOde;

  config_taskpretime_url: AnsiString;

procedure setPretimeMode_One(mode: TPretimeLookupMOde);
begin
  dbaddpre_plm1 := mode;
end;

procedure setPretimeMode_Two(mode: TPretimeLookupMOde);
begin
  dbaddpre_plm2 := mode;
end;

procedure setAddPretimeMode(mode: TAddPreMode);
begin
  dbaddpre_mode := mode;
end;

function GetPretimeMode: TPretimeLookupMOde;
begin
  Result := dbaddpre_plm1;
end;

function pretimeModeToString(mode: TPretimeLookupMOde): AnsiString;
begin
  case mode of
    plmNone: Result := 'None';
    plmHTTP: Result := 'HTTP';
    plmMYSQL: Result := 'MYSQL';
    plmSQLITE: Result := 'SQLite';
  end;
end;

function addPreModeToString(mode: TAddPreMode): AnsiString;
begin
  case mode of
    apmMem: Result := 'Memory';
    apmSQLITE: Result := 'SQLITE';
    apmMYSQL: Result := 'MYSQL';
    apmNone: Result := 'Skip';
  end;
end;

{ TDbAddPre }

constructor TDbAddPre.Create(rls: AnsiString; pretime: TDateTime);
begin
  self.rls := rls;
  self.pretime := pretime;
end;

destructor TDbAddPre.Destroy;
begin
  inherited;
end;

function GetPretimeURL: AnsiString;
begin
  Result := config.readString(section, 'url', '');
end;

function ReadPretimeOverHTTP(rls: AnsiString): TDateTime;
var
  response: TStringList;
  prex: TRegexpr;
  url: AnsiString;
  i: integer;
  read_count: integer;
begin
  Result := UnixToDateTime(0);
  if rls = '' then
    irc_adderror('No Releasename as parameter!');

  url := config_taskpretime_url;
  if url = '' then
  begin
    debug(dpSpam, section, 'URL value is empty');
    exit;
  end;

  response := TStringList.Create;
  response.Text := slUrlGet(Format(url, [rls]));
  debug(dpSpam, section, 'Pretime results for %s' + #13#10 + '%s', [rls, response.Text]);

  prex := TRegexpr.Create;
  try
  prex.ModifierM := True;
  prex.Expression := '(\S+) (\S+) (\S+) (\S+) (\S+)$';
  read_count := 0;

  for i := 0 to response.Count - 1 do
  begin
    Inc(read_count);
    if read_count > 500 then
    begin
      irc_addtext('CONSOLE', 'ADMIN', 'Read count higher then 500');
      Result := UnixToDateTime(0);
      break;
    end;
    if prex.Exec(response.strings[i]) then
    begin
      Debug(dpMessage, section, 'ReadPretimeOverHTTP : %s', [response.DelimitedText]);

      if (StrToIntDef(prex.Match[2], 0) <> 0) and
        (StrToIntDef(prex.Match[2], 0) <> 1295645417) then
      begin
        Result := UnixToDateTime(StrToIntDef(prex.Match[2], 0));
        if ((DaysBetween(Now(), Result) > 30) and
          config.ReadBool('kb', 'skip_rip_older_then_one_month', False)) then
        begin
          //        irc_addtext('CONSOLE','ADMIN','Days higher then 30 days');
          Result := UnixToDateTime(0);
        end;
      end
      else
      begin
        //      irc_addtext('CONSOLE','ADMIN','regex dosnot match');
        Result := UnixToDateTime(0);
      end;
    end;
  end;


  finally
    begin
    prex.Free;
    response.Free;
    end;
  end;
end;

function ReadPretimeOverSQLITE(rls: AnsiString): TDateTime;
var //time:int64;
  //    rlz_timestamp: String;
  i: integer;
begin
  Result := UnixToDateTime(0);
  if rls = '' then
    irc_adderror('No Releasename as parameter!');

  if addpreDB = nil then
    exit;
  if sql_gettime = nil then
    exit;

  try
    i := 0;
    addpreDB.Open(sql_gettime, [rls]);
    while addpreDB.Step(sql_gettime) do
    begin
      Inc(i);
      if i > 10 then
      begin
        Result := UnixToDateTime(0);
        exit;
      end;
      Result := UnixToDateTime(addpreDB.column_int64(sql_gettime, 0));
    end;
  except
    Result := UnixToDateTime(0);
  end;
end;

function ReadPretimeOverMYSQL(rls: AnsiString): TDateTime;
var
  q, mysql_result: AnsiString;
begin
  Result := UnixToDateTime(0);

  if mysqldb = nil then
  begin
    Debug(dpError, section, 'MySQL predb is not available! Check your mysql connections settings in slftp.ini.');
    exit;
  end;

  try
    mysql_lock.Enter;
    try
      q := Format('SELECT `%s` FROM `%s` WHERE `%s`',
        [SubString(config.ReadString('taskmysqlpretime', 'rlsdate_field', 'ts;3'),
          ';', 1), config.ReadString('taskmysqlpretime', 'tablename', 'addpre'),
        SubString(config.ReadString('taskmysqlpretime', 'rlsname_field', 'rlz;0'),
          ';', 1)]);
      q := q + ' = ''%s'';';
      mysql_result := gc(mysqldb, q, [rls]);
      if mysql_result <> '' then
        Result := UnixToDateTime(StrToIntDef(mysql_result, 0));
    finally
      mysql_lock.Leave;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] ReadPretimeOverMYSQL: %s', [e.Message]));
      exit;
    end;
  end;
end;

function getPretime(rlz: AnsiString): TPretimeResult;
begin
  Result.pretime := UnixToDateTime(0);
  Result.mode := 'None';
  if rlz = '' then
    irc_adderror('GETPRETIME --> No RLZ value!');

  case dbaddpre_plm1 of
    plmNone: Exit;
    plmHTTP: Result.pretime := ReadPretimeOverHTTP(rlz);
    plmMYSQL: Result.pretime := ReadPretimeOverMYSQL(rlz);
    plmSQLITE: Result.pretime := ReadPretimeOverSQLITE(rlz);
  else
    begin
      Debug(dpMessage, section, 'GetPretime unknown pretime mode : %d',
        [config.ReadInteger('taskpretime', 'mode', 0)]);
      Result.pretime := UnixToDateTime(0);
    end;
  end;

  if (result.pretime <> UnixToDateTime(0)) then
  begin
    Result.mode := pretimeModeToString(dbaddpre_plm1);
    Result.pretime := PrepareTimestamp(Result.pretime);
    exit;
  end;

  case dbaddpre_plm2 of
    plmNone: Exit;
    plmHTTP: Result.pretime := ReadPretimeOverHTTP(rlz);
    plmMYSQL: Result.pretime := ReadPretimeOverMYSQL(rlz);
    plmSQLITE: Result.pretime := ReadPretimeOverSQLITE(rlz);
  else
    begin
      Debug(dpMessage, section, 'GetPretime unknown pretime mode_2 : %d',
        [config.ReadInteger('taskpretime', 'mode_2', 0)]);
      Result.pretime := UnixToDateTime(0);
    end;
  end;
  if datetimetounix(Result.pretime) > 0 then
  begin
    Result.mode := pretimeModeToString(dbaddpre_plm2);
    Result.pretime := PrepareTimestamp(Result.pretime);
  end;

end;

function ReadPretime(rlz: AnsiString): TDateTime;
begin
  Result := UnixToDateTime(0);

  if rlz = '' then
    irc_adderror('GETPRETIME --> No RLZ value!');

  case dbaddpre_plm1 of
    plmNone: Exit;
    plmHTTP: Result := ReadPretimeOverHTTP(rlz);
    plmMYSQL: Result := ReadPretimeOverMYSQL(rlz);
    plmSQLITE: Result := ReadPretimeOverSQLITE(rlz);
  else
    begin
      Debug(dpMessage, section, 'GetPretime unknown pretime mode : %d',
        [config.ReadInteger('taskpretime', 'mode', 0)]);
      Result := UnixToDateTime(0);
    end;
  end;

  if ((Result = UnixToDateTime(0)) and (dbaddpre_plm2 <> plmNone)) then
  begin
    case dbaddpre_plm2 of
      plmNone: Exit;
      plmHTTP: Result := ReadPretimeOverHTTP(rlz);
      plmMYSQL: Result := ReadPretimeOverMYSQL(rlz);
      plmSQLITE: Result := ReadPretimeOverSQLITE(rlz);
    else
      begin
        Debug(dpMessage, section, 'GetPretime unknown pretime mode : %d',
          [config.ReadInteger('taskpretime', 'mode_2', 0)]);
        Result := UnixToDateTime(0);
      end;
    end;
  end;

  if (Result <> UnixToDateTime(0)) then
  begin
    Result := UnixToDateTime(PrepareTimestamp(DateTimeToUnix(Result)));
  end;
end;

function kb_Add_addpre(rls, section: AnsiString; event: AnsiString): integer;
var
  rls_section: AnsiString;
begin
  Result := -1;

  section := ProcessDoReplace(section);
  rls_section := '';
  rls_section := KibontasSection(' ' + section + ' ', '');
  rls_section := PrecatcherSectionMapping(rls, rls_section);
  if (rls_section = 'TRASH') then
  begin
    exit;
  end;

  if (rls_section = '') then
  begin
    irc_Addstats(Format('<c7>[ADDPRE]</c> %s %s (%s) : <b>No Sites</b>',
      [rls, rls_section, section]));
    exit;
  end;

  kb_Add('', '', config.ReadString('sites', 'admin_sitename', 'SLFTP'),
    rls_section, '', event, rls, '');
end;

function dbaddpre_ADDPRE(netname, channel, nickname: AnsiString; params: AnsiString;
  event: AnsiString): boolean;
var
  rls: AnsiString;
  rls_section: AnsiString;
begin
  rls := '';
  rls := SubString(params, ' ', 1);
  rls_section := '';
  rls_section := UpperCase(SubString(params, ' ', 2));
  if ((rls <> '') and (rls_section <> '') and (length(rls) > minimum_rlsname)) then
  begin

    if dbaddpre_mode <> apmNone then
      dbaddpre_InsertRlz(rls, rls_section, netname + '-' + channel + '-' + nickname);

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

function dbaddpre_GetRlz(rls: AnsiString): TDateTime;
var
  i: integer;
  addpredata: TDbAddPre;
  q, mysql_result: AnsiString;
begin
  Result := UnixToDateTime(0);

  // stor in memory
  if (dbaddpre_mode = apmMem) then
  begin
    try
      last_addpre_lock.Enter;
      try
        i := last_addpre.IndexOf(rls);
        if i = -1 then
        begin
          Result := UnixToDateTime(0);
          exit;
        end;
        addpredata := TDbAddPre(last_addpre.Objects[i]);
      finally
        last_addpre_lock.Leave;
      end;

      Result := addpredata.pretime;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] dbaddpre_GetRlz (memory): %s',
          [e.Message]));
        Result := UnixToDateTime(0);
        exit;
      end;
    end;
  end;

  // stor in sqlite
  if dbaddpre_mode = apmSQLITE then
  begin
    try
      i := 0;
      addpreDB.Open(sql_gettime, [rls]);
      while addpreDB.Step(sql_gettime) do
      begin
        Inc(i);
        if i > 10 then
        begin
          Result := UnixToDateTime(0);
          exit;
        end;
        Result := UnixToDateTime(addpreDB.column_int64(sql_gettime, 0));
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] dbaddpre_GetRlz (sqlite): %s',
          [e.Message]));
        Result := UnixToDateTime(0);
        exit;
      end;
    end;
  end;

  // stor in mysql
  if dbaddpre_mode = apmMYSQL then
  begin
    try
      mysql_lock.Enter;
      try
        q := Format('SELECT `%s` FROM `%s` WHERE `%s` ',
          [SubString(config.ReadString('taskmysqlpretime', 'rlsdate_field', 'section;1'),
            ';', 1), config.ReadString('taskmysqlpretime', 'tablename', 'addpre'),
          SubString(config.ReadString('taskmysqlpretime', 'rlsname_field', 'rlz;0'),
            ';', 1)]);

        q := q + ' = ''%s'';';
        mysql_result := gc(mysqldb, q, [rls]);
        if mysql_result <> '' then
          Result := UnixToDateTime(StrToIntDef(mysql_result, 0));
      finally
        mysql_lock.Leave;
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] dbaddpre_GetRlz (mysql): %s',
          [e.Message]));
        Result := UnixToDateTime(0);
        exit;
      end;
    end;
  end;
end;

function dbaddpre_InsertRlz(rls, rls_section, Source: AnsiString): boolean;
var
  i: integer;
  sql: AnsiString;
  pretime: TDateTime;
  addpredata: TDbAddPre;
begin
  Result := False;

  pretime := dbaddpre_GetRlz(rls);
  if pretime <> UnixToDateTime(0) then
    exit;

  //  if dbaddpre_mode = apmNone then

  case dbaddpre_mode of
    apmMem:
      begin
        try
          last_addpre_lock.Enter;
          try
            addpredata := TDbAddPre.Create(rls, Now());
            last_addpre.AddObject(rls, addpredata);

            i := last_addpre.Count;
            if i > 150 then
            begin
              while i > 100 do
              begin
                last_addpre.Delete(0);
                i := last_addpre.Count - 1;
              end;
            end;
          finally
            last_addpre_lock.Leave;
          end;
        except
          on e: Exception do
          begin
            Debug(dpError, section, Format('[EXCEPTION] dbaddpre_InsertRlz (memory): %s',
              [e.Message]));
            Result := False;
            exit;
          end;
        end;
      end;
    apmSQLITE:
      begin
        try
          pretime := dbaddpre_GetRlz(rls);
          if pretime <> UnixToDateTime(0) then
            exit;

          addpreDB.ExecSQL(sql_addrlz, [rls, rls_section, DateTimeToUnix(Now()), Source]);
        except
          on e: Exception do
          begin
            Debug(dpError, section, Format('[EXCEPTION] dbaddpre_InsertRlz (sqlite): %s',
              [e.Message]));
            Result := False;
            exit;
          end;
        end;
      end;
    apmMYSQL:
      begin
        try

          if config.ReadString('taskmysqlpretime', 'source_field', '-1') = '-1' then
          begin

            sql := Format('INSERT IGNORE INTO `%s` (`%s`, `%s`, `%s`) ',
              [config.ReadString('taskmysqlpretime', 'tablename', 'addpre'),
              SubString(config.ReadString('taskmysqlpretime', 'rlsname_field', 'rlz;0'),
                ';', 1), SubString(config.ReadString('taskmysqlpretime',
                'section_field', 'section;1'), ';', 1),
                SubString(config.ReadString('taskmysqlpretime', 'rlsdate_field', 'ts;3'),
                ';', 1)]);

            //        'INSERT IGNORE INTO addpre(rls, section, ts, source) VALUES (''%s'',''%s'', UNIX_TIMESTAMP(NOW()), ''%s'');';
            sql := sql + 'VALUES (''%s'',''%s'', UNIX_TIMESTAMP(NOW()));';
          end
          else
          begin
            sql := Format('INSERT IGNORE INTO %s (`%s`, `%s`, `%s`, `%s`) ',
              [config.ReadString('taskmysqlpretime', 'tablename', 'addpre'),
              SubString(config.ReadString('taskmysqlpretime', 'rlsname_field', 'rlz;0'),
                ';', 1), SubString(config.ReadString('taskmysqlpretime',
                'section_field', 'section;1'), ';', 1),
                SubString(config.ReadString('taskmysqlpretime', 'rlsdate_field', 'ts;3'),
                ';', 1), SubString(config.ReadString('taskmysqlpretime',
                'source_field', 'source;4'), ';', 1)]);

            //        'INSERT IGNORE INTO addpre(rls, section, ts, source) VALUES (''%s'',''%s'', UNIX_TIMESTAMP(NOW()), ''%s'');';
            sql := sql + 'VALUES (''%s'',''%s'', UNIX_TIMESTAMP(NOW()), ''%s'');';
          end;
          MySQLInsertQuery(sql, [rls, rls_section, Source]);
        except
          on e: Exception do
          begin
            Debug(dpError, section, Format('[EXCEPTION] dbaddpre_InsertRlz (mysql): %s',
              [e.Message]));
            Result := False;
            exit;
          end;
        end;

      end;
  end;
  Result := True;
end;

function dbaddpre_GetCount: integer;
var
  i: integer;
  q, res: AnsiString;
begin
  Result := 0;
  case dbaddpre_mode of
    apmMem: Result := last_addpre.Count;
    apmSQLITE:
      begin
        i := 0;
        addpreDB.Open(sql_countrlz);
        while addpreDB.Step(sql_countrlz) do
        begin
          Inc(i);
          if i > 10 then
          begin
            Result := 0;
            exit;
          end;
          Result := addpreDB.column_int(sql_countrlz, 0);
        end;
        exit;
      end;
    apmMYSQL:
      begin
        mysql_lock.Enter;
        try
          q := 'SELECT count(*) as count FROM ' + config.ReadString(
            'taskmysqlpretime', 'tablename', 'addpre') + ';';
          res := gc(mysqldb, q, []);
          Result := StrToIntDef(res, 0);
        finally
          mysql_lock.Leave;
        end;
      end;
  end;
end;

function dbaddpre_GetPreduration(rlz_pretime: TDateTime): AnsiString;
var
  preage: int64;
begin
  preage := DateTimeToUnix(Now) - DateTimeToUnix(rlz_pretime);
  if preage >= 604800 then
    Result := Format('%2.2d Weeks %1.1d Days %2.2d Hour %2.2d Min %2.2d Sec',
      [preage div 604800, (preage div 86400) mod 7, (preage div 3600) mod
      24, (preage div 60) mod 60, preage mod 60])
  else if preage >= 86400 then
    Result := Format('%1.1d Days %2.2d Hour %2.2d Min %2.2d Sec',
      [preage div 86400, (preage div 3600) mod 24, (preage div 60) mod
      60, preage mod 60])
  else if preage >= 3600 then
    Result := Format('%2.2d Hour %2.2d Min %2.2d Sec',
      [preage div 3600, (preage div 60) mod 60, preage mod 60])
  else if preage >= 60 then
    Result := Format('%2.2d Min %2.2d Sec', [(preage div 60) mod 60, preage mod 60])
  else
    Result := Format('%2.2d Sec', [preage mod 60]);
end;

function dbaddpre_Process(net, chan, nick, msg: AnsiString): boolean;
var
  ii: integer;
begin
  Result := False;
  ii := -1;
  try
    ii := addprecmd.IndexOf(substring(msg, ' ', 1));
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
        Debug(dpError, section, Format('[EXCEPTION] dbaddpre_Process: %s ',
          [e.Message]));
        dbaddpreUninit;
        dbaddpreinit;
      end;
    end;
  end;
end;

function dbaddpre_Status: AnsiString;
begin
  Result := '';
  Result := Format('<b>Dupe.db</b>: %d Rips, with %d in Memory', [dbaddpre_GetCount, last_addpre.Count]);
end;

procedure dbaddpreInit;
begin
  last_addpre_lock := TCriticalSection.Create;
  addprecmd := TStringList.Create;
  last_addpre := THashedStringList.Create;
  last_addpre.Duplicates := dupIgnore;
  last_addpre.CaseSensitive := False;
  last_addpre.Sorted := True;
end;

procedure dbaddpreStart;
var
  db_pre_name: AnsiString;
begin

  addprecmd.CommaText := config.ReadString(section, 'addprecmd', '!addpre');
  siteprecmd := config.ReadString(section, 'siteprecmd', '!sitepre');
  kbadd_addpre := config.ReadBool(section, 'kbadd_addpre', False);
  kbadd_sitepre := config.ReadBool(section, 'kbadd_sitepre', False);

  dbaddpre_mode := TAddPreMode(config.ReadInteger(section, 'mode', 3));
  dbaddpre_plm1 := TPretimeLookupMOde(config.ReadInteger('taskpretime', 'mode', 0));
  dbaddpre_plm2 := TPretimeLookupMOde(config.ReadInteger('taskpretime', 'mode_2', 0));

  config_taskpretime_url := config.readString('taskpretime', 'url', '');

  if slsqlite_inited and (dbaddpre_mode = apmSQLITE) then
  begin
    db_pre_name := Trim(config.ReadString(section, 'db_file', 'db_addpre.db'));

    // Addpre DB
    addpreDB := TslSqliteDB.Create(db_pre_name,
      config.ReadString(section, 'pragma', ''));
    addpreDB.ExecSQL(
      'CREATE TABLE IF NOT EXISTS addpre (rlz VARCHAR(255) NOT NULL, section VARCHAR(25) NOT NULL, ts INT(12) NOT NULL, source VARCHAR(255) NOT NULL)'
      );
    addpreDB.ExecSQL(
      'CREATE UNIQUE INDEX IF NOT EXISTS addpre_index ON addpre (rlz)'
      );

    sql_addrlz := addpreDB.Open(
      'INSERT OR IGNORE INTO addpre (rlz, section, ts, source) VALUES (?, ?, ?, ?)');
    sql_countrlz := addpreDB.Open('SELECT count(*) FROM addpre');
    sql_gettime := addpreDB.Open('SELECT ts FROM addpre WHERE rlz=?');
  end;

  case Integer(dbaddpre_mode) of
    0: Console_Addline('', 'Local addpre List Started...');
    1: Console_Addline('', 'Local SQLITE addpre DB Started...');
    2: Console_Addline('', 'Connected to MYSQL DupeDB...');
    //3: Exit;
  end;
end;

function AddPreDbAlive: boolean;
begin
  if addpreDB = nil then
    Result:=false
  else Result:=true;
end;

procedure dbaddpreUninit;
begin
  addprecmd.Free;
  last_addpre_lock.Free;
  last_addpre.Free;
  if addpreDB <> nil then
  begin
    addpreDB.Free;
    addpreDB := nil;
  end;
end;

end.

