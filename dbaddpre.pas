unit dbaddpre;

interface

uses Classes, IniFiles, irc, kb;

type
  TPretimeResult = record
    pretime: TDateTime;
    mode: String;
  end;

  TPretimeLookupMOde = (plmNone, plmHTTP, plmMYSQL, plmSQLITE);
  TAddPreMode = (apmMem, apmSQLITE, apmMYSQL, apmNone);

  TDbAddPre = class
    rls: String;
    pretime: TDateTime;
    constructor Create(const rls: String; const pretime: TDateTime);
    destructor Destroy; override;
  end;

function dbaddpre_ADDPRE(const netname, channel, nickname: String; const params: String; const event: String): boolean;
function dbaddpre_GetRlz(const rls: String): TDateTime;
function dbaddpre_InsertRlz(const rls, rls_section, Source: String): boolean;
function dbaddpre_GetCount: integer;
function dbaddpre_GetPreduration(const rlz_pretime: TDateTime): String;
function dbaddpre_Status: String;

function dbaddpre_Process(const net, chan, nick: String; msg: String): boolean;

procedure dbaddpreInit;
procedure dbaddpreStart;
procedure dbaddpreUnInit;

function getPretime(const rlz: String): TPretimeResult;

function ReadPretimeOverHTTP(const rls: String): TDateTime;
function ReadPretimeOverMYSQL(const rls: String): TDateTime;
function ReadPretimeOverSQLITE(const rls: String): TDateTime;

function GetPretimeMode: TPretimeLookupMOde;
function pretimeModeToString(mode: TPretimeLookupMOde): String;
function addPreModeToString(mode: TAddPreMode): String;

procedure setPretimeMode_One(mode: TPretimeLookupMOde);
procedure setPretimeMode_Two(mode: TPretimeLookupMOde);

procedure setAddPretimeMode(mode: TAddPreMode);

function AddPreDbAlive: boolean;

implementation

uses
  DateUtils, SysUtils, configunit, mystrings, console, sitesunit, regexpr,
  debugunit, precatcher, SyncObjs, taskpretime, dbhandler, SynDBSQLite3, SynDB, http;

const
  section = 'dbaddpre';

var
  addpreSQLite3DBCon: TSQLDBSQLite3ConnectionProperties = nil; //< SQLite3 database connection
  SQLite3Lock: TCriticalSection = nil; //< Critical Section used for read/write blocking as concurrently does not work flawless

  addprecmd: TStringList;
  siteprecmd: String;
  kbadd_addpre: boolean;
  kbadd_sitepre: boolean;

  last_addpre: THashedStringList;
  last_addpre_lock: TCriticalSection;

  dbaddpre_mode: TAddPreMode = TAddPreMode(3);
  dbaddpre_plm1: TPretimeLookupMOde;
  dbaddpre_plm2: TPretimeLookupMOde;

  config_taskpretime_url: String;

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

function pretimeModeToString(mode: TPretimeLookupMOde): String;
begin
  Result := 'None';

  case mode of
    plmNone: Result := 'None';
    plmHTTP: Result := 'HTTP';
    plmMYSQL: Result := 'MYSQL';
    plmSQLITE: Result := 'SQLite';
  end;
end;

function addPreModeToString(mode: TAddPreMode): String;
begin
  Result := 'Memory';

  case mode of
    apmMem: Result := 'Memory';
    apmSQLITE: Result := 'SQLITE';
    apmMYSQL: Result := 'MYSQL';
    apmNone: Result := 'Skip';
  end;
end;

{ TDbAddPre }

constructor TDbAddPre.Create(const rls: String; const pretime: TDateTime);
begin
  self.rls := rls;
  self.pretime := pretime;
end;

destructor TDbAddPre.Destroy;
begin
  inherited;
end;

function GetPretimeURL: String;
begin
  Result := config.readString(section, 'url', '');
end;

function ReadPretimeOverHTTP(const rls: String): TDateTime;
var
  response: TStringList;
  prex: TRegexpr;
  url: String;
  i: integer;
  read_count: integer;
  fHttpGetErrMsg: String;
  fStrHelper: String;
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
  prex := TRegexpr.Create;
  try
    prex.ModifierM := True;
    prex.Expression := '(\S+) (\S+) (\S+) (\S+) (\S+)$';
    read_count := 0;

    if not HttpGetUrl(Format(url, [rls]), fStrHelper, fHttpGetErrMsg) then
    begin
      Debug(dpError, section, Format('[FAILED] HTTP Pretime for %s --> %s ', [rls, fHttpGetErrMsg]));
      irc_Adderror(Format('<c4>[FAILED]</c> HTTP Pretime for %s --> %s', [rls, fHttpGetErrMsg]));
      exit;
    end;

    response.Text := fStrHelper;

    Debug(dpSpam, section, 'Pretime results for %s' + #13#10 + '%s', [rls, response.Text]);

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
    prex.Free;
    response.Free;
  end;
end;

function ReadPretimeOverSQLITE(const rls: String): TDateTime;
var
  fQuery: TQuery;
begin
  Result := UnixToDateTime(0);
  if rls = '' then
    irc_adderror('No Releasename as parameter!');

  SQLite3Lock.Enter;
  try
    fQuery := TQuery.Create(addpreSQLite3DBCon.ThreadSafeConnection);
    try
      fQuery.SQL.Text := 'SELECT ts FROM addpre WHERE rlz = :release';
      fQuery.ParamByName('release').AsString := rls;
      try
        fQuery.Open;
        if not fQuery.IsEmpty then
          Result := UnixToDateTime(fQuery.FieldByName('ts').AsInteger);
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[EXCEPTION] ReadPretimeOverSQLITE: %s', [e.Message]));
          exit;
        end;
      end;
    finally
      fQuery.free;
    end;
  finally
    SQLite3Lock.Leave;
  end;
end;

function ReadPretimeOverMYSQL(const rls: String): TDateTime;
var
  fQuery: TQuery;
  fTimeField, fTableName, fReleaseField: String;
begin
  Result := UnixToDateTime(0);
  if rls = '' then
    irc_adderror('No Releasename as parameter!');

  fTimeField := config.ReadString('taskmysqlpretime', 'rlsdate_field', 'ts');
  fTableName := config.ReadString('taskmysqlpretime', 'tablename', 'addpre');
  fReleaseField := config.ReadString('taskmysqlpretime', 'rlsname_field', 'rls');

  fQuery := TQuery.Create(MySQLCon.ThreadSafeConnection);
  try
    fQuery.SQL.Text := 'SELECT ' + fTimeField + ' FROM ' + fTableName + ' WHERE ' + fReleaseField + ' = :release';
    fQuery.ParamByName('release').AsString := rls;
    try
      fQuery.Open;
      if not fQuery.IsEmpty then
        Result := UnixToDateTime(fQuery.FieldByName(fTimeField).AsInteger);
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] ReadPretimeOverMYSQL: %s', [e.Message]));
        exit;
      end;
    end;
  finally
    fQuery.free;
  end;
end;

function getPretime(const rlz: String): TPretimeResult;
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

function kb_Add_addpre(rls, section: String; event: String): integer;
var
  rls_section: String;
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

  kb_Add('', '', getAdminSiteName, rls_section, '', event, rls, '');
end;

function dbaddpre_ADDPRE(const netname, channel, nickname: String; const params: String; const event: String): boolean;
var
  rls: String;
  rls_section: String;
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

function dbaddpre_GetRlz(const rls: String): TDateTime;
var
  i: integer;
  addpredata: TDbAddPre;
begin
  Result := UnixToDateTime(0);

  case dbaddpre_mode of
    apmMem:
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
            Debug(dpError, section, Format('[EXCEPTION] dbaddpre_GetRlz (memory): %s', [e.Message]));
            Result := UnixToDateTime(0);
            exit;
          end;
        end;
      end;
    apmSQLITE:
      begin
        Result := ReadPretimeOverSQLITE(rls);
      end;
    apmMYSQL:
      begin
        Result := ReadPretimeOverMYSQL(rls);
      end;
  end;
end;

function dbaddpre_InsertRlz(const rls, rls_section, Source: String): boolean;
var
  i: integer;
  sql: String;
  pretime: TDateTime;
  addpredata: TDbAddPre;
  fQuery: TQuery;
  fTableName, fReleaseField, fSectionField, fTimeField, fSourceField: String;
begin
  Result := False;

  pretime := dbaddpre_GetRlz(rls);
  if pretime <> UnixToDateTime(0) then
    exit;

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
            Debug(dpError, section, Format('[EXCEPTION] dbaddpre_InsertRlz (memory): %s', [e.Message]));
            Result := False;
            exit;
          end;
        end;
      end;
    apmSQLITE:
      begin
        SQLite3Lock.Enter;
        try
          fQuery := TQuery.Create(addpreSQLite3DBCon.ThreadSafeConnection);
          try
            fQuery.SQL.Text := 'INSERT OR IGNORE INTO addpre (rlz, section, ts, source) VALUES (:release, :section, :timestamp, :source)';
            fQuery.ParamByName('release').AsString := rls;
            fQuery.ParamByName('section').AsString := rls_section;
            fQuery.ParamByName('timestamp').AsInteger := DateTimeToUnix(Now());
            fQuery.ParamByName('source').AsString := Source;
            try
              fQuery.ExecSQL;
            except
              on e: Exception do
              begin
                Debug(dpError, section, Format('[EXCEPTION] dbaddpre_InsertRlz (sqlite): %s - values: %s %s %s', [e.Message, rls, rls_section, Source]));
                Result := False;
                exit;
              end;
            end;
          finally
            fQuery.free;
          end;
        finally
          SQLite3Lock.Leave;
        end;
      end;
    apmMYSQL:
      begin
        fQuery := TQuery.Create(MySQLCon.ThreadSafeConnection);
        try
          fTableName := config.ReadString('taskmysqlpretime', 'tablename', 'addpre');
          fReleaseField := config.ReadString('taskmysqlpretime', 'rlsname_field', 'rls');
          fSectionField := config.ReadString('taskmysqlpretime', 'section_field', 'section');
          fTimeField := config.ReadString('taskmysqlpretime', 'rlsdate_field', 'ts');
          fSourceField := config.ReadString('taskmysqlpretime', 'source_field', '-1');

          if fSourceField = '-1' then
          begin
            fQuery.SQL.Text := 'INSERT IGNORE INTO ' + fTableName + ' (' + fReleaseField + ', ' + fSectionField + ', ' + fTimeField + ') VALUES (:release, :section, :timestamp);';
          end
          else
          begin
            fQuery.SQL.Text := 'INSERT IGNORE INTO ' + fTableName + ' (' + fReleaseField + ', ' + fSectionField + ', ' + fTimeField + ', ' + fSourceField + ') VALUES (:release, :section, :timestamp, :source);';
            fQuery.ParamByName('source').AsString := Source;
          end;

          fQuery.ParamByName('release').AsString := rls;
          fQuery.ParamByName('section').AsString := rls_section;
          fQuery.ParamByName('timestamp').AsInteger := DateTimeToUnix(Now());
          try
            fQuery.ExecSQL;
          except
            on e: Exception do
            begin
              Debug(dpError, section, Format('[EXCEPTION] dbaddpre_InsertRlz (mysql): %s - values: %s %s %s', [e.Message, rls, rls_section, Source]));
              Result := False;
              exit;
            end;
          end;
        finally
          fQuery.free;
        end;
      end;
  end;

  Result := True;
end;

function dbaddpre_GetCount: integer;
var
  fQuery: TQuery;
  fTableName: String;
begin
  Result := 0;
  case dbaddpre_mode of
    apmMem:
      begin
        Result := last_addpre.Count;
      end;
    apmSQLITE:
      begin
        SQLite3Lock.Enter;
        try
          fQuery := TQuery.Create(addpreSQLite3DBCon.ThreadSafeConnection);
          try
            fQuery.SQL.Text := 'SELECT count(*) FROM addpre';
            fQuery.Open;

            if fQuery.IsEmpty then
              Result := 0
            else
              Result := fQuery.Fields[0].AsInteger;

          finally
            fQuery.Free;
          end;
        finally
          SQLite3Lock.Leave;
        end;
      end;
    apmMYSQL:
      begin
          fQuery := TQuery.Create(MySQLCon.ThreadSafeConnection);
          try
            fTableName := config.ReadString('taskmysqlpretime', 'tablename', 'addpre');
            fQuery.SQL.Text := 'SELECT count(*) FROM ' + fTableName;
            fQuery.Open;

            if fQuery.IsEmpty then
              Result := 0
            else
              Result := fQuery.Fields[0].AsInteger;

          finally
            fQuery.Free;
          end;
      end;
  end;
end;

function dbaddpre_GetPreduration(const rlz_pretime: TDateTime): String;
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

function dbaddpre_Process(const net, chan, nick: String; msg: String): boolean;
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

function dbaddpre_Status: String;
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
  db_pre_name: String;
begin
  addprecmd.CommaText := config.ReadString(section, 'addprecmd', '!addpre');
  siteprecmd := config.ReadString(section, 'siteprecmd', '!sitepre');
  kbadd_addpre := config.ReadBool(section, 'kbadd_addpre', False);
  kbadd_sitepre := config.ReadBool(section, 'kbadd_sitepre', False);

  dbaddpre_mode := TAddPreMode(config.ReadInteger(section, 'mode', 3));
  dbaddpre_plm1 := TPretimeLookupMOde(config.ReadInteger('taskpretime', 'mode', 0));
  dbaddpre_plm2 := TPretimeLookupMOde(config.ReadInteger('taskpretime', 'mode_2', 0));

  config_taskpretime_url := config.readString('taskpretime', 'url', '');

  if ( (dbaddpre_mode = apmSQLITE) or (dbaddpre_plm1 = plmSQLITE) or (dbaddpre_plm2 = plmSQLITE) ) then
  begin
    SQLite3Lock := TCriticalSection.Create;
    db_pre_name := Trim(config.ReadString(section, 'db_file', 'db_addpre.db'));

    try
      addpreSQLite3DBCon := CreateSQLite3DbConn(db_pre_name, '');

      addpreSQLite3DBCon.MainSQLite3DB.Execute(
        'CREATE TABLE IF NOT EXISTS addpre (rlz VARCHAR(255) NOT NULL, section VARCHAR(25) NOT NULL, ts INT(12) NOT NULL, source VARCHAR(255) NOT NULL)'
      );
      addpreSQLite3DBCon.MainSQLite3DB.Execute(
        'CREATE UNIQUE INDEX IF NOT EXISTS addpre_index ON addpre (rlz)'
      );
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] dbaddpreStart: %s ',[e.Message]));
        exit;
      end;
    end;
  end;

  case Integer(dbaddpre_mode) of
    0: Console_Addline('', 'Memory PreDB started...');
    1: Console_Addline('', 'SQLite PreDB started...');
    2: Console_Addline('', 'MySQL/Maria PreDB started...');
    //3: Exit;
  end;
end;

function AddPreDbAlive: boolean;
begin
  if addpreSQLite3DBCon = nil then
    Result := false
  else
    Result := true;
end;

procedure dbaddpreUninit;
begin
  Debug(dpSpam, section, 'Uninit1');
  addprecmd.Free;
  last_addpre_lock.Free;
  last_addpre.Free;

  if Assigned(SQLite3Lock) then
  begin
    FreeAndNil(SQLite3Lock);
  end;

  if Assigned(addpreSQLite3DBCon) then
  begin
    FreeAndNil(addpreSQLite3DBCon);
  end;
  Debug(dpSpam, section, 'Uninit2');
end;

end.

