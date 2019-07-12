unit dbaddpre;

interface

uses
  Classes, kb;

type
  TPretimeResult = record
    pretime: Int64; //< UTC pretime
    mode: String; //< method from @link(TPretimeLookupMode) which was used to get pretime
  end;

  {
  @value(plmNone no saving and no lookup of pretimes)
  @value(plmHTTP read pretime over HTTP)
  @value(plmMYSQL read pretime from MySQL/MariaDB)
  @value(plmSQLITE read pretime from local SQLite database)
  }
  TPretimeLookupMode = (plmNone, plmHTTP, plmMYSQL, plmSQLITE);
  {
  @value(apmMemory uses a run-time filled list of pretimes)
  @value(apmSQLITE SQLite database)
  @value(apmMYSQL MySQL/MariaDB)
  @value(apmNone no saving and no lookup)
  }
  TAddPreMode = (apmMemory, apmSQLITE, apmMYSQL, apmNone);

  TDbAddPre = class
    rls: String;
    pretime: Int64;
    constructor Create(const rls: String; const pretime: Int64);
    destructor Destroy; override;
  end;

function dbaddpre_ADDPRE(const netname, channel, nickname, params: String; event: TKBEventType): boolean;
function dbaddpre_GetRlz(const rls: String): Int64;
function dbaddpre_InsertRlz(const rls, rls_section, Source: String): boolean;
function dbaddpre_GetCount: integer;
function dbaddpre_GetPreduration(const rlz_pretime: Int64): String;
function dbaddpre_Status: String;

function dbaddpre_Process(const net, chan, nick: String; msg: String): boolean;

procedure dbaddpreInit;
procedure dbaddpreStart;
procedure dbaddpreUnInit;

function getPretime(const rlz: String): TPretimeResult;

function ReadPretimeOverHTTP(const rls: String): Int64;
function ReadPretimeOverMYSQL(const rls: String): Int64;
function ReadPretimeOverSQLITE(const rls: String): Int64;

function GetPretimeMode: TPretimeLookupMode;
{ Convert Pretime Lookup Mode to String
  @param(aPretimeLookupMode Pretime mode from @link(TPretimeLookupMode))
  @returns(Pretime mode as String without prefix) }
function pretimeModeToString(aPretimeLookupMode: TPretimeLookupMode): String;
{ Convert Addpre Mode to String
  @param(aAddPreMode Addpre mode from @link(TAddPreMode))
  @returns(Addpre mode as String without prefix) }
function addPreModeToString(aAddPreMode: TAddPreMode): String;

procedure setPretimeMode_One(mode: TPretimeLookupMode);
procedure setPretimeMode_Two(mode: TPretimeLookupMode);

procedure setAddPretimeMode(mode: TAddPreMode);

function AddPreDbAlive: boolean;

implementation

uses
  DateUtils, SysUtils, StrUtils, configunit, mystrings, console, sitesunit, regexpr, IniFiles,
  irc, debugunit, precatcher, SyncObjs, taskpretime, dbhandler, SynDBSQLite3, SynDB, http;

const
  section = 'dbaddpre';

var
  addpreSQLite3DBCon: TSQLDBSQLite3ConnectionProperties = nil; //< SQLite3 database connection
  SQLite3Lock: TCriticalSection = nil; //< Critical Section used for read/write blocking as concurrently does not work flawless

  addprecmd: TStringList;
  kbadd_addpre: boolean;

  last_addpre: THashedStringList;
  last_addpre_lock: TCriticalSection;

  dbaddpre_mode: TAddPreMode = TAddPreMode(3);
  dbaddpre_plm1: TPretimeLookupMode;
  dbaddpre_plm2: TPretimeLookupMode;

  config_taskpretime_url: String;

procedure setPretimeMode_One(mode: TPretimeLookupMode);
begin
  dbaddpre_plm1 := mode;
end;

procedure setPretimeMode_Two(mode: TPretimeLookupMode);
begin
  dbaddpre_plm2 := mode;
end;

procedure setAddPretimeMode(mode: TAddPreMode);
begin
  dbaddpre_mode := mode;
end;

function GetPretimeMode: TPretimeLookupMode;
begin
  Result := dbaddpre_plm1;
end;

function pretimeModeToString(aPretimeLookupMode: TPretimeLookupMode): String;
begin
  Result := ReplaceText(TEnum<TPretimeLookupMode>.ToString(aPretimeLookupMode), 'plm', '');
end;

function addPreModeToString(aAddPreMode: TAddPreMode): String;
begin
  Result := ReplaceText(TEnum<TAddPreMode>.ToString(aAddPreMode), 'apm', '');
end;

{ TDbAddPre }

constructor TDbAddPre.Create(const rls: String; const pretime: Int64);
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

function ReadPretimeOverHTTP(const rls: String): Int64;
var
  response: TStringList;
  prex: TRegexpr;
  url: String;
  i: integer;
  read_count: integer;
  fHttpGetErrMsg: String;
  fStrHelper: String;
begin
  Result := 0;
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
        Result := 0;
        break;
      end;
      if prex.Exec(response.strings[i]) then
      begin
        Debug(dpMessage, section, 'ReadPretimeOverHTTP : %s', [response.DelimitedText]);

        if (StrToIntDef(prex.Match[2], 0) <> 0) then
        begin
          Result := StrToIntDef(prex.Match[2], 0);
          if ((DaysBetween(Now(), UnixToDateTime(Result, False)) > 30) and
            config.ReadBool('kb', 'skip_rip_older_then_one_month', False)) then
          begin
            //        irc_addtext('CONSOLE','ADMIN','Days higher then 30 days');
            Result := 0;
          end;
        end
        else
        begin
          //      irc_addtext('CONSOLE','ADMIN','regex dosnot match');
          Result := 0;
        end;
      end;
    end;
  finally
    prex.Free;
    response.Free;
  end;
end;

function ReadPretimeOverSQLITE(const rls: String): Int64;
var
  fQuery: TQuery;
begin
  Result := 0;
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
          Result := fQuery.FieldByName('ts').AsInt64;
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

function ReadPretimeOverMYSQL(const rls: String): Int64;
var
  fQuery: TQuery;
  fTimeField, fTableName, fReleaseField: String;
begin
  Result := 0;
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
        Result := fQuery.FieldByName(fTimeField).AsInt64;
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
  Result.pretime := 0;
  Result.mode := pretimeModeToString(plmNone);
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
      Result.pretime := 0;
    end;
  end;

  if (Result.pretime > 0) then
  begin
    Result.mode := pretimeModeToString(dbaddpre_plm1);
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
      Result.pretime := 0;
    end;
  end;

  if Result.pretime > 0 then
  begin
    Result.mode := pretimeModeToString(dbaddpre_plm2);
  end;
end;

function kb_Add_addpre(const rls, section: String; event: TKBEventType): integer;
var
  rls_section: String;
  fSection: String;
begin
  Result := -1;

  fSection := ProcessDoReplace(section);
  rls_section := '';
  rls_section := KibontasSection(' ' + fSection + ' ', '');
  rls_section := PrecatcherSectionMapping(rls, rls_section);

  if (rls_section = 'TRASH') then
  begin
    exit;
  end;

  if (rls_section = '') then
  begin
    irc_Addstats(Format('<c7>[ADDPRE]</c> %s %s (%s) : <b>No Sites</b>', [rls, rls_section, fSection]));
    exit;
  end;

  Result := kb_Add('', '', getAdminSiteName, rls_section, '', event, rls, '');
end;

function dbaddpre_ADDPRE(const netname, channel, nickname, params: String; event: TKBEventType): boolean;
var
  rls: String;
  rls_section: String;
  kb_entry: String;
  p: Integer;
begin
  Result := False;

  rls := '';
  rls := SubString(params, ' ', 1);
  if ((rls <> '') and (length(rls) > minimum_rlsname)) then
  begin
    if dbaddpre_mode <> apmNone then
      dbaddpre_InsertRlz(rls, '', netname + '-' + channel + '-' + nickname);

    if ((event = kbeADDPRE) and (kbadd_addpre)) then
    begin
      kb_entry := FindReleaseInKbList('-' + rls);

      // TODO: might not work correctly if sections are TV-SD, TV-720P-FR, etc
      // introduced with merge-req #315
      if kb_entry <> '' then
      begin
        p := Pos('-', kb_entry);
        rls_section := Copy(kb_entry, 1, p - 1);
        if rls_section <> '' then
          kb_Add_addpre(rls, rls_section, event);
      end;
    end;
  end;

  Result := True;
end;

function dbaddpre_GetRlz(const rls: String): Int64;
var
  i: integer;
  addpredata: TDbAddPre;
begin
  Result := 0;

  case dbaddpre_mode of
    apmMemory:
      begin
        try
          last_addpre_lock.Enter;
          try
            i := last_addpre.IndexOf(rls);
            if i = -1 then
            begin
              Result := 0;
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
            Result := 0;
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
  pretime: Int64;
  addpredata: TDbAddPre;
  fQuery: TQuery;
  fTableName, fReleaseField, fSectionField, fTimeField, fSourceField: String;
begin
  Result := False;

  pretime := dbaddpre_GetRlz(rls);
  if pretime <> 0 then
    exit;

  case dbaddpre_mode of
    apmMemory:
      begin
        try
          last_addpre_lock.Enter;
          try
            addpredata := TDbAddPre.Create(rls, DateTimeToUnix(Now(), False));
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
            fQuery.ParamByName('timestamp').AsInt64 := DateTimeToUnix(Now(), False);
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
          fQuery.ParamByName('timestamp').AsInt64 := DateTimeToUnix(Now(), False);
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
    apmMemory:
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

function dbaddpre_GetPreduration(const rlz_pretime: Int64): String;
var
  preage: int64;
begin
  preage := DateTimeToUnix(Now(), False) - rlz_pretime;
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
        dbaddpre_ADDPRE(net, chan, nick, msg, kbeADDPRE);
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
  kbadd_addpre := config.ReadBool(section, 'kbadd_addpre', False);

  dbaddpre_mode := TAddPreMode(config.ReadInteger(section, 'mode', 3));
  dbaddpre_plm1 := TPretimeLookupMode(config.ReadInteger('taskpretime', 'mode', 0));
  dbaddpre_plm2 := TPretimeLookupMode(config.ReadInteger('taskpretime', 'mode_2', 0));

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

