unit dbaddpre;

interface

uses
  Classes, kb, kb.releaseinfo, mormot.orm.core, mormot.core.base, mormot.orm.base,
  mormot.rest.server, mormot.rest.client;

type
  TPretimeResult = record
    pretime: Int64; //< UTC pretime
    mode: String; //< method from @link(TPretimeLookupMode) which was used to get pretime
  end;

type
  TSQLAddPreRecord = class(TOrm)
  private
    FReleaseName: RawUTF8; //< releasename
    FSection: RawUTF8; //< filename
    FTimeStamp: Int64; //< filesize
    FSource: RawUTF8; //< creation time of the entry
  published
    property rlz: RawUTF8 read FReleaseName write FReleaseName stored AS_UNIQUE;
    property section: RawUTF8 read FSection write FSection;
    property ts: Int64 read FTimeStamp write FTimeStamp;
    property source: RawUTF8 read FSource write FSource;
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

function dbaddpre_ADDPRE(const netname, channel, nickname, aRlsName, aSection, params: String; event: TKBEventType): boolean;
function dbaddpre_InsertRlz(const rls, rls_section, Source: String; const aSkipDbCleanup: boolean = False): boolean;
function dbaddpre_GetCount: integer;
{Shows number of lines in AddPre Database
@Returns(Integer) with number of entries}
function dbaddpre_GetPreduration(const rlz_pretime: Int64): String;
function dbaddpre_Status: String;

function dbaddpre_Process(const net, chan, nick, msg: String): boolean;

procedure dbaddpreStart;
procedure dbaddpreUnInit;

function getPretime(const rlz: String): TPretimeResult;

function ReadPretimeOverHTTP(const rls: String): Int64;
function ReadPretime(const rls: String): Int64;

function GetPretimeMode: TPretimeLookupMode;
{ Convert Pretime Lookup Mode to String
  @param(aPretimeLookupMode Pretime mode from @link(TPretimeLookupMode))
  @returns(Pretime mode as String without prefix) }
function GetPretimeMode_2: TPretimeLookupMode;
{ Convert Pretime Lookup Mode to String
  @param(aPretimeLookupMode Pretime mode from @link(TPretimeLookupMode))
  @returns(Pretime mode as String without prefix from backup solution) }
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
  DateUtils, SysUtils, StrUtils, configunit, mystrings, console, sitesunit, FLRE, IniFiles,
  irc, debugunit, precatcher, SyncObjs, taskpretime, dbhandler, http,
  mormot.core.text,
  mormot.core.unicode,
  mormot.core.json,
  mormot.core.variants,
  mormot.core.os,
  mormot.orm.sql,
  mormot.rest.sqlite3,
  mormot.db.core,
  mormot.db.sql,
  mormot.db.sql.sqlite3,
  mormot.db.raw.sqlite3,
  mormot.db.raw.sqlite3.static,
  mormot.db.sql.zeos,
  ZPlainMySqlDriver;

const
  section = 'dbaddpre';
  DBCLEANUP_INTERVAL = 50;
  DBCLEANUP_NUM_ENTRIES_TO_KEEP = 300;

var
  ORMAddPreDBSqLite: TRestClientDb; //< Rest Client for all database interactions
  ORMAddPreModel: TSQLModel; //< SQL ORM model for stats database
  ORMAddPreDBMysql: TRestClientDb;

  addprecmd: TStringList;
  kbadd_addpre: boolean;
  add_to_kb_on_dbaddpre_insert: boolean;

  dbaddpre_mode: TAddPreMode = TAddPreMode(3);
  dbaddpre_plm1: TPretimeLookupMode;
  dbaddpre_plm2: TPretimeLookupMode;

  config_taskpretime_url: String;
  config_taskpretime_regexp: RawByteString;
  FDbCleanupCounter: integer;
  fLastRowId: integer;

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

function GetPretimeMode_2: TPretimeLookupMode;
begin
  Result := dbaddpre_plm2;
end;

function pretimeModeToString(aPretimeLookupMode: TPretimeLookupMode): String;
begin
  Result := ReplaceText(TEnum<TPretimeLookupMode>.ToString(aPretimeLookupMode), 'plm', '');
end;

function addPreModeToString(aAddPreMode: TAddPreMode): String;
begin
  Result := ReplaceText(TEnum<TAddPreMode>.ToString(aAddPreMode), 'apm', '');
end;

function GetPretimeURL: String;
begin
  Result := config.readString(section, 'url', '');
end;

function ReadPretimeOverHTTP(const rls: String): Int64;
var
  response: String;
  rx_pretime: TFLRE;
  rx_captures: TFLREMultiCaptures;
  url: String;
  aPretimePos: integer;
  aPreTimeStr: String;
  fHttpGetErrMsg: String;
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

  try
    rx_pretime := TFLRE.Create(config_taskpretime_regexp, []);

    if not HttpGetUrl(Format(url, [rls]), response, fHttpGetErrMsg) then
    begin
      Debug(dpError, section, Format('[FAILED] HTTP Pretime for %s --> %s ', [rls, fHttpGetErrMsg]));
      irc_Adderror(Format('<c4>[FAILED]</c> HTTP Pretime for %s --> %s', [rls, fHttpGetErrMsg]));
      exit;
    end;

    Debug(dpSpam, section, 'Pretime results for %s' + #13#10 + '%s', [rls, response]);
    if rx_pretime.MatchAll(RawByteString(response), rx_captures, 1, 1) then
    begin
      Debug(dpMessage, section, 'ReadPretimeOverHTTP : %s', [response]);
      aPretimePos := rx_pretime.NamedGroupIndices['pretime'];
      if aPretimePos < 0 then
      begin
        irc_addtext('CONSOLE','ADMIN','named capture group: pretime not found');
        exit;
      end;
      aPreTimeStr := Copy(response, rx_captures[0][aPretimePos].Start, rx_captures[0][aPretimePos].Length);
      if (aPretimePos >= 0) and (StrToIntDef(aPreTimeStr, 0) <> 0) then
      begin
        Result := StrToIntDef(aPreTimeStr, 0);
        if ((DaysBetween(Now(), UnixToDateTime(Result, False)) > 30) and
          config.ReadBool('kb', 'skip_rip_older_then_one_month', False)) then
        begin
          irc_addtext('CONSOLE','ADMIN','Days higher then 30 days');
          Result := 0;
        end;
      end
      else
      begin
        irc_addtext('CONSOLE','ADMIN','regex does not match');
        Result := 0;
      end;
    end;
  finally
    SetLength(rx_captures, 0);
    rx_pretime.Free;
  end;
end;

function ReadPretime(const rls: String): Int64;
var
  fAddPreRec: TSQLAddPreRecord;
  fTmpOrmAddPreDb: TRestClientDb;
begin
  Result := 0;
  if rls = '' then
  begin
    irc_adderror('No Releasename as parameter!');
    Exit;
  end;

  fTmpOrmAddPreDb := nil;
  if ((dbaddpre_mode = apmSQLITE) OR (dbaddpre_mode = apmMemory)) then
    fTmpOrmAddPreDb := ORMAddPreDBSqLite;
  if (dbaddpre_mode = apmMYSQL) then
    fTmpOrmAddPreDb := ORMAddPreDBMysql;

  if not Assigned(fTmpOrmAddPreDb) then
    Exit;

  fAddPreRec := TSQLAddPreRecord.CreateAndFillPrepare(fTmpOrmAddPreDb.Orm, 'rlz = ?',[], [rls]);
  try
    while fAddPreRec.FillOne do
    begin
      Result := fAddPreRec.ts;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] ReadPretime: %s', [e.Message]));
    end;
  end;
  fAddPreRec.Free;
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
    plmMYSQL, plmSQLITE: Result.pretime := ReadPretime(rlz);
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
    plmMYSQL, plmSQLITE: Result.pretime := ReadPretime(rlz);
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
  rls_section := FindSection(' ' + fSection + ' ');
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

function dbaddpre_ADDPRE(const netname, channel, nickname, aRlsName, aSection, params: String; event: TKBEventType): boolean;
var
  rls: String;
  rls_section: String;
  kb_entry: String;
  p: Integer;

  function IsUDPEnabled: Boolean;
  var
    rawEnable: String;
    udpIp: String;
    udpPort: Integer;
  begin
    rawEnable := Trim(config.ReadString('UDPConfig', 'EnableUDP', 'False'));
    udpIp := Trim(config.ReadString('UDPConfig', 'IP', ''));
    udpPort := config.ReadInteger('UDPConfig', 'Port', 0);
    Result := (SameText(rawEnable, 'True') or SameText(rawEnable, '1')) and
      (udpIp <> '') and (udpPort >= 1) and (udpPort <= 65535);
  end;
begin
  Result := False;

  rls := '';
  rls := SubString(aRlsName, ' ', 1);

  if ((rls <> '') and (length(rls) > minimum_rlsname)) then
  begin
    if dbaddpre_mode <> apmNone then
    begin
      if dbaddpre_InsertRlz(rls, '', netname + '-' + channel + '-' + nickname) then
      begin
        // we just inserted the pre time, find out if there's already a KB entry
        rls_section := FindReleaseInLatestKBList(rls);

        //send event to kb_add to trigger race evaluation
        if rls_section <> '' then
          kb_Add(netname, channel, getAdminSiteName, rls_section, '', event, rls, '')
        else if add_to_kb_on_dbaddpre_insert or IsUDPEnabled then
        begin
          rls_section := aSection;  // if the precatcher config has a fixed section ... I don't think it makes much sense, but it's possible
          if rls_section = '' then
          begin
            rls_section := ProcessDoReplace(params, rls);
            rls_section := FindSection(' ' + rls_section + ' ');
          end;

          rls_section := PrecatcherSectionMapping(rls, rls_section);
          kb_Add(netname, channel, getAdminSiteName, rls_section, '', event, rls, '');
        end;
      end;
    end;

    if ((event = kbeADDPRE) and (kbadd_addpre)) then // I assume this does the same as the code just above (issue a kb event after the pre time is there)
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

function dbaddpre_InsertRlz(const rls, rls_section, Source: String; const aSkipDbCleanup: boolean = False): boolean;
var
  fAddPreRec: TSQLAddPreRecord;
  fTmpOrmAddPreDb: TRestClientDb;
begin
  Result := False;

  if ((dbaddpre_mode = apmSQLITE) OR (dbaddpre_mode = apmMemory)) then
    fTmpOrmAddPreDb := ORMAddPreDBSqLite;
  if (dbaddpre_mode = apmMYSQL) then
    fTmpOrmAddPreDb := ORMAddPreDBMysql;

  case dbaddpre_mode of
    apmMemory, apmSQLITE, apmMYSQL:
      begin
        if not Assigned(fTmpOrmAddPreDb) then
          Exit;

        fAddPreRec := TSQLAddPreRecord.CreateAndFillPrepare(fTmpOrmAddPreDb.Orm, 'rlz = ?', [rls], 'ID');
        try
          if not fAddPreRec.FillOne then
          begin
            fAddPreRec.rlz := StringToUTF8(rls);
            fAddPreRec.section := StringToUTF8(rls_section);
            fAddPreRec.ts := DateTimeToUnix(Now(), False);
            fAddPreRec.source := StringToUTF8(Source);

            if fTmpOrmAddPreDb.Add(fAddPreRec, True, False) = 0 then
            begin
              Debug(dpError, section, Format('[EXCEPTION] dbaddpre_InsertRlz: values: %s %s %s', [rls, rls_section, Source]));
              Exit;
            end;
          end;
        finally
          fAddPreRec.Free;
        end;
      end;
  end;

  // db cleanup currently only for in-memory DB
  if Result and (dbaddpre_mode = apmMemory) then
  begin
    FDbCleanupCounter := FDbCleanupCounter + 1;

    try
      // get the last row id
      fLastRowId := fTmpOrmAddPreDb.Db.LastInsertRowID;
      if dbaddpre_GetCount > DBCLEANUP_NUM_ENTRIES_TO_KEEP then
      begin
        if not (fTmpOrmAddPreDb.Delete(TSQLAddPreRecord, 'ID < ?', [fLastRowId - DBCLEANUP_NUM_ENTRIES_TO_KEEP])) then
        begin
          Debug(dpError, section, '[RemoveStats] Could not remove with timestamp %d!', [DateTimeToUnix(Yesterday)]);
          exit;
        end
        else
        begin
          FDbCleanupCounter := 0;
        end;
      end;
      except
        on e: Exception do
        begin
          debug(dpError, section, Format('[EXCEPTION] DB Cleanup: %s ', [e.Message]));
        end;
      end;
  end;
end;

function dbaddpre_GetCount: integer;
begin
  Result := 0;
  if ((dbaddpre_mode = apmSQLITE) or (dbaddpre_mode = apmMemory) or
      (dbaddpre_plm1 = plmSQLITE) or (dbaddpre_plm2 = plmSQLITE)) then
  begin
    if Assigned(ORMAddPreDBSqLite) then
      Result := ORMAddPreDBSqLite.TableRowCount(TSQLAddPreRecord);
  end
  else if (dbaddpre_mode = apmMYSQL) then
  begin
    if Assigned(ORMAddPreDBMySQL) then
      Result := ORMAddPreDBMySQL.TableRowCount(TSQLAddPreRecord);
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

function dbaddpre_Process(const net, chan, nick, msg: String): boolean;
var
  ii: integer;
  fRlsname: String;
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
    fRlsname := Copy(msg, length(addprecmd.Strings[ii] + ' ') + 1, 1000);
    try
      dbaddpre_ADDPRE(net, chan, nick, fRlsname, '', msg, kbeADDPRE);
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] dbaddpre_Process: %s ',
          [e.Message]));
      end;
    end;
  end;
end;

function dbaddpre_Status: String;
begin
  Result := '';
  Result := Format('<b>Dupe.db</b>: %d Rips', [dbaddpre_GetCount]);
end;

procedure dbaddpreStart;
var
  fDBName: String;
  fHost, fPort, fUser, fPass, fDBMS, fLibName: String;
  fJsonMapping: RawUtf8;
  fConfig: IDocDict;
  fColumns: IDocDict;
  fORMMapping: POrmMapping;
  fTableName: RawUTF8;
  fprops: TSqlDBConnectionProperties;
  fConnectionString: RawUTF8;
  fKey: RawUtf8;
begin
  addprecmd := TStringList.Create;
  addprecmd.CommaText := config.ReadString(section, 'addprecmd', '!addpre');
  kbadd_addpre := config.ReadBool(section, 'kbadd_addpre', False);
  add_to_kb_on_dbaddpre_insert := config.ReadBool(section, 'add_to_kb_on_dbaddpre_insert', False);

  dbaddpre_mode := TAddPreMode(config.ReadInteger(section, 'mode', 3));
  dbaddpre_plm1 := TPretimeLookupMode(config.ReadInteger('taskpretime', 'mode', 0));
  dbaddpre_plm2 := TPretimeLookupMode(config.ReadInteger('taskpretime', 'mode_2', 0));

  config_taskpretime_url := config.readString('taskpretime', 'url', '');
  config_taskpretime_regexp := RawByteString(config.readString('taskpretime', 'regexp', '(\S+) (?<pretime>\d+) (\S+) (\S+) (\S+)$'));
  fTableName := config.ReadString('taskmysqlpretime', 'tablename', 'addpre');


  fJsonMapping :=
    '{' + #13#10 +
    '  "tableName": "' + fTableName + '",' + #13#10 +
    '  "columns": {' + #13#10 +
    '    "rlz": "' + config.ReadString('taskmysqlpretime', 'rlsname_field', 'rlz') + '",' + #13#10 +
    '    "ts": "' + config.ReadString('taskmysqlpretime', 'rlsdate_field', 'ts')+ '",' + #13#10 +
    '    "section": "' + config.ReadString('taskmysqlpretime', 'section_field', 'section')+ '",' + #13#10 +
    '    "source": "' + config.ReadString('taskmysqlpretime', 'source_field', 'source')+ '"' + #13#10 +
    '  }' + #13#10 +
    '}';

  ORMAddPreModel := TORMModel.Create([TSQLAddPreRecord]);
  FDbCleanupCounter := 0;

  if ( (dbaddpre_mode = apmSQLITE) or (dbaddpre_plm1 = plmSQLITE) or (dbaddpre_plm2 = plmSQLITE) ) then
  begin
    try
      begin
        fDBName := Trim(config.ReadString(section, 'db_file', 'db_addpre.db'));
        try
          ORMAddPreDBSqLite := CreateORMSQLite3DB(ORMAddPreModel, fDBName, '');
        except
          on e: Exception do
          begin
            Debug(dpError, section, Format('[EXCEPTION] statsInit: %s', [e.Message]));
            exit;
          end;
        end;
    end;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] dbaddpreStart: %s ',[e.Message]));
        exit;
      end;
    end;
  end;
  if dbaddpre_mode = apmMySQL then
  begin
    // initialize global MySQL/MariaD object
    fHost := config.ReadString('mysql', 'host', '0');
    if fHost <> '0' then
    begin

      fPort := IntToStr(config.ReadInteger('mysql', 'port', 3306));
      fUser := config.ReadString('mysql', 'user', 'dbuser');
      fPass := config.ReadString('mysql', 'pass', 'dbpass');
      fDbName := config.ReadString('mysql', 'dbname', 'slftp-addpre');
      fDBMS := UpperCase(config.ReadString('mysql', 'dbms', ''));

      // differentiate between db software, maybe not compatible in future
      if fDBMS = 'MYSQL' then
      begin
        fLibName := {$IFDEF MSWINDOWS}WINDOWS_DLL_LOCATION{$ELSE}LINUX_DLL_LOCATION{$ENDIF};
      end
      else if fDBMS = 'MARIADB' then
      begin
        fLibName := MARIADB_LOCATION;
      end
      else
      begin
        Debug(dpError, section, 'Please set DBMS entry for MySQL/MariaDB in config.');
        exit;
      end;

      fConnectionString :=
      FormatUtf8('zdbc:mysql://%:%/%?username=%;password=%',
      [fHost, fPort, fDbName, fUser, fPass]);

      fProps := TSQLDBZEOSConnectionProperties.Create(
        fConnectionString, // Host or connection string
        fDbName,       // Database name
        fUser, fPass);
      fProps.ThreadSafeConnection.Connect;

      fConfig := DocDict(fJsonMapping);
      fTableName := fConfig.S['tableName'];

      // Map the ORM class to external DB with custom table name from config
      fORMMapping := OrmMapExternal(ORMAddPreModel, TSQLAddPreRecord, fProps, fTableName);
      fColumns := fConfig.D['columns'];

      // Map individual columns from config
      for fKey in fColumns.Keys do
      begin
        fORMMapping^.MapField(fKey, fColumns.S[fKey]);
        WriteLn('Mapped: ', fKey, ' -> ', fColumns.S[fKey]);
      end;

      ORMAddPreDBMysql := CreateORMMysqlConnection(ORMAddPreModel, fDbName, fLibName, fHost, fUser, fPass, fPort);
    end
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
  Result := False;

  if ((dbaddpre_mode = apmSQLITE) or (dbaddpre_mode = apmMemory) or
      (dbaddpre_plm1 = plmSQLITE) or (dbaddpre_plm2 = plmSQLITE)) then
  begin
    Result := Assigned(ORMAddPreDBSqLite);
    Exit;
  end;

  if (dbaddpre_mode = apmMySQL) then
  begin
    Result := Assigned(ORMAddPreDBMysql);
    Exit;
  end;
end;

procedure dbaddpreUninit;
begin
  Debug(dpSpam, section, 'Uninit1');
  addprecmd.Free;

  if Assigned(ORMAddPreDBSqLite) then
  begin
    // Checkpoint WAL to merge changes back into main database and truncate WAL file
    if Assigned(addpreSQLite3DBCon) then
      addpreSQLite3DBCon.MainSQLite3DB.Execute('PRAGMA wal_checkpoint(TRUNCATE)');
    FreeAndNil(ORMAddPreDBSqLite);
    FreeAndNil(addpreSQLite3DBCon);
  end;
  if Assigned(ORMAddPreDBMySQL) then
  begin
    FreeAndNil(ORMAddPreDBMySQL);
  end;
  if Assigned(ORMAddPreModel) then
  begin
    FreeAndNil(ORMAddPreModel);
  end;
  Debug(dpSpam, section, 'Uninit2');
end;

end.
