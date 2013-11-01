unit dbaddpre;

interface

uses Classes, IniFiles, irc, slsqlite, kb;

function dbaddpre_ADDPRE(netname, channel, nickname: string; params: string): boolean;
function dbaddpre_InsertRlz(rls, rls_section, source: string): boolean;
function dbaddpre_GetCount: Integer;
function dbaddpre_GetPreduration(rlz_pretime: TDateTime): string;
function dbaddpre_Status: string;

function dbaddpre_Process(net, chan, nick, msg: string): Boolean;

procedure dbaddpreInit;
procedure dbaddpreStart;
procedure dbaddpreUnInit;

function ReadPretime(rlz: string): TDateTime;
function ReadPretimeOverHTTP(rls:string):TDateTime;
function ReadPretimeOverSQLITE(rls:string):TDateTime;

implementation

uses DateUtils, SysUtils, Math, configunit, mystrings, irccommandsunit, console, ircblowfish,
  sitesunit, queueunit, slmasks, slhttp, taskpretime, regexpr, debugunit, pazo, rulesunit, taskrace,
  mrdohutils, precatcher, SyncObjs;

const
  section = 'dbaddpre';

var
  addpreDB: TslSqliteDB = nil;
  sql_addrlz: Psqlite3_stmt = nil;
  sql_countrlz: Psqlite3_stmt = nil;
  sql_gettime: Psqlite3_stmt = nil;

  addprecmd: TStringlist;
  kbadd_addpre: Boolean;

  addpre_lock: TCriticalSection;
  last_addpre: THashedStringList;


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
begin
  Result := UnixToDateTime(0);
  if rls = '' then irc_adderror('No Releasename as parameter!');



  url:= config.readString('taskpretime','url','');//GetPretimeURL;
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
    irc_addtext('CONSOLE','ADMIN','Read coount higher then 500');
      Result := UnixToDateTime(0);
      break;
    end;
    if prex.Exec(response.strings[i]) then
    begin
      Debug(dpMessage, section, 'ReadPretimeOverHTTP : %s', [response.DelimitedText]);

      if (StrToIntDef(prex.Match[2], 0) <> 0) and (StrToIntDef(prex.Match[2], 0) <> 1295645417) then
      begin
        vctime:=PrepareTimestamp(strtoint(prex.Match[2]));
        result:=UnixToDateTime(vctime);

        if (DaysBetween(Now(), Result) > 30) then begin
        irc_addtext('CONSOLE','ADMIN','Days higher then 30 days');
          Result := UnixToDateTime(0);
        end;
      end else begin
      irc_addtext('CONSOLE','ADMIN','regex dosnot match');
        Result := UnixToDateTime(0);
      end;
    end;
  end;
  prex.free;
  response.free;
end;


function ReadPretimeOverSQLITE(rls:string):TDateTime;
var time:int64;
    rlz_timestamp: String;
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

function ReadPretime(rlz: string): TDateTime;
var plm:TPretimeLookupMOde;
begin
  Debug(dpSpam, section, 'GetPretime for %s with %d', [rlz, config.ReadInteger('taskpretime','mode',0)]);
  Result := UnixToDateTime(0);

  if rlz = '' then irc_adderror('GETPRETIME --> No RLZ value!');


  plm:= TPretimeLookupMOde(config.ReadInteger('taskpretime','mode',0));
  case plm of
    plmNone: Exit;
    plmHTTP: result:= ReadPretimeOverHTTP(rlz);
    plmSQLITE: result:= ReadPretimeOverSQLITE(rlz);
    else begin
      Debug(dpMessage, section, 'GetPretime unknown pretime mode : %d', [config.ReadInteger('taskpretime','mode',0)]);
      Result := UnixToDateTime(0);
    end;
  end;

  if ((Result = UnixToDateTime(0)) and (config.ReadInteger('taskpretime','mode_2',0) <> 0)) then
  begin
    plm:= TPretimeLookupMOde(config.ReadInteger('taskpretime','mode_2',0));
    case plm of
      plmNone: Exit;
      plmHTTP: result:= ReadPretimeOverHTTP(rlz);
      plmSQLITE: result:= ReadPretimeOverSQLITE(rlz);
      else begin
        Debug(dpMessage, section, 'GetPretime unknown pretime mode : %d', [config.ReadInteger('taskpretime','mode',0)]);
        Result := UnixToDateTime(0);
      end;
    end;
  end;
end;


//function ReadPretimeOverMYSQL(tls:string):TDateTime;


function kb_Add_addpre(rls, section: string): Integer;
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
    irc_Addstats(Format('<c7>[ADDPRE RLZ]</c> %s %s (%s) : <b>No Sites</b>', [rls, rls_section, section]));
    exit;
  end;

  kb_Add('', '', config.ReadString('sites', 'admin_sitename', 'SLFTP'), rls_section, '', 'ADDPRE', rls, '');
end;

function dbaddpre_ADDPRE(netname, channel, nickname: string; params: string): boolean;
var
  rls: string;
  rls_section: string;
begin
  Result := False;

  if addpreDB = nil then
    exit;
  if sql_addrlz = nil then
    exit;

  rls := '';
  rls := SubString(params, ' ', 1);
  rls_section := '';
  rls_section := UpperCase(SubString(params, ' ', 2));
  if ((rls <> '') and (rls_section <> '') and (length(rls) > minimum_rlsname)) then
  begin
    if dbaddpre_InsertRlz(rls, rls_section, netname+'-'+channel+'-'+nickname) then
    begin
      if kbadd_addpre then
      begin
        try
          kb_Add_addpre(rls, rls_section);
        except
          on E: Exception do
          begin
            Debug(dpError, 'kb', Format('[EXCEPTION] kb_Add_addpre: %s', [e.Message]));
            exit;
          end;
        end;
      end;
    end;
  end;

  Result := True;
end;

function dbaddpre_InsertRlz(rls, rls_section, source: string): Boolean;
var i: Integer;
begin
  Result:= False;
  i:= last_addpre.IndexOfName(rls);
  if (i = -1 ) then
  begin
    Result:= True;

    addpreDB.ExecSQL( sql_addrlz, [rls, rls_section, DateTimeToUnix(Now()), source]);

    try
      last_addpre.Add(rls+'='+IntToStr(DateTimeToUnix(Now())));
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] last_addpre.Add: %s ', [e.Message]));
      end;
    end;
  end;
end;

function dbaddpre_GetCount: Integer;
var i: Integer;
begin
  Result := 0;

  if addpreDB = nil then exit;
  if sql_countrlz = nil then exit;
  try
    i:= 0;
    addpreDB.Open(sql_countrlz);
    while addpreDB.Step(sql_countrlz) do
    begin
      inc(i); if i > 10 then begin Result:= 0; exit; end;
      Result := addpreDB.column_int(sql_countrlz, 0);
    end;
  except
    Result:= 0;
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
      addpre_lock.Enter;
      try
        dbaddpre_ADDPRE(net, chan, nick, msg);
      finally
        addpre_lock.Leave;
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

function dbaddpre_Status: string;
var plm:TPretimeLookupMOde;
begin
  Result := '';

  plm:= TPretimeLookupMOde(config.ReadInteger('taskpretime','mode',0));
  case plm of
    plmNone: Exit;
    plmHTTP: Exit;
    plmSQLITE: begin
      Result:= Format('<b>DB addpre</b>: %d (%d) rls',[dbaddpre_GetCount, last_addpre.Count]);
    end
    else Exit;
  end;
end;

procedure dbaddpreInit;
begin
  addpre_lock:= TCriticalSection.Create;
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

  addprecmd.CommaText := config.ReadString(section, 'addprecmd', '!addpre,!sitepre');
  kbadd_addpre := config.ReadBool(section, 'kbadd_addpre', False);

  if slsqlite_inited then
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

    Console_Addline('', 'Local addpre DB Started...');
  end;
end;

procedure dbaddpreUninit;
begin
  addprecmd.free;
  addpre_lock.Free;
  last_addpre.Free;
  if addpreDB <> nil then
  begin
    addpreDB.Free;
    addpreDB := nil;
  end;
end;

end.

