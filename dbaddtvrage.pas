unit dbaddtvrage;

interface

uses Classes, IniFiles, irc, slsqlite, kb, Contnrs;

type
  TDbTVRage = class
    rls_showname: string;
    tv_showid: string;
    tv_showname: string;
    tv_showurl: string;
    tv_premiered_year: Integer;
    tv_country: string;
    tv_status: string;
    tv_classification: string;
    tv_genres: TStringList;
    tv_network: string;
    tv_runtime: Integer;
    tv_endedyear: Integer;
    tv_running : Boolean;
//    tv_seasons:integer;
    last_updated:integer;
    constructor Create(rls_showname :string);//overload;
    destructor Destroy; override;
    function Name: string;
    procedure Save;
    procedure PostResults(rls : String = '');overload;
    procedure PostResults(Netname,Channel:string;rls : String = '');overload;
    procedure SetTVRageRelease(tr:TTVRelease);

  end;


function dbaddtvrage_gettvrage_show(rls_showname: String): TDbTVRage;
function dbaddtvrage_gettvrage_rls(rls: String): TDbTVRage;
function dbaddtvrage_gettvrage_id(tv_showid: String): TDbTVRage;

procedure dbaddtvrage_FireKbAdd(rls : String);

function dbaddtvrage_Process(net, chan, nick, msg: string) : Boolean;
procedure dbaddtvrage_addtvrage(params: string);
procedure dbaddtvrage_SaveTVRage(tv_showid: string; tvrage: TDbTVRage; rls : String = '');

function dbaddtvrage_GetCount: Integer;
function dbaddtvrage_Status: string;

procedure dbaddtvrageInit;
procedure dbaddtvrageStart;
procedure dbaddtvrageUnInit;


implementation

uses DateUtils, SysUtils, Math, configunit, mystrings, irccommandsunit, console, ircblowfish,
  sitesunit, queueunit, slmasks, slhttp, regexpr, debugunit,
  taskhttptvrage, pazo, mrdohutils;

const
  section = 'dbaddtvrage';

var
  addtvrageDB: TslSqliteDB = nil;
  sql_addtvrage: Psqlite3_stmt = nil;
  sql_counttvrage: Psqlite3_stmt = nil;

  addtvragecmd: string;
  oldtvragecmd: string;

  last_addtvrage: THashedStringList;

{ TDbTVRage }
constructor TDbTVRage.Create(rls_showname:string);
begin
  self.rls_showname := rls_showname;
  self.tv_genres:= TStringList.Create;
  self.tv_genres.QuoteChar:='"';
end;

destructor TDbTVRage.Destroy;
begin
  self.tv_genres.Free;
  inherited;
end;

function TDbTVRage.Name: string;
begin
  try
    Result:= 'TVRAGE :'+rls_showname+' : ';
  except
    Result:= 'TVRAGE';
  end;
end;

procedure TDbTVRage.Save;
begin
  try
    addtvrageDB.ExecSQL( sql_addtvrage, [rls_showname, tv_showid, tv_showname, tv_showurl, IntToStr(tv_premiered_year),
                       tv_country, tv_status, tv_classification, tv_genres.DelimitedText, tv_network, IntToStr(tv_runtime), BoolToStr(tv_running), InttoStr(tv_endedyear)]);//, IntToStr(tv_seasons)]);
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TDbTVRage.Save: %s ', [e.Message]));
      irc_Adderror(Format('<c4>[EXCEPTION]</c> TDbTVRage.Save: %s', [e.Message]));
    end;
  end;
end;

procedure TDbTVRage.PostResults(rls : String = '');
begin
  try
if ((rls = '') or (tv_showid = rls)) then rls:= rls_showname;
    irc_Addstats(Format('(<c9>i</c>)....<c7><b>TVRAGE (db)</b></c>....... <c0><b>info for</c></b> ...........: <b>%s</b> (%s) - http://tvrage.com/shows/id-%s/',[rls,IntToStr(tv_premiered_year),tv_showid]));
    irc_Addstats(Format('(<c9>i</c>)....<c7><b>TVRAGE (db)</b></c>.. <c9><b>Genre (Class) @ Status</c></b> ..: %s (%s) @ %s',[tv_genres.CommaText,tv_classification,tv_status]));
    irc_Addstats(Format('(<c9>i</c>)....<c7><b>TVRAGE (db)</b></c>....... <c4><b>Country/Channel</c></b> ....: <b>%s</b> (%s) ',[tv_country,tv_network]));
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TDbTVRage.PostResults: %s ', [e.Message]));
      irc_Adderror(Format('<c4>[EXCEPTION]</c> TDbTVRage.PostResults: %s', [e.Message]));
    end;
  end;
end;

procedure TDbTVRage.PostResults(Netname: string; Channel: string; rls: string = '');
begin
  try
if ((rls = '') or (tv_showid = rls)) then rls:= rls_showname;
    irc_AddText(Netname,CHannel,Format('(<c9>i</c>)....<c7><b>TVRAGE (db)</b></c>....... <c0><b>info for</c></b> ...........: <b>%s</b> (%s) - http://tvrage.com/shows/id-%s/',[rls,IntToStr(tv_premiered_year),tv_showid]));
    irc_AddText(Netname,CHannel,Format('(<c9>i</c>)....<c7><b>TVRAGE (db)</b></c>.. <c9><b>Genre (Class) @ Status</c></b> ..: %s (%s) @ %s',[tv_genres.CommaText,tv_classification,tv_status]));
    irc_AddText(Netname,CHannel,Format('(<c9>i</c>)....<c7><b>TVRAGE (db)</b></c>....... <c4><b>Country/Channel</c></b> ....: <b>%s</b> (%s) ',[tv_country,tv_network]));
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TDbTVRage.PostResults: %s ', [e.Message]));
    end;
  end;
end;


procedure TDbTVRage.SetTVRageRelease(tr:TTVRelease);
begin
  tr.showname := rls_showname;
  tr.showid := tv_showid;
  tr.premier_year := tv_premiered_year;
  tr.country := tv_country;
  tr.status := tv_status;
  tr.classification := tv_classification;
  tr.genres.Assign(tv_genres);
  tr.network := tv_network;
  tr.runtime := tv_runtime;
  tr.running := tv_running;
  tr.ended_year:= tv_endedyear;
//  tr.season:= tv_seasons;
  if tv_classification = 'Scripted' then tr.scripted:=True;
  PostResults(tr.rlsname);
end;


{ Global functions }
function dbaddtvrage_gettvrage_show(rls_showname: String): TDbTVRage;
var i: Integer;
    tvrage : TDbTVRage;
    gettvrage:Psqlite3_stmt;
begin
  Result := nil;
  if addtvrageDB = nil then exit;

  if (rls_showname = '') then exit;

  try
    i:= last_addtvrage.IndexOf(rls_showname);
    if i <> -1 then
    begin
      Result:= TDbTVRage(last_addtvrage.Objects[i]);
    end;
  except
    Result := nil;
  end;

    if (Result = nil) then
    begin
      try
        gettvrage:= addtvrageDB.Open('SELECT * FROM addtvrage WHERE rls_showname = "'+rls_showname+'"');
        if addtvrageDB.Step(gettvrage) then
        begin
          if (rls_showname <> addtvrageDB.column_text(gettvrage, 0)) then
          begin
            Result := nil;
            exit;
          end;

          tvrage := TDbTVRage.Create(rls_showname);
          tvrage.tv_showid := addtvrageDB.column_text(gettvrage, 1);
          tvrage.tv_showname := addtvrageDB.column_text(gettvrage, 2);
          tvrage.tv_showurl := addtvrageDB.column_text(gettvrage, 3);
          tvrage.tv_premiered_year := StrToIntDef(addtvrageDB.column_text(gettvrage, 4), 0);
          tvrage.tv_country := addtvrageDB.column_text(gettvrage, 5);
          tvrage.tv_status := addtvrageDB.column_text(gettvrage, 6);
          tvrage.tv_classification := addtvrageDB.column_text(gettvrage, 7);
//          tvrage.tv_genres.DelimitedText := addtvrageDB.column_text(gettvrage, 8);
          tvrage.tv_genres.CommaText := addtvrageDB.column_text(gettvrage, 8);
          tvrage.tv_network := addtvrageDB.column_text(gettvrage, 9);
          tvrage.tv_runtime := StrToIntDef(addtvrageDB.column_text(gettvrage, 10),0);
          tvrage.tv_running := StrToBoolDef(addtvrageDB.column_text(gettvrage, 11), False);
          tvrage.tv_endedyear:= StrToIntDef(addtvrageDB.column_text(gettvrage, 12),-1);
//          tvrage.tv_seasons:= StrToIntDef(addtvrageDB.column_text(gettvrage, 13),-1);
        //last_updated:=StrToIntDef(addtvrageDB.column_text(gettvrage, 14),-1);

 //         last_addtvrage.AddObject(rls_showname, tvrage);
          Result:= tvrage;
        end;
      except
        on e: Exception do
        begin
          Result := nil;
          Debug(dpError, section, Format('[EXCEPTION] dbaddtvrage_gettvrage_show: %s ', [e.Message]));
        end;
      end;
    end;
end;

function dbaddtvrage_gettvrage_rls(rls: String): TDbTVRage;
var showname : String;
    tvr: TTVRelease;
begin
  Result := nil;

  tvr := TTVRelease.Create(rls, '');
  showname:=  tvr.showname;

  if (showname <> '') then
  begin
    Result := dbaddtvrage_gettvrage_show(showname);
  end;
end;

function dbaddtvrage_gettvrage_id(tv_showid: String): TDbTVRage;
var i: Integer;
    tvrage : TDbTVRage;
    gettvrage:Psqlite3_stmt;
begin
  Result := nil;
  if addtvrageDB = nil then exit;

    for i := last_addtvrage.Count - 1 downto 0 do
    begin
      try if i < 0 then Break; except Break; end;
      try
        tvrage := TDbTVRage(last_addtvrage.Objects[i]);
        if (tvrage.tv_showid = tv_showid) then
        begin
          Result := tvrage;
          break;
        end;
      except
        break;
      end;
    end;

    if (Result = nil) then
    begin
      try
        gettvrage:= addtvrageDB.Open('SELECT * FROM addtvrage WHERE tv_showid = "'+tv_showid+'"');
        if addtvrageDB.Step(gettvrage) then
        begin
          tvrage := TDbTVRage.Create(addtvrageDB.column_text(gettvrage, 0));
          tvrage.tv_showid := addtvrageDB.column_text(gettvrage, 1);
          tvrage.tv_showname := addtvrageDB.column_text(gettvrage, 2);
          tvrage.tv_showurl := addtvrageDB.column_text(gettvrage, 3);
          tvrage.tv_premiered_year := addtvrageDB.column_int(gettvrage, 4);
          tvrage.tv_country := addtvrageDB.column_text(gettvrage, 5);
          tvrage.tv_status := addtvrageDB.column_text(gettvrage, 6);
          tvrage.tv_classification := addtvrageDB.column_text(gettvrage, 7);
//          tvrage.tv_genres.DelimitedText := addtvrageDB.column_text(gettvrage, 8);
          tvrage.tv_genres.CommaText := addtvrageDB.column_text(gettvrage, 8);
          tvrage.tv_network := addtvrageDB.column_text(gettvrage, 9);
          tvrage.tv_runtime := addtvrageDB.column_int(gettvrage, 10);
          tvrage.tv_running := StrToBoolDef(addtvrageDB.column_text(gettvrage, 11), False);
          tvrage.tv_endedyear := addtvrageDB.column_int(gettvrage, 12);
//          tvrage.tv_seasons := addtvrageDB.column_int(gettvrage, 13);
//          tvrage.last_updated := addtvrageDB.column_int(gettvrage, 14);

//          last_addtvrage.AddObject(tvrage.tv_showname, tvrage);
          Result:= tvrage;
        end;
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[EXCEPTION] dbaddtvrage_gettvrage_id: %s ', [e.Message]));
        end;
      end;
    end;
end;

procedure dbaddtvrage_FireKbAdd(rls : String);
var p : TPazo;
    ps: TPazoSite;
begin
  p:= FindPazoByRls(rls);
  if (p <> nil) then
  begin
    ps := FindMostCompleteSite(p);
    if ((ps = nil) and (p.sites.Count > 0)) then
      ps:= TPazoSite(p.sites[0]);

    if (ps <> nil) then
    begin
      try
        if spamcfg.ReadBool('addinfo','tvrageupdate',True) then
          irc_Addadmin(Format('<c3>[TVRAGE RLZ]</c> %s %s now have TVRage infos (%s)', [p.rls.section, p.rls.rlsname, ps.name]));
        kb_Add('', '', ps.name, p.rls.section, '', 'UPDATE', p.rls.rlsname, '');
      except
        on e: Exception do
        begin
          Debug(dpError, section, '[EXCEPTION] kb_Add : %s', [e.Message]);
        end;
      end;
    end;
  end;
end;

function dbaddtvrage_Process(net, chan, nick, msg: string): Boolean;
begin
  Result := False;
  if (1 = Pos(addtvragecmd, msg)) then
  begin
    msg := Copy(msg, length(addtvragecmd + ' ') + 1, 1000);
    dbaddtvrage_addtvrage(msg);
    Result := True;
  end;
  if (1 = Pos(oldtvragecmd, msg)) then
  begin
    msg := Copy(msg, length(oldtvragecmd + ' ') + 1, 1000);
    dbaddtvrage_addtvrage(msg);
    Result := True;
  end;
end;

procedure dbaddtvrage_addtvrage(params: string);
var
  rls: string;
  tv_showid: string;
begin
  rls := '';
  rls := SubString(params, ' ', 1);
  tv_showid := '';
  tv_showid := SubString(params, ' ', 2);

  try
    AddTask(TPazoHTTPTVRageTask.Create(tv_showid, rls));
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('Exception in dbaddtvrage_addtvrage AddTask: %s', [e.Message]));
      exit;
    end;
  end;
end;


procedure dbaddtvrage_SaveTVRage(tv_showid: string; tvrage: TDbTVRage; rls : String = '');
var save_tvrage : TDbTVRage;
begin
  if (dbaddtvrage_gettvrage_id(tv_showid) = nil) then
  begin
    // add the tvrage
    save_tvrage := TDbTVRage(tvrage);
    try
//      last_addtvrage.AddObject(tvrage.tv_showname, tvrage);
      if (rls <> '') then
        irc_Addtext_by_key('ADDTVRAGE', '!addtvrage '+rls+' '+tv_showid);
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('Exception in dbaddtvrage_SaveTVRage last_addtvrage.Add: %s', [e.Message]));
        exit;
      end;
    end;

    if (rls <> '') then
    begin
       dbaddtvrage_FireKbAdd(rls);
    end;

    try
      save_tvrage.Save;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] tvrage.Save: %s ', [e.Message]));
      end;
    end;
  end;
end;

function dbaddtvrage_GetCount: Integer;
begin
  Result := 0;

  if addtvrageDB = nil then exit;
  if sql_counttvrage = nil then exit;
  try
    addtvrageDB.Open(sql_counttvrage);
    while addtvrageDB.Step(sql_counttvrage) do
      Result := addtvrageDB.column_int(sql_counttvrage, 0);
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] dbaddtvrage_GetCount: %s ', [e.Message]));
    end;
  end;
end;

function dbaddtvrage_Status: string;
begin
  Result := '';

  Result:= Format('<b>DB addtvrage</b>: %d (%d) tvrage infos',[dbaddtvrage_GetCount, last_addtvrage.Count]);
end;

procedure dbaddtvrageInit;
begin
  last_addtvrage:= THashedStringList.Create;
  last_addtvrage.CaseSensitive:= False;
end;

procedure dbaddtvrageStart;
var
  db_pre_name: string;
begin
  addtvragecmd := config.ReadString(section, 'addtvragecmd', '!addtvrage');
  oldtvragecmd := config.ReadString(section, 'oldtvragecmd', '!oldtvrage');

  if slsqlite_inited then
  begin
    db_pre_name := Trim(config.ReadString(section, 'db_file', 'db_addtvrage.db'));

    // addtvrage DB
    addtvrageDB := TslSqliteDB.Create(db_pre_name, config.ReadString(section, 'pragma', ''));
    addtvrageDB.ExecSQL(
      'CREATE TABLE IF NOT EXISTS addtvrage (rls_showname VARCHAR(255) NOT NULL, tv_showid VARCHAR(255), tv_showname VARCHAR(255),'+
      'tv_showurl VARCHAR(255), tv_premiered_year INT(10), tv_country VARCHAR(255), tv_status VARCHAR(255), tv_classification VARCHAR(255),'+
      'tv_genres VARCHAR(255), tv_network VARCHAR(255), tv_runtime INT(10), tv_running VARCHAR(5), tv_endedyear INT(10))'//, tv_seasons INT(5), last_updated INT(50))'
      );
    addtvrageDB.ExecSQL(
      'CREATE UNIQUE INDEX IF NOT EXISTS addtvrage_index ON addtvrage (rls_showname)'
      );

    sql_addtvrage := addtvrageDB.Open('INSERT OR IGNORE INTO addtvrage (rls_showname, tv_showid, tv_showname, tv_showurl, tv_premiered_year, tv_country, tv_status, tv_classification, tv_genres, tv_network, tv_runtime, tv_running, tv_endedyear)'+//, tv_seasons ,last_updated)' +
                   'VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)');//, ?, ?)');
    sql_counttvrage := addtvrageDB.Open('SELECT count(*) FROM addtvrage');

    Console_Addline('', 'Local addtvrage DB Started...');
  end;
end;

procedure dbaddtvrageUninit;
begin
  last_addtvrage.Free;
  if addtvrageDB <> nil then
  begin
    addtvrageDB.Free;
    addtvrageDB := nil;
  end;
end;

end.

