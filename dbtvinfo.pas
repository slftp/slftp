unit dbtvinfo;

interface

uses Classes, IniFiles, irc, slsqlite, kb, Contnrs;

type
  TTVInfoDB = class
  private
    last_updated: integer;
  public
    rls_showname: string;
    tv_showid: string;
    tv_tvrageid: string;
    tv_tvmazeid: string;
    tv_showname: string;
    tv_country: string;
    tv_status: string;
    tv_classification: string;
    tv_genres: TStringList;
    tv_network: string;
    //tv_runtime: integer;
    tv_premiered_year: integer;
    tv_endedyear: integer;
    tv_running: boolean;
    tv_scripted: boolean;
    tv_next_season: integer;
    tv_next_ep: integer;
    tv_next_date: integer;
    constructor Create(rls_showname: string); //overload;
    destructor Destroy; override;
    function Name: string;
    procedure Save;
    procedure PostResults(rls: string = ''); overload;
    procedure PostResults(Netname, Channel: string; rls: string = ''); overload;
    procedure SetTVDbRelease(tr: TTVRelease);

  end;

function getTVInfoCount: integer;
function getTVInfoSeriesCount: integer;

function TheTVDbStatus: string;

procedure dbTVInfoInit;
procedure dbTVInfoStart;
procedure dbTVInfoUnInit;

function getTVInfoByShowName(rls_showname: string): TTVInfoDB;
function getTVInfoByReleaseName(rls: string): TTVInfoDB;

function getTVInfoByShowID(tv_showid: string): TTVInfoDB;

procedure saveTVInfos(tv_showid: string; tvrage: TTVInfoDB; rls: string = '');

procedure addTVInfos(params: string);
procedure TVInfoFireKbAdd(rls: string);

implementation

uses DateUtils, SysUtils, Math, configunit, mystrings, irccommandsunit, console, ircblowfish, sitesunit, queueunit, slmasks, slhttp, regexpr, debugunit,
  tasktvinfolookup, pazo, mrdohutils;

const
  section = 'tasktvinfo';

var
  tvinfodb: TslSqliteDB = nil;
  sql_addtvinfodb: Psqlite3_stmt = nil;
  sql_counttvinfodb: Psqlite3_stmt = nil;

  addtinfodbcmd: string;
  oldtvinfodbcmd: string;

  //  last_addthetvdb: THashedStringList;

function getTVInfoCount: integer;
var
  icount: Psqlite3_stmt;
begin
  icount := tvinfodb.Open('SELECT count(*) FROM infos;');
  if tvinfodb.Step(icount) then
    result := tvinfodb.column_int(icount, 0)
  else
    result := 0;
end;

function getTVInfoSeriesCount: integer;
var
  icount: Psqlite3_stmt;
begin
  icount := tvinfodb.Open('SELECT count(*) FROM series;');
  if tvinfodb.Step(icount) then
    result := tvinfodb.column_int(icount, 0)
  else
    result := 0;
end;

procedure TTVInfoDB.Save;
var
  n: Psqlite3_stmt;
  dbid: integer;
begin
  dbid := -1;
  try
    tvinfodb.ExecSQL(Format('INSERT OR IGNORE INTO  infos (tvdb_id,premiered_year,country,status,classification,network,genre,ended_year,last_updated,tvrage_id, tvmaze_id) VALUES (%d,%d,"%s","%s","%s","%s","%s",%d,%d,%d,%d)',
      [StrToInt(tv_showid), tv_premiered_year, tv_country, tv_status, tv_classification, tv_network, tv_genres.CommaText, tv_endedyear, DateTimeToUnix(now()),
        StrToInt(tv_tvrageid), StrToInt(tv_tvmazeid)]));
  except on E: Exception do
    begin
      Irc_AddText('', '', 'Error@TTVInfoDB.Save_INSERT infos %s', [e.Message]);
    end;
  end;

  tvinfodb.ExecSQL(Format('INSERT OR IGNORE INTO series (rip,showname,id) VALUES ("%s","%s",%d);', [rls_showname,
    tv_showname, StrToInt(tv_showid)]));
end;

procedure TTVInfoDB.SetTVDbRelease(tr: TTVRelease);
begin

  tr.showname := rls_showname;
  tr.showid := tv_showid;
  tr.premier_year := tv_premiered_year;
  tr.country := tv_country;
  tr.status := tv_status;
  tr.classification := tv_classification;
  tr.genres.Assign(tv_genres);
  tr.network := tv_network;
  tr.running := tv_running;
  tr.ended_year := tv_endedyear;
  tr.scripted := tv_scripted;

  tr.tvmazeid:= tv_tvmazeid;
  tr.tvrageid:= tv_tvrageid;


  //  tr.season:= tv_seasons;

  if config.ReadBool(section, 'post_lookup_infos', false) then
    PostResults(tr.rlsname);

end;

constructor TTVInfoDB.Create(rls_showname: string);
begin
  self.rls_showname := rls_showname;
  self.tv_genres := TStringList.Create;
  self.tv_genres.QuoteChar := '"';
  self.tv_endedyear := -1;
end;

destructor TTVInfoDB.Destroy;
begin
  self.tv_genres.Free;
  inherited;
end;

function TTVInfoDB.Name: string;
begin
  try
    Result := 'TTVRelease :' + rls_showname + ' : ';
  except
    Result := 'TTVRelease';
  end;
end;

(*
<c3>[<b>TTVRelease</b>]</c><b>Falling Skies</b> -<b>Premiere Year</b> 2011 - <b>The TVDB info</b>http://tvrage.com/shows/id-26205/
<c3>[<b>TTVRelease</b>]</c><b>Genre</b> Action,Military/War,Mystery,Sci-Fi - <b>Classification</b>Scripted - <b>Status</b> Final Season
<c3>[<b>TTVRelease</b>]</c><b>Country</b> USA - <b>Network</b> TNT DRAMA

*)

procedure TTVInfoDB.PostResults(rls: string = '');
begin
  try
    if ((rls = '') or (tv_showid = rls)) then
      rls := rls_showname;
    if config.ReadBool(section, 'use_new_announce_style', True) then
    begin
      irc_Addstats(Format('<c10>[<b>TVInfo</b>]</c> <b>%s</b> - <b>Premiere Year</b> %s - <b>The TVDB info</b> http://tvinfodb.com/?tab=series&id=%s', [rls,
        IntToStr(tv_premiered_year), tv_showid]));
      irc_Addstats(Format('<c10>[<b>TVInfo</b>]</c> <b>Genre</b> %s - <b>Classification</b> %s - <b>Status</b> %s', [tv_genres.CommaText, tv_classification, tv_status]));
      irc_Addstats(Format('<c10>[<b>TVInfo</b>]</c> <b>Country</b> %s - <b>Network</b> %s', [tv_country,
        tv_network]));
    end
    else
    begin
      irc_Addstats(Format('(<c9>i</c>)....<c7><b>TVRAGE (db)</b></c>....... <c0><b>info for</c></b> ...........: <b>%s</b> (%s) - http://tvrage.com/shows/id-%s/', [rls,
        IntToStr(tv_premiered_year), tv_showid]));
      irc_Addstats(Format('(<c9>i</c>)....<c7><b>TVRAGE (db)</b></c>.. <c9><b>Genre (Class) @ Status</c></b> ..: %s (%s) @ %s', [tv_genres.CommaText,
        tv_classification, tv_status]));
      irc_Addstats(Format('(<c9>i</c>)....<c7><b>TVRAGE (db)</b></c>....... <c4><b>Country/Channel</c></b> ....: <b>%s</b> (%s) ', [tv_country,
        tv_network]));
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TTVInfoDB.PostResultsA: %s ',
        [e.Message]));
      irc_Adderror(Format('<c4>[EXCEPTION]</c> TTVInfoDB.PostResultsA: %s',
        [e.Message]));
    end;
  end;
end;

procedure TTVInfoDB.PostResults(Netname: string; Channel: string; rls: string =
  '');
begin
  try
    if ((rls = '') or (tv_showid = rls)) then
      rls := rls_showname;
    if config.ReadBool(section, 'use_new_announce_style', True) then
    begin
      irc_Addtext(Netname, Channel,
        Format('<c10>[<b>TTVRelease</b>]</c> <b>%s</b> - <b>Premiere Year</b> %s - <b>The TVDB info</b> http://tvinfodb.com/?tab=series&id=%s', [rls,
        IntToStr(tv_premiered_year), tv_showid]));
      irc_Addtext(Netname, Channel,
        Format('<c10>[<b>TTVRelease</b>]</c> <b>Genre</b> %s - <b>Classification</b> %s - <b>Status</b> %s',
        [tv_genres.CommaText, tv_classification, tv_status]));
      irc_Addtext(Netname, Channel,
        Format('<c10>[<b>TTVRelease</b>]</c> <b>Country</b> %s - <b>Network</b> %s',
        [tv_country, tv_network]));
    end
    else
    begin
      irc_AddText(Netname, CHannel, Format('(<c9>i</c>)....<c7><b>TVRAGE (db)</b></c>....... <c0><b>info for</c></b> ...........: <b>%s</b> (%s) - http://tvrage.com/shows/id-%s/',
        [rls, IntToStr(tv_premiered_year), tv_showid]));
      irc_AddText(Netname, CHannel, Format('(<c9>i</c>)....<c7><b>TVRAGE (db)</b></c>.. <c9><b>Genre (Class) @ Status</c></b> ..: %s (%s) @ %s', [tv_genres.CommaText,
        tv_classification, tv_status]));
      irc_AddText(Netname, CHannel, Format('(<c9>i</c>)....<c7><b>TVRAGE (db)</b></c>....... <c4><b>Country/Channel</c></b> ....: <b>%s</b> (%s)', [tv_country, tv_network]));
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TTVInfoDB.PostResultsB: %s ',
        [e.Message]));
      irc_Adderror(Format('<c4>[EXCEPTION]</c> TTVInfoDB.PostResultsB: %s',
        [e.Message]));
    end;
  end;
end;

function TheTVDbStatus: string;
begin
  Result := Format('<b>TVInfo.db</b>: %d Series, with %d infos',
    [getTVInfoSeriesCount, getTVInfoCount]);
end;

procedure dbTVInfoStart;
var
  db_name, db_params: string;
begin
  addtinfodbcmd := config.ReadString(section, 'addcmd', '!addthetvdb');
  if slsqlite_inited then
  begin
    db_name := Trim(config.ReadString(section, 'db_file', 'tvinfos.db'));
    db_params := config.ReadString(section, 'pragma', 'main.locking_mode = NORMAL');
    tvinfodb := TslSqliteDB.Create(db_name, db_params);

    tvinfodb.ExecSQL('CREATE TABLE IF NOT EXISTS "series" ("rip"  TEXT NOT NULL,"showname"  TEXT NOT NULL,"rip_country"  TEXT,"id"  INTEGER NOT NULL,PRIMARY KEY ("rip"));');

    tvinfodb.ExecSQL('CREATE TABLE IF NOT EXISTS "infos" ("tvdb_id"  INTEGER NOT NULL,"tvrage_id"  INTEGER,"tvmaze_id"  INTEGER,"premiered_year"  INTEGER NOT NULL,' +
      '"country"  TEXT NOT NULL DEFAULT unknown,"status"  TEXT NOT NULL DEFAULT unknown,"classification"  TEXT NOT NULL DEFAULT unknown,"network"  TEXT NOT NULL DEFAULT unknown,' +
      '"genre"  TEXT NOT NULL DEFAULT unknown,"ended_year"  INTEGER,"last_updated"  INTEGER NOT NULL DEFAULT -1,"next_date"  INTEGER,"next_season"  INTEGER,"next_episode"  INTEGER,PRIMARY KEY ("tvdb_id" ASC));');

    tvinfodb.ExecSQL('CREATE UNIQUE INDEX IF NOT EXISTS "main"."tvinfo" ON "infos" ("tvdb_id" ASC);');
    tvinfodb.ExecSQL('CREATE UNIQUE INDEX IF NOT EXISTS "main"."Rips" ON "series" ("rip" ASC);');

    Console_Addline('',
      Format('TVInfo db loaded. %d Series, with %d infos',
      [getTVInfoSeriesCount, getTVInfoCount]));
  end;
end;

procedure dbTVInfoInit;
begin
  //  last_addthetvdb := THashedStringList.Create;
  //  last_addthetvdb.CaseSensitive := False;
end;

procedure dbTVInfoUninit;
begin
  //  last_addthetvdb.Free;
  try
    if tvinfodb <> nil then
    begin
      tvinfodb.Free;
      tvinfodb := nil;
    end;
  except on E: Exception do
      Debug(dpError, section,
        Format('Exception in dbTVInfoUninit: %s',
        [e.Message]));
  end;
end;

function getTVDBByNameFromMemory(name: string): TTVInfoDB;
var
  i: integer;
begin
  (*
    try
      i := last_addthetvdb.IndexOf(name);
      if i <> -1 then
      begin
        Result := TTVInfoDB(last_addthetvdb.Objects[i]);
      end;
    except
      Result := nil;
    end;
  *)
end;

function getTVDBByIDFromMemory(id: string): TTVInfoDB;
var
  i: integer;
  tvrage: TTVInfoDB;
begin
  (*
    for i := last_addthetvdb.Count - 1 downto 0 do
    begin
      try
        if i < 0 then
          Break;
      except
        Break;
      end;
      try
        tvrage := TTVInfoDB(last_addthetvdb.Objects[i]);
        if (tvrage.tv_showid = id) then
        begin
          Result := tvrage;
          break;
        end;
      except
        break;
      end;
    end;
    *)
end;

(* broken!
function fillTTheTvDBfromDB(const item: Psqlite3_stmt; show: string = ''):
  TTVInfoDB;
begin

  if item = nil then
  begin
    Result := nil;
    Debug(dpError, section, 'fillTTheTvDBfromDB item is nil');
    Exit;
  end;

  if tvinfodb.Step(item) then
  begin
    if (LowerCase(show) <> LowerCase(tvinfodb.column_text(item, 0))) then
    begin
      Result := nil;
      Debug(dpError, section, 'fillTTheTvDBfromDB is nil');
      exit;
    end;
    if show = '' then
      show := tvinfodb.column_text(item, 0);
    result := TTVInfoDB.Create(show);
    result.tv_showid := tvinfodb.column_text(item, 3);
    result.tv_showname := tvinfodb.column_text(item, 1);
    result.tv_premiered_year := StrToIntDef(tvinfodb.column_text(item, 5), 0);
    result.tv_country := tvinfodb.column_text(item, 6);
    result.tv_status := tvinfodb.column_text(item, 7);
    result.tv_classification := tvinfodb.column_text(item, 8);
    result.tv_genres.CommaText := tvinfodb.column_text(item, 10);
    result.tv_network := tvinfodb.column_text(item, 9);
    result.tv_running := Boolean(lowercase(result.tv_status) = 'running');
    result.tv_scripted := Boolean(lowercase(result.tv_classification) =
      'scripted');
    result.last_updated := StrToIntDef(tvinfodb.column_text(item, 12), -1);
  end;
end;
 *)

function getTVInfoByShowName(rls_showname: string): TTVInfoDB;
var
  i: integer;
  tvi: TTVInfoDB;
  gettvrage: Psqlite3_stmt;
begin
  result := nil;

  if tvinfodb = nil then
  begin
    Debug(dpError, section,
      '[EXCEPTION] getTheTVDBbyShowName: tvinfodb = nil ');
    exit;
  end;

  if (rls_showname = '') then
  begin
    Debug(dpError, section,
      '[EXCEPTION] getTheTVDBbyShowName: rls_showname is empty');
    exit;
  end;

  gettvrage := tvinfodb.Open(
    'SELECT * FROM series LEFT JOIN infos ON infos.tvdb_id = series.id WHERE rip LIKE "' + rls_showname
    + '";'); //so we can handle the aka's .

  if tvinfodb.Step(gettvrage) then
  begin

    if (LowerCase(rls_showname) <> LowerCase(tvinfodb.column_text(gettvrage,
      0))) then
    begin
      Result := nil;
      Debug(dpError, section,
        'fillTTheTvDBfromDB LowerCase(rls_showname) <> LowerCase(tvinfodb.column_text(gettvrage,0)))');
      exit;
    end;
    try
      tvi := TTVInfoDB.Create(rls_showname);
      tvi.tv_showid := tvinfodb.column_text(gettvrage, 3);
      tvi.tv_tvrageid := tvinfodb.column_text(gettvrage, 5);
      tvi.tv_tvmazeid := tvinfodb.column_text(gettvrage, 6);

      tvi.tv_showname := tvinfodb.column_text(gettvrage, 1);
      tvi.tv_premiered_year := StrToIntDef(tvinfodb.column_text(gettvrage,
        7), 0);
      tvi.tv_country := tvinfodb.column_text(gettvrage, 8);
      tvi.tv_status := tvinfodb.column_text(gettvrage, 9);
      tvi.tv_classification := tvinfodb.column_text(gettvrage, 10);
      tvi.tv_genres.CommaText := tvinfodb.column_text(gettvrage, 12);
      tvi.tv_network := tvinfodb.column_text(gettvrage, 11);
      tvi.tv_running := Boolean(lowercase(tvi.tv_status) = 'running');
      tvi.tv_scripted := Boolean(lowercase(tvi.tv_classification) = 'scripted');
      tvi.last_updated := StrToIntDef(tvinfodb.column_text(gettvrage, 14), -1);
      tvi.tv_next_date := StrToIntDef(tvinfodb.column_text(gettvrage, 15), -1);
      tvi.tv_next_season := StrToIntDef(tvinfodb.column_text(gettvrage, 16), -1);
      tvi.tv_next_ep := StrToIntDef(tvinfodb.column_text(gettvrage, 17),
        -1);
      result := tvi;
    except
      on e: Exception do
      begin
        Result := nil;
        Debug(dpError, section, Format('[EXCEPTION] getTheTVDBbyShowName: %s ',
          [e.Message]));
      end;
    end;

  end;

  // result := fillTTheTvDBfromDB(gettvrage, rls_showname);
//  end;
end;

function getTVInfoByReleaseName(rls: string): TTVInfoDB;
var
  showname: string;
  rx: TRegexpr;
begin
  Result := nil;
  showname := rls;
  rx := TRegexpr.Create;
  try
    rx.ModifierI := True;

    rx.Expression :=
      '(.*)[\._-](\d{4}[\._-]\d{2}[\._-]\d{2}|\d{2}[\._-]\d{2}[\._-]\d{4})[\._-](.*)';
    if rx.Exec(rls) then
    begin
      showname := rx.Match[1];
    end;

    rx.Expression := '(.*)[\._-](\d+)x(\d+)[\._-](.*)';
    if rx.Exec(rls) then
      showname := rx.Match[1];

    rx.Expression :=
      '(.*)[\._-]S(\d{1,3})(\.?([DE]|EP|Episode|Part)(\d{1,4})\w?)?[\._-](.*)';
    if rx.Exec(rls) then
      showname := rx.Match[1];
    rx.Expression := '[\.\_]';
    showname := rx.Replace(showname, ' ');
  finally
    rx.Free;
  end;

  if (showname <> '') then
  begin
    Result := getTVInfoByShowName(showname);
  end;
end;

function getTVInfoByShowID(tv_showid: string): TTVInfoDB;
var
  i: integer;
  tvi: TTVInfoDB;
  gettvrage: Psqlite3_stmt;
begin
  Result := nil;
  if tvinfodb = nil then
    exit;

  //result:=getTVDBByIDFromMemory(tv_showid);

  if (Result = nil) then
  begin
    try

      gettvrage := tvinfodb.Open(
        'SELECT * FROM series LEFT JOIN infos ON infos.tvdb_id = series.id WHERE id = ' + tv_showid + ';');

      if tvinfodb.Step(gettvrage) then
      begin
        tvi := TTVInfoDB.Create(tvinfodb.column_text(gettvrage, 0));
        tvi.tv_showid := tvinfodb.column_text(gettvrage, 3);
        tvi.tv_tvrageid := tvinfodb.column_text(gettvrage, 5);
        tvi.tv_tvmazeid := tvinfodb.column_text(gettvrage, 6);

        tvi.tv_showname := tvinfodb.column_text(gettvrage, 1);
        tvi.tv_premiered_year := StrToIntDef(tvinfodb.column_text(gettvrage,
          7), 0);
        tvi.tv_country := tvinfodb.column_text(gettvrage, 8);
        tvi.tv_status := tvinfodb.column_text(gettvrage, 9);
        tvi.tv_classification := tvinfodb.column_text(gettvrage, 10);
        tvi.tv_genres.CommaText := tvinfodb.column_text(gettvrage, 12);
        tvi.tv_network := tvinfodb.column_text(gettvrage, 11);
        tvi.tv_running := Boolean(lowercase(tvi.tv_status) = 'running');
        tvi.tv_scripted := Boolean(lowercase(tvi.tv_classification) = 'scripted');
        tvi.last_updated := StrToIntDef(tvinfodb.column_text(gettvrage, 14), -1);
        tvi.tv_next_date := StrToIntDef(tvinfodb.column_text(gettvrage, 15), -1);
        tvi.tv_next_season := StrToIntDef(tvinfodb.column_text(gettvrage, 16), -1);
        tvi.tv_next_ep := StrToIntDef(tvinfodb.column_text(gettvrage, 17),
          -1);
        result := tvi;
      end;

    except
      on e: Exception do
      begin
        Debug(dpError, section,
          Format('[EXCEPTION] getTheTVDbByShowID: %s ',
          [e.Message]));
      end;
    end;
  end;
end;

procedure addTVInfos(params: string);
var
  rls: string;
  tv_showid: string;
begin
  rls := '';
  rls := SubString(params, ' ', 1);
  tv_showid := '';
  tv_showid := SubString(params, ' ', 2);

  try
    AddTask(TPazoHTTPTVInfoTask.Create(tv_showid, rls));
  except
    on e: Exception do
    begin
      Debug(dpError, section,
        Format('Exception in addTheTVDBInfos AddTask: %s', [e.Message]));
      exit;
    end;
  end;
end;

procedure saveTVInfos(tv_showid: string; tvrage: TTVInfoDB; rls: string = '');
var
  save_tvrage: TTVInfoDB;
begin
  if (getTVInfoByShowID(tv_showid) = nil) then
  begin
    // add the tvrage
    save_tvrage := TTVInfoDB(tvrage);
    try
      //      last_addtvrage.AddObject(tvrage.tv_showname, tvrage);
      if (rls <> '') then
        irc_Addtext_by_key('ADDTVRAGE', '!addtvrage ' + rls + ' ' + tv_showid);
    except
      on e: Exception do
      begin
        Debug(dpError, section,
          Format('Exception in saveTheTVDbInfos last_addtvrage.Add: %s',
          [e.Message]));
        exit;
      end;
    end;

    try
      save_tvrage.Save;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] tvrage.Save: %s ',
          [e.Message]));
      end;
    end;

    if (rls <> '') then
    begin
      TVInfoFireKbAdd(rls);
    end;

  end;
end;

procedure TVInfoFireKbAdd(rls: string);
var
  p: TPazo;
  ps: TPazoSite;
begin
  p := FindPazoByRls(rls);
  if (p <> nil) then
  begin
    ps := FindMostCompleteSite(p);
    if ((ps = nil) and (p.sites.Count > 0)) then
      ps := TPazoSite(p.sites[0]);

    if (ps <> nil) then
    begin
      try
        if spamcfg.ReadBool('addinfo', 'tvrageupdate', True) then
          irc_Addadmin(Format('<c3>[TTVRelease]</c> %s %s now has TV infos (%s)',
            [p.rls.section, p.rls.rlsname, ps.Name]));
        kb_Add('', '', ps.Name, p.rls.section, '', 'UPDATE', p.rls.rlsname, '');
      except
        on e: Exception do
        begin
          Debug(dpError, section, '[EXCEPTION] TTVRelease_kb_Add : %s',
            [e.Message]);
        end;
      end;
    end;
  end;
end;

end.

