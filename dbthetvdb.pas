unit dbthetvdb;

interface

uses Classes, IniFiles, irc, slsqlite, kb, Contnrs;

type
  TTheTvDB = class
  private
    last_updated: integer;
  public
    rls_showname: string;
    tv_showid: string;
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
    constructor Create(rls_showname: string); //overload;
    destructor Destroy; override;
    function Name: string;
    procedure Save;
    procedure PostResults(rls: string = ''); overload;
    procedure PostResults(Netname, Channel: string; rls: string = ''); overload;
    procedure SetTVDbRelease(tr: TTVRelease);

  end;

function getTheTVDbInfoCount: integer;
function getTheTVDbSeriesCount: integer;

function TheTVDbStatus: string;

procedure dbthetvdbInit;
procedure dbthetvdbStart;
procedure dbthetvdbUnInit;

function getTheTVDBbyShowName(rls_showname: string): TTheTvDB;
function getTheTVDBbyReleaseName(rls: string): TTheTvDB;
function getTheTVDBbyShowID(tv_showid: string): TTheTvDB;

procedure saveTheTVDbInfos(tv_showid: string; tvrage: TTheTvDB; rls: string =
  '');
procedure addTheTVDBInfos(params: string);
procedure TheTVDbFireKbAdd(rls: string);

implementation

uses DateUtils, SysUtils, Math, configunit, mystrings, irccommandsunit,
  console, ircblowfish,
  sitesunit, queueunit, slmasks, slhttp, regexpr, debugunit,
  taskthetvdblookup, pazo, mrdohutils;

const
  section = 'tasktvdb';

var
  thetvdb: TslSqliteDB = nil;
  sql_addthetvdb: Psqlite3_stmt = nil;
  sql_countthetvdb: Psqlite3_stmt = nil;

  addthetvdbcmd: string;
  oldthetvdbcmd: string;

  last_addthetvdb: THashedStringList;

function getTheTVDbInfoCount: integer;
var
  icount: Psqlite3_stmt;
begin
  icount := thetvdb.Open('SELECT count(*) FROM infos;');
  if thetvdb.Step(icount) then
    result := thetvdb.column_int(icount, 0)
  else
    result := -1;
end;

function getTheTVDbSeriesCount: integer;
var
  icount: Psqlite3_stmt;
begin
  icount := thetvdb.Open('SELECT count(*) FROM series;');
  if thetvdb.Step(icount) then
    result := thetvdb.column_int(icount, 0)
  else
    result := -1;
end;

procedure TTheTvDB.Save;
var
  n: Psqlite3_stmt;
  dbid: integer;
begin
  dbid := -1;

  try
    thetvdb.ExecSQL(Format('INSERT OR IGNORE INTO  infos (tvdb_id,premiered_year,country,status,classification,network,genre,endedyear,last_updated) VALUES (%d,%d,"%s","%s","%s","%s","%s",%d,%d)',
      [StrToInt(tv_showid), tv_premiered_year, tv_country, tv_status,
      tv_classification, tv_network, tv_genres.CommaText, tv_endedyear,
        DateTimeToUnix(now())]));
  except on E: Exception do
    begin
      Irc_AddText('', '', 'Error@TTheTvDB.Save_INSERT infos %s', [e.Message]);
    end;
  end;

  thetvdb.ExecSQL(Format('INSERT OR IGNORE INTO series (rip,showname,id) VALUES ("%s","%s",%d);', [rls_showname, tv_showname, StrToInt(tv_showid)]));
end;

procedure TTheTvDB.SetTVDbRelease(tr: TTVRelease);
begin
  (*
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
    *)
    //  tr.season:= tv_seasons;
    (*
  if config.ReadBool(section, 'post_lookup_infos', false) then
    PostResults(tr.rlsname);
    *)
end;

constructor TTheTvDB.Create(rls_showname: string);
begin
  self.rls_showname := rls_showname;
  self.tv_genres := TStringList.Create;
  self.tv_genres.QuoteChar := '"';
  self.tv_endedyear := -1;
end;

destructor TTheTvDB.Destroy;
begin
  self.tv_genres.Free;
  inherited;
end;

function TTheTvDB.Name: string;
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

procedure TTheTvDB.PostResults(rls: string = '');
begin
  try
    if ((rls = '') or (tv_showid = rls)) then
      rls := rls_showname;
    irc_Addstats(Format('<c3>[<b>TTVRelease</b>]</c> <b>%s</b> - <b>Premiere Year</b> %s - <b>The TVDB info</b> http://thetvdb.com/?tab=series&id=%s', [rls, IntToStr(tv_premiered_year), tv_showid]));
    irc_Addstats(Format('<c3>[<b>TTVRelease</b>]</c> <b>Genre</b> %s - <b>Classification</b> %s - <b>Status</b> %s', [tv_genres.CommaText, tv_classification, tv_status]));
    irc_Addstats(Format('<c3>[<b>TTVRelease</b>]</c> <b>Country</b> %s - <b>Network</b> %s', [tv_country, tv_network]));
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TTheTVDb.PostResultsA: %s ',
        [e.Message]));
      irc_Adderror(Format('<c4>[EXCEPTION]</c> TTheTVDb.PostResultsA: %s',
        [e.Message]));
    end;
  end;
end;

procedure TTheTvDB.PostResults(Netname: string; Channel: string; rls: string =
  '');
begin
  try
    if ((rls = '') or (tv_showid = rls)) then
      rls := rls_showname;
    irc_Addtext(Netname, Channel,
      Format('<c3>[<b>TTVRelease</b>]</c> <b>%s</b> - <b>Premiere Year</b> %s - <b>The TVDB info</b> http://thetvdb.com/?tab=series&id=%s', [rls, IntToStr(tv_premiered_year), tv_showid]));
    irc_Addtext(Netname, Channel,
      Format('<c3>[<b>TTVRelease</b>]</c> <b>Genre</b> %s - <b>Classification</b> %s - <b>Status</b> %s', [tv_genres.CommaText, tv_classification, tv_status]));
    irc_Addtext(Netname, Channel,
      Format('<c3>[<b>TTVRelease</b>]</c> <b>Country</b> %s - <b>Network</b> %s',
      [tv_country, tv_network]));
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TTheTVDb.PostResultsB: %s ',
        [e.Message]));
      irc_Adderror(Format('<c4>[EXCEPTION]</c> TTheTVDb.PostResultsB: %s',
        [e.Message]));
    end;
  end;
end;

function TheTVDbStatus: string;
begin
  Result := Format('<b>TTVRelease.db</b>: %d Series, with %d infos',
    [getTheTVDbSeriesCount, getTheTVDbInfoCount]);
end;

procedure dbTheTVDbStart;
var
  db_name, db_params: string;
begin
  addthetvdbcmd := config.ReadString(section, 'addcmd', '!addthetvdb');
  if slsqlite_inited then
  begin
    db_name := Trim(config.ReadString(section, 'db_file', 'ttvrelease.db'));
    db_params := config.ReadString(section, 'pragma', '');
    thetvdb := TslSqliteDB.Create(db_name, db_params);

    thetvdb.ExecSQL('CREATE TABLE "series" ("rip"  TEXT NOT NULL,"showname"  TEXT NOT NULL,"rip_country"  TEXT,"id"  INTEGER NOT NULL,PRIMARY KEY ("rip"));');
    thetvdb.ExecSQL('CREATE TABLE "infos" ("tvdb_id"  INTEGER PRIMARY KEY NOT NULL ON CONFLICT ABORT COLLATE NOCASE,"premiered_year"  INTEGER NOT NULL,' +
      '"country"  TEXT NOT NULL,"status"  TEXT NOT NULL,"classification"  TEXT NOT NULL,"network"  TEXT NOT NULL,"genre"  TEXT NOT NULL,"endedyear"  INTEGER DEFAULT -1,"last_updated"  INTEGER NOT NULL DEFAULT -1);');

    thetvdb.ExecSQL('CREATE UNIQUE INDEX "main"."tvinfo" ON "infos" ("tvdb_id" ASC);');
    thetvdb.ExecSQL('CREATE UNIQUE INDEX "main"."Rips" ON "series" ("rip" ASC);');

    Console_Addline('',
      Format('TTVRelease SQLite.db loaded. %d Series, with %d infos',
      [getTheTVDbSeriesCount, getTheTVDbInfoCount]));
  end;
end;

procedure dbTheTVDbInit;
begin
  //  last_addthetvdb := THashedStringList.Create;
  //  last_addthetvdb.CaseSensitive := False;
end;

procedure dbTheTVDbUninit;
begin
  //  last_addthetvdb.Free;
  try
    if thetvdb <> nil then
    begin
      thetvdb.Free;
      thetvdb := nil;
    end;
  except on E: Exception do
      Debug(dpError, section,
        Format('Exception in dbTheTVDbUninit: %s',
        [e.Message]));
  end;
end;

function getTVDBByNameFromMemory(name: string): TTheTvDB;
var
  i: integer;
begin
  try
    i := last_addthetvdb.IndexOf(name);
    if i <> -1 then
    begin
      Result := TTheTvDB(last_addthetvdb.Objects[i]);
    end;
  except
    Result := nil;
  end;

end;

function getTVDBByIDFromMemory(id: string): TTheTvDB;
var
  i: integer;
  tvrage: TTheTvDB;
begin
  for i := last_addthetvdb.Count - 1 downto 0 do
  begin
    try
      if i < 0 then
        Break;
    except
      Break;
    end;
    try
      tvrage := TTheTvDB(last_addthetvdb.Objects[i]);
      if (tvrage.tv_showid = id) then
      begin
        Result := tvrage;
        break;
      end;
    except
      break;
    end;
  end;
end;

function fillTTheTvDBfromDB(item: Psqlite3_stmt; show: string = ''): TTheTvDB;
begin
  if thetvdb.Step(item) then
  begin
    if (LowerCase(show) <> LowerCase(thetvdb.column_text(item, 0))) then
    begin
      Result := nil;
      exit;
    end;
    if show = '' then
      show := thetvdb.column_text(item, 0);
    result := TTheTvDB.Create(show);
    result.tv_showid := thetvdb.column_text(item, 3);
    result.tv_showname := thetvdb.column_text(item, 1);
    result.tv_premiered_year := StrToIntDef(thetvdb.column_text(item, 5), 0);
    result.tv_country := thetvdb.column_text(item, 6);
    result.tv_status := thetvdb.column_text(item, 7);
    result.tv_classification := thetvdb.column_text(item, 8);
    result.tv_genres.CommaText := thetvdb.column_text(item, 10);
    result.tv_network := thetvdb.column_text(item, 9);
    result.tv_running := Boolean(lowercase(result.tv_status) = 'running');
    result.tv_scripted := Boolean(lowercase(result.tv_classification) =
      'scripted');
    result.last_updated := StrToIntDef(thetvdb.column_text(item, 12), -1);
  end;
end;

function getTheTVDBbyShowName(rls_showname: string): TTheTvDB;
var
  i: integer;
  //  tvrage: TTheTvDB;
  gettvrage: Psqlite3_stmt;
begin
  Result := nil;
  if thetvdb = nil then
    exit;

  if (rls_showname = '') then
    exit;
  //  result:= getTVDBByNameFromMemory(rls_showname);
  if (Result = nil) then
  begin
    try
      gettvrage := thetvdb.Open(
        'SELECT * FROM series LEFT JOIN infos ON infos.tvdb_id = series.id WHERE rip LIKE "' + rls_showname
        + '";'); //so we can handle the aka's .
      result := fillTTheTvDBfromDB(gettvrage, rls_showname);
    except
      on e: Exception do
      begin
        Result := nil;
        Debug(dpError, section, Format('[EXCEPTION] getTheTVDBbyShowName: %s ',
          [e.Message]));
      end;
    end;
  end;
end;

function getTheTVDBbyReleaseName(rls: string): TTheTvDB;
var
  showname: string;
  rx: TRegexpr;
begin
  Result := nil;
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
    Result := getTheTVDBbyShowName(showname);
  end;
end;

function getTheTVDBbyShowID(tv_showid: string): TTheTvDB;
var
  i: integer;
  tvrage: TTheTvDB;
  gettvrage: Psqlite3_stmt;
begin
  Result := nil;
  if thetvdb = nil then
    exit;

  //result:=getTVDBByIDFromMemory(tv_showid);

  if (Result = nil) then
  begin
    try

      gettvrage := thetvdb.Open(
        'SELECT * FROM series LEFT JOIN infos ON infos.tvdb_id = series.id WHERE id = ' + tv_showid
        + ';');

      tvrage := fillTTheTvDBfromDB(gettvrage);
      Result := tvrage;

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

procedure addTheTVDBInfos(params: string);
var
  rls: string;
  tv_showid: string;
begin
  rls := '';
  rls := SubString(params, ' ', 1);
  tv_showid := '';
  tv_showid := SubString(params, ' ', 2);

  try
    AddTask(TPazoHTTPTheTVDbTask.Create(tv_showid, rls));
  except
    on e: Exception do
    begin
      Debug(dpError, section,
        Format('Exception in addTheTVDBInfos AddTask: %s',
        [e.Message]));
      exit;
    end;
  end;
end;

procedure saveTheTVDbInfos(tv_showid: string; tvrage: TTheTvDB; rls: string =
  '');
var
  save_tvrage: TTheTvDB;
begin
  if (getTheTVDBbyShowID(tv_showid) = nil) then
  begin
    // add the tvrage
    save_tvrage := TTheTvDB(tvrage);
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
      TheTVDbFireKbAdd(rls);
    end;

  end;
end;

procedure TheTVDbFireKbAdd(rls: string);
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

