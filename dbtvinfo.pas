unit dbtvinfo;

interface

uses Classes, IniFiles, irc, slsqlite, kb, Contnrs;

type
  TTVInfoDB = class
  public
    ripname: string;
    rls_showname: string;
    //tv_showid: string;
    tvmaze_id: string;
    thetvdb_id: string;
    tvrage_id: string;

    tv_showname: string;
    tv_country: string;
    tv_url: string;
    tv_status: string;
    tv_classification: string;
    tv_genres: TStringList;
    tv_days: TStringList;
    tv_network: string;
    //tv_runtime: integer;
    tv_premiered_year: integer;
    tv_endedyear: integer;
    tv_running: boolean;
    tv_scripted: boolean;
    tv_next_season: integer;
    tv_next_ep: integer;
    tv_next_date: integer;
    last_updated: integer;
    tv_daily: boolean;
    constructor Create(rls_showname: string); //overload;
    destructor Destroy; override;
    function Name: string;
    procedure Save;

    procedure PostResultsv2(rls: string = ''; netname: string = ''; channel: string = '');
    //    procedure PostResults(rls: string = ''); overload;
    //    procedure PostResults(Netname, Channel: string; rls: string = ''); overload;
    procedure SetTVDbRelease(tr: TTVRelease);
    function UpdateIRC: boolean;
    function Update: boolean;
    procedure setTheTVDbID(id: integer);
    procedure setTVRageID(id: integer);
    //    private
    //     procedure UpdateDBEntry;
  end;

function getTVInfoCount: integer;
function getTVInfoSeriesCount: integer;

function TheTVDbStatus: string;

procedure dbTVInfoInit;
procedure dbTVInfoStart;
procedure dbTVInfoUnInit;

function getTVInfoByShowName(rls_showname: string): TTVInfoDB;
function getTVInfoByReleaseName(rls: string): TTVInfoDB;

function getTVInfoByShowID(tvmaze_id: string): TTVInfoDB;

procedure saveTVInfos(tvmaze_id: string; tvrage: TTVInfoDB; rls: string = ''; fireKb: boolean = True);

function deleteTVInfoByID(id: string): Integer;
function deleteTVInfoByRipName(Name: string): Integer;

procedure addTVInfos(params: string);

procedure TVInfoFireKbAdd(rls: string; msg: string = '<c3>[TVInfo]</c> %s %s now has TV infos (%s)');

function dbTVInfo_Process(net, chan, nick, msg: string): boolean;

function updateToV2: boolean;

procedure getShowValues(rip: string; out showName: string; out season: integer; out episode: int64); overload;
procedure getShowValues(rip: string; out showName: string); overload;

//function findTVMazeIDv2(showname:string):string;
function replaceTVShowChars(name: string; forWebFetch: boolean = false): string;

implementation

uses DateUtils, SysUtils, Math, configunit, mystrings, irccommandsunit, console, ircblowfish, sitesunit, queueunit, slmasks, slhttp, regexpr, debugunit,
  tasktvinfolookup, pazo, mrdohutils, uLkJSON;

const
  section = 'tasktvinfo';
  dbversion: integer = 2;

var
  tvinfodb: TslSqliteDB = nil;
  sql_addtvinfodb: Psqlite3_stmt = nil;
  sql_counttvinfodb: Psqlite3_stmt = nil;

  addtinfodbcmd: string;
  oldtvinfodbcmd: string;

  //  last_addthetvdb: THashedStringList;

function replaceTVShowChars(name: string; forWebFetch: boolean = false): string;
begin
  //this is a protction!!!!  Dispatches will not end up in Disp@ches
  name := Csere(name, ' ', '.');

  //  result := name;
  name := Csere(name, '.and.', '.&.');
  name := Csere(name, '.at.', '.@.');
  name := Csere(name, '_and_', '_&_');
  name := Csere(name, '_at_', '_@_');
  name := Csere(name, '', chr(39));
  if forWebFetch then
  begin
    name := Csere(name, ' ', '+');
    name := Csere(name, '.', '+');
    name := Csere(name, '_', '+');
  end;
  result := name;
end;

procedure getShowValues(rip: string; out showName: string);
var
  season: integer;
  episode: int64;
begin
  getShowValues(rip, showName, season, episode);
end;

procedure getShowValues(rip: string; out showname: string; out season: integer; out episode: int64);
var
  rx: TRegexpr;
  dt: TDateTime;
  ttags: TStringlist;
begin
  rx := TRegexpr.Create;
  showName := rip;
  try
    rx.ModifierI := True;
    //dated shows like Stern.TV.2016.01.27.GERMAN.Doku.WS.dTV.x264-FiXTv //      Y/M/D
    rx.Expression := '(.*)[\._-](\d{4})[\.\-](\d{2})[\.\-](\d{2}|\d{2}[\.\-]\d{2}[\.\-]\d{4})[\._-](.*)';
    if rx.Exec(rip) then
    begin
      showname := rx.Match[1];
      if DateUtils.IsValidDate(StrToInt(rx.Match[2]), StrToInt(rx.Match[3]), StrToInt(rx.Match[4])) then
      begin
        season := -99;
        episode := DateTimeToUnix(StrToDateTime(rx.Match[4] + DateSeparator + rx.Match[3] + DateSeparator + rx.Match[2]));
      end
      else
      begin
        irc_Adderror('<c4><b>ERROR</c></b>: ' + rx.Match[4] + DateSeparator + rx.Match[3] + DateSeparator + rx.Match[2] + ' is no vailed date.');
        Debug(dpMessage, section, 'ERROR: ' + rx.Match[4] + DateSeparator + rx.Match[3] + DateSeparator + rx.Match[2] + ' is no vailed date.');
      end;
      //      episode := DateTimeToUnix(StrToDateTime(Format('%s/%s/%s', [rx.Match[2], rx.Match[4], rx.Match[3]])));
      exit;
    end;

    rx.Expression := '(.*)[\._-](\d+)x(\d+)[\._-](.*)';
    if rx.Exec(rip) then
    begin
      showname := rx.Match[1];
      season := StrToIntDef(rx.Match[2], 0);
      episode := StrToIntDef(rx.Match[3], 0);
      exit;
    end;

    rx.Expression := '(.*)[\._-](S(\d{1,3}))?(\.?([DE]|EP|Episode|Part\.?)(\d{1,4})\w?(E(\d{1,4}))?)?[\._-](.*)';
    if rx.Exec(rip) then
    begin
      showname := rx.Match[1];
      season := StrToIntDef(rx.Match[3], 0);
      if StrToIntDef(rx.Match[8], 0) > 0 then
        episode := StrToIntDef(rx.Match[8], 0)
      else
        episode := StrToIntDef(rx.Match[6], 0);
      Exit;
    end;

    // neither dated nor season-epiode nor disk or anyther known TV-Series tags are found. so we snip up the release scene infos to make a clean tbvmaze lookup,
    // which will fail!

    ttags := TStringlist.Create;
    try
      ttags.Assign(tvtags);
      ttags.Delimiter := '|';
      rx.Expression := '(\d{4}|' + ttags.DelimitedText + ').*$';
      season := 0;
      episode := 0;
      showName := rx.Replace(rip, '');

    finally
      ttags.free;
    end;

  finally
    rx.free;
  end;
end;

{   TTVInfoDB                                 }

procedure TTVInfoDB.setTheTVDbID(id: integer);
begin
  try
    tvinfodb.ExecSQL(Format('UPDATE infos set tvdb_id = %d WHERE tvmaze_id = %s', [id, tvmaze_id]));
  except on E: Exception do
      Irc_AddAdmin('<c4><b>Exception</c></b>: setTheTVDbID: %s', [e.Message]);
  end;

end;

procedure TTVInfoDB.setTVRageID(id: integer);
begin
  try
    tvinfodb.ExecSQL(Format('UPDATE infos set tvrage_id = %d WHERE tvmaze_id = %s', [id, tvmaze_id]));
  except on E: Exception do
      Irc_AddAdmin('<c4><b>Exception</c></b>: setTVRageID: %s', [e.Message]);
  end;

end;

procedure TTVInfoDB.Save;
begin
  try
    if
      tvinfodb.ExecSQL(Format('INSERT OR IGNORE INTO infos (tvdb_id,premiered_year,country,status,classification,network,genre,ended_year,last_updated,tvrage_id,tvmaze_id,airdays,next_date,next_season,next_episode) VALUES (%d,%d,"%s","%s","%s","%s",''%s'',%d,%d,%d,%d,''%s'',%d,%d,%d)',
      [StrToIntDef(thetvdb_id, -1), tv_premiered_year, tv_country, tv_status, tv_classification, tv_network, tv_genres.CommaText, tv_endedyear, DateTimeToUnix(now()),
      StrToIntDef(tvrage_id, -1), StrToInt(tvmaze_id), tv_days.CommaText, tv_next_date, tv_next_season, tv_next_ep])) then
      last_updated := DateTimeToUnix(now())
    else
      last_updated := 3817;

  except on E: Exception do
      Irc_AddAdmin('<c4><b>Exception</c></b>: TTVInfoDB.INSERT infos %s', [e.Message]);
  end;

  try
    tvinfodb.ExecSQL(Format('INSERT OR IGNORE INTO series (rip,showname,id,tvmaze_url) VALUES ("%s","%s",%d,"%s");', [rls_showname, tv_showname, StrToInt(tvmaze_id), tv_url]));
  except on E: Exception do
    begin
      Irc_AddAdmin('<c4><b>Exception</c></b>: TTVInfoDB.INSERT series %s', [e.Message]);
    end;
  end;
end;

procedure TTVInfoDB.SetTVDbRelease(tr: TTVRelease);
begin

  tr.showname := rls_showname;
  tr.thetvdbid := thetvdb_id;
  tr.tvrageid := tvrage_id;
  tr.showid := tvmaze_id;
  tr.premier_year := tv_premiered_year;
  tr.country := tv_country;
  tr.status := tv_status;
  tr.classification := tv_classification;
  tr.genres.Assign(tv_genres);
  tr.network := tv_network;
  tr.running := tv_running;
  tr.ended_year := tv_endedyear;
  tr.scripted := tv_scripted;
  tr.daily := Boolean(tv_days.Count > 1);
  tr.currentseason := false;
  tr.currentepisode := false;
  tr.currentair := false;

  if YearOf(now) = tv_next_season then
  begin
    tv_next_season := tr.season;
    //    tr.currentseason := true;
    //    tr.currentepisode := Boolean(tv_next_ep = tr.episode);
    //    tr.currentair := tr.currentepisode;
  end;

  case tv_next_season of
    -5:
      begin
        //Prev and Next are on the same day.
        tv_next_ep := tr.episode;
        tv_next_season := tr.season;
        tr.currentseason := true;
        tr.currentepisode := true;
        tr.currentair := true;
      end;
    -10:
      begin
        //show is ended.
        tv_next_ep := 0;
        tv_next_season := 0;
        tr.currentseason := False;
        tr.currentepisode := False;
        tr.currentair := False;
      end;
    -15:
      begin
        //neither next nor prev

        if tr.episode > 031337 then
        begin
          // looks like a date tag was found.
          tr.season := YearOf(UnixToDateTime(tr.episode));
          self.tv_next_season := tr.season;
          tr.currentseason := Boolean(CurrentYear = tr.season);
          tr.currentepisode := Boolean(tr.currentseason and (UnixToDateTime(tr.episode + 86400) >= now));
          tr.currentair := tr.currentepisode;
        end;
        self.tv_next_ep := 0;
        tr.episode := 0;

        (*
        CurrentYear
        YearOf(UnixToDateTime(tr.episode));
        MonthOf(UnixToDateTime(tr.episode))
        DayOf(UnixToDateTime(tr.episode+86400))

        if UnixToDateTime(tr.episode+86400) >= now then irc_addAdmin('>=') else irc_addAdmin('<=');
        irc_addAdmin(IntToStr(tr.episode+86400 - DateTimeToUnix(now)));
        *)
      end;
    -99:
      begin
        //dated show
        tr.season := YearOf(UnixToDateTime(tr.episode));
        tv_next_season := YearOf(UnixToDateTime(tv_next_date));
        tr.episode := self.tv_next_ep; // no episode tag, so we must trust tvmaze
        tr.currentseason := Boolean(CurrentYear = tr.season);
        tr.currentepisode := Boolean((CurrentYear = tr.season) and (tv_next_ep = tr.episode));
        tr.currentair := Boolean((tv_next_season = tr.season) and (tv_next_ep = tr.episode));
      end
  else
    begin
      tr.currentseason := Boolean(tv_next_season = tr.season);
      tr.currentepisode := Boolean((tv_next_season = tr.season) and (tv_next_ep = tr.episode));
      tr.currentair := Boolean((tv_next_season = tr.season) and (tv_next_ep = tr.episode));
    end;
  end;

  if config.ReadBool(section, 'post_lookup_infos', false) then
    PostResultsv2(rls_showname);

end;

constructor TTVInfoDB.Create(rls_showname: string);
begin
  self.rls_showname := rls_showname;
  self.tv_genres := TStringList.Create;
  self.tv_genres.QuoteChar := '"';
  self.tv_days := TStringList.Create;
  self.tv_days.QuoteChar := '"';
  self.tv_endedyear := -1;
end;

destructor TTVInfoDB.Destroy;
begin
  self.tv_genres.Free;
  self.tv_days.free;
  inherited;
end;

function TTVInfoDB.Name: string;
begin
  try
    Result := 'TVInfo :' + rls_showname + ' : ';
  except
    Result := 'TVInfo';
  end;
end;

(*
<c3>[<b>TTVRelease</b>]</c><b>Falling Skies</b> -<b>Premiere Year</b> 2011 - <b>The TVDB info</b>http://tvrage.com/shows/id-26205/
<c3>[<b>TTVRelease</b>]</c><b>Genre</b> Action,Military/War,Mystery,Sci-Fi - <b>Classification</b>Scripted - <b>Status</b> Final Season
<c3>[<b>TTVRelease</b>]</c><b>Country</b> USA - <b>Network</b> TNT DRAMA

*)

procedure TTVInfoDB.PostResultsv2(rls: string = ''; netname: string = ''; channel: string = '');
var
  toAnnounce: TStringlist;
  toStats: boolean;
  I: Integer;
begin
  toAnnounce := TStringlist.Create;
  toStats := Boolean((netname = '') and (channel = ''));
  if ((rls = '') or (tvmaze_id = rls)) then
    rls := rls_showname;
  try

    if config.ReadBool(section, 'use_new_announce_style', True) then
    begin
      if tv_endedyear > 0 then
        toAnnounce.add(Format('<c10>[<b>TVInfo</b>]</c> <b>%s</b> (%s - %s) - <b>TVMaze info</b> %s', [rls, IntToStr(tv_premiered_year), IntToStr(tv_endedyear), tv_url]))
      else
        toAnnounce.add(Format('<c10>[<b>TVInfo</b>]</c> <b>%s</b> - <b>Premiere Year</b> %s - <b>TVMaze info</b> %s', [rls, IntToStr(tv_premiered_year), tv_url]));

      if ((tv_next_season > 0) and (tv_next_ep > 0)) then
        toAnnounce.add(Format('<c10>[<b>TVInfo</b>]</c> <b>Season</b> %d - <b>Episode</b> %d - <b>Date</b> %s', [tv_next_season, tv_next_ep, FormatDateTime('yyyy-mm-dd',
            UnixToDateTime(tv_next_date))]));

      toAnnounce.add(Format('<c10>[<b>TVInfo</b>]</c> <b>Genre</b> %s - <b>Classification</b> %s - <b>Status</b> %s', [tv_genres.CommaText, tv_classification, tv_status]));
      toAnnounce.add(Format('<c10>[<b>TVInfo</b>]</c> <b>Country</b> %s - <b>Network</b> %s', [tv_country, tv_network]));
      toAnnounce.add(Format('<c10>[<b>TVInfo</b>]</c> <b>Last update</b> %s', [DateTimeToStr(UnixToDateTime(last_updated))]));
    end
    else
    begin

      if tv_endedyear > 0 then
        toAnnounce.add(Format('(<c9>i</c>)....<c7><b>TVInfo (db)</b></c>....... <c0><b>info for</c></b> ...........: <b>%s</b> (%s - %s) - %s', [rls, IntToStr(tv_premiered_year),
          IntToStr(tv_endedyear), tv_url]))
      else
        toAnnounce.add(Format('(<c9>i</c>)....<c7><b>TVInfo (db)</b></c>....... <c0><b>info for</c></b> ...........: <b>%s</b> (%s) - %s', [rls, IntToStr(tv_premiered_year),
          tv_url]));
      //toAnnounce := toAnnounce+  need to add season and episode infos ?

      if ((tv_next_season > 0) and (tv_next_ep > 0)) then
        toAnnounce.add(Format('(<c9>i</c>)....<c7><b>TVInfo (db)</b></c>....... <c9><b>Season/Episode (Date)</c></b> ...........: <b>%d.%d</b> (%s)', [tv_next_season, tv_next_ep,
          FormatDateTime('yyyy-mm-dd',
            UnixToDateTime(tv_next_date))]));
      toAnnounce.add(Format('(<c9>i</c>)....<c7><b>TVInfo (db)</b></c>.. <c9><b>Genre (Class) @ Status</c></b> ..: %s (%s) @ %s', [tv_genres.CommaText, tv_classification,
        tv_status]));
      toAnnounce.add(Format('(<c9>i</c>)....<c7><b>TVInfo (db)</b></c>....... <c4><b>Country/Channel</c></b> ....: <b>%s</b> (%s) ', [tv_country, tv_network]));
      toAnnounce.add(Format('(<c9>i</c>)....<c7><b>TVInfo (db)</b></c>....... <c4><b>Last update</c></b> ....: <b>%s</b>', [FormatDateTime('yyyy-mm-dd hh:nn:ss',
          UnixToDateTime(last_updated))]));
    end;

    for I := 0 to toAnnounce.Count - 1 do
    begin
      if toStats then
        irc_Addstats(toAnnounce.Strings[i])
      else
        irc_addtext(Netname, Channel, toAnnounce.Strings[i]);
    end;
  finally
    toAnnounce.free;
  end;

end;

function TTVInfoDB.Update: boolean;
var
  rls_name: string;
  respo: string;
  sql: string;
  //  js: TlkJSONobject;
begin
  //variable will be overwriten by  parseTVMazeInfos.
  rls_name := self.ripname;
  result := False;
  respo := slUrlGet('http://api.tvmaze.com/shows/' + tvmaze_id + '?embed[]=nextepisode&embed[]=previousepisode');

  if respo = '' then
  begin
    irc_Adderror('<c4><b>Error</c></b>: TVMaze api response was empty.');
    Debug(dpError, section, 'TVMaze api response was empty.');
    Exit;
  end;

  try
    self := parseTVMazeInfos(respo);
  except on e: Exception do
    begin
      irc_AddError(Format('<c4>[EXCEPTION]</c> TTVInfoDB.Update: %s', [e.Message]));
      Debug(dpError, section, 'TTVInfoDB.Update: %s', [e.Message]);
      Exit;
    end;
  end;

  result :=
    tvinfodb.ExecSQL(Format('UPDATE infos SET tvdb_id = %d, status = "%s", genre = ''%s'', airdays=''%s'' ,ended_year = %d, tvrage_id = %d, last_updated = %d, next_date = %d, next_season = %d, next_episode = %d WHERE tvmaze_id = %d; ',
    [StrToIntDef(thetvdb_id, -1), tv_status, tv_genres.CommaText, tv_days.CommaText, tv_endedyear, StrToIntDef(tvrage_id, -1), DateTimeToUnix(now()), tv_next_date, tv_next_season,
    tv_next_ep,
      StrToInt(tvmaze_id)]));

  try

    if result then
      TVInfoFireKbAdd(rls_name, '<c9>[TVInfo]</c> Updated -> %s %s (%s)');
  except on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TTVInfoDB.Update.fireKB: %s ', [e.Message]));
      irc_Adderror(Format('<c4>[EXCEPTION]</c> TTVInfoDB.Update.fireKB: %s', [e.Message]));
    end;
  end;

end;

function TTVInfoDB.UpdateIRC: boolean;
begin
  try
    result :=
      tvinfodb.ExecSQL(Format('UPDATE infos SET tvdb_id = %d, status = "%s", genre = ''%s'', airdays=''%s'' ,ended_year = %d, tvrage_id = %d, last_updated = %d, next_date = %d, next_season = %d, next_episode = %d WHERE tvmaze_id = %d; ',
      [StrToIntDef(thetvdb_id, -1), tv_status, tv_genres.CommaText, tv_days.CommaText, tv_endedyear, StrToIntDef(tvrage_id, -1), DateTimeToUnix(now()), tv_next_date,
      tv_next_season,
        tv_next_ep,
        StrToInt(tvmaze_id)]));
  except on E: Exception do
      irc_Adderror(Format('<c4>[EXCEPTION]</c> TTVInfoDB.UpdateIRC: %s', [e.Message]));
  end;

end;

{   misc                                       }

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

function TheTVDbStatus: string;
begin
  Result := Format('<b>TVInfo.db</b>: %d Series, with %d infos', [getTVInfoSeriesCount, getTVInfoCount]);
end;

function deleteTVInfoByID(id: string): Integer;
begin
  if not tvinfodb.ExecSQL(Format('DELETE FROM infos WHERE tvmaze_id = %s;', [id])) then
  begin
    result := 10;
    Exit;
  end;
  if not tvinfodb.ExecSQL(Format('DELETE FROM series WHERE id = %s;', [id])) then
  begin
    result := 11;
    Exit;
  end;
  result := 1;
end;

function deleteTVInfoByRipName(Name: string): Integer;
var
  count: integer;
  cinfo: Psqlite3_stmt;
begin
  cinfo := tvinfodb.Open(Format('SELECT COUNT(*) FROM series WHERE rip = %s;', [Name]));
  if tvinfodb.Step(cinfo) then
    count := tvinfodb.column_int(cinfo, 0)
  else
    count := 0;

  case count of
    0:
      begin
        result := 0;
        Exit;
      end;
    1:
      begin
        if not tvinfodb.ExecSQL(Format('DELETE FROM series WHERE rip = %s;', [name])) then
          result := 12
        else
          result := 1;
        Exit;
      end;
  else
    begin
      cinfo := tvinfodb.Open(Format('SELECT id FROM series WHERE rip = %s;', [Name]));
      if tvinfodb.Step(cinfo) then
        result := deleteTVInfoByID(tvinfodb.column_text(cinfo, 0))
      else
        result := 13;
      Exit;
    end;
  end;
end;

function getTVInfoByShowName(rls_showname: string): TTVInfoDB;
var
  tvi: TTVInfoDB;
  gettvrage: Psqlite3_stmt;
begin
  result := nil;

  if tvinfodb = nil then
  begin
    Debug(dpError, section, '[EXCEPTION] getTVInfoByShowName: tvinfodb = nil ');
    exit;
  end;

  if (rls_showname = '') then
  begin
    Debug(dpError, section, '[EXCEPTION] getTVInfoByShowName: rls_showname is empty');
    exit;
  end;

  try
    gettvrage := tvinfodb.Open('SELECT * FROM series LEFT JOIN infos ON infos.tvmaze_id = series.id WHERE rip LIKE "' + rls_showname + '";'); //so we can handle the aka's .
  except on E: Exception do
      Debug(dpError, section, Format('[EXCEPTION] getTheTVDbByShowID.tvinfodb.Open: %s ', [e.Message]));
  end;

  if tvinfodb.Step(gettvrage) then
  begin

    if (LowerCase(rls_showname) <> LowerCase(tvinfodb.column_text(gettvrage,
      0))) then
    begin
      Result := nil;
      Debug(dpError, section, 'fillTTVInfoFromDB LowerCase(rls_showname) <> LowerCase(tvinfodb.column_text(gettvrage,0)))');
      exit;
    end;
    try
      tvi := TTVInfoDB.Create(rls_showname);
      tvi.tv_showname := tvinfodb.column_text(gettvrage, 1);
      tvi.tv_url := tvinfodb.column_text(gettvrage, 3);
      tvi.thetvdb_id := tvinfodb.column_text(gettvrage, 5);
      tvi.tvrage_id := tvinfodb.column_text(gettvrage, 6);
      tvi.tvmaze_id := tvinfodb.column_text(gettvrage, 7);
      tvi.tv_premiered_year := StrToIntDef(tvinfodb.column_text(gettvrage, 8), -1);
      tvi.tv_endedyear := StrToIntDef(tvinfodb.column_text(gettvrage, 14), -1);
      tvi.tv_country := tvinfodb.column_text(gettvrage, 9);
      tvi.tv_status := tvinfodb.column_text(gettvrage, 10);
      tvi.tv_classification := tvinfodb.column_text(gettvrage, 11);
      tvi.tv_genres.CommaText := tvinfodb.column_text(gettvrage, 13);
      tvi.tv_days.CommaText := tvinfodb.column_text(gettvrage, 19);
      tvi.tv_network := tvinfodb.column_text(gettvrage, 12);
      tvi.tv_running := Boolean(lowercase(tvi.tv_status) = 'running');
      tvi.tv_scripted := Boolean(lowercase(tvi.tv_classification) = 'scripted');
      tvi.last_updated := StrToIntDef(tvinfodb.column_text(gettvrage, 15), -1);
      tvi.tv_next_date := StrToIntDef(tvinfodb.column_text(gettvrage, 16), -1);
      tvi.tv_next_season := StrToIntDef(tvinfodb.column_text(gettvrage, 17), -1);
      tvi.tv_next_ep := StrToIntDef(tvinfodb.column_text(gettvrage, 18), -1);

      result := tvi;
    except
      on e: Exception do
      begin
        Result := nil;
        Debug(dpError, section, Format('[EXCEPTION] getTVInfoByShowName: %s ', [e.Message]));
      end;
    end;

  end;
end;

function getTVInfoByReleaseName(rls: string): TTVInfoDB;
var
  showname: string;
  rx: TRegexpr;
begin
  Result := nil;
  showname := rls;
  getShowValues(rls, showname);
  showname := Csere(showname, '.', ' ');
  showname := Csere(showname, '_', ' ');

  if (showname <> '') then
  begin
    Result := getTVInfoByShowName(showname);
  end;
end;

function getTVInfoByShowID(tvmaze_id: string): TTVInfoDB;
var
  //  i: integer;
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
      gettvrage := tvinfodb.Open('SELECT * FROM series LEFT JOIN infos ON infos.tvmaze_id = series.id WHERE id = ' + tvmaze_id + ';');
      if tvinfodb.Step(gettvrage) then
      begin
        tvi := TTVInfoDB.Create(tvinfodb.column_text(gettvrage, 0));
        tvi.thetvdb_id := tvinfodb.column_text(gettvrage, 5);
        tvi.tvrage_id := tvinfodb.column_text(gettvrage, 6);
        tvi.tvmaze_id := tvinfodb.column_text(gettvrage, 7);
        tvi.tv_url := tvinfodb.column_text(gettvrage, 3);
        tvi.tv_showname := tvinfodb.column_text(gettvrage, 1);
        tvi.tv_premiered_year := StrToIntDef(tvinfodb.column_text(gettvrage, 8), -1);
        tvi.tv_endedyear := StrToIntDef(tvinfodb.column_text(gettvrage, 14), -1);
        tvi.tv_country := tvinfodb.column_text(gettvrage, 9);
        tvi.tv_status := tvinfodb.column_text(gettvrage, 10);
        tvi.tv_classification := tvinfodb.column_text(gettvrage, 11);
        tvi.tv_genres.CommaText := tvinfodb.column_text(gettvrage, 13);
        tvi.tv_days.CommaText := tvinfodb.column_text(gettvrage, 19);
        tvi.tv_network := tvinfodb.column_text(gettvrage, 12);
        tvi.tv_running := Boolean(lowercase(tvi.tv_status) = 'running');
        tvi.tv_scripted := Boolean(lowercase(tvi.tv_classification) = 'scripted');
        tvi.last_updated := StrToIntDef(tvinfodb.column_text(gettvrage, 15), -1);
        tvi.tv_next_date := StrToIntDef(tvinfodb.column_text(gettvrage, 16), 0);
        tvi.tv_next_season := StrToIntDef(tvinfodb.column_text(gettvrage, 17), 0);
        tvi.tv_next_ep := StrToIntDef(tvinfodb.column_text(gettvrage, 18), 0);
        result := tvi;
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] getTheTVDbByShowID: %s ', [e.Message]));
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

procedure saveTVInfos(tvmaze_id: string; tvrage: TTVInfoDB; rls: string = ''; fireKb: boolean = True);
var
  save_tvrage: TTVInfoDB;
begin
  if (getTVInfoByShowID(tvmaze_id) = nil) then
  begin
    // add the tvrage
    save_tvrage := TTVInfoDB(tvrage);
    try
      //      last_addtvrage.AddObject(tvrage.tv_showname, tvrage);
      if (rls <> '') then

        irc_Addtext_by_key('ADDTVMAZE', addtinfodbcmd + ' ' + rls + ' ' + tvmaze_id);
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

    if ((rls <> '') and (fireKb)) then
      TVInfoFireKbAdd(rls);

  end;
end;

procedure TVInfoFireKbAdd(rls: string; msg: string = '<c3>[TVInfo]</c> %s %s now has TV infos (%s)');
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
        if spamcfg.ReadBool('addinfo', 'tvinfoupdate', True) then
          //          irc_Addadmin(Format(msg,[p.rls.section, p.rls.rlsname, ps.Name]));
          irc_Addadmin(Format(msg, [p.rls.section, p.rls.rlsname, ps.Name]));
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

function fixMyCrap(userVersion: integer): boolean;
var
  u: Psqlite3_stmt;
begin
  result := false;
  u := tvinfodb.open('SELECT COUNT(*) FROM _infos_old_v1;');
  if (userVersion = 0) then
  begin

    if not tvinfodb.ExecSQL('DROP TABLE IF EXISTS _infos_old_v1') then
    begin

      Debug(dpError, section, '[ERROR] in fixMyCrap: DROP TABLE Failed');
      Exit;
    end;

    if not tvinfodb.ExecSQL('PRAGMA user_version = 2;') then
    begin
      Debug(dpError, section, '[ERROR] fiyMyCrap: PRAGMA user_version = 2');
      Exit;
    end;
    result := True;
  end;

end;

procedure dbTVInfoStart;
var
  uV: integer;
  db_name, db_params: string;
  user_version: Psqlite3_stmt;
begin
  addtinfodbcmd := config.ReadString(section, 'addcmd', '!addtvmaze');
  if slsqlite_inited then
  begin
    db_name := Trim(config.ReadString(section, 'db_file', 'tvinfos.db'));
    db_params := config.ReadString(section, 'pragma', ' locking_mode=NORMAL;');
    tvinfodb := TslSqliteDB.Create(db_name, '');
    // tvinfodb.ExecSQL('PRAGMA locking_mode=normal;');

    //Just fix the poo i did :)
    user_version := tvinfodb.Open('PRAGMA user_version;');
    if tvinfodb.Step(user_version) then
      uV := StrToIntDef(tvinfodb.column_text(user_version, 0), -1);
    if not tvinfodb.Close(user_version) then
      Irc_AddAdmin('konnte user_version nicht closen...');

    if uV = 0 then
    begin

      if not tvinfodb.ExecSQL('DROP TABLE IF EXISTS _infos_old_v1;') then
      begin
        irc_AddAdmin('[ERROR] in fixMyCrap: DROP TABLE Failed ' + sqlite3_errmsg(tvinfodb));
        Debug(dpError, section, '[ERROR] in fixMyCrap: DROP TABLE Failed: ' + sqlite3_errmsg(tvinfodb));
        Exit;
      end
      else
      begin

        tvinfodb.ExecSQL('PRAGMA user_version = 2;');
      end;

    end;

    tvinfodb.ExecSQL('CREATE TABLE IF NOT EXISTS "series" ("rip"  TEXT NOT NULL,"showname"  TEXT NOT NULL,"rip_country"  TEXT,"tvmaze_url"  TEXT,"id"  INTEGER NOT NULL,PRIMARY KEY ("rip"));');

    tvinfodb.ExecSQL(
      'CREATE TABLE IF NOT EXISTS "infos" ("tvdb_id"  INTEGER,"tvrage_id"  INTEGER,"tvmaze_id"  INTEGER NOT NULL,"premiered_year"  INTEGER NOT NULL,"country"  TEXT NOT NULL DEFAULT unknown,"status"  TEXT NOT NULL DEFAULT unknown,' +
      '"classification"  TEXT NOT NULL DEFAULT unknown,"network"  TEXT NOT NULL DEFAULT unknown,"genre"  TEXT NOT NULL DEFAULT unknown,"ended_year"  INTEGER,"last_updated"  INTEGER NOT NULL DEFAULT -1,"next_date"  INTEGER,"next_season"  INTEGER,' +
      '"next_episode"  INTEGER,"airdays"  TEXT,PRIMARY KEY ("tvmaze_id" ASC));');

    tvinfodb.ExecSQL('CREATE UNIQUE INDEX IF NOT EXISTS "main"."tvinfo" ON "infos" ("tvmaze_id" ASC);');
    tvinfodb.ExecSQL('CREATE UNIQUE INDEX IF NOT EXISTS "main"."Rips" ON "series" ("rip" ASC);');

  end;
  Console_Addline('', Format('TVInfo db loaded. %d Series, with %d infos', [getTVInfoSeriesCount, getTVInfoCount]));
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
      Debug(dpError, section, Format('Exception in dbTVInfoUninit: %s', [e.Message]));
  end;
end;

function dbTVInfo_Process(net, chan, nick, msg: string): boolean;
begin
  Result := False;
  if (1 = Pos(addtinfodbcmd, msg)) then
  begin
    msg := Copy(msg, length(addtinfodbcmd + ' ') + 1, 1000);
    addTVInfos(msg);
    Result := True;
  end;
  (*
    if (1 = Pos(oldtvragecmd, msg)) then
    begin
      msg := Copy(msg, length(oldtvragecmd + ' ') + 1, 1000);
      dbaddtvrage_addtvrage(msg);
      Result := True;
    end;
    *)
end;

{
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
   }
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

{  DataBase Version Helper...}

(*
function updateToV3: boolean;
begin
  result := False;
  if tvinfodb = nil then
    Exit;

if not tvinfodb.ExecSQL('DROP TABLE "main"."_infos_old_v1";') then
  begin
    Debug(dpError, section, '[ERROR] in updateToV2: DROP old TABLE Failed');
    Exit;
  end;

end;
*)

function updateToV2: boolean;
begin
  result := False;
  if tvinfodb = nil then
    Exit;

  if not tvinfodb.ExecSQL('ALTER TABLE "main"."infos" RENAME TO "_infos_old_v1";') then
  begin
    Debug(dpError, section, '[ERROR] in updateToV2: ALTER TABLE Failed');
    Exit;
  end;
  if not tvinfodb.ExecSQL('DROP INDEX "main"."tvinfo";') then
  begin
    Debug(dpError, section, '[ERROR] in updateToV2: DROP old TABLE Failed');
    Exit;
  end;

  if not
    tvinfodb.ExecSQL('CREATE TABLE "main"."infos" ("tvdb_id"  INTEGER,"tvrage_id"  INTEGER,"tvmaze_id"  INTEGER NOT NULL,"premiered_year"  INTEGER NOT NULL,"country"  TEXT NOT NULL DEFAULT unknown,"status"  TEXT NOT NULL DEFAULT ' +
    'unknown,"classification"  TEXT NOT NULL DEFAULT unknown,"network"  TEXT NOT NULL DEFAULT unknown,"genre"  TEXT NOT NULL DEFAULT unknown,"ended_year"  INTEGER,"last_updated"  INTEGER NOT NULL DEFAULT -1,"next_date"  ' +
    'INTEGER,"next_season"  INTEGER,"next_episode"  INTEGER,"airdays"  TEXT,PRIMARY KEY ("tvmaze_id" ASC));') then
  begin
    Debug(dpError, section, '[ERROR] in updateToV2: CREATE new Tabel Failed');
    Exit;
  end;

  if not
    tvinfodb.ExecSQL('INSERT INTO "main"."infos" ("tvdb_id", "tvrage_id", "tvmaze_id", "premiered_year", "country", "status", "classification", "network", "genre", "ended_year", "last_updated", "next_date", "next_season", ' +
    '"next_episode") SELECT "tvdb_id", "tvrage_id", "tvmaze_id", "premiered_year", "country", "status", "classification", "network", "genre", "ended_year", "last_updated", "next_date", "next_season", "next_episode" FROM "_infos_old_v1";') then
  begin
    Debug(dpError, section, '[ERROR] in updateToV2: INTER INTO new TABEL Failed');
    Exit;
  end;

  if not tvinfodb.ExecSQL('CREATE UNIQUE INDEX "main"."tvinfo" ON "infos" ("tvmaze_id" ASC);') then
  begin
    Debug(dpError, section, '[ERROR] in updateToV2: CREATE UNIQUE INDEX Failed');
    Exit;
  end;

  if not tvinfodb.ExecSQL('PRAGMA user_version = 2;') then
  begin
    Debug(dpError, section, '[ERROR] in updateToV2: PRAGMA user_version = 2');
    Exit;
  end;

  if not tvinfodb.ExecSQL('DROP TABLE "main"."_infos_old_v1";') then
  begin
    Debug(dpError, section, '[ERROR] in updateToV2: DROP ALTERED TABLE Failed');
    Exit;
  end;

  result := True;
end;

end.

