unit dbtvinfo;

interface

uses Classes, IniFiles, irc, slsqlite, kb, Contnrs;

type
  TTVInfoDB = class
  public
    ripname: AnsiString;
    rls_showname: AnsiString;
    tvmaze_id: AnsiString;
    thetvdb_id: AnsiString;
    tvrage_id: AnsiString;

    tv_showname: AnsiString;
    tv_country: AnsiString;
    tv_url: AnsiString;
    tv_status: AnsiString;
    tv_classification: AnsiString;
    tv_genres: TStringList;
    tv_days: TStringList;
    tv_network: AnsiString;
    tv_language: AnsiString;
    tv_premiered_year: integer;
    tv_endedyear: integer;
    tv_running: boolean;
    tv_scripted: boolean;
    tv_next_season: integer;
    tv_next_ep: integer;
    tv_next_date: integer;
    last_updated: integer;
    tv_daily: boolean;
    constructor Create(rls_showname: AnsiString); //overload;
    destructor Destroy; override;
    function Name: AnsiString;
    procedure Save;

    procedure PostResults(rls: AnsiString = ''; netname: AnsiString = ''; channel: AnsiString = '');
    procedure SetTVDbRelease(tr: TTVRelease);
    function Update(fromIRC:Boolean = False): boolean;

    function executeUpdate: boolean;

    procedure setTheTVDbID(id: integer);
    procedure setTVRageID(id: integer);
  end;

function getTVInfoCount: integer;
function getTVInfoSeriesCount: integer;

function TheTVDbStatus: AnsiString;

procedure dbTVInfoInit;
procedure dbTVInfoStart;
procedure dbTVInfoUnInit;

function getTVInfoByShowName(rls_showname: AnsiString): TTVInfoDB;
function getTVInfoByReleaseName(rls: AnsiString): TTVInfoDB;

function getTVInfoByShowID(tvmaze_id: AnsiString): TTVInfoDB;

procedure saveTVInfos(tvmaze_id: AnsiString; tvrage: TTVInfoDB; rls: AnsiString = ''; fireKb: boolean = True);

function deleteTVInfoByID(id: AnsiString): Integer;
function deleteTVInfoByRipName(Name: AnsiString): Integer;

procedure addTVInfos(params: AnsiString);

procedure TVInfoFireKbAdd(rls: AnsiString; msg: AnsiString = '<c3>[TVInfo]</c> %s %s now has TV infos (%s)');

function dbTVInfo_Process(net, chan, nick, msg: AnsiString): boolean;

procedure getShowValues(rip: AnsiString; out showName: AnsiString; out season: integer; out episode: int64); overload;
procedure getShowValues(rip: AnsiString; out showName: AnsiString); overload;

function replaceTVShowChars(name: AnsiString; forWebFetch: boolean = false): AnsiString;

function TVInfoDbAlive: boolean;

implementation

uses DateUtils, SysUtils, Math, configunit, mystrings, irccommandsunit, console, ircblowfish, sitesunit, queueunit, slmasks, slhttp, regexpr, debugunit,
  tasktvinfolookup, pazo, mrdohutils, uLkJSON;

const
  section = 'tasktvinfo';

var
  tvinfodb: TslSqliteDB = nil;
  addtinfodbcmd: AnsiString;

function replaceTVShowChars(name: AnsiString; forWebFetch: boolean = false): AnsiString;
begin
  //this is a protction!!!!  Dispatches will not end up in Disp@ches
  name := Csere(name, ' ', '.');
  name := Csere(name, '.and.', '.&.');
  name := Csere(name, '.at.', '.@.');
  name := Csere(name, '_and_', '_&_');
  name := Csere(name, '_at_', '_@_');
  name := Csere(name, '', Chr(39));
  if forWebFetch then
  begin
    name := Csere(name, ' ', '+');
    name := Csere(name, '.', '+');
    name := Csere(name, '_', '+');
  end;
  if name[Length(name)] = '+' then
    Delete(name,Length(name),1);
  result := name;
end;

procedure getShowValues(rip: AnsiString; out showName: AnsiString);
var
  season: integer;
  episode: int64;
begin
  getShowValues(rip, showName, season, episode);
end;

procedure getShowValues(rip: AnsiString; out showname: AnsiString; out season: integer; out episode: int64);
var
  rx: TRegexpr;
  ttags: TStringlist;
begin
  rx := TRegexpr.Create;
  showName := rip;
  try
    rx.ModifierI := True;
    rx.ModifierG := True;


    (* dated shows like Stern.TV.2016.01.27.GERMAN.Doku.WS.dTV.x264-FiXTv *)
    (* YYYY/MM/DD *)
    rx.Expression := '(.*)[\._-](\d{4})[\.\-](\d{2})[\.\-](\d{2}|\d{2}[\.\-]\d{2}[\.\-]\d{4})[\._-](.*)';
    if rx.Exec(rip) then
    begin
      showname := rx.Match[1];
      if DateUtils.IsValidDate(StrToInt(rx.Match[2]), StrToInt(rx.Match[3]), StrToInt(rx.Match[4])) then
      begin
        season := -99;
        episode := DateTimeToUnix(StrToDateTime(rx.Match[4] + {$IFDEF MSWINDOWS}DateSeparator{$ELSE}DefaultFormatSettings.DateSeparator{$ENDIF} + rx.Match[3] + {$IFDEF MSWINDOWS}DateSeparator{$ELSE}DefaultFormatSettings.DateSeparator{$ENDIF} + rx.Match[2]));
      end
      else
      begin
        irc_Adderror('<c4><b>ERROR</c></b>: ' + rx.Match[4] + {$IFDEF MSWINDOWS}DateSeparator{$ELSE}DefaultFormatSettings.DateSeparator{$ENDIF} + rx.Match[3] + {$IFDEF MSWINDOWS}DateSeparator{$ELSE}DefaultFormatSettings.DateSeparator{$ENDIF} + rx.Match[2] + ' is no vailed date.');
        Debug(dpError, section, 'ERROR: ' + rx.Match[4] + {$IFDEF MSWINDOWS}DateSeparator{$ELSE}DefaultFormatSettings.DateSeparator{$ENDIF} + rx.Match[3] + {$IFDEF MSWINDOWS}DateSeparator{$ELSE}DefaultFormatSettings.DateSeparator{$ENDIF} + rx.Match[2] + ' is no vailed date.');
      end;
      exit;
    end;


    (* regular series tagging like S01E02 and 1x02 *)
    rx.Expression := '(.*?)[._-](S(\d{1,3})(E(\d{1,3}))?|(\d+)x(\d+))';
    if rx.Exec(rip) then
    begin
      showname := rx.Match[1];
      if StrToIntDef(rx.Match[3], 0) > 0 then
      begin
        season := StrToIntDef(rx.Match[3], 0);
        episode := StrToIntDef(rx.Match[5], 0);
        exit;
      end;

      if StrToIntDef(rx.Match[6], 0) > 0 then
      begin
        season := StrToIntDef(rx.Match[6], 0);
        episode := StrToIntDef(rx.Match[7], 0);
        exit;
      end;
    end;

    rx.Expression := '(.*?)[._-]((S(taffel)?)(\d{1,3}))?[._]?(D|EP|Episode|DVD[._]?|Part[_.]?)(\d{1,3})(.*?)';
    if rx.Exec(rip) then
    begin
      showname := rx.Match[1];
      episode := StrToIntDef(rx.Match[7], 0);

      if StrToIntDef(rx.Match[5], 0) > 0 then
        episode := StrToIntDef(rx.Match[5], 0)
      else
        episode := StrToIntDef(rx.Match[7], 0);
      Exit;
    end;

    rx.Expression := '(.*?)[._-]((W|V|S(taffel|eason|aison))[._]?(\d{1,3})[._]?)?(SE|DIS[CK]|Y|E|EPS?|VOL(UME)?)[._]?(\d{1,3}).*?';
    if rx.Exec(rip) then
    begin
      showname := rx.Match[1];

      if StrToIntDef(rx.Match[4], 0) > 0 then
      begin
        episode := StrToIntDef(rx.Match[4], 0);
        season := StrToIntDef(rx.Match[4], 0);
      end
      else
      begin
        episode := StrToIntDef(rx.Match[7], 0);
        season := StrToIntDef(rx.Match[7], 0);
      end;
      Exit;
    end;


    (* remove scene/tv tags from releasename *)
    ttags := TStringlist.Create;
    try
      ttags.Assign(tvtags);
      ttags.Delimiter := '|';
      rx.Expression := '[._-\s]((19|20)\d{2}|720p|1080p|' + ttags.DelimitedText + ').*$';
      season := 0;
      episode := 0;
      showName := rx.Replace(rip, '', False);
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
      tvinfodb.ExecSQL(Format('INSERT OR IGNORE INTO infos (tvdb_id,premiered_year,country,status,classification,network,genre,ended_year,last_updated,tvrage_id,tvmaze_id,airdays,next_date,next_season,next_episode,tv_language) VALUES '+
      '(%1:d,%2:d,%0:s%3:s%0:s,%0:s%4:s%0:s,%0:s%5:s%0:s,%0:s%6:s%0:s,%0:s%7:s%0:s,%8:d,%9:d,%10:d,%11:d,%0:s%12:s%0:s,%13:d,%14:d,%15:d,%0:s%16:s%0:s)',
      [chr(39),StrToIntDef(thetvdb_id, -1),tv_premiered_year,tv_country,
      tv_status,tv_classification,tv_network,tv_genres.CommaText,
      tv_endedyear,DateTimeToUnix(now()),StrToIntDef(tvrage_id, -1),
      StrToInt(tvmaze_id),tv_days.CommaText,tv_next_date,tv_next_season,
      tv_next_ep,tv_language])) then
      last_updated := DateTimeToUnix(now())
    else begin
      last_updated := 3817;
    end;

  except on E: Exception do
      Irc_AddAdmin('<c4><b>Exception</c></b>: TTVInfoDB.INSERT infos %s', [e.Message]);
  end;

  try
    tvinfodb.ExecSQL(Format('INSERT OR IGNORE INTO series (rip,showname,id,tvmaze_url) VALUES (%0:s%1:s%0:s,%0:s%2:s%0:s,%3:d,%0:s%4:s%0:s);',
    [chr(39),rls_showname, tv_showname, StrToInt(tvmaze_id), tv_url]));
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
  tr.tvlanguage := tv_language;

  if YearOf(now) = tv_next_season then
  begin
    tv_next_season := tr.season;
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
    PostResults(rls_showname);

end;

constructor TTVInfoDB.Create(rls_showname: AnsiString);
begin
  self.rls_showname := rls_showname;
  self.tv_genres := TStringList.Create;
  self.tv_genres.QuoteChar := '"';
  self.tv_days := TStringList.Create;
  self.tv_days.QuoteChar := '"';
  self.tv_endedyear := -1;
  self.last_updated:= 3817;

end;

destructor TTVInfoDB.Destroy;
begin
  self.tv_genres.Free;
  self.tv_days.free;
  inherited;
end;

function TTVInfoDB.Name: AnsiString;
begin
  try
    Result := 'TVInfo :' + rls_showname + ' : ';
  except
    Result := 'TVInfo';
  end;
end;

procedure TTVInfoDB.PostResults(rls: AnsiString = ''; netname: AnsiString = ''; channel: AnsiString = '');
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
        toAnnounce.add(Format('<c10>[<b>TVInfo</b>]</c> <b>Season</b> %d - <b>Episode</b> %d - <b>Date</b> %s', [tv_next_season, tv_next_ep, FormatDateTime('yyyy-mm-dd', UnixToDateTime(tv_next_date))]));

      toAnnounce.add(Format('<c10>[<b>TVInfo</b>]</c> <b>Genre</b> %s - <b>Classification</b> %s - <b>Status</b> %s', [tv_genres.CommaText, tv_classification, tv_status]));
      toAnnounce.add(Format('<c10>[<b>TVInfo</b>]</c> <b>Country</b> %s - <b>Network</b> %s - <b>Language</b> %s', [tv_country, tv_network,tv_language]));
      toAnnounce.add(Format('<c10>[<b>TVInfo</b>]</c> <b>Last update</b> %s', [DateTimeToStr(UnixToDateTime(last_updated))]));
    end
    else
    begin

      if tv_endedyear > 0 then
        toAnnounce.add(Format('(<c9>i</c>)....<c7><b>TVInfo (db)</b></c>....... <c0><b>info for</c></b> ...........: <b>%s</b> (%s - %s) - %s', [rls, IntToStr(tv_premiered_year), IntToStr(tv_endedyear), tv_url]))
      else
        toAnnounce.add(Format('(<c9>i</c>)....<c7><b>TVInfo (db)</b></c>....... <c0><b>info for</c></b> ...........: <b>%s</b> (%s) - %s', [rls, IntToStr(tv_premiered_year), tv_url]));

      if ((tv_next_season > 0) and (tv_next_ep > 0)) then
        toAnnounce.add(Format('(<c9>i</c>)....<c7><b>TVInfo (db)</b></c>....... <c9><b>Season/Episode (Date)</c></b> ...........: <b>%d.%d</b> (%s)', [tv_next_season, tv_next_ep, FormatDateTime('yyyy-mm-dd', UnixToDateTime(tv_next_date))]));

      toAnnounce.add(Format('(<c9>i</c>)....<c7><b>TVInfo (db)</b></c>.. <c9><b>Genre (Class) @ Status</c></b> ..: %s (%s) @ %s', [tv_genres.CommaText, tv_classification, tv_status]));
      toAnnounce.add(Format('(<c9>i</c>)....<c7><b>TVInfo (db)</b></c>....... <c4><b>Country/Channel</c></b> ....: <b>%s</b> (%s) ', [tv_country, tv_network]));
      toAnnounce.add(Format('(<c9>i</c>)....<c7><b>TVInfo (db)</b></c>....... <c4><b>Last update</c></b> ....: <b>%s</b>', [FormatDateTime('yyyy-mm-dd hh:nn:ss', UnixToDateTime(last_updated))]));
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



function TTVInfoDB.executeUpdate: Boolean;
var
  update_sql: String;
begin
  Result := False;
  update_sql := Format('UPDATE infos SET ' +

     // columns
    'tvdb_id = %d, ' +
    'tvrage_id = %d, ' +
    'status = %s, ' +
    'country = %s, ' +
    'tv_language = %s, ' +
    'network = %s, ' +
    'classification = %s, ' +
    'genre = %s, ' +
    'airdays = %s, ' +
    'premiered_year = %d, ' +
    'ended_year = %d, ' +
    'next_date = %d, ' +
    'next_season = %d, ' +
    'next_episode = %d, ' +
    'last_updated = %d ' +

     // condition
    'WHERE tvmaze_id = %d;',

      [
        // data
        StrToIntDef(thetvdb_id, -1),
        StrToIntDef(tvrage_id, -1),
        QuotedStr(tv_status),
        QuotedStr(tv_country),
        QuotedStr(tv_language),
        QuotedStr(tv_network),
        QuotedStr(tv_classification),
        QuotedStr(tv_genres.CommaText),
        QuotedStr(tv_days.CommaText),
        tv_premiered_year,
        tv_endedyear,
        tv_next_date,
        tv_next_season,
        tv_next_ep,
        DateTimeToUnix(now()),

        // condition value
        StrToInt(tvmaze_id)
      ]
  );

  try
    if tvinfodb.ExecSQL(update_sql) then
      Result := True
    else
      Debug(dpError, section, 'ERROR: TTVInfoDB.executeUpdate. Query failed: Query was: %s', [update_sql]);
  except on E: Exception do
    irc_Adderror(Format('<c4>[EXCEPTION]</c> TTVInfoDB.executeUpdate: %s', [e.Message]));
  end;
end;

function TTVInfoDB.Update(fromIRC: boolean = False): boolean;
var
  rls_name: AnsiString;
  respo: AnsiString;
begin

  // Update asked from irc. Update and exit.
  if fromIRC then
  begin
    try
      Result := executeUpdate;
    except on E: Exception do
      irc_Adderror(Format('<c4>[EXCEPTION]</c> TTVInfoDB.Update (from IRC): %s', [e.Message]));
    end;
    exit;
  end;

  // Update from event
  // Note: variable will be overwriten by parseTVMazeInfos
  rls_name := self.ripname;
  Result := False;
  respo := slUrlGet('http://api.tvmaze.com/shows/' + tvmaze_id + '?embed[]=nextepisode&embed[]=previousepisode');

  if respo = '' then
  begin
    irc_Adderror('<c4><b>Error</c></b>: TVMaze api response was empty.');
    Debug(dpError, section, 'TVMaze api response was empty.');
    exit;
  end;

  try
    self := parseTVMazeInfos(respo);
  except on e: Exception do
    begin
      irc_AddError(Format('<c4>[EXCEPTION]</c> TTVInfoDB.Update: %s', [e.Message]));
      Debug(dpError, section, 'TTVInfoDB.Update: %s', [e.Message]);
      exit;
    end;
  end;

  try
    Result := executeUpdate;
  except on E: Exception do
    irc_Adderror(Format('<c4>[EXCEPTION]</c> TTVInfoDB.Update: %s', [e.Message]));
  end;

  try
    if Result then
      TVInfoFireKbAdd(rls_name, '<c9>[TVInfo]</c> Updated -> %s %s (%s)');
  except on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TTVInfoDB.Update.fireKB: %s ', [e.Message]));
      irc_Adderror(Format('<c4>[EXCEPTION]</c> TTVInfoDB.Update.fireKB: %s', [e.Message]));
    end;
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

function TheTVDbStatus: AnsiString;
begin
  Result := Format('<b>TVInfo.db</b>: %d Series, with %d infos', [getTVInfoSeriesCount, getTVInfoCount]);
end;

function deleteTVInfoByID(id: AnsiString): Integer;
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

function deleteTVInfoByRipName(Name: AnsiString): Integer;
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

function getTVInfoByShowName(rls_showname: AnsiString): TTVInfoDB;
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
      tvi.tvmaze_id := tvinfodb.column_text(gettvrage, 4);
      tvi.tv_premiered_year := StrToIntDef(tvinfodb.column_text(gettvrage, 8), -1);
      tvi.tv_endedyear := StrToIntDef(tvinfodb.column_text(gettvrage, 14), -1);
      tvi.tv_country := tvinfodb.column_text(gettvrage, 9);
      tvi.tv_status := tvinfodb.column_text(gettvrage, 10);
      tvi.tv_classification := tvinfodb.column_text(gettvrage, 11);
      tvi.tv_genres.CommaText := tvinfodb.column_text(gettvrage, 13);
      tvi.tv_days.CommaText := tvinfodb.column_text(gettvrage, 19);
      tvi.tv_network := tvinfodb.column_text(gettvrage, 12);
      tvi.tv_running := Boolean( (lowercase(tvi.tv_status) = 'running') or (lowercase(tvi.tv_status) = 'in development') );
      tvi.tv_scripted := Boolean(lowercase(tvi.tv_classification) = 'scripted');
      tvi.last_updated := StrToIntDef(tvinfodb.column_text(gettvrage, 15), -1);
      tvi.tv_next_date := StrToIntDef(tvinfodb.column_text(gettvrage, 16), -1);
      tvi.tv_next_season := StrToIntDef(tvinfodb.column_text(gettvrage, 17), -1);
      tvi.tv_next_ep := StrToIntDef(tvinfodb.column_text(gettvrage, 18), -1);
      tvi.tv_language:= tvinfodb.column_text(gettvrage, 20);

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

function getTVInfoByReleaseName(rls: AnsiString): TTVInfoDB;
var
  showname: AnsiString;
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

function getTVInfoByShowID(tvmaze_id: AnsiString): TTVInfoDB;
var
  tvi: TTVInfoDB;
  gettvrage: Psqlite3_stmt;
begin
  Result := nil;
  if tvinfodb = nil then
    exit;

  if (Result = nil) then
  begin
    try
      gettvrage := tvinfodb.Open('SELECT * FROM series LEFT JOIN infos ON infos.tvmaze_id = series.id WHERE id = ' + tvmaze_id + ';');
      if tvinfodb.Step(gettvrage) then
      begin
        tvi := TTVInfoDB.Create(tvinfodb.column_text(gettvrage, 0));
        tvi.thetvdb_id := tvinfodb.column_text(gettvrage, 5);
        tvi.tvrage_id := tvinfodb.column_text(gettvrage, 6);
        tvi.tvmaze_id := tvinfodb.column_text(gettvrage, 4);
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
        tvi.tv_running := Boolean( (lowercase(tvi.tv_status) = 'running') or (lowercase(tvi.tv_status) = 'in development') );
        tvi.tv_scripted := Boolean(lowercase(tvi.tv_classification) = 'scripted');
        tvi.last_updated := StrToIntDef(tvinfodb.column_text(gettvrage, 15), -1);
        tvi.tv_next_date := StrToIntDef(tvinfodb.column_text(gettvrage, 16), 0);
        tvi.tv_next_season := StrToIntDef(tvinfodb.column_text(gettvrage, 17), 0);
        tvi.tv_next_ep := StrToIntDef(tvinfodb.column_text(gettvrage, 18), 0);
        tvi.tv_language:= tvinfodb.column_text(gettvrage, 20);
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

procedure addTVInfos(params: AnsiString);
var
  rls: AnsiString;
  tv_showid: AnsiString;
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

procedure saveTVInfos(tvmaze_id: AnsiString; tvrage: TTVInfoDB; rls: AnsiString = ''; fireKb: boolean = True);
var
  save_tvrage: TTVInfoDB;
begin
  if (getTVInfoByShowID(tvmaze_id) = nil) then
  begin
    // add the tvrage
    save_tvrage := TTVInfoDB(tvrage);
    try
      if (rls <> '') then
        irc_Addtext_by_key('ADDTVMAZE',Format('%s %s %s',[addtinfodbcmd,rls,tvmaze_id]));
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

procedure TVInfoFireKbAdd(rls: AnsiString; msg: AnsiString = '<c3>[TVInfo]</c> %s %s now has TV infos (%s)');
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
          irc_SendUPDATE(Format(msg, [p.rls.section, p.rls.rlsname, ps.Name]));
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



procedure _updateToV3;
begin
  tvinfodb.ExecSQL('ALTER TABLE infos ADD COLUMN tv_language TEXT;');
  tvinfodb.ExecSQL('PRAGMA user_version = 3;');
end;

procedure dbTVInfoStart;
const currentDbVersion: integer = 3;
var
  db_name, db_params: AnsiString;
  user_version: Psqlite3_stmt;
  uV: integer;
begin
  if slsqlite_inited then
  begin
    db_name := Trim(config.ReadString(section, 'database', 'tvinfos.db'));
    db_params := config.ReadString(section, 'pragma', ' locking_mode=NORMAL;');
    tvinfodb := TslSqliteDB.Create(db_name, db_params);

    // retrieve current db version
    user_version := tvinfodb.Open('PRAGMA user_version;');

    if tvinfodb.Step(user_version) then
      uV := StrToIntDef(tvinfodb.column_text(user_version, 0), -1);

    if not tvinfodb.Close(user_version) then
      Irc_AddAdmin('Unable to close user_version...');

    // decide whether we have to update the db version
    case uV of
      0: tvinfodb.ExecSQL(Format('PRAGMA user_version = %d;', [currentDbVersion]));
      2: _updateToV3;
    end;

    // Create tables and indexes if they don't exist (new file)
    // series table
    tvinfodb.ExecSQL(
      'CREATE TABLE IF NOT EXISTS "series"(' +
        '"rip" TEXT NOT NULL,' +
        '"showname" TEXT NOT NULL,' +
        '"rip_country" TEXT,' +
        '"tvmaze_url" TEXT,' +
        '"id" INTEGER NOT NULL,' +
        '"PRIMARY KEY ("rip")' +
      ');'
      );

    // infos table
    tvinfodb.ExecSQL(
      'CREATE TABLE IF NOT EXISTS "infos"(' +
        '"tvdb_id" INTEGER,' +
        '"tvrage_id" INTEGER,' +
        '"tvmaze_id" INTEGER NOT NULL,' +
        '"premiered_year" INTEGER NOT NULL,' +
        '"country" TEXT NOT NULL DEFAULT unknown,' +
        '"status"  TEXT NOT NULL DEFAULT unknown,' +
        '"classification" TEXT NOT NULL DEFAULT unknown,' +
        '"network" TEXT NOT NULL DEFAULT unknown,' +
        '"genre" TEXT NOT NULL DEFAULT unknown,' +
        '"ended_year" INTEGER,' +
        '"last_updated" INTEGER NOT NULL DEFAULT -1,' +
        '"next_date" INTEGER,' +
        '"next_season" INTEGER,' +
        '"next_episode" INTEGER,' +
        '"airdays" TEXT,' +
        '"tv_language" TEXT,' +
        'PRIMARY KEY ("tvmaze_id" ASC)' +
      ');'
    );

    // indexes
    tvinfodb.ExecSQL('CREATE UNIQUE INDEX IF NOT EXISTS "main"."tvinfo" ON "infos" ("tvmaze_id" ASC);');
    tvinfodb.ExecSQL('CREATE UNIQUE INDEX IF NOT EXISTS "main"."Rips" ON "series" ("rip" ASC);');
  end;

  Console_Addline('', Format('TVInfo db loaded. %d Series, with %d infos', [getTVInfoSeriesCount, getTVInfoCount]));
end;

procedure dbTVInfoInit;
begin
  addtinfodbcmd := config.ReadString(section, 'addcmd', '!addtvmaze');
end;

procedure dbTVInfoUninit;
begin
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

function dbTVInfo_Process(net, chan, nick, msg: AnsiString): boolean;
begin
  Result := False;
  if (1 = Pos(addtinfodbcmd, msg)) then
  begin
    msg := Copy(msg, length(addtinfodbcmd + ' ') + 1, 1000);
    addTVInfos(msg);
    Result := True;
  end;
end;

function TVInfoDbAlive: boolean;
begin
  if tvinfodb = nil then
    Result:=false
  else Result:=true;
end;

end.

