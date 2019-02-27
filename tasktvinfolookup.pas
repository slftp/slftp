unit tasktvinfolookup;

interface

uses
  Classes, pazo, tasksunit, taskrace, xmlwrapper, dbtvinfo, StrUtils;

type
  TPazoTVInfoLookupTask = class(TPazoPlainTask)
  private
    attempt: integer;
    initial_site: String;
  public
    constructor Create(const netname, channel, site: String; pazo: TPazo; attempt: integer = 0);
    function Execute(slot: Pointer): boolean; override;
    function Name: String; override;
  end;

  {* for !addtvmaze channels *}
  TPazoHTTPTVInfoTask = class(TTask)
  private
    rls: String;
    tvmaze_id: String;
  public
    constructor Create(const tvmaze_id: String; rls: String = '');
    function Execute(slot: Pointer): boolean; override;
    function Name: String; override;
  end;

function parseTVMazeInfos(const jsonStr: String; Showname: String = ''; uurl: String = ''): TTVInfoDB;
function findTVMazeIDByName(const name: String; Netname: String = ''; Channel: String = ''): String;

implementation

uses
  DateUtils, Contnrs, SysUtils, queueunit, debugunit, configunit, mystrings, kb,
  http, RegExpr, irc, mrdohutils, uLkJSON, news, sitesunit;

const
  section = 'tasktvinfo';

function findTVMazeIDByName(const name: String; Netname: String = ''; Channel: String = ''): String;
var
  showA, showB, resp: String;
  x: TRegExpr;
  i: integer;
  ddate, res: TStringlist;
  fromIRC, hadYear, hadCountry: boolean;
  tv_country, showName, year, country: String;
  jl: TlkJSONlist;
  fHttpGetErrMsg: String;
begin
  result := 'FAILED';
  hadYear := False;
  hadCountry := False;
  fromIRC := Boolean((Netname <> '') and (Channel <> ''));

  // TODO: guess this call is useless as name is already fine, so name == showName
  getShowValues(name, showName);

  x := TRegExpr.Create;
  try
    x.ModifierI := False;
    x.ModifierM := True;

    // Cut off Year tag
    x.Expression := '[._-\s]((19|20)\d{2})[\s._-]?$';
    if x.Exec(showName) then
    begin
      year := x.Match[1];

      //we add 10 years to current year
      if StrToInt(year) < (StrToInt(FormatDateTime('yyyy', Now)) + 10) then
      begin
        showName := x.Replace(showName, '', False);
        hadYear := True;
      end;
    end;

    // Cut off Country tag
    x.Expression := '[._-\s](US|UK|AU|CA|NZ)[\s._-]?$';
    if x.Exec(showName) then
    begin
      country := x.Match[1];
      showName := x.Replace(showName, '', False);
      hadCountry := True;
    end;
  finally
    x.free;
  end;

  if not HttpGetUrl('https://api.tvmaze.com/search/shows?q=' + replaceTVShowChars(showName, True), resp, fHttpGetErrMsg) then
  begin
    irc_Adderror(Format('<c4>[FAILED]</c> TVMAZE API search by Name for %s --> %s', [replaceTVShowChars(showName, True), fHttpGetErrMsg]));
    Debug(dpError, section, Format('[FAILED] TVMAZE API search by Name for %s --> %s ', [replaceTVShowChars(showName, True), fHttpGetErrMsg]));
    exit;
  end;

  if ((resp = '') or (resp = '[]')) then
  begin
    irc_addtext(Netname, Channel, '<c5><b>TVInfo</c></b>: No search result for %s ( %s )', [ReplaceText(showName, '.', ' '), replaceTVShowChars(showName, true)]);
    Exit;
  end;

  jl := TlkJSONlist.Create;
  try
    try
      jl := TlkJSON.ParseText(resp) as TlkJSONlist;
    except
      on e: Exception do
      begin
        irc_AddText(Netname, Channel, '<c4>[EXCEPTION]</c> findTVMazeIDByName (parsing): %s', [e.Message]);
        Debug(dpError, section, '[EXCEPTION] findTVMazeIDByName (parsing): %s', [e.Message]);
        Exit;
      end;
    end;

    res := TStringlist.Create;
    ddate := TStringlist.Create;
    try
      for I := 0 to jl.Count - 1 do
      begin
        showA := replaceTVShowChars(ReplaceText(showName, '.', ' '));
        showB := replaceTVShowChars(jl.Child[i].Field['show'].Field['name'].Value);

        if onlyEnglishAlpha(showA) = onlyEnglishAlpha(showB) then
        begin
          if hadCountry then
          begin
            if jl.Child[i].Field['show'].Field['network'].SelfType <> jsNull then
            begin
              if jl.Child[i].Field['show'].Field['network'].Field['country'].SelfType <> jsNull then
                tv_country := String(jl.Child[i].Field['show'].Field['network'].Field['country'].Field['code'].Value);
            end;

            if jl.Child[i].Field['show'].Field['webChannel'].SelfType <> jsNull then
            begin
              if jl.Child[i].Field['show'].Field['webChannel'].Field['country'].SelfType <> jsNull then
                tv_country := String(jl.Child[i].Field['show'].Field['webChannel'].Field['country'].Field['code'].Value);
            end;

            if tv_country = 'GB' then
              tv_country := 'UK';

            if UpperCase(tv_country) = UpperCase(country) then
            begin
              if not fromIRC then
              begin
                result := String(jl.Child[i].Field['show'].Field['id'].Value);
                Break;
              end;
            end;
            res.Add(Format('<b>%s %s</b>: %s => %saddtvinfo %s %s %s', [String(jl.Child[i].Field['show'].Field['name'].Value),
              tv_country, String(jl.Child[i].Field['show'].Field['url'].Value), irccmdprefix,
              String(jl.Child[i].Field['show'].Field['id'].Value), ReplaceText(showName, '.', ' '), country])
            );
          end;

          if hadYear then
          begin
            ddate.Delimiter := '-';
            if jl.Child[i].Field['show'].Field['premiered'].SelfType <> jsNull then
              ddate.DelimitedText := String(jl.Child[i].Field['show'].Field['premiered'].Value)
            else
              ddate.DelimitedText := '1970-01-01';
            if year = ddate.Strings[0] then
            begin
              if not fromIRC then
              begin
                result := String(jl.Child[i].Field['show'].Field['id'].Value);
                Break;
              end;
            end;
            res.Add(Format('<b>%s %s</b>: %s => %saddtvinfo %s %s %s', [String(jl.Child[i].Field['show'].Field['name'].Value),
              ddate.Strings[0], String(jl.Child[i].Field['show'].Field['url'].Value), irccmdprefix,
              String(jl.Child[i].Field['show'].Field['id'].Value), ReplaceText(showName, '.', ' '), year])
            );
          end;
        end;

        if ((not hadYear) and (not hadCountry)) then
        begin
          if not fromIRC then
          begin
            result := String(jl.Child[i].Field['show'].Field['id'].Value);
            Break;
          end;

            res.Add(Format('<b>%s</b>: %s => %saddtvinfo %s %s', [String(jl.Child[i].Field['show'].Field['name'].Value),
              String(jl.Child[i].Field['show'].Field['url'].Value), irccmdprefix,
              String(jl.Child[i].Field['show'].Field['id'].Value), ReplaceText(showName, '.', ' ')])
            );
        end;
      end;

      if fromIRC then
      begin
        if (res.Count = 0) then
          result := 'FAILED'
        else
          result := res.CommaText;
      end;
    finally
      ddate.free;
      res.free;
    end;
  finally
    jl.free;
  end;
end;

procedure findCurrentAirDate(json: TlkJSONobject; out season, episode: Integer; out date: TDateTime);
var
  ep_nextnum, ep_prevnum: integer;
  se_nextnum, se_prevnum: integer;
  nextdt, prevdt: TDateTime;
  airt: String;
  formatSettings: TFormatSettings;
  hadPrev, hadNext: boolean;
begin
  se_prevnum := -1;
  ep_prevnum := -1;
  se_nextnum := -1;
  ep_nextnum := -1;

  date := UnixToDateTime(3817); //1.1.1990 031337

  {$IFDEF MSWINDOWS}
    GetLocaleFormatSettings(1033, formatSettings);
  {$ELSE}
    formatSettings := DefaultFormatSettings;
  {$ENDIF}
  formatSettings.ShortDateFormat := 'yyyy-mm-dd'; // Year-Month-Day order
  formatSettings.ShortTimeFormat := 'hh:mm';
  formatSettings.DateSeparator := '-';
  formatSettings.TimeSeparator := ':';

  hadPrev := False;
  hadNext := False;

  try
    if ((json.Field['_embedded'] <> nil) and (json.Field['_embedded'].Field['previousepisode'] <> nil)) then
    begin
      ep_prevnum := StrToIntDef(string(json.Field['_embedded'].Field['previousepisode'].Field['number'].Value), -1);
      se_prevnum := StrToIntDef(string(json.Field['_embedded'].Field['previousepisode'].Field['season'].Value), -1);
      prevdt := UnixToDateTime(0);

      if String(json.Field['_embedded'].Field['previousepisode'].Field['airtime'].Value) = '' then
        airt := '00:00'
      else
        airt := String(json.Field['_embedded'].Field['previousepisode'].Field['airtime'].Value);

      prevdt := StrToDateTime(string(json.Field['_embedded'].Field['previousepisode'].Field['airdate'].Value) + ' ' + airt, formatSettings);
      hadPrev := True;
    end;
  except on e: Exception do
    begin
      Debug(dpError, section, '[EXCEPTION] in findCurrentAirDate (previousepisode): ' + e.Message);
      Irc_AddAdmin('[EXCEPTION] findCurrentAirDate (previousepisode): ' + e.Message);
    end;
  end;

  try
    if ((json.Field['_embedded'] <> nil) and (json.Field['_embedded'].Field['nextepisode'] <> nil)) then
    begin
      ep_nextnum := StrToIntDef(string(json.Field['_embedded'].Field['nextepisode'].Field['number'].Value), -1);
      se_nextnum := StrToIntDef(string(json.Field['_embedded'].Field['nextepisode'].Field['season'].Value), -1);
      nextdt := UnixToDateTime(0);

      if String(json.Field['_embedded'].Field['nextepisode'].Field['airtime'].Value) = '' then
        airt := '00:00'
      else
        airt := String(json.Field['_embedded'].Field['nextepisode'].Field['airtime'].Value);

      nextdt := StrToDateTime(string(json.Field['_embedded'].Field['nextepisode'].Field['airdate'].Value) + ' ' + airt, formatSettings);
      hadNext := True;
    end;
  except on e: Exception do
    begin
      Debug(dpError, section, '[EXCEPTION] findCurrentAirDate (nextepisode): ' + e.Message);
      Irc_AddAdmin('[EXCEPTION] findCurrentAirDate (nextepisode): ' + e.Message);
    end;
  end;

  if ((not hadNext) and (not hadPrev)) then
  begin
    episode := -15;
    season := -15;
    date := UnixToDateTime(3817); //1.1.1970 031337
    exit;
  end;

  if not hadNext then
  begin
    episode := ep_prevnum;
    season := se_prevnum;
    date := prevdt;
    exit;
  end;

  if IsSameDay(prevdt, nextdt) then
  begin
    episode := -5;
    season := -5;
    date := nextdt;
    exit;
  end;

  if (DateTimeToUnix(nextdt)) <= DateTimeToUnix(now()) then
  begin
    // next date is smaller|equal to now()..
    episode := ep_nextnum;
    season := se_nextnum;
    date := nextdt;
    Exit;
  end;

  if (DateTimeToUnix(prevdt) + 86400) >= DateTimeToUnix(now()) then
  begin
    //previous date + 1Day is grater|equal to now()
    episode := ep_prevnum;
    season := se_prevnum;
    date := prevdt;
    Exit;
  end;

  if ((not hadPrev) AND (hadNext)) then
  begin
    if json.Field['status'].SelfType <> jsNull then
    begin
      //somehow the group catch the episode early, maybe a "pre-air-pilot" ...
      if (String(json.Field['status'].Value) = 'In Development') then
      begin
        episode := ep_nextnum;
        season := se_nextnum;
        date := nextdt;
      end;
    end;
    Exit;
  end;

  if (DateTimeToUnix(nextdt)) > DateTimeToUnix(now()) then
  begin
    // nothing before matched and next_date is greater then now, so we took this.
    episode := ep_nextnum;
    season := se_nextnum;
    date := nextdt;
  end;
end;

function parseTVMazeInfos(const jsonStr: String; Showname: String = ''; uurl: String = ''): TTVInfoDB;
var
  tvr: TTVInfoDB;
  i: integer;
  s: String;
  js: TlkJSONobject;
  season, episode: Integer;
  date: TDateTime;
begin
  Result := nil;
  js := nil;

  if Showname <> '' then
    s := ReplaceText(Showname, '.', ' ')
  else
    s := '';

  tvr := TTVInfoDB.Create(s);
  tvr.tv_genres.Sorted := True;
  tvr.tv_genres.Duplicates := dupIgnore;
  js := TlkJSONObject.Create();
  try
    try
      js := TlkJSON.ParseText(jsonStr) as TlkJSONObject;
    except
      on e: Exception do
      begin
        irc_Adderror(format('<c4>[EXCEPTION]</c> parseTVInfos (JSON.ParseText): %s', [e.Message]));
        Debug(dpError, section, '[EXCEPTION] parseTVInfos (JSON.ParseText): %s', [e.Message]);
        exit;
      end;
    end;

    if js = nil then
      Exit;

    tvr.tv_showname := String(js.Field['name'].Value);

    if LowerCase(tvr.tv_showname) = 'not found' then
    begin
      irc_addAdmin('<c14><b>WARNING</c></b>: TVMaze returned a 404 Not Found page for show <b>%s</b>. Show ID changed?', [Showname]);
      Exit;
    end;

    tvr.tvmaze_id := String(js.Field['id'].Value);
    tvr.tv_url := String(js.Field['url'].Value);

    if js.Field['language'].SelfType <> jsNull then
    tvr.tv_language:=String(js.Field['language'].Value);

    if js.Field['status'].SelfType = jsNull then
      tvr.tv_status := 'unknown'
    else
      tvr.tv_status := String(js.Field['status'].Value);

    if js.Field['type'].SelfType = jsNull then
      tvr.tv_classification := 'unknown'
    else
      tvr.tv_classification := String(js.Field['type'].Value);

    tvr.tv_running := Boolean( (lowercase(tvr.tv_status) = 'running') or (lowercase(tvr.tv_status) = 'in development') );
    tvr.tv_scripted := Boolean(lowercase(tvr.tv_classification) = 'scripted');

    if js.Field['externals'].Field['thetvdb'].SelfType <> jsNull then
      tvr.thetvdb_id := String(js.Field['externals'].Field['thetvdb'].Value);

    // TODO: Remove tvrage ?
    if js.Field['externals'].Field['tvrage'].SelfType <> jsNull then
      tvr.tvrage_id := String(js.Field['externals'].Field['tvrage'].Value);

    if js.Field['network'].SelfType = jsNull then
    begin
      if js.Field['webChannel'].SelfType <> jsNull then
      begin
        tvr.tv_network := String(js.Field['webChannel'].Field['name'].Value);

        if js.Field['webChannel'].Field['country'].SelfType = jsNull then
          tvr.tv_country := 'unknown'
        else
          tvr.tv_country := String(js.Field['webChannel'].Field['country'].Field['code'].Value);
      end
      else
      begin
        tvr.tv_network := 'unknown';
        tvr.tv_country := 'unknown';
      end;
    end
    else
    begin
      tvr.tv_network := String(js.Field['network'].Field['name'].Value);

      if js.Field['network'].Field['country'].SelfType = jsNull then
        tvr.tv_country := 'unknown'
      else
        tvr.tv_country := String(js.Field['network'].Field['country'].Field['code'].Value);
    end;

    if tvr.tv_country = 'US' then
      tvr.tv_country := 'USA';
    if tvr.tv_country = 'GB' then
      tvr.tv_country := 'UK';

    if js.Field['schedule'].SelfType <> jsNull then
      for i := 0 to js.Field['schedule'].Field['days'].Count - 1 do
        tvr.tv_days.Add(string(js.Field['schedule'].Field['days'].Child[i].Value));

    if js.Field['genres'].SelfType <> jsNull then
    begin
      for I := 0 to js.Field['genres'].Count - 1 do
        tvr.tv_genres.Add(string(js.Field['genres'].Child[i].Value));
    end;
    Debug(dpSpam, section, 'parseTVMazeInfos (genres): tvmaze_id: %s Genres: %s URL: %s', [tvr.tvmaze_id, tvr.tv_genres.CommaText, uurl]);

    if js.Field['premiered'].SelfType <> jsNull then
      tvr.tv_premiered_year := StrToIntDef(copy(string(js.Field['premiered'].Value), 1, 4), -1)
    else
      tvr.tv_premiered_year := -1;

      tvr.tv_endedyear := -1;
      tvr.tv_next_ep := -10;
      tvr.tv_next_season := -10;
      tvr.tv_next_date := 3817;

    // Show not ended so we check for next.
    if lowercase(tvr.tv_status) <> 'ended' then
    begin
      findCurrentAirDate(js, season, episode, date);
      tvr.tv_next_season := season;
      tvr.tv_next_ep := episode;
      tvr.tv_next_date := DateTimeToUnix(date);
    end
    else
      if ((js.Field['_embedded'] <> nil) and (js.Field['_embedded'].Field['previousepisode'] <> nil)) then
        tvr.tv_endedyear := StrtoIntdef(Copy(string(js.Field['_embedded'].Field['previousepisode'].Field['airdate'].Value), 1, 4), -1);

    tvr.last_updated := DateTimeToUnix(now());
    Result := tvr;
  finally
    js.free;
  end;
end;

{ TPazoTVInfoLookupTask }

constructor TPazoTVInfoLookupTask.Create(const netname, channel, site: String; pazo: TPazo; attempt: integer = 0);
begin
  inherited Create(netname, channel, site, '', pazo);
  self.attempt := attempt;
  self.initial_site := site;
end;

function TPazoTVInfoLookupTask.Execute(slot: Pointer): boolean;
var
  tr: TTVRelease;
  r: TPazoTVInfoLookupTask;
  showA, showB, tvmaz, sid, uurl: String;
  db_tvinfo: TTVInfoDB;
  ps: TPazoSite;
  fHttpGetErrMsg: String;
begin
  tr := TTVRelease(mainpazo.rls);

  // Show is in DataBase? Here we could add some Update routine and CurrentAired EP.
  try
    db_tvinfo := getTVInfoByShowName(tr.showname);
    if (db_tvinfo <> nil) then
    begin
      db_tvinfo.SetTVDbRelease(tr);
      ready := True;
      Result := True;
      exit;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('Exception in getTVInfoByShowName: %s', [e.Message])); // anpassen.
      ready := True;
      Result := True;
      exit;
    end;
  end;

  sid := findTVMazeIDByName(tr.showname);

  //Show is not found in the DB.
  if sid = 'FAILED' then
  begin
    if attempt < config.readInteger(section, 'readd_attempts', 5) then
    begin
      debug(dpSpam, section, 'READD: retrying TVMaze lookup for %s later', [tr.showname]);
      r := TPazoTVInfoLookupTask.Create(netname, channel, initial_site, mainpazo, attempt + 1);
      r.startat := IncSecond(Now, config.ReadInteger(section, 'readd_interval', 60));
      try
        AddTask(r);
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[Exception] in TPazoTVInfoLookupTask Search %s', [e.Message]));
          irc_Adderror(Format('<c4>[Exception]</c> in TPazoTVInfoLookupTask Search %s', [e.Message]));
          readyerror := True;
          Result := True;
          exit;
        end;
      end;
    end
    else
    begin
      debug(dpSpam, section, 'READD: no more attempts for %s...', [tr.showname]);
      irc_addadmin('<c4>ERROR</c> No TVMaze ID found for <b>%s</b> ( %s )', [tr.showname, tr.rlsname]);
      SlftpNewsAdd('TVMAZE', Format('<c4>ERROR</c> No TVMaze ID found for <b>%s</b>', [tr.showname]), True);
    end;

    ready := True;
    Result := True;
    exit;
  end;

  uurl := 'https://api.tvmaze.com/shows/' + sid + '?embed[]=nextepisode&embed[]=previousepisode';

  if not HttpGetUrl(uurl, tvmaz, fHttpGetErrMsg) then
  begin
    Debug(dpError, section, Format('[FAILED] TVMAZE API fetch for show ID %s --> %s', [sid, fHttpGetErrMsg]));
    irc_Adderror(Format('<c4>[FAILED]</c> TVMAZE API fetch for show ID %s --> %s', [sid, fHttpGetErrMsg]));
    Result := True;
    ready := True;
    exit;
  end;

  if ((tvmaz = '') or (tvmaz = '[]')) then
  begin
    irc_addadmin('<c4><b>ERROR</c></b> http response is empty for ' + tr.showname);
    Debug(dpSpam, section, 'ERROR http response is empty for ' + tr.showname);
    Result := True;
    readyerror := True;
    exit;
  end;

  db_tvinfo := parseTVMazeInfos(tvmaz, tr.showname, uurl);

  if db_tvinfo = nil then
  begin
    Debug(dpError, section, 'Error parseTVMazeInfos returns nil.');
    Result := True;
    readyerror := True;
    exit;
  end;

  showA := replaceTVShowChars(db_tvinfo.tv_showname);
  showB := replaceTVShowChars(tr.showname);

  if ((config.ReadBool(section, 'stop_on_englishcheck', True)) and (onlyEnglishAlpha(showA) <> onlyEnglishAlpha(showB))) then
  begin
    irc_addadmin('<c14><b>Info</b></c>: Alphanumeric check dont match! %s <> %s', [onlyEnglishAlpha(showA), onlyEnglishAlpha(showB)]);
    Result := True;
    ready := True;
    exit;
  end;

  try
    irc_Addtext_by_key('ADDTVMAZE', Format('%s %s %s', [config.ReadString(section, 'addcmd', '!addtvmaze'), mainpazo.rls.rlsname, db_tvinfo.tvmaze_id]));
    db_tvinfo.Save;
    db_tvinfo.SetTVDbRelease(tr);
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('Exception in addtvinfo_SaveTVRage: %s', [e.Message]));
      Result := True;
      readyerror := True;
      db_tvinfo.free;
      exit;
    end;
  end;

  try
    ps := FindMostCompleteSite(mainpazo);
    if ((ps = nil) and (mainpazo.sites.Count > 0)) then
      ps := TPazoSite(mainpazo.sites[0]);
  // don't know why ps can be nil - have to check later
      if ps <> nil then
      begin
        kb_add(netname, channel, ps.Name, mainpazo.rls.section, '', 'UPDATE', mainpazo.rls.rlsname, '');
      end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('Exception in TPazoTVInfoLookupTask kb_add: %s', [e.Message]));
    end;
  end;

  db_tvinfo.free;
  ready := True;
  Result := True;
end;

function TPazoTVInfoLookupTask.Name: String;
begin
  try
    Result := format('TVInfo PazoID(%d) %s @ %s attempts(%d)', [mainpazo.pazo_id, mainpazo.rls.rlsname, site1, attempt]);
  except
    Result := 'TVInfo';
  end;
end;

{ TPazoHTTPTVInfoTask }

constructor TPazoHTTPTVInfoTask.Create(const tvmaze_id: String; rls: String = '');
begin
  inherited Create('', '', getAdminSiteName);
  self.tvmaze_id := tvmaze_id;
  self.rls := rls;
end;

function TPazoHTTPTVInfoTask.Name: String;
begin
  try
    Result := Format('HTTP TVMaze lookup via addtvmaze channel : TVID %s for %s', [tvmaze_id, rls]);
  except
    Result := 'HTTP TVMaze lookup via addtvmaze channel';
  end;
end;

function TPazoHTTPTVInfoTask.Execute(slot: Pointer): boolean;
var
  tvdb: TTVInfoDB;
  sname: String;
  fHttpGetErrMsg: String;
begin
  // remove 'scene' tagging
  getShowValues(rls, sname);
  ReplaceText(sname, '.', ' ');
  ReplaceText(sname, '_', ' ');

  if not HttpGetUrl('https://api.tvmaze.com/shows/' + tvmaze_id + '?embed[]=nextepisode&embed[]=previousepisode', response, fHttpGetErrMsg) then
  begin
    Debug(dpMessage, section, Format('[FAILED] No TVMAZE Infos for %s (%s : %s) from addtvmaze channel : %s', [rls, sname, tvmaze_id, fHttpGetErrMsg]));
    irc_Adderror(Format('<c4>[FAILED]</c> No TVMAZE Infos for %s (%s : %s) from addtvmaze channel : %s', [rls, sname, tvmaze_id, fHttpGetErrMsg]));
    Result := True;
    readyerror := True;
    exit;
  end;

  if ((response = '') or (response = '[]')) then
  begin
    irc_Adderror(Format('<c4><b>ERROR</b></c> HTTP Response is empty for %s (%s) from addtvmaze channel', [sname, tvmaze_id]));
    Debug(dpSpam, section, 'ERROR HTTP Response is empty for %s (%s) from addtvmaze channel', [sname, tvmaze_id]);
    Result := True;
    readyerror := True;
    exit;
  end;

  tvdb := parseTVMazeInfos(response, sname);
  try
    if tvdb <> nil then
      saveTVInfos(tvmaze_id, tvdb, rls, False);
  finally
    tvdb.free;
  end;

  ready := True;
  Result := True;
end;

end.
