unit tasktvinfolookup;

interface

uses
  Classes, pazo, tasksunit, taskrace, xmlwrapper, dbtvinfo, StrUtils;

//const apkikey:string = 'FFFFFFFFFFFFFFFF'; //   just a 64bit (16*4) hex string and you are vailed, no RESTful api :P

type
  TPazoTVInfoLookupTask = class(TPazoPlainTask)
  private
    attempt: integer;
    initial_site: AnsiString;
  public
    constructor Create(const netname, channel: AnsiString; site: AnsiString;
      pazo: TPazo; attempt: integer = 0);
    function Execute(slot: Pointer): boolean; override;
    function Name: AnsiString; override;
  end;

  (*didn't work like i wanted :/*)
  TPazoHTTPUpdateTVInfoTask = class(TPazoPlainTask)
  private
    showname: AnsiString;
    tvmaze_id: AnsiString;
  public
    constructor Create(const netname, channel: AnsiString; site: AnsiString; pazo: TPazo; attempt: integer = 0);
    destructor Destroy; override;
    function Execute(slot: Pointer): boolean; override;
    function Name: AnsiString; override;
  end;

  (*for !addmaze channels*)

  TPazoHTTPTVInfoTask = class(TTask)
  private
    rls: AnsiString;
    tvmaze_id: AnsiString;
  public
    constructor Create(const tvmaze_id: AnsiString; rls: AnsiString = '');
    destructor Destroy; override;
    function Execute(slot: Pointer): boolean; override;
    function Name: AnsiString; override;
  end;

function parseTVMazeInfos(jsonStr: AnsiString; Showname: AnsiString = ''; uurl: AnsiString = ''): TTVInfoDB;
function findTheTVDbIDByName(name: AnsiString): AnsiString;
function findTVMazeIDByName(name: AnsiString; Netname: AnsiString = ''; Channel: AnsiString = ''): AnsiString;

implementation

uses
  DateUtils, SysUtils, queueunit, debugunit, configunit, mystrings, kb,
  sltcp, slhttp, RegExpr, irc, mrdohutils, uLkJSON, news;

const
  section = 'tasktvinfo';


function findTVMazeIDByName(name: AnsiString; Netname: AnsiString = ''; Channel: AnsiString = ''): AnsiString;
var
  showA, showB, resp: AnsiString;
  x: TRegExpr;
  i: integer;
  ddate, res: TStringlist;
  fromIRC, hadYear, hadCountry: boolean;
  tv_country, showName, year, country: AnsiString;
  jl: TlkJSONlist;
begin
  result := 'FAILED';
  hadYear := False;
  hadCountry := False;
  fromIRC := Boolean((Netname <> '') and (Channel <> ''));

  x := TRegExpr.Create;
  try
    x.ModifierI := False;
    x.ModifierM := True;
    getShowValues(name, showName);

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

  resp := slUrlGet('https://api.tvmaze.com/search/shows', 'q=' + replaceTVShowChars(showName, true));
  if ((resp = '') or (resp = '[]')) then
  begin
    irc_addtext(Netname, Channel, '<c5><b>TVInfo</c></b>: No search result for %s ( %s )', [Csere(showName, '.', ' '), replaceTVShowChars(showName, true)]);
    Exit;
  end;

  jl := TlkJSONlist.Create;
  try
    jl := TlkJSON.ParseText(resp) as TlkJSONlist;
  except
    on e: Exception do
    begin
      irc_AddText(Netname, Channel, '<c4>[EXCEPTION]</c> IrcAddTVMazeToDb (search): %s', [e.Message]);
      Debug(dpError, section, '[EXCEPTION] rcAddTVMazeToDb (search): %s', [e.Message]);
      jl.free;
      Exit;
    end;
  end;

  res := TStringlist.Create;
  ddate := TStringlist.Create;
  try
    for I := 0 to jl.Count - 1 do
    begin
      showA := replaceTVShowChars(Csere(showName, '.', ' '));
      showB := replaceTVShowChars(jl.Child[i].Field['show'].Field['name'].Value);

      if onlyEnglishAlpha(showA) = onlyEnglishAlpha(showB) then
      begin
        if hadCountry then
        begin
          if jl.Child[i].Field['show'].Field['network'].SelfType <> jsNull then
            tv_country := AnsiString(jl.Child[i].Field['show'].Field['network'].Field['country'].Field['code'].Value);
          if jl.Child[i].Field['show'].Field['webChannel'].SelfType <> jsNull then
            tv_country := AnsiString(jl.Child[i].Field['show'].Field['webChannel'].Field['country'].Field['code'].Value);

          if tv_country = 'GB' then
            tv_country := 'UK';

          if UpperCase(tv_country) = uppercase(country) then
          begin
            if not fromIRC then
            begin
              result := AnsiString(jl.Child[i].Field['show'].Field['id'].Value);
              Break;
            end;
          end;
          res.Add(
            Format('<b>%s %s</b>: %s => %saddtvinfo %s %s %s',
            [AnsiString(jl.Child[i].Field['show'].Field['name'].Value),
            tv_country,
              AnsiString(jl.Child[i].Field['show'].Field['url'].Value),
              irccmdprefix,
              AnsiString(jl.Child[i].Field['show'].Field['id'].Value),
              Csere(showName, '.', ' '), country]));
        end;
        if hadYear then
        begin
          ddate.Delimiter := '-';
          if jl.Child[i].Field['show'].Field['premiered'].SelfType <> jsNull then
            ddate.DelimitedText := AnsiString(jl.Child[i].Field['show'].Field['premiered'].Value)
          else
            ddate.DelimitedText := '1970-01-01';
          if year = ddate.Strings[0] then
          begin
            if not fromIRC then
            begin
              result := AnsiString(jl.Child[i].Field['show'].Field['id'].Value);
              Break;
            end;
          end;
          res.Add(
            Format('<b>%s %s</b>: %s => %saddtvinfo %s %s %s',
            [AnsiString(jl.Child[i].Field['show'].Field['name'].Value),
            ddate.Strings[0],
              AnsiString(jl.Child[i].Field['show'].Field['url'].Value),
              irccmdprefix,
              AnsiString(jl.Child[i].Field['show'].Field['id'].Value),
              Csere(showName, '.', ' '), year]));
        end;
      end;

      if ((not hadYear) and (not hadCountry)) then
      begin
        if not fromIRC then
        begin
          result := AnsiString(jl.Child[i].Field['show'].Field['id'].Value);
          Break;
        end;

         res.Add(
            Format('<b>%s</b>: %s => %saddtvinfo %s %s',
            [AnsiString(jl.Child[i].Field['show'].Field['name'].Value),
              AnsiString(jl.Child[i].Field['show'].Field['url'].Value),
              irccmdprefix,
              AnsiString(jl.Child[i].Field['show'].Field['id'].Value),
              Csere(showName, '.', ' ')]));
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
    jl.free;
    res.free;
    ddate.free;
  end;
end;

function findTheTVDbIDByName(name: AnsiString): AnsiString;
var
  s, url, response: AnsiString;
  js: TlkJSONobject;
begin
  result := 'FAILED';
  s := Csere(name, '.and.', '.&.');
  s := Csere(s, '.at.', '.@.');
  s := Csere(s, '_and_', '_&_');
  s := Csere(s, '_at_', '_@_');
  s := Csere(s, '', Chr(39));
  s := Csere(s, ' ', '+');
  s := Csere(s, '.', '+');

  url := 'https://api.tvmaze.com/singlesearch/shows?q=' + s;
  try
    response := slUrlGet(url);
  except on e: Exception do
    begin
      Debug(dpError, section, '[EXCEPTION] findTheTVDbIDByName (httpGET): %s', [e.Message]);
      irc_Adderror(format('<c4>[EXCEPTION]</c> findTheTVDbIDByName (httpGET): %s', [e.Message]));
      Exit;
    end;
  end;

  if ((response = '') or (response = '[]')) then
  begin
    Debug(dpError, section, 'Cant find theTVDB id for ' + name);
    irc_addadmin('Cant find theTVDB id for ' + name);
    Exit;
  end;

  js := TlkJSON.ParseText(response) as TlkJSONobject;
  try
    if js.Field['externals'].Field['thetvdb'].SelfType <> jsNull then
      result := AnsiString(js.Field['externals'].Field['thetvdb'].Value)
    else
    begin
      Debug(dpError, section, 'Cant find theTVDB id for ' + name);
      irc_addadmin('<b><c14>Info</b></c>: Cant find theTVDB id for ' + name);
    end;

  finally
    js.Free;
  end;
end;

function getGenreFromTheTVDb(const id: AnsiString): AnsiString;
var
  s, url: AnsiString;
  xml: TSLXMLDocument;
  gn, nn, n: TSLXMLNode;
  x: TStringList;
  rx: TRegExpr;
  ts: TStream;
begin

  Result := '';

  url := 'https://thetvdb.com/api/FFFFFFFFFFFFFFFF/series/' + id + '/';
  s := slUrlGet(url);
  (*
    There can be 3 posible responses:

      1: an XML, first line MUST start with <?xml
      2: 404 page
      3: TVDb Lags.. could be done with an timeout.  maybe we retrun something like 0xBAADF00D
  *)

  if not AnsiStartsText('<?xml', s) then
  begin
    Debug(dpError, section, 'Warning: TheTVDB answer for show id %s was not in XML format.', [id]);
    exit;
  end;

  xml := TSLXMLDocument.Create;

  ts := TStringStream.Create(s);
  try
    try
      ts.Position := 0;
      xml.LoadFromStream(ts)
    except
      on E: Exception do
      begin
        xml.Free;
        Debug(dpError, section, '[EXCEPTION] getGenreFromTheTVDb (LoadFromStream): %s', [e.Message]);
        exit;
      end;
    end;
  finally
    ts.Free;
  end;

  try
    try
      n := xml.GetDocumentElement;
      nn := xml.FindChildNode(n, 'Series');
      gn := xml.FindChildNode(nn, 'Genre');
      s := xml.GetNodeValue(gn);
    except
      on E: Exception do
      begin
        Debug(dpError, section, '[EXCEPTION] getGenreFromTheTVDb (GetNodeValue): %s', [e.Message]);
        exit;
      end;
    end;
  finally
    xml.Free;
  end;

  x := TStringList.Create;
  rx := TRegExpr.create;
  try
    try
      x.QuoteChar := '"';
      rx.ModifierI := True;
      rx.Expression := '\|?(.*?)\|';

      if rx.Exec(s) then
      repeat
        x.Add(rx.Match[1]);
      until not rx.ExecNext();
    except
      on E: Exception do
      begin
        Debug(dpError, section, '[EXCEPTION] getGenreFromTheTVDb (x/rx): %s', [e.Message]);
        exit;
      end;
    end;

    Result := x.CommaText;
    Debug(dpSpam, section, '[DEBUG] getGenreFromTheTVDb: TVDBId: %s Result: %s ', [id, Result, url]);

  finally
    rx.Free;
    x.Free;
  end;
end;

procedure findCurrentAirDate(json: TlkJSONobject; out season, episdoe: Integer; out date: TDateTime);
var
  ep_nextnum, ep_prevnum: integer;
  se_nextnum, se_prevnum: integer;
  nextdt, prevdt: TDateTime;
  airt: AnsiString;
  formatSettings: TFormatSettings;
  hadPrev, hadNext: boolean;
begin

  date := UnixToDateTime(3817); //1.1.1990 031337

{$IFDEF MSWINDOWS}
  GetLocaleFormatSettings(1033, formatSettings);
{$ELSE}
  formatSettings := DefaultFormatSettings;
{$ENDIF}
  formatSettings.ShortDateFormat := 'yyyy-mm-dd';
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

      if AnsiString(json.Field['_embedded'].Field['previousepisode'].Field['airtime'].Value) = '' then
        airt := '00:00'
      else
        airt := AnsiString(json.Field['_embedded'].Field['previousepisode'].Field['airtime'].Value);

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
      if AnsiString(json.Field['_embedded'].Field['nextepisode'].Field['airtime'].Value) = '' then
        airt := '00:00'
      else
        airt := AnsiString(json.Field['_embedded'].Field['nextepisode'].Field['airtime'].Value);

      nextdt := StrToDateTime(string(json.Field['_embedded'].Field['nextepisode'].Field['airdate'].Value) + ' ' + airt, formatSettings);
      hadNext := True;
    end;
  except on e: Exception do
    begin
      Debug(dpError, section, '[EXCEPTION] findCurrentAirDate (nextepisode): ' + e.Message);
      Irc_AddAdmin('[EXCEPTION] findCurrentAirDate (previousepisode): ' + e.Message);
    end;
  end;

  if ((not hadNext) and (not hadPrev)) then
  begin
    episdoe := -15;
    season := -15;
    date := UnixToDateTime(3817); //1.1.1970 031337
    exit;
  end;

  if not hadNext then
  begin
    episdoe := ep_prevnum;
    season := se_prevnum;
    date := prevdt;
    exit;
  end;

  if IsSameDay(prevdt, nextdt) then
  begin
    episdoe := -5;
    season := -5;
    date := nextdt;
    exit;
  end;

  if (DateTimeToUnix(nextdt)) <= DateTimeToUnix(now()) then
  begin
    // next date is smaller|equal to now()..
    episdoe := ep_nextnum;
    season := se_nextnum;
    date := nextdt;
    Exit;
  end;

  if (DateTimeToUnix(prevdt) + 86400) >= DateTimeToUnix(now()) then
  begin
    //previous date + 1Day is grater|equal to now()
    episdoe := ep_prevnum;
    season := se_prevnum;
    date := prevdt;
    Exit;
  end;

  if ((not hadPrev) AND (hadNext)) then
  begin
    if json.Field['status'].SelfType <> jsNull then
      //somehow the group catch the episode early, maybe a "pre-air-pilot"..
      if (AnsiString(json.Field['status'].Value) = 'In Development') then
      begin
        episdoe := ep_nextnum;
        season := se_nextnum;
        date := nextdt;
      end;
    Exit;
  end;

  if (DateTimeToUnix(nextdt)) > DateTimeToUnix(now()) then
  begin
    // nothing before matched and next_date is grater then now, so we took this.
    episdoe := ep_nextnum;
    season := se_nextnum;
    date := nextdt;
  end;
end;

function parseTVMazeInfos(jsonStr: AnsiString; Showname: AnsiString = ''; uurl: AnsiString = ''): TTVInfoDB;
label
  TryToGetTheTVDBGenre;
var
  tvr: TTVInfoDB;
  i: integer;
  s: AnsiString;
  js: TlkJSONobject;
  gTVDB: TStringlist;
  season, episdoe: Integer;
  date: TDateTime;
  numerrors: Integer;
  TheTVDBGenreFailure: Boolean;
  ExceptionMessage: AnsiString;
begin
  Result := nil;
  js := nil;
  numerrors := 0;

  if Showname <> '' then
    s := Csere(Showname, '.', ' ')
  else
    s := '';

  tvr := TTVInfoDB.Create(s);
  tvr.tv_genres.Sorted := True;
  tvr.tv_genres.Duplicates := dupIgnore;
  gTVDB := TStringlist.Create;
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

    tvr.tv_showname := AnsiString(js.Field['name'].Value);

    if LowerCase(tvr.tv_showname) = 'not found' then
    begin
      irc_addAdmin('<c14><b>WARNING</c></b>: TVMaze returned a 404 Not Found page for show <b>%s</b>. Show ID changed?', [Showname]);
      Exit;
    end;

    tvr.tvmaze_id := AnsiString(js.Field['id'].Value);
    tvr.tv_url := AnsiString(js.Field['url'].Value);

    if js.Field['language'].SelfType <> jsNull then
    tvr.tv_language:=AnsiString(js.Field['language'].Value);

    if js.Field['status'].SelfType = jsNull then
      tvr.tv_status := 'unknown'
    else
      tvr.tv_status := AnsiString(js.Field['status'].Value);

    if js.Field['type'].SelfType = jsNull then
      tvr.tv_classification := 'unknown'
    else
      tvr.tv_classification := AnsiString(js.Field['type'].Value);

    tvr.tv_running := Boolean( (lowercase(tvr.tv_status) = 'running') or (lowercase(tvr.tv_status) = 'in development') );
    tvr.tv_scripted := Boolean(lowercase(tvr.tv_classification) = 'scripted');

    if js.Field['externals'].Field['thetvdb'].SelfType <> jsNull then
      tvr.thetvdb_id := AnsiString(js.Field['externals'].Field['thetvdb'].Value);

    // TODO: Remove tvrage ?
    if js.Field['externals'].Field['tvrage'].SelfType <> jsNull then
      tvr.tvrage_id := AnsiString(js.Field['externals'].Field['tvrage'].Value);

    if js.Field['network'].SelfType = jsNull then
    begin
      if js.Field['webChannel'].SelfType <> jsNull then
      begin
        tvr.tv_network := AnsiString(js.Field['webChannel'].Field['name'].Value);
        if js.Field['webChannel'].Field['country'].SelfType = jsNull then
          tvr.tv_country := 'unknown'
        else
          tvr.tv_country := AnsiString(js.Field['webChannel'].Field['country'].Field['code'].Value);
      end
      else
      begin
        tvr.tv_network := 'unknown';
        tvr.tv_country := 'unknown';
      end;
    end
    else
    begin
      tvr.tv_network := AnsiString(js.Field['network'].Field['name'].Value);
      if js.Field['network'].Field['country'].SelfType = jsNull then
        tvr.tv_country := 'unknown'
      else
        tvr.tv_country := AnsiString(js.Field['network'].Field['country'].Field['code'].Value);
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
    Debug(dpSpam, section, 'parseTVMazeInfos (genres): tvmaze_id: %s Result: %s ', [tvr.tvmaze_id, tvr.tv_genres.CommaText, uurl]);

    // just a hotfix to be ready when the API is down (October 1st, 2017)
    if StrToDateTime('30/09/2017') > Now then
    begin
      TryToGetTheTVDBGenre:
      TheTVDBGenreFailure := False;
      try
        inc(numerrors);

        if js.Field['externals'].Field['thetvdb'].SelfType <> jsNull then
        begin
          gTVDB.CommaText := getGenreFromTheTVDb(tvr.thetvdb_id);

          for I := 0 to gTVDB.Count - 1 do
            tvr.tv_genres.Add(gTVDB.Strings[i]);
        end;
      except
        on e: Exception do
        begin
          TheTVDBGenreFailure := True;
          ExceptionMessage := e.Message;
        end;
      end;

      if TheTVDBGenreFailure then
      begin
        case numerrors of
          0..2:
            begin
              goto TryToGetTheTVDBGenre;
            end;
          3:
            begin
              Debug(dpMessage, section, '[EXCEPTION] parseTVMazeInfos TheTVDB Genre Exception : %s - Show: %s (ID: %s)', [ExceptionMessage, tvr.tv_showname, tvr.tvmaze_id]);
              irc_addadmin('<c4><b>[EXCEPTION]</b></c> parseTVMazeInfos TheTVDB Genre Exception : %s - Show: %s (ID: %s)', [ExceptionMessage, tvr.tv_showname, tvr.tvmaze_id]);
            end;
        end;
      end;
    end;

    if js.Field['premiered'].SelfType <> jsNull then
      tvr.tv_premiered_year := StrToIntDef(copy(string(js.Field['premiered'].Value), 1, 4), -1)
    else
      tvr.tv_premiered_year := -1;

      tvr.tv_endedyear := -1;
      tvr.tv_next_ep := -10;
      tvr.tv_next_season := -10;
      tvr.tv_next_date := 3817;

    //Show not ended so we check for next.
    if lowercase(tvr.tv_status) <> 'ended' then
    begin
      findCurrentAirDate(js, season, episdoe, date);
      tvr.tv_next_season := season;
      tvr.tv_next_ep := episdoe;
      tvr.tv_next_date := DateTimeToUnix(date);
    end
    else
      if ((js.Field['_embedded'] <> nil) and (js.Field['_embedded'].Field['previousepisode'] <> nil)) then
        tvr.tv_endedyear := StrtoIntdef(Copy(string(js.Field['_embedded'].Field['previousepisode'].Field['airdate'].Value), 1, 4), -1);

    tvr.last_updated := DateTimeToUnix(now());
    Result := tvr;
  finally
    js.free;
    gTVDB.free;
  end;
end;

{ TPazoTheTVDbLookupTask }

constructor TPazoTVInfoLookupTask.Create(const netname, channel: AnsiString; site: AnsiString; pazo: TPazo; attempt: integer = 0);
begin
  self.attempt := attempt;
  self.initial_site := site;
  inherited Create(netname, channel, site, '', pazo);
end;

function TPazoTVInfoLookupTask.Execute(slot: Pointer): boolean;
var
  tr: TTVRelease;
  r: TPazoTVInfoLookupTask;
  showA, showB, tvmaz, sid, uurl: AnsiString;
  db_tvinfo: TTVInfoDB;
  ps: TPazoSite;
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
      irc_addadmin('<c4>ERROR</c> No TVMaze ID found for <b>%s</b>', [tr.showname]);
      SlftpNewsAdd('TVMAZE', Format('<c4>ERROR</c> No TVMaze ID found for <b>%s</b>', [tr.showname]), True);
    end;

    ready := True;
    Result := True;
    exit;
  end;

  try
    uurl := 'https://api.tvmaze.com/shows/' + sid + '?embed[]=nextepisode&embed[]=previousepisode';
    tvmaz := slUrlGet(uurl);
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TPazoTVInfoLookupTask.execute httpGET(TVMaze): Exception : %s', [e.Message]));
      irc_Adderror(Format('<c4>[EXCEPTION]</c> TPazoTVInfoLookupTask.execute httpGET(TVMaze): Exception : %s', [e.Message]));
      Result := True;
      ready := True;
      exit;
    end;
  end;

  if tvmaz = '' then
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
    kb_add(netname, channel, ps.Name, mainpazo.rls.section, '', 'UPDATE', mainpazo.rls.rlsname, '');
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

function TPazoTVInfoLookupTask.Name: AnsiString;
begin
  try
    Result := format('TVInfo PazoID(%d) %s @ %s Count(%d)', [mainpazo.pazo_id, mainpazo.rls.rlsname, site1, attempt]);
  except
    Result := 'TVInfo';
  end;
end;

//TPazoHTTPUpdateTVInfoTask

constructor TPazoHTTPUpdateTVInfoTask.Create(const netname: AnsiString; const channel: AnsiString; site: AnsiString; pazo: TPazo; attempt: Integer = 0);
begin

  inherited Create(netname, channel, site, '', pazo);
  //  inherited Create('', '', config.ReadString('sites', 'admin_sitename', 'SLFTP'));
end;

destructor TPazoHTTPUpdateTVInfoTask.Destroy;
begin
  inherited;
end;

function TPazoHTTPUpdateTVInfoTask.Name: AnsiString;
begin
  Result := Format('httpUpdateTVInfo : %s', [showname]);
end;

function TPazoHTTPUpdateTVInfoTask.Execute(slot: Pointer): boolean;
var
  tvdb: TTVInfoDB;
  response: AnsiString;
  ps: TPazoSite;
begin
  showname := TTvRelease(mainpazo.rls).showname;
  tvmaze_id := TTvRelease(mainpazo.rls).showid;

  response := slUrlGet('https://api.tvmaze.com/shows/' + tvmaze_id + '?embed[]=nextepisode&embed[]=previousepisode');
  if response = '' then
  begin
    irc_addadmin('<c4><b>ERROR</c></b> TVInfo updated for ' + showname + ' failed, response for was empty');
    Debug(dpSpam, section, 'ERROR TVInfo updated for ' + showname + ' failed, response for was empty');
    Result := True;
    readyerror := True;
    exit;
  end;
  try
    tvdb := parseTVMazeInfos(response, showname);
  except on E: Exception do
      irc_adderror(Format('parseTVMazeInfos  -=> %s', [e.Message]));
  end;

  try
    try
      result := tvdb.Update;
    except on E: Exception do
        irc_adderror(Format('parseTVMazeInfos  -=> %s', [e.Message]));
    end;

  finally
    //tvdb.free;
  end;

  try
    ps := FindMostCompleteSite(mainpazo);
    if ((ps = nil) and (mainpazo.sites.Count > 0)) then
      ps := TPazoSite(mainpazo.sites[0]);
    kb_add(netname, channel, ps.Name, mainpazo.rls.section, '', 'UPDATE', mainpazo.rls.rlsname, '');
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('Exception in TPazoTVInfoUpdateTask kb_add: %s', [e.Message]));
    end;
  end;

end;

{ TPazoHTTPTVInfoTask }

constructor TPazoHTTPTVInfoTask.Create(const tvmaze_id: AnsiString; rls: AnsiString =
  '');
begin
  self.tvmaze_id := tvmaze_id;
  self.rls := rls;
  inherited Create('', '', config.ReadString('sites', 'admin_sitename', 'SLFTP'));
end;

function TPazoHTTPTVInfoTask.Name: AnsiString;
begin
  try
    Result := Format('httpTVMaze : %s', [tvmaze_id]);
  except
    Result := 'httpTVMaze';
  end;
end;

destructor TPazoHTTPTVInfoTask.Destroy;
begin
  inherited;
end;

function TPazoHTTPTVInfoTask.Execute(slot: Pointer): boolean;
var
  tvdb: TTVInfoDB;
  rx: TRegExpr;
  uurl, sname: AnsiString;

begin

  rx := TRegexpr.Create;
  try
    rx.ModifierI := True;

    rx.Expression :=
      '(.*)[\._-](\d{4}\.\d{2}\.\d{2}|\d{2}\.\d{2}\.\d{4})[\._-](.*)';
    if rx.Exec(rls) then
    begin
      sname := rx.Match[1];
    end;

    rx.Expression := '(.*)[\._-](\d+)x(\d+)[\._-](.*)';
    if rx.Exec(rls) then
    begin
      sname := rx.Match[1];
      //    season   := StrToIntDef(rx.Match[2], 0);
      //    episode  := StrToIntDef(rx.Match[3], 0);
    end;

    rx.Expression :=
      '(.*)[\._-]S(\d{1,3})(\.?([DE]|EP|Episode|Part)(\d{1,4})\w?(E\d{1,4})?)?[\._-](.*)';
    if rx.Exec(rls) then
    begin
      sname := rx.Match[1];
      //    season   := StrToIntDef(rx.Match[2], 0);
      //    episode  := StrToIntDef(rx.Match[5], 0);
    end;

    rx.Expression := '[\.\_]';
    sname := rx.Replace(sname, ' ', False);
  finally
    rx.Free;
  end;

  uurl := 'https://api.tvmaze.com/shows/' + tvmaze_id + '?embed[]=nextepisode&embed[]=previousepisode';
  response := slUrlGet(uurl);

  if response = '' then
  begin
    irc_addadmin('<c4><b>ERROR</c></b> no infos found for ' + rls + '(' + sname + ')');
    Debug(dpSpam, section, '<c4><b>ERROR</c></b> no infos found for ' + rls + '(' + sname + ')');
    Result := True;
    readyerror := True;
    exit;
  end;

  tvdb := parseTVMazeInfos(response, sname);
  try
    if tvdb <> nil then
      saveTVInfos(tvmaze_id, tvdb, rls, false);
  finally
    tvdb.free;
  end;

  result := True;
end;

end.
