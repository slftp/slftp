unit tasktvinfolookup;

interface

uses
  Classes, pazo, tasksunit, taskrace, dbtvinfo, StrUtils;

type
  { Outcome of one search; ambiguity must not trigger broader queries. }
  TTVMazeSearchStatus = (tvssError, tvssNoMatch, tvssAmbiguous, tvssMatched);

  { Accumulated monotonic durations across queued lookup attempts, in milliseconds. }
  TTVMazeLookupMetrics = record
    FCreatedAt: TDateTime; //< Wall-clock start for the IRC report
    FCreatedTick: Int64; //< Monotonic start for the total duration
    FQueueMs, FBackoffMs: Int64; //< Slot wait and intentional delay, counted separately
    FSearchMs, FFetchMs, FDbMs, FApplyMs: Int64; //< Accumulated stage durations
    FSearches, FReductions: Integer; //< Search calls and shortened queries
  end;

  TPazoTVInfoLookupTask = class(TPazoPlainTask)
  private
    attempt: integer;
    initial_site: String;
    FOriginalName, FQueryName, FYear, FCountry: String; //< Original constraints and current query
    FHadYear, FHadCountry, FSearchInitialized, FAllowPrefix: Boolean; //< Search constraints
    FQueryAttempt: Integer; //< Query number within the current attempt (1..5)
    FQueuedTick, FNotBeforeTick: Int64; //< Monotonic enqueue time and requested deadline
    FMetrics: TTVMazeLookupMetrics; //< Totals shared by successor tasks via value copies
    { Enqueues a successor with the current metrics, optionally restarting the query sequence. }
    procedure QueueNextSearch(const aDelayMs: Integer; const aRestart: Boolean);
    { Reports terminal timings to the log and successful lookups to IRC STATS. }
    procedure ReportTiming(const aSource, aOutcome: String; const aSuccess: Boolean);
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

function parseTVMazeInfos(const jsonStr, Showname, uurl: String): TTVInfoDB;
{ Pure helpers are exposed so TVMaze matching can be tested without HTTP. }
function TVMazeReduceShowNameByOneWord(var aName: String): Boolean;
function TVMazeIsSafePrefixMatch(const aOriginalName, aCandidateName: String; const aScore: Double): Boolean;
{ Selects a unique candidate without network access.
  @param(aResponse JSON search response)
  @param(aOriginalName Original normalized query, never the shortened query)
  @param(aYear Optional premiere year)
  @param(aCountry Optional country code)
  @param(aHadYear Whether the original query specified a year)
  @param(aHadCountry Whether the original query specified a country)
  @param(aFromIRC Whether to collect interactive choices instead of selecting automatically)
  @param(aAllowPrefix Whether a unique high-score prefix may be accepted)
  @param(aRes Receives interactive choices; must be assigned)
  @param(aShowId Receives the selected automatic show ID)
  @returns(Match, ambiguity, genuine miss, or response error) }
function TVMazeParseSearchResponse(const aResponse, aOriginalName, aYear, aCountry: String;
  aHadYear, aHadCountry, aFromIRC, aAllowPrefix: Boolean;
  aRes: TStringList; out aShowId: String): TTVMazeSearchStatus;
{ Advances only genuine misses, bounded to five queries including the initial query.
  @param(aQuery Current query, shortened on success)
  @param(aAttempt Current query number, incremented on success)
  @param(aStatus Outcome of the current search)
  @returns(True when the caller should queue another search after releasing its slot) }
function TVMazeNextQuery(var aQuery: String; var aAttempt: Integer;
  const aStatus: TTVMazeSearchStatus): Boolean;
function findTVMazeIDByName(const name: String; Netname: String = ''; Channel: String = ''): String;

implementation

uses
  DateUtils, Contnrs, SysUtils, queueunit, debugunit, configunit, mystrings, kb.releaseinfo,
  kb, http, RegExpr, irc, mrdohutils, uLkJSON, news, sitesunit, mormot.core.os;

const
  section = 'tasktvinfo';


function TVMazeReduceShowNameByOneWord(var aName: String): Boolean;
var
  i: Integer;
begin
  Result := False;
  aName := TrimRight(aName);
  i := Length(aName);
  while (i > 0) and not CharInSet(aName[i], [' ', '.', '_', '-']) do
    Dec(i);
  if i <= 0 then
    Exit;
  aName := TrimRight(Copy(aName, 1, i - 1));
  Result := aName <> '';
end;

function TVMazeIsSafePrefixMatch(const aOriginalName, aCandidateName: String; const aScore: Double): Boolean;
var
  originalAlpha, candidateAlpha: String;
begin
  originalAlpha := onlyEnglishAlpha(replaceTVShowChars(ReplaceText(aOriginalName, '.', ' ')));
  candidateAlpha := onlyEnglishAlpha(replaceTVShowChars(ReplaceText(aCandidateName, '.', ' ')));
  Result := (aScore >= 0.7) and (Length(candidateAlpha) >= 4) and
    (Length(candidateAlpha) < Length(originalAlpha)) and
    AnsiStartsText(candidateAlpha, originalAlpha);
end;

function TVMazeParseSearchResponse(const aResponse, aOriginalName, aYear, aCountry: String;
  aHadYear, aHadCountry, aFromIRC, aAllowPrefix: Boolean;
  aRes: TStringList; out aShowId: String): TTVMazeSearchStatus;
var
  jl: TlkJSONlist;
  fRoot: TlkJSONbase;
  ddate: TStringList;
  i: Integer;
  originalAlpha, showB, tv_country, premiered, candidateId: String;
  tvScore: Double;
  exactMatch, prefixMatch, yearMatch, countryMatch: Boolean;
  exactShowId, prefixShowId: String;
  exactAmbiguous, prefixAmbiguous: Boolean;
begin
  Result := tvssError;
  aShowId := '';
  if aResponse = '' then
    Exit;
  if Trim(aResponse) = '[]' then
  begin
    Result := tvssNoMatch;
    Exit;
  end;

  fRoot := nil;
  try
    try
      fRoot := TlkJSON.ParseText(AnsiString(aResponse));
      if not (fRoot is TlkJSONlist) then
        Exit;
      jl := TlkJSONlist(fRoot);

      ddate := TStringList.Create;
      try
        originalAlpha := onlyEnglishAlpha(replaceTVShowChars(ReplaceText(aOriginalName, '.', ' ')));
        exactShowId := '';
        prefixShowId := '';
        exactAmbiguous := False;
        prefixAmbiguous := False;

        for i := 0 to jl.Count - 1 do
        begin
          showB := onlyEnglishAlpha(replaceTVShowChars(jl.Child[i].Field['show'].Field['name'].Value));
          exactMatch := CompareText(originalAlpha, showB) = 0;
          prefixMatch := False;
          tvScore := 0;
          try
            tvScore := jl.Child[i].Field['score'].Value;
          except
            // Missing or malformed API scores cannot authorize a fuzzy prefix match.
          end;

          if aAllowPrefix and not exactMatch and not aFromIRC and not aHadYear and not aHadCountry then
            prefixMatch := TVMazeIsSafePrefixMatch(aOriginalName,
              String(jl.Child[i].Field['show'].Field['name'].Value), tvScore);

          if aFromIRC then
          begin
            // Keep interactive search explicit: list all API results without tags,
            // and require an exact title when the user supplied a year or country.
            if (not aHadYear) and (not aHadCountry) then
              aRes.Add(Format('<b>%s</b>: %s => %saddtvinfo %s %s',
                [String(jl.Child[i].Field['show'].Field['name'].Value),
                 String(jl.Child[i].Field['show'].Field['url'].Value), irccmdprefix,
                 String(jl.Child[i].Field['show'].Field['id'].Value), ReplaceText(aOriginalName, '.', ' ')]))
            else if exactMatch then
            begin
              if aHadCountry then
              begin
                tv_country := '';
                if jl.Child[i].Field['show'].Field['network'].SelfType <> jsNull then
                  if jl.Child[i].Field['show'].Field['network'].Field['country'].SelfType <> jsNull then
                    tv_country := String(jl.Child[i].Field['show'].Field['network'].Field['country'].Field['code'].Value);
                if jl.Child[i].Field['show'].Field['webChannel'].SelfType <> jsNull then
                  if jl.Child[i].Field['show'].Field['webChannel'].Field['country'].SelfType <> jsNull then
                    tv_country := String(jl.Child[i].Field['show'].Field['webChannel'].Field['country'].Field['code'].Value);
                if tv_country = 'GB' then
                  tv_country := 'UK';
                aRes.Add(Format('<b>%s %s</b>: %s => %saddtvinfo %s %s %s',
                  [String(jl.Child[i].Field['show'].Field['name'].Value), tv_country,
                   String(jl.Child[i].Field['show'].Field['url'].Value), irccmdprefix,
                   String(jl.Child[i].Field['show'].Field['id'].Value),
                   ReplaceText(aOriginalName, '.', ' '), aCountry]));
              end;
              if aHadYear then
              begin
                premiered := '1970-01-01';
                if jl.Child[i].Field['show'].Field['premiered'].SelfType <> jsNull then
                  premiered := String(jl.Child[i].Field['show'].Field['premiered'].Value);
                ddate.Delimiter := '-';
                ddate.DelimitedText := premiered;
                aRes.Add(Format('<b>%s %s</b>: %s => %saddtvinfo %s %s %s',
                  [String(jl.Child[i].Field['show'].Field['name'].Value), ddate.Strings[0],
                   String(jl.Child[i].Field['show'].Field['url'].Value), irccmdprefix,
                   String(jl.Child[i].Field['show'].Field['id'].Value),
                   ReplaceText(aOriginalName, '.', ' '), aYear]));
              end;
            end;
            Continue;
          end;

          if not exactMatch and not prefixMatch then
            Continue;

          yearMatch := True;
          if aHadYear then
          begin
            premiered := '1970-01-01';
            if jl.Child[i].Field['show'].Field['premiered'].SelfType <> jsNull then
              premiered := String(jl.Child[i].Field['show'].Field['premiered'].Value);
            ddate.Delimiter := '-';
            ddate.DelimitedText := premiered;
            yearMatch := aYear = ddate.Strings[0];
          end;

          countryMatch := True;
          if aHadCountry then
          begin
            tv_country := '';
            if jl.Child[i].Field['show'].Field['network'].SelfType <> jsNull then
              if jl.Child[i].Field['show'].Field['network'].Field['country'].SelfType <> jsNull then
                tv_country := String(jl.Child[i].Field['show'].Field['network'].Field['country'].Field['code'].Value);
            if jl.Child[i].Field['show'].Field['webChannel'].SelfType <> jsNull then
              if jl.Child[i].Field['show'].Field['webChannel'].Field['country'].SelfType <> jsNull then
                tv_country := String(jl.Child[i].Field['show'].Field['webChannel'].Field['country'].Field['code'].Value);
            if tv_country = 'GB' then
              tv_country := 'UK';
            countryMatch := SameText(tv_country, aCountry);
          end;

          if not yearMatch or not countryMatch then
            Continue;

          candidateId := String(jl.Child[i].Field['show'].Field['id'].Value);
          if exactMatch then
          begin
            if (exactShowId <> '') and (exactShowId <> candidateId) then
              exactAmbiguous := True
            else
              exactShowId := candidateId;
          end
          else if prefixMatch then
          begin
            if (prefixShowId <> '') and (prefixShowId <> candidateId) then
              prefixAmbiguous := True
            else
              prefixShowId := candidateId;
          end;
        end;
      finally
        ddate.Free;
      end;

      if exactShowId <> '' then
      begin
        if not exactAmbiguous then
        begin
          aShowId := exactShowId;
          Result := tvssMatched;
        end
        else
          Result := tvssAmbiguous;
      end
      else if prefixShowId <> '' then
      begin
        if not prefixAmbiguous then
        begin
          aShowId := prefixShowId;
          Result := tvssMatched;
        end
        else
          Result := tvssAmbiguous;
      end
      else if aFromIRC and (aRes.Count > 0) then
        Result := tvssMatched
      else
        Result := tvssNoMatch;
    except
      on e: Exception do
      begin
        aShowId := '';
        aRes.Clear;
        Result := tvssError;
      end;
    end;
  finally
    fRoot.Free;
  end;
end;

function _TVMazeSearchOnce(const aQueryName, aOriginalName, aYear, aCountry: String;
  aHadYear, aHadCountry, aFromIRC, aAllowPrefix: Boolean;
  aRes: TStringList; out aShowId: String): TTVMazeSearchStatus;
var
  fResponse, fError: String;
begin
  aShowId := '';
  if not HttpGetUrl('https://api.tvmaze.com/search/shows?q=' +
    replaceTVShowChars(aQueryName, True), fResponse, fError) then
  begin
    Debug(dpError, section, 'TVMAZE search failed for "%s": %s', [aQueryName, fError]);
    Result := tvssError;
    Exit;
  end;
  Result := TVMazeParseSearchResponse(fResponse, aOriginalName, aYear, aCountry,
    aHadYear, aHadCountry, aFromIRC, aAllowPrefix, aRes, aShowId);
end;

procedure _TVMazePrepareQuery(const aName: String; out aShowName, aYear, aCountry: String;
  out aHadYear, aHadCountry: Boolean);
var
  x: TRegExpr;
begin
  aShowName := aName;
  aYear := '';
  aCountry := '';
  aHadYear := False;
  aHadCountry := False;
  x := TRegExpr.Create;
  try
    x.ModifierI := False;
    x.ModifierM := True;
    x.Expression := '[-._\s]((19|20)\d{2})[\s._-]?$';
    if x.Exec(aShowName) then
    begin
      aYear := x.Match[1];
      if StrToInt(aYear) < (StrToInt(FormatDateTime('yyyy', Now)) + 10) then
      begin
        aShowName := x.Replace(aShowName, '', False);
        aHadYear := True;
      end;
    end;

    x.Expression := '[-._\s](US|UK|AU|CA|NZ)[\s._-]?$';
    if x.Exec(aShowName) then
    begin
      aCountry := x.Match[1];
      aShowName := x.Replace(aShowName, '', False);
      aHadCountry := True;
    end;
  finally
    x.Free;
  end;

end;

function TVMazeNextQuery(var aQuery: String; var aAttempt: Integer;
  const aStatus: TTVMazeSearchStatus): Boolean;
begin
  Result := False;
  if (aStatus <> tvssNoMatch) or (aAttempt >= 5) then
    Exit;
  if not TVMazeReduceShowNameByOneWord(aQuery) then
    Exit;
  Inc(aAttempt);
  Result := True;
end;

function findTVMazeIDByName(const name: String; Netname: String = ''; Channel: String = ''): String;
var
  fShowName, fYear, fCountry, fShowId: String;
  fHadYear, fHadCountry, fFromIRC: Boolean;
  fResults: TStringList;
  fStatus: TTVMazeSearchStatus;
begin
  Result := 'FAILED';
  fFromIRC := (Netname <> '') and (Channel <> '');
  _TVMazePrepareQuery(name, fShowName, fYear, fCountry, fHadYear, fHadCountry);
  fResults := TStringList.Create;
  try
    // Interactive search stays a single request. Automatic reductions are queued by the task.
    fStatus := _TVMazeSearchOnce(fShowName, fShowName, fYear, fCountry,
      fHadYear, fHadCountry, fFromIRC, not config.ReadBool(section,
      'stop_on_englishcheck', True), fResults, fShowId);
    if fStatus = tvssMatched then
    begin
      if fFromIRC then
        Result := fResults.CommaText
      else
        Result := fShowId;
    end
    else if fFromIRC then
      irc_AddText(Netname, Channel, 'TVInfo: No usable search result for %s', [name]);
  finally
    fResults.Free;
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
  nextdt := UnixToDateTime(3817);
  prevdt := UnixToDateTime(3817);

  {$IFDEF MSWINDOWS}
    formatSettings := TFormatSettings.Create('en-US');
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
      if (json.Field['_embedded'].Field['previousepisode'].Field['number'] <> NIL) then
        ep_prevnum := StrToIntDef(string(json.Field['_embedded'].Field['previousepisode'].Field['number'].Value), -1)
      else
        ep_prevnum := -1;

      if (json.Field['_embedded'].Field['previousepisode'].Field['season'] <> NIL) then
        se_prevnum := StrToIntDef(string(json.Field['_embedded'].Field['previousepisode'].Field['season'].Value), -1)
      else
        se_prevnum := -1;
      prevdt := UnixToDateTime(0);

      if String(json.Field['_embedded'].Field['previousepisode'].Field['airtime'].Value) = '' then
        airt := '00:00'
      else
        airt := String(json.Field['_embedded'].Field['previousepisode'].Field['airtime'].Value);

      if ((json.Field['_embedded'].Field['previousepisode'].Field['airdate'] <> nil) AND (string(json.Field['_embedded'].Field['previousepisode'].Field['airdate'].Value) <> '')) then
      begin
        prevdt := StrToDateTime(string(json.Field['_embedded'].Field['previousepisode'].Field['airdate'].Value) + ' ' + airt, formatSettings);
        hadPrev := True;
      end;
    end;
  except on e: Exception do
    begin
      Debug(dpError, section, '[EXCEPTION] in findCurrentAirDate (previousepisode): ' + e.Message);
      Irc_AddError('[EXCEPTION] findCurrentAirDate (previousepisode): ' + e.Message);
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
      Irc_AddError('[EXCEPTION] findCurrentAirDate (nextepisode): ' + e.Message);
    end;
  end;

  if ((not hadNext) and (not hadPrev)) then
  begin
    episode := Ord(tvSeEpNoNextOrPrev);
    season := Ord(tvSeEpNoNextOrPrev);
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
    episode := Ord(tvSeEpAirdatePrevAndNextOnSameDay);
    season := Ord(tvSeEpAirdatePrevAndNextOnSameDay);
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

function parseTVMazeInfos(const jsonStr, Showname, uurl: String): TTVInfoDB;
var
  tvr: TTVInfoDB;
  i: integer;
  s: String;
  js: TlkJSONobject;
  season, episode: Integer;
  date: TDateTime;
  fStrHelper: String;
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
      js := TlkJSON.ParseText(AnsiString(jsonStr)) as TlkJSONObject;
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
      tvr.tv_language:=String(js.Field['language'].Value)
    else
      tvr.tv_language := 'unknown';

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
    tvr.tv_next_ep := Ord(tvSeEpShowEnded);
    tvr.tv_next_season := Ord(tvSeEpShowEnded);
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

    if ((js.Field['rating'].SelfType <> jsNull) and (js.Field['rating'].Field['average'].SelfType <> jsNull)) then
    begin
      fStrHelper := String(js.Field['rating'].Field['average'].Value);
      fStrHelper := fStrHelper.Replace('.', '').Replace(',', '');
      tvr.tv_rating := StrToIntDef(fStrHelper, 0);
      if (tvr.tv_rating <= 10) then
      begin
        // a rating of 6.0 is shown as 6
        tvr.tv_rating := tvr.tv_rating * 10;
      end;
    end
    else
      tvr.tv_rating := 0;

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
  FMetrics.FCreatedAt := Now;
  FMetrics.FCreatedTick := mormot.core.os.GetTickCount64;
  FQueuedTick := FMetrics.FCreatedTick;
  FNotBeforeTick := FQueuedTick;
end;

procedure TPazoTVInfoLookupTask.QueueNextSearch(const aDelayMs: Integer; const aRestart: Boolean);
var
  fNext: TPazoTVInfoLookupTask;
begin
  fNext := TPazoTVInfoLookupTask.Create(netname, channel, initial_site, mainpazo, attempt);
  fNext.FOriginalName := FOriginalName;
  fNext.FQueryName := FQueryName;
  fNext.FYear := FYear;
  fNext.FCountry := FCountry;
  fNext.FHadYear := FHadYear;
  fNext.FHadCountry := FHadCountry;
  fNext.FAllowPrefix := FAllowPrefix;
  fNext.FSearchInitialized := True;
  fNext.FQueryAttempt := FQueryAttempt;
  fNext.FMetrics := FMetrics;
  if aRestart then
  begin
    Inc(fNext.attempt);
    fNext.FQueryName := FOriginalName;
    fNext.FQueryAttempt := 1;
  end;
  fNext.FNotBeforeTick := fNext.FQueuedTick + aDelayMs;
  fNext.startat := IncMilliSecond(Now, aDelayMs);
  // The current task completes, releasing its admin slot before the next query.
  AddTask(fNext, True);
end;

procedure TPazoTVInfoLookupTask.ReportTiming(const aSource, aOutcome: String; const aSuccess: Boolean);
var
  fText: String;
begin
  fText := Format('[TVINFO PERF] %s: source=%s outcome=%s start=%s end=%s ' +
    'total_ms=%d queue_ms=%d backoff_ms=%d search_ms=%d fetch_ms=%d db_ms=%d ' +
    'apply_ms=%d searches=%d reductions=%d',
    [mainpazo.rls.rlsname, aSource, aOutcome,
     FormatDateTime('hh:nn:ss.zzz', FMetrics.FCreatedAt),
     FormatDateTime('hh:nn:ss.zzz', Now),
     mormot.core.os.GetTickCount64 - FMetrics.FCreatedTick, FMetrics.FQueueMs,
     FMetrics.FBackoffMs, FMetrics.FSearchMs, FMetrics.FFetchMs, FMetrics.FDbMs,
     FMetrics.FApplyMs, FMetrics.FSearches, FMetrics.FReductions]);
  Debug(dpMessage, section, fText);
  if aSuccess then
    irc_Addstats(fText);
end;

function TPazoTVInfoLookupTask.Execute(slot: Pointer): boolean;
var
  tr: TTVRelease;
  tvmaz, sid, uurl, fSource, fOutcome: String;
  db_tvinfo: TTVInfoDB;
  ps: TPazoSite;
  fHttpGetErrMsg: String;
  fResults: TStringList;
  fStatus: TTVMazeSearchStatus;
  fTick, fWait, fBackoff: Int64;
  fSuccess, fDeferred: Boolean;
begin
  Result := True;
  tr := TTVRelease(mainpazo.rls);
  fTick := mormot.core.os.GetTickCount64;
  fWait := fTick - FQueuedTick;
  fBackoff := FNotBeforeTick - FQueuedTick;
  if fBackoff > fWait then
    fBackoff := fWait;
  Inc(FMetrics.FBackoffMs, fBackoff);
  Inc(FMetrics.FQueueMs, fWait - fBackoff);
  db_tvinfo := nil;
  fSuccess := False;
  fDeferred := False;
  fSource := 'db';
  fOutcome := 'error';
  try
    try
      fTick := mormot.core.os.GetTickCount64;
      try
        db_tvinfo := getTVInfoByShowName(tr.showname);
      finally
        Inc(FMetrics.FDbMs, mormot.core.os.GetTickCount64 - fTick);
      end;

      if db_tvinfo = nil then
      begin
        fSource := 'api';
        if not FSearchInitialized then
        begin
          _TVMazePrepareQuery(tr.showname, FOriginalName, FYear, FCountry, FHadYear, FHadCountry);
          FQueryName := FOriginalName;
          FQueryAttempt := 1;
          FAllowPrefix := not config.ReadBool(section, 'stop_on_englishcheck', True);
          FSearchInitialized := True;
        end;
        fResults := TStringList.Create;
        fTick := mormot.core.os.GetTickCount64;
        Inc(FMetrics.FSearches);
        try
          fStatus := _TVMazeSearchOnce(FQueryName, FOriginalName, FYear, FCountry,
            FHadYear, FHadCountry, False, FAllowPrefix, fResults, sid);
        finally
          Inc(FMetrics.FSearchMs, mormot.core.os.GetTickCount64 - fTick);
          fResults.Free;
        end;

        if fStatus <> tvssMatched then
        begin
          if TVMazeNextQuery(FQueryName, FQueryAttempt, fStatus) then
          begin
            Inc(FMetrics.FReductions);
            QueueNextSearch(300, False);
            fDeferred := True;
          end
          else if (fStatus <> tvssAmbiguous) and
            (attempt < config.ReadInteger(section, 'readd_attempts', 5)) then
          begin
            QueueNextSearch(config.ReadInteger(section, 'readd_interval', 60) * 1000, True);
            fDeferred := True;
          end
          else
          begin
            case fStatus of
              tvssAmbiguous: fOutcome := 'ambiguous';
              tvssNoMatch: fOutcome := 'no_match';
              tvssError: fOutcome := 'search_error';
            end;
            Irc_AddError(Format('<c4>ERROR</c> No unique TVMaze ID for <b>%s</b> (%s): %s',
              [tr.showname, tr.rlsname, fOutcome]));
            SlftpNewsAdd('TVMAZE', Format('<c4>ERROR</c> No unique TVMaze ID for <b>%s</b>',
              [tr.showname]), True);
          end;
          Exit;
        end;

        uurl := Format('https://api.tvmaze.com/shows/%s?embed[]=nextepisode&embed[]=previousepisode', [sid]);
        fTick := mormot.core.os.GetTickCount64;
        try
          if not HttpGetUrl(uurl, tvmaz, fHttpGetErrMsg) then
          begin
            fOutcome := 'fetch_error';
            irc_Adderror(Format('<c4>[FAILED]</c> TVMAZE API fetch for show ID %s --> %s',
              [sid, fHttpGetErrMsg]));
            Exit;
          end;
          if (Trim(tvmaz) = '') or (Trim(tvmaz) = '[]') then
          begin
            fOutcome := 'empty_response';
            readyerror := True;
            Exit;
          end;
          db_tvinfo := parseTVMazeInfos(tvmaz, tr.showname, uurl);
        finally
          Inc(FMetrics.FFetchMs, mormot.core.os.GetTickCount64 - fTick);
        end;
        if db_tvinfo = nil then
        begin
          fOutcome := 'parse_error';
          readyerror := True;
          Exit;
        end;

        // Selection already applied the original title/year/country constraints.
        // Keep the machine-readable echo unchanged; timing goes to STATS separately.
        irc_Addtext_by_key('ADDTVMAZEECHO', Format('%s %s %s',
          [config.ReadString(section, 'addcmd', '!addtvmaze'), tr.rlsname, db_tvinfo.tvmaze_id]));
        fTick := mormot.core.os.GetTickCount64;
        try
          db_tvinfo.Save;
        finally
          Inc(FMetrics.FDbMs, mormot.core.os.GetTickCount64 - fTick);
        end;
      end;

      fTick := mormot.core.os.GetTickCount64;
      try
        db_tvinfo.SetTVDbRelease(tr);
        // Preserve the existing cache-hit path; API results also trigger rule evaluation.
        if fSource = 'api' then
        begin
          ps := FindMostCompleteSite(mainpazo);
          if (ps = nil) and (mainpazo.PazoSitesList.Count > 0) then
            ps := TPazoSite(mainpazo.PazoSitesList[0]);
          if ps <> nil then
            kb_add(netname, channel, ps.Name, tr.section, '', kbeUPDATE, tr.rlsname, '');
        end;
      finally
        Inc(FMetrics.FApplyMs, mormot.core.os.GetTickCount64 - fTick);
      end;
      fOutcome := 'found';
      fSuccess := True;
    except
      on e: Exception do
      begin
        readyerror := True;
        Debug(dpError, section, 'TVMaze lookup failed for %s: %s', [tr.showname, e.Message]);
      end;
    end;
  finally
    db_tvinfo.Free;
    if not fDeferred then
      ReportTiming(fSource, fOutcome, fSuccess);
    ready := True;
  end;
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
  url: String;
begin
  // remove 'scene' tagging
  getShowValues(rls, sname);
  ReplaceText(sname, '.', ' ');
  ReplaceText(sname, '_', ' ');

  url := Format('https://api.tvmaze.com/shows/%s?embed[]=nextepisode&embed[]=previousepisode', [tvmaze_id]);

  if not HttpGetUrl(url, response, fHttpGetErrMsg) then
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

  tvdb := parseTVMazeInfos(response, sname, url);
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
