unit tasktvinfolookup;

interface

uses
  Classes, pazo, tasksunit, taskrace, dbtvinfo, StrUtils;

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

function parseTVMazeInfos(const jsonStr, Showname, uurl: String): TTVInfoDB;
function findTVMazeIDByName(const name: String; Netname: String = ''; Channel: String = ''): String;

implementation

uses
  DateUtils, Contnrs, SysUtils, queueunit, debugunit, configunit, mystrings, kb.releaseinfo,
  kb, http, RegExpr, irc, mrdohutils, uLkJSON, news, sitesunit;

const
  section = 'tasktvinfo';

{ Removes the last whitespace-/dot-/underscore-/hyphen-separated word from
  aName. Returns True if a word was removed and aName still has at least one
  word, False otherwise. Used by progressive search reduction. }
function _ReduceShowNameByOneWord(var aName: String): Boolean;
var
  i: Integer;
begin
  Result := False;
  aName := TrimRight(aName);
  i := Length(aName);
  while (i > 0) and (aName[i] <> ' ') and (aName[i] <> '.') and
        (aName[i] <> '_') and (aName[i] <> '-') do
    Dec(i);
  if i <= 0 then
    Exit; // no separator -> only one word, can't reduce
  aName := TrimRight(Copy(aName, 1, i - 1));
  Result := aName <> '';
end;

{ Performs ONE TVMaze /search/shows?q=... call and tries to find a matching
  show. Returns the TVMaze ID on match, '' on no result (caller can retry with
  a shorter query), or 'FAILED' on a hard error.

  For fromIRC=True, also populates aRes with formatted option lines (the
  user picks one); for fromIRC=False the first qualifying result wins.

  Match rules (evaluated per result returned from the API):
  - Exact name match after onlyEnglishAlpha+replaceTVShowChars (existing behavior)
  - OR (auto-lookup only) prefix match: TVMaze name is a prefix of the original
    full showName, OR the current query is a prefix of TVMaze name. Combined
    with a minimum score threshold (aMinScore) this catches localized titles
    like "Murdoch Mysteries Auf den Spuren..." where TVMaze only knows
    "Murdoch Mysteries". }
function _TVMazeSearchOnce(const aQueryName, aOrigShowAlpha, aYear, aCountry: String;
  aHadYear, aHadCountry, aFromIRC: Boolean; aMinScore: Double; aRes: TStringList): String;
var
  resp: String;
  fHttpGetErrMsg: String;
  jl: TlkJSONlist;
  ddate: TStringList;
  i: Integer;
  showA, showB: String;
  tv_country: String;
  tvScore: Double;
  exactMatch, prefixMatch: Boolean;
begin
  Result := 'FAILED';

  if not HttpGetUrl('https://api.tvmaze.com/search/shows?q=' + replaceTVShowChars(aQueryName, True), resp, fHttpGetErrMsg) then
  begin
    Debug(dpError, section, Format('[FAILED] TVMAZE API search by Name for %s --> %s', [replaceTVShowChars(aQueryName, True), fHttpGetErrMsg]));
    Exit;
  end;

  if (resp = '') or (resp = '[]') then
  begin
    Result := ''; // no result, caller may retry with a shorter query
    Exit;
  end;

  jl := nil;
  try
    try
      jl := TlkJSON.ParseText(AnsiString(resp)) as TlkJSONlist;
    except
      on e: Exception do
      begin
        Debug(dpError, section, '[EXCEPTION] _TVMazeSearchOnce parse: %s', [e.Message]);
        Exit;
      end;
    end;

    if jl = nil then
      Exit;

    ddate := TStringlist.Create;
    try
      for i := 0 to jl.Count - 1 do
      begin
        // Read TVMaze score (results are returned in descending score order)
        try
          tvScore := jl.Child[i].Field['score'].Value;
        except
          tvScore := 0;
        end;

        showA := onlyEnglishAlpha(replaceTVShowChars(ReplaceText(aQueryName, '.', ' ')));
        showB := onlyEnglishAlpha(replaceTVShowChars(jl.Child[i].Field['show'].Field['name'].Value));

        exactMatch := (CompareText(showA, showB) = 0);
        prefixMatch := False;
        if (not exactMatch) and (not aFromIRC) and (showB <> '') and (aOrigShowAlpha <> '') then
        begin
          // TVMaze result name is a prefix of the original full release name
          // (e.g. result "MurdochMysteries" matches original
          // "MurdochMysteriesAufdenSpuren..."). Caps the false-positive risk
          // by combining with the score threshold.
          if (Length(showB) <= Length(aOrigShowAlpha)) and AnsiStartsText(showB, aOrigShowAlpha) then
            prefixMatch := True
          // Or current (already reduced) query is a prefix of the TVMaze name
          // - useful when reduction overshoots (e.g. query "Middag" against
          // result "Middag Paa Michelin Restauranterne").
          else if (showA <> '') and (Length(showA) <= Length(showB)) and AnsiStartsText(showA, showB) then
            prefixMatch := True;
        end;

        // Apply score threshold for non-exact matches in auto-lookup
        if prefixMatch and (tvScore < aMinScore) then
        begin
          Debug(dpSpam, section, 'TVMAZE skip prefix match below score threshold: query="%s" candidate="%s" score=%.3f<%.3f',
            [aQueryName, String(jl.Child[i].Field['show'].Field['name'].Value), tvScore, aMinScore]);
          prefixMatch := False;
        end;

        if exactMatch or prefixMatch then
        begin
          if aHadCountry then
          begin
            tv_country := '';
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

            if UpperCase(tv_country) = UpperCase(aCountry) then
            begin
              if not aFromIRC then
              begin
                Result := String(jl.Child[i].Field['show'].Field['id'].Value);
                Debug(dpSpam, section, 'TVMAZE match (country): query="%s" matched="%s" id=%s score=%.3f exact=%d prefix=%d',
                  [aQueryName, String(jl.Child[i].Field['show'].Field['name'].Value), Result, tvScore, Ord(exactMatch), Ord(prefixMatch)]);
                Break;
              end;
            end;
            aRes.Add(Format('<b>%s %s</b>: %s => %saddtvinfo %s %s %s', [String(jl.Child[i].Field['show'].Field['name'].Value),
              tv_country, String(jl.Child[i].Field['show'].Field['url'].Value), irccmdprefix,
              String(jl.Child[i].Field['show'].Field['id'].Value), ReplaceText(aQueryName, '.', ' '), aCountry])
            );
          end;

          if aHadYear then
          begin
            ddate.Delimiter := '-';
            if jl.Child[i].Field['show'].Field['premiered'].SelfType <> jsNull then
              ddate.DelimitedText := String(jl.Child[i].Field['show'].Field['premiered'].Value)
            else
              ddate.DelimitedText := '1970-01-01';
            if aYear = ddate.Strings[0] then
            begin
              if not aFromIRC then
              begin
                Result := String(jl.Child[i].Field['show'].Field['id'].Value);
                Debug(dpSpam, section, 'TVMAZE match (year): query="%s" matched="%s" id=%s score=%.3f exact=%d prefix=%d',
                  [aQueryName, String(jl.Child[i].Field['show'].Field['name'].Value), Result, tvScore, Ord(exactMatch), Ord(prefixMatch)]);
                Break;
              end;
            end;
            aRes.Add(Format('<b>%s %s</b>: %s => %saddtvinfo %s %s %s', [String(jl.Child[i].Field['show'].Field['name'].Value),
              ddate.Strings[0], String(jl.Child[i].Field['show'].Field['url'].Value), irccmdprefix,
              String(jl.Child[i].Field['show'].Field['id'].Value), ReplaceText(aQueryName, '.', ' '), aYear])
            );
          end;
        end;

        if ((not aHadYear) and (not aHadCountry)) then
        begin
          // For non-strict auto-lookup: the first result is typically the highest
          // score. We only accept it if either the names match (exact or prefix)
          // OR this is the very first attempt (preserves legacy behaviour). The
          // caller's progressive reduction relies on the prefix check kicking in
          // for shorter queries to avoid drifting to an unrelated show.
          if (not aFromIRC) then
          begin
            if exactMatch or prefixMatch or (aMinScore <= 0) then
            begin
              Result := String(jl.Child[i].Field['show'].Field['id'].Value);
              Debug(dpSpam, section, 'TVMAZE match: query="%s" matched="%s" id=%s score=%.3f exact=%d prefix=%d',
                [aQueryName, String(jl.Child[i].Field['show'].Field['name'].Value), Result, tvScore, Ord(exactMatch), Ord(prefixMatch)]);
              Break;
            end;
          end
          else
          begin
            aRes.Add(Format('<b>%s</b>: %s => %saddtvinfo %s %s', [String(jl.Child[i].Field['show'].Field['name'].Value),
              String(jl.Child[i].Field['show'].Field['url'].Value), irccmdprefix,
              String(jl.Child[i].Field['show'].Field['id'].Value), ReplaceText(aQueryName, '.', ' ')])
            );
          end;
        end;
      end;
    finally
      ddate.Free;
    end;
  finally
    jl.Free;
  end;
end;

function findTVMazeIDByName(const name: String; Netname: String = ''; Channel: String = ''): String;
const
  C_MAX_REDUCTION_ATTEMPTS = 5; // 1 initial + up to 4 reductions
  C_REDUCTION_SLEEP_MS = 300;   // throttle to stay below TVMaze rate limit
  C_PREFIX_MATCH_MIN_SCORE = 0.7;
var
  showName, year, country: String;
  origShowAlpha: String;
  hadYear, hadCountry, fromIRC: Boolean;
  x: TRegExpr;
  res: TStringList;
  reducedName: String;
  attempt: Integer;
  attemptResult: String;
begin
  Result := 'FAILED';
  hadYear := False;
  hadCountry := False;
  fromIRC := Boolean((Netname <> '') and (Channel <> ''));
  showName := name;

  x := TRegExpr.Create;
  try
    x.ModifierI := False;
    x.ModifierM := True;

    // Cut off Year tag
    x.Expression := '[-._\s]((19|20)\d{2})[\s._-]?$';
    if x.Exec(showName) then
    begin
      year := x.Match[1];
      if StrToInt(year) < (StrToInt(FormatDateTime('yyyy', Now)) + 10) then
      begin
        showName := x.Replace(showName, '', False);
        hadYear := True;
      end;
    end;

    // Cut off Country tag
    x.Expression := '[-._\s](US|UK|AU|CA|NZ)[\s._-]?$';
    if x.Exec(showName) then
    begin
      country := x.Match[1];
      showName := x.Replace(showName, '', False);
      hadCountry := True;
    end;
  finally
    x.Free;
  end;

  // Captured ONCE before any reduction so prefix-matching always compares
  // against the full original release name, not the reduced query.
  origShowAlpha := onlyEnglishAlpha(replaceTVShowChars(ReplaceText(showName, '.', ' ')));

  res := TStringList.Create;
  try
    // fromIRC: keep legacy behaviour - single attempt, returns CSV of options
    if fromIRC then
    begin
      attemptResult := _TVMazeSearchOnce(showName, origShowAlpha, year, country,
        hadYear, hadCountry, fromIRC, 0, res);
      if (attemptResult = '') then
      begin
        irc_addtext(Netname, Channel, '<c5><b>TVInfo</c></b>: No search result for %s ( %s )',
          [ReplaceText(showName, '.', ' '), replaceTVShowChars(showName, True)]);
        Exit;
      end;
      if attemptResult = 'FAILED' then
      begin
        irc_AddText(Netname, Channel, '<c4>[FAILED]</c> TVMAZE API search by Name for %s',
          [replaceTVShowChars(showName, True)]);
        Exit;
      end;
      if res.Count = 0 then
        Result := 'FAILED'
      else
        Result := res.CommaText;
      Exit;
    end;

    // Strict mode (year or country given): also single attempt, no reduction
    if hadYear or hadCountry then
    begin
      attemptResult := _TVMazeSearchOnce(showName, origShowAlpha, year, country,
        hadYear, hadCountry, False, 0, res);
      if (attemptResult <> '') and (attemptResult <> 'FAILED') then
        Result := attemptResult;
      if attemptResult = '' then
        Debug(dpSpam, section, 'TVMAZE: no search result for "%s" (strict mode, hadYear=%d hadCountry=%d)',
          [showName, Ord(hadYear), Ord(hadCountry)]);
      Exit;
    end;

    // Auto-lookup without year/country: progressive word reduction with
    // score+prefix filter on the reduced attempts.
    reducedName := showName;
    for attempt := 0 to C_MAX_REDUCTION_ATTEMPTS - 1 do
    begin
      if attempt > 0 then
        Sleep(C_REDUCTION_SLEEP_MS); // rate-limit protection (TVMaze ~20/10s)

      // First attempt uses the original lenient behavior (aMinScore=0 means
      // accept the highest-scored result like the legacy code did). Subsequent
      // attempts on a reduced query require a prefix match with a non-trivial
      // score so we don't accept totally unrelated shows.
      if attempt = 0 then
        attemptResult := _TVMazeSearchOnce(reducedName, origShowAlpha, year, country,
          hadYear, hadCountry, False, 0, res)
      else
        attemptResult := _TVMazeSearchOnce(reducedName, origShowAlpha, year, country,
          hadYear, hadCountry, False, C_PREFIX_MATCH_MIN_SCORE, res);

      if (attemptResult <> '') and (attemptResult <> 'FAILED') then
      begin
        Result := attemptResult;
        if attempt > 0 then
          Debug(dpSpam, section, 'TVMAZE: matched after %d reduction(s): "%s" -> id=%s',
            [attempt, reducedName, Result]);
        Exit;
      end;

      // Hard error - don't keep hammering the API
      if attemptResult = 'FAILED' then
        Exit;

      // Try one more reduction
      if not _ReduceShowNameByOneWord(reducedName) then
      begin
        Debug(dpSpam, section, 'TVMAZE: no further reduction possible for "%s" after %d attempt(s)',
          [showName, attempt + 1]);
        Break;
      end;
      Debug(dpSpam, section, 'TVMAZE: no result, reducing query to "%s" (attempt %d/%d)',
        [reducedName, attempt + 1, C_MAX_REDUCTION_ATTEMPTS]);
    end;

    // No match after all attempts
    Debug(dpSpam, section, 'TVMAZE: no match for "%s" after progressive reduction', [showName]);
  finally
    res.Free;
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
      Irc_AddError(Format('<c4>ERROR</c> No TVMaze ID found for <b>%s</b> (%s)', [tr.showname, tr.rlsname]));
      SlftpNewsAdd('TVMAZE', Format('<c4>ERROR</c> No TVMaze ID found for <b>%s</b>', [tr.showname]), True);
    end;

    ready := True;
    Result := True;
    exit;
  end;

  uurl := Format('https://api.tvmaze.com/shows/%s?embed[]=nextepisode&embed[]=previousepisode', [sid]);

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
    Irc_AddError('<c4><b>ERROR</c></b> http response is empty for ' + tr.showname);
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
    Irc_AddError(Format('<c14><b>Info</b></c>: Alphanumeric check dont match! %s <> %s', [onlyEnglishAlpha(showA), onlyEnglishAlpha(showB)]));
    Result := True;
    ready := True;
    exit;
  end;

  try
    irc_Addtext_by_key('ADDTVMAZEECHO', Format('%s %s %s', [config.ReadString(section, 'addcmd', '!addtvmaze'), mainpazo.rls.rlsname, db_tvinfo.tvmaze_id]));
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
    if ((ps = nil) and (mainpazo.PazoSitesList.Count > 0)) then
      ps := TPazoSite(mainpazo.PazoSitesList[0]);
  // don't know why ps can be nil - have to check later
      if ps <> nil then
      begin
        kb_add(netname, channel, ps.Name, mainpazo.rls.section, '', kbeUPDATE, mainpazo.rls.rlsname, '');
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
