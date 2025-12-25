unit dbtvinfo;

interface

uses
  Classes, IniFiles, irc, kb.releaseinfo, Contnrs, dbhandler,
  mormot.orm.core, mormot.core.base, mormot.orm.base, mormot.rest.sqlite3,
  mormot.core.unicode, mormot.core.os, slcriticalsection2, DateUtils;

type
  { NOTE: everything which starts with TVMaze is data from TVMaze }

  TSQLTVInfo = class(TOrm)
  private
    Ftvmaze_id: Integer;
    Fthetvdb_id: Integer;
    Ftvrage_id: Integer;
    Fpremiered_year: Integer;
    Fcountry: RawUTF8;
    Fstatus: RawUTF8;
    Fclassification: RawUTF8;
    Fnetwork: RawUTF8;
    Fgenre: RawUTF8;
    Fended_year: Integer;
    Flast_updated: TDateTime;
    Fnext_date: TDateTime;
    Fnext_season: Integer;
    Fnext_episode: Integer;
    Frating: Integer;
    Fairdays: RawUTF8;
    Ftv_language: RawUTF8;
  published
    property tvmaze_id: Integer read Ftvmaze_id write Ftvmaze_id stored AS_UNIQUE;
    property thetvdb_id: Integer read Fthetvdb_id write Fthetvdb_id;
    property tvrage_id: Integer read Ftvrage_id write Ftvrage_id;
    property premiered_year: Integer read Fpremiered_year write Fpremiered_year;
    property country: RawUTF8 read Fcountry write Fcountry;
    property status: RawUTF8 read Fstatus write Fstatus;
    property classification: RawUTF8 read Fclassification write Fclassification;
    property network: RawUTF8 read Fnetwork write Fnetwork;
    property genre: RawUTF8 read Fgenre write Fgenre;
    property ended_year: Integer read Fended_year write Fended_year;
    property last_updated: TDateTime read Flast_updated write Flast_updated;
    property next_date: TDateTime read Fnext_date write Fnext_date;
    property next_season: Integer read Fnext_season write Fnext_season;
    property next_episode: Integer read Fnext_episode write Fnext_episode;
    property rating: Integer read Frating write Frating;
    property airdays: RawUTF8 read Fairdays write Fairdays;
    property tv_language: RawUTF8 read Ftv_language write Ftv_language;
  end;

  TSQLTVSeries = class(TOrm)
  private
    Frip: RawUTF8;
    Fshowname: RawUTF8;
    Ftvmaze_url: RawUTF8;
    Ftvmaze_id: Integer;
  published
    property rip: RawUTF8 read Frip write Frip stored AS_UNIQUE;
    property showname: RawUTF8 read Fshowname write Fshowname;
    property tvmaze_url: RawUTF8 read Ftvmaze_url write Ftvmaze_url;
    property tvmaze_id: Integer read Ftvmaze_id write Ftvmaze_id;
  end;

var
  glTVInfoDb: TSQLRestClientDB;
  glTVInfoModel: TSQLModel;

type
  { @abstract(Possible return values for special cases in getShowValues procedure)
    @value(tvInitialValue Initial value which is set as default value)
    @value(tvNotMatched For cases where main regex matched but single matches don't contain useful values)
    @value(tvConversionError Value if StrToIntDef failed to convert input)
    @value(tvDatedShow Season value for dated shows)
    @value(tvRegularSerieWithoutSeason Season value for shows which only have an episode tag)
    @value(tvNoExplicitShowTag Shows without season/episode/dated tag (mostly tv movies or sports))
    @value(tvNoEpisodeTag Shows without episode tag (mostly full season releases)) }
  TTVGetShowValuesIdentifier = (tvNoEpisodeTag = -110, tvNoExplicitShowTag = -100, tvRegularSerieWithoutSeason = -90, 
    tvDatedShow = -80, tvConversionError = -70, tvNotMatched = -60, tvInitialValue = -50);

  { @abstract(Possible 'error' values for season and episode info lookups on the web)
    @value(tvSeEpInitialValue Initial value which is set as default value)
    @value(tvSeEpAirdatePrevAndNextOnSameDay Airdate of previous and next episode are on the same day)
    @value(tvSeEpShowEnded Show ended)
    @value(tvSeEpNoNextOrPrev No information about the next episode and next season) }
  TTVSeasonEpisodeWebInfo = (tvSeEpNoNextOrPrev = -6, tvSeEpShowEnded = -5, tvSeEpAirdatePrevAndNextOnSameDay = -4, tvSeEpInitialValue = -3);

  TTVInfoDB = class
  public
    ripname: String;
    rls_showname: String;
    tvmaze_id: String;
    thetvdb_id: String;
    tvrage_id: String;

    tv_showname: String;
    tv_country: String;
    tv_url: String;
    tv_status: String;
    tv_classification: String;
    tv_genres: TStringList;
    tv_days: TStringList;
    tv_network: String;
    tv_language: String;
    tv_premiered_year: integer;
    tv_endedyear: integer;
    tv_running: boolean;
    tv_scripted: boolean;
    tv_next_season: integer;
    tv_next_ep: integer;
    tv_next_date: integer;
    tv_rating: integer; //< tv rating value (max score is 100, min score is 0)
    last_updated: integer;
    tv_daily: boolean;
    constructor Create(const rls_showname: String); //overload;
    destructor Destroy; override;
    function Name: String;
    procedure Save;

    procedure PostResults(rls: String = ''; netname: String = ''; channel: String = '');
    procedure SetTVDbRelease(tr: TTVRelease);
    function Update(fromIRC:Boolean = False): boolean;

    function executeUpdate: boolean;

    procedure setTheTVDbID(const aID: integer);
    procedure setTVRageID(const aID: integer);
  end;

function getTVInfoCount: integer;
function getTVInfoSeriesCount: integer;

function TheTVDbStatus: String;

procedure dbTVInfoInit;
procedure dbTVInfoStart;
procedure dbTVInfoUnInit;

function getTVInfoByShowName(const aRls_Showname: String): TTVInfoDB;
function getTVInfoByReleaseName(const aRLS: String): TTVInfoDB;

function getTVInfoByShowID(const aTVMazeID: String): TTVInfoDB;

procedure saveTVInfos(const TVMazeID: String; tvrage: TTVInfoDB; rls: String = ''; fireKb: boolean = True);

function deleteTVInfoByID(const aID: String): Integer;
function deleteTVInfoByRipName(const aName: String): Integer;

procedure dbtvinfo_AddOrUpdate(const aShowName: string; const aJsonData: RawUTF8);

procedure addTVInfos(const aParams: String);

procedure TVInfoFireKbAdd(const aRls: String; msg: String = '<c3>[TVInfo]</c> %s %s now has TV infos (%s)');

function dbTVInfo_Process(const aNet, aChan, aNick: String; aMSG: String): boolean;

{ Removes scene tagging for TV releases like languages or tvtags and tries to extract showname
  @param(aRlsname Releasename with scene tagging)
  @param(showName Plain TV showname from @link(aRlsname) without any scene tags) }
procedure getShowValues(const aRlsname: String; out showName: String); overload;

{ Removes scene tagging for TV releases like languages or tvtags and tries to extract showname, season and episode from releasename
  @param(aRlsname Releasename with scene tagging)
  @param(showName Plain TV showname from @link(aRlsname) without any scene tags)
  @param(season Extracted season number from @link(aRlsname))
  @param(episode Extracted episode number from @link(aRlsname)) }
procedure getShowValues(const aRlsname: String; out showName: String; out season: integer; out episode: int64); overload;

{ Replaces TV showname words (and, at) with (&, @) and replaces whitespaces with dots
  @param(aName TV showname)
  @param(forWebFetch If set to @true, it replaces whitespaces, dots and underscores with '+'' for better web search results)
  @returns(TV showname with replaced chars) }
function replaceTVShowChars(const aName: String; forWebFetch: boolean = false): String;

function TVInfoDbAlive: boolean;

implementation

uses
  SysUtils, Math, configunit, StrUtils, mystrings, console, sitesunit, queueunit, slmasks, http, RegExpr,
  debugunit, tasktvinfolookup, pazo, mrdohutils, uLkJSON, SyncObjs, sllanguagebase, mormot.db.sql.sqlite3,
  Generics.Collections, news, kb, mormot.core.data, mormot.core.json, mormot.core.variants;

const
  section = 'tasktvinfo';

var
  SQLite3Lock: TSlCriticalSection2 = nil; //< Critical Section used for read/write blocking as concurrently does not work flawless
  addtinfodbcmd: String; //< irc command for addtvmaze channel, default: !addtvmaze
  LastAddtvmazeIDs: TList<String>; // ugly way to prevent looping of !addtvmaze announces when info is already stored with different ID

function replaceTVShowChars(const aName: String; forWebFetch: boolean = false): String;
var
  fHelper: String;
begin
  // this is a protection!!!! Dispatches will not end up in Disp@ches
  fHelper := ReplaceText(aName, ' ', '.');
  fHelper := ReplaceText(fHelper, '.and.', '.%26.');
  fHelper := ReplaceText(fHelper, '_and_', '_%26_');
  fHelper := ReplaceText(fHelper, '', Chr(39));
  fHelper := ReplaceText(fHelper, '''', '');

  if forWebFetch then
  begin
    fHelper := ReplaceText(fHelper, ' ', '+');
    fHelper := ReplaceText(fHelper, '.', '+');
    fHelper := ReplaceText(fHelper, '_', '+');
  end;

  // do not end up with 'tv.show.name.' or 'tv+show+name+'
  if CharInSet(fHelper[Length(fHelper)], ['.', '+']) then
    SetLength(fHelper, Length(fHelper) - 1);

  Result := fHelper;
end;

procedure getShowValues(const aRlsname: String; out showName: String);
var
  fSeason: integer;
  fEpisode: int64;
begin
  getShowValues(aRlsname, showName, fSeason, fEpisode);
end;

procedure getShowValues(const aRlsname: String; out showName: String; out season: integer; out episode: int64);
var
  rx: TRegexpr;
  ttags, ltags: TStringlist;
  showDate: TDateTime;

  procedure SetNotMatchedValues;
  begin
    season := Ord(tvNotMatched);
    episode := Ord(tvNotMatched);
  end;

begin
  showName := aRlsname;

  // default values for not parsed/matched
  season := Ord(tvInitialValue);
  episode := Ord(tvInitialValue);

  rx := TRegexpr.Create;
  try
    rx.ModifierI := True;
    rx.ModifierG := True;


    (* dated shows like Stern.TV.2016.01.27.GERMAN.Doku.WS.dTV.x264-FiXTv *)
    (* YYYY/MM/DD *)
    rx.Expression := '(.*)[\._-](\d{4})[\.\-](\d{2})[\.\-](\d{2}|\d{2}[\.\-]\d{2}[\.\-]\d{4})[\._-](.*)';
    if rx.Exec(aRlsname) then
    begin
      showName := rx.Match[1];
      SetNotMatchedValues;

      {$IFDEF DEBUG}
        Debug(dpSpam, section, Format('getShowValues-case-1 - matches: %s %s %s %s', [rx.Match[1], rx.Match[2], rx.Match[3], rx.Match[4]]));
      {$ENDIF}

      if DateUtils.IsValidDate(StrToInt(rx.Match[2]), StrToInt(rx.Match[3]), StrToInt(rx.Match[4]))
       and TryEncodeDateTime(StrToInt(rx.Match[2]), StrToInt(rx.Match[3]), StrToInt(rx.Match[4]), 0, 0 , 0, 0 , showDate) then
      begin
        season := Ord(tvDatedShow);
        episode := DateTimeToUnix(showDate);
      end
      else
      begin
        irc_Adderror('<c4><b>getShowValues ERROR</c></b>: ' + rx.Match[4] + '-' + rx.Match[3] + '-' + rx.Match[2] + ' is no valid date.');
        Debug(dpError, section, 'getShowValues ERROR: ' + rx.Match[4] + '-' + rx.Match[3] + '-' + rx.Match[2] + ' is no valid date.');
      end;

      {$IFDEF DEBUG}
        Debug(dpSpam, section, Format('getShowValues-case-1 - rls: %s, showname: %s, season: %d, episode: %d', [aRlsname, showName, season, episode]));
      {$ENDIF}

      exit;
    end;


    (* regular series tagging like S01E02 and 1x02 *)
    rx.Expression := '(.*?)[._-](S(\d{1,3})(E(\d{1,3}))?|(\d+)x(\d+))';
    if rx.Exec(aRlsname) then
    begin
      showName := rx.Match[1];
      SetNotMatchedValues;

      {$IFDEF DEBUG}
        Debug(dpSpam, section, Format('getShowValues-case-2 - matches: %s %s %s %s %s', [rx.Match[1], rx.Match[3], rx.Match[5], rx.Match[6], rx.Match[7]]));
      {$ENDIF}

      if StrToIntDef(rx.Match[3], 0) > 0 then
      begin
        season := StrToIntDef(rx.Match[3], Ord(tvConversionError));

        if StrToIntDef(rx.Match[5], -1) = -1 then
          episode := Ord(tvNoEpisodeTag)
        else
          episode := StrToIntDef(rx.Match[5], Ord(tvConversionError));

        {$IFDEF DEBUG}
          Debug(dpSpam, section, Format('getShowValues-case-2-1 - rls: %s, showname: %s, season: %d, episode: %d', [aRlsname, showName, season, episode]));
        {$ENDIF}

        exit;
      end;

      if StrToIntDef(rx.Match[6], 0) > 0 then
      begin
        season := StrToIntDef(rx.Match[6], Ord(tvConversionError));
        episode := StrToIntDef(rx.Match[7], Ord(tvConversionError));

        {$IFDEF DEBUG}
          Debug(dpSpam, section, Format('getShowValues-case-2-2 - rls: %s, showname: %s, season: %d, episode: %d', [aRlsname, showName, season, episode]));
        {$ENDIF}

        exit;
      end;
    end;


    rx.Expression := '(.*?)[._-]((S(taffel)?)(\d{1,3}))?[._]?(D|E|EP|Episode|DVD[._]?|Part[_.]?)(\d{1,3})(.*?)';
    if rx.Exec(aRlsname) then
    begin
      showName := rx.Match[1];
      SetNotMatchedValues;

      {$IFDEF DEBUG}
        Debug(dpSpam, section, Format('getShowValues-case-3 - matches: %s %s %s', [rx.Match[1], rx.Match[5], rx.Match[7]]));
      {$ENDIF}

      season := Ord(tvRegularSerieWithoutSeason);
      episode := StrToIntDef(rx.Match[7], Ord(tvConversionError));

      if StrToIntDef(rx.Match[5], 0) > 0 then
      begin
        episode := StrToIntDef(rx.Match[5], Ord(tvConversionError));

        {$IFDEF DEBUG}
          Debug(dpSpam, section, Format('getShowValues-case-3-1 - rls: %s, showname: %s, season: %d, episode: %d', [aRlsname, showName, season, episode]));
        {$ENDIF}
      end
      else
      begin
        episode := StrToIntDef(rx.Match[7], Ord(tvConversionError));

        {$IFDEF DEBUG}
          Debug(dpSpam, section, Format('getShowValues-case-3-2 - rls: %s, showname: %s, season: %d, episode: %d', [aRlsname, showName, season, episode]));
        {$ENDIF}
      end;

      exit;
    end;


    rx.Expression := '(.*?)[._-]((W|V|S(taffel|eason|aison))[._]?(\d{1,3})[._]?)?(SE|DIS[CK]|Y|E|EPS?|VOL(UME)?)[._]?(\d{1,3}).*?';
    if rx.Exec(aRlsname) then
    begin
      showName := rx.Match[1];
      SetNotMatchedValues;

      {$IFDEF DEBUG}
        Debug(dpSpam, section, Format('getShowValues-case-4 - matches: %s %s %s', [rx.Match[1], rx.Match[4], rx.Match[7]]));
      {$ENDIF}

      if StrToIntDef(rx.Match[4], 0) > 0 then
      begin
        episode := StrToIntDef(rx.Match[4], Ord(tvConversionError));
        season := StrToIntDef(rx.Match[4], Ord(tvConversionError));

        {$IFDEF DEBUG}
          Debug(dpSpam, section, Format('getShowValues-case-4-1 - rls: %s, showname: %s, season: %d, episode: %d', [aRlsname, showName, season, episode]));
        {$ENDIF}
      end
      else
      begin
        episode := StrToIntDef(rx.Match[7], Ord(tvConversionError));
        season := StrToIntDef(rx.Match[7], Ord(tvConversionError));

        {$IFDEF DEBUG}
          Debug(dpSpam, section, Format('getShowValues-case-4-2 - rls: %s, showname: %s, season: %d, episode: %d', [aRlsname, showName, season, episode]));
        {$ENDIF}
      end;

      exit;
    end;


    (* remove scene/language/tv tags from releasename *)
    ttags := TStringlist.Create;
    try
      ttags.Assign(GlTvTags);
      ttags.Delimiter := '|';

      ltags := TStringlist.Create;
      try
        SLGetLanguagesExpression(ltags);
        ltags.Delimiter := '|';

        // language and tvtags (needs to be removed first due to enforcing of .<lang|tag>.)
        rx.Expression := '[._\-\s](' + ltags.DelimitedText + '|' + ttags.DelimitedText + ')[._\-\s].*$';
        showName := rx.Replace(showName, '', False);
        // scene specific tags for <showname>.REAL.<scenetags>
        rx.Expression := '[._\-\s]REAL[._\-\s]((480|720|1080|1440|2160)(p|i)|REPACK|PROPER|INTERNAL|(DIR|NFO|SFV|PROOF|SAMPLE)[._]?FIX).*$';
        showName := rx.Replace(showName, '', False);
        // scene specific tags
        rx.Expression := '[._\-\s]((19|20)\d{2}|(480|720|1080|1440|2160)(p|i)|REPACK|PROPER|INTERNAL|(DIR|NFO|SFV|PROOF|SAMPLE)[._]?FIX).*$';
        showName := rx.Replace(showName, '', False);

        season := Ord(tvNoExplicitShowTag);
        episode := Ord(tvNoExplicitShowTag);

        {$IFDEF DEBUG}
          Debug(dpSpam, section, Format('getShowValues-case-5 - rls: %s, showname: %s, season: %d, episode: %d', [aRlsname, showName, season, episode]));
        {$ENDIF}

      finally
        ltags.free;
      end;
    finally
      ttags.free;
    end;

  finally
    rx.free;
  end;
end;

{ TTVInfoDB }

procedure TTVInfoDB.setTheTVDbID(const aID: integer);
var
  fInfo: TSQLTVInfo;
begin
  if glTVInfoDb = nil then exit;
  
  fInfo := TSQLTVInfo.CreateAndFillPrepare(glTVInfoDb.Client, 'tvmaze_id = ?', [], [StrToIntDef(tvmaze_id, 0)]);
  try
    if fInfo.FillOne then
    begin
      fInfo.thetvdb_id := aID;
      glTVInfoDb.Update(fInfo);
    end;
  finally
    fInfo.Free;
  end;
end;

procedure TTVInfoDB.setTVRageID(const aID: integer);
var
  fInfo: TSQLTVInfo;
begin
  if glTVInfoDb = nil then exit;
  
  fInfo := TSQLTVInfo.CreateAndFillPrepare(glTVInfoDb.Client, 'tvmaze_id = ?', [], [StrToIntDef(tvmaze_id, 0)]);
  try
    if fInfo.FillOne then
    begin
      fInfo.tvrage_id := aID;
      glTVInfoDb.Update(fInfo);
    end;
  finally
    fInfo.Free;
  end;
end;

procedure TTVInfoDB.Save;
var
  fInfo: TSQLTVInfo;
  fSeries: TSQLTVSeries;
  fDoUpdate: boolean;
begin
  if glTVInfoDb = nil then exit;

  fInfo := TSQLTVInfo.CreateAndFillPrepare(glTVInfoDb.Client, 'tvmaze_id = ?', [], [StrToIntDef(tvmaze_id, 0)]);
  try
    fDoUpdate := fInfo.FillOne;
    if not fDoUpdate then
    begin
      fInfo.Free;
      fInfo := TSQLTVInfo.Create;
      fInfo.tvmaze_id := StrToIntDef(tvmaze_id, 0);
    end;

    fInfo.thetvdb_id := StrToIntDef(thetvdb_id, -1);
    fInfo.tvrage_id := StrToIntDef(tvrage_id, -1);
    fInfo.premiered_year := tv_premiered_year;
    fInfo.country := StringToUTF8(tv_country);
    fInfo.status := StringToUTF8(tv_status);
    fInfo.classification := StringToUTF8(tv_classification);
    fInfo.network := StringToUTF8(tv_network);
    fInfo.genre := StringToUTF8(tv_genres.CommaText);
    fInfo.ended_year := tv_endedyear;
    fInfo.last_updated := now();
    fInfo.airdays := StringToUTF8(tv_days.CommaText);
    fInfo.next_date := UnixToDateTime(tv_next_date);
    fInfo.next_season := tv_next_season;
    fInfo.next_episode := tv_next_ep;
    fInfo.tv_language := StringToUTF8(tv_language);
    fInfo.rating := tv_rating;

    if fDoUpdate then
      glTVInfoDb.Update(fInfo)
    else
      glTVInfoDb.Add(fInfo, True);
      
    last_updated := DateTimeToUnix(now());
  finally
    fInfo.Free;
  end;

  fSeries := TSQLTVSeries.CreateAndFillPrepare(glTVInfoDb.Client, 'rip = ?', [], [StringToUTF8(rls_showname)]);
  try
    fDoUpdate := fSeries.FillOne;
    if not fDoUpdate then
    begin
      fSeries.Free;
      fSeries := TSQLTVSeries.Create;
      fSeries.rip := StringToUTF8(rls_showname);
    end;

    fSeries.showname := StringToUTF8(tv_showname);
    fSeries.tvmaze_id := StrToIntDef(tvmaze_id, 0);
    fSeries.tvmaze_url := StringToUTF8(tv_url);

    if fDoUpdate then
      glTVInfoDb.Update(fSeries)
    else
      glTVInfoDb.Add(fSeries, True);
  finally
    fSeries.Free;
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
  tr.tvrating := tv_rating;

  if YearOf(now) = tv_next_season then
  begin
    tv_next_season := tr.season;
  end;

  case tv_next_season of
    Ord(tvSeEpAirdatePrevAndNextOnSameDay):
      begin
        //Prev and Next are on the same day.
        tv_next_ep := tr.episode;
        tv_next_season := tr.season;
        tr.currentseason := true;
        tr.currentepisode := true;
        tr.currentair := true;
      end;
    Ord(tvSeEpShowEnded):
      begin
        //show is ended.
        tv_next_ep := 0;
        tv_next_season := 0;
        tr.currentseason := False;
        tr.currentepisode := False;
        tr.currentair := False;
      end;
    Ord(tvSeEpNoNextOrPrev):
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
    Ord(tvDatedShow): // probably set by TTVRelease.Create
      begin
        // dated show
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

  tr.FLookupDone := True;

  if config.ReadBool(section, 'post_lookup_infos', False) then
    PostResults(rls_showname);
end;

constructor TTVInfoDB.Create(const rls_showname: String);
begin
  self.rls_showname := rls_showname;
  self.tv_genres := TStringList.Create;
  self.tv_genres.QuoteChar := '"';
  self.tv_days := TStringList.Create;
  self.tv_days.QuoteChar := '"';
  self.tv_endedyear := -1;
  self.tv_rating := 0;
  self.last_updated:= 3817;
  self.tv_next_ep := Ord(tvSeEpInitialValue);
  self.tv_next_season := Ord(tvSeEpInitialValue);
end;

destructor TTVInfoDB.Destroy;
begin
  self.tv_genres.Free;
  self.tv_days.free;
  inherited;
end;

function TTVInfoDB.Name: String;
begin
  try
    Result := 'TVInfo :' + rls_showname + ' : ';
  except
    Result := 'TVInfo';
  end;
end;

procedure TTVInfoDB.PostResults(rls: String = ''; netname: String = ''; channel: String = '');
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
        toAnnounce.Add(Format('<c10>[<b>TVInfo</b>]</c> <b>%s</b> (%d - %d) - <b>TVMaze info</b> %s', [rls, tv_premiered_year, tv_endedyear, tv_url]))
      else
        toAnnounce.Add(Format('<c10>[<b>TVInfo</b>]</c> <b>%s</b> - <b>Premiere Year</b> %d - <b>TVMaze info</b> %s', [rls, tv_premiered_year, tv_url]));

      if ((tv_next_season > 0) and (tv_next_ep > 0)) then
        toAnnounce.Add(Format('<c10>[<b>TVInfo</b>]</c> <b>Season</b> %d - <b>Episode</b> %d - <b>Date</b> %s', [tv_next_season, tv_next_ep, FormatDateTime('yyyy-mm-dd', UnixToDateTime(tv_next_date))]));

      toAnnounce.Add(Format('<c10>[<b>TVInfo</b>]</c> <b>Genre</b> %s - <b>Classification</b> %s - <b>Status</b> %s', [tv_genres.CommaText, tv_classification, tv_status]));
      toAnnounce.Add(Format('<c10>[<b>TVInfo</b>]</c> <b>Country</b> %s - <b>Network</b> %s - <b>Language</b> %s - <b>Rating</b> %d/100', [tv_country, tv_network, tv_language, tv_rating]));
      toAnnounce.Add(Format('<c10>[<b>TVInfo</b>]</c> <b>Last update</b> %s', [DateTimeToStr(UnixToDateTime(last_updated))]));
    end
    else
    begin
      if tv_endedyear > 0 then
        toAnnounce.Add(Format('(<c9>i</c>)....<c7><b>TVInfo (db)</b></c>....... <c0><b>info for</c></b> ...........: <b>%s</b> (%s - %s) - %s', [rls, IntToStr(tv_premiered_year), IntToStr(tv_endedyear), tv_url]))
      else
        toAnnounce.Add(Format('(<c9>i</c>)....<c7><b>TVInfo (db)</b></c>....... <c0><b>info for</c></b> ...........: <b>%s</b> (%s) - %s', [rls, IntToStr(tv_premiered_year), tv_url]));

      if ((tv_next_season > 0) and (tv_next_ep > 0)) then
        toAnnounce.Add(Format('(<c9>i</c>)....<c7><b>TVInfo (db)</b></c>....... <c9><b>Season/Episode (Date)</c></b> ...........: <b>%d.%d</b> (%s)', [tv_next_season, tv_next_ep, FormatDateTime('yyyy-mm-dd', UnixToDateTime(tv_next_date))]));

      toAnnounce.Add(Format('(<c9>i</c>)....<c7><b>TVInfo (db)</b></c>.. <c9><b>Genre (Class) @ Status</c></b> ..: %s (%s) @ %s', [tv_genres.CommaText, tv_classification, tv_status]));
      toAnnounce.Add(Format('(<c9>i</c>)....<c7><b>TVInfo (db)</b></c>....... <c4><b>Country/Channel</c></b> ....: <b>%s</b> (%s) ', [tv_country, tv_network]));
      toAnnounce.Add(Format('(<c9>i</c>)....<c7><b>TVInfo (db)</b></c>....... <c4><b>Last update</c></b> ....: <b>%s</b>', [FormatDateTime('yyyy-mm-dd hh:nn:ss', UnixToDateTime(last_updated))]));
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
  fInfo: TSQLTVInfo;
begin
  Result := False;
  if glTVInfoDb = nil then exit;

  fInfo := TSQLTVInfo.CreateAndFillPrepare(glTVInfoDb.Client, 'tvmaze_id = ?', [], [StrToIntDef(tvmaze_id, 0)]);
  try
    if fInfo.FillOne then
    begin
      fInfo.thetvdb_id := StrToIntDef(thetvdb_id, -1);
      fInfo.tvrage_id := StrToIntDef(tvrage_id, -1);
      fInfo.status := StringToUTF8(tv_status);
      fInfo.country := StringToUTF8(tv_country);
      fInfo.tv_language := StringToUTF8(tv_language);
      fInfo.network := StringToUTF8(tv_network);
      fInfo.classification := StringToUTF8(tv_classification);
      fInfo.genre := StringToUTF8(tv_genres.CommaText);
      fInfo.airdays := StringToUTF8(tv_days.CommaText);
      fInfo.premiered_year := tv_premiered_year;
      fInfo.ended_year := tv_endedyear;
      fInfo.next_date := UnixToDateTime(tv_next_date);
      fInfo.next_season := tv_next_season;
      fInfo.next_episode := tv_next_ep;
      fInfo.rating := tv_rating;
      fInfo.last_updated := now();
      
      Result := glTVInfoDb.Update(fInfo);
    end;
  finally
    fInfo.Free;
  end;
end;

procedure dbtvinfo_AddOrUpdate(const aShowName: string; const aJsonData: RawUTF8);
var
  TV: TSQLTVInfo;
  Series: TSQLTVSeries;
  Doc: TDocVariantData;
  fDoUpdate: boolean;
  fID: Integer;
begin
  if glTVInfoDb = nil then Exit;

  if not Doc.InitJson(aJsonData) then Exit;

  fID := Doc.I['id'];
  if fID = 0 then Exit;

  TV := TSQLTVInfo.CreateAndFillPrepare(glTVInfoDb.Client, 'tvmaze_id = ?', [], [fID]);
  try
    fDoUpdate := TV.FillOne;
    if not fDoUpdate then
    begin
      TV.Free;
      TV := TSQLTVInfo.Create;
      TV.tvmaze_id := fID;
    end;

    TV.premiered_year := StrToIntDef(Copy(UTF8ToString(Doc.U['premiered']), 1, 4), -1);
    TV.status := Doc.U['status'];
    TV.classification := Doc.U['type'];
    TV.genre := Doc.U['genres'];
    TV.tv_language := Doc.U['language'];
    
    if Doc.Exists('network') then
    begin
      TV.network := Doc.U['network.name'];
      TV.country := Doc.U['network.country.code'];
    end
    else if Doc.Exists('webChannel') then
    begin
      TV.network := Doc.U['webChannel.name'];
      TV.country := Doc.U['webChannel.country.code'];
    end;

    if TV.country = 'US' then TV.country := 'USA';
    if TV.country = 'GB' then TV.country := 'UK';

    TV.last_updated := now();
    
    if fDoUpdate then
      glTVInfoDb.Update(TV)
    else
      glTVInfoDb.Add(TV, True);
  finally
    TV.Free;
  end;

  if aShowName <> '' then
  begin
    Series := TSQLTVSeries.CreateAndFillPrepare(glTVInfoDb.Client, 'rip = ?', [], [StringToUTF8(aShowName)]);
    try
      fDoUpdate := Series.FillOne;
      if not fDoUpdate then
      begin
        Series.Free;
        Series := TSQLTVSeries.Create;
        Series.rip := StringToUTF8(aShowName);
      end;
      Series.showname := Doc.U['name'];
      Series.tvmaze_id := fID;
      Series.tvmaze_url := Doc.U['url'];

      if fDoUpdate then
        glTVInfoDb.Update(Series)
      else
        glTVInfoDb.Add(Series, True);
    finally
      Series.Free;
    end;
  end;
end;

function TTVInfoDB.Update(fromIRC: boolean = False): boolean;
var
  rls_name: String;
  respo: String;
  fHttpGetErrMsg: String;
  url: String;
begin
  Result := False;
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

  url := Format('https://api.tvmaze.com/shows/%s?embed[]=nextepisode&embed[]=previousepisode', [tvmaze_id]);
  if not HttpGetUrl(url, respo, fHttpGetErrMsg) then
  begin
    Debug(dpError, section, Format('[FAILED] TVMaze API Update: --> %s ', [fHttpGetErrMsg]));
    irc_Adderror(Format('<c4>[FAILED]</c> TVMaze API Update --> %s', [fHttpGetErrMsg]));
    exit;
  end;

  if ((respo = '') or (respo = '[]')) then
  begin
    irc_Adderror(Format('<c4>TTVInfoDB</c>: No Result from TVMaze API when updating %s', [tvmaze_id]));
    Exit;
  end;

  try
    self := parseTVMazeInfos(respo, '', url);
  except on e: Exception do
    begin
      irc_Adderror(Format('<c4>[EXCEPTION]</c> TTVInfoDB.Update: %s', [e.Message]));
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
begin
  Result := 0;
  if glTVInfoDb <> nil then
  begin
    try
      Result := glTVInfoDb.TableRowCount(TSQLTVInfo);
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] getTVInfoCount: %s', [e.Message]));
      end;
    end;
  end;
end;

function getTVInfoSeriesCount: integer;
begin
  Result := 0;
  if glTVInfoDb <> nil then
  begin
    try
      Result := glTVInfoDb.TableRowCount(TSQLTVSeries);
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] getTVInfoSeriesCount: %s', [e.Message]));
      end;
    end;
  end;
end;

function TheTVDbStatus: String;
begin
  Result := Format('<b>TVInfo.db</b>: %d Series, with %d infos', [getTVInfoSeriesCount, getTVInfoCount]);
end;

function deleteTVInfoByID(const aID: String): Integer;
var
  fID: integer;
  fInfo: TSQLTVInfo;
  fSeries: TSQLTVSeries;
begin
  Result := 1;
  if glTVInfoDb = nil then exit;

  fID := StrToIntDef(aID, -1);
  if fID = -1 then exit;

  glTVInfoDb.BatchStart;
  try
    fInfo := TSQLTVInfo.CreateAndFillPrepare(glTVInfoDb.Client, 'tvmaze_id = ?', [], [fID]);
    try
      while fInfo.FillOne do
        glTVInfoDb.Delete(TSQLTVInfo, fInfo.IDValue);
    finally
      fInfo.Free;
    end;

    fSeries := TSQLTVSeries.CreateAndFillPrepare(glTVInfoDb.Client, 'tvmaze_id = ?', [], [fID]);
    try
      while fSeries.FillOne do
        glTVInfoDb.Delete(TSQLTVSeries, fSeries.IDValue);
    finally
      fSeries.Free;
    end;
    
    glTVInfoDb.BatchSend;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] deleteTVInfoByID: %s', [e.Message]));
      Result := 10;
    end;
  end;
end;

function deleteTVInfoByRipName(const aName: String): Integer;
var
  fSeries: TSQLTVSeries;
  fRip: RawUTF8;
begin
  Result := 0;
  if glTVInfoDb = nil then exit;

  fRip := StringToUTF8(aName);
  fSeries := TSQLTVSeries.CreateAndFillPrepare(glTVInfoDb.Client, 'rip = ?', [fRip]);
  try
    if fSeries.FillOne then
    begin
      if glTVInfoDb.Delete(TSQLTVSeries, fSeries.IDValue) then
        Result := 1;
    end;
  finally
    fSeries.Free;
  end;
end;

function getTVInfoByShowName(const aRls_Showname: String): TTVInfoDB;
var
  tvi: TTVInfoDB;
  fSeries: TSQLTVSeries;
  fInfo: TSQLTVInfo;
  fRip: RawUTF8;
begin
  Result := nil;
  if glTVInfoDb = nil then exit;

  if (aRls_Showname = '') then
  begin
    Debug(dpError, section, '[EXCEPTION] getTVInfoByShowName: rls_showname is empty');
    exit;
  end;

  fRip := StringToUTF8(aRls_Showname);
  fSeries := TSQLTVSeries.CreateAndFillPrepare(glTVInfoDb.Client, 'rip = ?', [fRip]);
  try
    if fSeries.FillOne then
    begin
      fInfo := TSQLTVInfo.CreateAndFillPrepare(glTVInfoDb.Client, 'tvmaze_id = ?', [fSeries.tvmaze_id]);
      try
        if fInfo.FillOne then
        begin
          tvi := TTVInfoDB.Create(aRls_Showname);

          tvi.tv_showname := UTF8ToString(fSeries.showname);
          tvi.tv_url := UTF8ToString(fSeries.tvmaze_url);
          tvi.tvmaze_id := IntToStr(fInfo.tvmaze_id);
          tvi.thetvdb_id := IntToStr(fInfo.thetvdb_id);
          tvi.tvrage_id := IntToStr(fInfo.tvrage_id);
          tvi.tv_premiered_year := fInfo.premiered_year;
          tvi.tv_country := UTF8ToString(fInfo.country);
          tvi.tv_status := UTF8ToString(fInfo.status);
          tvi.tv_classification := UTF8ToString(fInfo.classification);
          tvi.tv_network := UTF8ToString(fInfo.network);
          tvi.tv_genres.CommaText := UTF8ToString(fInfo.genre);
          tvi.tv_endedyear := fInfo.ended_year;
          tvi.last_updated := DateTimeToUnix(fInfo.last_updated);
          tvi.tv_next_date := DateTimeToUnix(fInfo.next_date);
          tvi.tv_next_season := fInfo.next_season;
          tvi.tv_next_ep := fInfo.next_episode;
          tvi.tv_days.CommaText := UTF8ToString(fInfo.airdays);
          tvi.tv_rating := fInfo.rating;
          tvi.tv_language:= UTF8ToString(fInfo.tv_language);

          tvi.tv_running := Boolean( (SysUtils.LowerCase(tvi.tv_status) = 'running') or (SysUtils.LowerCase(tvi.tv_status) = 'in development') );
          tvi.tv_scripted := Boolean(SysUtils.LowerCase(tvi.tv_classification) = 'scripted');

          Result := tvi;
        end;
      finally
        fInfo.Free;
      end;
    end;
  finally
    fSeries.Free;
  end;
end;

function getTVInfoByReleaseName(const aRLS: String): TTVInfoDB;
var
  showname: String;
begin
  Result := nil;
  showname := aRLS;
  getShowValues(aRLS, showname);
  showname := ReplaceText(showname, '.', ' ');
  showname := ReplaceText(showname, '_', ' ');

  if (showname <> '') then
  begin
    Result := getTVInfoByShowName(showname);
  end;
end;

function getTVInfoByShowID(const aTVMazeID: String): TTVInfoDB;
var
  tvi: TTVInfoDB;
  fInfo: TSQLTVInfo;
  fSeries: TSQLTVSeries;
begin
  Result := nil;
  if glTVInfoDb = nil then exit;

  if (aTVMazeID = '') then
  begin
    Debug(dpError, section, '[EXCEPTION] getTVInfoByShowID: TVMaze ID is empty');
    exit;
  end;

  fInfo := TSQLTVInfo.CreateAndFillPrepare(glTVInfoDb.Client, 'tvmaze_id = ?', [], [StrToIntDef(aTVMazeID, 0)]);
  try
    if fInfo.FillOne then
    begin
      fSeries := TSQLTVSeries.CreateAndFillPrepare(glTVInfoDb.Client, 'tvmaze_id = ?', [], [fInfo.tvmaze_id]);
      try
        if fSeries.FillOne then
          tvi := TTVInfoDB.Create(UTF8ToString(fSeries.rip))
        else
          tvi := TTVInfoDB.Create('');

        tvi.tv_showname := UTF8ToString(fSeries.showname);
        tvi.tv_url := UTF8ToString(fSeries.tvmaze_url);
        tvi.tvmaze_id := IntToStr(fInfo.tvmaze_id);
        tvi.thetvdb_id := IntToStr(fInfo.thetvdb_id);
        tvi.tvrage_id := IntToStr(fInfo.tvrage_id);
        tvi.tv_premiered_year := fInfo.premiered_year;
        tvi.tv_country := UTF8ToString(fInfo.country);
        tvi.tv_status := UTF8ToString(fInfo.status);
        tvi.tv_classification := UTF8ToString(fInfo.classification);
        tvi.tv_network := UTF8ToString(fInfo.network);
        tvi.tv_genres.CommaText := UTF8ToString(fInfo.genre);
        tvi.tv_endedyear := fInfo.ended_year;
        tvi.last_updated := DateTimeToUnix(fInfo.last_updated);
        tvi.tv_next_date := DateTimeToUnix(fInfo.next_date);
        tvi.tv_next_season := fInfo.next_season;
        tvi.tv_next_ep := fInfo.next_episode;
        tvi.tv_days.CommaText := UTF8ToString(fInfo.airdays);
        tvi.tv_rating := fInfo.rating;
        tvi.tv_language:= UTF8ToString(fInfo.tv_language);

        tvi.tv_running := Boolean( (SysUtils.LowerCase(tvi.tv_status) = 'running') or (SysUtils.LowerCase(tvi.tv_status) = 'in development') );
        tvi.tv_scripted := Boolean(SysUtils.LowerCase(tvi.tv_classification) = 'scripted');

        Result := tvi;
      finally
        fSeries.Free;
      end;
    end;
  finally
    fInfo.Free;
  end;
end;

procedure addTVInfos(const aParams: String);
var
  rls: String;
  tv_showid: String;
  dbtvinfo: TTVInfoDB;
begin
  rls := '';
  rls := SubString(aParams, ' ', 1);
  tv_showid := '';
  tv_showid := SubString(aParams, ' ', 2);

  if ((rls <> '') and (tv_showid <> '')) then
  begin
    dbtvinfo := getTVInfoByShowID(tv_showid);
    try
      if (dbtvinfo = nil) then
      begin
        if not LastAddtvmazeIDs.Contains(tv_showid) then
        begin
          // if the list grow to more than 50 items, delete the first 25
          if LastAddtvmazeIDs.Count > 50 then
          begin
            LastAddtvmazeIDs.DeleteRange(0, 25);
          end;
          LastAddtvmazeIDs.Add(tv_showid);

          // create an INSERT task for non existing show
          try
            AddTask(TPazoHTTPTVInfoTask.Create(tv_showid, rls));
          except
            on e: Exception do
            begin
              Debug(dpError, section, Format('[EXCEPTION] addTVInfos: %s', [e.Message]));
              exit;
            end;
          end;
        end
        else
        begin
          SlftpNewsAdd('TVMAZE', Format('Possible mismatch for <b>%s</b> with TVMaze ID <b>%s</b>', [rls, tv_showid]), True);
        end;
      end
      else if (DaysBetween(UnixToDateTime(dbtvinfo.last_updated), Now()) >= config.ReadInteger(section, 'days_between_last_update', 6)) then
      begin
        // UPDATE the show because our infos are too old
        if not dbtvinfo.Update then
        begin
          Debug(dpMessage, section, Format('[ERROR] updating of %s with ID %s failed.', [rls, tv_showid]));
        end;
      end;
    finally
      dbtvinfo.Free;
    end;
  end;
end;

procedure saveTVInfos(const TVMazeID: String; tvrage: TTVInfoDB; rls: String = ''; fireKb: boolean = True);
var
  save_tvrage: TTVInfoDB;
begin
    // add the tvinfo
    save_tvrage := TTVInfoDB(tvrage);
    try
      if (rls <> '') then
        irc_Addtext_by_key('ADDTVMAZEECHO', Format('%s %s %s', [addtinfodbcmd, rls, TVMazeID]));
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] saveTVInfos irc_Addtext_by_key: %s', [e.Message]));
        exit;
      end;
    end;

    try
      save_tvrage.Save;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] saveTVInfos Save: %s ', [e.Message]));
      end;
    end;

    if ((rls <> '') and (fireKb)) then
      TVInfoFireKbAdd(rls);
end;

procedure TVInfoFireKbAdd(const aRls: String; msg: String = '<c3>[TVInfo]</c> %s %s now has TV infos (%s)');
var
  p: TPazo;
  ps: TPazoSite;
begin
  p := FindPazoByRls(aRls);
  if (p <> nil) then
  begin
    ps := FindMostCompleteSite(p);
    if ((ps = nil) and (p.PazoSitesList.Count > 0)) then
      ps := TPazoSite(p.PazoSitesList[0]);

    if (ps <> nil) then
    begin
      try
        if spamcfg.ReadBool('addinfo', 'tvinfoupdate', True) then
          irc_SendUPDATE(Format(msg, [p.rls.section, p.rls.rlsname, ps.Name]));

        kb_Add('', '', ps.Name, p.rls.section, '', kbeUPDATE, p.rls.rlsname, '');
      except
        on e: Exception do
        begin
          Debug(dpError, section, '[EXCEPTION] TVInfoFireKbAdd kb_Add: %s', [e.Message]);
        end;
      end;
    end;
  end;
end;

function CreateTVInfoModel: TSQLModel;
begin
  result := TSQLModel.Create([TSQLTVInfo, TSQLTVSeries]);
end;

procedure dbTVInfoStart;
var
  fDBName: String;
begin
  SQLite3Lock := TSlCriticalSection2.Create('tvdb');

  fDBName := Trim(config.ReadString(section, 'database', 'tvinfos.db'));
  
  glTVInfoModel := CreateTVInfoModel;
  try
    glTVInfoDb := CreateORMSQLite3DB(glTVInfoModel, fDBName, '');
    Debug(dpSpam, section, Format('TVInfo db loaded. %d Series, with %d infos', [glTVInfoDb.TableRowCount(TSQLTVSeries), glTVInfoDb.TableRowCount(TSQLTVInfo)]));
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] dbTVInfoStart: %s', [e.Message]));
      exit;
    end;
  end;

  LastAddtvmazeIDs := TList<String>.Create;

  Console_Addline('', Format('TVInfo db loaded. %d Series, with %d infos', [getTVInfoSeriesCount, getTVInfoCount]));
end;

procedure dbTVInfoInit;
begin
  addtinfodbcmd := config.ReadString(section, 'addcmd', '!addtvmaze');
end;

procedure dbTVInfoUninit;
begin
  if Assigned(SQLite3Lock) then
  begin
    FreeAndNil(SQLite3Lock);
  end;

  if Assigned(glTVInfoDb) then
  begin
    FreeAndNil(glTVInfoDb);
  end;

  if Assigned(glTVInfoModel) then
  begin
    FreeAndNil(glTVInfoModel);
  end;

  if Assigned(LastAddtvmazeIDs) then
  begin
    FreeAndNil(LastAddtvmazeIDs);
  end;
end;

function dbTVInfo_Process(const aNet, aChan, aNick: String; aMSG: String): boolean;
begin
  Result := False;
  if (1 = Pos(addtinfodbcmd, aMSG)) then
  begin
    aMSG := Copy(aMSG, length(addtinfodbcmd + ' ') + 1, 1000);
    addTVInfos(aMSG);
    Result := True;
  end;
end;

function TVInfoDbAlive: boolean;
begin
  if glTVInfoDb = nil then
    Result := false
  else
    Result := true;
end;

end.

