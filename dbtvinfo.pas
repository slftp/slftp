unit dbtvinfo;

interface

uses
  Classes, IniFiles, irc, kb.releaseinfo, Contnrs, dbhandler, tvinfo.types,
  mormot.orm.core, mormot.rest.sqlite3;

type
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

var
  GlTVInfoDb: TSQLRestClientDB = nil; //< global mORMot2 ORM database connection for tv info
  GlTVInfoModel: TSQLModel = nil; //< global mORMot2 ORM model for tv info (must remain for complete runtime)

function getTVInfoCount: integer;
function getTVInfoSeriesCount: integer;

function TheTVDbStatus: String;

procedure dbTVInfoInit;

{ Initializes the mORMot2 ORM database for tv info (creates missing tables and
  migrates legacy Zeos tables series/infos on first start)
  @param(aDbName database file name, if empty the value from config section tasktvinfo/database is used) }
procedure dbTVInfoStart(const aDbName: String = '');
procedure dbTVInfoUnInit;

function getTVInfoByShowName(const aRls_Showname: String): TTVInfoDB;
function getTVInfoByReleaseName(const aRLS: String): TTVInfoDB;

function getTVInfoByShowID(const aTVMazeID: String): TTVInfoDB;

procedure saveTVInfos(const TVMazeID: String; tvrage: TTVInfoDB; rls: String = ''; fireKb: boolean = True);

function deleteTVInfoByID(const aID: String): Integer;
function deleteTVInfoByRipName(const aName: String): Integer;

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
  DateUtils, SysUtils, Math, configunit, StrUtils, mystrings, console, sitesunit, queueunit, slmasks, http, RegExpr,
  debugunit, tasktvinfolookup, pazo, mrdohutils, uLkJSON, sllanguagebase,
  Generics.Collections, news, kb, mormot.core.unicode, mormot.core.base, mormot.orm.base,
  mormot.db.raw.sqlite3;

const
  section = 'tasktvinfo';

type
  { NOTE: everything which starts with TVMaze is data from TVMaze }

  { ORM row of the TVInfo table, holds the TVMaze show infos.
    Property names stay snake_case on purpose (deviating from the PascalCase
    used in statsunit.pas): they map 1:1 onto the legacy Zeos infos table
    columns, which keeps the one-time SQL data migration a plain INSERT SELECT }
  TSQLTVInfo = class(TOrm)
  private
    FTvmazeId: Integer; //< TVMaze show id
    FThetvdbId: Integer; //< TheTVDb show id (-1 if unknown)
    FTvrageId: Integer; //< TVRage show id (-1 if unknown)
    FPremieredYear: Integer; //< year the show premiered
    FCountry: RawUTF8; //< country code of the network (US/GB are normalized to USA/UK)
    FStatus: RawUTF8; //< show status (Running, Ended, In Development, ...)
    FClassification: RawUTF8; //< show classification (Scripted, Reality, ...)
    FNetwork: RawUTF8; //< network or web channel name
    FGenre: RawUTF8; //< genres as CommaText
    FEndedYear: Integer; //< year the show ended (-1 if still running/unknown)
    FLastUpdated: Integer; //< unix timestamp of the last update from TVMaze
    FNextDate: Integer; //< unix timestamp of the next episode airdate
    FNextSeason: Integer; //< season number of the next episode
    FNextEpisode: Integer; //< episode number of the next episode
    FRating: Integer; //< rating value (max score is 100, min score is 0)
    FAirdays: RawUTF8; //< airdays as CommaText
    FTvLanguage: RawUTF8; //< language of the show
  published
    property tvmaze_id: Integer read FTvmazeId write FTvmazeId stored AS_UNIQUE;
    property thetvdb_id: Integer read FThetvdbId write FThetvdbId;
    property tvrage_id: Integer read FTvrageId write FTvrageId;
    property premiered_year: Integer read FPremieredYear write FPremieredYear;
    property country: RawUTF8 read FCountry write FCountry;
    property status: RawUTF8 read FStatus write FStatus;
    property classification: RawUTF8 read FClassification write FClassification;
    property network: RawUTF8 read FNetwork write FNetwork;
    property genre: RawUTF8 read FGenre write FGenre;
    property ended_year: Integer read FEndedYear write FEndedYear;
    property last_updated: Integer read FLastUpdated write FLastUpdated;
    property next_date: Integer read FNextDate write FNextDate;
    property next_season: Integer read FNextSeason write FNextSeason;
    property next_episode: Integer read FNextEpisode write FNextEpisode;
    property rating: Integer read FRating write FRating;
    property airdays: RawUTF8 read FAirdays write FAirdays;
    property tv_language: RawUTF8 read FTvLanguage write FTvLanguage;
  end;

  { ORM row of the TVSeries table, maps a ripped showname to a TVMaze show.
    The legacy rip_country column is dropped on purpose: it was never read
    by any code path (legacy leftover) }
  TSQLTVSeries = class(TOrm)
  private
    FRip: RawUTF8; //< ripped showname as extracted from the releasename
    FShowname: RawUTF8; //< plain TVMaze showname
    FTvmazeUrl: RawUTF8; //< TVMaze url of the show
    FTvmazeId: Integer; //< TVMaze show id, references TSQLTVInfo.tvmaze_id
  published
    property rip: RawUTF8 read FRip write FRip stored AS_UNIQUE;
    property showname: RawUTF8 read FShowname write FShowname;
    property tvmaze_url: RawUTF8 read FTvmazeUrl write FTvmazeUrl;
    property tvmaze_id: Integer read FTvmazeId write FTvmazeId;
  end;

var
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

{ Fills an ORM tv info row from the TTVInfoDB fields, last_updated is always set to now }
procedure _FillOrmFromTVInfoDB(const aTvi: TTVInfoDB; const aInfo: TSQLTVInfo);
begin
  aInfo.tvmaze_id := StrToIntDef(aTvi.tvmaze_id, -1);
  aInfo.thetvdb_id := StrToIntDef(aTvi.thetvdb_id, -1);
  aInfo.tvrage_id := StrToIntDef(aTvi.tvrage_id, -1);
  aInfo.premiered_year := aTvi.tv_premiered_year;
  aInfo.country := StringToUTF8(aTvi.tv_country);
  aInfo.status := StringToUTF8(aTvi.tv_status);
  aInfo.classification := StringToUTF8(aTvi.tv_classification);
  aInfo.network := StringToUTF8(aTvi.tv_network);
  aInfo.genre := StringToUTF8(aTvi.tv_genres.CommaText);
  aInfo.ended_year := aTvi.tv_endedyear;
  aInfo.last_updated := DateTimeToUnix(now());
  aInfo.next_date := aTvi.tv_next_date;
  aInfo.next_season := aTvi.tv_next_season;
  aInfo.next_episode := aTvi.tv_next_ep;
  aInfo.rating := aTvi.tv_rating;
  aInfo.airdays := StringToUTF8(aTvi.tv_days.CommaText);
  aInfo.tv_language := StringToUTF8(aTvi.tv_language);
end;

{ Fills the TTVInfoDB fields from an ORM tv info row and computes tv_running/tv_scripted.
  A nil aInfo replicates the old LEFT JOIN behavior (no infos row -> default values).
  @param(aInfo ORM tv info row or nil if there is none)
  @param(aTvi DTO which gets filled)
  @param(aNextDefault default for tv_next_date/season/ep without infos row, keeps the historic getTVInfoByShowName(-1)/getTVInfoByShowID(0) difference) }
procedure _FillTVInfoDBFromOrm(const aInfo: TSQLTVInfo; const aTvi: TTVInfoDB; const aNextDefault: Integer);
begin
  if aInfo <> nil then
  begin
    aTvi.thetvdb_id := IntToStr(aInfo.thetvdb_id);
    aTvi.tvrage_id := IntToStr(aInfo.tvrage_id);
    aTvi.tv_premiered_year := aInfo.premiered_year;
    aTvi.tv_country := UTF8ToString(aInfo.country);
    aTvi.tv_status := UTF8ToString(aInfo.status);
    aTvi.tv_classification := UTF8ToString(aInfo.classification);
    aTvi.tv_network := UTF8ToString(aInfo.network);
    aTvi.tv_genres.CommaText := UTF8ToString(aInfo.genre);
    aTvi.tv_endedyear := aInfo.ended_year;
    aTvi.last_updated := aInfo.last_updated;
    aTvi.tv_next_date := aInfo.next_date;
    aTvi.tv_next_season := aInfo.next_season;
    aTvi.tv_next_ep := aInfo.next_episode;
    aTvi.tv_days.CommaText := UTF8ToString(aInfo.airdays);
    aTvi.tv_rating := aInfo.rating;
    aTvi.tv_language := UTF8ToString(aInfo.tv_language);
  end
  else
  begin
    // legacy LEFT JOIN case: series row without matching infos row
    aTvi.thetvdb_id := '';
    aTvi.tvrage_id := '';
    aTvi.tv_premiered_year := -1;
    aTvi.tv_country := '';
    aTvi.tv_status := '';
    aTvi.tv_classification := '';
    aTvi.tv_network := '';
    aTvi.tv_genres.Clear;
    aTvi.tv_endedyear := -1;
    aTvi.last_updated := -1;
    aTvi.tv_next_date := aNextDefault;
    aTvi.tv_next_season := aNextDefault;
    aTvi.tv_next_ep := aNextDefault;
    aTvi.tv_days.Clear;
    aTvi.tv_rating := 0;
    aTvi.tv_language := '';
  end;

  aTvi.tv_running := Boolean( (SysUtils.LowerCase(aTvi.tv_status) = 'running') or (SysUtils.LowerCase(aTvi.tv_status) = 'in development') );
  aTvi.tv_scripted := Boolean(SysUtils.LowerCase(aTvi.tv_classification) = 'scripted');
end;

{ Drops the legacy Zeos indexes tvinfo (on infos) and Rips (on series) which the
  old code created on every start. SQLite shares one namespace for table and
  index names, so the index 'tvinfo' would otherwise block CREATE TABLE TVInfo
  in CreateMissingTables. Must run before the ORM database is opened.
  Any error is only logged, never raised }
procedure _DropLegacyTvinfoIndexes(const aDbName: String);
var
  fDbPath: String;
  fDb: TSQLDataBase;
begin
  fDbPath := GetDatabaseFilePath(aDbName);
  if not FileExists(fDbPath) then
    exit;

  try
    fDb := TSQLDataBase.Create(StringToUTF8(fDbPath));
    try
      fDb.Execute('DROP INDEX IF EXISTS tvinfo');
      fDb.Execute('DROP INDEX IF EXISTS Rips');
    finally
      fDb.Free;
    end;
  except
    on e: Exception do
      Debug(dpError, section, Format('[EXCEPTION] _DropLegacyTvinfoIndexes: %s', [e.Message]));
  end;
end;

{ Migrates the legacy Zeos tables series/infos into the ORM tables and drops
  them afterwards. On any error the legacy tables are kept and the error is
  only logged, so the next start retries the migration }
procedure _MigrateLegacyTvinfoTables;
var
  fTables: TRawUTF8DynArray;
  fTableName: RawUTF8;
  fHasInfos, fHasSeries: boolean;
begin
  fHasInfos := False;
  fHasSeries := False;

  GlTVInfoDb.DB.GetTableNames(fTables);
  for fTableName in fTables do
  begin
    if SameText(fTableName, 'infos') then
      fHasInfos := True;
    if SameText(fTableName, 'series') then
      fHasSeries := True;
  end;

  if not (fHasInfos or fHasSeries) then
    exit;

  try
    if fHasInfos then
    begin
      // property names of TSQLTVInfo map 1:1 onto the legacy columns, only tvdb_id was renamed to thetvdb_id
      GlTVInfoDb.DB.Execute('INSERT OR IGNORE INTO TVInfo ' +
        '(tvmaze_id, thetvdb_id, tvrage_id, premiered_year, country, status, classification, network, genre, ' +
        'ended_year, last_updated, next_date, next_season, next_episode, rating, airdays, tv_language) ' +
        'SELECT tvmaze_id, tvdb_id, tvrage_id, premiered_year, country, status, classification, network, genre, ' +
        'ended_year, last_updated, next_date, next_season, next_episode, rating, airdays, tv_language FROM infos');
      GlTVInfoDb.DB.Execute('DROP TABLE infos');
    end;

    if fHasSeries then
    begin
      // legacy series.id holds the tvmaze id, rip_country is dropped (never read by any code)
      GlTVInfoDb.DB.Execute('INSERT OR IGNORE INTO TVSeries (rip, showname, tvmaze_url, tvmaze_id) ' +
        'SELECT rip, showname, tvmaze_url, id FROM series');
      GlTVInfoDb.DB.Execute('DROP TABLE series');
    end;

    Debug(dpMessage, section, 'Migrated legacy tvinfo Zeos tables into mORMot2 ORM tables');
  except
    on e: Exception do
      Debug(dpError, section, Format('[EXCEPTION] _MigrateLegacyTvinfoTables: %s (legacy tables kept, retry on next start)', [e.Message]));
  end;
end;

procedure TTVInfoDB.setTheTVDbID(const aID: integer);
var
  fInfo: TSQLTVInfo;
begin
  if GlTVInfoDb = nil then
    exit;

  try
    fInfo := TSQLTVInfo.CreateAndFillPrepare(GlTVInfoDb.Orm, 'tvmaze_id = ?', [], [StrToIntDef(tvmaze_id, -1)]);
    try
      if fInfo.FillOne then
      begin
        fInfo.thetvdb_id := aID;
        GlTVInfoDb.Update(fInfo);
      end;
    finally
      fInfo.Free;
    end;
  except
    on e: Exception do
      Debug(dpError, section, Format('[EXCEPTION] setTheTVDbID: %s, ID: %d, TVMAZE-ID: %s', [e.Message, aID, tvmaze_id]));
  end;
end;

procedure TTVInfoDB.setTVRageID(const aID: integer);
var
  fInfo: TSQLTVInfo;
begin
  if GlTVInfoDb = nil then
    exit;

  try
    fInfo := TSQLTVInfo.CreateAndFillPrepare(GlTVInfoDb.Orm, 'tvmaze_id = ?', [], [StrToIntDef(tvmaze_id, -1)]);
    try
      if fInfo.FillOne then
      begin
        fInfo.tvrage_id := aID;
        GlTVInfoDb.Update(fInfo);
      end;
    finally
      fInfo.Free;
    end;
  except
    on e: Exception do
      Debug(dpError, section, Format('[EXCEPTION] setTVRageID: %s, ID: %d, TVMAZE-ID: %s', [e.Message, aID, tvmaze_id]));
  end;
end;

procedure TTVInfoDB.Save;
var
  fInfo: TSQLTVInfo;
  fSeries: TSQLTVSeries;
begin
  if GlTVInfoDb = nil then
    exit;

  try
    // INSERT OR IGNORE INTO infos
    fInfo := TSQLTVInfo.CreateAndFillPrepare(GlTVInfoDb.Orm, 'tvmaze_id = ?', [], [StrToIntDef(tvmaze_id, -1)]);
    try
      if fInfo.FillOne then
        // row existed already, mimics the ignored insert of the old code
        last_updated := 3817
      else
      begin
        fInfo.Free;
        fInfo := TSQLTVInfo.Create;
        _FillOrmFromTVInfoDB(self, fInfo);
        GlTVInfoDb.Add(fInfo, True);
        last_updated := DateTimeToUnix(now());
      end;
    finally
      fInfo.Free;
    end;

    // INSERT OR IGNORE INTO series
    fSeries := TSQLTVSeries.CreateAndFillPrepare(GlTVInfoDb.Orm, 'rip = ?', [], [StringToUTF8(rls_showname)]);
    try
      if not fSeries.FillOne then
      begin
        fSeries.Free;
        fSeries := TSQLTVSeries.Create;
        fSeries.rip := StringToUTF8(rls_showname);
        fSeries.showname := StringToUTF8(tv_showname);
        fSeries.tvmaze_url := StringToUTF8(tv_url);
        fSeries.tvmaze_id := StrToIntDef(tvmaze_id, -1);
        GlTVInfoDb.Add(fSeries, True);
      end;
    finally
      fSeries.Free;
    end;
  except
    on e: Exception do
      Debug(dpError, section, Format('[EXCEPTION] TTVInfoDB.Save: %s', [e.Message]));
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

  if GlTVInfoDb = nil then
    exit;

  try
    fInfo := TSQLTVInfo.CreateAndFillPrepare(GlTVInfoDb.Orm, 'tvmaze_id = ?', [], [StrToIntDef(tvmaze_id, -1)]);
    try
      if fInfo.FillOne then
      begin
        _FillOrmFromTVInfoDB(self, fInfo);
        Result := GlTVInfoDb.Update(fInfo);
      end;
    finally
      fInfo.Free;
    end;
  except
    on e: Exception do
      Debug(dpError, section, Format('[EXCEPTION] TTVInfoDB.executeUpdate: %s', [e.Message]));
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
  if GlTVInfoDb = nil then
    Result := 0
  else
    Result := GlTVInfoDb.TableRowCount(TSQLTVInfo);
end;

function getTVInfoSeriesCount: integer;
begin
  if GlTVInfoDb = nil then
    Result := 0
  else
    Result := GlTVInfoDb.TableRowCount(TSQLTVSeries);
end;

function TheTVDbStatus: String;
begin
  Result := Format('<b>TVInfo.db</b>: %d Series, with %d infos', [getTVInfoSeriesCount, getTVInfoCount]);
end;

function deleteTVInfoByID(const aID: String): Integer;
var
  fInfo: TSQLTVInfo;
  fSeries: TSQLTVSeries;
begin
  Result := 1;

  if GlTVInfoDb = nil then
    exit;

  try
    fInfo := TSQLTVInfo.CreateAndFillPrepare(GlTVInfoDb.Orm, 'tvmaze_id = ?', [], [StrToIntDef(aID, -1)]);
    try
      if fInfo.FillOne then
        GlTVInfoDb.Delete(TSQLTVInfo, fInfo.ID)
      else
      begin
        Result := 10;
        exit;
      end;
    finally
      fInfo.Free;
    end;

    fSeries := TSQLTVSeries.CreateAndFillPrepare(GlTVInfoDb.Orm, 'tvmaze_id = ?', [], [StrToIntDef(aID, -1)]);
    try
      if fSeries.FillOne then
        GlTVInfoDb.Delete(TSQLTVSeries, fSeries.ID)
      else
      begin
        Result := 11;
        exit;
      end;
    finally
      fSeries.Free;
    end;
  except
    on e: Exception do
      Debug(dpError, section, Format('[EXCEPTION] deleteTVInfoByID: %s', [e.Message]));
  end;
end;

function deleteTVInfoByRipName(const aName: String): Integer;
var
  fSeries: TSQLTVSeries;
  fCount: Int64;
begin
  Result := 1;

  if GlTVInfoDb = nil then
    exit;

  try
    fSeries := TSQLTVSeries.CreateAndFillPrepare(GlTVInfoDb.Orm, 'rip = ?', [], [StringToUTF8(aName)]);
    try
      if not fSeries.FillOne then
      begin
        Result := 0;
        exit;
      end;

      if not GlTVInfoDb.OneFieldValue(TSQLTVSeries, 'COUNT(*)', 'rip = ?', [], [StringToUTF8(aName)], fCount) then
        fCount := 0;
      if fCount > 1 then
        // multiple aka's for the same rip, delete infos and series via the tvmaze id
        Result := deleteTVInfoByID(IntToStr(fSeries.tvmaze_id))
      else if GlTVInfoDb.Delete(TSQLTVSeries, fSeries.ID) then
        Result := 1
      else
        Result := 12;
    finally
      fSeries.Free;
    end;
  except
    on e: Exception do
      Debug(dpError, section, Format('[EXCEPTION] deleteTVInfoByRipName: %s', [e.Message]));
  end;
end;

function getTVInfoByShowName(const aRls_Showname: String): TTVInfoDB;
var
  fTvi: TTVInfoDB;
  fSeries: TSQLTVSeries;
  fInfo: TSQLTVInfo;
begin
  Result := nil;

  if (aRls_Showname = '') then
  begin
    Debug(dpError, section, '[EXCEPTION] getTVInfoByShowName: rls_showname is empty');
    exit;
  end;

  if GlTVInfoDb = nil then
    exit;

  fTvi := nil;
  // LIKE + lowercase compare keeps the old case-insensitive rip matching behavior (incl. aka's)
  fSeries := TSQLTVSeries.CreateAndFillPrepare(GlTVInfoDb.Orm, 'rip LIKE ?', [], [StringToUTF8(aRls_Showname)]);
  try
    try
      if not fSeries.FillOne then
        exit;

      if (SysUtils.LowerCase(aRls_Showname) <> SysUtils.LowerCase(UTF8ToString(fSeries.rip))) then
      begin
        Debug(dpError, section, 'getTVInfoByShowName LowerCase(%s) <> LowerCase(%s)', [aRls_Showname, UTF8ToString(fSeries.rip)]);
        exit;
      end;

      fTvi := TTVInfoDB.Create(aRls_Showname);
      fTvi.tv_showname := UTF8ToString(fSeries.showname);
      fTvi.tv_url := UTF8ToString(fSeries.tvmaze_url);
      fTvi.tvmaze_id := IntToStr(fSeries.tvmaze_id);

      fInfo := TSQLTVInfo.CreateAndFillPrepare(GlTVInfoDb.Orm, 'tvmaze_id = ?', [], [fSeries.tvmaze_id]);
      try
        if not fInfo.FillOne then
          FreeAndNil(fInfo);
        _FillTVInfoDBFromOrm(fInfo, fTvi, -1);
      finally
        fInfo.Free;
      end;

      Result := fTvi;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] getTVInfoByShowName: %s', [e.Message]));
        FreeAndNil(fTvi);
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
  fTvi: TTVInfoDB;
  fSeries: TSQLTVSeries;
  fInfo: TSQLTVInfo;
begin
  Result := nil;

  if (aTVMazeID = '') then
  begin
    Debug(dpError, section, '[EXCEPTION] getTVInfoByShowID: TVMaze ID is empty');
    exit;
  end;

  if GlTVInfoDb = nil then
    exit;

  fTvi := nil;
  // legacy series.id holds the tvmaze id
  fSeries := TSQLTVSeries.CreateAndFillPrepare(GlTVInfoDb.Orm, 'tvmaze_id = ?', [], [StrToIntDef(aTVMazeID, -1)]);
  try
    try
      if not fSeries.FillOne then
        exit;

      fTvi := TTVInfoDB.Create(UTF8ToString(fSeries.rip));
      fTvi.tv_showname := UTF8ToString(fSeries.showname);
      fTvi.tv_url := UTF8ToString(fSeries.tvmaze_url);
      fTvi.tvmaze_id := IntToStr(fSeries.tvmaze_id);

      fInfo := TSQLTVInfo.CreateAndFillPrepare(GlTVInfoDb.Orm, 'tvmaze_id = ?', [], [fSeries.tvmaze_id]);
      try
        if not fInfo.FillOne then
          FreeAndNil(fInfo);
        // 0 as next default, keeps the historic difference to getTVInfoByShowName (-1)
        _FillTVInfoDBFromOrm(fInfo, fTvi, 0);
      finally
        fInfo.Free;
      end;

      Result := fTvi;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] getTVInfoByShowID: %s', [e.Message]));
        FreeAndNil(fTvi);
      end;
    end;
  finally
    fSeries.Free;
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

procedure dbTVInfoStart(const aDbName: String = '');
var
  fDBName: String;
begin
  if GlTVInfoDb <> nil then
    dbTVInfoUninit;

  fDBName := Trim(aDbName);
  if fDBName = '' then
    fDBName := Trim(config.ReadString(section, 'database', 'tvinfos.db'));

  // legacy Zeos index 'tvinfo' blocks CREATE TABLE TVInfo (shared name namespace),
  // so it must be dropped before CreateORMSQLite3DB runs CreateMissingTables
  _DropLegacyTvinfoIndexes(fDBName);

  GlTVInfoModel := TSQLModel.Create([TSQLTVInfo, TSQLTVSeries]);
  try
    GlTVInfoDb := CreateORMSQLite3DB(GlTVInfoModel, fDBName, '');
    if GlTVInfoDb = nil then
      Debug(dpError, section, Format('dbTVInfoStart: could not initialize ORM database %s', [fDBName]))
    else
      _MigrateLegacyTvinfoTables;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] dbTVInfoStart ORM init failed: %s', [e.Message]));
      FreeAndNil(GlTVInfoDb);
      FreeAndNil(GlTVInfoModel);
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
  if Assigned(LastAddtvmazeIDs) then
  begin
    FreeAndNil(LastAddtvmazeIDs);
  end;

  if Assigned(GlTVInfoDb) then
  begin
    FreeAndNil(GlTVInfoDb);
  end;

  if Assigned(GlTVInfoModel) then
  begin
    FreeAndNil(GlTVInfoModel);
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
  Result := GlTVInfoDb <> nil;
end;

end.

