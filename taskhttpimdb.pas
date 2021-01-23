unit taskhttpimdb;

interface

uses
  tasksunit, Generics.Collections;

type
  { @abstract(Class for IMDb release date information) }
  TIMDbReleaseDateInfo = class
  private
    FCountry: String; //< country name
    FReleaseDate: String; //< release date
    FExtraInfo: String; //< additional info like dvd premiere, festival, location of premiere, etc
  public
    { Creates a class for specific release date infos }
    constructor Create(const aCountry, aReleaseDate, aExtraInfo: String);

    property Country: String read FCountry;
    property ReleaseDate: String read FReleaseDate;
    property ExtraInfo: String read FExtraInfo;
  end;

  { @abstract(Class for IMDb also known as (AKA) information) }
  TIMDbAlsoKnownAsInfo = class
  private
    FCountry: String; //< country name (+ extra title information)
    FTitle: String; //< title name
  public
    { Creates a class for specific AKA infos }
    constructor Create(const aCountry, aTitle: String);

    property Country: String read FCountry;
    property Title: String read FTitle;
  end;

  { @abstract(Extracts IMDb information from HTML page source) }
  THtmlIMDbParser = class
  public
    { Parses title information from the meta property @italic(title) tag
      @param(aPageSource Webpage HTML sourcecode)
      @param(aMovieTitle Title of the movie (can be empty))
      @param(aTitleExtraInfo Additional info (e.g. TV Series) from the title (can be empty))
      @param(aYear Year of the movie (0 if not available)) }
    class procedure ParseMetaTitleInformation(const aPageSource: String; out aMovieTitle, aTitleExtraInfo: String; out aYear: Integer);

    { Parses votes and rating and removes dots and commas @br @note(default value for both is 0)
      @param(aPageSource Webpage HTML sourcecode)
      @param(aVotes Votes of the movie)
      @param(aRating Rating of the movie) }
    class procedure ParseVotesAndRating(const aPageSource: String; out aVotes, aRating: String);

    { Parses language(s)
      @param(aPageSource Webpage HTML sourcecode)
      @param(aLanguageList Language(s) of the movie as comma separated list) }
    class procedure ParseMovieLanguage(const aPageSource: String; out aLanguageList: String);

    { Parses Countrie(s)
      @param(aPageSource Webpage HTML sourcecode)
      @param(aCountriesList Countrie(s) of the movie as comma separated list) }
    class procedure ParseMovieCountries(const aPageSource: String; out aCountriesList: String);

    { Parses Genre(s)
      @param(aPageSource Webpage HTML sourcecode)
      @param(aGenresList Genre(s) of the movie as comma separated list) }
    class procedure ParseMovieGenres(const aPageSource: String; out aGenresList: String);

    { Parses Releasedates(s) for countries included in slftp.imdbcountries
      @param(aPageSource Releasedate Webpage HTML sourcecode)
      @param(aReleaseDateInfoList List of releasedate information) }
    class procedure ParseReleaseDateInfo(const aPageSource: String; var aReleaseDateInfoList: TObjectList<TIMDbReleaseDateInfo>);

    { Parses 'Also Known As' (AKA) information for countries included in slftp.imdbcountries plus original title
      @param(aPageSource Releasedate Webpage HTML sourcecode)
      @param(aAlsoKnownAsList List of AKA information) }
    class procedure ParseAlsoKnownAsInfo(const aPageSource: String; var aAlsoKnownAsList: TObjectList<TIMDbAlsoKnownAsInfo>);
  end;

  { @abstract(Extracts Box Office information from boxofficemojo.com HTML page source) }
  THtmlBoxOfficeMojoParser = class
  public
    { Parses countries (only includes one from slftp.imdbcountries) and links from the title overview page @br @note(Domestic gets renamed to USA, United Kingdom gets renamed to UK)
      @param(aPageSource Webpage HTML sourcecode)
      @param(aCountryLinks Countryname and link to the release information) }
    class procedure GetCountrySpecificLinks(const aPageSource: String; out aCountryLinks: TDictionary<String, String>);

    { Parses theaters screens of widest release
      @param(aPageSource Webpage HTML sourcecode)
      @returns(Screens count of widest release (0 if nothing found)) }
    class function GetWidestScreensCount(const aPageSource: String): Integer;
  end;

  { @abstract(Functions to extract information from already parsed data) }
  TIMDbInfoChecks = class
  public
    { Checks if the title extra info indicate that the movie is definitely a STV movie
      @param(aTitleExtraInfo Additional title information extracted via @link(ParseMetaTitleInformation))
      @returns(aIsSTV @True if extra info indicates that it's STV, @false otherwise) }
    class function IsSTVBasedOnTitleExtraInfo(const aTitleExtraInfo: String): Boolean;

    { Checks which of the english language countries (USA or UK) is listed first in Country list on IMDb page
      @param(aImdbCountries List of Countries from IMDb main page)
      @returns(Abbreviation of the country names; in case none is listed it defaults to USA) }
    class function EstimateEnglishCountryOrder(const aImdbCountries: String): String;
  end;

  TPazoHTTPImdbTask = class(TTask)
  private
    FReleaseName: String; //< releasename
    FImdbTitleID: String; //< imdb title id, tt<numbers>
  public
    constructor Create(const aImdbTitleID: String; const aReleaseName: String);
    destructor Destroy; override;
    function Execute(slot: Pointer): Boolean; override;
    function Name: String; override;
  end;

implementation

uses
  SysUtils, irc, StrUtils, debugunit, dateutils, configunit, kb, kb.releaseinfo, http,
  sitesunit, RegExpr, dbaddimdb, mystrings, dbtvinfo;

const
  section = 'taskhttpimdb';

{ TIMDbReleaseDateInfo }

constructor TIMDbReleaseDateInfo.Create(const aCountry, aReleaseDate, aExtraInfo: String);
begin
  FCountry := aCountry;
  FReleaseDate := aReleaseDate;
  FExtraInfo := aExtraInfo;
end;

{ TIMDbAlsoKnownAsInfo }

constructor TIMDbAlsoKnownAsInfo.Create(const aCountry, aTitle: String);
begin
  FCountry := aCountry;
  FTitle := aTitle;
end;

{ THtmlIMDbParser }

class procedure THtmlIMDbParser.ParseMetaTitleInformation(const aPageSource: String; out aMovieTitle, aTitleExtraInfo: String; out aYear: Integer);
var
  rr: TRegExpr;
begin
  rr := TRegExpr.Create;
  try
    rr.ModifierI := True;
    rr.Expression := '<meta property=\''og:title\'' content="(.*?)\s*\((.*?)?\s*(\d{4}).*?\"';
    if rr.Exec(aPageSource) then
    begin
      aMovieTitle := rr.Match[1];
      aTitleExtraInfo := rr.Match[2];
      aYear := StrToIntDef(rr.Match[3], 0);
    end;
  finally
    rr.Free;
  end;
end;

class procedure THtmlIMDbParser.ParseVotesAndRating(const aPageSource: String; out aVotes, aRating: String);
type
  TRegexItem = record
    RegexString: String; // regex
    MatchIndex: Integer; // index of match to be used
  end;

(*
  Initialize valid regex for imdb rating/votes
  This allows us to reduce alot of code duplication and nested if/else
  constructs.
  Newest versions should be added on top as they will be checked against the
  website in ascending order. Quicker match = less overhead.
*)

const
  VotesRegexList: array [0..3] of TRegexItem = (
    (RegexString: '<strong.*?on (\S+) user ratings\"><span.*?>\d+[,.]\d<\/span>'; MatchIndex: 1),
    (RegexString: '<span[^<>]*itemprop="ratingCount">(\S+)<\/span>'; MatchIndex: 1),
    (RegexString: '\>(\S+) votes<\/a>\)'; MatchIndex: 1),
    (RegexString: '<a href=\"ratings\" class=\"tn15more\">(.*?) (Bewertungen|votes|Stimmen)<\/a>'; MatchIndex: 1)
  );

  RatingRegexList: array [0..3] of TRegexItem = (
    (RegexString: '<strong.*?user ratings\"><span.*?>(\d+[,.]\d)<\/span>'; MatchIndex: 1),
    (RegexString: '<span[^<>]*itemprop="ratingValue">(\d+[,.]\d+)<\/span>'; MatchIndex: 1),
    (RegexString: '<span class="rating-rating">(\d+[,.]\d+)<span>\/10<\/span><\/span>'; MatchIndex: 1),
    (RegexString: '<b>(\d+[,.]\d+)\/10<\/b>'; MatchIndex: 1)
  );

var
  rr: TRegExpr;
  fRegexItem: TRegexItem;
begin
  aVotes := '0';
  aRating := '0';

  rr := TRegExpr.Create;
  try
    rr.ModifierI := True;

    for fRegexItem in VotesRegexList do
    begin
      rr.Expression := fRegexItem.RegexString;
      if rr.Exec(aPageSource) then
      begin
        aVotes := rr.Match[fRegexItem.MatchIndex];
        break;
      end;
    end;

    for fRegexItem in RatingRegexList do
    begin
      rr.Expression := fRegexItem.RegexString;
      if rr.Exec(aPageSource) then
      begin
        aRating := rr.Match[fRegexItem.MatchIndex];
        break;
      end;
    end;
  finally
    rr.Free;
  end;

  aVotes := StringReplace(aVotes, '.', '', [rfReplaceAll, rfIgnoreCase]);
  aVotes := StringReplace(aVotes, ',', '', [rfReplaceAll, rfIgnoreCase]);

  aRating := StringReplace(aRating, '.', '', [rfReplaceAll, rfIgnoreCase]);
  aRating := StringReplace(aRating, ',', '', [rfReplaceAll, rfIgnoreCase]);
end;

class procedure THtmlIMDbParser.ParseMovieLanguage(const aPageSource: String; out aLanguageList: String);
var
  rr, rr2: TRegExpr;
begin
  rr := TRegExpr.Create;
  try
    rr.ModifierI := True;

    rr2 := TRegExpr.Create;
    try
      rr2.ModifierI := True;
      rr2.Expression := '<a[^>]+href=[^>]+>([^<]+)<\/a>';

      // Trying new layout of IMDb first
      rr.Expression := '<div class="txt-block">[^<]*<h4 class="inline">Language:<\/h4>[^<]*(<.*?<\/a>)[^<]*<\/div>';
      if rr.Exec(aPageSource) then
      begin
        if rr2.Exec(rr.Match[1]) then
        begin
          repeat
            aLanguageList := aLanguageList + rr2.Match[1] + ',';
          until not rr2.ExecNext;
        end;
      end
      else
      begin
        // Trying old layout of IMDb if new layout fails
        rr.Expression := '<div class=\"info\"><h5>Language:<\/h5><div class=\"info-content\">(<.*?<\/a>)<\/div><\/div>';
        if rr.Exec(aPageSource) then
        begin
          if rr2.Exec(rr.Match[1]) then
            repeat
              aLanguageList := aLanguageList + rr2.Match[3] + ',';
            until not rr2.ExecNext;
        end;
      end;
    finally
      rr2.Free;
    end;
  finally
    rr.Free;
  end;

  // remove additional comma
  SetLength(aLanguageList, Length(aLanguageList) - 1);
end;

class procedure THtmlIMDbParser.ParseMovieCountries(const aPageSource: String; out aCountriesList: String);
var
  rr, rr2: TRegExpr;
begin
  rr := TRegExpr.Create;
  try
    rr.ModifierI := True;

    rr2 := TRegExpr.Create;
    try
      rr2.ModifierI := True;
      rr2.Expression := '<a[^>]+href=[^>]+>([^<]+)<\/a>';

      // Trying new layout of IMDb first
      rr.Expression := '<div class="txt-block">[^<]*<h4 class="inline">Country:<\/h4>[^<]*(<.*?<\/a>)[^<]*<\/div>';
      if rr.Exec(aPageSource) then
      begin
        if rr2.Exec(rr.Match[1]) then
        begin
          repeat
            aCountriesList := aCountriesList + rr2.Match[1] + ',';
          until not rr2.ExecNext;
        end;
      end
      else
      begin
        // Trying old layout of IMDb if new layout fails
        rr.Expression := '<div class=\"info\"><h5>Country:<\/h5><div class=\"info-content\">(<.*?<\/a>)<\/div><\/div>';
        if rr.Exec(aPageSource) then
        begin
          if rr2.Exec(rr.Match[1]) then
            repeat
              aCountriesList := aCountriesList + rr2.Match[4] + ',';
            until not rr2.ExecNext;
        end;
      end;
    finally
      rr2.Free;
    end;
  finally
    rr.Free;
  end;

  // remove additional comma
  SetLength(aCountriesList, Length(aCountriesList) - 1);
end;

class procedure THtmlIMDbParser.ParseMovieGenres(const aPageSource: String; out aGenresList: String);
var
  rr, rr2: TRegExpr;
begin
  rr := TRegExpr.Create;
  try
    rr.ModifierI := True;

    rr2 := TRegExpr.Create;
    try
      rr2.ModifierI := True;
      rr2.Expression := '<a[^>]+>\s(\S+)<\/a>';

      // Trying new layout of IMDb first
      rr.Expression := '<h4 class="inline">Genres:<\/h4>((\s*<a href\S+\s*>.*?<\/a>(?:\S+<\/span>)?\s*)+)<\/div>';
      if rr.Exec(aPageSource) then
      begin
        if rr2.Exec(rr.Match[1]) then
        begin
          repeat
            aGenresList := aGenresList + rr2.Match[1] + ',';
          until not rr2.ExecNext;
        end;
      end
      else
      begin
        // Trying old layout of IMDb if new layout fails
        rr.Expression := '<h5>Genre:<\/h5>\n<div class=\"info-content\">\s+(.*?)<\/div>';
        if rr.Exec(aPageSource) then
        begin
          if rr2.Exec(rr.Match[1]) then
            repeat
              aGenresList := aGenresList + rr2.Match[3] + ',';
            until not rr2.ExecNext;
        end;
      end;
    finally
      rr2.Free;
    end;
  finally
    rr.Free;
  end;

  // remove additional comma
  SetLength(aGenresList, Length(aGenresList) - 1);
end;

class procedure THtmlIMDbParser.ParseReleaseDateInfo(const aPageSource: String; var aReleaseDateInfoList: TObjectList<TIMDbReleaseDateInfo>);
var
  rr: TRegExpr;
  fCountryCode: String;
  fCountry: String;
  fReleaseDate: String;
  fExtraInfo: String;
begin
  rr := TRegExpr.Create;
  try
    rr.ModifierI := True;
    rr.Expression := '<td class="release-date.*?><a href="\/calendar\/\?region\=(.*?)\&.*?>(.*?)<\/a><\/td>[\s\n]*?' +
        '<td class="release-date.*?>(.*?)<\/td>[\s\n]*?<td class="release-date.*?>(.*?)<\/td>';

    if rr.Exec(aPageSource) then
    begin
      repeat
        fCountryCode := Trim(rr.Match[1]);
        fCountry := Trim(rr.Match[2]);
        fReleaseDate := Trim(rr.Match[3]);
        fExtraInfo := Trim(rr.Match[4]);

        if ExcludeCountry(fCountry) then
          Continue;

        aReleaseDateInfoList.Add(TIMDbReleaseDateInfo.Create(fCountry, fReleaseDate, fExtraInfo));
      until not rr.ExecNext;
    end;
  finally
    rr.Free;
  end;
end;

class procedure THtmlIMDbParser.ParseAlsoKnownAsInfo(const aPageSource: String; var aAlsoKnownAsList: TObjectList<TIMDbAlsoKnownAsInfo>);
var
  rr: TRegExpr;
  fCountry: String;
  fTitle: String;
begin
  rr := TRegExpr.Create;
  try
    rr.ModifierI := True;
    rr.Expression := '<tr class=.*?\saka-item">[\s\n]*?.*?"aka-item__name">(.*?)<\/td>'
        + '[\s\n]*?<td class="aka-item__title">(.*?)<\/td>[\s\n]*?<\/tr>';

    if rr.Exec(aPageSource) then
    begin
      repeat
        fCountry := Trim(rr.Match[1]);
        fTitle := Trim(rr.Match[2]);

        fTitle := fTitle.Replace(':', '', [rfReplaceAll, rfIgnoreCase]);
        // TODO: also replace special chars of a language like ø, ä, é?

        if not LowerCase(fCountry).Contains('original title') and ExcludeCountry(fCountry) then
          Continue;

        aAlsoKnownAsList.Add(TIMDbAlsoKnownAsInfo.Create(fCountry, fTitle));
      until not rr.ExecNext;
    end;
  finally
    rr.Free;
  end;
end;

{ THtmlBoxOfficeMojoParser }

class procedure THtmlBoxOfficeMojoParser.GetCountrySpecificLinks(const aPageSource: String; out aCountryLinks: TDictionary<String, String>);
var
  rr: TRegExpr;
  fLink: String;
  fCountry: String;
begin
  rr := TRegExpr.Create;
  try
    rr.ModifierI := True;
    rr.Expression := '<a class="a-link-normal" href="(\/release\/rl\d+).*?">(.*?)<\/a>';

    if rr.Exec(aPageSource) then
    begin
      repeat
        fLink := Trim(rr.Match[1]);
        fCountry := Trim(rr.Match[2]);

        if fCountry = 'Domestic' then
          fCountry := 'USA';
        if fCountry = 'United Kingdom' then
          fCountry := 'UK';

        if ExcludeCountry(fCountry) then
          Continue;

        aCountryLinks.Add(fCountry, fLink);
      until not rr.ExecNext;
    end;
  finally
    rr.Free;
  end;
end;

class function THtmlBoxOfficeMojoParser.GetWidestScreensCount(const aPageSource: String): Integer;
var
  rr: TRegExpr;
  fScreensCount: String;
begin
  rr := TRegExpr.Create;
  try
    rr.ModifierI := True;
    rr.Expression := '<div[^>]*><span>Widest Release<\/span><span>([0-9,]*) theaters<\/span><\/div>';

    if rr.Exec(aPageSource) then
    begin
      fScreensCount := rr.Match[1];
    end;
  finally
    rr.Free;
  end;

  fScreensCount := StringReplace(fScreensCount, ',', '', [rfReplaceAll, rfIgnoreCase]);
  Result := StrToIntDef(fScreensCount, 0);
end;

class function TIMDbInfoChecks.IsSTVBasedOnTitleExtraInfo(const aTitleExtraInfo: String): Boolean;
begin
  Result := False;

  if AnsiContainsText(aTitleExtraInfo, 'TV mini-series') then
  begin
    (* Mini Series designed for Television *)
    Result := True;
  end
  else if AnsiContainsText(aTitleExtraInfo, 'TV') then
  begin
    (* TV or STV Production *)
    Result := True;
  end
  else if AnsiContainsText(aTitleExtraInfo, 'Video') then
  begin
    (* Videogame *)
    Result := True;
  end;
end;

class function TIMDbInfoChecks.EstimateEnglishCountryOrder(const aImdbCountries: String): String;
var
  fStringIndex1: Integer;
  fStringIndex2: Integer;
begin
  // get index to check which country is listed first
  fStringIndex1 := aImdbCountries.IndexOf('USA');
  // fStringIndex1 := aImdbCountries.IndexOf('United States');
  fStringIndex2 := aImdbCountries.IndexOf('UK');
  // fStringIndex2 := aImdbCountries.IndexOf('United Kingdom');

  // pick first one with according country representation used in slftp
  if ((fStringIndex1 <> -1) and (fStringIndex2 <> -1)) then
  begin
    // both are listed, take the first occurring one
    if fStringIndex1 < fStringIndex2 then
      Result := 'USA'
    else
      Result := 'UK';
  end
  else if (fStringIndex2 <> -1) then
  begin
    // only UK is listed
    Result := 'UK';
  end
  else
  begin
    // USA is listed or used as default fallback
    Result := 'USA';
  end;
end;

{ TPazoHTTPImdbTask }

constructor TPazoHTTPImdbTask.Create(const aImdbTitleID: String; const aReleaseName: String);
begin
  self.FImdbTitleID := aImdbTitleID;
  self.FReleaseName := aReleaseName;
  inherited Create('', '', getAdminSiteName);
end;

function TPazoHTTPImdbTask.Execute(slot: Pointer): Boolean;
var
  ir: TImdbRelease;
  imdbdata: TDbImdbData;

  fHttpGetErrMsg: String;
  fRegExpr: TRegExpr;
  fStrHelper: String;
  fReleasenameCountry: String;
  fStringIndex1: Integer;
  fStringIndex2: Integer;
  i: Integer;

  fImdbMainPage: String;
  fImdbReleasePage: String;
  fImdbOriginalTitle: String;
  FImdbYear: Integer;
  fImdbTitleExtraInfo: String;
  fImdbVotes: String;
  fImdbRating: String;
  fImdbLanguage: String;
  fImdbCountry: String;
  fImdbGenre: String;
  fIsSTV: Boolean;
  fIsFestival: Boolean;
  fIsWide: Boolean;
  fIsLimited: Boolean;
  fStatusReason: String;
  fStatusReasonList: TList<String>;
  fImdbReleaseDateInfoList: TObjectList<TIMDbReleaseDateInfo>;
  fImdbReleaseDateInfo: TIMDbReleaseDateInfo;
  fImdbReleaseDate: String;
  fImdbRlsdateExtraInfo: String;
  fImdbCineYear: Integer;

  fTvShowname: String;
  fTvSeason: Integer;
  fTvEpisode: Int64;

  fBomMainPage: String;
  fBomCountryPage: String;
  fBOMCountryLinks: TDictionary<String, String>; // countryname and release link
  fBOMCountryLinkPair: TPair<String, String>;
  fBOMCountryScreens: TDictionary<String, Integer>; // countryname and screens count
  fBomScreensCount: Integer;
begin
  Result := False;

  if (FReleaseName = '') then
  begin
    irc_Adderror(Format('<c4>[ERROR]</c> TPazoHTTPImdbTask rls empty.', []));
    Result := True;
    ready := True;
    exit;
  end;

  // TODO
  // maybe simple use FindPazoByRls(FReleaseName) and then pazo.rls.<whatever is needed>?
  // kb seems to not offer a function to get kb info for a given rlsname
  try
    ir := TImdbRelease.Create(FReleaseName, '');
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TPazoHTTPImdbTask TImdbRelease.Create: %s ', [e.Message]));
      irc_Adderror(Format('<c4>[EXCEPTION]</c> TPazoHTTPImdbTask TImdbRelease.Create: %s', [e.Message]));
      Result := True;
      ready := True;
      exit;
    end;
  end;

  // TODO: json stuff of new layout
  // startindex = Pos('type="application/json">', http_response);
  // endindex = Pos('</script>', http_response, startindex);
  // count := endindex - startindex;
  // json := Copy(http_response, startindex + Length('type="application/json">'), count);

  (* Get IMDb main page *)
  if not HttpGetUrl('https://www.imdb.com/title/' + FImdbTitleID + '/', fImdbMainPage, fHttpGetErrMsg) then
  begin
    Debug(dpMessage, section, Format('[FAILED] TPazoHTTPImdbTask mainpage --> %s ', [fHttpGetErrMsg]));
    irc_Adderror(Format('<c4>[FAILED]</c> TPazoHTTPImdbTask mainpage --> %s', [fHttpGetErrMsg]));
    Result := True;
    ready := True;
    exit;
  end;

  (* Fetch MovieTitle/Extra/Year *)
  THtmlIMDbParser.ParseMetaTitleInformation(fImdbMainPage, fImdbOriginalTitle, fImdbTitleExtraInfo, FImdbYear);

  (* Fetch Votes and Rating *)
  THtmlIMDbParser.ParseVotesAndRating(fImdbMainPage, fImdbVotes, fImdbRating);

  (* Fetch Languages *)
  THtmlIMDbParser.ParseMovieLanguage(fImdbMainPage, fImdbLanguage);

  (* Fetch Countries *)
  THtmlIMDbParser.ParseMovieCountries(fImdbMainPage, fImdbCountry);

  (* Fetch Genres *)
  THtmlIMDbParser.ParseMovieGenres(fImdbMainPage, fImdbGenre);


  // TODO:
  // 1. BOM screens have highest priority for STV/Limited/Wide determination
  // "Box Office Mojo by IMDbPro considers a movie in wide release or about to go wide when it is playing at 600 or more theaters, which generally indicates a nationwide release (the term is short for "nationwide").
  // A movie is considered to be in limited release when playing at fewer than 600 theaters (i.e. released in one or more markets but not nationwide)."

  // 2. (fallback) /releaseinfo page info is used as fallback for STV determination in negative way -> wide = true at beginning and changed if e.g. dvd/tv
  //    iterates through all infos to determine the final result but only determines STV as limited/wide is only done via bom screens

  // 3. movie extra info have lowest priority as it might not indicate the correct info for each country

  fIsSTV := False;
  fIsLimited := False;
  fIsWide := True;
  fIsFestival := False;

  fStatusReasonList := TList<String>.Create;
  try
    (* Check global STV status based on title *)
    fIsSTV := TIMDbInfoChecks.IsSTVBasedOnTitleExtraInfo(fImdbTitleExtraInfo);
    if fIsSTV then
    begin
      fStatusReasonList.Add(Format('STV due to title extra info: %s', [fImdbTitleExtraInfo]));
      Debug(dpSpam, section, Format('Status from Releasename: %s', [fStatusReasonList.Last]));
    end;

    (* Check if it's a TV release *)
    // if we get values for season or episode -> tv show which doesn't has any screens
    getShowValues(FReleaseName, fTvShowname, fTvSeason, fTvEpisode);
    if not ((fTvSeason > 0) or (fTvEpisode > 0) or (fTvSeason = Ord(tvDatedShow))
           or (fTvSeason = Ord(tvRegularSerieWithoutSeason)) or (fTvEpisode = Ord(tvNoEpisodeTag))) then
    begin
      fIsSTV := True;
      fStatusReasonList.Add(Format('STV due to being a TV show with season %d and/or episode %d', [fTvSeason, fTvEpisode]));
      Debug(dpSpam, section, Format('Status from Releasename: %s', [fStatusReasonList.Last]));
    end;

    (* Get IMDb releaseinfo page *)
    if not HttpGetUrl('https://www.imdb.com/title/' + FImdbTitleID + '/releaseinfo', fImdbReleasePage, fHttpGetErrMsg) then
    begin
      Debug(dpMessage, section, Format('[FAILED] TPazoHTTPImdbTask releaseinfo --> %s ', [fHttpGetErrMsg]));
      irc_Adderror(Format('<c4>[FAILED]</c> TPazoHTTPImdbTask releaseinfo --> %s', [fHttpGetErrMsg]));
      Result := True;
      ready := True;
      exit;
    end;

    (* Extract releaseinfo *)
    fImdbReleaseDateInfoList := TObjectList<TIMDbReleaseDateInfo>.Create(True);
    try
      THtmlIMDbParser.ParseReleaseDateInfo(fImdbReleasePage, fImdbReleaseDateInfoList);

          { NOTE: all that needs to be done separately for each dedicated releasename }
          (* get language of release (should be moved later to kb? as it depends on the actual releasename) *)
          if (ir.language <> 'English') then
            fStrHelper := ir.language
          else
            fStrHelper := TIMDbInfoChecks.EstimateEnglishCountryOrder(fImdbCountry);

          fReleasenameCountry := TMapLanguageCountry.GetCountrynameByLanguage(fStrHelper);
          Debug(dpSpam, section, Format('Release language %s maps to country %s', [ir.language, fReleasenameCountry]));
          if fReleasenameCountry = '' then
          begin
            Debug(dpError, section, Format('[ERROR] No mapping for language %s to a country found', [ir.language]));
            ready := True;
            Result := True;
            Exit;
          end;

          // TODO: remove this in future code
          ir.Free;

          (* STV infos *)
          for fImdbReleaseDateInfo in fImdbReleaseDateInfoList do
          begin
            if fImdbReleaseDateInfo.FCountry = fReleasenameCountry then
            begin
              fImdbReleaseDate := fImdbReleaseDateInfo.FReleaseDate;
              fImdbRlsdateExtraInfo := fImdbReleaseDateInfo.FExtraInfo;

              if fImdbRlsdateExtraInfo <> '' then
              begin
                fRegExpr := TRegexpr.Create;
                try
                  fRegExpr.ModifierI := True;

                  fRegExpr.Expression := '(DVD|video|TV|Bluray|Blueray)(\s|\.|\-)?premiere';
                  if fRegExpr.Exec(fImdbRlsdateExtraInfo) then
                  begin
                    fIsSTV := True;
                    fStatusReasonList.Add(Format('STV in %s due to %s on %s', [fReleasenameCountry, fImdbRlsdateExtraInfo, fImdbReleaseDate]));
                    Debug(dpSpam, section, Format('Status from Releasepage: %s', [fStatusReasonList.Last]));
                    Break;
                  end;
                finally
                  fRegExpr.free;
                end;
              end;
            end;
          end;

          (* Festival info is independent from STV/Limited/Wide -> screened on a festival for movie enthuastics *)
          for fImdbReleaseDateInfo in fImdbReleaseDateInfoList do
          begin
            if fImdbReleaseDateInfo.FCountry = fReleasenameCountry then
            begin
              fImdbReleaseDate := fImdbReleaseDateInfo.FReleaseDate;
              fImdbRlsdateExtraInfo := fImdbReleaseDateInfo.FExtraInfo;

              if fImdbRlsdateExtraInfo <> '' then
              begin
                fRegExpr := TRegexpr.Create;
                try
                  fRegExpr.ModifierI := True;

                  fRegExpr.Expression := 'F(estival|ilmfest|est|ilm(\s|\.|\-)?Market?)';
                  if fRegExpr.Exec(fImdbRlsdateExtraInfo) then
                  begin
                    fIsFestival := True;
                    fStatusReasonList.Add(Format('Festival in %s due to %s on %s', [fReleasenameCountry, fImdbRlsdateExtraInfo, fImdbReleaseDate]));
                    Debug(dpSpam, section, Format('Status from Releasepage: %s', [fStatusReasonList.Last]));
                    Break;
                  end;
                finally
                  fRegExpr.free;
                end;
              end;
            end;
          end;

          (* Cinedate info *)
          if not fIsSTV then
          begin
            // pick first entry where third row (extra info) is empty
            for fImdbReleaseDateInfo in fImdbReleaseDateInfoList do
            begin
              if fImdbReleaseDateInfo.FCountry = fReleasenameCountry then
              begin
                fImdbReleaseDate := fImdbReleaseDateInfo.FReleaseDate;
                fImdbRlsdateExtraInfo := fImdbReleaseDateInfo.FExtraInfo;

                if fImdbRlsdateExtraInfo = '' then
                begin
                  fImdbCineYear := StrToIntDef(Copy(fImdbReleaseDate, Length(fImdbReleaseDate) - 4, 4), 0);
                  fStatusReasonList.Add(Format('Cine year for %s is %d taken from %s', [fReleasenameCountry, fImdbCineYear, fImdbReleaseDate]));
                  Debug(dpSpam, section, Format('Status from Releasepage: %s', [fStatusReasonList.Last]));
                  Break;
                end;
              end;
            end;
          end;
          { NOTE: all that needs to be done separately for each dedicated releasename }
    finally
      fImdbReleaseDateInfoList.Free;
    end;


    (* Get Box Office Mojo main page *)
    if not HttpGetUrl('https://www.boxofficemojo.com/title/' + FImdbTitleID + '/', fBomMainPage, fHttpGetErrMsg) then
    begin
      Debug(dpMessage, section, Format('[FAILED] TPazoHTTPImdbTask BoxOfficeMojo --> %s ', [fHttpGetErrMsg]));
      irc_Adderror(Format('<c4>[FAILED]</c> TPazoHTTPImdbTask BoxOfficeMojo --> %s', [fHttpGetErrMsg]));
      Result := True;
      ready := True;
      exit;
    end;

    (* Get links to each Country and from there the single screen counts *)
    fBOMCountryLinks := TDictionary<String, String>.Create;
    try
      THtmlBoxOfficeMojoParser.GetCountrySpecificLinks(fBomMainPage, fBOMCountryLinks);

      fBOMCountryScreens := TDictionary<String, Integer>.Create;
      try
        for fBOMCountryLinkPair in fBOMCountryLinks do
        begin
          // all links on original release page have this reference
          if not HttpGetUrl('https://www.boxofficemojo.com' + fBOMCountryLinkPair.Value + '?ref_=bo_gr_rls', fBomCountryPage, fHttpGetErrMsg) then
          begin
            Debug(dpMessage, section, Format('[FAILED] TPazoHTTPImdbTask BoxOfficeMojo --> %s ', [fHttpGetErrMsg]));
            irc_Adderror(Format('<c4>[FAILED]</c> TPazoHTTPImdbTask BoxOfficeMojo --> %s', [fHttpGetErrMsg]));
            Result := True;
            ready := True;
            exit;
          end;

          { NOTE: this needs to be saved }
          fBOMCountryScreens.Add(fBOMCountryLinkPair.Key, THtmlBoxOfficeMojoParser.GetWidestScreensCount(fBomCountryPage));
        end;

        { NOTE: all that needs to be done separately for each dedicated releasename based on the language->country -- check USA & UK for english }
        if not fBOMCountryScreens.TryGetValue(fReleasenameCountry, fBomScreensCount) then
          fBomScreensCount := 0;
        { NOTE: all that needs to be done separately for each dedicated releasename based on the language->country -- check USA & UK for english }
      finally
        fBOMCountryScreens.Free;
      end;
    finally
      fBOMCountryLinks.Free;
    end;

    (* Check screen count *)
    if fBomScreensCount = 0 then
    begin
      fIsSTV := True;
      fIsLimited := False;
      fIsWide := False;
      fStatusReasonList.Add(Format('STV due to screens count being zero for %s', [fReleasenameCountry]));
      Debug(dpSpam, section, Format('Status from Screen count: %s', [fStatusReasonList.Last]));
    end
    else if (fBomScreensCount < 600) then
    begin
      // limited release when playing at fewer than 600 theaters
      fIsSTV := False;
      fIsLimited := True;
      fIsWide := False;
      fStatusReasonList.Add(Format('Limited due to screens %d < 600 for %s', [fBomScreensCount, fReleasenameCountry]));
      Debug(dpSpam, section, Format('Status from Screen count: %s', [fStatusReasonList.Last]));
    end
    else
    begin
      // movie in wide release or about to go wide when it is playing at 600 or more theaters
      fIsSTV := False;
      fIsLimited := False;
      fIsWide := True;
      fStatusReasonList.Add(Format('Wide due to screens %d => 600 for %s', [fBomScreensCount, fReleasenameCountry]));
      Debug(dpSpam, section, Format('Status from Screen count: %s', [fStatusReasonList.Last]));
    end;

    for i := 0 to fStatusReasonList.Count - 1 do
    begin
      fStatusReason := fStatusReason + Format('%d - %s%s', [i + 1, fStatusReasonList[i], #13#10]);
    end;
  finally
    fStatusReasonList.Free;
  end;


  // create dataset to get it work for the moment
  imdbdata := TDbImdbData.Create(FImdbTitleID);
  imdbdata.imdb_id := FImdbTitleID;
  imdbdata.imdb_year := FImdbYear;
  imdbdata.imdb_languages.CommaText := fImdbLanguage;
  imdbdata.imdb_countries.CommaText := fImdbCountry;
  imdbdata.imdb_genres.CommaText := fImdbGenre;
  imdbdata.imdb_screens := fBomScreensCount;
  imdbdata.imdb_rating := StrToIntDef(fImdbRating, 0);
  imdbdata.imdb_votes := StrToIntDef(fImdbVotes, 0);
  imdbdata.imdb_cineyear := fImdbCineYear;
  imdbdata.imdb_ldt := fIsLimited;
  imdbdata.imdb_wide := fIsWide;
  imdbdata.imdb_festival := fIsFestival;
  imdbdata.imdb_stvm := fIsSTV;
  imdbdata.imdb_stvs := fStatusReason;
  imdbdata.imdb_origtitle := fImdbOriginalTitle;
  try
    dbaddimdb_SaveImdbData(FReleaseName, imdbdata);
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TPazoHTTPImdbTask dbaddimdb_SaveImdb: %s ', [e.Message]));
    end;
  end;

  ready := True;
  Result := True;
end;

function TPazoHTTPImdbTask.Name: String;
begin
  try
    Result := Format('HTTP IMDb for %s : ID %s', [FReleaseName, FImdbTitleID]);
  except
    Result := 'HTTP IMDb';
  end;
end;

destructor TPazoHTTPImdbTask.Destroy;
begin
  inherited;
end;

end.

