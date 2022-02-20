unit taskhttpimdb;

interface

uses
  tasksunit, Generics.Collections, SynCommons, Variants, dbaddimdb;

type

  { @abstract(Extracts IMDb information from HTML page source) }
  THtmlIMDbParser = class
  public
    { Parses the JSON from the page source and returns a JSON object variant
      @param(aPageSource Webpage HTML sourcecode)
      @returns(a JSON object variant) }
    class function GenerateJSONObject(const aPageSource, aImdbID: string): Variant;

    { Parses title information from the meta property @italic(title) tag
      @param(aJsonObject The JSON from the page as Variant object)
      @param(aMovieTitle Title of the movie (can be empty))
      @param(aTitleExtraInfo Additional info (e.g. TV Series) from the title (can be empty))
      @param(aYear Year of the movie (0 if not available)) }
    class procedure ParseMetaTitleInformation(const aJsonObject: Variant; out aMovieTitle, aTitleExtraInfo: String; out aYear: Integer);

    { Parses votes and rating and removes dots and commas @br @note(default value for both is 0)
      @param(aJsonObject The JSON from the page as Variant object)
      @param(aVotes Votes of the movie, default value is 0)
      @param(aRating Rating of the movie, default value is 0) }
    class procedure ParseVotesAndRating(const aJsonObject: Variant; out aVotes, aRating: Integer);

    { Parses language(s)
      @param(aPageSource Webpage HTML sourcecode)
      @param(aLanguageList Language(s) of the movie as comma separated list) }
    class procedure ParseMovieLanguage(const aPageSource: String; out aLanguageList: String);

    { Parses Countrie(s)
      @param(aPageSource Webpage HTML sourcecode)
      @param(aCountriesList Countrie(s) of the movie as comma separated list) }
    class procedure ParseMovieCountries(const aPageSource: String; out aCountriesList: String);

    { Parses Genre(s)
      @param(aJsonObject The JSON from the page as Variant object)
      @param(aGenresList Genre(s) of the movie as comma separated list) }
    class procedure ParseMovieGenres(const aJsonObject: Variant; out aGenresList: String);

    { Parses Releasedates(s) for countries included in slftp.imdbcountries
      @param(aPageSource Releasedate Webpage HTML sourcecode)
      @param(aReleaseDateInfoList List of releasedate information) }
    class procedure ParseReleaseDateInfo(const aPageSource: String; var aReleaseDateInfoList: TObjectList<TIMDbReleaseDateInfo>);

    { Parses 'Also Known As' (AKA) information for countries included in slftp.imdbcountries plus original title @br @note(Does not replace any special characters)
      @param(aPageSource Releasedate Webpage HTML sourcecode)
      @param(aAlsoKnownAsList List of AKA information) }
    class procedure ParseAlsoKnownAsInfo(const aPageSource: String; var aAlsoKnownAsList: TObjectList<TIMDbAlsoKnownAsInfo>);
  end;

  { @abstract(Extracts Box Office information from boxofficemojo.com HTML page source) }
  THtmlBoxOfficeMojoParser = class
  public
    { Checks if the given webpage lists infos for different release groups (e.g. re-releases or releases for different markets)
      @param(aPageSource Webpage HTML sourcecode)
      @returns(@true if it lists 'By Release', @false otherwise) }
    class function ListsOnlyReleaseGroups(const aPageSource: String): Boolean;

    { Parses the release groups and links from the title overview page which are shown only if a movie e.g. had some re-releases
      @param(aPageSource Webpage HTML sourcecode)
      @param(aReleaseGroupLinks Release Group and link to the country information for this release group) }
    class procedure GetGroupSpecificLinks(const aPageSource: String; out aReleaseGroupLinks: TDictionary<String, String>);

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
  sitesunit, RegExpr, mystrings, dbtvinfo, sllanguagebase;

const
  section = 'taskhttpimdb';

{ THtmlIMDbParser }

class function THtmlImdbParser.GenerateJSONObject(const aPageSource, aImdbID: string): Variant;
var
  fStartIndex, fEndIndex, fCount: integer;
  fJsonObject: variant;
  fJsonString: string;
  fJsonImdbID, fJsonReleaseYear, fTitleType: RawUTF8;
  rr: TRegExpr;
  doc: TDocVariantData;
  pdoc: PDocVariantData;
begin
  Result := Variants.Null;
  fStartIndex := Pos('type="application/json">', aPageSource);

  if fStartIndex < 1 then
    Exit;

  fEndIndex := Pos('</script>', aPageSource, fStartIndex);
  fCount := fEndIndex - fStartIndex;
  fJsonString := Copy(aPageSource, fStartIndex + Length('type="application/json">'), fCount);
  fJsonObject := _JsonFast(fJsonString);


  //ugly way to find the right JSON, is there a better way?
  rr := TRegExpr.Create;
  try
    rr.Expression := '"([0-9]{4,99})+":\{"data":';

    if rr.Exec(aPageSource) then
    repeat
      doc := TDocVariantData(fJsonObject);
      doc.GetAsDocVariant('props', pdoc);
      pdoc.GetAsDocVariant('pageProps', pdoc);
      pdoc.GetAsDocVariant('urqlState', pdoc);
      pdoc.GetAsDocVariant(rr.Match[1], pdoc);
      pdoc.GetAsDocVariant('data', pdoc);
      pdoc.GetAsDocVariant('title', pdoc);
      pdoc.GetAsRawUTF8('id', fJsonImdbID);
      pdoc.GetAsRawUTF8('releaseYear', fJsonReleaseYear);
      pdoc.GetAsRawUTF8('titleType', fTitleType);
      if (fJsonImdbID = aImdbID) and (fJsonReleaseYear <> '') and (0 <> Pos('text', fTitleType)) then
      begin
        Result := _JsonFast(pdoc.ToJSON());
        exit;
      end;
    until not rr.ExecNext;
  finally
    rr.Free;
  end;
end;

class procedure THtmlIMDbParser.ParseMetaTitleInformation(const aJsonObject: Variant; out aMovieTitle, aTitleExtraInfo: String; out aYear: Integer);
begin
  if not VarIsNull(aJsonObject) then
  begin
    aMovieTitle := aJsonObject.originalTitleText.text;
    aTitleExtraInfo := aJsonObject.titleType.text;
    aYear := aJsonObject.releaseYear.year;
  end;
end;

class procedure THtmlIMDbParser.ParseVotesAndRating(const aJsonObject: Variant; out aVotes, aRating: Integer);
var
  fVotes, fRating: String;
begin
  if not VarIsNull(aJsonObject) then
  begin
    if VarIsNull(aJsonObject.ratingsSummary.voteCount) then
      fVotes := '0'
    else
      fVotes := aJsonObject.ratingsSummary.voteCount;

    if VarIsNull(aJsonObject.ratingsSummary.aggregateRating) then
      fRating := '0'
    else
      fRating := aJsonObject.ratingsSummary.aggregateRating;

    fVotes := StringReplace(fVotes, '.', '', [rfReplaceAll, rfIgnoreCase]);
    fVotes := StringReplace(fVotes, ',', '', [rfReplaceAll, rfIgnoreCase]);
    aVotes := StrToIntDef(fVotes, 0);

    // if the rating is an even number, it's without decimal place in the JSON
    // because we use rating*10 in the rules, add a '0' here.
    if Length(fRating) = 1 then
      fRating := fRating + '0'
    else
    begin
      fRating := StringReplace(fRating, '.', '', [rfReplaceAll, rfIgnoreCase]);
      fRating := StringReplace(fRating, ',', '', [rfReplaceAll, rfIgnoreCase]);
    end;
    aRating := StrToIntDef(fRating, 0);
  end;
end;

class procedure THtmlIMDbParser.ParseMovieLanguage(const aPageSource: String; out aLanguageList: String);
var
  fRegex: TRegExpr;
  fMatch: string;
begin
  fRegex := TRegExpr.Create;
  try
    fRegex.Expression := 'data-testid="title-details-languages">.*?<div(.*?<\/a>)<\/li><\/ul><\/div><\/li>';
    if fRegex.Exec(aPageSource) then
    begin
      fMatch := fRegex.Match[1];
      fRegex.Expression := 'ref_=tt_dt_ln">(.*?)<\/a>';
      if fRegex.Exec(fMatch) then
      begin
        repeat
          aLanguageList := aLanguageList + fRegex.Match[1] + ',';
        until not fRegex.ExecNext;
      end;
    end
  finally
    fRegex.Free;
  end;

  // remove additional comma
  SetLength(aLanguageList, Length(aLanguageList) - 1);
end;

class procedure THtmlIMDbParser.ParseMovieCountries(const aPageSource: String; out aCountriesList: String);
var
  fRegex: TRegExpr;
  fMatch: string;
begin
  fRegex := TRegExpr.Create;
  try
    fRegex.Expression := 'data-testid="title-details-origin">.*?<div(.*?<\/a>)<\/li><\/ul><\/div><\/li>';
    if fRegex.Exec(aPageSource) then
    begin
      fMatch := fRegex.Match[1];
      fRegex.Expression := 'ref_=tt_dt_cn">(.*?)<\/a>';
      if fRegex.Exec(fMatch) then
      begin
        repeat
          fMatch := fRegex.Match[1];

          // rewrite to old format
          if fMatch = 'United States' then
            fMatch := 'USA'
          else if fMatch = 'United Kingdom' then
            fMatch := 'UK';

          aCountriesList := aCountriesList + fMatch + ',';
        until not fRegex.ExecNext;
      end;
    end
  finally
    fRegex.Free;
  end;

  // remove additional comma
  SetLength(aCountriesList, Length(aCountriesList) - 1);
end;

class procedure THtmlIMDbParser.ParseMovieGenres(const aJsonObject: Variant; out aGenresList: String);
var
  fDocVariant: PDocVariantData;
  fVariant: Variant;
begin
  if not VarIsNull(aJsonObject) then
  begin
    TDocVariantData(aJsonObject).GetAsDocVariant('genres', fDocVariant);
    fDocVariant.GetAsDocVariant('genres', fDocVariant);
    for fVariant in fDocVariant.Values do
    begin
      aGenresList := aGenresList + fVariant.text + ',';
    end;
  end;

  // remove additional comma
  SetLength(aGenresList, Length(aGenresList) - 1);
end;

class procedure THtmlIMDbParser.ParseReleaseDateInfo(const aPageSource: String; var aReleaseDateInfoList: TObjectList<TIMDbReleaseDateInfo>);
var
  rr: TRegExpr;
  fCountryCode: String;
  fCountry: String;
  fReleaseDateString: String;
  fExtraInfo: String;
  fStringHelper: String;
  fReleaseDate: TDateTime;
  fReleaseDateSplit: TArray<String>;
  fReleaseDateMonth: integer;
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
        fReleaseDateString := Trim(rr.Match[3]);
        fExtraInfo := Trim(rr.Match[4]);

        if ExcludeCountry(fCountry) then
          Continue;

        // try to get a TDateTime from the date string of the web page. Date from is coming like this: '30 September 2018'
        try
          fReleaseDateSplit := fReleaseDateString.Split([' ']);

          //ugly: try to parse the month part of the date, TODO: make the parse work with format string somehow?
          fStringHelper := fReleaseDateSplit[1];
          if (fStringHelper = 'January') then
            fReleaseDateMonth := 1
          else if (fStringHelper = 'February') then
            fReleaseDateMonth := 2
          else if (fStringHelper = 'March') then
            fReleaseDateMonth := 3
          else if (fStringHelper = 'April') then
            fReleaseDateMonth := 4
          else if (fStringHelper = 'May') then
            fReleaseDateMonth := 5
          else if (fStringHelper = 'June') then
            fReleaseDateMonth := 6
          else if (fStringHelper = 'July') then
            fReleaseDateMonth := 7
          else if (fStringHelper = 'August') then
            fReleaseDateMonth := 8
          else if (fStringHelper = 'September') then
            fReleaseDateMonth := 9
          else if (fStringHelper = 'October') then
            fReleaseDateMonth := 10
          else if (fStringHelper = 'November') then
            fReleaseDateMonth := 11
          else if (fStringHelper = 'December') then
            fReleaseDateMonth := 12
          else
            raise Exception.Create('Unknown month: ' + fStringHelper);

          fReleaseDate := EncodeDate(StrToInt(fReleaseDateSplit[2]), fReleaseDateMonth, StrToInt(fReleaseDateSplit[0]));
        except
          on e: Exception do
          begin
            Debug(dpMessage, section, 'IMDB release date info: unable to parse as DateTime: ' + fReleaseDateString + ' (' + e.Message + ')');
            Continue;
          end;
        end;

        aReleaseDateInfoList.Add(TIMDbReleaseDateInfo.Create(fCountry, fExtraInfo, fReleaseDate));
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

class function THtmlBoxOfficeMojoParser.ListsOnlyReleaseGroups(const aPageSource: String): Boolean;
begin
  Result := aPageSource.Contains('<h3 class="a-spacing-base">By Release</h3>');
end;

class procedure THtmlBoxOfficeMojoParser.GetGroupSpecificLinks(const aPageSource: String; out aReleaseGroupLinks: TDictionary<String, String>);
var
  rr: TRegExpr;
  fLink: String;
  fReleaseGroup: String;
begin
  rr := TRegExpr.Create;
  try
    rr.ModifierI := True;
    // only get the matches in 'Release Group' column
    rr.Expression := '<tr[^>]*>.*?<a class="a-link-normal" href="(\/releasegroup\/gr\d+).*?">(.*?)<\/a>';

    if rr.Exec(aPageSource) then
    begin
      repeat
        fLink := Trim(rr.Match[1]);
        fReleaseGroup := Trim(rr.Match[2]);

        // some pages list the same release group more than once therefore check if that group already exists
        if not aReleaseGroupLinks.ContainsKey(fReleaseGroup) then
          aReleaseGroupLinks.Add(fReleaseGroup, fLink);
      until not rr.ExecNext;
    end;
  finally
    rr.Free;
  end;
end;

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
  imdbdata: TDbImdbData;

  fHttpGetErrMsg: String;
  fRegExpr: TRegExpr;
  fStrHelper: String;
  fReleasenameCountry: String;
  fStringIndex1: Integer;
  fStringIndex2: Integer;
  i: Integer;
  fLanguageFromReleasename: String;

  fJsonObject: Variant;

  fImdbMainPage: String;
  fImdbReleasePage: String;
  fImdbOriginalTitle: String;
  FImdbYear: Integer;
  fImdbTitleExtraInfo: String;
  fImdbVotes: Integer;
  fImdbRating: Integer;
  fImdbLanguage: String;
  fImdbCountry: String;
  fImdbGenre: String;
  fIsSTV: Boolean;
  fIsFestival: Boolean;
  fIsWide: Boolean;
  fIsLimited: Boolean;
  fImdbReleaseDateInfoList: TObjectList<TIMDbReleaseDateInfo>;
  fIMDbAlsoKnownAsInfoList: TObjectList<TIMDbAlsoKnownAsInfo>;
  fImdbReleaseDateInfo: TIMDbReleaseDateInfo;
  fImdbReleaseDate: String;
  fImdbRlsdateExtraInfo: String;
  fImdbCineYear: Integer;

  fTvShowname: String;
  fTvSeason: Integer;
  fTvEpisode: Int64;

  fBomMainPage: String;
  fBomGroupUrl: String;
  fBomCountryPage: String;
  fBOMReleaseGroups: TDictionary<String, String>; // release group and link
  fBOMCountryLinks: TDictionary<String, String>; // countryname and release link
  fBOMReleaseGroupPair, fBOMCountryLinkPair: TPair<String, String>;
  fBOMCountryScreens: TDictionary<String, Integer>; // countryname and screens count
  fBomScreensCount: Integer;

  fFound_LastImdb: Integer;
  fFound: Boolean;

begin
  Result := False;

    // exit if imdb info is already known in last_imdbdata
  gDbAddimdb_cs.Enter;
  try
    fFound_LastImdb := last_addimdb.IndexOf(getMovieNameWithoutSceneTags(FReleaseName));
    if fFound_LastImdb = -1 then
    begin
      Debug(dpError, section, Format('[Info] taskhttpimdb add Release to temp Hashlist: %s - %s', [FReleaseName, getMovieNameWithoutSceneTags(FReleaseName)]));
      last_addimdb.add(getMovieNameWithoutSceneTags(FReleaseName));
    end;
  finally
    gDbAddimdb_cs.Leave;
  end;

	try
    ffound := (fFound_LastImdb <> -1);
    if ffound = True then
    begin
      Result := True;
      ready := True;
      exit;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] taskhttpimdb last_imdbdata.IndexOf: %s', [e.Message]));
      readyerror := True;
      exit;
    end;
  end;

  (* Get IMDb main page *)
  if not HttpGetUrl('https://www.imdb.com/title/' + FImdbTitleID + '/', fImdbMainPage, fHttpGetErrMsg) then
  begin
    Debug(dpMessage, section, Format('[FAILED] TPazoHTTPImdbTask mainpage --> %s ', [fHttpGetErrMsg]));
    irc_Adderror(Format('<c4>[FAILED]</c> TPazoHTTPImdbTask mainpage --> %s', [fHttpGetErrMsg]));
    Result := True;
    ready := True;
    exit;
  end;

  fJsonObject := THtmlImdbParser.GenerateJSONObject(fImdbMainPage, FImdbTitleID);
  if not VarIsNull(fJsonObject) then
  begin
    (* Fetch MovieTitle/Extra/Year *)
    THtmlIMDbParser.ParseMetaTitleInformation(fJsonObject, fImdbOriginalTitle, fImdbTitleExtraInfo, FImdbYear);

    (* Fetch Votes and Rating *)
    THtmlIMDbParser.ParseVotesAndRating(fJsonObject, fImdbVotes, fImdbRating);

    (* Fetch Genres *)
    THtmlIMDbParser.ParseMovieGenres(fJsonObject, fImdbGenre);
  end
  else
  begin
    // some values from JSON are needed for further parsing and evaluation of e.g. STV
    irc_Adderror(Format('<c4>[FAILED]</c> Unable to extract JSON for %s from IMDb', [FImdbTitleID]));
    Result := True;
    ready := True;
    exit;
  end;

  (* Fetch Languages *)
  THtmlIMDbParser.ParseMovieLanguage(fImdbMainPage, fImdbLanguage);

  (* Fetch Countries *)
  THtmlIMDbParser.ParseMovieCountries(fImdbMainPage, fImdbCountry);


  // TODO:
  // 1. BOM screens have highest priority for STV/Limited/Wide determination

  // 2. (fallback) /releaseinfo page info is used as fallback for STV determination in negative way -> wide = true at beginning and changed if e.g. dvd/tv
  //    iterates through all infos to determine the final result but only determines STV as limited/wide is only done via bom screens

  // 3. movie extra info have lowest priority as it might not indicate the correct info for each country


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
    THtmlIMDbParser.ParseReleaseDateInfo(fImdbReleasePage, fImdbReleaseDateInfoList);

    (* Extract AlsoKnownAsInfo *)
    fIMDbAlsoKnownAsInfoList := TObjectList<TIMDbAlsoKnownAsInfo>.Create(True);
    THtmlIMDbParser.ParseAlsoKnownAsInfo(fImdbReleasePage, fIMDbAlsoKnownAsInfoList);

    (* Get Box Office Mojo main page *)
    if not HttpGetUrl('https://www.boxofficemojo.com/title/' + FImdbTitleID + '/', fBomMainPage, fHttpGetErrMsg) then
    begin
      Debug(dpMessage, section, Format('[FAILED] TPazoHTTPImdbTask BoxOfficeMojo --> %s ', [fHttpGetErrMsg]));
      irc_Adderror(Format('<c4>[FAILED]</c> TPazoHTTPImdbTask BoxOfficeMojo --> %s', [fHttpGetErrMsg]));
      Result := True;
      ready := True;
      exit;
    end;

    (* Check if it shows different Release Groups (e.g. has re-releases) as this needs different handling first *)
    if THtmlBoxOfficeMojoParser.ListsOnlyReleaseGroups(fBomMainPage) then
    begin
      fBOMReleaseGroups := TDictionary<String, String>.Create;
      try
        THtmlBoxOfficeMojoParser.GetGroupSpecificLinks(fBomMainPage, fBOMReleaseGroups);

        for fBOMReleaseGroupPair in fBOMReleaseGroups do
        begin
          fBomGroupUrl := fBOMReleaseGroupPair.Value;
          Debug(dpSpam, section, Format('[BOM] The mainpage contains release groups - fetching %s with link %s', [fBOMReleaseGroupPair.Key, fBOMReleaseGroupPair.Value]));
          Break;
        end;
      finally
        fBOMReleaseGroups.Free;
      end;

      if fBomGroupUrl = '' then
      begin
        // no link found -> exit
        // TODO: maybe implement a better way instead of full aborting...
        Debug(dpMessage, section, Format('[FAILED] BoxOfficeMojo Release Group links not found for %s', [FImdbTitleID]));
        irc_Adderror(Format('<c4>[FAILED]</c> BoxOfficeMojo Release Group links not found for %s', [FImdbTitleID]));
        Result := True;
        ready := True;
        exit;
      end;

      if not HttpGetUrl('https://www.boxofficemojo.com' + fBomGroupUrl + '/', fBomMainPage, fHttpGetErrMsg) then
      begin
        Debug(dpMessage, section, Format('[FAILED] TPazoHTTPImdbTask BoxOfficeMojo --> %s ', [fHttpGetErrMsg]));
        irc_Adderror(Format('<c4>[FAILED]</c> TPazoHTTPImdbTask BoxOfficeMojo --> %s', [fHttpGetErrMsg]));
        Result := True;
        ready := True;
        exit;
      end;
    end;

    (* Get links to each Country and from there the single screen counts *)
    fBOMCountryLinks := TDictionary<String, String>.Create;
    try
      THtmlBoxOfficeMojoParser.GetCountrySpecificLinks(fBomMainPage, fBOMCountryLinks);

      fBOMCountryScreens := TDictionary<String, Integer>.Create;
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
    finally
      fBOMCountryLinks.Free;
    end;


  // create dataset to get it work for the moment
  imdbdata := TDbImdbData.Create(FImdbTitleID);
  imdbdata.imdb_id := FImdbTitleID;
  imdbdata.imdb_year := FImdbYear;
  imdbdata.imdb_languages.CommaText := fImdbLanguage;
  imdbdata.imdb_countries.CommaText := fImdbCountry;
  imdbdata.imdb_genres.CommaText := fImdbGenre;
  imdbdata.imdb_rating := fImdbRating;
  imdbdata.imdb_votes := fImdbVotes;
  imdbdata.imdb_origtitle := fImdbOriginalTitle;
  try
    try
      dbaddimdb_SaveImdbData(FReleaseName, imdbdata, fIMDbAlsoKnownAsInfoList, fImdbReleaseDateInfoList, fBOMCountryScreens);
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] TPazoHTTPImdbTask dbaddimdb_SaveImdb: %s ', [e.Message]));
      end;
    end;
  finally
    fImdbReleaseDateInfoList.Free;
    fIMDbAlsoKnownAsInfoList.Free;
    fBOMCountryScreens.Free;
    imdbdata.Free;
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
