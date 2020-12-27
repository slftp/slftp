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

  TPazoHTTPImdbTask = class(TTask)
  private
    rls: String;
    imdb_id: String;
  public
    constructor Create(const imdb_id: String; const rls: String);
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

      // Trying new layout of iMDB first
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
        // Trying old layout of iMDB if new layout fails
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

      // Trying new layout of iMDB first
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
        // Trying old layout of iMDB if new layout fails
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

      // Trying new layout of iMDB first
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
        // Trying old layout of iMDB if new layout fails
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
    rr.Expression := '<a class="a-link-normal" href="(\/release\/rl\d+)\?.*?">(.*?)<\/a>';

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

{ TPazoHTTPImdbTask }

constructor TPazoHTTPImdbTask.Create(const imdb_id: String; const rls: String);
begin
  self.imdb_id := imdb_id;
  self.rls := rls;
  inherited Create('', '', getAdminSiteName);
end;

function TPazoHTTPImdbTask.Execute(slot: Pointer): Boolean;
var
  imdb_stv: boolean;
  imdb_year, imdb_screens: Integer;
  imdbdata: TDbImdbData;
  rr, rr2: TRegexpr;
  imdb_mtitle, imdb_extra, imdb_date, s, imdb_counline, imdb_country, rlang,
    imdb_genr, imdb_countr, imdb_lang, imdb_region, bom_date, imdb_votes, imdb_rating: String;
  ir: TImdbRelease;
  i: integer;
  mainsite, rlsdatesite, businesssite, bomsite: String;
  release_date: TDateTime;
  formatSettings: TFormatSettings;
  showname: String;
  season: integer;
  episode: int64;
  fBOMSearchNeeded: boolean;
  fBusinessInfoPart: String;
  fRlsdateExtraInfo: String;
  fPictureID: String;
  fHttpGetErrMsg: String;
  fReleasegroupID: String;
  fDomesticReleaseID: String;
  fBOMCountryLinks: TDictionary<String, String>; // countryname and release link
  fBOMCountryLinkPair: TPair<String, String>;
  fBOMCountryScreens: TDictionary<String, Integer>; // countryname and screens count
begin
  Result := False;
  fPictureID := '';

  if (rls = '') then
  begin
    irc_Adderror(Format('<c4>[ERROR]</c> TPazoHTTPImdbTask rls empty.', []));
    Result := True;
    ready := True;
    exit;
  end;

  try
    ir := TImdbRelease.Create(rls, '');
  except
    on e: Exception do
    begin
      mainsite := '';
      rlsdatesite := '';
      businesssite := '';
      Debug(dpError, section,
        Format('[EXCEPTION] TPazoHTTPImdbTask TImdbRelease.Create: %s ',
        [e.Message]));
      irc_Adderror(Format('<c4>[EXCEPTION]</c> TPazoHTTPImdbTask TImdbRelease.Create: %s', [e.Message]));
      Result := True;
      ready := True;
      exit;
    end;
  end;

  rr := TRegexpr.Create;
  try
    rr.ModifierI := True;

    imdb_year := 0;

    (* Fetch MainInfoPage from iMDB *)
    if not HttpGetUrl('https://www.imdb.com/title/' + imdb_id + '/', mainsite, fHttpGetErrMsg) then
    begin
      Debug(dpMessage, section, Format('[FAILED] TPazoHTTPImdbTask mainpage --> %s ', [fHttpGetErrMsg]));
      irc_Adderror(Format('<c4>[FAILED]</c> TPazoHTTPImdbTask mainpage --> %s', [fHttpGetErrMsg]));
      Result := True;
      ready := True;
      exit;
    end;

    (* Fetch MovieTitle/Extra/Year from iMDB *)
    THtmlIMDbParser.ParseMetaTitleInformation(mainsite, imdb_mtitle, imdb_extra, imdb_year);

    imdbdata := TDbImdbData.Create(imdb_id);

    imdbdata.imdb_origtitle := imdb_mtitle;

    (* Fetch Votes and Rating from iMDB *)
    THtmlIMDbParser.ParseVotesAndRating(mainsite, imdb_votes, imdb_rating);
    // TODO: what to do in error case? StrToIntDef in function?
    imdbdata.imdb_votes := StrToIntDef(imdb_votes, -5);
    imdbdata.imdb_rating := StrToIntDef(imdb_rating, -5);

    (* Fetch Languages from iMDB *)
    THtmlIMDbParser.ParseMovieLanguage(mainsite, imdb_lang);
    imdbdata.imdb_languages.CommaText := imdb_lang;

    (* Fetch Countries from iMDB *)
    THtmlIMDbParser.ParseMovieCountries(mainsite, imdb_countr);
    imdbdata.imdb_countries.CommaText := imdb_countr;

    (* Fetch Genres from iMDB *)
    THtmlIMDbParser.ParseMovieGenres(mainsite, imdb_genr);
    imdbdata.imdb_genres.CommaText := imdb_genr;

    rr2 := TRegexpr.Create;
    try
      rr2.ModifierI := True;

      //irc_addtext('CONSOLE','ADMIN','LANGUAGE = %s -- rlang = %s',[ir.languages.text,rlang]);

      (* Get Cleanup STV Infos - mod done by a kraut so u see we can do beauty things too ;) - *)
      ir.imdb_stvs := '/!\ UNTOUCHED /!\';
      ir.imdb_stvm := True;
      imdb_stv := False;
      imdb_country := '';
      imdb_counline := '';
      imdb_region := '';

      if (ir.language <> 'English') then
        rlang := ir.language
      else
        rlang := 'USA';

      //imdb_counline := imdbcountries.ReadString('COMMON', rlang, '');
      imdb_region := SubString(imdb_counline, ',', 1); // TODO: check if regex is still needed and thus the variable
      imdb_country := TMapLanguageCountry.GetCountrynameByLanguage(rlang);

      (* Movie is actually a MiniSeries designed for Television *)
      if imdb_extra = 'TV mini-series' then
      begin
        imdbdata.imdb_stvm := True;
        imdb_stv := True;
        imdbdata.imdb_stvs := 'Mini_series';
      end;

      (* Movie is actually a Videogame, STV or TV Production *)
      if not imdb_stv then
      begin
        if ( AnsiContainsText(imdb_extra, 'TV') or AnsiContainsText(imdb_extra, 'Video') ) then
        begin
          imdbdata.imdb_stvm := True;
          imdb_stv := True;
          imdbdata.imdb_stvs := 'Videogame_TV_Video';
        end;
      end;

      imdb_date := '';

      (* Get STV Info through releaseinfo page from iMDB *)
      if not HttpGetUrl('https://www.imdb.com/title/' + imdb_id + '/releaseinfo', rlsdatesite, fHttpGetErrMsg) then
      begin
        Debug(dpMessage, section, Format('[FAILED] TPazoHTTPImdbTask releaseinfo --> %s ', [fHttpGetErrMsg]));
        irc_Adderror(Format('<c4>[FAILED]</c> TPazoHTTPImdbTask releaseinfo --> %s', [fHttpGetErrMsg]));
        Result := True;
        ready := True;
        exit; // TODO: skip releaseinfo webpage crawl if failed instead of stoping complete imdb parsing task
      end;

      if not imdb_stv then
      begin
        rr.ModifierI := True;

        // Examples:
        //          <tr class="even"><td><a href="/calendar/?region=my&ref_=ttrel_rel_2">Malaysia</a></td><td class="release_date">28 September 2017</td><td></td></tr>
        //          <tr class="odd"><td><a href="/calendar/?region=us&ref_=ttrel_rel_11">USA</a></td><td class="release_date">20 October 1983</td><td> (New York City, New York)</td></tr>
        //          <tr class="even"><td><a href="/calendar/?region=us&ref_=ttrel_rel_12">USA</a></td><td class="release_date">1 December 1983</td><td></td></tr>
        //          <tr class="odd"><td><a href="/calendar/?region=us&ref_=ttrel_rel_1">USA</a></td><td class="release_date">24 September 2017</td><td> (Beijing) (premiere)</td></tr>
        //          <tr class="even"><td><a href="/calendar/?region=us&ref_=ttrel_rel_32">USA</a></td><td class="release_date">18 December 2017</td><td> (internet)</td></tr>
        rr.Expression :=
        '<tr class="(odd|even)">[\s\n]*?<td><a href=\"\/calendar\/\?region\=' + imdb_region + '\&ref\_\=ttrel\_rel\_\d+"\s*>' + imdb_country
          + '<\/a><\/td>[\s\n]*?<td class="release_date">(.*?)<\/td>[\s\n]*?<td>(.*?)<\/td><\/tr>';
        if rr.Exec(rlsdatesite) then
        begin
          imdb_date := Trim(rr.Match[2]);
          fRlsdateExtraInfo := Trim(rr.Match[3]);
          imdbdata.imdb_stvs := 'Cinedate: ' + imdb_date;
          imdbdata.imdb_stvm := False;
          imdb_stv := False;
          (* Fetching Cinedate for imdb_country *)
          imdbdata.imdb_cineyear :=  StrToIntDef(copy(imdb_date, Length(imdb_date) - 4, 4), -1);

          if fRlsdateExtraInfo <> '' then
          begin
            rr2.ModifierI := True;
            rr2.Expression := '(DVD|video|TV|Bluray|Blueray)(\s|\.|\-)?premiere';
            if rr2.Exec(fRlsdateExtraInfo) then
            begin
              imdbdata.imdb_stvs := Format('%s, %s [%s]', [imdb_country, imdb_date, fRlsdateExtraInfo]); // USA, 5 December 2005 [(New York City, New York) (premiere)]
              imdb_stv := True;
            end;
            (*  Fetching Festival infos for imdb_country  *)
            rr2.Expression := 'F(estival|ilmfest|est|ilm(\s|\.|\-)?Market?)';
            if rr2.Exec(fRlsdateExtraInfo) then
            begin
              imdbdata.imdb_festival := True;
            end;
          end;
        end;
      end;

      s := '0';
      imdb_screens := 0;
      fBOMSearchNeeded := True;

      imdbdata.imdb_screens := imdb_screens;
      imdbdata.imdb_wide := False;
      imdbdata.imdb_ldt := False;

      // if we get values for season or episode, it's a tv show which don't has any screens
      getShowValues(rls, showname, season, episode);
      if not ((season > 0) or (episode > 0) or (season = Ord(tvDatedShow)) or (season = Ord(tvRegularSerieWithoutSeason)) or (episode = Ord(tvNoEpisodeTag))) then
      begin

        rr.Expression := '<h\d?.*?>Box\s*Office<\/h\d?>(.*?)<hr\s*\/>';
        if rr.Exec(mainsite) then
        begin
          fBusinessInfoPart := rr.Match[1];

          // maybe check first if it list: Opening Weekend USA or other relevant countries

          //Debug(dpError, section, Format('IMDb Box Office: %s', [fBusinessInfoPart]));

          // check what kind of Box Office info we have
          if AnsiContainsText(fBusinessInfoPart, 'Wide Release') then
          begin
            imdb_stv := False;
            imdbdata.imdb_stvs := 'Wide Release';
            imdbdata.imdb_wide := True;
            imdbdata.imdb_ldt := False;
            fBOMSearchNeeded := False;
          end
          else if AnsiContainsText(fBusinessInfoPart, 'Limited') then
          begin
            imdb_stv := False;
            imdbdata.imdb_stvs := 'Limited';
            imdbdata.imdb_wide := False;
            imdbdata.imdb_ldt := True;
            fBOMSearchNeeded := False;
          end
          else if AnsiContainsText(fBusinessInfoPart, 'Gross') then
          begin
            imdb_stv := False;
            imdbdata.imdb_stvs := 'Gross weight found, so its not STV!';
            imdbdata.imdb_wide := False;
            imdbdata.imdb_ldt := False;
            fBOMSearchNeeded := True; // better to a BOM check
          end
          else
          begin
            imdb_stv := True;
            imdbdata.imdb_stvs := 'Box Office found, but not handled!';
            imdbdata.imdb_wide := False;
            imdbdata.imdb_ldt := False;
            fBOMSearchNeeded := True; // BOM check needed!
          end;
        end
        else
        begin
          imdb_stv := True;
          imdbdata.imdb_stvs := 'No Box Office found, so its STV!';
          imdbdata.imdb_wide := False;
          imdbdata.imdb_ldt := False;
          fBOMSearchNeeded := True;
        end;

        if config.ReadBool('dbaddimdb', 'parse_boxofficemojo_always', False) then
        begin
          fBOMSearchNeeded := True;
        end;

        if fBOMSearchNeeded then
        begin
          if not HttpGetUrl('https://www.boxofficemojo.com/title/' + imdb_id + '/', bomsite, fHttpGetErrMsg) then
          begin
            Debug(dpMessage, section, Format('[FAILED] TPazoHTTPImdbTask BoxOfficeMojo --> %s ', [fHttpGetErrMsg]));
            irc_Adderror(Format('<c4>[FAILED]</c> TPazoHTTPImdbTask BoxOfficeMojo --> %s', [fHttpGetErrMsg]));
            Result := True;
            ready := True;
            exit; // TODO: skip boxofficemojo webpage crawl if failed instead of stoping complete imdb parsing task
          end;

          fBOMCountryLinks := TDictionary<String, String>.Create;
          try
            THtmlBoxOfficeMojoParser.GetCountrySpecificLinks(bomsite, fBOMCountryLinks);

            fBOMCountryScreens := TDictionary<String, Integer>.Create;
            try
              for fBOMCountryLinkPair in fBOMCountryLinks do
              begin
                // all links on original release page have this reference
                if not HttpGetUrl('https://www.boxofficemojo.com' + fBOMCountryLinkPair.Value + '?ref_=bo_gr_rls', bomsite, fHttpGetErrMsg) then
                begin
                  Debug(dpMessage, section, Format('[FAILED] TPazoHTTPImdbTask BoxOfficeMojo --> %s ', [fHttpGetErrMsg]));
                  irc_Adderror(Format('<c4>[FAILED]</c> TPazoHTTPImdbTask BoxOfficeMojo --> %s', [fHttpGetErrMsg]));
                  Result := True;
                  ready := True;
                  exit; // TODO: skip boxofficemojo webpage crawl if failed instead of stoping complete imdb parsing task
                end;

                fBOMCountryScreens.Add(fBOMCountryLinkPair.Key, THtmlBoxOfficeMojoParser.GetWidestScreensCount(bomsite));
              end;

              if not fBOMCountryScreens.TryGetValue('USA', imdb_screens) then
                imdb_screens := -10;
            finally
              fBOMCountryScreens.Free;
            end;
          finally
            fBOMCountryLinks.Free;
          end;

          imdbdata.imdb_screens := imdb_screens;

          imdbdata.imdb_wide := False;
          imdbdata.imdb_ldt := False;

          if (imdbdata.imdb_screens > 599) then
          begin
            imdbdata.imdb_wide := True;
            imdbdata.imdb_ldt := False;
          end
          else
          begin
            imdbdata.imdb_wide := False;
            imdbdata.imdb_ldt := True;
          end;

          if rlang = 'USA' then
          begin
            if imdbdata.imdb_screens = 0 then
            begin
              imdb_stv := True;
              imdbdata.imdb_stvs := 'USA and zero screens = STV!';
              imdbdata.imdb_wide := False;
              imdbdata.imdb_ldt := False;

              //Sometimes imdb box office has no screens for cine stuff, so we need to be tricky ;) yay i love that game :)
              rr.Expression :=
                '<a href="\/title\/tt\d+\/business\?ref_=.*?"[\r\n\s]+class=\"quicklink quicklinkGray\" >Box Office\/Business<\/a>';
              if not rr.Exec(mainsite) then
              begin
                imdb_stv := True;
                imdbdata.imdb_stvs := 'No Link to business found, so its STV!';
                imdbdata.imdb_wide := False;
                imdbdata.imdb_ldt := False;
              end;

              rr.Expression :=
                '<h\d?.*?>Box\s*Office<\/h\d?>(.*?)<hr\s*\/>';
              if not rr.Exec(mainsite) then
              begin
                imdb_stv := True;
                imdbdata.imdb_stvs := 'No Box Office found, so its STV!';
                imdbdata.imdb_wide := False;
                imdbdata.imdb_ldt := False;
              end;
            end
            else
            begin
              imdb_stv := False;
              imdbdata.imdb_stvs := 'USA with screens can''t be STV!';
            end;
          end;

        end;

      end;

    finally
      rr2.free;
    end;
  finally
    rr.free;
  end;


  imdbdata.imdb_id := imdb_id;
  imdbdata.imdb_year := imdb_year;
  imdbdata.imdb_stvm := imdb_stv;
  mainsite := '';
  rlsdatesite := '';
  businesssite := '';
  bomsite := '';

  ir.Free;

  try
    dbaddimdb_SaveImdbData(rls, imdbdata);
  except
    on e: Exception do
    begin
      Debug(dpError, section,
        Format('[EXCEPTION] TPazoHTTPImdbTask dbaddimdb_SaveImdb: %s ',
        [e.Message]));
    end;
  end;

  ready := True;
  Result := True;
end;

function TPazoHTTPImdbTask.Name: String;
begin
  try
    Result := Format('HTTPImdb %s : %s', [rls, imdb_id]);
  except
    Result := 'HTTPImdb';
  end;
end;

destructor TPazoHTTPImdbTask.Destroy;
begin
  inherited;
end;

end.

