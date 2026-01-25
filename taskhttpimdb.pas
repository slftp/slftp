unit taskhttpimdb;

interface

uses
  tasksunit, Generics.Collections, Variants, dbaddimdb, Classes;

type
  { @abstract(Extracts screen count information from Box Office Mojo HTML) }
  THtmlBoxOfficeMojoParser = class
  public
    { Parses countries (only includes one from slftp.imdbcountries) and links from the title overview page @br @note(Domestic gets renamed to USA, United Kingdom gets renamed to UK)
      @param(aPageSource Webpage HTML sourcecode)
      @param(aCountryLinks Countryname and link to the release information) }
    class procedure GetCountrySpecificLinks(const aPageSource: String; out aCountryLinks: TDictionary<String, String>);

    { Finds "Original Release" group link (used when main page doesn't have USA/Domestic)
      @param(aPageSource Webpage HTML sourcecode)
      @returns(Link to Original Release group, or empty string if not found) }
    class function GetOriginalReleaseGroupLink(const aPageSource: String): String;

    { Parses theaters screens of widest release
      @param(aPageSource Webpage HTML sourcecode)
      @returns(Screens count of widest release (0 if nothing found)) }
    class function GetWidestScreensCount(const aPageSource: String): Integer;
  end;

  { @abstract(Processes IMDb JSON data into TDbImdbData structure) }
  TImdbDataProcessor = class
  public
    class function Process(const aReleaseName, aImdbId: String; const aTitleJson, aReleaseDatesJson: Variant; const aBomScreenCounts: TDictionary<String, Integer>; out aImdbData: TDbImdbData): String;
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
  SysUtils, irc, StrUtils, debugunit, dateutils, configunit, kb, kb.releaseinfo,
  sitesunit, mystrings, dbtvinfo, sllanguagebase, mormot.core.variants,
  imdbapi, http, RegExpr;

const
  section = 'taskhttpimdb';

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

        aCountryLinks.AddOrSetValue(fCountry, fLink);
      until not rr.ExecNext;
    end;
  finally
    rr.Free;
  end;
end;

class function THtmlBoxOfficeMojoParser.GetOriginalReleaseGroupLink(const aPageSource: String): String;
var
  rr: TRegExpr;
begin
  Result := '';
  rr := TRegExpr.Create;
  try
    rr.ModifierI := True;
    // Look for "Original Release" group link
    rr.Expression := '<a class="a-link-normal" href="(\/releasegroup\/gr\d+).*?">Original Release<\/a>';

    if rr.Exec(aPageSource) then
      Result := Trim(rr.Match[1]);
  finally
    rr.Free;
  end;
end;

class function THtmlBoxOfficeMojoParser.GetWidestScreensCount(const aPageSource: String): Integer;
var
  rr: TRegExpr;
  fScreensCount: String;
begin
  Result := 0;
  rr := TRegExpr.Create;
  try
    rr.ModifierI := True;
    rr.Expression := '<div[^>]*><span>Widest Release<\/span><span>([0-9,]*) theaters<\/span><\/div>';

    if rr.Exec(aPageSource) then
    begin
      fScreensCount := rr.Match[1];
      fScreensCount := StringReplace(fScreensCount, ',', '', [rfReplaceAll, rfIgnoreCase]);
      Result := StrToIntDef(fScreensCount, 0);
    end;
  finally
    rr.Free;
  end;
end;

{ TImdbDataProcessor }

class function TImdbDataProcessor.Process(const aReleaseName, aImdbId: String; const aTitleJson, aReleaseDatesJson: Variant; const aBomScreenCounts: TDictionary<String, Integer>; out aImdbData: TDbImdbData): String;
var
  fImdbOriginalTitle: String;
  fImdbTitleExtraInfo: String;
  fImdbVotes: Integer;
  fImdbRating: Integer;
  
  fIsSTV: Boolean;
  fIsLimited: Boolean;
  fIsWide: Boolean;
  fIsFestival: Boolean;
  fStatusReason: String;
  fStatusReasonList: TList<String>;
  
  fImdbCineYear: Integer;
  fImdbReleaseDate: String;
  
  fTvShowname: String;
  fTvSeason: Integer;
  fTvEpisode: Int64;
  
  fLanguageFromReleasename: String;
  fStrHelper: String;
  fReleasenameCountry: String;
  
  i, j: Integer;
  fVariant, fVariant2: Variant;
  fCountryName, fLanguageName, fAttributesStr: String;
begin
  aImdbData := TDbImdbData.Create(aImdbId);
  fImdbCineYear := 0;
  Result := ''; // Status reason

  // Parse Main Data
  if not VarIsNull(aTitleJson) then
  begin
    fVariant := TDocVariantData(aTitleJson).GetValueOrNull('originalTitle');
    if not VarIsNull(fVariant) then
      fImdbOriginalTitle := fVariant
    else
    begin
      fVariant := TDocVariantData(aTitleJson).GetValueOrNull('primaryTitle');
      if not VarIsNull(fVariant) then
        fImdbOriginalTitle := fVariant;
    end;
      
    // 'type' is a keyword, use explicit access
    // Try titleType first (preferred), fallback to type
    fVariant := TDocVariantData(aTitleJson).GetValueOrNull('titleType');
    if VarIsNull(fVariant) then
      fVariant := TDocVariantData(aTitleJson).GetValueOrNull('type');
    if not VarIsNull(fVariant) then
      fImdbTitleExtraInfo := fVariant; 
      
    fVariant := TDocVariantData(aTitleJson).GetValueOrNull('startYear');
    if not (VarIsNull(fVariant) or VarIsEmpty(fVariant)) then
      aImdbData.imdb_year := fVariant
    else
      aImdbData.imdb_year := 0;
      
    // Rating & Votes
    fVariant := TDocVariantData(aTitleJson).GetValueOrNull('rating');
    if not VarIsNull(fVariant) then
    begin
      fVariant2 := TDocVariantData(fVariant).GetValueOrNull('voteCount');
      if not VarIsNull(fVariant2) then
        fImdbVotes := fVariant2
      else
        fImdbVotes := 0;
        
      fVariant2 := TDocVariantData(fVariant).GetValueOrNull('aggregateRating');
      if not VarIsNull(fVariant2) then
        fImdbRating := Round(Double(fVariant2) * 10) // 7.5 -> 75
      else
        fImdbRating := 0;
    end
    else
    begin
      fImdbVotes := 0;
      fImdbRating := 0;
    end;
    
    // Genres
    aImdbData.imdb_genres.Clear;
    fVariant := TDocVariantData(aTitleJson).GetValueOrNull('genres');
    if not VarIsNull(fVariant) then
    begin
      for i := 0 to TDocVariantData(fVariant).Count - 1 do
      begin
        fStrHelper := VarToStr(TDocVariantData(fVariant).Values[i]);
        if fStrHelper <> '' then
          aImdbData.imdb_genres.Add(fStrHelper);
      end;
    end;
    
    // Countries
    aImdbData.imdb_countries.Clear;
    fVariant := TDocVariantData(aTitleJson).GetValueOrNull('originCountries');
    if not VarIsNull(fVariant) then
    begin
      for i := 0 to TDocVariantData(fVariant).Count - 1 do
      begin
        fVariant2 := TDocVariantData(fVariant).Values[i];
        fStrHelper := TDocVariantData(fVariant2).GetValueOrNull('name');
        if fStrHelper <> '' then
        begin
          fCountryName := fStrHelper;
          if fCountryName = 'United States' then fCountryName := 'USA'
          else if fCountryName = 'United Kingdom' then fCountryName := 'UK'
          else if Pos('Hong Kong', fCountryName) > 0 then fCountryName := 'Hong Kong'
          else if Pos('(', fCountryName) > 0 then
          begin
            // Remove everything in parentheses (e.g., "Taiwan (Province of China)" -> "Taiwan")
            fCountryName := Trim(Copy(fCountryName, 1, Pos('(', fCountryName) - 1));
          end;
          aImdbData.imdb_countries.Add(fCountryName);
        end;
      end;
    end;
    
    // Languages
    aImdbData.imdb_languages.Clear;
    fVariant := TDocVariantData(aTitleJson).GetValueOrNull('spokenLanguages');
    if not VarIsNull(fVariant) then
    begin
      for i := 0 to TDocVariantData(fVariant).Count - 1 do
      begin
        fVariant2 := TDocVariantData(fVariant).Values[i];
        fStrHelper := TDocVariantData(fVariant2).GetValueOrNull('name');
        if fStrHelper <> '' then
        begin
          fLanguageName := fStrHelper;
          // Simplify language variants to base language
          if Pos('Chinese', fLanguageName) > 0 then fLanguageName := 'Chinese'
          else if Pos('Arabic', fLanguageName) > 0 then fLanguageName := 'Arabic'
          else if Pos('Spanish', fLanguageName) > 0 then fLanguageName := 'Spanish'
          else if Pos('Portuguese', fLanguageName) > 0 then fLanguageName := 'Portuguese'
          else if Pos('French', fLanguageName) > 0 then fLanguageName := 'French'
          else if Pos('German', fLanguageName) > 0 then fLanguageName := 'German';
          aImdbData.imdb_languages.Add(fLanguageName);
        end;
      end;
    end;
  end;

  fIsSTV := False;
  fIsLimited := False;
  fIsWide := False; // Can no longer be determined without screen counts
  fIsFestival := False;
  
  fStatusReasonList := TList<String>.Create;
  try
    // Logic 1: Type based STV check (Case-Insensitive)
    // API returns camelCase: tvMovie, videoGame, tvSpecial, video
    // Note: short, tvShort, tvEpisode are not exposed by imdbapi.dev but checked for safety
    if SameText(fImdbTitleExtraInfo, 'tvMovie') or
       SameText(fImdbTitleExtraInfo, 'video') or
       SameText(fImdbTitleExtraInfo, 'tvSpecial') or
       SameText(fImdbTitleExtraInfo, 'short') or
       SameText(fImdbTitleExtraInfo, 'tvShort') or
       SameText(fImdbTitleExtraInfo, 'tvEpisode') then
    begin
      fIsSTV := True;
      fStatusReasonList.Add(Format('STV due to title type: %s', [fImdbTitleExtraInfo]));
    end
    else if SameText(fImdbTitleExtraInfo, 'videoGame') then
    begin
      fIsSTV := True;
      fStatusReasonList.Add('STV due to being a Video Game');
    end;

    // Logic 2: TV Show check
    getShowValues(aReleaseName, fTvShowname, fTvSeason, fTvEpisode);
    // If we detected it is a TV show release (has Season/Episode)
    if not ((fTvSeason > 0) or (fTvEpisode > 0) or (fTvSeason = Ord(tvDatedShow))
           or (fTvSeason = Ord(tvRegularSerieWithoutSeason)) or (fTvEpisode = Ord(tvNoEpisodeTag))) then
    begin
       // NO-OP: It means getShowValues failed to find TV tags.
       // Check if the IMDB type itself says it is a TV Series or TV Movie
       if SameText(fImdbTitleExtraInfo, 'tvSeries') or SameText(fImdbTitleExtraInfo, 'tvMiniSeries') or SameText(fImdbTitleExtraInfo, 'tvMovie') then
       begin
         fIsSTV := True;
         fStatusReasonList.Add(Format('STV due to being a TV show (%s)', [fImdbTitleExtraInfo]));
       end;
    end
    else
    begin
       // getShowValues returned true (it IS a TV release). 
       fIsSTV := True; 
       fStatusReasonList.Add(Format('STV due to being a TV show release (S%dE%d)', [fTvSeason, fTvEpisode]));
    end;
    
    // 2. Fetch Release Dates for CineYear/Festival/STV logic
    fIsFestival := False;
    if not VarIsNull(aReleaseDatesJson) then
    begin
        fLanguageFromReleasename := FindLanguageOnDirectory(aReleaseName);
        
        // Helper to map language (e.g. German) to Country (Germany)
        if (fLanguageFromReleasename = 'English') then
        begin
           if aImdbData.imdb_countries.IndexOf('USA') >= 0 then fStrHelper := 'USA'
           else if aImdbData.imdb_countries.IndexOf('UK') >= 0 then fStrHelper := 'UK'
           else fStrHelper := 'USA'; // Fallback
        end
        else
           fStrHelper := fLanguageFromReleasename;

        fReleasenameCountry := TMapLanguageCountry.GetCountrynameByLanguage(fStrHelper);
        
        fVariant := TDocVariantData(aReleaseDatesJson).GetValueOrNull('releaseDates');
        if (fReleasenameCountry <> '') and not VarIsNull(fVariant) then
        begin
             for i := 0 to TDocVariantData(fVariant).Count - 1 do
             begin
                fVariant2 := TDocVariantData(fVariant).Values[i];
                if VarIsNull(fVariant2.country) or VarIsNull(fVariant2.country.name) then Continue;
                
                fCountryName := fVariant2.country.name;
                
                // Rewrite USA/UK to match internal standard if needed
                if fCountryName = 'United States' then fCountryName := 'USA'
                else if fCountryName = 'United Kingdom' then fCountryName := 'UK';
                
                if fCountryName = fReleasenameCountry then
                begin
                   if not VarIsNull(fVariant2.releaseDate) then
                     fImdbReleaseDate := Format('%d-%d-%d', [Integer(fVariant2.releaseDate.year), Integer(fVariant2.releaseDate.month), Integer(fVariant2.releaseDate.day)]);
                   
                   fAttributesStr := '';
                   if not VarIsNull(fVariant2.attributes) then
                   begin
                     for j := 0 to TDocVariantData(fVariant2.attributes).Count - 1 do
                        fAttributesStr := fAttributesStr + VarToStr(TDocVariantData(fVariant2.attributes).Values[j]) + ' ';
                   end;
                   fAttributesStr := Trim(fAttributesStr);
                   
                   // Check STV in attributes
                   if (Pos('video premiere', LowerCase(fAttributesStr)) > 0) or 
                      (Pos('tv premiere', LowerCase(fAttributesStr)) > 0) or
                      (Pos('dvd premiere', LowerCase(fAttributesStr)) > 0) then
                   begin
                      fIsSTV := True;
                      fStatusReasonList.Add(Format('STV in %s due to %s on %s', [fReleasenameCountry, fAttributesStr, fImdbReleaseDate]));
                   end;
                   
                   // Check Festival
                   if (Pos('festival', LowerCase(fAttributesStr)) > 0) then
                   begin
                      fIsFestival := True;
                      fStatusReasonList.Add(Format('Festival in %s due to %s on %s', [fReleasenameCountry, fAttributesStr, fImdbReleaseDate]));
                   end;
                   
                   // Check CineYear (First theatrical release)
                   if (not fIsSTV) and (not fIsFestival) and (fImdbCineYear = 0) and not VarIsNull(fVariant2.releaseDate) then
                   begin
                      fImdbCineYear := fVariant2.releaseDate.year;
                      fStatusReasonList.Add(Format('Cine year for %s is %d taken from %s (Attributes: %s)', [fReleasenameCountry, fImdbCineYear, fImdbReleaseDate, fAttributesStr]));
                   end;
                end;
             end;
        end;
    end;

    // Screen counts from BOM (if available)
    if (aBomScreenCounts <> nil) then
    begin
      // Try release country first, then fall back to USA, then UK
      if not ((fReleasenameCountry <> '') and aBomScreenCounts.TryGetValue(fReleasenameCountry, aImdbData.imdb_screens)) then
      begin
        if not aBomScreenCounts.TryGetValue('USA', aImdbData.imdb_screens) then
          aBomScreenCounts.TryGetValue('UK', aImdbData.imdb_screens);
      end;

      // Apply classification thresholds based on screen count (always use USA/UK thresholds since we fetch those)
      if aImdbData.imdb_screens = 0 then
      begin
        fIsSTV := True;
        fStatusReasonList.Add('STV due to zero screens');
      end
      else if aImdbData.imdb_screens > 0 then
      begin
        if aImdbData.imdb_screens >= 500 then
        begin
          fIsWide := True;
          fStatusReasonList.Add(Format('Wide due to %d screens', [aImdbData.imdb_screens]));
        end
        else if aImdbData.imdb_screens >= 250 then
        begin
          fIsLimited := True;
          fStatusReasonList.Add(Format('Limited due to %d screens', [aImdbData.imdb_screens]));
        end;
      end
      else
      begin
        // Screen count not found
        aImdbData.imdb_screens := -1;
      end;
    end
    else
    begin
      // BOM not used
      aImdbData.imdb_screens := -1;
    end;

    // Status String Construction
    for i := 0 to fStatusReasonList.Count - 1 do
    begin
      fStatusReason := fStatusReason + Format('%d - %s%s', [i + 1, fStatusReasonList[i], #13#10]);
    end;

  finally
    fStatusReasonList.Free;
  end;

  aImdbData.imdb_rating := fImdbRating;
  aImdbData.imdb_votes := fImdbVotes;
  aImdbData.imdb_cineyear := fImdbCineYear;
  aImdbData.imdb_ldt := fIsLimited;
  aImdbData.imdb_wide := fIsWide;
  aImdbData.imdb_festival := fIsFestival;
  aImdbData.imdb_stvm := fIsSTV;
  aImdbData.imdb_stvs := fStatusReason;
  aImdbData.imdb_type := fImdbTitleExtraInfo;
  aImdbData.imdb_origtitle := fImdbOriginalTitle;
  
  Result := fStatusReason;
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
  fTitleJson: Variant;
  fReleaseDatesJson: Variant;
  fBomScreenCounts: TDictionary<String, Integer>;
  fBomCountryLinks: TDictionary<String, String>;
  fBomCountryLinkPair: TPair<String, String>;
  fBomMainPage: String;
  fBomCountryPage: String;
  fBomGroupLink: String;
  fBomGroupPage: String;
  fHttpGetErrMsg: String;
  fParseBOM: Boolean;
begin
  Result := False;
  fBomScreenCounts := nil;

  // 1. Fetch Main Title Data from API
  if not TImdbApi.GetTitle(FImdbTitleID, fTitleJson) then
  begin
    irc_Adderror(Format('<c4>[FAILED]</c> Unable to fetch JSON for %s from IMDb API', [FImdbTitleID]));
    ready := True;
    Result := True;
    Exit;
  end;

  // 2. Fetch Release Dates from API
  if not TImdbApi.GetReleaseDates(FImdbTitleID, fReleaseDatesJson) then
    fReleaseDatesJson := Null;

  // 3. Optionally fetch BOM screen counts (if enabled in config)
  fParseBOM := config.ReadBool('dbaddimdb', 'parse_boxofficemojo_always', True);
  Debug(dpMessage, section, Format('[BOM] parse_boxofficemojo_always=%s for %s', [BoolToStr(fParseBOM, True), FImdbTitleID]));
  if fParseBOM then
  begin
    Debug(dpMessage, section, Format('[BOM] Starting BOM fetch for %s', [FImdbTitleID]));
    try
      if HttpGetUrl(Format('https://www.boxofficemojo.com/title/%s/', [FImdbTitleID]), fBomMainPage, fHttpGetErrMsg) then
      begin
        fBomCountryLinks := TDictionary<String, String>.Create;
        try
          THtmlBoxOfficeMojoParser.GetCountrySpecificLinks(fBomMainPage, fBomCountryLinks);
          Debug(dpMessage, section, Format('[BOM] Found %d country links on main page for %s (USA=%s, UK=%s)', [fBomCountryLinks.Count, FImdbTitleID, BoolToStr(fBomCountryLinks.ContainsKey('USA'), True), BoolToStr(fBomCountryLinks.ContainsKey('UK'), True)]));

          // If USA/UK not found on main page, try "Original Release" group
          if not (fBomCountryLinks.ContainsKey('USA') or fBomCountryLinks.ContainsKey('UK')) then
          begin
            Debug(dpMessage, section, Format('[BOM] USA/UK not found on main page, checking for Original Release group for %s', [FImdbTitleID]));
            fBomGroupLink := THtmlBoxOfficeMojoParser.GetOriginalReleaseGroupLink(fBomMainPage);

            if fBomGroupLink <> '' then
            begin
              Debug(dpMessage, section, Format('[BOM] Found Original Release group: %s', [fBomGroupLink]));

              if HttpGetUrl('https://www.boxofficemojo.com' + fBomGroupLink, fBomGroupPage, fHttpGetErrMsg) then
              begin
                // Parse countries from Original Release group and add to existing dictionary
                THtmlBoxOfficeMojoParser.GetCountrySpecificLinks(fBomGroupPage, fBomCountryLinks);
                Debug(dpMessage, section, Format('[BOM] After Original Release group: total %d country links', [fBomCountryLinks.Count]));
              end
              else
              begin
                Debug(dpMessage, section, Format('[BOM] Failed to fetch Original Release group page: %s', [fHttpGetErrMsg]));
              end;
            end
            else
            begin
              Debug(dpMessage, section, '[BOM] No Original Release group found');
            end;
          end;

          if fBomCountryLinks.Count > 0 then
          begin
            fBomScreenCounts := TDictionary<String, Integer>.Create;

            // Only fetch USA and UK screen counts (all we need for Wide/Limited classification)
            if fBomCountryLinks.ContainsKey('USA') then
            begin
              if HttpGetUrl('https://www.boxofficemojo.com' + fBomCountryLinks['USA'] + '?ref_=bo_gr_rls', fBomCountryPage, fHttpGetErrMsg) then
              begin
                fBomScreenCounts.AddOrSetValue('USA', THtmlBoxOfficeMojoParser.GetWidestScreensCount(fBomCountryPage));
                Debug(dpMessage, section, Format('[BOM] USA: %d screens', [fBomScreenCounts['USA']]));
              end;
            end;

            if fBomCountryLinks.ContainsKey('UK') then
            begin
              if HttpGetUrl('https://www.boxofficemojo.com' + fBomCountryLinks['UK'] + '?ref_=bo_gr_rls', fBomCountryPage, fHttpGetErrMsg) then
              begin
                fBomScreenCounts.AddOrSetValue('UK', THtmlBoxOfficeMojoParser.GetWidestScreensCount(fBomCountryPage));
                Debug(dpMessage, section, Format('[BOM] UK: %d screens', [fBomScreenCounts['UK']]));
              end;
            end;

            Debug(dpMessage, section, Format('[BOM] Fetched screen counts for %d countries for %s', [fBomScreenCounts.Count, FImdbTitleID]));
          end
          else
          begin
            Debug(dpMessage, section, Format('[BOM] No matching countries found on BOM for %s', [FImdbTitleID]));
          end;
        finally
          fBomCountryLinks.Free;
        end;
      end
      else
      begin
        Debug(dpMessage, section, Format('[BOM] Failed to fetch main page for %s: %s', [FImdbTitleID, fHttpGetErrMsg]));
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] BOM scraping failed for %s: %s', [FImdbTitleID, e.Message]));
        if fBomScreenCounts <> nil then
          FreeAndNil(fBomScreenCounts);
      end;
    end;
  end;

  // 4. Process with optional BOM data
  try
    TImdbDataProcessor.Process(FReleaseName, FImdbTitleID, fTitleJson, fReleaseDatesJson, fBomScreenCounts, imdbdata);
  finally
    if fBomScreenCounts <> nil then
      fBomScreenCounts.Free;
  end;

  // 5. Save
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
    Result := Format('API IMDb for %s : ID %s', [FReleaseName, FImdbTitleID]);
  except
    Result := 'API IMDb';
  end;
end;

destructor TPazoHTTPImdbTask.Destroy;
begin
  inherited;
end;

end.
