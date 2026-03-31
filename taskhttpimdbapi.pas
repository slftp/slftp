unit taskhttpimdbapi;

interface

uses
  tasksunit, taskhttpimdb, Generics.Collections, Variants, mormot.core.variants;

type
  { @abstract(IMDB API data structure for movie information) }
  TImdbApiMovieData = class
  private
    FId: String;
    FTitle: String;
    FYear: Integer;
    FRating: Integer; // multiplied by 10 (e.g. 8.5 -> 85)
    FVotes: Integer;
    FGenres: String; // comma separated
    FLanguages: String; // comma separated
    FCountries: String; // comma separated
    FType: String; // movie, tvSeries, etc.
  public
    property Id: String read FId write FId;
    property Title: String read FTitle write FTitle;
    property Year: Integer read FYear write FYear;
    property Rating: Integer read FRating write FRating;
    property Votes: Integer read FVotes write FVotes;
    property Genres: String read FGenres write FGenres;
    property Languages: String read FLanguages write FLanguages;
    property Countries: String read FCountries write FCountries;
    property MovieType: String read FType write FType;
  end;

  { @abstract(Parser for IMDB API JSON responses) }
  TImdbApiParser = class
  public
    { Searches for a title using the IMDB API
      @param(aQuery Search query (release name))
      @param(aYear Optional year filter)
      @returns(IMDB ID if found, empty string otherwise) }
    class function SearchTitle(const aQuery: String; aYear: Integer = 0): String;

    { Gets detailed movie information from IMDB API
      @param(aImdbId IMDB ID (tt1234567))
      @returns(TImdbApiMovieData object with movie info, nil if not found) }
    class function GetMovieDetails(const aImdbId: String): TImdbApiMovieData;

    { Converts API genre array to comma-separated string
      @param(aGenresArray JSON array of genres)
      @returns(Comma-separated genre string) }
    class function ParseGenres(const aGenresArray: Variant): String;

    { Converts API language array to comma-separated string
      @param(aLanguagesArray JSON array of languages)
      @returns(Comma-separated language string) }
    class function ParseLanguages(const aLanguagesArray: Variant): String;

    { Converts API country array to comma-separated string
      @param(aCountriesArray JSON array of countries)
      @returns(Comma-separated country string) }
    class function ParseCountries(const aCountriesArray: Variant): String;
  end;

  { @abstract(Enhanced IMDB task with API support and fallback to scraping) }
  TPazoHTTPImdbApiTask = class(TTask)
  private
    FReleaseName: String;
    FImdbTitleID: String;
    FUseApiFirst: Boolean;
    FExecuted: Boolean; // Guard against multiple executions
  public
    constructor Create(const aImdbTitleID: String; const aReleaseName: String; aUseApiFirst: Boolean = True);
    destructor Destroy; override;
    function Execute(slot: Pointer): Boolean; override;
    function Name: String; override;
  private
    { Tries to get movie data via IMDB API
      @returns(True if successful, False if fallback needed) }
    function TryApiApproach: Boolean;
    
    { Falls back to original scraping method
      @returns(True if successful) }
    function FallbackToScraping: Boolean;

    { Determines if the release is STV (Straight-to-Video) based on movie type and release name
      @param(aMovieType Movie type from API)
      @param(aReleaseName Release name to check for TV patterns)
      @returns(True if STV, False if theatrical) }
    function DetermineSTVStatus(const aMovieType: String; const aReleaseName: String): Boolean;
  end;

implementation

uses
  SysUtils, irc, StrUtils, debugunit, configunit, kb, kb.releaseinfo, http,
  sitesunit, dbaddimdb, mystrings, mormot.core.base, mormot.core.json, Classes, dbtvinfo;

const
  section = 'taskhttpimdbapi';
  IMDB_API_BASE_URL = 'https://api.imdbapi.dev';

{ TImdbApiMovieData }

{ TImdbApiParser }

class function TImdbApiParser.SearchTitle(const aQuery: String; aYear: Integer = 0): String;
var
  fHttpResponse: String;
  fJsonDoc: Variant;
  fResults: Variant;
  fUrl: String;
  fErrorMsg: String;
  fBestMatch: Variant;
  i: Integer;
  fCurrentYear: Integer;
begin
  Result := '';
  
  try
    // Build search URL - using simple URL encoding
    fUrl := IMDB_API_BASE_URL + '/search/titles?query=' + StringReplace(aQuery, ' ', '%20', [rfReplaceAll]);
    if aYear > 0 then
      fUrl := fUrl + '&year=' + IntToStr(aYear);
    
    
    // Make HTTP request using http module with configurable retries
    if not HttpGetUrl(fUrl, fHttpResponse, fErrorMsg, config.ReadInteger(section, 'api_max_retries', 1)) then
    begin
      Debug(dpError, section, Format('Failed to connect to IMDB API: %s', [fErrorMsg]));
      Exit;
    end;
    
    // Parse JSON response
    fJsonDoc := _Json(fHttpResponse);
    if VarIsNull(fJsonDoc) then
    begin
      Debug(dpError, section, 'Invalid JSON response from IMDB API');
      Exit;
    end;
    
    fResults := fJsonDoc.results;
    if VarIsNull(fResults) or (VarType(fResults) and varArray = 0) then
    begin
      Debug(dpSpam, section, 'No results found in IMDB API response');
      Exit;
    end;
    
    // Find best match - prefer movies from specified year
    for i := 0 to VarArrayHighBound(fResults, 1) do
    begin
      fBestMatch := fResults[i];
      
      // Skip if not a movie/series
      if VarIsNull(fBestMatch.titleType) then
        Continue;
        
      // If year specified, prefer exact year match
      if aYear > 0 then
      begin
        if not VarIsNull(fBestMatch.year) then
        begin
          fCurrentYear := fBestMatch.year;
          if fCurrentYear = aYear then
          begin
            Result := fBestMatch.id;
            Debug(dpSpam, section, Format('Found exact year match: %s (%d)', [fBestMatch.title, fCurrentYear]));
            Exit;
          end;
        end;
      end
      else
      begin
        // No year specified, take first result
        Result := fBestMatch.id;
        Debug(dpSpam, section, Format('Found match: %s', [fBestMatch.title]));
        Exit;
      end;
    end;
    
    // If no exact year match found but results exist, take first result
    if (Result = '') and (VarArrayHighBound(fResults, 1) >= 0) then
    begin
      fBestMatch := fResults[0];
      Result := fBestMatch.id;
      Debug(dpSpam, section, Format('Using first result: %s', [fBestMatch.title]));
    end;
    
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('Exception in SearchTitle: %s', [E.Message]));
      Result := '';
    end;
  end;
end;

class function TImdbApiParser.GetMovieDetails(const aImdbId: String): TImdbApiMovieData;
var
  fHttpResponse: String;
  fJsonDoc: Variant;
  fUrl: String;
  fErrorMsg: String;
begin
  Result := nil;
  
  try
    // Build details URL
    fUrl := IMDB_API_BASE_URL + '/titles/' + aImdbId;
    
    // Make HTTP request using http module with configurable retries
    if not HttpGetUrl(fUrl, fHttpResponse, fErrorMsg, config.ReadInteger(section, 'api_max_retries', 1)) then
    begin
      Debug(dpError, section, Format('IMDB API request failed: %s', [fErrorMsg]));
      // Notify IRC about API failure
      irc_Addstats(Format('<c4>[iMDB API ERROR]</c> Failed to connect to IMDB API for <b>%s</b>: %s', [aImdbId, fErrorMsg]));
      Exit;
    end;
    
    // Parse JSON response
    fJsonDoc := _Json(fHttpResponse);
    if VarIsNull(fJsonDoc) then
    begin
      Debug(dpError, section, 'Invalid JSON response for movie details');
      // Notify IRC about JSON parsing failure
      irc_Addstats(Format('<c4>[iMDB API ERROR]</c> Invalid JSON response from IMDB API for <b>%s</b>', [aImdbId]));
      Exit;
    end;
    
    // Create result object
    Result := TImdbApiMovieData.Create;
    
    // Parse basic info
    Result.Id := aImdbId;
    if not VarIsNull(fJsonDoc.primaryTitle) then
      Result.Title := fJsonDoc.primaryTitle;
    if not VarIsNull(fJsonDoc.startYear) then
      Result.Year := fJsonDoc.startYear;
    
    // Access 'type' field using _Safe since 'type' is a reserved word
    if _Safe(fJsonDoc)^.Exists('type') then
    begin
      Result.MovieType := _Safe(fJsonDoc)^.Value['type'];
    end
    else
    begin
      Result.MovieType := '';
    end;
    
    // Parse rating (convert to integer * 10) - rating is nested object
    if not VarIsNull(fJsonDoc.rating) and not VarIsNull(fJsonDoc.rating.aggregateRating) then
      Result.Rating := Round(fJsonDoc.rating.aggregateRating * 10);
    
    // Parse votes - votes is nested in rating object
    if not VarIsNull(fJsonDoc.rating) and not VarIsNull(fJsonDoc.rating.voteCount) then
      Result.Votes := fJsonDoc.rating.voteCount;
    
    // Parse arrays with detailed logging
    if not VarIsNull(fJsonDoc.genres) then
    begin
      Result.Genres := ParseGenres(fJsonDoc.genres);
    end;
      
    if not VarIsNull(fJsonDoc.spokenLanguages) then
    begin
      Result.Languages := ParseLanguages(fJsonDoc.spokenLanguages);
    end;
      
    if not VarIsNull(fJsonDoc.originCountries) then
    begin
      Result.Countries := ParseCountries(fJsonDoc.originCountries);
    end;
    
    Debug(dpSpam, section, Format('Successfully parsed movie details for %s', [aImdbId]));
    
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('Exception in GetMovieDetails: %s', [E.Message]));
      FreeAndNil(Result);
    end;
  end;
end;

class function TImdbApiParser.ParseGenres(const aGenresArray: Variant): String;
var
  i: Integer;
  fGenreList: TStringList;
  fGenreItem: Variant;
  fArrayLen: Integer;
  fDocVariant: PDocVariantData;
begin
  Result := '';
  
  if VarIsNull(aGenresArray) then
  begin
    Exit;
  end;
  
  
  // Try to get DocVariant data pointer
  fDocVariant := _Safe(aGenresArray);
  if (fDocVariant = nil) or not fDocVariant^.IsArray then
  begin
    Exit;
  end;
  
  fArrayLen := fDocVariant^.Count;
  
  if fArrayLen <= 0 then
  begin
    Exit;
  end;
    
  fGenreList := TStringList.Create;
  try
    for i := 0 to fArrayLen - 1 do
    begin
      try
        fGenreItem := fDocVariant^.Values[i];
        if not VarIsNull(fGenreItem) then
        begin
          fGenreList.Add(VarToStr(fGenreItem));
        end
        else
        begin
        end;
      except
        on E: Exception do
        begin
          Debug(dpError, section, Format('[PARSEGENRES][%d] *** EXCEPTION *** accessing item: %s', [i, E.Message]));
        end;
      end;
    end;
    
    // Use DelimitedText to avoid quotes around genre names
    fGenreList.Delimiter := ',';
    fGenreList.QuoteChar := #0;  // Disable quoting
    Result := fGenreList.DelimitedText;
  finally
    fGenreList.Free;
  end;
  
end;

class function TImdbApiParser.ParseLanguages(const aLanguagesArray: Variant): String;
var
  i: Integer;
  fLangList: TStringList;
  fLangItem: Variant;
  fArrayLen: Integer;
  fDocVariant: PDocVariantData;
begin
  Result := '';
  Debug(dpSpam, section, '[PARSELANGUAGES] === PARSE LANGUAGES START ===');
  
  if VarIsNull(aLanguagesArray) then
  begin
    Debug(dpError, section, '[PARSELANGUAGES] *** ERROR *** Input is null');
    Exit;
  end;
  
  
  Debug(dpSpam, section, Format('[PARSELANGUAGES] Input type=%d', [VarType(aLanguagesArray)]));
  
  // Try to get DocVariant data pointer
  fDocVariant := _Safe(aLanguagesArray);
  if (fDocVariant = nil) or not fDocVariant^.IsArray then
  begin
    Debug(dpError, section, '[PARSELANGUAGES] *** ERROR *** Not a DocVariant array');
    Exit;
  end;
  
  fArrayLen := fDocVariant^.Count;
  Debug(dpSpam, section, Format('[PARSELANGUAGES] *** SUCCESS *** DocVariant array has %d items', [fArrayLen]));
  
  if fArrayLen <= 0 then
  begin
    Debug(dpError, section, '[PARSELANGUAGES] *** ERROR *** Array is empty');
    Exit;
  end;
    
  fLangList := TStringList.Create;
  try
    for i := 0 to fArrayLen - 1 do
    begin
      try
        fLangItem := fDocVariant^.Values[i];
        if not VarIsNull(fLangItem) then
        begin
          // Try different possible field names for language objects
          try
            if not VarIsNull(fLangItem.name) then
              fLangList.Add(fLangItem.name)
            else if not VarIsNull(fLangItem.primaryName) then
              fLangList.Add(fLangItem.primaryName)
            else if not VarIsNull(fLangItem.language) then
              fLangList.Add(fLangItem.language)
            else
              // If it's a simple string or fallback
              fLangList.Add(VarToStr(fLangItem));
          except
            on E: Exception do
            begin
              // Fallback to string representation
              fLangList.Add(VarToStr(fLangItem));
            end;
          end;
        end
        else
        begin
        end;
      except
        on E: Exception do
        begin
          Debug(dpError, section, Format('[PARSELANGUAGES][%d] *** EXCEPTION *** accessing item: %s', [i, E.Message]));
        end;
      end;
    end;
    
    // Use DelimitedText to avoid quotes around language names
    fLangList.Delimiter := ',';
    fLangList.QuoteChar := #0;  // Disable quoting
    Result := fLangList.DelimitedText;
  finally
    fLangList.Free;
  end;
  
end;

class function TImdbApiParser.ParseCountries(const aCountriesArray: Variant): String;
var
  i: Integer;
  fCountryList: TStringList;
  fCountryItem: Variant;
  fArrayLen: Integer;
  fDocVariant: PDocVariantData;
  fCountryName: String;
begin
  Result := '';
  Debug(dpSpam, section, '[PARSECOUNTRIES] === PARSE COUNTRIES START ===');
  
  if VarIsNull(aCountriesArray) then
  begin
    Debug(dpError, section, '[PARSECOUNTRIES] *** ERROR *** Input is null');
    Exit;
  end;
  
  
  Debug(dpSpam, section, Format('[PARSECOUNTRIES] Input type=%d', [VarType(aCountriesArray)]));
  
  // Try to get DocVariant data pointer
  fDocVariant := _Safe(aCountriesArray);
  if (fDocVariant = nil) or not fDocVariant^.IsArray then
  begin
    Debug(dpError, section, '[PARSECOUNTRIES] *** ERROR *** Not a DocVariant array');
    Exit;
  end;
  
  fArrayLen := fDocVariant^.Count;
  Debug(dpSpam, section, Format('[PARSECOUNTRIES] *** SUCCESS *** DocVariant array has %d items', [fArrayLen]));
  
  if fArrayLen <= 0 then
  begin
    Debug(dpError, section, '[PARSECOUNTRIES] *** ERROR *** Array is empty');
    Exit;
  end;
    
  fCountryList := TStringList.Create;
  try
    for i := 0 to fArrayLen - 1 do
    begin
      try
        fCountryItem := fDocVariant^.Values[i];
        if not VarIsNull(fCountryItem) then
        begin
          // Try different possible field names for country objects
          try
            if not VarIsNull(fCountryItem.name) then
              fCountryName := fCountryItem.name
            else if not VarIsNull(fCountryItem.primaryName) then
              fCountryName := fCountryItem.primaryName
            else if not VarIsNull(fCountryItem.country) then
              fCountryName := fCountryItem.country
            else
              // If it's a simple string or fallback
              fCountryName := VarToStr(fCountryItem);
              
            // Apply same country name conversions as original scraping code
            if fCountryName = 'United States' then
              fCountryName := 'USA'
            else if fCountryName = 'United Kingdom' then
              fCountryName := 'UK'
            else if fCountryName = 'Domestic' then
              fCountryName := 'USA';
              
            fCountryList.Add(fCountryName);
          except
            on E: Exception do
            begin
              // Fallback to string representation
              fCountryName := VarToStr(fCountryItem);
              // Apply conversions even for fallback
              if fCountryName = 'United States' then
                fCountryName := 'USA'
              else if fCountryName = 'United Kingdom' then
                fCountryName := 'UK'
              else if fCountryName = 'Domestic' then
                fCountryName := 'USA';
              fCountryList.Add(fCountryName);
            end;
          end;
        end
        else
        begin
        end;
      except
        on E: Exception do
        begin
          Debug(dpError, section, Format('[PARSECOUNTRIES][%d] *** EXCEPTION *** accessing item: %s', [i, E.Message]));
        end;
      end;
    end;
    
    // Use DelimitedText to avoid quotes around country names
    fCountryList.Delimiter := ',';
    fCountryList.QuoteChar := #0;  // Disable quoting
    Result := fCountryList.DelimitedText;
  finally
    fCountryList.Free;
  end;
  
end;

{ TPazoHTTPImdbApiTask }

constructor TPazoHTTPImdbApiTask.Create(const aImdbTitleID: String; const aReleaseName: String; aUseApiFirst: Boolean = True);
begin
  inherited Create('', '', getAdminSiteName);
  
  FReleaseName := aReleaseName;
  FImdbTitleID := aImdbTitleID;
  FUseApiFirst := aUseApiFirst;
  FExecuted := False;
end;

destructor TPazoHTTPImdbApiTask.Destroy;
begin
  inherited Destroy;
end;

function TPazoHTTPImdbApiTask.DetermineSTVStatus(const aMovieType: String; const aReleaseName: String): Boolean;
var
  fTvShowname: String;
  fTvSeason: Integer;
  fTvEpisode: Int64;
begin
  Result := False;
  
  // Check if it's a TV-related type based on API type field
  if AnsiContainsText(aMovieType, 'tvSeries') or 
     AnsiContainsText(aMovieType, 'tvMiniSeries') or
     AnsiContainsText(aMovieType, 'tvSpecial') or
     AnsiContainsText(aMovieType, 'tvMovie') or
     AnsiContainsText(aMovieType, 'tvShort') then
  begin
    Debug(dpSpam, section, Format('STV detected due to TV type: %s', [aMovieType]));
    Result := True;
    Exit;
  end;
  
  // Check if it's a video type
  if AnsiContainsText(aMovieType, 'video') or
     AnsiContainsText(aMovieType, 'videoGame') then
  begin
    Debug(dpSpam, section, Format('STV detected due to video type: %s', [aMovieType]));
    Result := True;
    Exit;
  end;
  
  // Also check release name for TV show patterns (same logic as original)
  getShowValues(aReleaseName, fTvShowname, fTvSeason, fTvEpisode);
  if not ((fTvSeason > 0) or (fTvEpisode > 0) or (fTvSeason = Ord(tvDatedShow))
         or (fTvSeason = Ord(tvRegularSerieWithoutSeason)) or (fTvEpisode = Ord(tvNoEpisodeTag))) then
  begin
    Debug(dpSpam, section, Format('STV detected due to TV show pattern in release name: season=%d, episode=%d', [fTvSeason, fTvEpisode]));
    Result := True;
    Exit;
  end;
  
  // If it's a regular movie type, it's theatrical (Cine)
  Debug(dpSpam, section, Format('Theatrical release detected, type: %s', [aMovieType]));
end;

function TPazoHTTPImdbApiTask.Execute(slot: Pointer): Boolean;
var
  i: Integer;
begin
  Result := False;

  // Guard against multiple executions of the same task object
  if FExecuted then
  begin
    Result := True;
    ready := True;
    Exit;
  end;

  Debug(dpSpam, section, Format('[IMDB-FLOW15] Task execution started for release: %s, IMDB-ID: %s', [FReleaseName, FImdbTitleID]));
  FExecuted := True;
  
  try
    
    // Move from pending to running state
    dbaddimdb_cs.Enter('Execute-move-pending-to-running');
    try
      
      i := pending_imdb_tasks.IndexOf(FReleaseName);
      if pending_imdb_tasks.Count > 0 then
      begin
      end;
      
      if i <> -1 then
      begin
        pending_imdb_tasks.Delete(i);
        running_imdb_tasks.Add(FReleaseName);
      end
      else
      begin
        
        // If not in pending, check if already in running (shouldn't happen but safety check)
        i := running_imdb_tasks.IndexOf(FReleaseName);
        if running_imdb_tasks.Count > 0 then
        begin
        end;
        
        if i <> -1 then
        begin
          Result := True; // Return success to avoid duplicate processing
          Exit;
        end
        else
        begin
          // Not in pending or running - add to running as fallback
          running_imdb_tasks.Add(FReleaseName);
        end;
      end;
    finally
      dbaddimdb_cs.Leave;
    end;
    
    // Try API approach first if enabled
    if FUseApiFirst then
    begin
      Debug(dpSpam, section, Format('[IMDB-FLOW16] Trying API approach for: %s', [FReleaseName]));
      if TryApiApproach then
      begin
        Debug(dpSpam, section, Format('[IMDB-FLOW17] API approach succeeded for: %s', [FReleaseName]));
        Result := True;
        Exit;
      end
      else
      begin
        Debug(dpSpam, section, Format('[IMDB-FLOW18] API approach failed, fallback needed for: %s', [FReleaseName]));
        // Don't send IRC message here - specific error was already sent in TryApiApproach
      end;
    end;

    // Fallback to original scraping method
    Debug(dpSpam, section, Format('[IMDB-FLOW19] Falling back to scraping for: %s', [FReleaseName]));
    irc_Addstats(Format('<c7>[iMDB]</c> API failed for <b>%s</b>, falling back to scraping method...', [FReleaseName]));
    Result := FallbackToScraping;
    Debug(dpSpam, section, Format('[EXECUTE] Scraping result for %s: %s', [FReleaseName, BoolToStr(Result, True)]));
    if not Result then
    begin
      irc_Addstats(Format('<c4>[iMDB ERROR]</c> Both API and scraping failed for <b>%s</b> - no IMDB data available', [FReleaseName]));
    end;
    
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXECUTE] *** EXCEPTION *** in Execute for %s: %s', [FReleaseName, E.Message]));
      irc_Addstats(Format('<c4>[iMDB ERROR]</c> Critical error processing <b>%s</b>: %s', [FReleaseName, E.Message]));
      Result := False;
    end;
  end;
  
  // Always clean up the running task entry when we're done (success or failure)
  try
    dbaddimdb_cs.Enter('Execute-cleanup');
    try
      i := running_imdb_tasks.IndexOf(FReleaseName);
      if i <> -1 then
      begin
        running_imdb_tasks.Delete(i);
      end;
      
      // Also clean up any remaining pending entry (shouldn't exist but safety)
      i := pending_imdb_tasks.IndexOf(FReleaseName);
      if i <> -1 then
      begin
        pending_imdb_tasks.Delete(i);
      end;
    finally
      dbaddimdb_cs.Leave;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXECUTE] *** CLEANUP EXCEPTION *** cleaning up task for %s: %s', [FReleaseName, E.Message]));
    end;
  end;
  
  ready := True;
  Debug(dpSpam, section, Format('[EXECUTE] === TASK COMPLETED === for release: %s, Result: %s', [FReleaseName, BoolToStr(Result, True)]));
end;

function TPazoHTTPImdbApiTask.Name: String;
begin
  Result := Format('IMDBAPI: %s', [FReleaseName]);
end;

function TPazoHTTPImdbApiTask.TryApiApproach: Boolean;
var
  fMovieData: TImdbApiMovieData;
  fImdbId: String;
  fDbImdbData: TDbImdbData;
  i: Integer;
begin
  Result := False;
  Debug(dpSpam, section, Format('[TRYAPI] === API APPROACH STARTED === for release: %s', [FReleaseName]));
  
  try
    // Check if we already have IMDB data for this release to prevent loops
    dbaddimdb_cs.Enter('TryApiApproach-check');
    try
      i := last_imdbdata.IndexOf(FReleaseName);
      if i <> -1 then
      begin
        Debug(dpError, section, Format('[TRYAPI] Release %s found in memory cache, skipping API call', [FReleaseName]));
        Result := True; // Return true to indicate "success" but don't process
        Exit;
      end;
    finally
      dbaddimdb_cs.Leave;
    end;
    
    // Also check persistent database to prevent redundant API calls
    if foundMovieAlreadyInDbWithReleaseName(FReleaseName) then
    begin
      Debug(dpError, section, Format('[TRYAPI] Release %s already exists in persistent database, skipping API call', [FReleaseName]));
      irc_Addstats(Format('(<c9>i</c>).....<c2><b>IMDB</b></c>........ <c0><b>for : %s</b></c> .......: found in Database!', [FReleaseName]));
      Result := True;
      Exit;
    end;
    
    // If we don't have an IMDB ID, try to search for it
    fImdbId := FImdbTitleID;
    if (fImdbId = '') or (fImdbId = '0') then
    begin
      fImdbId := TImdbApiParser.SearchTitle(FReleaseName);
      if fImdbId = '' then
      begin
        Debug(dpError, section, Format('[TRYAPI] IMDB search failed for: %s', [FReleaseName]));
        irc_Addstats(Format('<c4>[iMDB API ERROR]</c> Could not find IMDB ID for <b>%s</b> via API search', [FReleaseName]));
        Exit;
      end;
    end;
    
    
    // Get movie details
    fMovieData := TImdbApiParser.GetMovieDetails(fImdbId);
    if fMovieData = nil then
    begin
      Debug(dpError, section, Format('[TRYAPI] GetMovieDetails failed for: %s (%s)', [FReleaseName, fImdbId]));
      irc_Addstats(Format('<c4>[iMDB API ERROR]</c> Failed to get movie details for <b>%s</b> (ID: %s)', [FReleaseName, fImdbId]));
      Exit;
    end;
    
    
    try
      // Convert to database format and save using existing infrastructure
      Debug(dpSpam, section, Format('[DATA CREATION] Creating TDbImdbData with ID: "%s"', [fImdbId]));
      Debug(dpSpam, section, Format('[DATA CREATION] fMovieData.Title: "%s", Year: %d, Rating: %d', [fMovieData.Title, fMovieData.Year, fMovieData.Rating]));
      
      fDbImdbData := TDbImdbData.Create(fImdbId);
      try
        Debug(dpSpam, section, Format('[DATA CREATION] After constructor - fDbImdbData.imdb_id: "%s"', [fDbImdbData.imdb_id]));
        
        fDbImdbData.imdb_id := fImdbId;
        fDbImdbData.imdb_origtitle := fMovieData.Title;
        fDbImdbData.imdb_year := fMovieData.Year;
        
        Debug(dpSpam, section, Format('[DATA ASSIGNMENT] After assignment - imdb_id: "%s", origtitle: "%s", year: %d', [fDbImdbData.imdb_id, fDbImdbData.imdb_origtitle, fDbImdbData.imdb_year]));
        fDbImdbData.imdb_rating := fMovieData.Rating;
        fDbImdbData.imdb_votes := fMovieData.Votes;
        // Manually split comma-separated strings and add to TStringList
        Debug(dpSpam, section, Format('[DATA ASSIGNMENT] Languages from API: "%s"', [fMovieData.Languages]));
        if fMovieData.Languages <> '' then
        begin
          fDbImdbData.imdb_languages.Clear;
          fDbImdbData.imdb_languages.Delimiter := ',';
          fDbImdbData.imdb_languages.DelimitedText := fMovieData.Languages;
          Debug(dpSpam, section, Format('[DATA ASSIGNMENT] Languages assigned to TStringList: Count=%d, DelimitedText="%s"', [fDbImdbData.imdb_languages.Count, fDbImdbData.imdb_languages.DelimitedText]));
        end
        else
        begin
          Debug(dpSpam, section, '[DATA ASSIGNMENT] Languages is empty!');
        end;
        
        Debug(dpSpam, section, Format('[DATA ASSIGNMENT] Countries from API: "%s"', [fMovieData.Countries]));
        if fMovieData.Countries <> '' then
        begin
          fDbImdbData.imdb_countries.Clear;
          fDbImdbData.imdb_countries.Delimiter := ',';
          fDbImdbData.imdb_countries.DelimitedText := fMovieData.Countries;
          Debug(dpSpam, section, Format('[DATA ASSIGNMENT] Countries assigned to TStringList: Count=%d, DelimitedText="%s"', [fDbImdbData.imdb_countries.Count, fDbImdbData.imdb_countries.DelimitedText]));
        end
        else
        begin
          Debug(dpSpam, section, '[DATA ASSIGNMENT] Countries is empty, defaulting to USA!');
          fDbImdbData.imdb_countries.Clear;
          fDbImdbData.imdb_countries.Add('USA');
          Debug(dpSpam, section, '[DATA ASSIGNMENT] Added default country: USA');
        end;
        
        Debug(dpSpam, section, Format('[DATA ASSIGNMENT] Genres from API: "%s"', [fMovieData.Genres]));
        if fMovieData.Genres <> '' then
        begin
          fDbImdbData.imdb_genres.Clear;
          fDbImdbData.imdb_genres.Delimiter := ',';
          fDbImdbData.imdb_genres.DelimitedText := fMovieData.Genres;
          Debug(dpSpam, section, Format('[DATA ASSIGNMENT] Genres assigned to TStringList: Count=%d, DelimitedText="%s"', [fDbImdbData.imdb_genres.Count, fDbImdbData.imdb_genres.DelimitedText]));
        end
        else
        begin
          Debug(dpSpam, section, '[DATA ASSIGNMENT] Genres is empty!');
        end;
        fDbImdbData.imdb_type := fMovieData.MovieType;
        
        // Set default values for missing API fields
        fDbImdbData.imdb_screens := 0;
        fDbImdbData.imdb_cineyear := 0;
        fDbImdbData.imdb_ldt := False;
        fDbImdbData.imdb_wide := False;
        fDbImdbData.imdb_festival := False;
        
        // Determine STV status based on API type information
        fDbImdbData.imdb_stvm := DetermineSTVStatus(fMovieData.MovieType, FReleaseName);
        if fDbImdbData.imdb_stvm then
          fDbImdbData.imdb_stvs := Format('STV based on type: %s', [fMovieData.MovieType])
        else
          fDbImdbData.imdb_stvs := 'API source - no box office data';
        
        // Save using the correct existing function
        // NOTE: dbaddimdb_SaveImdbData already calls dbaddimdb_FireKbAdd internally
        Debug(dpSpam, section, Format('About to save IMDB data for release: %s', [FReleaseName]));
        dbaddimdb_SaveImdbData(FReleaseName, fDbImdbData);
        Debug(dpSpam, section, Format('Successfully saved IMDB data for release: %s', [FReleaseName]));
        
        Result := True;
        
        // DON'T free fDbImdbData here - ownership is transferred to last_imdbdata
        // The THashedStringList with OwnsObjects=True will handle cleanup when needed
      except
        on E: Exception do
        begin
          Debug(dpError, section, Format('[DATA SAVE] Exception saving data: %s', [E.Message]));
          FreeAndNil(fDbImdbData); // Free on error since it wasn't saved
          Result := False;
        end;
      end;
    finally
      fMovieData.Free;
    end;
    
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[TRYAPI] *** EXCEPTION *** in TryApiApproach for %s: %s', [FReleaseName, E.Message]));
      irc_Addstats(Format('<c4>[iMDB API ERROR]</c> Exception in API processing for <b>%s</b>: %s', [FReleaseName, E.Message]));
      Result := False;
    end;
  end;
  
end;

function TPazoHTTPImdbApiTask.FallbackToScraping: Boolean;
var
  fScrapingTask: TPazoHTTPImdbTask;
begin
  Result := False;
  
  try
    
    // Create and execute original scraping task
    fScrapingTask := TPazoHTTPImdbTask.Create(FImdbTitleID, FReleaseName);
    try
      Result := fScrapingTask.Execute(nil);
      if not Result then
        Debug(dpSpam, section, 'Scraping fallback failed');
    finally
      fScrapingTask.Free;
    end;
    
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('Exception in FallbackToScraping: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

end.