unit taskhttpimdb;

interface

uses
  tasksunit, Generics.Collections, Variants, dbaddimdb, Classes;

type
  { @abstract(Processes IMDb JSON data into TDbImdbData structure) }
  TImdbDataProcessor = class
  public
    class function Process(const aReleaseName, aImdbId: String; const aTitleJson, aReleaseDatesJson: Variant; out aImdbData: TDbImdbData): String;
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
  imdbapi;

const
  section = 'taskhttpimdb';

{ TImdbDataProcessor }

class function TImdbDataProcessor.Process(const aReleaseName, aImdbId: String; const aTitleJson, aReleaseDatesJson: Variant; out aImdbData: TDbImdbData): String;
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
  fCountryName, fAttributesStr: String;
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
          else if fCountryName = 'United Kingdom' then fCountryName := 'UK';
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
          aImdbData.imdb_languages.Add(fStrHelper);
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
    
    // Status String Construction
    for i := 0 to fStatusReasonList.Count - 1 do
    begin
      fStatusReason := fStatusReason + Format('%d - %s%s', [i + 1, fStatusReasonList[i], #13#10]);
    end;
    
  finally
    fStatusReasonList.Free;
  end;

  // Screen counts are not available via imdbapi.dev; mark as unknown.
  aImdbData.imdb_screens := -1;
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
begin
  Result := False;
  
  // 1. Fetch Main Title Data
  if not TImdbApi.GetTitle(FImdbTitleID, fTitleJson) then
  begin
    irc_Adderror(Format('<c4>[FAILED]</c> Unable to fetch JSON for %s from IMDb API', [FImdbTitleID]));
    ready := True;
    Result := True;
    Exit;
  end;
  
  // 2. Fetch Release Dates
  // We allow this to fail silently or return null, process logic handles null
  if not TImdbApi.GetReleaseDates(FImdbTitleID, fReleaseDatesJson) then
    fReleaseDatesJson := Null;

  // 3. Process
  TImdbDataProcessor.Process(FReleaseName, FImdbTitleID, fTitleJson, fReleaseDatesJson, imdbdata);
  
  // 4. Save
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