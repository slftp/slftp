unit dbaddimdb;

interface

uses Classes, SysUtils, StrUtils, Contnrs, Generics.Collections, IniFiles, irc, SyncObjs, dbhandler,
     mormot.orm.core, mormot.core.base, mormot.orm.base, mormot.rest.sqlite3,
     mormot.core.unicode, mormot.core.os, slcriticalsection2, DateUtils, kb.releaseinfo;

type
  { @abstract(Class for information from each single line of the slftp.imdbcountries file) }
  TMapLanguageCountry = class
  private
    FLanguage: String;
    FCountryCode: String;
    FCountry: String;
  public
    { Creates a class with the given information }
    constructor Create(const aLanguage, aCountryCode, aCountry: String);

    { Returns the Countryname for a given Language
      @param(aLanguage Name of Language)
      @returns(Countryname @br @note(empty string if Language does not exist)) }
    class function GetCountrynameByLanguage(const aLanguage: String): String;

    property Language: String read FLanguage;
    property CountryCode: String read FCountryCode;
    property Country: String read FCountry;
  end;


var
  imdb_remove_words_list: TStringList;
  ImdbDatabase: TSQLRestClientDB;
  ImdbDbModel: TSQLModel;
  imdbcountries: TIniFile;
  addimdbcmd: String;

type

  { NOTE: everything which starts with IMDb is data from IMDb }

  TIMDbDataRecord = class(TOrm)
  private
    FIMDbID: RawUTF8;
    FIMDbTitle: RawUTF8;
    FIMDbTitleCleaned: RawUTF8;
    FIMDbTitleExtras: RawUTF8;
    FIMDbYear: Integer;
    FIMDbCineyear: Integer;
    FIMDbRating: Integer;
    FIMDbVotes: Integer;
    FIMDbLanguages: RawUTF8;
    FIMDbCountries: RawUTF8;
    FIMDbGenres: RawUTF8;
    FIMDbType: RawUTF8;
    FCreationTime: TDateTime;
    FUpdatedTime: TDateTime;
  published
    //property IMDbData: TID read fIMDbDataId write fIMDbDataId;
    property IMDbID: RawUTF8 read FIMDbID write FIMDbID stored AS_UNIQUE;
    property IMDbTitle: RawUTF8 read FIMDbTitle write FIMDbTitle;
    property IMDbTitleCleaned: RawUTF8 read FIMDbTitleCleaned write FIMDbTitleCleaned;
    property IMDbTitleExtras: RawUTF8 read FIMDbTitleExtras write FIMDbTitleExtras;
    property IMDbYear: Integer read FIMDbYear write FIMDbYear;
    property IMDbCineyear: Integer read FIMDbCineyear write FIMDbCineyear;
    property IMDbRating: Integer read FIMDbRating write FIMDbRating;
    property IMDbVotes: Integer read FIMDbVotes write FIMDbVotes;

    property IMDbLanguages: RawUTF8 read FIMDbLanguages write FIMDbLanguages;
    property IMDbCountries: RawUTF8 read FIMDbCountries write FIMDbCountries;
    property IMDbGenres: RawUTF8 read FIMDbGenres write FIMDbGenres;
    property IMDbType: RawUTF8 read FIMDbType write FIMDbType;

    property CreationTime: TDateTime read FCreationTime write FCreationTime;
    property UpdatedTime: TDateTime read FUpdatedTime write FUpdatedTime;
    //...
  end;


TIMDbBomDataRecord = class(TOrmNoCase)
  private
    FIMDbCountry: RawUTF8;
    FIMDbScreens: Integer;
    FIMDbData:  TIMDbDataRecord;
  published
    property IMDbCountry: RawUTF8 read FIMDbCountry write FIMDbCountry;
    property IMDbScreens: Integer read FIMDbScreens write FIMDbScreens;
    property IMDbData: TIMDbDataRecord read FIMDbData write FIMDbData;
  end;

  // data should be filtered to sort out countries which never get a release
  TIMDbReleaseDatesRecord = class(TOrmNoCase)
  private
    FIMDbCountry: RawUTF8;
    FIMDbReleaseDate: TDateTime;
    FIMDbReleaseDateExtraInfo: RawUTF8;
    FIMDbData:  TIMDbDataRecord;
  published
    property IMDbCountry: RawUTF8 read FIMDbCountry write FIMDbCountry;
    property IMDbReleaseDate: TDateTime read FIMDbReleaseDate write FIMDbReleaseDate;
    property IMDbReleaseDateExtraInfo: RawUTF8 read FIMDbReleaseDateExtraInfo write FIMDbReleaseDateExtraInfo;
    property IMDbData: TIMDbDataRecord read FIMDbData write FIMDbData;
  end;

  TIMDbAlsoKnownAsRecord = class(TOrmNoCase)
  private
    FCountry: RawUTF8;
    FIMDbTitleCleaned: RawUTF8;
    FImdbTitle: RawUTF8;
    FFromImdb: boolean;
    FIMDbData:  TIMDbDataRecord;
  published
    property Country: RawUTF8 read FCountry write FCountry;
    property IMDbTitleCleaned: RawUTF8 read FIMDbTitleCleaned write FIMDbTitleCleaned;
    property ImdbTitle: RawUTF8 read FImdbTitle write FImdbTitle;
    property FromImdb: boolean read FFromImdb write FFromImdb;
    property IMDbData: TIMDbDataRecord read FIMDbData write FIMDbData;
  end;

  // Legacy compatibility class for existing code
  TDbImdbData = class
    public
    imdb_id: RawUTF8;
    imdb_year: Integer;
    imdb_languages: TStringList;
    imdb_countries: TStringList;
    imdb_genres: TStringList;
    imdb_screens: Integer;
    imdb_rating: Integer;
    imdb_votes: Integer;
    imdb_cineyear:integer;
    imdb_ldt:boolean;
    imdb_wide:boolean;
    imdb_festival:boolean;
    imdb_stvm:boolean;
    imdb_stvs:String;
    imdb_type:String;
    imdb_bom_country: String;
    imdb_origtitle: String;
    UpdatedTime: TDateTime;
    constructor Create(const aIMDbId:String);
    destructor Destroy; override;
    procedure PostResults(const aRls: String);
    procedure SetIMDBRelease(ir: TIMDBRelease);
end;

{ Checks if Country should be excluded (doesn't exist in slftp.imdbcountries file)
  @param(aCountryname Name of Country to be checked)
  @returns(@true if no entry exists in file (exclude), @false otherwise) }
function ExcludeCountry(const aCountryname: String): Boolean;
/// an easy way to create a database model for client and server
function CreateIMDBModel: TSQLModel;
{ Removes IMDbData from Database based on the IMDbId
  @param(aIMDbId IMDbId)
  @returns(@true if deletion was successful, @false if some problem occured) }
function DeleteIMDbDataWithImdbId(const aIMDbId: String): Boolean;
{ Removes IMDbData from Database based on the ReleaseName
  @param(aReleaseName ReleaseName)
  @returns(@true if deletion was successful, @false if some problem occured) }
function DeleteIMDbDataWithReleaseName(const aReleaseName: String): Boolean;
{ Returns the Number of lines in the TIMDBData Table in SQLite
  @returns(@NumberOfLines Integer) }
Function getNbrOfImdbEntries: Integer;
{ Removes Scene relevant Data from the ReleaseName
  @param(aReleaseName ReleaseName)
  @returns(@CleanedReleaseName String) }
function getMovieNameWithoutSceneTags(const aReleasename: String): String;
{ Updates the FLookupDone flag in the knowledge base TIMDBRelease object
  @param(aReleaseName Release name to find in knowledge base) }
procedure UpdateIMDBLookupDoneFlag(const aReleaseName: String);
{ Returns True if the ReleaseName is found in Database
  @param(aReleasename ReleaseName)
  @returns(@True if found, @False if not found) }
Function foundMovieAlreadyInDbWithReleaseName(const aReleasename: String): Boolean;
{ Returns IMDB Movie Data for ReleaseName from Database
  @param(aReleaseName ReleaseName)
  @param(aPostInIrc Post result in IRC)
  @returns(@Instance of TDbImdbData if found, nil if not found) }
function GetImdbMovieData(const aReleaseName: String; const aPostInIrc: boolean = True): TDbImdbData;
{ Returns True if the IMDbID is found in Database
  @param(aIMDbId IMDbId)
  @returns(@True if found, @False if not found) }
Function foundMovieAlreadyInDbWithImdbId(const aIMDbId: String): Boolean;
{ Returns Instance of TImdbRelease populated with Database Data
  @param(aIMDbId IMDbId)
  @returns(@Instance of TImdbRelease populated with Database Data if found, nil if not found) }
function getImdbReleaseFromDatabase(const aIMDbId: String): TDbImdbData;
{ Saves IMDB data to database and announces to channels
  @param(rls Release name)
  @param(imdbdata IMDB data to save) }
procedure dbaddimdb_SaveImdbData(rls: String; imdbdata: TDbImdbData);
{ Parses IMDB ID from text using regex
  @param(text Text to parse)
  @param(imdbid Output IMDB ID)
  @returns(True if ID found, False otherwise) }
function dbaddimdb_parseid(const text: String; out imdbid: String): Boolean;
{ Saves IMDB data with just release name and IMDB ID
  @param(rls Release name)
  @param(imdb_id IMDB ID) }
procedure dbaddimdb_SaveImdb(rls, imdb_id: String);
{ Returns status string showing IMDB database statistics }
function dbaddimdb_Status: String;
{ Processes IMDB commands from IRC
  @param(net Network name)
  @param(chan Channel name)  
  @param(nick Nickname)
  @param(msg Message)
  @returns(True if command was processed, False otherwise) }
function dbaddimdb_Process(net, chan, nick, msg: String): Boolean;
{ Checks if IMDB ID is valid
  @param(aIMDbId IMDB ID to check)
  @returns(True if valid, False otherwise) }
function check_ImdbId(const aIMDbId: String): Boolean;
{ Checks if movie update is needed for release name
  @param(aReleasename Release name)
  @returns(True if update needed, False otherwise) }
function UpdateMovieInDbWithReleaseNameNeeded(const aReleasename: String): Boolean;
{ Checks if movie update is needed for IMDB data
  @param(aImdbData IMDB data)
  @returns(True if update needed, False otherwise) }
function UpdateMovieInDbWithImdbDataNeeded(const aImdbData: TDbImdbData): Boolean;
{ Gets country from release name for AKA mapping
  @param(aReleaseName Release name)
  @param(countriesLst List of countries from IMDB)
  @returns(Country name for AKA record) }
function getCountryFromReleaseName(const aReleaseName: string; const countriesLst: TStringList): string;

var
  last_addimdb: THashedStringList;
  dbaddimdb_cs: TSlCriticalSection2;
  pending_imdb_tasks: THashedStringList; // Track tasks that are about to be created
  running_imdb_tasks: THashedStringList; // Track tasks that are currently running
  last_imdbdata: THashedStringList; // Cache for IMDB data

// Procedures
procedure dbaddimdbInit;
procedure dbaddimdbStart;
procedure dbaddimdbUnInit;
procedure dbaddimdbReload;
{ Creates HTTP task to fetch IMDB data from API
  @param(aReleaseName Release name)
  @param(aIMDbId IMDB ID) }
procedure CreateHttpTask(const aReleaseName, aIMDbId: String);

implementation

uses Math, configunit, mystrings, kb,
  sitesunit, RegExpr, debugunit, taskhttpimdb, pazo, mrdohutils, dbtvinfo, FLRE,
  tasksunit;

const
  section = 'dbaddimdb';
  IMDBREPLACEFILENAME = 'slftp.imdbreplace';

var
  rx_imdbid: TFLRE;
  glLanguageCountryMappingList: TObjectList<TMapLanguageCountry>;
  fStrList: TStringList;

{ TMapLanguageCountry }

constructor TMapLanguageCountry.Create(const aLanguage, aCountryCode, aCountry: String);
begin
  inherited Create;
  FLanguage := aLanguage;
  FCountryCode := aCountryCode;
  FCountry := aCountry;
end;

class function TMapLanguageCountry.GetCountrynameByLanguage(const aLanguage: String): String;
var
  fItem: TMapLanguageCountry;
begin
  Result := '';
  for fItem in glLanguageCountryMappingList do
  begin
    if fItem.FLanguage = aLanguage then
      Exit(fItem.Country);
  end;
end;

{ Processes raw country names from API and maps them to proper language/country codes }
function ProcessCountriesForDisplay(const aRawCountries: TStringList): string;
var
  i: Integer;
  fValidCountries: TStringList;
  fCountryName: string;
begin
  Result := '';
  if Assigned(aRawCountries) then
    Debug(dpSpam, section, Format('[PROCESSCOUNTRIES] Input countries count: %d', [aRawCountries.Count]))
  else
    Debug(dpSpam, section, '[PROCESSCOUNTRIES] Input countries count: 0 (aRawCountries is nil)');
  
  if not Assigned(aRawCountries) or (aRawCountries.Count = 0) then
  begin
    Debug(dpSpam, section, '[PROCESSCOUNTRIES] aRawCountries is nil or empty, returning empty string');
    Exit;
  end;

  // Log all input countries
  for i := 0 to aRawCountries.Count - 1 do
  begin
    Debug(dpSpam, section, Format('[PROCESSCOUNTRIES] Input country[%d]: "%s"', [i, aRawCountries[i]]));
  end;

  fValidCountries := TStringList.Create;
  fValidCountries.QuoteChar := #0;
  try
    for i := 0 to aRawCountries.Count - 1 do
    begin
      fCountryName := Trim(aRawCountries[i]);
      Debug(dpSpam, section, Format('[PROCESSCOUNTRIES] Processing country: "%s"', [fCountryName]));
      
      // Only include countries that are not excluded (exist in slftp.imdbcountries)
      if not ExcludeCountry(fCountryName) then
      begin
        Debug(dpSpam, section, Format('[PROCESSCOUNTRIES] Country "%s" is NOT excluded, adding to valid list', [fCountryName]));
        if fValidCountries.IndexOf(fCountryName) = -1 then
          fValidCountries.Add(fCountryName);
      end
      else
      begin
        Debug(dpSpam, section, Format('[PROCESSCOUNTRIES] Country "%s" is EXCLUDED, skipping', [fCountryName]));
      end;
    end;
    
    Debug(dpSpam, section, Format('[PROCESSCOUNTRIES] Valid countries count: %d', [fValidCountries.Count]));
    for i := 0 to fValidCountries.Count - 1 do
    begin
      Debug(dpSpam, section, Format('[PROCESSCOUNTRIES] Valid country[%d]: "%s"', [i, fValidCountries[i]]));
    end;
    
    // Return comma-separated list of valid countries  
    Result := fValidCountries.DelimitedText;
    Debug(dpSpam, section, Format('[PROCESSCOUNTRIES] Final result: "%s"', [Result]));
  finally
    fValidCountries.Free;
  end;
end;

function FormatListForDisplay(const aList: TStringList): string;
var
  i: Integer;
begin
  Result := '';
  if (aList = nil) or (aList.Count = 0) then Exit;
  
  // Build string manually to avoid quoting logic of TStringList
  for i := 0 to aList.Count - 1 do
  begin
    Result := Result + aList[i];
    if i < aList.Count - 1 then
      Result := Result + ',';
  end;
end;

function CreateIMDBModel: TSQLModel;
begin
  result := TSQLModel.Create([TIMDbDataRecord,TIMDbReleaseDatesRecord,TIMDbAlsoKnownAsRecord, TIMDbBomDataRecord]);
end;

{ Extracts the moviename from given releasename by stripping year and scene tags
  @param(aReleasename Releasename)
  @param(aYear Year value from releasename (see TRelease.year))
  @returns(Moviename without scene taggins and year) }
function getMovieNameWithoutSceneTags(const aReleasename: String): String;
var
  fRx: TRegexpr;
  fLine, fReleaseNameYear: String;
  fYear, fCnt: Integer;
  fReleaseNameSplitted: TStringList;
begin
  Result := aReleasename;

  fReleaseNameSplitted := TStringList.Create();
  fReleaseNameSplitted.Delimiter := ' ';
  fReleaseNameSplitted.CaseSensitive := False;

  fReleaseNameYear := ReplaceText(aReleasename, '(', '');
  fReleaseNameYear := ReplaceText(fReleaseNameYear, ')', '');
  fReleaseNameYear := ReplaceText(fReleaseNameYear, '.', ' ');
  fReleaseNameYear := ReplaceText(fReleaseNameYear, '-', ' ');
  fReleaseNameYear := ReplaceText(fReleaseNameYear, '_', ' ');
  fReleaseNameSplitted.DelimitedText := fReleaseNameYear;

  for fCnt := fReleaseNameSplitted.Count - 1 downto 0 do
  begin
    fYear := StrToIntDef(fReleaseNameSplitted[fCnt], 0);
    if fYear > 1900 then
    begin
      Result := Result.Replace(fYear.ToString, '', [rfReplaceAll, SysUtils.rfIgnoreCase]);
      break;
    end;
  end;

  fRx := TRegexpr.Create;
  try
    fRx.ModifierI := True;
    fRx.ModifierG := True;
    try
      if imdb_remove_words_list <> nil then
      begin
        for fCnt := 0 to imdb_remove_words_list.Count - 1 do
        begin
          fLine := Trim(imdb_remove_words_list[fCnt]);
          if ((fLine = '') or (fLine[1] = '#')) then
            continue;
          fRx.Expression := fLine;
          Result := fRx.Replace(Result, '', True);
        end;
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('Exception in getMovieNameWithoutSceneTags: %s', [e.Message]));
        exit;
      end;
    end;
  finally
    fRx.Free;
    fReleaseNameSplitted.Free;
  end;

  // remove possible whitespace
  Result := Result.Replace(' ', '', [rfReplaceAll, SysUtils.rfIgnoreCase]);
  // remove scene delimiters
  Result := Result.Replace('.', '', [rfReplaceAll, SysUtils.rfIgnoreCase]);
  Result := Result.Replace('_', '', [rfReplaceAll, SysUtils.rfIgnoreCase]);

  Debug(dpSpam, section, Format('[getMovieNameWithoutSceneTags] before: %s - after: %s', [aReleasename, Result]));
end;

function GetImdbMovieData(const aReleaseName: String; const aPostInIrc: boolean = True): TDbImdbData;
var
  fMovieImdbDataRec: TIMDbDataRecord;
  fAlsoKnownAsDataRec: TIMDbAlsoKnownAsRecord;
  fCleanedMovieName, fReleasenameCountry: string;
  fRelease: TRelease;
  fReleaseYear: integer;
begin
  Result := nil;
  fMovieImdbDataRec := nil;
  fAlsoKnownAsDataRec := nil;
  fRelease := nil;

  try
    if ImdbDatabase = nil then
      Exit;

    fCleanedMovieName := getMovieNameWithoutSceneTags(aReleaseName);
    Debug(dpSpam, section, Format('[GETIMDBMOVIEDATA] Cleaned movie name: %s from release: %s', [fCleanedMovieName, aReleaseName]));

    // Create release to get year - using T0DayRelease as concrete implementation
    fRelease := T0DayRelease.Create(aReleaseName, 'IMDB', False);
    fReleaseYear := fRelease.year;
    if (fReleaseYear = 0) then
      fReleaseYear := SysUtils.CurrentYear;
    
    fReleasenameCountry := getCountryFromReleaseName(aReleaseName, nil);
    if fReleasenameCountry = '' then
      fReleasenameCountry := '(original title)';
    
    Debug(dpSpam, section, Format('[GETIMDBMOVIEDATA] Using year: %d, country: %s', [fReleaseYear, fReleasenameCountry]));

    // First try: Search main table with year
    fMovieImdbDataRec := TIMDbDataRecord.CreateAndFillPrepare(ImdbDatabase.Client,
      'IMDbTitleCleaned = ? and IMDbYear = ?', [],
      [StringToUTF8(fCleanedMovieName), fReleaseYear]);

    if fMovieImdbDataRec.FillOne then
    begin
      Debug(dpSpam, section, '[GETIMDBMOVIEDATA] Found in main table with year');
      // Create result object
      Result := TDbImdbData.Create(fMovieImdbDataRec.IMDbID);
      Result.imdb_id := fMovieImdbDataRec.IMDbID;
      Result.imdb_year := fMovieImdbDataRec.IMDbYear;
      Result.imdb_origtitle := UTF8ToString(fMovieImdbDataRec.IMDbTitle);
      Result.imdb_rating := fMovieImdbDataRec.IMDbRating;
      Result.imdb_votes := fMovieImdbDataRec.IMDbVotes;
      Result.imdb_cineyear := fMovieImdbDataRec.IMDbCineyear;
      Result.UpdatedTime := fMovieImdbDataRec.UpdatedTime;
      
      // Convert RawUTF8 fields to StringList
      if fMovieImdbDataRec.IMDbLanguages <> '' then
        Result.imdb_languages.CommaText := UTF8ToString(fMovieImdbDataRec.IMDbLanguages);
      if fMovieImdbDataRec.IMDbCountries <> '' then
        Result.imdb_countries.CommaText := UTF8ToString(fMovieImdbDataRec.IMDbCountries);
      if fMovieImdbDataRec.IMDbGenres <> '' then
        Result.imdb_genres.CommaText := UTF8ToString(fMovieImdbDataRec.IMDbGenres);
      Result.imdb_type := UTF8ToString(fMovieImdbDataRec.IMDbType);
    end
    else
    begin
      Debug(dpSpam, section, '[GETIMDBMOVIEDATA] Not found in main table, searching AKA table with year');
      // Second try: Search AKA table with year and country
      fAlsoKnownAsDataRec := TIMDbAlsoKnownAsRecord.CreateAndFillPrepare(ImdbDatabase.Client,
        'IMDbTitleCleaned = ? and Country = ?', [],
        [StringToUTF8(fCleanedMovieName), StringToUTF8(fReleasenameCountry)]);
      
      if fAlsoKnownAsDataRec.FillOne then
      begin
        Debug(dpSpam, section, '[GETIMDBMOVIEDATA] Found in AKA table, loading main record');
        // Get the main record from the AKA reference
        fMovieImdbDataRec.Free;
        fMovieImdbDataRec := TIMDbDataRecord.CreateAndFillPrepare(ImdbDatabase.Client, 
          'ID = ? and IMDbYear = ?', [], 
          [fAlsoKnownAsDataRec.IMDbData.ID, fReleaseYear]);
        
        if fMovieImdbDataRec.FillOne then
        begin
          Debug(dpSpam, section, '[GETIMDBMOVIEDATA] Found main record via AKA with year');
          // Create result object
          Result := TDbImdbData.Create(fMovieImdbDataRec.IMDbID);
          Result.imdb_id := fMovieImdbDataRec.IMDbID;
          Result.imdb_year := fMovieImdbDataRec.IMDbYear;
          Result.imdb_origtitle := UTF8ToString(fMovieImdbDataRec.IMDbTitle);
          Result.imdb_rating := fMovieImdbDataRec.IMDbRating;
          Result.imdb_votes := fMovieImdbDataRec.IMDbVotes;
          Result.imdb_cineyear := fMovieImdbDataRec.IMDbCineyear;
          Result.UpdatedTime := fMovieImdbDataRec.UpdatedTime;
          
          // Convert RawUTF8 fields to StringList
          if fMovieImdbDataRec.IMDbLanguages <> '' then
            Result.imdb_languages.CommaText := UTF8ToString(fMovieImdbDataRec.IMDbLanguages);
          if fMovieImdbDataRec.IMDbCountries <> '' then
            Result.imdb_countries.CommaText := UTF8ToString(fMovieImdbDataRec.IMDbCountries);
          if fMovieImdbDataRec.IMDbGenres <> '' then
            Result.imdb_genres.CommaText := UTF8ToString(fMovieImdbDataRec.IMDbGenres);
          Result.imdb_type := UTF8ToString(fMovieImdbDataRec.IMDbType);
        end;
      end;

      // Third try: If no year or year doesn't match, try without year
      if (Result = nil) and (fRelease.year = 0) then
      begin
        Debug(dpSpam, section, '[GETIMDBMOVIEDATA] Trying main table without year');
        fMovieImdbDataRec.Free;
        fMovieImdbDataRec := TIMDbDataRecord.CreateAndFillPrepare(ImdbDatabase.Client,
          'IMDbTitleCleaned = ?', [],
          [StringToUTF8(fCleanedMovieName)]);
          
        if fMovieImdbDataRec.FillOne then
        begin
          Debug(dpSpam, section, '[GETIMDBMOVIEDATA] Found in main table without year');
          // Create result object
          Result := TDbImdbData.Create(fMovieImdbDataRec.IMDbID);
          Result.imdb_id := fMovieImdbDataRec.IMDbID;
          Result.imdb_year := fMovieImdbDataRec.IMDbYear;
          Result.imdb_origtitle := UTF8ToString(fMovieImdbDataRec.IMDbTitle);
          Result.imdb_rating := fMovieImdbDataRec.IMDbRating;
          Result.imdb_votes := fMovieImdbDataRec.IMDbVotes;
          Result.imdb_cineyear := fMovieImdbDataRec.IMDbCineyear;
          Result.UpdatedTime := fMovieImdbDataRec.UpdatedTime;
          
          // Convert RawUTF8 fields to StringList
          if fMovieImdbDataRec.IMDbLanguages <> '' then
            Result.imdb_languages.CommaText := UTF8ToString(fMovieImdbDataRec.IMDbLanguages);
          if fMovieImdbDataRec.IMDbCountries <> '' then
            Result.imdb_countries.CommaText := UTF8ToString(fMovieImdbDataRec.IMDbCountries);
          if fMovieImdbDataRec.IMDbGenres <> '' then
            Result.imdb_genres.CommaText := UTF8ToString(fMovieImdbDataRec.IMDbGenres);
          Result.imdb_type := UTF8ToString(fMovieImdbDataRec.IMDbType);
        end
        else
        begin
          Debug(dpSpam, section, '[GETIMDBMOVIEDATA] Trying AKA table without year');
          // Fourth try: AKA table without year
          fAlsoKnownAsDataRec.Free;
          fAlsoKnownAsDataRec := TIMDbAlsoKnownAsRecord.CreateAndFillPrepare(ImdbDatabase.Client,
            'IMDbTitleCleaned = ? and Country = ?', [],
            [StringToUTF8(fCleanedMovieName), StringToUTF8(fReleasenameCountry)]);
          
          if fAlsoKnownAsDataRec.FillOne then
          begin
            Debug(dpSpam, section, '[GETIMDBMOVIEDATA] Found in AKA table without year, loading main record');
            // Get the main record from the AKA reference
            fMovieImdbDataRec.Free;
            fMovieImdbDataRec := TIMDbDataRecord.CreateAndFillPrepare(ImdbDatabase.Client, 
              'ID = ?', [], 
              [fAlsoKnownAsDataRec.IMDbData.ID]);
            
            if fMovieImdbDataRec.FillOne then
            begin
              Debug(dpSpam, section, '[GETIMDBMOVIEDATA] Found main record via AKA without year');
              // Create result object
              Result := TDbImdbData.Create(fMovieImdbDataRec.IMDbID);
              Result.imdb_id := fMovieImdbDataRec.IMDbID;
              Result.imdb_year := fMovieImdbDataRec.IMDbYear;
              Result.imdb_origtitle := UTF8ToString(fMovieImdbDataRec.IMDbTitle);
              Result.imdb_rating := fMovieImdbDataRec.IMDbRating;
              Result.imdb_votes := fMovieImdbDataRec.IMDbVotes;
              Result.imdb_cineyear := fMovieImdbDataRec.IMDbCineyear;
              Result.UpdatedTime := fMovieImdbDataRec.UpdatedTime;
              
              // Convert RawUTF8 fields to StringList
              if fMovieImdbDataRec.IMDbLanguages <> '' then
                Result.imdb_languages.CommaText := UTF8ToString(fMovieImdbDataRec.IMDbLanguages);
              if fMovieImdbDataRec.IMDbCountries <> '' then
                Result.imdb_countries.CommaText := UTF8ToString(fMovieImdbDataRec.IMDbCountries);
              if fMovieImdbDataRec.IMDbGenres <> '' then
                Result.imdb_genres.CommaText := UTF8ToString(fMovieImdbDataRec.IMDbGenres);
              Result.imdb_type := UTF8ToString(fMovieImdbDataRec.IMDbType);
            end;
          end;
        end;
      end;
    end;
    
    if Result <> nil then
      Debug(dpSpam, section, Format('[GETIMDBMOVIEDATA] Successfully found IMDB data for: %s (ID: %s)', [aReleaseName, Result.imdb_id]))
    else
      Debug(dpSpam, section, Format('[GETIMDBMOVIEDATA] No IMDB data found for: %s', [aReleaseName]));
      
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('Exception in GetImdbMovieData: %s', [e.Message]));
    end;
  end;

  try
    if Assigned(fMovieImdbDataRec) then
      fMovieImdbDataRec.Free;
    if Assigned(fAlsoKnownAsDataRec) then
      fAlsoKnownAsDataRec.Free;
    if Assigned(fRelease) then
      fRelease.Free;
  except
    // Ignore cleanup errors
  end;
end;

function foundMovieAlreadyInDbWithReleaseName(const aReleasename: String): Boolean;
var
  fDbImdbData: TDbImdbData;
begin
  Result := False;

  try
    fDbImdbData := GetImdbMovieData(aReleasename, False);
    Result := fDbImdbData <> nil;
  finally
    if fDbImdbData <> nil then
      fDbImdbData.Free;
  end;
end;

function foundMovieAlreadyInDbWithImdbId(const aIMDbId: String): Boolean;
var
  dbRecord: TIMDbDataRecord;
begin
  Result := False;
  if ImdbDatabase = nil then
    Exit;
    
  try
    dbRecord := TIMDbDataRecord.CreateAndFillPrepare(ImdbDatabase.Client, 'IMDbID = ?', [], [StringToUTF8(aIMDbId)]);
    try
      Result := dbRecord.FillOne;
    finally
      dbRecord.Free;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] foundMovieAlreadyInDbWithImdbId: %s', [e.Message]));
    end;
  end;
end;

function UpdateMovieInDbWithImdbDataNeeded(const aImdbData: TDbImdbData): Boolean;
begin
  Result := DaysBetween(now, aImdbData.UpdatedTime) >= config.ReadInteger(section, 'update_time_in_days', 7);
end;

function getCountryFromReleaseName(const aReleaseName: string; const countriesLst: TStringList): string;
begin
  // For now, return a default country name
  // In full implementation, this would analyze the release name for language/country hints
  Result := '(original title)';
end;

function getImdbReleaseFromDatabase(const aIMDbId: String): TDbImdbData;
var
  dbRecord: TIMDbDataRecord;
begin
  Result := nil;
  if ImdbDatabase = nil then
    Exit;
    
  try
    dbRecord := TIMDbDataRecord.CreateAndFillPrepare(ImdbDatabase.Client, 'IMDbID = ?', [], [StringToUTF8(aIMDbId)]);
    try
      if dbRecord.FillOne then
      begin
        Result := TDbImdbData.Create(aIMDbId);
        // Fill data from database record
        Result.imdb_id := UTF8ToString(dbRecord.IMDbID);
        Result.imdb_year := dbRecord.IMDbYear;
        Result.imdb_origtitle := UTF8ToString(dbRecord.IMDbTitle);
        Result.imdb_rating := dbRecord.IMDbRating;
        Result.imdb_votes := dbRecord.IMDbVotes;
        Result.imdb_cineyear := dbRecord.IMDbCineyear;
        Result.UpdatedTime := dbRecord.UpdatedTime;
        
        // Convert RawUTF8 properties from database to TStringList
        Result.imdb_languages.DelimitedText := UTF8ToString(dbRecord.IMDbLanguages);
        Result.imdb_countries.DelimitedText := UTF8ToString(dbRecord.IMDbCountries);
        Result.imdb_genres.DelimitedText := UTF8ToString(dbRecord.IMDbGenres);
        Result.imdb_type := UTF8ToString(dbRecord.IMDbType);
      end;
    finally
      dbRecord.Free;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] getImdbReleaseFromDatabase: %s', [e.Message]));
      if Result <> nil then
      begin
        Result.Free;
        Result := nil;
      end;
    end;
  end;
end;

function DeleteIMDbDataWithImdbId(const aIMDbId: String): Boolean;
var
  dbRecord: TIMDbDataRecord;
begin
  Result := False;
  if ImdbDatabase = nil then
    Exit;
    
  try
    dbRecord := TIMDbDataRecord.CreateAndFillPrepare(ImdbDatabase.Client, 'IMDbID = ?', [], [StringToUTF8(aIMDbId)]);
    try
      if dbRecord.FillOne then
      begin
        Result := ImdbDatabase.Delete(TIMDbDataRecord, dbRecord.IDValue);
      end;
    finally
      dbRecord.Free;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] DeleteIMDbDataWithImdbId: %s', [e.Message]));
    end;
  end;
end;

function DeleteIMDbDataWithReleaseName(const aReleaseName: String): Boolean;
begin
  Result := False;
  // Implementation would need to find IMDbID by release name first
  // For now return false to maintain compatibility
end;

function getNbrOfImdbEntries: Integer;
begin
  Result := 0;
  if ImdbDatabase <> nil then
  begin
    try
      Result := ImdbDatabase.TableRowCount(TIMDbDataRecord);
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] getNbrOfImdbEntries: %s', [e.Message]));
      end;
    end;
  end;
end;

function dbaddimdb_Status: String;
begin
  Result := Format('<b>iMDB Movie Infos</b>: %d', [getNbrOfImdbEntries()]);
end;

function ExcludeCountry(const aCountryname: String): Boolean;
var
  fItem: TMapLanguageCountry;
begin
  Result := True;
  Debug(dpSpam, section, Format('[EXCLUDECOUNTRY] Checking country: "%s"', [aCountryname]));
  Debug(dpSpam, section, Format('[EXCLUDECOUNTRY] glLanguageCountryMappingList count: %d', [glLanguageCountryMappingList.Count]));
  
  for fItem in glLanguageCountryMappingList do
  begin
    if (fItem.Country = 'UK') or (fItem.Country = 'USA') then
    begin
      Debug(dpSpam, section, Format('[EXCLUDECOUNTRY] Checking exact match for UK/USA: "%s" vs "%s"', [aCountryname, fItem.Country]));
      // to avoid matching Ukraine with UK
      if aCountryname.StartsWith(fItem.Country, False) then
      begin
        Debug(dpSpam, section, Format('[EXCLUDECOUNTRY] Found match for "%s" with "%s", returning FALSE (not excluded)', [aCountryname, fItem.Country]));
        Exit(False);
      end;
    end
    else
    begin
      Debug(dpSpam, section, Format('[EXCLUDECOUNTRY] Checking case-insensitive match: "%s" vs "%s"', [aCountryname, fItem.Country]));
      // match things like 'Canada (French title)' and 'Canada (English title)'
      if aCountryname.StartsWith(fItem.Country, True) then
      begin
        Debug(dpSpam, section, Format('[EXCLUDECOUNTRY] Found case-insensitive match for "%s" with "%s", returning FALSE (not excluded)', [aCountryname, fItem.Country]));
        Exit(False);
      end;
    end;
  end;
  
  Debug(dpSpam, section, Format('[EXCLUDECOUNTRY] No match found for "%s", returning TRUE (excluded)', [aCountryname]));
end;

{ TDbImdbData }

constructor TDbImdbData.Create(const aIMDbId: String);
begin
  inherited Create;
  imdb_id := aIMDbId;
  imdb_languages := TStringList.Create;
  imdb_languages.Delimiter := ',';
  imdb_languages.StrictDelimiter := True;
  imdb_countries := TStringList.Create;
  imdb_countries.Delimiter := ',';
  imdb_countries.StrictDelimiter := True;
  imdb_genres := TStringList.Create;
  imdb_genres.Delimiter := ',';
  imdb_genres.StrictDelimiter := True;
  imdb_year := 0;
  imdb_rating := 0;
  imdb_votes := 0;
  imdb_cineyear := 0;
  imdb_screens := 0;
  imdb_ldt := False;
  imdb_wide := False;
  imdb_festival := False;
  imdb_stvm := False;
  imdb_stvs := '';
  imdb_type := '';
  imdb_bom_country := '';
  imdb_origtitle := '';
  UpdatedTime := Now;
end;

destructor TDbImdbData.Destroy;
begin
  imdb_languages.Free;
  imdb_countries.Free;
  imdb_genres.Free;
  inherited;
end;

procedure TDbImdbData.PostResults(const aRls: String);
var
  dbRecord: TIMDbDataRecord;
  akaRecord: TIMDbAlsoKnownAsRecord;
  currentTime: TDateTime;
  status: String;
  fReleasenameCountry, fCleanedMovieName: String;
  fDoUpdate: Boolean;
begin
  Debug(dpSpam, section, Format('[POSTRESULTS] Called with aRls: %s, imdb_id: %s', [aRls, imdb_id]));
  
  if ImdbDatabase = nil then
  begin
    Debug(dpError, section, '[POSTRESULTS] ImdbDatabase is nil, exiting');
    Exit;
  end;
    
  try
    currentTime := Now;
    Debug(dpSpam, section, Format('[POSTRESULTS] Creating database record query for: %s', [imdb_id]));
    
    // Check if record already exists
    dbRecord := TIMDbDataRecord.CreateAndFillPrepare(ImdbDatabase.Client, 'IMDbID = ?', [], [StringToUTF8(imdb_id)]);
    try
      Debug(dpSpam, section, '[POSTRESULTS] Checking if record exists');
      fDoUpdate := dbRecord.FillOne;
      
      if fDoUpdate then
      begin
        Debug(dpSpam, section, '[POSTRESULTS] Record exists, updating');
        // Update existing record
        dbRecord.UpdatedTime := currentTime;
      end
      else
      begin
        Debug(dpSpam, section, '[POSTRESULTS] Record does not exist, creating new');
        // Create new record
        dbRecord.Free;
        dbRecord := TIMDbDataRecord.Create;
        dbRecord.IMDbID := StringToUTF8(imdb_id);
        dbRecord.CreationTime := currentTime;
        dbRecord.UpdatedTime := currentTime;
      end;
      
      Debug(dpSpam, section, '[POSTRESULTS] Filling record with data');
      // Fill record with data
      dbRecord.IMDbTitle := StringToUTF8(imdb_origtitle);
      dbRecord.IMDbTitleCleaned := StringToUTF8(getMovieNameWithoutSceneTags(imdb_origtitle));
      dbRecord.IMDbYear := imdb_year;
      dbRecord.IMDbRating := imdb_rating;
      dbRecord.IMDbVotes := imdb_votes;
      dbRecord.IMDbCineyear := imdb_cineyear;
      
      Debug(dpSpam, section, '[POSTRESULTS] Converting string lists');
      // Convert TStringList to delimited text for storage
      if Assigned(imdb_languages) then
        dbRecord.IMDbLanguages := StringToUTF8(imdb_languages.DelimitedText)
      else
        Debug(dpError, section, '[POSTRESULTS] WARNING: imdb_languages is nil');
        
      if Assigned(imdb_countries) then
        dbRecord.IMDbCountries := StringToUTF8(imdb_countries.DelimitedText)
      else
        Debug(dpError, section, '[POSTRESULTS] WARNING: imdb_countries is nil');
        
      if Assigned(imdb_genres) then
        dbRecord.IMDbGenres := StringToUTF8(imdb_genres.DelimitedText)
      else
        Debug(dpError, section, '[POSTRESULTS] WARNING: imdb_genres is nil');

      dbRecord.IMDbType := StringToUTF8(imdb_type);

      Debug(dpSpam, section, '[POSTRESULTS] Saving to database');
      // Save to database
      if fDoUpdate then
        ImdbDatabase.Update(dbRecord)
      else
        ImdbDatabase.Add(dbRecord, True);
      Debug(dpSpam, section, '[POSTRESULTS] Successfully saved to database');
      
      // Create AKA record for the release name if provided
      if aRls <> '' then
      begin
        Debug(dpSpam, section, Format('[POSTRESULTS] Creating AKA record for release: %s', [aRls]));
        fReleasenameCountry := getCountryFromReleaseName(aRls, imdb_countries);
        if fReleasenameCountry = '' then
          fReleasenameCountry := '(original title)';
        
        fCleanedMovieName := getMovieNameWithoutSceneTags(aRls);
        Debug(dpSpam, section, Format('[POSTRESULTS] AKA details - Country: %s, Cleaned: %s', [fReleasenameCountry, fCleanedMovieName]));
        
        // Check if AKA record already exists
        akaRecord := TIMDbAlsoKnownAsRecord.CreateAndFillPrepare(ImdbDatabase.Client, 
          'IMDbData = ? and IMDbTitleCleaned = ? and Country = ?', [], 
          [dbRecord.IDValue, StringToUTF8(fCleanedMovieName), StringToUTF8(fReleasenameCountry)]);
        try
          if not akaRecord.FillOne then
          begin
            Debug(dpSpam, section, '[POSTRESULTS] Creating new AKA record');
            // Create new AKA record
            akaRecord.Free;
            akaRecord := TIMDbAlsoKnownAsRecord.Create;
            akaRecord.Country := StringToUTF8(fReleasenameCountry);
            akaRecord.IMDbTitleCleaned := StringToUTF8(fCleanedMovieName);
            akaRecord.ImdbTitle := StringToUTF8(aRls);
            akaRecord.FromImdb := False; // This comes from release name, not IMDB
            akaRecord.IMDbData := dbRecord.AsTOrm;
            ImdbDatabase.Add(akaRecord, True);
            Debug(dpSpam, section, '[POSTRESULTS] AKA record created successfully');
          end
          else
          begin
            Debug(dpSpam, section, '[POSTRESULTS] AKA record already exists');
          end;
        finally
          akaRecord.Free;
        end;
      end;
      
    finally
      dbRecord.Free;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TDbImdbData.PostResults: %s', [e.Message]));
      raise;
    end;
  end;

  // IRC output like in mORMot2 reference
  Debug(dpSpam, section, '[POSTRESULTS] Starting IRC output');
  if imdb_stvm then status := 'STV'
  else if imdb_festival then status := 'Festival'
  else if imdb_ldt then status := 'Limited'
  else if imdb_wide then status := 'Wide'
  else status := 'Cine';

  Debug(dpSpam, section, Format('[POSTRESULTS] About to call irc_Addstats for: %s', [aRls]));
  irc_Addstats(Format('(<c9>i</c>).....<c2><b>IMDB</b></c>........ <c0><b>for : %s</b></c> .......: https://www.imdb.com/title/%s/', [aRls, imdb_id]));
  irc_Addstats(Format('(<c9>i</c>).....<c2><b>IMDB</b></c>........ <c0><b>Original Title - Year</b></c> ...: %s (%d)', [imdb_origtitle, imdb_year]));
  irc_Addstats(Format('(<c9>i</c>).....<c2><b>IMDB</b></c>........ <b><c9>Country - Languages</b></c> ..: %s - %s', [ProcessCountriesForDisplay(imdb_countries), FormatListForDisplay(imdb_languages)]));
  irc_Addstats(Format('(<c9>i</c>).....<c2><b>IMDB</b></c>........ <b><c5>Genres</b></c> .........: %s', [FormatListForDisplay(imdb_genres)]));
  irc_Addstats(Format('(<c9>i</c>).....<c2><b>IMDB</b></c>........ <c7><b>Rating</b>/<b>Type</b></c> ....: <b>%d</b> of 100 (%d) (%s) | Type: %s', [imdb_rating,imdb_votes,status,imdb_type]));
  Debug(dpSpam, section, '[POSTRESULTS] IRC output completed');
end;

procedure TDbImdbData.SetIMDBRelease(ir: TIMDBRelease);
begin
  ir.imdb_id := UTF8ToString(imdb_id);
  ir.imdb_year := imdb_year;
  ir.imdb_languages.CommaText := imdb_languages.CommaText;
  ir.imdb_countries.CommaText := imdb_countries.CommaText;
  ir.imdb_genres.CommaText := imdb_genres.CommaText;
  ir.imdb_screens := imdb_screens;
  ir.imdb_rating := imdb_rating;
  ir.imdb_votes := imdb_votes;
  ir.CineYear := imdb_cineyear;
  ir.imdb_ldt := imdb_ldt;
  ir.imdb_wide := imdb_wide;
  ir.imdb_festival := imdb_festival;
  ir.imdb_stvm := imdb_stvm;
  ir.imdb_stvs := imdb_stvs;
  ir.imdb_type := imdb_type;

  ir.SetLookupDone;
end;

{ Updates the FLookupDone flag in the knowledge base TIMDBRelease object }
procedure UpdateIMDBLookupDoneFlag(const aReleaseName: String);
var
  fPazo: TPazo;
  fIMDBRelease: TIMDBRelease;
begin
  try
    // Try to find the release in knowledge base (works for releases from IRC precatchers)
    fPazo := FindPazoByName('', aReleaseName);
    if Assigned(fPazo) and (fPazo.rls is TIMDBRelease) then
    begin
      fIMDBRelease := TIMDBRelease(fPazo.rls);
      fIMDBRelease.SetLookupDone;
      Debug(dpSpam, section, Format('[IMDBLOOKUPDONE] UpdateIMDBLookupDoneFlag: Set FLookupDone=True for release %s (found in knowledge base)', [aReleaseName]));
    end
    else
    begin
      // This is normal for manual !addimdb commands - they don't create knowledge base entries
      // Only log at spam level since it's expected behavior
      Debug(dpSpam, section, Format('[IMDBLOOKUPDONE] UpdateIMDBLookupDoneFlag: No TIMDBRelease in knowledge base for %s (likely manual !addimdb command)', [aReleaseName]));
    end;
  except
    on e: Exception do
      Debug(dpSpam, section, Format('[IMDBLOOKUPDONE] UpdateIMDBLookupDoneFlag exception for %s: %s', [aReleaseName, e.Message]));
  end;
end;

procedure dbaddimdb_SaveImdbData(rls: String; imdbdata: TDbImdbData);
var
  fPazo: TPazo;
begin
  if imdbdata = nil then
  begin
    Debug(dpError, section, Format('[SAVEIMDB] ERROR: imdbdata is nil for release: %s', [rls]));
    Exit;
  end;

  Debug(dpSpam, section, Format('[IMDB-FLOW20] dbaddimdb_SaveImdbData called for release: %s', [rls]));
  Debug(dpSpam, section, Format('[SAVEIMDB] Countries count before check: %d', [imdbdata.imdb_countries.Count]));
  
  // Log current countries content for debugging
  if imdbdata.imdb_countries.Count > 0 then
  begin
    Debug(dpSpam, section, Format('[SAVEIMDB] Current countries: %s', [imdbdata.imdb_countries.CommaText]));
  end
  else
  begin
    Debug(dpSpam, section, '[SAVEIMDB] Countries list is empty!');
  end;
  
  // Final fallback: Apply USA default if countries are still empty
  // This catches cases where both API and scraping failed or returned empty data
  if imdbdata.imdb_countries.Count = 0 then
  begin
    Debug(dpSpam, section, '[SAVEIMDB] Final fallback: Countries are empty, applying USA default for streaming content');
    imdbdata.imdb_countries.Add('USA');
    Debug(dpSpam, section, Format('[SAVEIMDB] After adding USA default, countries count: %d', [imdbdata.imdb_countries.Count]));
    Debug(dpSpam, section, Format('[SAVEIMDB] After adding USA default, countries content: %s', [imdbdata.imdb_countries.CommaText]));
  end
  else
  begin
    Debug(dpSpam, section, '[SAVEIMDB] Countries are not empty, skipping USA default');
  end;

  // Save to persistent database using PostResults method
  try
    imdbdata.PostResults(rls);
    Debug(dpSpam, section, Format('[IMDB-FLOW21] Data saved to persistent database: %s', [imdbdata.imdb_id]));

    // Populate IMDB fields in knowledge base using SetIMDBRelease (like TV does with SetTVDbRelease)
    try
      fPazo := FindPazoByRls(rls);
      if (fPazo <> nil) and (fPazo.rls is TIMDBRelease) then
      begin
        Debug(dpSpam, section, Format('[IMDB-FLOW24] Calling SetIMDBRelease for pazo: %s', [rls]));
        imdbdata.SetIMDBRelease(TIMDBRelease(fPazo.rls));
        Debug(dpSpam, section, Format('[IMDB-FLOW25] SetIMDBRelease completed for pazo: %s', [rls]));
      end
      else
      begin
        Debug(dpSpam, section, Format('[IMDBLOOKUPDONE] No pazo found or not TIMDBRelease for: %s', [rls]));
      end;
    except
      on e: Exception do
        Debug(dpSpam, section, Format('[IMDBLOOKUPDONE] SetIMDBRelease exception for %s: %s', [rls, e.Message]));
    end;
  except
    on e: Exception do
      Debug(dpError, section, Format('[SAVEIMDB] Database save error: %s', [e.Message]));
  end;

  // Store in memory cache for performance
  dbaddimdb_cs.Enter('SaveImdbData');
  try
    last_addimdb.AddObject(rls, imdbdata);
    Debug(dpSpam, section, Format('[SAVEIMDB] Successfully added to last_addimdb: %s (total count: %d)', [rls, last_addimdb.Count]));
    
    // Also add to last_imdbdata for TIMDBRelease.Aktualizal compatibility
    last_imdbdata.AddObject(rls, imdbdata);
    Debug(dpSpam, section, Format('[SAVEIMDB] Successfully added to last_imdbdata: %s (total count: %d)', [rls, last_imdbdata.Count]));
    
    // Cleanup old entries if we have too many
    while last_addimdb.Count > config.ReadInteger(section, 'max_results', 100) do
      last_addimdb.Delete(0);
      
    while last_imdbdata.Count > config.ReadInteger(section, 'max_results', 100) do
      last_imdbdata.Delete(0);
  finally
    dbaddimdb_cs.Leave;
  end;

  // Announce to channels if enabled
  if config.ReadBool(section, 'post_lookup_infos', true) then
  begin
    Debug(dpSpam, section, Format('[AUTO ANNOUNCE] About to announce IMDB data for: %s', [rls]));
    irc_AddInfo(Format('<c7>[iMDB Data]</c> for <b>%s</b> : %s', [rls, imdbdata.imdb_id]));
    Debug(dpSpam, section, Format('[AUTO ANNOUNCE] IMDB announcement completed for: %s', [rls]));
  end;
end;

{ Init }
procedure dbaddimdbInit;
var
  fDBName: String;
  fStrList: TStringList;
  i, j: Integer;
  fLang, fCC, fCountry, fHelper: String;
  fItem: TMapLanguageCountry;
  fDupe: Boolean;
begin
  fDBName := Trim(config.ReadString(section, 'database', 'imdb.db'));

  imdb_remove_words_list := TStringList.Create;
  try
    imdb_remove_words_list.LoadFromFile(ExtractFilePath(ParamStr(0)) + IMDBREPLACEFILENAME);
  except
    on e: Exception do
      Debug(dpError, section, Format('Could not load %s: %s', [IMDBREPLACEFILENAME, e.Message]));
  end;

  dbaddimdb_cs := TSlCriticalSection2.Create('dbaddimdb');
  last_addimdb:= THashedStringList.Create;
  last_addimdb.CaseSensitive:= False;
  
  pending_imdb_tasks := THashedStringList.Create;
  pending_imdb_tasks.CaseSensitive := False;
  running_imdb_tasks := THashedStringList.Create;
  running_imdb_tasks.CaseSensitive := False;
  last_imdbdata := THashedStringList.Create;
  last_imdbdata.CaseSensitive := False;

  ImdbDBModel := CreateIMDBModel;
  try
    ImdbDatabase := CreateORMSQLite3DB(ImdbDBModel, fDBName, '');
    Debug(dpSpam, section, Format('IMDb db loaded. %d Movies', [ImdbDatabase.TableRowCount(TIMDbDataRecord)]));
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] dbaddimdbInit: %s', [e.Message]));
      exit;
    end;
  end;

  rx_imdbid := TFLRE.Create('tt(\d{6,8})', [rfIGNORECASE]);
  addimdbcmd := config.ReadString(section, 'addimdbcmd', '!addimdb');
  
  Debug(dpSpam, section, Format('[INIT] addimdbcmd initialized to: "%s"', [addimdbcmd]));
  Debug(dpSpam, section, '[INIT] rx_imdbid regex initialized');

  glLanguageCountryMappingList := TObjectList<TMapLanguageCountry>.Create(True);
  fStrList := TStringList.Create;
  try
    Debug(dpSpam, section, Format('[INIT] Loading country mapping from: %s', [ExtractFilePath(ParamStr(0)) + 'slftp.imdbcountries']));
    fStrList.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'slftp.imdbcountries');
    Debug(dpSpam, section, Format('[INIT] Country file loaded, %d lines found', [fStrList.Count]));
    for i := 0 to fStrList.Count - 1 do
    begin
      fHelper := Trim(fStrList[i]);
      if ((fHelper = '') or (fHelper[1] = '#')) then
        continue;

      fLang := '';
      fCC := '';
      fCountry := '';

      j := Pos('=', fHelper);
      if j > 0 then
      begin
        fLang := Trim(Copy(fHelper, 1, j - 1));
        fHelper := Trim(Copy(fHelper, j + 1, 1000));

        j := Pos(',', fHelper);
        if j > 0 then
        begin
          fCC := Trim(Copy(fHelper, 1, j - 1));
          fCountry := Trim(Copy(fHelper, j + 1, 1000));
        end;
      end;

      if ((fLang <> '') and (fCC <> '') and (fCountry <> '')) then
      begin
        fDupe := False;
        for fItem in glLanguageCountryMappingList do
        begin
          if ((fItem.Language = fLang) and (fItem.CountryCode = fCC) and (fItem.Country = fCountry)) then
          begin
            fDupe := True;
            break;
          end;
        end;

        if not fDupe then
        begin
          glLanguageCountryMappingList.Add(TMapLanguageCountry.Create(fLang, fCC, fCountry));
          Debug(dpSpam, section, Format('[INIT] Added mapping: %s=%s,%s', [fLang, fCC, fCountry]));
        end;
      end;
    end;
    Debug(dpSpam, section, Format('[INIT] Country mapping loaded successfully, %d entries created', [glLanguageCountryMappingList.Count]));
  except
    on e: Exception do
      Debug(dpError, section, Format('Could not load slftp.imdbcountries: %s', [e.Message]));
  end;
  fStrList.Free;
end;

procedure dbaddimdbStart;
begin
  // Nothing special needed for start
end;

procedure dbaddimdbUnInit;
begin
  dbaddimdb_cs.Enter('uninit');
  try
    if Assigned(last_addimdb) then
      last_addimdb.Free;
    if Assigned(pending_imdb_tasks) then
      pending_imdb_tasks.Free;
    if Assigned(running_imdb_tasks) then
      running_imdb_tasks.Free;
    if Assigned(last_imdbdata) then
      last_imdbdata.Free;
    if Assigned(imdb_remove_words_list) then
      imdb_remove_words_list.Free;
    if Assigned(rx_imdbid) then
      rx_imdbid.Free;
    if Assigned(glLanguageCountryMappingList) then
      glLanguageCountryMappingList.Free;
    if Assigned(ImdbDatabase) then
      ImdbDatabase.Free;
    if Assigned(ImdbDbModel) then
      ImdbDbModel.Free;
  finally
    dbaddimdb_cs.Leave;
  end;
  
  FreeAndNil(dbaddimdb_cs);
end;

procedure dbaddimdbReload;
begin
  dbaddimdbUnInit;
  dbaddimdbInit;
end;

function dbaddimdb_parseid(const text: String; out imdbid: String): Boolean;
var
  fMultiCaptures: TFLREMultiCaptures;
begin
  imdbid := '';
  Result := False;
  try
    dbaddimdb_cs.Enter('parseid');
    try
      if rx_imdbid.MatchAll(text, fMultiCaptures, 1, 1) then
      begin
        if Length(fMultiCaptures) > 0 then
        begin
          imdbid := Copy(text, fMultiCaptures[0][0].Start, fMultiCaptures[0][0].Length);
          Result := True;
        end;
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] dbaddimdb_parseid: %s', [e.Message]));
      end;
    end;
  finally
    SetLength(fMultiCaptures, 0);
    dbaddimdb_cs.Leave;
  end;
end;

procedure dbaddimdb_SaveImdb(rls, imdb_id: String);
begin
  Debug(dpSpam, section, Format('[SAVEIMDB] Called with rls: %s, imdb_id: %s', [rls, imdb_id]));
  try
    // Start HTTP task to fetch IMDB data from API
    Debug(dpSpam, section, '[SAVEIMDB] Creating HTTP task to fetch IMDB data');
    CreateHttpTask(rls, imdb_id);
    Debug(dpSpam, section, Format('[SAVEIMDB] Successfully started HTTP task for %s', [rls]));
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] dbaddimdb_SaveImdb: %s', [e.Message]));
      raise;
    end;
  end;
end;

function check_ImdbId(const aIMDbId: String): Boolean;
var
  fNumericPart: String;
  fValue: Integer;
begin
  Result := False;
  Debug(dpSpam, section, Format('[CHECK_IMDBID] Validating IMDB ID: %s', [aIMDbId]));

  dbaddimdb_cs.Enter('checkid');
  try
    try
      if rx_imdbid.Find(aIMDbId) <> 0 then
      begin
        fNumericPart := Copy(aIMDbId, 3, Length(aIMDbId) - 2);
        fValue := StrToIntDef(fNumericPart, 0);

        if fValue = 0 then
        begin
          Debug(dpSpam, section, Format('[CHECK_IMDBID] Invalid IMDB ID (all zeros): %s', [aIMDbId]));
          Exit;
        end;

        Result := True;
        Debug(dpSpam, section, Format('[CHECK_IMDBID] Valid IMDB ID: %s', [aIMDbId]));
      end
      else
      begin
        Debug(dpSpam, section, Format('[CHECK_IMDBID] Invalid IMDB ID: %s', [aIMDbId]));
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] check_ImdbId: %s', [e.Message]));
      end;
    end;
  finally
    dbaddimdb_cs.Leave;
  end;
end;

function UpdateMovieInDbWithReleaseNameNeeded(const aReleasename: String): Boolean;
var
  fImdbData: TDbImdbData;
begin
  Result := True;
  // For now, always return true to allow updates
  // In a full implementation, this would check if the existing data is outdated
end;

function dbaddimdb_Process(net, chan, nick, msg: String): Boolean;
var
  fRls: String;
  fImdbId: String;
  fUpdateNeeded: boolean;
  fRlsFound: boolean;
  fPazo: TPazo;
  fImdbData: TDbImdbData;
begin
  Result := False;
  Debug(dpSpam, section, Format('[PROCESS] addimdbcmd value: "%s"', [addimdbcmd]));
  Debug(dpSpam, section, Format('[PROCESS] Message length: %d, First characters: "%s"', [Length(msg), Copy(msg, 1, Min(20, Length(msg)))]));

  // Check if message starts with addimdbcmd followed by space (not colon like "IMDBiNFO:")
  if (1 = Pos(addimdbcmd + ' ', msg)) then
  begin
    Debug(dpSpam, section, Format('[IMDB-FLOW1] Command received: net=%s, chan=%s, nick=%s, msg=%s', [net, chan, nick, msg]));
    Debug(dpSpam, section, Format('[IMDB-FLOW2] Command matched: %s', [addimdbcmd]));
    msg := Copy(msg, length(addimdbcmd + ' ') + 1, 1000);
    Debug(dpSpam, section, Format('[PROCESS] Parsed msg: %s', [msg]));

    fRls := '';
    fRls := SubString(msg, ' ', 1);
    Debug(dpSpam, section, Format('[IMDB-FLOW3] Parsed release: %s', [fRls]));
    fImdbId := '';
    fImdbId := SubString(msg, ' ', 2);
    Debug(dpSpam, section, Format('[IMDB-FLOW4] Parsed IMDB-ID: %s', [fImdbId]));

    if not check_ImdbId(fImdbId) then
    begin
      Debug(dpSpam, section, Format('[IMDB-FLOW5] Invalid IMDB-ID for %s: %s', [fRls, fImdbId]));
      // No IRC error message - only log invalid ID like in original
      exit;
    end;

    Debug(dpSpam, section, Format('[IMDB-FLOW6] IMDB-ID validation passed: %s', [fImdbId]));

    if ((fRls <> '') and (fImdbId <> '')) then
    begin
      Debug(dpSpam, section, Format('[IMDB-FLOW7] Starting processing for release %s with IMDB-ID %s', [fRls, fImdbId]));

      // Check if IMDB ID already exists in database
      if foundMovieAlreadyInDbWithImdbId(fImdbId) then
      begin
        Debug(dpSpam, section, Format('[IMDB-FLOW8] IMDB-ID %s already exists in database for release %s', [fImdbId, fRls]));
        irc_Addstats(Format('(<c9>i</c>).....<c2><b>IMDB</b></c>........ <c0><b>for : %s</b></c> .......: IMDB ID %s already in Database!', [fRls, fImdbId]));

        // Populate IMDB data from database using the IMDB ID (not release name)
        try
          Debug(dpSpam, section, Format('[IMDB-FLOW8a] Retrieving IMDB data for ID: %s', [fImdbId]));
          fImdbData := getImdbReleaseFromDatabase(fImdbId);
          if fImdbData <> nil then
          begin
            try
              Debug(dpSpam, section, Format('[IMDB-FLOW8b] Posting cached IMDB data to IRC for: %s', [fRls]));
              fImdbData.PostResults(fRls);

              fPazo := FindPazoByRls(fRls);
              if (fPazo <> nil) and (fPazo.rls is TIMDBRelease) then
              begin
                Debug(dpSpam, section, Format('[IMDB-FLOW8c] Calling SetIMDBRelease for pazo: %s', [fRls]));
                fImdbData.SetIMDBRelease(TIMDBRelease(fPazo.rls));
                Debug(dpSpam, section, Format('[IMDB-FLOW8d] SetIMDBRelease completed for: %s', [fRls]));
              end
              else
              begin
                Debug(dpSpam, section, Format('[IMDB-FLOW8e] No pazo found or not TIMDBRelease for: %s', [fRls]));
              end;
            finally
              fImdbData.Free;
            end;
          end
          else
          begin
            Debug(dpSpam, section, Format('[IMDB-FLOW8f] Failed to retrieve IMDB data for ID: %s', [fImdbId]));
          end;
        except
          on e: Exception do
            Debug(dpSpam, section, Format('[IMDB-FLOW8g] Exception populating IMDB data for %s: %s', [fRls, e.Message]));
        end;
        exit;
      end;
      
      fUpdateNeeded := UpdateMovieInDbWithReleaseNameNeeded(fRls);
      fRlsFound := foundMovieAlreadyInDbWithReleaseName(fRls);
      Debug(dpSpam, section, Format('[PROCESS] Update needed: %s, Release found: %s', [BoolToStr(fUpdateNeeded, True), BoolToStr(fRlsFound, True)]));
      if (not fUpdateNeeded AND fRlsFound) then
      begin
        Debug(dpSpam, section, '[PROCESS] Skipping - no update needed and release found');
        irc_Addstats(Format('<c7>[iMDB]</c> Release <b>%s</b> already exists in database', [fRls]));
        exit;
      end;

      try
        Debug(dpSpam, section, Format('[IMDB-FLOW9] Calling dbaddimdb_SaveImdb for %s, %s', [fRls, fImdbId]));
        dbaddimdb_SaveImdb(fRls, fImdbId);
        Debug(dpSpam, section, Format('[IMDB-FLOW10] Task queued successfully for %s', [fRls]));
        irc_Addstats(Format('<c7>[iMDB]</c> Successfully added <b>%s</b> with IMDB ID <b>%s</b> to database', [fRls, fImdbId]));
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('Exception in dbaddimdb_Process (SaveImdb): %s', [e.Message]));
          // No IRC error message - only log the error
          exit;
        end;
      end;
    end
    else
    begin
      Debug(dpSpam, section, Format('[PROCESS] Invalid parameters - rls: %s, imdbid: %s', [fRls, fImdbId]));
      // No IRC usage message - only log invalid parameters like in original
    end;

    Result := True;
  end;
end;

procedure CreateHttpTask(const aReleaseName, aIMDbId: String);
begin
  try
    // Check if data already exists to prevent redundant API calls
    if foundMovieAlreadyInDbWithReleaseName(aReleaseName) then
    begin
      Debug(dpSpam, section, Format('[IMDB-FLOW12] Release %s already in database, skipping task creation', [aReleaseName]));
      irc_Addstats(Format('(<c9>i</c>).....<c2><b>IMDB</b></c>........ <c0><b>for : %s</b></c> .......: found in Database!', [aReleaseName]));
      Exit;
    end;

    if foundMovieAlreadyInDbWithImdbId(aIMDbId) then
    begin
      Debug(dpSpam, section, Format('[IMDB-FLOW13] IMDB-ID %s already in database, skipping task creation', [aIMDbId]));
      Exit;
    end;

    // Check if task is already pending or running to prevent duplicates
    dbaddimdb_cs.Enter('CreateHttpTask-check');
    try
      if pending_imdb_tasks.IndexOf(aReleaseName) <> -1 then
      begin
        Debug(dpSpam, section, Format('[IMDB-FLOW12] Task already pending for %s, skipping duplicate', [aReleaseName]));
        Exit;
      end;
      if running_imdb_tasks.IndexOf(aReleaseName) <> -1 then
      begin
        Debug(dpSpam, section, Format('[IMDB-FLOW12] Task already running for %s, skipping duplicate', [aReleaseName]));
        Exit;
      end;
      // Add to pending list before creating task to prevent race condition
      pending_imdb_tasks.Add(aReleaseName);
    finally
      dbaddimdb_cs.Leave;
    end;

    Debug(dpSpam, section, Format('[IMDB-FLOW11] Creating IMDB API task for %s, IMDB-ID: %s', [aReleaseName, aIMDbId]));
    AddTask(TPazoHTTPImdbTask.Create(aIMDbId, aReleaseName), true);
    Debug(dpSpam, section, Format('[IMDB-FLOW14] Task object created and added to task queue for %s', [aReleaseName]));
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] in CreateHttpTask AddTask: %s', [e.Message]));
    end;
  end;
end;

end.
