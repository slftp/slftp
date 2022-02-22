unit dbaddimdb;

interface

uses Classes, SysUtils, Contnrs, Generics.Collections, IniFiles, irc, SyncObjs, SynCommons, mORMot, dbhandler, mORMotSQLite3;

type
  { @abstract(Class for information from each single line of the slftp.imdbcountries file) }
  TMapLanguageCountry = class
  private
    FLanguage: String; //< language
    FCountryCode: String; //< country code (for backward compatibility of the existing file, not really needed at the moment)
    FCountry: String; //< country name
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
  imdb_remove_words_list: TStringList; //< contains the lines which should be used to remove scene taggings to get the pure moviename
  ImdbDatabase: TSQLRestClientDB; //< Rest Client for all database interactions
  ImdbDbModel: TSQLModel; //< SQL ORM model for stats database
  imdbcountries: TIniFile;
  addimdbcmd: String;

type

  { NOTE: everything which starts with IMDb is data from IMDb }

  TIMDbDataRecord = class(TSQLRecord)
  private
    FIMDbID: RawUTF8; //< IMDb title ID like tt12345
    FIMDbTitle: RawUTF8; //< title from webpage
    FIMDbTitleCleaned: RawUTF8; //< title from webpage
    FIMDbTitleExtras: RawUTF8; //< extra info from title if available, e.g. TV* or VIDEO* stuff
    FIMDbYear: Integer; //< movie year
    FIMDbCineyear: Integer; //< cinema year (TODO: which value if not available?)
    FIMDbRating: Integer; //< rating (multiplied by 10)
    FIMDbVotes: Integer; //< votes

    // save as comma separated list and create TStringList only in TImdbRelease for backward compatibility of rules checking code
    FIMDbLanguages: TStringList; //< movie languages
    FIMDbCountries: TStringList; //< countries
    FIMDbGenres: TStringList; //< genres

    FCreationTime: TDateTime; //< time when the data was fetched
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

    property IMDbLanguages: TStringList read FIMDbLanguages write FIMDbLanguages;
    property IMDbCountries: TStringList read FIMDbCountries write FIMDbCountries;
    property IMDbGenres: TStringList read FIMDbGenres write FIMDbGenres;


    property CreationTime: TDateTime read FCreationTime write FCreationTime;
    property UpdatedTime: TDateTime read FUpdatedTime write FUpdatedTime;
    //...
  end;


TIMDbBomDataRecord = class(TSQLRecordNoCase)
  private
    FIMDbCountry: RawUTF8; //< country from releaseinfo page
    FIMDbScreens: Integer; //< screens
    FIMDbData:  TIMDbDataRecord;
  published
    property IMDbCountry: RawUTF8 read FIMDbCountry write FIMDbCountry;
    property IMDbScreens: Integer read FIMDbScreens write FIMDbScreens;
    property IMDbData: TIMDbDataRecord read FIMDbData write FIMDbData;
  end;

  // data should be filtered to sort out countries which never get a release
  TIMDbReleaseDatesRecord = class(TSQLRecordNoCase)
  private
    FIMDbCountry: RawUTF8; //< country from releaseinfo page
    FIMDbReleaseDate: TDateTime; //< release date from releaseinfo page
    FIMDbReleaseDateExtraInfo: RawUTF8; //< additional info from releaseinfo page
    FIMDbData:  TIMDbDataRecord;
  published
    property IMDbCountry: RawUTF8 read FIMDbCountry write FIMDbCountry;
    property IMDbReleaseDate: TDateTime read FIMDbReleaseDate write FIMDbReleaseDate;
    property IMDbReleaseDateExtraInfo: RawUTF8 read FIMDbReleaseDateExtraInfo write FIMDbReleaseDateExtraInfo;
    property IMDbData: TIMDbDataRecord read FIMDbData write FIMDbData;
  end;

  TIMDbAlsoKnownAsRecord = class(TSQLRecordNoCase)
  private
    FCountry: RawUTF8;  //< country from the IMDB homepage
    FIMDbTitleCleaned: RawUTF8; //< Cleaned title from the release name
    FImdbTitle: RawUTF8; //< Title from the IMDB homepage
    FFromImdb: boolean; //< @true if this entry originated from the IMDB homepage, @false if we added it from an unknown release name
    FIMDbData:  TIMDbDataRecord;
  published
    property Country: RawUTF8 read FCountry write FCountry;
    property IMDbTitleCleaned: RawUTF8 read FIMDbTitleCleaned write FIMDbTitleCleaned;
    property ImdbTitle: RawUTF8 read FImdbTitle write FImdbTitle;
    property FromImdb: Boolean read FFromImdb write FFromImdb;
    property IMDbData: TIMDbDataRecord read FIMDbData write FIMDbData;
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

  { @abstract(Class for IMDb release date information) }
  TIMDbReleaseDateInfo = class
  private
    FCountry: String; //< country name
    FReleaseDate: TDateTime; //< release date
    FExtraInfo: String; //< additional info like dvd premiere, festival, location of premiere, etc
  public
    { Creates a class for specific release date infos }
    constructor Create(const aCountry, aExtraInfo: String; const aReleaseDate: TDateTime);

    property Country: String read FCountry;
    property ReleaseDate: TDateTime read FReleaseDate;
    property ExtraInfo: String read FExtraInfo;
  end;

  TDbImdbData = class
    imdb_id: String;
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
    imdb_bom_country: String;
    // additional infos
    imdb_origtitle: String; //< original imdb/movie title
    UpdatedTime: TDateTime; //< when was the data updated last time
    constructor Create(const aIMDbId:String);
    destructor Destroy; override;
    procedure PostResults(const aRls : String = '');overload;
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
//// Gets the IMDbDatabase Object
//Function getImdbDatabase: TSQLRestClientDB;
{ Removes Scene relevant Data from the ReleaseName
  @param(aReleaseName ReleaseName)
  @returns(@CleanedReleaseName String) }
function getMovieNameWithoutSceneTags(const aReleasename: String): String;
{ Returns True if the ReleaseName is found in Database
  @param(aReleasename ReleaseName)
  @returns(@True if found, @False if not found) }
Function foundMovieAlreadyInDbWithReleaseName(const aReleasename: String): Boolean;
{ Returns True if the IMDbID is found in Database
  @param(aIMDbID IMDbID)
  @returns(@True if found, @False if not found) }
Function foundMovieAlreadyInDbWithImdbId(const aImdbId: String): Boolean;
{ Returns a TImdbData object if the IMDbID is found in Database
  @param(aIMDbID IMDbID)
  @param(aReleaseName The release name)
  @returns(a TImdbData object if found, nil otherwise) }
function FindImdbDataByImdbId(const aImdbId, aReleaseName: String): TDbImdbData;
{ Returns True if the IMDB-Data needs to be updated
  @param(aaReleasename aReleasename)
  @returns(@True if updated needed, @False if updated not needed) }
function UpdateMovieInDbWithReleaseNameNeeded(const aReleasename: String): Boolean;
{ Returns True if the IMDB-Data needs to be updated
  @param(aaReleasename aReleasename)
  @returns(@True if updated needed, @False if updated not needed) }
function UpdateMovieInDbWithImdbDataNeeded(const aImdbData: TDbImdbData): Boolean;

{ Creates a backup of IMDb-database - this is needed because the file is in use and can't be copied
  @param(aPath path where the backup should be stored in the filesystem with last slash, e.g. /path/to/file/)
  @param(aFileName filename including fileextension) }
procedure doIMDbDbBackup(const aPath, aFileName: String);

{ Parses the IRC-Message and extracts the IMDB-ID. After that call IMDB_Save.
  Returns True if update is done, else False. }
function dbaddimdb_Process(aNet, aChan, aNick, aMsg: String): Boolean;
{ Checks if the release is TV, creates the IMDBData Object and after that calls CreateHttpTask. }
procedure dbaddimdb_SaveImdb(const aReleaseName, aIMDbId: String);
{ save the IMDB-Data into a DataObject. }
procedure dbaddimdb_SaveImdbData(const aReleaseName: String; aImdbData: TDbImdbData; aIMDbAlsoKnownAsInfoList: TObjectList<TIMDbAlsoKnownAsInfo>; aImdbReleaseDateInfoList: TObjectList<TIMDbReleaseDateInfo>; aBOMCountryScreens: TDictionary<string, Integer>);
{ process found IMDB-Data. }
procedure dbaddimdb_ProcessImdbData(const aReleaseName: String; aImdbData: TDbImdbData);
{ Creates the httptask and checks for duplicates. }
procedure CreateHttpTask(const aReleaseName, aIMDbId: String);
 { Retriggers the queue. }
procedure dbaddimdb_FireKbAdd(const aReleaseName : String);
 { Insert a new IMDB-ReleaseName when neccesary. }
function dbaddimdb_Status: String;
 { Checks the IMDb-ID on right regex conform }
function check_ImdbId(const aIMDbId: String): Boolean;
 { parses the Imdb-ID from a String }
function parseImdbIDFromString(const aText: String; out aIMDbId: String): Boolean;
 { returns the Movie Data within a ImDB-Dataobject }
function GetImdbMovieData(const aReleaseName: String): TDbImdbData;
 { creates all relevant objects }
procedure dbaddimdbInit;
 { destroys all relevant objects }
procedure dbaddimdbUnInit;

var
  last_addimdb: THashedStringList;
  gDbAddimdb_cs: TCriticalSection;

implementation

uses

  debugunit, configunit, sitesunit, console, StrUtils, RegExpr,
  DateUtils, mystrings, FLRE, kb, kb.releaseinfo, sllanguagebase,
  queueunit, taskhttpimdb, pazo, mrdohutils, dbtvinfo;

var

  rx_imdbid: TFLRE;
  rx_captures: TFLREMultiCaptures;
  glLanguageCountryMappingList: TObjectList<TMapLanguageCountry>;

const
  IMDBREPLACEFILENAME = 'slftp.imdbreplace';
  section = 'dbaddimdb';

{ TIMDbAlsoKnownAsInfo }

constructor TIMDbAlsoKnownAsInfo.Create(const aCountry, aTitle: String);
begin
  FCountry := aCountry;
  FTitle := aTitle;
end;

{ TIMDbReleaseDateInfo }

constructor TIMDbReleaseDateInfo.Create(const aCountry, aExtraInfo: String; const aReleaseDate: TDateTime);
begin
  FCountry := aCountry;
  FReleaseDate := aReleaseDate;
  FExtraInfo := aExtraInfo;
end;

{ TMapLanguageCountry }

constructor TMapLanguageCountry.Create(const aLanguage, aCountryCode, aCountry: String);
begin
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
  fPazo: TPazo;
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
      for fLine in imdb_remove_words_list do
      begin
        fRx.Expression := Trim(fLine);
        Result := fRx.Replace(Result, '', True);
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

  Debug(dpError, section, Format('[getMovieNameWithoutSceneTags] before: %s - after: %s', [aReleasename, Result]));
end;

{ Converts the special characters in a moviename into scene-notation
  @param(aInput MovieName)
  @returns(Moviename in clean scene notation) }
function InternationalToAsciiScene(const aInput: String): String;
begin
  Result := aInput;

  // remove possible whitespace
  Result := Result.Replace(' ', '', [rfReplaceAll, SysUtils.rfIgnoreCase]);
  // remove scene delimiters
  Result := Result.Replace('.', '', [rfReplaceAll, SysUtils.rfIgnoreCase]);
  Result := Result.Replace('_', '', [rfReplaceAll, SysUtils.rfIgnoreCase]);
  // change special characters
  Result := Result.Replace('ÿ', 'y', [rfReplaceAll, SysUtils.rfIgnoreCase]);
  Result := Result.Replace('ü', 'ue', [rfReplaceAll, SysUtils.rfIgnoreCase]);
  Result := Result.Replace('ö', 'oe', [rfReplaceAll, SysUtils.rfIgnoreCase]);
  Result := Result.Replace('ï', 'i', [rfReplaceAll, SysUtils.rfIgnoreCase]);
  Result := Result.Replace('ë', 'e', [rfReplaceAll, SysUtils.rfIgnoreCase]);
  Result := Result.Replace('ä', 'ae', [rfReplaceAll, SysUtils.rfIgnoreCase]);
  Result := Result.Replace('À', 'a', [rfReplaceAll, SysUtils.rfIgnoreCase]);
  Result := Result.Replace('Á', 'a', [rfReplaceAll, SysUtils.rfIgnoreCase]);
  Result := Result.Replace('Â', 'a', [rfReplaceAll, SysUtils.rfIgnoreCase]);
  Result := Result.Replace('Ã', 'a', [rfReplaceAll, SysUtils.rfIgnoreCase]);
  Result := Result.Replace('Å', 'a', [rfReplaceAll, SysUtils.rfIgnoreCase]);
  Result := Result.Replace('Æ', 'ae', [rfReplaceAll, SysUtils.rfIgnoreCase]);
  Result := Result.Replace('Ç', 'c', [rfReplaceAll, SysUtils.rfIgnoreCase]);
  Result := Result.Replace('È', 'e', [rfReplaceAll, SysUtils.rfIgnoreCase]);
  Result := Result.Replace('É', 'e', [rfReplaceAll, SysUtils.rfIgnoreCase]);
  Result := Result.Replace('Ê', 'e', [rfReplaceAll, SysUtils.rfIgnoreCase]);
  Result := Result.Replace('Ì', 'i', [rfReplaceAll, SysUtils.rfIgnoreCase]);
  Result := Result.Replace('Í', 'i', [rfReplaceAll, SysUtils.rfIgnoreCase]);
  Result := Result.Replace('Î', 'i', [rfReplaceAll, SysUtils.rfIgnoreCase]);
  Result := Result.Replace('Ð', 'd', [rfReplaceAll, SysUtils.rfIgnoreCase]);
  Result := Result.Replace('Ñ', 'n', [rfReplaceAll, SysUtils.rfIgnoreCase]);
  Result := Result.Replace('Ò', 'o', [rfReplaceAll, SysUtils.rfIgnoreCase]);
  Result := Result.Replace('Ó', 'o', [rfReplaceAll, SysUtils.rfIgnoreCase]);
  Result := Result.Replace('Ô', 'o', [rfReplaceAll, SysUtils.rfIgnoreCase]);
  Result := Result.Replace('Õ', 'o', [rfReplaceAll, SysUtils.rfIgnoreCase]);
  Result := Result.Replace('Ø', 'o', [rfReplaceAll, SysUtils.rfIgnoreCase]);
  Result := Result.Replace('Œ', 'oe', [rfReplaceAll, SysUtils.rfIgnoreCase]);
  Result := Result.Replace('Ù', 'u', [rfReplaceAll, SysUtils.rfIgnoreCase]);
  Result := Result.Replace('Ú', 'u', [rfReplaceAll, SysUtils.rfIgnoreCase]);
  Result := Result.Replace('Û', 'u', [rfReplaceAll, SysUtils.rfIgnoreCase]);
  Result := Result.Replace('Ý', 'y', [rfReplaceAll, SysUtils.rfIgnoreCase]);
  //Result := Result.Replace('Þ', '', [rfReplaceAll, SysUtils.rfIgnoreCase]);
  Result := Result.Replace('Š', '', [rfReplaceAll, SysUtils.rfIgnoreCase]);
  Result := Result.Replace('C', '', [rfReplaceAll, SysUtils.rfIgnoreCase]);

  Debug(dpError, section, Format('[InternationalToAsciiScene] before: %s - after: %s', [aInput, Result]));
end;

function DeleteIMDbDataWithImdbId(const aIMDbId: String): boolean;
var
  FIMDbData: TIMDbDataRecord;
begin
  FIMDbData := TIMDbDataRecord.CreateAndFillPrepare(ImdbDatabase, 'IMDbID = ?', [], [aIMDbId]);
  try
    if FIMDbData.FillOne then
    begin
      ImdbDatabase.Delete(TIMDbReleaseDatesRecord, 'IMDbData = ?', [FIMDbData.ID]);
      ImdbDatabase.Delete(TIMDbBomDataRecord, 'IMDbData = ?', [FIMDbData.ID]);
      ImdbDatabase.Delete(TIMDbAlsoKnownAsRecord, 'IMDbData = ?', [FIMDbData.ID]);
      ImdbDatabase.Delete(TIMDbDataRecord, 'IMDbID=?', [aIMDbId]);
    end;
  finally
    FIMDbData.Free;
  end;
end;

function DeleteIMDbDataWithReleaseName(const aReleaseName: String): boolean;
var
  FIMDbData: TDbImdbData;
begin
  FIMDbData := GetImdbMovieData(aReleaseName);
  if (FIMDbData <> nil) then
  begin
    try
      DeleteIMDbDataWithImdbId(FIMDbData.imdb_id);
    finally
      FIMDbData.Free;
    end;
  end;
end;

function getCountryFromReleaseName(const aReleaseName: string; const countriesLst: TStringList): string;
var
  fLanguageFromReleasename, fStrHelper: string;
begin
  fLanguageFromReleasename := FindLanguageOnDirectory(aReleaseName);
  if (fLanguageFromReleasename <> 'English') then
    fStrHelper := fLanguageFromReleasename
  else
  begin
    if (countriesLst <> nil) then
      fStrHelper := TIMDbInfoChecks.EstimateEnglishCountryOrder(countriesLst.CommaText)
    else
      fStrHelper := 'USA';
  end;

  Result := TMapLanguageCountry.GetCountrynameByLanguage(fStrHelper);
end;

function GetTDbImdbDataFromRec(const aImdbRec: TIMDbDataRecord; const aReleaseName: string): TDbImdbData;
var
  fRegExpr: TRegexpr;
  fImdbMovieData: TDbImdbData;
  fLanguageFromReleasename, fStrHelper, fReleasenameCountry, fImdbRlsdateExtraInfo, fTvShowname, fStatusReason: string;
  fIsSTV, fIsFestival, fIsLimited, fIsWide: boolean;
  i, FIMDbCineyear, fTvSeason, fScreensCount: Integer;
  fTvEpisode: Int64;
  FIMDbReleaseDate: TDateTime;
  fIMDbReleaseDatesRecord: TIMDbReleaseDatesRecord;
  fStatusReasonList: TList<string>;
  fReleaseDateList: TObjectList<TIMDbReleaseDateInfo>;
  fIMDbReleaseDateInfo: TIMDbReleaseDateInfo;
  fIMDbBomData: TIMDbBomDataRecord;
begin
  Result := nil;
  fImdbMovieData := nil;

  try
    fImdbMovieData := TDbImdbData.Create(aImdbRec.IMDbID);
    fImdbMovieData.imdb_id := aImdbRec.IMDbID;
    fImdbMovieData.imdb_year := aImdbRec.IMDbYear;
    fImdbMovieData.imdb_origtitle := UTF8ToString(aImdbRec.IMDbTitle);

    fImdbMovieData.imdb_languages := aImdbRec.IMDbLanguages;
    fImdbMovieData.imdb_countries := aImdbRec.IMDbCountries;
    fImdbMovieData.imdb_genres := aImdbRec.IMDbGenres;

    fImdbMovieData.imdb_rating := aImdbRec.IMDbRating;
    fImdbMovieData.imdb_votes := aImdbRec.IMDbVotes;
    fImdbMovieData.imdb_cineyear := aImdbRec.IMDbCineyear;

    fImdbMovieData.UpdatedTime := aImdbRec.UpdatedTime;

    fIsSTV := False;
    fIsLimited := False;
    fIsWide := True;
    fIsFestival := False;
    fScreensCount := 0;

    fStatusReasonList := TList<String>.Create;

    (* Check global STV status based on title *)
    fIsSTV := TIMDbInfoChecks.IsSTVBasedOnTitleExtraInfo(aImdbRec.FIMDbTitleExtras);
    if fIsSTV then
    begin
      fStatusReasonList.Add(Format('STV due to title extra info: %s', [aImdbRec.FIMDbTitleExtras]));
      Debug(dpSpam, section, Format('Status from Releasename: %s', [fStatusReasonList.Last]));
    end;

    (* Check if it's a TV release *)
    // if we get values for season or episode -> tv show which doesn't has any screens
    getShowValues(aReleaseName, fTvShowname, fTvSeason, fTvEpisode);
    if not((fTvSeason > 0) or (fTvEpisode > 0) or (fTvSeason = Ord(tvDatedShow))
      or (fTvSeason = Ord(tvRegularSerieWithoutSeason)) or
      (fTvEpisode = Ord(tvNoEpisodeTag))) then
    begin
      fIsSTV := True;
      fStatusReasonList.Add(Format('STV due to being a TV show with season %d and/or episode %d', [fTvSeason, fTvEpisode]));
      Debug(dpSpam, section, Format('Status from Releasename: %s', [fStatusReasonList.Last]));
    end;

    fReleasenameCountry := getCountryFromReleaseName(aReleaseName, fImdbMovieData.imdb_countries);
    Debug(dpSpam, section, Format('Release language %s maps to country %s', [fLanguageFromReleasename, fReleasenameCountry]));
    if fReleasenameCountry = '' then
    begin
      fStrHelper := '[ERROR] No mapping for language to a country found';
      Debug(dpError, section, fStrHelper);

      fIsSTV := True;
      fStatusReasonList.Add('STV because ' + fStrHelper);

      fIsFestival := True;
      fStatusReasonList.Add('Festival because ' + fStrHelper);

      FIMDbCineyear := 0;
      fStatusReasonList.Add('NOT Cine ' + fStrHelper);
    end
    else
    begin

      // get the release info from DB
      fReleaseDateList := TObjectList<TIMDbReleaseDateInfo>.Create(True);
      fIMDbReleaseDatesRecord := TIMDbReleaseDatesRecord.CreateAndFillPrepare(ImdbDatabase, 'IMDbData = ?', [], [aImdbRec.ID]);

      while fIMDbReleaseDatesRecord.FillOne do
      begin
        fReleaseDateList.Add(TIMDbReleaseDateInfo.Create(fIMDbReleaseDatesRecord.IMDbCountry, fIMDbReleaseDatesRecord.IMDbReleaseDateExtraInfo, fIMDbReleaseDatesRecord.IMDbReleaseDate));
      end;

      (* STV *)
      for fIMDbReleaseDateInfo in fReleaseDateList do
      begin
        if fIMDbReleaseDateInfo.FCountry = fReleasenameCountry then
        begin
          FIMDbReleaseDate := fIMDbReleaseDateInfo.FReleaseDate;
          fImdbRlsdateExtraInfo := fIMDbReleaseDateInfo.FExtraInfo;

          if fImdbRlsdateExtraInfo <> '' then
          begin
            fRegExpr := TRegexpr.Create;
            try
              fRegExpr.ModifierI := True;

              fRegExpr.Expression := '(DVD|video|TV|Bluray|Blueray)(\s|\.|\-)?premiere';
              if fRegExpr.Exec(fImdbRlsdateExtraInfo) then
              begin
                fIsSTV := True;
                fStatusReasonList.Add(Format('STV in %s due to %s on ', [fReleasenameCountry, fImdbRlsdateExtraInfo]) + FormatDateTime('dddddd', FIMDbReleaseDate));
                Debug(dpSpam, section, Format('Status from Releasepage: %s', [fStatusReasonList.Last]));
                break;
              end;
            finally
              fRegExpr.Free;
            end;
          end;
        end;
      end;

      (* Festival info is independent from STV/Limited/Wide -> screened on a festival for movie enthuastics *)
      for fIMDbReleaseDateInfo in fReleaseDateList do
      begin
        if fIMDbReleaseDateInfo.FCountry = fReleasenameCountry then
        begin
          FIMDbReleaseDate := fIMDbReleaseDateInfo.FReleaseDate;
          fImdbRlsdateExtraInfo := fIMDbReleaseDateInfo.FExtraInfo;

          if fImdbRlsdateExtraInfo <> '' then
          begin
            fRegExpr := TRegexpr.Create;
            try
              fRegExpr.ModifierI := True;

              fRegExpr.Expression := 'F(estival|ilmfest|est|ilm(\s|\.|\-)?Market?)';
              if fRegExpr.Exec(fImdbRlsdateExtraInfo) then
              begin
                fIsFestival := True;
                fStatusReasonList.Add(Format('Festival in %s due to %s on ', [fReleasenameCountry, fImdbRlsdateExtraInfo]) + FormatDateTime('dddddd', FIMDbReleaseDate));
                Debug(dpSpam, section, Format('Status from Releasepage: %s', [fStatusReasonList.Last]));
                break;
              end;
            finally
              fRegExpr.Free;
            end;
          end;
        end;
      end;

      (* Cinedate info *)
      if not fIsSTV then
      begin
        // pick first entry where third row (extra info) is empty
        for fIMDbReleaseDateInfo in fReleaseDateList do
        begin
          if fIMDbReleaseDateInfo.FCountry = fReleasenameCountry then
          begin
            FIMDbReleaseDate := fIMDbReleaseDateInfo.FReleaseDate;
            fImdbRlsdateExtraInfo := fIMDbReleaseDateInfo.FExtraInfo;

            if fImdbRlsdateExtraInfo = '' then
            begin
              FIMDbCineyear := DateUtils.YearOf(FIMDbReleaseDate);
              fStatusReasonList.Add(Format('Cine year for %s is %d taken from ', [fReleasenameCountry, FIMDbCineyear]) + FormatDateTime('dddddd', FIMDbReleaseDate));
              Debug(dpSpam, section, Format('Status from Releasepage: %s', [fStatusReasonList.Last]));
              break;
            end;
          end;
        end;
      end;
      { NOTE: all that needs to be done separately for each dedicated releasename }

      fIMDbBomData := TIMDbBomDataRecord.CreateAndFillPrepare(ImdbDatabase, 'IMDbData = ?', [], [aImdbRec.ID]);
      while fIMDbBomData.FillOne do
      begin
        if fIMDbBomData.IMDbCountry = fReleasenameCountry then
        begin
          fScreensCount := fIMDbBomData.IMDbScreens;
          break;
        end;
      end;

      (* Check screen count *)
      if fScreensCount = 0 then
      begin
        fIsSTV := True;
        fIsLimited := False;
        fIsWide := False;
        fStatusReasonList.Add(Format('STV due to screens count being zero for %s', [fReleasenameCountry]));
        Debug(dpSpam, section, Format('Status from Screen count: %s', [fStatusReasonList.Last]));
      end
      else if (fScreensCount < 600) then
      begin
        // limited release when playing at fewer than 600 theaters
        fIsSTV := False;
        fIsLimited := True;
        fIsWide := False;
        fStatusReasonList.Add(Format('Limited due to screens %d < 600 for %s', [fScreensCount, fReleasenameCountry]));
        Debug(dpSpam, section, Format('Status from Screen count: %s', [fStatusReasonList.Last]));
      end
      else
      begin
        // movie in wide release or about to go wide when it is playing at 600 or more theaters
        fIsSTV := False;
        fIsLimited := False;
        fIsWide := True;
        fStatusReasonList.Add(Format('Wide due to screens %d => 600 for %s', [fScreensCount, fReleasenameCountry]));
        Debug(dpSpam, section, Format('Status from Screen count: %s', [fStatusReasonList.Last]));
      end;

    end;

    for i := 0 to fStatusReasonList.Count - 1 do
    begin
      fStatusReason := fStatusReason + Format('%d - %s%s', [i + 1, fStatusReasonList[i], #13#10]);
    end;

    fImdbMovieData.imdb_ldt := fIsLimited;
    fImdbMovieData.imdb_wide := fIsWide;
    fImdbMovieData.imdb_festival := fIsFestival;
    fImdbMovieData.imdb_stvm := fIsSTV;
    fImdbMovieData.imdb_stvs := fStatusReason;

    Result := fImdbMovieData;
  finally
    fIMDbReleaseDatesRecord.Free;
    fStatusReasonList.Free;
    fReleaseDateList.Free;
    fIMDbBomData.Free;
  end;
end;

function FindImdbDataByImdbId(const aIMDbId, aReleaseName: String): TDbImdbData;
var
  fImdbRec: TIMDbDataRecord;
begin
  Result := nil;
  fImdbRec := TIMDbDataRecord.CreateAndFillPrepare(ImdbDatabase, 'IMDbID = ?', [], [aIMDbId]);
  fImdbRec.IMDbLanguages := TStringList.Create;
  fImdbRec.IMDbcountries := TStringList.Create;
  fImdbRec.IMDbGenres := TStringList.Create;
  try
    if fImdbRec.FillOne then
      Result := GetTDbImdbDataFromRec(fImdbRec, aReleaseName);

    if fImdbRec.FillOne then
    begin
      // it's not unique
      Result.Free;
      Result := nil;
      raise Exception.Create('No unique IMDB entry in database found.');
    end;
  finally
    fImdbRec.Free;
  end;
end;

Function foundMovieAlreadyInDbWithImdbId(const aIMDbId: String): boolean;
var
  fId_FIMDbData: TIMDbDataRecord;
begin
  Result := False;
  fId_FIMDbData := TIMDbDataRecord.CreateAndFillPrepare(ImdbDatabase, 'IMDbID = ?', [], [aIMDbId]);
  try
    Result := fId_FIMDbData.FillOne;
  finally
    fId_FIMDbData.Free;
  end;
end;

Function getNbrOfImdbEntries: Integer;
begin
  Result := ImdbDatabase.TableRowCount(TIMDbDataRecord);
end;

Function foundMovieAlreadyInDbWithReleaseName(const aReleaseName: String): boolean;
var
  fDbImdbData: TDbImdbData;
begin
  Result := False;

  try
    fDbImdbData := GetImdbMovieData(aReleaseName);
    Result := fDbImdbData <> nil;
  finally
    fDbImdbData.Free;
  end;
end;

function UpdateMovieInDbWithImdbDataNeeded(const aImdbData: TDbImdbData): Boolean;
begin
  Result := DaysBetween(now, aImdbData.UpdatedTime) >= config.ReadInteger(section, 'update_time_in_days', 7);
end;

Function UpdateMovieInDbWithReleaseNameNeeded(const aReleasename: String): Boolean;
var
  fImdbData: TDbImdbData;
begin
  Result := True;

  try
    fImdbData := GetImdbMovieData(aReleasename);
    if (fImdbData <> nil) and not UpdateMovieInDbWithImdbDataNeeded(fImdbData) then
      Result := False;
  finally
    fImdbData.Free;
  end;
end;

procedure dbaddimdb_SaveImdb(const aReleaseName, aIMDbId: String);
var
  i: Integer;
  showname: String;
  season: Integer;
  episode: int64;
  fImdbData: TDbImdbData;
begin
  if config.ReadBool(section, 'skip_tv_releases', false) then
  begin
    getShowValues(aReleaseName, showname, season, episode);
    (*
      if getShowValues does not find tv-related info for rls then showname will
      contain the same value as rls, otherwise it will contain a (shorter)
      showname. season and/or episode will be set if the release is tv-related.
    *)
    if (aReleaseName <> showname) and ((season > 0) or (episode > 0)) then
      exit;
  end;
    irc_AddInfo(Format('<c7>[iMDB]</c> for <b>%s</b> : %s', [aReleaseName, aIMDbId]));
    irc_Addtext_by_key('ADDIMDBECHO', '!addimdb '+aReleaseName+' '+aIMDbId);

    //it might happen, that we have the IMDB ID in our DB, but could not find it with the rls title. So check
    //if we have the ID in the DB and if so, use the data that we have.
    fImdbData := FindImdbDataByImdbId(aIMDbId, aReleaseName);
    if fImdbData <> nil then
    begin
      try
        //invoke SaveImdbData so that the 'also known as' entry will be created and fire kb will be done.
        dbaddimdb_SaveImdbData(aReleaseName, fImdbData, nil, nil, nil);
        exit;
      finally
        fImdbData.Free;
      end;
    end;

    try
      CreateHttpTask(aReleaseName, aIMDbId);
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] dbaddimdb_SaveImdb (Parse): %s', [e.Message]));
        exit;
      end;
  end;
end;

procedure dbaddimdb_ProcessImdbData(const aReleaseName: String; aImdbData: TDbImdbData);
begin
 if config.ReadBool(section, 'post_lookup_infos', false) then
  begin
    irc_AddInfo(Format('<c7>[iMDB Data]</c> for <b>%s</b> : %s', [aReleaseName, aImdbData.imdb_id]));
    aImdbData.PostResults(aReleaseName);
  end;

  try
    dbaddimdb_FireKbAdd(aReleaseName);
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] dbaddimdb_ProcessImdbData (FireKbAdd): %s', [e.Message]));
      exit;
    end;
  end;
end;

procedure dbaddimdb_SaveImdbData(const aReleaseName: String; aImdbData: TDbImdbData; aIMDbAlsoKnownAsInfoList: TObjectList<TIMDbAlsoKnownAsInfo>; aImdbReleaseDateInfoList: TObjectList<TIMDbReleaseDateInfo>; aBOMCountryScreens: TDictionary<string, Integer>);
var
  fIMDbDataRec:   TIMDbDataRecord;
  fIMDbReleaseDatesRecordRec: TIMDbReleaseDatesRecord;
  fIMDbAlsoKnownAsRecordRec: TIMDbAlsoKnownAsRecord;
  fIMDbBomDataRecordRec: TIMDbBomDataRecord;
  fDoUpdate: boolean;
  fStrHelper, fCleanedMovieName, fLanguageFromReleasename, fReleasenameCountry: string;
  fIMDbAlsoKnownAsInfo: TIMDbAlsoKnownAsInfo;
  fIMDbReleaseDateInfo: TIMDbReleaseDateInfo;
  fBOMCountryScreens: TPair<string, integer>;
begin

  try
    fIMDbDataRec := TIMDbDataRecord.CreateAndFillPrepare(ImdbDatabase, 'IMDbID = ?', [], [aImdbData.imdb_id]);
    fDoUpdate := fIMDbDataRec.FillOne;

    if not fDoUpdate then
    begin
      fIMDbDataRec.CreationTime := now;
      fIMDbDataRec.IMDbID := aImdbData.imdb_id;
    end;

    fIMDbDataRec.IMDbTitle := StringToUTF8(aImdbData.imdb_origtitle);
    fIMDbDataRec.IMDbTitleCleaned := getMovieNameWithoutSceneTags(StringToUTF8(aImdbData.imdb_origtitle));
    fIMDbDataRec.IMDbTitleExtras := '';
    fIMDbDataRec.IMDbYear := aImdbData.imdb_year;
    fIMDbDataRec.IMDbCineyear := aImdbData.imdb_cineyear;
    fIMDbDataRec.IMDbRating := aImdbData.imdb_rating;
    fIMDbDataRec.IMDbVotes := aImdbData.imdb_votes;

    fIMDbDataRec.IMDbLanguages := aImdbData.imdb_languages;
    fIMDbDataRec.IMDbCountries := aImdbData.imdb_countries;
    fIMDbDataRec.IMDbGenres := aImdbData.imdb_genres;

    fIMDbDataRec.UpdatedTime := now;

    if fDoUpdate then
      fDoUpdate := ImdbDatabase.Update(fIMDbDataRec)
    else
      ImdbDatabase.Add(fIMDbDataRec, True);

    fReleasenameCountry := getCountryFromReleaseName(aReleaseName, aImdbData.imdb_countries);

    if (fReleasenameCountry = '') then
      fReleasenameCountry := '(original title)';

    fCleanedMovieName := getMovieNameWithoutSceneTags(aReleaseName);
    fIMDbAlsoKnownAsRecordRec := TIMDbAlsoKnownAsRecord.CreateAndFillPrepare(ImdbDatabase, 'IMDbData = ? and IMDbTitleCleaned = ? and Country = ?', [], [fIMDbDataRec.ID, fCleanedMovieName, fReleasenameCountry]);
    if not fIMDbAlsoKnownAsRecordRec.FillOne then
    begin

      // no aka entry found - create one.
      fIMDbAlsoKnownAsRecordRec := TIMDbAlsoKnownAsRecord.Create;
      fIMDbAlsoKnownAsRecordRec.Country := fReleasenameCountry;
      fIMDbAlsoKnownAsRecordRec.IMDbTitleCleaned := fCleanedMovieName;
      fIMDbAlsoKnownAsRecordRec.FromImdb := False;
      fIMDbAlsoKnownAsRecordRec.IMDbData := fIMDbDataRec.AsTSQLRecord;
      ImdbDatabase.Add(fIMDbAlsoKnownAsRecordRec, True);
    end;

    // delete existing entries if we do update
    if fDoUpdate then
    begin
      if (aImdbReleaseDateInfoList <> nil) then
        ImdbDatabase.Delete(TIMDbReleaseDatesRecord, 'IMDbData=?', [fIMDbDataRec.ID]);
      if (aBOMCountryScreens <> nil) then
        ImdbDatabase.Delete(TIMDbBomDataRecord, 'IMDbData=?', [fIMDbDataRec.ID]);
      if (aIMDbAlsoKnownAsInfoList <> nil) then
        ImdbDatabase.Delete(TIMDbAlsoKnownAsRecord, 'IMDbData=? and FromImdb=?', [fIMDbDataRec.ID, True]);
    end;

    if (aIMDbAlsoKnownAsInfoList <> nil) then
    begin
      for fIMDbAlsoKnownAsInfo in aIMDbAlsoKnownAsInfoList do
      begin
        fIMDbAlsoKnownAsRecordRec := TIMDbAlsoKnownAsRecord.Create;
        fIMDbAlsoKnownAsRecordRec.Country := fIMDbAlsoKnownAsInfo.Country;
        fIMDbAlsoKnownAsRecordRec.IMDbTitleCleaned := getMovieNameWithoutSceneTags(fIMDbAlsoKnownAsInfo.Title);
        fIMDbAlsoKnownAsRecordRec.FromImdb := True;
        fIMDbAlsoKnownAsRecordRec.IMDbData := fIMDbDataRec.AsTSQLRecord;
        ImdbDatabase.Add(fIMDbAlsoKnownAsRecordRec, True);
      end;
    end;

    if (aImdbReleaseDateInfoList <> nil) then
    begin
      for fIMDbReleaseDateInfo in aImdbReleaseDateInfoList do
      begin
        fIMDbReleaseDatesRecordRec := TIMDbReleaseDatesRecord.Create;
        fIMDbReleaseDatesRecordRec.IMDbCountry := fIMDbReleaseDateInfo.Country;
        fIMDbReleaseDatesRecordRec.IMDbReleaseDate := fIMDbReleaseDateInfo.ReleaseDate;
        fIMDbReleaseDatesRecordRec.IMDbReleaseDateExtraInfo := fIMDbReleaseDateInfo.ExtraInfo;
        fIMDbReleaseDatesRecordRec.IMDbData := fIMDbDataRec.AsTSQLRecord;
        ImdbDatabase.Add(fIMDbReleaseDatesRecordRec, True);
      end;
    end;

    if (aBOMCountryScreens <> nil) then
    begin
      for fBOMCountryScreens in aBOMCountryScreens do
      begin
        fIMDbBomDataRecordRec := TIMDbBomDataRecord.Create;
        fIMDbBomDataRecordRec.IMDbCountry := fBOMCountryScreens.Key;
        fIMDbBomDataRecordRec.IMDbScreens := fBOMCountryScreens.Value;
        fIMDbBomDataRecordRec.IMDbData := fIMDbDataRec.AsTSQLRecord;
        ImdbDatabase.Add(fIMDbBomDataRecordRec, True);
      end;
    end;
  finally
    fIMDbDataRec.Free;
    //fIMDbReleaseDatesRecordRec.Free;
    //fIMDbAlsoKnownAsRecordRec.Free;
    //fIMDbBomDataRecordRec.Free;
  end;

  dbaddimdb_ProcessImdbData(aReleaseName, aImdbData);
End;

function GetImdbMovieData(const aReleaseName: String): TDbImdbData;
var
  fAlsoKnownDataRec: TIMDbAlsoKnownAsRecord;
  fMovieImdbDataRec: TIMDbDataRecord;
  fImdbMovieData: TDbImdbData;
  fCleanedMovieName, fReleasenameCountry: string;
  fRelease: TRelease;
  fReleaseYear: integer;
begin
  Result := nil;
  fImdbMovieData := nil;
  fMovieImdbDataRec := nil;
  fAlsoKnownDataRec := nil;
  fRelease := nil;

  try
    fCleanedMovieName := getMovieNameWithoutSceneTags(aReleaseName);

    // TODO: extract function to get the year from the release name so we don't have to create a TRelease at this place
    fRelease := TRelease.Create(aReleaseName, 'IMDB', False);
    fReleasenameCountry := getCountryFromReleaseName(aReleaseName, nil);
    fReleaseYear := fRelease.year;
    if (fReleaseYear = 0) then
      fReleaseYear := SysUtils.CurrentYear;

    if (fReleasenameCountry = '') then
      fReleasenameCountry := '(original title)';

    fMovieImdbDataRec := TIMDbDataRecord.CreateAndFillPrepare(ImdbDatabase,
      'IMDbTitleCleaned = ? and IMDbYear = ?', [],
      [fCleanedMovieName, fRelease.year]);
    fMovieImdbDataRec.FIMDbCountries := TStringList.Create;
    fMovieImdbDataRec.FIMDbLanguages := TStringList.Create;
    fMovieImdbDataRec.FIMDbGenres := TStringList.Create;

    if not fMovieImdbDataRec.FillOne then
    begin
      fAlsoKnownDataRec := TIMDbAlsoKnownAsRecord.CreateAndFillPrepareJoined(ImdbDatabase,
        'IMDbAlsoKnownAsRecord.IMDbTitleCleaned = ? and IMDbAlsoKnownAsRecord.Country = ? and IMDbData.IMDbYear = ?',
        [], [fCleanedMovieName, fReleasenameCountry, fReleaseYear]);

      if fAlsoKnownDataRec.FillOne then
      begin
        fMovieImdbDataRec := TIMDbDataRecord.CreateAndFillPrepare(ImdbDatabase, 'ID = ?', [], [fAlsoKnownDataRec.IMDbData.ID]);
        fMovieImdbDataRec.FIMDbCountries := TStringList.Create;
        fMovieImdbDataRec.FIMDbLanguages := TStringList.Create;
        fMovieImdbDataRec.FIMDbGenres := TStringList.Create;
        fMovieImdbDataRec.FillOne;
      end
      else
        Exit;
    end;

    Result := GetTDbImdbDataFromRec(fMovieImdbDataRec, aReleaseName);
  finally
    fMovieImdbDataRec.Free;
    //fAlsoKnownDataRec.Free;
    fRelease.Free;
  end;
end;

function dbaddimdb_Process(aNet, aChan, aNick, aMsg: String): Boolean;
var
  fRls: String;
  fImdbId: String;
  fUpdateNeeded: boolean;
  fRlsFound: boolean;
begin
  Result := False;

  if (1 = Pos(addimdbcmd, aMsg)) then
  begin
    aMsg := Copy(aMsg, length(addimdbcmd + ' ') + 1, 1000);

    fRls := '';
    fRls := SubString(aMsg, ' ', 1);
    fImdbId := '';
    fImdbId := SubString(aMsg, ' ', 2);

    if not check_ImdbId(fImdbId) then
    begin
      Debug(dpError, section, '[ADDIMDB] Invalid IMDB ID for %s: %s', [fRls, fImdbId]);
      exit;
    end;

    if ((fRls <> '') and (fImdbId <> '')) then
    begin
      fUpdateNeeded := UpdateMovieInDbWithReleaseNameNeeded(fRls);
      fRlsFound := foundMovieAlreadyInDbWithReleaseName(fRls);
      if (not fUpdateNeeded AND fRlsFound) then
      begin
        exit;
      end;

      try
        dbaddimdb_SaveImdb(fRls, fImdbId);
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('Exception in dbaddimdb_Process (SaveImdb): %s', [e.Message]));
          exit;
        end;
      end;
    end;

    Result := True;
  end;
end;

procedure CreateHttpTask(const aReleaseName, aIMDbId: String);
var fTask : TPazoHTTPImdbTask;
begin
  try
    fTask := TPazoHTTPImdbTask.Create(aIMDbId, aReleaseName);
    //if not TaskAlreadyInQueue(fTask) then
      AddTask(fTask);
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[Exception] in CreateHttpTask AddTask: %s', [e.Message]));
      exit;
    end;
  end;
end;

procedure dbaddimdb_FireKbAdd(const aReleaseName : String);
var p : TPazo;
    ps: TPazoSite;
    last_addimdb_count: Integer;
begin
  try
    p:= FindPazoByRls(aReleaseName);
    if (p <> nil) then
    begin
      if p.rls is TIMDBRelease then
      begin
        p.rls.Aktualizal(p);
        ps := FindMostCompleteSite(p);
        if ((ps = nil) and (p.PazoSitesList.Count > 0)) then
          ps:= TPazoSite(p.PazoSitesList[0]);

        if (ps <> nil) then
        begin
          if spamcfg.ReadBool('addinfo','imdbupdate',True) then
            irc_SendUPDATE(Format('<c3>[ADDIMDB]</c> %s %s now has iMDB infos (%s)', [p.rls.section, p.rls.rlsname, ps.name]));
          kb_Add('', '', ps.name, p.rls.section, '', kbeUPDATE, p.rls.rlsname, '');
        end;
      end;
    end;

    gDbAddimdb_cs.Enter;
    try
      last_addimdb_count := last_addimdb.Count;
      try
        while last_addimdb_count > 15 do
        begin
          last_addimdb.Delete(0);
          last_addimdb_count := last_addimdb.Count - 1;
        end;
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[EXCEPTION] dbaddimdb_FireKbAdd (cleanup): %s', [e.Message]));
          exit;
        end;
      end;
    finally
      gDbAddimdb_cs.Leave;
    end;

  except
    on e: Exception do
    begin
      Debug(dpError, section, '[EXCEPTION] dbaddimdb_FireKbAdd : %s', [e.Message]);
    end;
  end;
end;

{ Check if IMDBId is in good structure }
function check_ImdbId(const aIMDbId: String): Boolean;
begin
  Result := False;
    try
      if rx_imdbid.Find(aIMDbId) <> 0 then
      begin
        Result := True;
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] dbaddimdb_checkid: Exception : %s', [e.Message]));
        exit;
      end;
    end;
end;

{ Parseid }
function parseImdbIDFromString(const aText: String; out aIMDbId: String): Boolean;
begin
  aIMDbId := '';
  Result := False;
  try
    try
      if rx_imdbid.MatchAll(aText, rx_captures, 1 ,1) then
      begin
        aIMDbId := Copy(aText, rx_captures[0][0].Start, rx_captures[0][0].Length);
        Result := True;
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] dbaddimdb_checkid: Exception : %s', [e.Message]));
        exit;
      end;
    end;
  finally
    SetLength(rx_captures, 0);
  end;
end;


{ Status }
function dbaddimdb_Status: String;
begin
  Result := Format('<b>iMDB Movie Infos</b>: %d',[getNbrOfImdbEntries()]);
end;

{ Backup Database}
procedure doIMDbDbBackup(const aPath, aFileName: String);
begin
  if IMDbDatabase.DB.BackupBackground(aPath + aFileName, -1, 0, nil) then
    IMDbDatabase.DB.BackupBackgroundWaitUntilFinished(5);
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
  imdb_remove_words_list.LoadFromFile(ExtractFilePath(ParamStr(0)) + IMDBREPLACEFILENAME);

  gDbAddimdb_cs := TCriticalSection.Create;
  last_addimdb:= THashedStringList.Create;
  last_addimdb.CaseSensitive:= False;
  last_addimdb.OwnsObjects:= true;

  ImdbDBModel := CreateIMDBModel;
  try
    ImdbDatabase := CreateORMSQLite3DB(ImdbDBModel, fDBName, '');
    Console_Addline('', Format('IMDb db loaded. %d Movies', [ImdbDatabase.TableRowCount(TIMDbDataRecord)]));
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] dbaddimdbInit: %s', [e.Message]));
      exit;
    end;
  end;

  rx_imdbid := TFLRE.Create('tt(\d{6,8})', [rfIGNORECASE]);
  addimdbcmd := config.ReadString(section, 'addimdbcmd', '!addimdb');

  glLanguageCountryMappingList := TObjectList<TMapLanguageCountry>.Create(True);
  fStrList := TStringList.Create;
  try
    fStrList.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'slftp.imdbcountries');
    for i := 0 to fStrList.Count - 1 do
    begin
      (* ignore inifile section and comments *)
      if fStrList.Strings[i].StartsWith('[') then
        Continue;
      if fStrList.Strings[i].Contains('//') then
        Continue;
      if fStrList.Strings[i].Contains('#') then
        Continue;

      fLang := Trim(SubString(fStrList.Strings[i], '=', 1));
      fHelper := SubString(fStrList.Strings[i], '=', 2);
      fCC := Trim(SubString(fHelper, ',', 1));
      fCountry := Trim(SubString(fHelper, ',', 2));

      fDupe := False;
      for fItem in glLanguageCountryMappingList do
      begin
        // IMDb and BOM differ for that language, see config file
        if fLang = 'Czech' then
          Continue;
        if fItem.Language = fLang then
        begin
          Debug(dpError, section, Format('Ignoring language %s with country %s-%s from slftp.imdbcountries because it already exists', [fLang, fCC, fCountry]));
          fDupe := True;
        end;
      end;

      if fDupe then
        Continue;

      glLanguageCountryMappingList.Add(TMapLanguageCountry.Create(fLang, fCC, fCountry));
    end;
  finally
    fStrList.Free;
  end;
end;

procedure dbaddimdbUninit;
begin
  try
    if Assigned(ImdbDatabase) then
    begin
      ImdbDatabase.Free;
    end;
    if Assigned(ImdbDbModel) then
    begin
      ImdbDbModel.Free;
    end;
    FreeAndNil(imdb_remove_words_list);
    //FreeAndNil(addimdbcmd);
    FreeAndNil(imdbcountries);
    FreeAndNil(rx_imdbid);
    FreeAndNil(last_addimdb);
    FreeAndNil(gDbAddimdb_cs);
    FreeAndNil(glLanguageCountryMappingList);
  finally
    Debug(dpSpam, section, 'Uninit ImdbDatabase');
  end;
end;

function ExcludeCountry(const aCountryname: String): Boolean;
var
  fItem: TMapLanguageCountry;
begin
  Result := True;
  for fItem in glLanguageCountryMappingList do
     begin
    if (fItem.Country = 'UK') or (fItem.Country = 'USA') then
    begin
      // to avoid matching Ukraine with UK
      if aCountryname.StartsWith(fItem.Country, False) then
        Exit(False);
    end
    else
    begin
      // match things like 'Canada (French title)' and 'Canada (English title)'
      if aCountryname.StartsWith(fItem.Country, True) then
        Exit(False);
    end;
  end;
end;

{ TDbImdbData }
constructor TDbImdbData.Create(const aIMDbId:String);
begin
  self.imdb_id := imdb_id;
  imdb_languages:= TStringList.Create;
  imdb_countries:= TStringList.Create;
  imdb_genres:= TStringList.Create;
end;

destructor TDbImdbData.Destroy;
begin
  imdb_languages.Free;
  imdb_countries.Free;
  imdb_genres.Free;
  inherited;
end;

procedure TDbImdbData.PostResults(const aRls : String = '');
var status: String;
begin

  if imdb_stvm then status := 'STV'
  else if imdb_festival then status := 'Festival'
  else if imdb_ldt then status := 'Limited'
  else if imdb_wide then status := 'Wide'
  else status :='Cine';

  irc_Addstats(Format('(<c9>i</c>).....<c2><b>IMDB</b></c>........ <c0><b>for : %s</b></c> .......: https://www.imdb.com/title/%s/',[aRls, imdb_id]));
  irc_Addstats(Format('(<c9>i</c>).....<c2><b>IMDB</b></c>........ <c0><b>Original Title - Year</b></c> ...: %s (%d)',[imdb_origtitle, imdb_year]));
  irc_Addstats(Format('(<c9>i</c>).....<c2><b>IMDB</b></c>........ <b><c9>Country - Languages</b></c> ..: %s - %s',[imdb_countries.DelimitedText,imdb_languages.DelimitedText]));
  irc_Addstats(Format('(<c9>i</c>).....<c2><b>IMDB</b></c>........ <b><c5>Genres</b></c> .........: %s', [imdb_genres.DelimitedText]));
  irc_Addstats(Format('(<c9>i</c>).....<c2><b>IMDB</b></c>........ <c7><b>Rating</b>/<b>Type</b></c> ....: <b>%d</b> of 100 (%d) @ %d Screens (%s)',[imdb_rating,imdb_votes,imdb_screens,status]));
end;

end.
