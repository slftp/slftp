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

TIMDbBoomData = class(TSQLRecordNoCase)
  private
    FIMDbCountry: RawUTF8; //< country from releaseinfo page
    // TODO: might need rework as IMDb don't give infos for it anymore?
    // TODO: therefore some infos are taken from BOM but in the future maybe other sites are supported as well
    FIMDbScreens: Integer; //< screens
    FIMDbLimited: Boolean; //< @true if limited, @false otherwise
    FIMDbWide: Boolean; //< @true if wide, @false otherwise
    FIMDbFestival: Boolean; //< @true if aired on a festival, @false otherwise
    FIMDbSTV: Boolean; //< @true if STV Movie, @false otherwise
    FIMDbSTVReason: RawUTF8; //< reason for STV status
  published
    property IMDbCountry: RawUTF8 read FIMDbCountry write FIMDbCountry;
    property IMDbScreens: Integer read FIMDbScreens write FIMDbScreens;
    property IMDbLimited: Boolean read FIMDbLimited write FIMDbLimited;
    property IMDbWide: Boolean read FIMDbWide write FIMDbWide;
    property IMDbFestival: Boolean read FIMDbFestival write FIMDbFestival;
    property IMDbSTV: Boolean read FIMDbSTV write FIMDbSTV;
    property IMDbSTVReason: RawUTF8 read FIMDbSTVReason write FIMDbSTVReason;
  end;

  // data should be filtered to sort out countries which never get a release
  TIMDbReleaseDatesRecord = class(TSQLRecordNoCase)
  private
    FIMDbCountry: RawUTF8; //< country from releaseinfo page
    FIMDbReleaseDate: TDateTime; //< release date from releaseinfo page
    FIMDbReleaseDateExtraInfo: RawUTF8; //< additional info from releaseinfo page
  published
    property IMDbCountry: RawUTF8 read FIMDbCountry write FIMDbCountry;
    property IMDbReleaseDate: TDateTime read FIMDbReleaseDate write FIMDbReleaseDate;
    property IMDbReleaseDateExtraInfo: RawUTF8 read FIMDbReleaseDateExtraInfo write FIMDbReleaseDateExtraInfo;
  end;

  TIMDbData = class(TSQLRecord)
  private
    FIMDbID: RawUTF8; //< IMDb title ID like tt12345
    FIMDbTitle: RawUTF8; //< title from webpage
    FIMDbTitleExtras: RawUTF8; //< extra info from title if available, e.g. TV* or VIDEO* stuff
    FIMDbYear: Integer; //< movie year
    FIMDbCineyear: Integer; //< cinema year (TODO: which value if not available?)
    FIMDbRating: Integer; //< rating (multiplied by 10)
    FIMDbVotes: Integer; //< votes

    // save as comma separated list and create TStringList only in TImdbRelease for backward compatibility of rules checking code
    FIMDbLanguages: TStringList; //< movie languages
    FIMDbCountries: TStringList; //< countries
    FIMDbGenres: TStringList; //< genres


    //FIMDbAlsoKnownAsData: TIMDbAlsoKnownAsRecord; //< AKA info from releaseinfo page
    // code has to select the appropriate country info depending on the release language of the release
    FIMDbReleaseInfos: TIMDbReleaseDatesRecord; //< all release dates information
    // has all Website-specific Infos for Boom
    FIMDbBoomInfos: TIMDbBoomData; //< all release dates information

    FCreationTime: TDateTime; //< time when the data was fetched
    FUpdatedTime: TDateTime;
  published
    //property IMDbData: TID read fIMDbDataId write fIMDbDataId;
    property IMDbID: RawUTF8 read FIMDbID write FIMDbID stored AS_UNIQUE;
    property IMDbTitle: RawUTF8 read FIMDbTitle write FIMDbTitle;
    property IMDbTitleExtras: RawUTF8 read FIMDbTitleExtras write FIMDbTitleExtras;
    property IMDbYear: Integer read FIMDbYear write FIMDbYear;
    property IMDbCineyear: Integer read FIMDbCineyear write FIMDbCineyear;
    property IMDbRating: Integer read FIMDbRating write FIMDbRating;
    property IMDbVotes: Integer read FIMDbVotes write FIMDbVotes;

    property IMDbLanguages: TStringList read FIMDbLanguages write FIMDbLanguages;
    property IMDbCountries: TStringList read FIMDbCountries write FIMDbCountries;
    property IMDbGenres: TStringList read FIMDbGenres write FIMDbGenres;

    //property IMDbAlsoKnownAsData: TIMDbAlsoKnownAsRecord read FIMDbAlsoKnownAsData write FIMDbAlsoKnownAsData;
    property IMDbReleaseInfos: TIMDbReleaseDatesRecord read FIMDbReleaseInfos write FIMDbReleaseInfos;
    property IMDbBoomInfos: TIMDbBoomData read FIMDbBoomInfos write FIMDbBoomInfos;


    property CreationTime: TDateTime read FCreationTime write FCreationTime;
    property UpdatedTime: TDateTime read FUpdatedTime write FUpdatedTime;
    //...
  end;

// data should be filtered to sort out countries which never get a release
  TIMDbAlsoKnownAsRecord = class(TSQLRecordNoCase)
  private
    FIMDbCountry: RawUTF8; //< country from releaseinfo page
    FIMDbTitle: RawUTF8; //< title from releaseinfo page
    FIMDbData:  TIMDbData;
  published
    property IMDbCountry: RawUTF8 read FIMDbCountry write FIMDbCountry;
    property IMDbTitle: RawUTF8 read FIMDbTitle write FIMDbTitle;
    property IMDbData: TIMDbData read FIMDbData write FIMDbData;
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
{ Returns True if the IMDB-Data needs to be updated
  @param(aaReleasename aReleasename)
  @returns(@True if updated needed, @False if updated not needed) }
function UpdateMovieInDbWithReleaseNameNeeded(const aReleasename: String): Boolean;

{ Creates a backup of IMDb-database - this is needed because the file is in use and can't be copied
  @param(aPath path where the backup should be stored in the filesystem with last slash, e.g. /path/to/file/)
  @param(aFileName filename including fileextension) }
procedure doIMDbDbBackup(const aPath, aFileName: String);

type

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
    imdb_boom_country: String;
    // additional infos
    imdb_origtitle: String; //< original imdb/movie title
    constructor Create(const aIMDbId:String);
    destructor Destroy; override;
    procedure PostResults(const aRls : String = '');overload;
  end;

{ Parses the IRC-Message and extracts the IMDB-ID. After that call IMDB_Save.
  Returns True if update is done, else False. }
function dbaddimdb_Process(aNet, aChan, aNick, aMsg: String): Boolean;
{ Checks if the release is TV, creates the IMDBData Object and after that calls CreateHttpTask. }
procedure dbaddimdb_SaveImdb(const aReleaseName, aIMDbId: String);
{ save the IMDB-Data into a DataObject. }
procedure dbaddimdb_SaveImdbData(const aReleaseName: String; aImdbData: TDbImdbData);
{ Creates the httptask and checks for duplicates. }
procedure CreateHttpTask(const aReleaseName, aIMDbId: String);
 { Retriggers the queue. }
procedure dbaddimdb_FireKbAdd(const aReleaseName : String);
 { Updates the IMDB-Data when neccesary. }
procedure dbaddimdb_UpdateImdbData(const aReleaseName: String; aImdbData: TDbImdbData);
 { Returns the IMDB-Count in Database as String }
 procedure  dbaddimdb_InsertOnlyAlsoKnownAs(const aReleaseName: String; aImdbData: TDbImdbData);
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

  debugunit, configunit, sitesunit, console, StrUtils,
  DateUtils, mystrings, FLRE, kb, kb.releaseinfo,
  queueunit, RegExpr, taskhttpimdb, pazo, mrdohutils, dbtvinfo;

var

  rx_imdbid: TFLRE;
  rx_captures: TFLREMultiCaptures;
  glLanguageCountryMappingList: TObjectList<TMapLanguageCountry>;

const
  IMDBREPLACEFILENAME = 'slftp.imdbreplace';
  section = 'dbaddimdb';

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
  result := TSQLModel.Create([TIMDbData,TIMDbReleaseDatesRecord,TIMDbAlsoKnownAsRecord, TIMDbBoomData]);
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

function DeleteIMDbDataWithImdbId(const aIMDbId: String): Boolean;
var
    fIMDbDataAlsoKnownAsRec:  TIMDbAlsoKnownAsRecord;
    fId_FIMDbAlsoKnownAsData, fID_IMDbReleaseInfos, fID_IMDbBoomData:           Int64;
begin
    Result := False;
    fIMDbDataAlsoKnownAsRec := TIMDbAlsoKnownAsRecord.CreateAndFillPrepareJoined(ImdbDatabase, 'imdbdata.IMDbId = ?', [], [aIMDbId]);
    try
      while fIMDbDataAlsoKnownAsRec.FillOne do
      begin
        fID_FIMDbAlsoKnownAsData := TID(fIMDbDataAlsoKnownAsRec);
        fID_IMDbReleaseInfos := TID(fIMDbDataAlsoKnownAsRec.imdbdata.IMDbReleaseInfos);
        fID_IMDbBoomData := TID(fIMDbDataAlsoKnownAsRec.imdbdata.IMDbBoomInfos);

        ImdbDatabase.Delete(TIMDbReleaseDatesRecord,fID_IMDbReleaseInfos);
        ImdbDatabase.Delete(TIMDbBoomData,fID_IMDbBoomData);
        ImdbDatabase.Delete(TIMDbAlsoKnownAsRecord,fID_FIMDbAlsoKnownAsData);
        ImdbDatabase.Delete(TIMDbData, 'IMDbID=?',[aIMDbId]);
      end;
    finally
      fIMDbDataAlsoKnownAsRec.Free;
      Result := True;
    end;
end;

function DeleteIMDbDataWithReleaseName(const aReleaseName: String): Boolean;
var
    fIMDbDataAlsoKnownAsRec:  TIMDbAlsoKnownAsRecord;
    fId_FIMDbAlsoKnownAsData, fID_IMDbReleaseInfos, fID_IMDbBoomData:           Int64;
begin
    Result := False;
    fIMDbDataAlsoKnownAsRec := TIMDbAlsoKnownAsRecord.CreateAndFillPrepareJoined(ImdbDatabase, 'IMDbAlsoKnownAsRecord.IMDbTitle = ?', [], [aReleaseName]);
    try
      while fIMDbDataAlsoKnownAsRec.FillOne do
      begin
        fID_FIMDbAlsoKnownAsData := TID(fIMDbDataAlsoKnownAsRec);
        fID_IMDbReleaseInfos := TID(fIMDbDataAlsoKnownAsRec.imdbdata.IMDbReleaseInfos);
        fID_IMDbBoomData := TID(fIMDbDataAlsoKnownAsRec.imdbdata.IMDbBoomInfos);

        ImdbDatabase.Delete(TIMDbReleaseDatesRecord,fID_IMDbReleaseInfos);
        ImdbDatabase.Delete(TIMDbBoomData,fID_IMDbBoomData);
        ImdbDatabase.Delete(TIMDbAlsoKnownAsRecord,fID_FIMDbAlsoKnownAsData);
        ImdbDatabase.Delete(TIMDbData, 'IMDbID=?',[fIMDbDataAlsoKnownAsRec.IMDbData.IMDbID]);
      end;
    finally
      fIMDbDataAlsoKnownAsRec.Free;
      Result := True;
    end;
  end;

Function foundMovieAlreadyInDbWithImdbId(const aImdbId: String): Boolean;
var
    fId_FIMDbData: TIMDbData;
begin
    Result := False;
    fId_FIMDbData := TIMDbData.CreateAndFillPrepare(ImdbDatabase, 'IMDbID = ?', [], [aImdbId]);
    try
      Result := fId_FIMDbData.FillOne;
    finally
      fId_FIMDbData.Free;
    end;
end;

Function getNbrOfImdbEntries: Integer;
begin
  Result := ImdbDatabase.TableRowCount(TIMDbData);
end;

Function foundMovieAlreadyInDbWithReleaseName(const aReleasename: String): Boolean;
var
    fId_FIMDbAlsoKnownAsRecord: TIMDbAlsoKnownAsRecord;
    fCleanedMovieName: String;
begin
    Result := False;
    fCleanedMovieName := getMovieNameWithoutSceneTags(aReleaseName);
    fId_FIMDbAlsoKnownAsRecord := TIMDbAlsoKnownAsRecord.CreateAndFillPrepareJoined(ImdbDatabase, 'IMDbAlsoKnownAsRecord.IMDbTitle = ?', [], [fCleanedMovieName]);
    try
      Result := fId_FIMDbAlsoKnownAsRecord.FillOne;
    finally
      fId_FIMDbAlsoKnownAsRecord.Free;
    end;
end;

Function UpdateMovieInDbWithReleaseNameNeeded(const aReleasename: String): Boolean;
var
    fId_FIMDBAlsoKnownAs: TIMDBAlsoKnownAsRecord;
    fCleanedMovieName: String;
begin
    Result := False;
    fCleanedMovieName := getMovieNameWithoutSceneTags(aReleaseName);
    fId_FIMDBAlsoKnownAs := TIMDBAlsoKnownAsRecord.CreateAndFillPrepareJoined(ImdbDatabase, 'IMDbAlsoKnownAsRecord.IMDbTitle = ?', [], [fCleanedMovieName]);
    try
      while fId_FIMDBAlsoKnownAs.FillOne do
      Result := False;
      begin
        if DaysBetween(now,fId_FIMDBAlsoKnownAs.IMDbData.UpdatedTime)>=config.ReadInteger(section, 'update_time_in_days', 7) then
          begin
            Debug(dpError, section, Format('[Info] UpdateMovieInDbWithReleaseNameNeeded - Update Needed: %s - %s', [aReleaseName, fCleanedMovieName]));
            Result := True;
          end;
      end;
    finally
      if not Result then
      begin
        Debug(dpError, section, Format('[Info] UpdateMovieInDbWithReleaseNameNeeded - Update NOT Needed: %s - %s', [aReleaseName, fCleanedMovieName]));
      end;
      fId_FIMDBAlsoKnownAs.Free;
    end;
end;

procedure dbaddimdb_SaveImdb(const aReleaseName, aIMDbId: String);
var
  i: Integer;
  showname: String;
  season: Integer;
  episode: int64;
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

procedure dbaddimdb_SaveImdbData(const aReleaseName: String; aImdbData: TDbImdbData);
var
//    FIMDbID: String; //< IMDb title ID like tt12345
//    FIMDbTitle: String; //< title from webpage
//    FIMDbTitleExtras: String; //< extra info from title if available, e.g. TV* or VIDEO* stuff
//    FIMDbYear: Integer; //< movie year
//    FIMDbCineyear: Integer; //< cinema year (TODO: which value if not available?)
//    FIMDbRating: Integer; //< rating (multiplied by 10)
//    FIMDbVotes: Integer; //< votes
//
//    FIMDbScreens: Integer; //< screens
//    FIMDbLimited: Boolean; //< @true if limited, @false otherwise
//    FIMDbWide: Boolean; //< @true if wide, @false otherwise
//    FIMDbFestival: Boolean; //< @true if aired on a festival, @false otherwise
//    FIMDbSTV: Boolean; //< @true if STV Movie, @false otherwise
//    FIMDbSTVReason: String; //< reason for STV status
//
//    FCreationTime: TDateTime; //< time when the data was fetched
//
//    FIMDbAlsoKnownAsData: TIMDbAlsoKnownAsRecord; //< AKA info from releaseinfo page
//    // code has to select the appropriate country info depending on the release language of the release
//    FIMDbReleaseInfos: TIMDbReleaseDatesRecord; //< all release dates information

    TIMDbDataRec:   TIMDbData;
    TIMDbReleaseDatesRecordRec: TIMDbReleaseDatesRecord;
    TIMDbAlsoKnownAsRecordRec: TIMDbAlsoKnownAsRecord;
    FIMDbBoomDataRecordRec: TIMDbBoomData;
begin

    if foundMovieAlreadyInDbWithReleaseName(aReleaseName) then
    begin
      dbaddimdb_UpdateImdbData(aReleaseName,aImdbData)
    end
    else if foundMovieAlreadyInDbWithImdbId(aImdbData.imdb_id) then
    begin
      dbaddimdb_InsertOnlyAlsoKnownAs(aReleaseName,aImdbData)
    end
    else
    begin
    try
      try
      Debug(dpError, section, Format('[INFO] dbaddimdb_SaveImdbData : Start Adding Data: %s', [aReleaseName]));
      TIMDbDataRec := TIMDbData.Create;
      TIMDbDataRec.IMDbID := aImdbData.imdb_id;
      TIMDbDataRec.IMDbTitle := StringToUTF8(aImdbData.imdb_origtitle);

      TIMDbDataRec.IMDbTitleExtras := '';
      TIMDbDataRec.IMDbYear :=  aImdbData.imdb_year;
      TIMDbDataRec.IMDbCineyear := aImdbData.imdb_cineyear;
      TIMDbDataRec.IMDbRating := aImdbData.imdb_rating;
      TIMDbDataRec.IMDbVotes :=  aImdbData.imdb_votes;

      TIMDbDataRec.IMDbLanguages := aImdbData.imdb_languages;
      TIMDbDataRec.IMDbCountries := aImdbData.imdb_countries;
      TIMDbDataRec.IMDbGenres := aImdbData.imdb_genres;

      FIMDbBoomDataRecordRec := TIMDbBoomData.Create;

      Debug(dpError, section, Format('[INFO] dbaddimdb_SaveImdbData : Adding Boom Data: %s', [aReleaseName]));
      FIMDbBoomDataRecordRec.IMDbScreens := aImdbData.imdb_screens;
      FIMDbBoomDataRecordRec.IMDbLimited := aImdbData.imdb_ldt;
      FIMDbBoomDataRecordRec.IMDbWide :=  aImdbData.imdb_wide;
      FIMDbBoomDataRecordRec.IMDbFestival := aImdbData.imdb_festival;
      FIMDbBoomDataRecordRec.IMDbSTV := aImdbData.imdb_stvm;
      FIMDbBoomDataRecordRec.IMDbSTVReason := aImdbData.imdb_stvs;
      FIMDbBoomDataRecordRec.IMDbCountry := 'DE';

      TIMDbDataRec.CreationTime := Now;
      TIMDbDataRec.UpdatedTime := Now;

      Debug(dpError, section, Format('[INFO] dbaddimdb_SaveImdbData : Adding ReleaseDate Data: %s', [aReleaseName]));
      TIMDbReleaseDatesRecordRec := TIMDbReleaseDatesRecord.Create;
      TIMDbReleaseDatesRecordRec.IMDbCountry := 'DE';
      TIMDbReleaseDatesRecordRec.IMDbReleaseDate := Now;
      TIMDbReleaseDatesRecordRec.IMDbReleaseDateExtraInfo := 'No Infos';

      Debug(dpError, section, Format('[INFO] dbaddimdb_SaveImdbData : Adding AlsoKnownAs Data: %s', [aReleaseName]));
      TIMDbAlsoKnownAsRecordRec := TIMDbAlsoKnownAsRecord.Create;
      TIMDbAlsoKnownAsRecordRec.IMDbCountry := 'DE';
      TIMDbAlsoKnownAsRecordRec.IMDbTitle := getMovieNameWithoutSceneTags(aReleaseName);

      Debug(dpError, section, Format('[INFO] dbaddimdb_SaveImdbData : Adding all Elements to DB: : %s - %s', [aReleaseName, aImdbData.imdb_id]));

      if not TIMDbReleaseDatesRecordRec.FillOne then
          ImdbDatabase.Add(TIMDbReleaseDatesRecordRec,true);
      if not FIMDbBoomDataRecordRec.FillOne then
          ImdbDatabase.Add(FIMDbBoomDataRecordRec,true);

      //TIMDbDataRec.IMDbAlsoKnownAsData := TIMDbAlsoKnownAsRecordRec.AsTSQLRecord;
      TIMDbDataRec.IMDbReleaseInfos := TIMDbReleaseDatesRecordRec.AsTSQLRecord;
      TIMDbDataRec.IMDbBoomInfos := FIMDbBoomDataRecordRec.AsTSQLRecord;


      if not TIMDbDataRec.FillOne then
      begin
        if ImdbDatabase.Add(TIMDbDataRec,true)=0 then
        Begin
          if foundMovieAlreadyInDbWithImdbId(aImdbData.imdb_id) then
          Begin
            Debug(dpError, section, FORMAT('[Info] dbaddimdb_SaveImdbData : Error adding the data for Release: %s - %s - Error: %s - Cleaning Database!', [aReleaseName, aImdbData.imdb_id,UTF8ToString(ImdbDatabase.LastErrorMessage)]));
            //ImdbDatabase.Delete(TIMDbAlsoKnownAsRecord, TIMDbAlsoKnownAsRecordRec.fiD);
            ImdbDatabase.Delete(TIMDbReleaseDatesRecord, TIMDbReleaseDatesRecordRec.fiD);
            ImdbDatabase.Delete(TIMDbBoomData,FIMDbBoomDataRecordRec.fiD);
          End;
        end
        else
        begin
          TIMDbAlsoKnownAsRecordRec.IMDbData := TIMDbDataRec.AsTSQLRecord;
          if not TIMDbAlsoKnownAsRecordRec.FillOne then
            ImdbDatabase.Add(TIMDbAlsoKnownAsRecordRec,true);
          Debug(dpError, section, Format('[Info] dbaddimdb_SaveImdbData : Data for Release: %s successful saved in Database!', [aReleaseName]));
        end;
      end;

      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[EXCEPTION] dbaddimdb_SaveImdbData : Data for Release: %s - %s - Error: %s!', [aReleaseName, aImdbData.imdb_id, e.Message]));
          exit;
        end;
      end;
      finally
        if TIMDbReleaseDatesRecordRec <> nil then
          FreeAndNil(TIMDbReleaseDatesRecordRec);
        if TIMDbAlsoKnownAsRecordRec <> nil then
          FreeAndNil(TIMDbAlsoKnownAsRecordRec);
        if FIMDbBoomDataRecordRec <> nil then
          FreeAndNil(FIMDbBoomDataRecordRec);
        if TIMDbDataRec <> nil then
          FreeAndNil(TIMDbDataRec);
      end;
    end;

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
        Debug(dpError, section, Format('[EXCEPTION] dbaddimdb_SaveImdbData (FireKbAdd): %s', [e.Message]));
        exit;
      end;
    end;
End;

procedure dbaddimdb_UpdateImdbData(const aReleaseName: String; aImdbData: TDbImdbData);
var
//    FIMDbID: String; //< IMDb title ID like tt12345
//    FIMDbTitle: String; //< title from webpage
//    FIMDbTitleExtras: String; //< extra info from title if available, e.g. TV* or VIDEO* stuff
//    FIMDbYear: Integer; //< movie year
//    FIMDbCineyear: Integer; //< cinema year (TODO: which value if not available?)
//    FIMDbRating: Integer; //< rating (multiplied by 10)
//    FIMDbVotes: Integer; //< votes
//
//    FIMDbScreens: Integer; //< screens
//    FIMDbLimited: Boolean; //< @true if limited, @false otherwise
//    FIMDbWide: Boolean; //< @true if wide, @false otherwise
//    FIMDbFestival: Boolean; //< @true if aired on a festival, @false otherwise
//    FIMDbSTV: Boolean; //< @true if STV Movie, @false otherwise
//    FIMDbSTVReason: String; //< reason for STV status
//
//    FCreationTime: TDateTime; //< time when the data was fetched
//
//    FIMDbAlsoKnownAsData: TIMDbAlsoKnownAsRecord; //< AKA info from releaseinfo page
//    // code has to select the appropriate country info depending on the release language of the release
//    FIMDbReleaseInfos: TIMDbReleaseDatesRecord; //< all release dates information

    TIMDbDataRec:   TIMDbData;
    TIMDbReleaseDatesRecordRec: TIMDbReleaseDatesRecord;
    TIMDbAlsoKnownAsRecordRec: TIMDbAlsoKnownAsRecord;
    FIMDbBoomDataRecordRec: TIMDbBoomData;
    fCleanedMovieName : String;
begin

    fCleanedMovieName := getMovieNameWithoutSceneTags(aReleaseName);

    TIMDbAlsoKnownAsRecordRec := TIMDbAlsoKnownAsRecord.CreateAndFillPrepareJoined(ImdbDatabase,
        'IMDbAlsoKnownAsRecord.IMDbTitle = ?', [], [fCleanedMovieName]);
        Debug(dpError, section, Format('[INFO] dbaddimdb_UpdateImdbData : Start Update Data: %s - %s', [fCleanedMovieName, TIMDbDataRec.IMDbID]));
    try
      try
      if TIMDbAlsoKnownAsRecordRec.FillOne then
      begin

          TIMDbDataRec := TIMDbAlsoKnownAsRecordRec.IMDbData.AsTSQLRecord;
          TIMDbDataRec.IMDbLanguages := TStringList.Create;
          TIMDbDataRec.IMDbCountries := TStringList.Create;
          TIMDbDataRec.IMDbGenres := TStringList.Create;

          TIMDbDataRec.IMDbID := aImdbData.imdb_id;
          TIMDbDataRec.IMDbTitle := StringToUTF8(aImdbData.imdb_origtitle);

          TIMDbDataRec.IMDbTitleExtras := '';
          TIMDbDataRec.IMDbYear :=  aImdbData.imdb_year;
          TIMDbDataRec.IMDbCineyear := aImdbData.imdb_cineyear;
          TIMDbDataRec.IMDbRating := aImdbData.imdb_rating;
          TIMDbDataRec.IMDbVotes := aImdbData.imdb_votes;

          TIMDbDataRec.IMDbLanguages := aImdbData.imdb_languages;
          TIMDbDataRec.IMDbCountries := aImdbData.imdb_countries;
          TIMDbDataRec.IMDbGenres := aImdbData.imdb_genres;

          FIMDbBoomDataRecordRec := TIMDbDataRec.IMDbBoomInfos;

          Debug(dpError, section, Format('[INFO] dbaddimdb_UpdateImdbData : Update Boom Data: %s', [aReleaseName]));
          FIMDbBoomDataRecordRec.IMDbScreens := aImdbData.imdb_screens;
          FIMDbBoomDataRecordRec.IMDbLimited := aImdbData.imdb_ldt;
          FIMDbBoomDataRecordRec.IMDbWide :=  aImdbData.imdb_wide;
          FIMDbBoomDataRecordRec.IMDbFestival := aImdbData.imdb_festival;
          FIMDbBoomDataRecordRec.IMDbSTV := aImdbData.imdb_stvm;
          FIMDbBoomDataRecordRec.IMDbSTVReason := aImdbData.imdb_stvs;
          TIMDbDataRec.IMDbBoomInfos.IMDbCountry := 'DE';

          TIMDbDataRec.UpdatedTime := Now;

          Debug(dpError, section, Format('[INFO] dbaddimdb_UpdateImdbData : Update ReleaseDate Data: %s', [aReleaseName]));
          TIMDbReleaseDatesRecordRec := TIMDbDataRec.IMDbReleaseInfos;
          TIMDbReleaseDatesRecordRec.IMDbCountry := 'DE';
          TIMDbReleaseDatesRecordRec.IMDbReleaseDate := Now;
          TIMDbReleaseDatesRecordRec.IMDbReleaseDateExtraInfo := 'No Infos';

          Debug(dpError, section, Format('[INFO] dbaddimdb_UpdateImdbData : Update AlsoKnownAs Data: %s', [aReleaseName]));
          TIMDbAlsoKnownAsRecordRec.IMDbData := TIMDbDataRec.AsTSQLRecord;
          TIMDbAlsoKnownAsRecordRec.IMDbCountry := 'DE';
          TIMDbAlsoKnownAsRecordRec.IMDbTitle := fCleanedMovieName;

          Debug(dpError, section, Format('[INFO] dbaddimdb_UpdateImdbData : Update AsTSQLRecord Data: %s', [aReleaseName]));
          //TIMDbDataRec.IMDbAlsoKnownAsData := TIMDbAlsoKnownAsRecordRec.AsTSQLRecord;
          TIMDbDataRec.IMDbReleaseInfos := TIMDbReleaseDatesRecordRec.AsTSQLRecord;
          TIMDbDataRec.IMDbBoomInfos := FIMDbBoomDataRecordRec.AsTSQLRecord;
          Debug(dpError, section, Format('[INFO] dbaddimdb_UpdateImdbData : Insert Row Data: %s', [aReleaseName]));

          if ImdbDatabase.Update(FIMDbBoomDataRecordRec, 'IMDbCountry,IMDbScreens,IMDbLimited,IMDbWide,IMDbFestival,IMDbSTV,IMDbSTVReason')=false then
          begin
            Debug(dpError, section, FORMAT('[EXCEPTION] dbaddimdb_UpdateImdbData : Error updating TIMDbBoomData data for Release: %s', [aReleaseName]));
          end;
          if ImdbDatabase.Update(TIMDbAlsoKnownAsRecordRec, 'IMDbCountry,IMDbTitle')=false then
          begin
            Debug(dpError, section, FORMAT('[EXCEPTION] dbaddimdb_UpdateImdbData : Error updating TIMDbAlsoKnownAsRecord data for Release: %s', [aReleaseName]));
          end;
          if ImdbDatabase.Update(TIMDbReleaseDatesRecordRec, 'IMDbCountry,IMDbReleaseDate,IMDbReleaseDateExtraInfo')=false then
          begin
            Debug(dpError, section, FORMAT('[EXCEPTION] dbaddimdb_UpdateImdbData : Error updating TIMDbReleaseDatesRecord data for Release: %s', [aReleaseName]));
          end;
          if ImdbDatabase.Update(TIMDbDataRec, 'IMDbID,IMDbTitle,IMDbTitleExtras,IMDbYear,IMDbCineyear,IMDbRating,IMDbVotes,IMDbLanguages,IMDbCountries,IMDbGenres,CreationTime,UpdatedTime')=false then
          begin
            Debug(dpError, section, FORMAT('[EXCEPTION] dbaddimdb_UpdateImdbData : Error updating TIMDbData data for Release: %s - Error: %s', [aReleaseName, UTF8ToString(ImdbDatabase.LastErrorMessage)]));
          end;

        end;
        finally
          //ClearObject(TIMDbReleaseDatesRecordRec);
          //ClearObject(TIMDbAlsoKnownAsRecordRec);
          //ClearObject(FIMDbBoomDataRecordRec);
          //ClearObject(TIMDbDataRec);

          //TIMDbReleaseDatesRecordRec.Free;
          //TIMDbAlsoKnownAsRecordRec.Free;
          //FIMDbBoomDataRecordRec.Free;
          //TIMDbDataRec.Free;
          //FreeAndNil(TIMDbReleaseDatesRecordRec);
          //FreeAndNil(TIMDbAlsoKnownAsRecordRec);
          //FreeAndNil(FIMDbBoomDataRecordRec);
          //FreeAndNil(TIMDbDataRec);
        end;
        except
          on e: Exception do
          begin
            Debug(dpError, section, Format('[EXCEPTION] dbaddimdb_UpdateImdbData : Data for Release: %s - %s Error: %s!', [aReleaseName,aImdbData.imdb_id, e.Message]));
            Exit;
          end;
      end;
    Debug(dpError, section, Format('[Info] dbaddimdb_UpdateImdbData : Data for Release: %s successful updated in Database!', [aReleaseName]));
End;

procedure dbaddimdb_InsertOnlyAlsoKnownAs(const aReleaseName: String; aImdbData: TDbImdbData);
var
//    FIMDbID: String; //< IMDb title ID like tt12345
//    FIMDbTitle: String; //< title from webpage
//    FIMDbTitleExtras: String; //< extra info from title if available, e.g. TV* or VIDEO* stuff
//    FIMDbYear: Integer; //< movie year
//    FIMDbCineyear: Integer; //< cinema year (TODO: which value if not available?)
//    FIMDbRating: Integer; //< rating (multiplied by 10)
//    FIMDbVotes: Integer; //< votes
//
//    FIMDbScreens: Integer; //< screens
//    FIMDbLimited: Boolean; //< @true if limited, @false otherwise
//    FIMDbWide: Boolean; //< @true if wide, @false otherwise
//    FIMDbFestival: Boolean; //< @true if aired on a festival, @false otherwise
//    FIMDbSTV: Boolean; //< @true if STV Movie, @false otherwise
//    FIMDbSTVReason: String; //< reason for STV status
//
//    FCreationTime: TDateTime; //< time when the data was fetched
//
//    FIMDbAlsoKnownAsData: TIMDbAlsoKnownAsRecord; //< AKA info from releaseinfo page
//    // code has to select the appropriate country info depending on the release language of the release
//    FIMDbReleaseInfos: TIMDbReleaseDatesRecord; //< all release dates information

    TIMDbDataRec:   TIMDbData;
    TIMDbAlsoKnownAsRecordRec: TIMDbAlsoKnownAsRecord;
    fCleanedMovieName : String;
begin
    try

    fCleanedMovieName := getMovieNameWithoutSceneTags(aReleaseName);

    TIMDbAlsoKnownAsRecordRec := TIMDbAlsoKnownAsRecord.CreateAndFillPrepareJoined(ImdbDatabase,
        'IMDBData.IMDbId = ?', [], [aImdbData.imdb_id]);
        Debug(dpError, section, Format('[INFO] dbaddimdb_InsertOnlyAlsoKnownAs : Start Inserting Data: %s - %s', [fCleanedMovieName, TIMDbAlsoKnownAsRecordRec.ImdbData.IMDbID]));
    while TIMDbAlsoKnownAsRecordRec.FillOne do
    begin
      try
          TIMDbDataRec := TIMDbAlsoKnownAsRecordRec.IMDbData;
          TIMDbDataRec.UpdatedTime := Now;

          Debug(dpError, section, Format('[INFO] dbaddimdb_InsertOnlyAlsoKnownAs : Update AlsoKnownAs Data: %s', [aReleaseName]));
          TIMDbAlsoKnownAsRecordRec.IMDbData := TIMDbDataRec.AsTSQLRecord;
          TIMDbAlsoKnownAsRecordRec.IMDbCountry := 'DE';
          TIMDbAlsoKnownAsRecordRec.IMDbTitle := fCleanedMovieName;

          Debug(dpError, section, Format('[INFO] dbaddimdb_InsertOnlyAlsoKnownAs : Update AsTSQLRecord Data: %s', [aReleaseName]));
          //TIMDbDataRec.IMDbAlsoKnownAsData := TIMDbAlsoKnownAsRecordRec.AsTSQLRecord;
          Debug(dpError, section, Format('[INFO] dbaddimdb_InsertOnlyAlsoKnownAs : Insert Row Data: %s', [aReleaseName]));

          if not TIMDbAlsoKnownAsRecordRec.FillOne then
          begin
            if (ImdbDatabase.Add(TIMDbAlsoKnownAsRecordRec,true)=0) then
            begin
              Debug(dpError, section, FORMAT('[EXCEPTION] dbaddimdb_InsertOnlyAlsoKnownAs : Error adding the data for Release: %s - %s - Error: %s!', [aReleaseName, aImdbData.imdb_id,UTF8ToString(ImdbDatabase.LastErrorMessage)]));
            end;
          end;
          if ImdbDatabase.Update(TIMDbDataRec, 'UpdatedTime')=false then
          begin
            Debug(dpError, section, FORMAT('[EXCEPTION] dbaddimdb_InsertOnlyAlsoKnownAs : Error updating TIMDbData data for Release: %s', [aReleaseName]));
          end;
        finally
          //ClearObject(TIMDbAlsoKnownAsRecordRec);
          //ClearObject(TIMDbDataRec);
          //TIMDbDataRec.Free;
          //TIMDbAlsoKnownAsRecordRec.Free;
          //FreeAndNil(TIMDbDataRec);
        end;
        end;
        except
          on e: Exception do
          begin
            Debug(dpError, section, Format('[EXCEPTION] dbaddimdb_InsertOnlyAlsoKnownAs : Data for Release: %s - %s Error: %s!', [aReleaseName,aImdbData.imdb_id, e.Message]));
            Exit;
          end;
      end;
   Debug(dpError, section, Format('[Info] dbaddimdb_InsertOnlyAlsoKnownAs : Data for Release: %s successful inserted in Database!', [aReleaseName]));
End;

function GetImdbMovieData(const aReleaseName: String): TDbImdbData;
var
  fMovieDataRec: TIMDbAlsoKnownAsRecord;
  fMovieImdbDataRec: TIMDbData;
  fImdbMovieData: TDbImdbData;
  fCleanedMovieName: string;

begin
  result := nil;
  fImdbMovieData := nil;
  fMovieImdbDataRec := nil;
  fCleanedMovieName := getMovieNameWithoutSceneTags(aReleaseName);

  fMovieDataRec := TIMDbAlsoKnownAsRecord.CreateAndFillPrepareJoined(ImdbDatabase,
  'IMDbAlsoKnownAsRecord.IMDbTitle = ?', [], [fCleanedMovieName]);

  try
    fMovieDataRec.Imdbdata.IMDbLanguages := TStringList.Create;
    fMovieDataRec.Imdbdata.IMDbCountries := TStringList.Create;
    fMovieDataRec.Imdbdata.IMDbGenres := TStringList.Create;

    while fMovieDataRec.FillOne do
    begin
      fImdbMovieData := TDbImdbData.Create(fMovieDataRec.Imdbdata.IMDbID);
      fImdbMovieData.imdb_id := fMovieDataRec.Imdbdata.IMDbID;
      fImdbMovieData.imdb_year := fMovieDataRec.Imdbdata.IMDbYear;
      fImdbMovieData.imdb_origtitle := UTF8ToString(fMovieDataRec.IMDbTitle);

      fImdbMovieData.imdb_languages :=  fMovieDataRec.Imdbdata.IMDbLanguages;
      fImdbMovieData.imdb_countries :=  fMovieDataRec.Imdbdata.IMDbCountries;
      fImdbMovieData.imdb_genres :=  fMovieDataRec.Imdbdata.IMDbGenres;

      fMovieImdbDataRec := TIMDbData.CreateAndFillPrepareJoined(ImdbDatabase,
      'IMDbID = ?', [], [fMovieDataRec.Imdbdata.IMDbID]);

      while fMovieImdbDataRec.FillOne do
      begin
        fImdbMovieData.imdb_screens := fMovieImdbDataRec.IMDbBoomInfos.IMDbScreens;
        fImdbMovieData.imdb_rating := fMovieImdbDataRec.IMDbRating;
        fImdbMovieData.imdb_votes := fMovieImdbDataRec.IMDbVotes;
        fImdbMovieData.imdb_cineyear := fMovieImdbDataRec.IMDbCineyear;
        fImdbMovieData.imdb_ldt := fMovieImdbDataRec.IMDbBoomInfos.IMDbLimited;
        fImdbMovieData.imdb_wide := fMovieImdbDataRec.IMDbBoomInfos.IMDbWide;
        fImdbMovieData.imdb_festival := fMovieImdbDataRec.IMDbBoomInfos.IMDbFestival;
        fImdbMovieData.imdb_stvm := fMovieImdbDataRec.IMDbBoomInfos.IMDbSTV;
        fImdbMovieData.imdb_stvs := fMovieImdbDataRec.IMDbBoomInfos.IMDbSTVReason;
      end;
    end;
    finally
      result := fImdbMovieData;
      fMovieDataRec.Free;
      fMovieImdbDataRec.Free;
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
    Console_Addline('', Format('IMDb db loaded. %d Movies', [ImdbDatabase.TableRowCount(TIMDbData)]));
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


