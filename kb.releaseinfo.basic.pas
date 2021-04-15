{
  @abstract(Knowledge base class for information from releasenames)
}
unit kb.releaseinfo.basic;

interface

type
  { storing class for basic info from releasename }
  TBasicInfoStore = class//(TSQLRecord)
  private
    FSectionName: String; //< sectionname
    FReleaseName: String; //< releasename
    FCurrentYear: Integer; //< Value of the current year (e.g. 2019)
    FYear: Integer; //< the year parsed from the release name
    FGroupName: String; //< name of release group extracted from @link(FReleaseName) by \-([^\-]+)$ regex
    FReleaseNameWithoutGroupName: String; //< @link(rlsname) with removed @link(FGroupName)
    FIsInternal: Boolean; //< @true if @link(FReleaseName) matches [\_\-\.]\(?(internal|int)\)?([\_\-\.]|$) regex, otherwise @false
    FLanguage: String; //< contains the language string which is detected from @link(FReleaseName)
    FWordsList: TStringList; //< list of all words which occur in @link(FReleaseName), firstly removes () and then replaces .-_ with whitespace
    FNumberOfDisks: Integer; //< number of disks from rlsname, e.g. Foobar.2008.NTSC.3DiSC.MDVDR-GRP -> 3
    FKnownGroup: TKnownGroup; //< value indicating whether this rls group is in slftp.knowngroups file
    { for fake checking }
    FNumberOfDots: Integer; //< amount of dots ('.') in @link(FReleaseName)
    FNumberOfDifferentChars: Integer; //< number of different characters in the release name
    FNumberOfVowels: Integer; //< amount of vowels [aeiouAEIOU] in @link(FReleaseName)
    FIsFake: Boolean; //< @true if the release name has been detected as fake, otherwise @false
    FFakereason: String; //< if this rls has been detected as fake, this field contains the reason for it
  public
    { Create the information store
      @param(aSection section)
      @param(aRlsname releasename) }
    constructor Create(const aSection, aRlsname: String);
    { Cleanup the private fields }
    destructor Destroy;
  published
    property SectionName: String read FSectionName;
    property ReleaseName: String read FReleaseName;
    property CurrentYear: Integer read FCurrentYear;
    property Year: Integer read FYear;
    property GroupName: String read FGroupName;
    property ReleaseNameWithoutGroupName: String read FReleaseNameWithoutGroupName;
    property IsInternal: Boolean read FIsInternal;
    property Language: String read FLanguage;
    property WordsList: TStringList read FWordsList;
    property NumberOfDisks: Integer read FNumberOfDisks;
    property KnownGroup: TKnownGroup read FKnownGroup;
    property NumberOfDots: Integer read FNumberOfDots;
    property NumberOfDifferentChars: Integer read FNumberOfDifferentChars;
    property NumberOfVowels: Integer read FNumberOfVowels;
    property IsFake: Boolean read FIsFake;
    property Fakereason: String read FFakereason;
  end;

  { storing class for pre information of the releasename }
  TPreInfoStore = class//(TSQLRecord)
  private
    FSectionName: String; //< sectionname
    FReleaseName: String; //< releasename
    FUTCPreTime: Int64; //< UTC pretime for release
    FPreTimeSource: String; // info where we found the pretime (see @link(dbaddpre.TPretimeResult))
    FInitialKbEvent: TKBEventType; //< the KB event type from which the rls was created
    FIsPredOnAnySite: Boolean; //< indicates if it's pred on any of your sites
  public
    { Create the information store
      @param(aSection section)
      @param(aRlsname releasename) }
    constructor Create(const aSection, aRlsname: String);
    { Cleanup the private fields }
    //destructor Destroy;
  published
    property SectionName: String read FSectionName;
    property ReleaseName: String read FReleaseName;
    property UTCPreTime: Int64 read FUTCPreTime;
    property PreTimeSource: String read FPreTimeSource;
    property InitialKbEvent: TKBEventType read FInitialKbEvent;
    property IsPredOnAnySite: Boolean read FIsPredOnAnySite;
  end;

  { storing class for 0-DAY information of the releasename }
  TZeroDayInfoStore = class//(TSQLRecord)
  private
    FSectionName: String; //< sectionname
    FReleaseName: String; //< releasename
    FZeroDaySource: String; //< platform type (operating system) @br (Note: default value is WIN if no platform found in releasename)
  public
    { Create the information store
      @param(aSection section)
      @param(aRlsname releasename)
      @param(aZeroDaySource target operating system) }
    constructor Create(const aSection, aRlsname, aZeroDaySource: String);
  published
    property SectionName: String read FSectionName;
    property ReleaseName: String read FReleaseName;
    property ZeroDaySource: String read FZeroDaySource;
  end;

type
  { class for basic info from releasename }
  TBasicController = class(TAggregatedObject, IBasicInfo)//IPreInfo
  private
    FBasicInfoStore: TBasicInfoStore; //< class used to store the information
    //FPreInfoStore: TPreInfoStore; //< class used to store the pre information

    { Extract the single words of the releasename
      @param(aRlsname releasename)
      @returns(releasename with removed special characters) }
    function ExtractSingleWords(const aRlsname: String): String;

    { Extract the groupname from the releasename
      @param(aRlsname releasename)
      @param(aRlsnameWordsList single words of the releasename)
      @returns(groupname) }
    function GetGroupname(const aRlsname: String; const aRlsnameWordsList: TStringList): String;

    { Find out if the release is tagged internal
      @param(aRlsname releasename)
      @returns(@true if internal, otherwise @false) }
    function DetectInternal(const aRlsname: String): Boolean;

    { Counts the different characters in the releasename
      @param(aRlsname releasename)
      @returns(Number of different characters) }
    function GetAmountOfDifferentChars(const aRlsname: String): Integer;

    { Counts the dots in the releasename
      @param(aRlsname releasename)
      @returns(Number of dots in releasename) }
    function GetNumberOfDots(const aRlsname: String): Integer;

    { Counts the vowels in the releasename
      @param(aRlsname releasename)
      @returns(Number of vowels in releasename) }
    function CountVowels(const aRlsname: String): Integer;

    { Get the year from the releasename
      @param(aRlsname releasename)
      @param(aRlsnameWordsList single words of the releasename)
      @returns(Parsed year) }
    function GetYear(const aRlsname: String; const aRlsnameWordsList: TStringList): Integer;

    { Get the number of discs for the release
      @param(aRlsnameWordsList single words of the releasename)
      @returns(Number if discs) }
    function GetNumberOfDiscs(const aRlsnameWordsList: TStringList): Integer;

// TODO: initial values of store can be stored from within controller constructor but the values needs to be updated once something was changed -> update values from Controller update function?
// TODO: class probably should also hold the useless infos like LookupDone for TV/IMDB stuff
// TODO: should also provide the aktualizal boolean thingy and the aktualizald function
// TODO: probably also needs to provide "class function Name, DefaultSections, SectionAccepted" as this needs
//       to return the sections for IBasicInfo so that TTvImdbRelease can put both strings together like (tv_sections|imdb_sections)
//       might need also a TDefaultInfo = class(TAggregatedObject) where all inherit from and these functions are defined as abstract like for T*Release
// TODO: this class should probably take care of adding/updating the stored info into the database by TSQLRest

// TODO: need some key to combine the infos which are stored in separate tables in DB to avoid storing releasename/section in all tables?
  public
    { Create and parse the basic info from the releasename
      @param(aRefController reference to the class which takes care of the reference count)
      @param(aSection section)
      @param(aRlsname releasename)
      @param(aLanguageParsing Method of parsing the language from the releasename) }
    constructor Create(const aRefController: IUnknown; const aSection, aRlsname: String; const aLanguageParsing: TLanguageParsingMode);
    { Cleanup the private fields }
    destructor Destroy;
  published
    { Provide access to the published properties of the internally used information stores }
    function GetBasicInfo: TBasicInfoStore;
    //function GetPreInfo: TPreInfoStore;
  end;

// TODO: separate TPreController?

  TZeroDayController = class(TAggregatedObject, IZeroDayInfo)
  private
    FZeroDayInfoStore: TZeroDayInfoStore; //< class used to store the 0-Day information

    { Extract the platform (operating system) from the releasename
      @param(aRlsname releasename)
      @returns(releasename with removed special characters) }
    function ExtractZeroDayTag(const aRlsname: String): String;
  public
    { Create and parse the 0-Day info from the releasename
      @param(aRefController reference to the class which takes care of the reference count)
      @param(aSection section)
      @param(aRlsname releasename) }
    constructor Create(const aRefController: IUnknown; const aSection, aRlsname: String);
  published
    { Provide access to the published properties of the internally used information stores }
    function GetZeroDayInfo: TZeroDayInfoStore;
  end;























implementation

uses
  SysUtils, debugunit, RegExpr;

const
  rsections = 'kb.releaseinfo';

constructor TBasicInfoStore.Create(const aSection, aRlsname: String);
begin
  inherited Create;
  FSectionName := aSection;
  ReleaseName := aRlsname;
  FWordsList := TStringList.Create;
  FWordsList.Delimiter := ' ';
  FWordsList.CaseSensitive := False;
end;

destructor TBasicInfoStore.Destroy;
begin
  FWordsList.Free;
  inherited;
end;




constructor TBasicController.Create(const aRefController: IUnknown; const aSection, aRlsname: String; const aLanguageParsing: TLanguageParsingMode);
begin
  inherited Create(aRefController);
  FBasicInfoStore := TBasicInfoStore.Create(aSection, aRlsname);

  FBasicInfoStore.FIsPredOnAnySite := False;
  FBasicInfoStore.FWordsList.DelimitedText := ExtractSingleWords(aRlsname);
  FBasicInfoStore.FGroupName := GetGroupname(aRlsname, FBasicInfoStore.FWordsList);
  FBasicInfoStore.FIsInternal := DetectInternal(aRlsname);
  FBasicInfoStore.FReleaseNameWithoutGroupName := Copy(aRlsname, 1, Length(aRlsname) - Length(FBasicInfoStore.FGroupName));
  case aLanguageParsing of
    lpm_default: FBasicInfoStore.FLanguage := FindLanguageOnDirectory(rlsname);
    lpm_audio: FBasicInfoStore.FLanguage := FindMusicLanguageOnDirectory(rlsname);
    lpm_musicvideo: FBasicInfoStore.FLanguage := FindMusicVideoLanguageOnDirectory(rlsname);
  end;
  FBasicInfoStore.FNumberOfDifferentChars := GetAmountOfDifferentChars(aRlsname);
  FBasicInfoStore.FNumberOfDots := GetNumberOfDots(aRlsname);
  FBasicInfoStore.FNumberOfVowels := CountVowels(aRlsname);


// TODO: pretime + fakecheck


  FBasicInfoStore.FKnownGroup := IsKnownGroup(aSection, FBasicInfoStore.FGroupName);
  FBasicInfoStore.FCurrentYear := StrToInt(FormatDateTime('yyyy', Now));
  FBasicInfoStore.FYear := GetYear(aRlsname, FBasicInfoStore.FWordsList);
  FBasicInfoStore.FNumberOfDisks := GetNumberOfDiscs(FBasicInfoStore.FWordsList);
end;

destructor TBasicController.Destroy;
begin
  FBasicInfoStore.Free;
  inherited;
end;

function TBasicController.GetBasicInfo: TBasicInfoStore;
begin
  Result := FBasicInfoStore;
end;

function TBasicController.ExtractSingleWords(const aRlsname: String): String;
begin
  Result := ReplaceText(aRlsname, '(', '');
  Result := ReplaceText(Result, ')', '');
  Result := ReplaceText(Result, '.', ' ');
  Result := ReplaceText(Result, '-', ' ');
  Result := ReplaceText(Result, '_', ' ');
end;

function TBasicController.GetGroupname(const aRlsname: String; const aRlsnameWordsList: TStringList): String;
var
  fRegEx: TRegExpr;
begin
  Result := '';

  fRegEx := TRegExpr.Create;
  try
    fRegEx.ModifierI := True;

    fRegEx.Expression := '\-([^\-]+)$';
    if fRegEx.Exec(aRlsname) then
    begin
      Result := fRegEx.Match[1];
    end;
  finally
    fRegEx.free;
  end;

  // different way if groupname not found by regex
  if (Result = '') then
  begin
    if UpperCase(aRlsnameWordsList.strings[aRlsnameWordsList.Count - 1]) = 'INT' then
      Result := aRlsnameWordsList.strings[aRlsnameWordsList.Count - 2] + '_' + aRlsnameWordsList.strings[aRlsnameWordsList.Count - 1]
    else
      Result := aRlsnameWordsList.strings[aRlsnameWordsList.Count - 1];
  end;
end;

function TBasicController.DetectInternal(const aRlsname: String): Boolean;
var
  fRegEx: TRegExpr;
begin
  Result := False;

  fRegEx := TRegExpr.Create;
  try
    fRegEx.ModifierI := True;

    fRegEx.Expression := '[\_\-\.]\(?(internal|int)\)?([\_\-\.]|$)';
    if fRegEx.Exec(aRlsname) then
    begin
      Result := True;
    end;
  finally
    fRegEx.free;
  end;
end;

function TBasicController.GetAmountOfDifferentChars(const aRlsname: String): Integer;
var
  fStr: String;
begin
  Result := 0;

  fStr := '';
  for i := 1 to Length(aRlsname) do
  begin
    if 0 = Pos(aRlsname[i], fStr) then
    begin
      Inc(Result);
      fStr := fStr + aRlsname[i];
    end;
  end;
end;

function TBasicController.GetNumberOfDots(const aRlsname: String): Integer;
begin
  Result := 0;

  for i := 1 to Length(aRlsname) do
  begin
    if aRlsname[i] = '.' then
      Inc(Result);
  end;
end;

function TBasicController.CountVowels(const aRlsname: String): Integer;
begin
  Result := 0;

  for i := 1 to Length(aRlsname) do
  begin
    if (aRlsname[i] in ['a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U']) then
      Inc(Result);
  end;
end;

function TBasicController.GetYear(const aRlsname: String; const aRlsnameWordsList: TStringList): Integer;
var
  fYear: Integer;
begin
  for i := aRlsnameWordsList.Count - 1 downto 0 do
  begin
    fYear := StrToIntDef(aRlsnameWordsList[i], 0);
    if fYear > 1900 then
      break;
  end;
  if fYear < 1900 then
    fYear := 0;

  Result := fYear;
end;

function TBasicController.GetNumberOfDiscs(const aRlsnameWordsList: TStringList): Integer;
var
  fDiscs: Integer;
  i, j: Integer;
begin
  fDiscs := 1;
  for i := aRlsnameWordsList.Count - 1 downto 0 do
  begin
    if ContainsText(aRlsnameWordsList[i], 'disc') then
    begin
      fDiscs := 0;
      j := 1;
      while (j <= Length(aRlsnameWordsList[i])) do
      begin
        if aRlsnameWordsList[i][j] in ['0'..'9'] then
          fDiscs := fDiscs * 10 + Ord(aRlsnameWordsList[i][j]) - 48
        else
          Break;
        Inc(j);
      end;

      Break;
    end;
  end;

  Result := fDiscs;
end;

constructor TZeroDayController.Create(const aRefController: IUnknown; const aSection, aRlsname: String);
begin
  inherited Create(aRefController);
  FZeroDayInfoStore := TZeroDayController.Create(aSection, aRlsname);

  FZeroDayInfoStore.FZeroDaySource := ExtractZeroDayTag(aRlsname);
end;

destructor TZeroDayController.Destroy;
begin
  FZeroDayInfoStore.Free;
  inherited;
end;

function TZeroDayController.GetZeroDayInfo: TZeroDayInfoStore;
begin
  Result := FZeroDayInfoStore;
end;

function TZeroDayController.ExtractZeroDayTag(const aRlsname: String): String;
begin
  for i := words.Count - 1 downto 1 do // TODO: need access to words list from TBasicInfo
  begin
    for j := 0 to GlNullDayPlatformTags.Count - 1 do
    begin
      if (AnsiContainsText(GlNullDayPlatformTags.ValueFromIndex[j], ' ' + words[i] + ' ')) then
      begin
        Result := GlNullDayPlatformTags.Names[j];
        Break;
      end;
    end;

    if Result <> '' then
      Break;
  end;

  if Result = '' then
    Result := 'WIN';
end;

end.

