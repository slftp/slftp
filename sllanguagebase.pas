unit sllanguagebase;

interface

uses
  SysUtils, Classes, Contnrs, Generics.Collections;

type
  { @abstract(Class for language and its expression(s) in scene tagging) }
  TSLLanguages = class
  private
    FLanguage: String; //< language (text before equality sign in file)
    FExpression: String; //< expression(s) which are used in tagging for these language (text after equality sign in file)
    FExpressionLength: Integer; //< String length of @link(FExpression)
    FFileIndex: integer; //< row number from file with the infos
    FIsRegexExpression: Boolean; //< if @true it uses slower regex matching, otherwise it uses fast RTL Pos() matching
  public
    { Just a helper function to create a new @link(TSLLanguages) class
      @param(aLanguage language)
      @param(aExpression expression(s) for these language)
      @param(aFileIndex row number in file)
      @param(aIsRegexExpression set to @true if Language should be found via regex matching (indicated by | in expression), otherwise set it to @false ) }
    constructor Create(const aLanguage, aExpression: String; const aFileIndex: integer; const aIsRegexExpression: Boolean);

    property Language: String read FLanguage; //< language
    property Expression: String read FExpression; //< expression(s) which are used in tagging for these language
    property ExpressionLength: Integer read FExpressionLength; //< String length of @link(Expression)
    property NeedsRegexMatching: Boolean read FIsRegexExpression; //< @true if @link(Expression) is a regex pattern, @false otherwise
  end;

  { @abstract(Class for music language in scene tagging) }
  TMusicLanguage = class
  private
    FLanguageCode: String; //< uppercased music language code
  public
    { Just a helper function to create a new @link(TMusicLanguage) class
      @param(aLanguageCode Language code) }
    constructor Create(const aLanguageCode: String);

    property LanguageCode: String read FLanguageCode; //< music language code
  end;

{ Just a helper function to initialize @link(sllanguages) with languages from slftp.languagebase and @link(slmusiclanguages) with infos from [kb] section }
procedure SLLanguagesInit;

{ Just a helper function to free @link(sllanguages) and @link(slmusiclanguages) }
procedure SLLanguagesUninit;

{ Loads current values from files to @link(sllanguages) and @link(slmusiclanguages)
  @returns(Count of loaded languages and music languages) }
function SLLanguagesReload: String;

{ Creates a list of all expression(s) for every language from @link(sllanguages), @italic(input list will be automatically cleared)
  @param(aStringList Stringlist which should be used for storing the list of language expressions) }
procedure SLGetLanguagesExpression(var aStringList: TStringList);

{ Iterates through @link(sllanguage) and searches with @link(TSLLanguages.Expression) in @link(aRlsname)
  @param(aRlsname string in which it searches for the language)
  @returns(@link(TSLLanguages.Language) string if found, otherwise default 'English') }
function FindLanguageOnDirectory(const aRlsname: String): String;

{ Iterates through @link(slmusiclanguages) and searches with each string of it in @link(aRlsname) with respect to the specific tagging convention (-LANG-)
  @param(aRlsname string in which it searches for the language)
  @returns(Language string of matched @link(slmusiclanguages) if found, otherwise default 'EN') }
function FindMusicLanguageOnDirectory(const aRlsname: String): String;

{ Iterates through @link(slmusiclanguages) and searches with each string of it in @link(aRlsname) with respect to the specific tagging convention (-LANG- or .LANG.)
  @param(aRlsname string in which it searches for the language)
  @returns(Language string of matched @link(slmusiclanguages) if found, otherwise default 'EN') }
function FindMusicVideoLanguageOnDirectory(const aRlsname: String): String;

{ Iterates through @link(slmusiclanguages) and compares to @link(aLanguage)
  @param(aLanguage string with a language to compare to, to check for existance)
  @returns(@True if string @link(aLanguage) is a valid language code in @link(slmusiclanguages), otherwise @False) }
function VerifyMusicLanguage(const aLanguage: String): boolean;

implementation

uses
  StrUtils, configunit, mystrings, debugunit, Regexpr;

const
  rsections = 'sllanguagebase';

var
  sllanguages: TObjectList<TSLLanguages>; //< Holds @link(TSLLanguages) objects with the expressions/language from slftp.languagebase
  slmusiclanguages: TObjectList<TMusicLanguage>; //< Holds music languages from kb section in inifile

{ Reads languages from slftp.languagebase and stores them in @link(aObjectList), @italic(list will be automatically cleared)
  @param(aObjectList ObjectList which should be used for storing languages) }
procedure _ReadLanguagesFromIniFile(var aObjectList: TObjectList<TSLLanguages>);
var
  i: Integer;
  y: TStringList;
  sllang: TSLLanguages;
  fIsRegexPattern: Boolean;
begin
  aObjectList.Clear;

  y := TStringList.Create;
  try
    y.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'slftp.languagebase');
    for i := 0 to y.Count - 1 do
    begin
      if ((y.Strings[i][1] = '[') and (y.Strings[i][Length(y.Strings[i])] = ']')) then
        Continue;

      // chars which indicate regex use
      fIsRegexPattern := (SubString(y.Strings[i], '=', 2).IndexOfAny(['|', '[']) <> -1);

      sllang := TSLLanguages.Create(SubString(y.Strings[i], '=', 1), SubString(y.Strings[i], '=', 2), i, fIsRegexPattern);
      aObjectList.Add(sllang);
    end;
  finally
    y.Free;
  end;
end;

{ Reads languages from slftp.ini [kb] and stores them in @link(aObjectList), @italic(list will be automatically cleared)
  @param(aObjectList ObjectList which should be used for storing music languages) }
procedure _SLLoadMusicLanguagesFromIniFile(var aObjectList: TObjectList<TMusicLanguage>);
var
  i: Integer;
  y: TStringList;
  fMusicLanguage: TMusicLanguage;
begin
  aObjectList.Clear;

  y := TStringList.Create;
  try
    y.Delimiter := ' ';
    y.QuoteChar := '"';
    y.DelimitedText := UpperCase(config.ReadString('kb', 'mp3languages', ''));

    for i := 0 to y.Count - 1 do
    begin
      fMusicLanguage := TMusicLanguage.Create(y.Strings[i]);
      aObjectList.Add(fMusicLanguage);
    end;
  finally
    y.Free;
  end;
end;

{ TSLLanguages }

constructor TSLLanguages.Create(const aLanguage, aExpression: String; const aFileIndex: integer; const aIsRegexExpression: Boolean);
begin
  FLanguage := aLanguage;
  FExpression := aExpression;
  FExpressionLength := aExpression.Length;
  FFileIndex := aFileIndex;
  FIsRegexExpression := aIsRegexExpression;
end;

{ TMusicLanguage }

constructor TMusicLanguage.Create(const aLanguageCode: String);
begin
  FLanguageCode := aLanguageCode;
end;

procedure SLLanguagesInit;
begin
  Debug(dpSpam, rsections, '-> Loading Language Base');

  sllanguages := TObjectList<TSLLanguages>.Create(True);
  _ReadLanguagesFromIniFile(sllanguages);

  Debug(dpSpam, rsections, Format('<- Ready - Languages loaded: %d', [sllanguages.Count]));

  slmusiclanguages := TObjectList<TMusicLanguage>.Create(True);
  _SLLoadMusicLanguagesFromIniFile(slmusiclanguages);
  Debug(dpSpam, rsections, Format('<- Ready - Music Languages loaded: %d', [slmusiclanguages.Count]));
end;

procedure SLLanguagesUninit;
begin
  Debug(dpSpam, rsections, 'Uninit1');
  sllanguages.Free;
  slmusiclanguages.Free;
  Debug(dpSpam, rsections, 'Uninit2');
end;

function SLLanguagesReload: String;
begin
  Debug(dpSpam, rsections, '-> Reload Language Base');

  _ReadLanguagesFromIniFile(sllanguages);
  _SLLoadMusicLanguagesFromIniFile(slmusiclanguages);

  Result := Format('Reload done! %d languages and %d music languages loaded.', [sllanguages.Count, slmusiclanguages.Count]);

  Debug(dpSpam, rsections, '<- Reload Language Base');
end;

procedure SLGetLanguagesExpression(var aStringList: TStringList);
var
  sllang: TSLLanguages;
  fStrArray: TArray<String>;
  fStr: String;
begin
  aStringList.Clear;

  for sllang in sllanguages do
  begin
    if sllang.NeedsRegexMatching then
    begin
      fStrArray := sllang.Expression.Split(['|']);
      for fStr in fStrArray do
        aStringList.Append(fStr);
    end
    else
    begin
      aStringList.Append(sllang.Expression);
    end;
  end;
end;

function FindLanguageOnDirectory(const aRlsname: String): String;
var
  j: integer;
  lrx: TRegExpr;
  sllang: TSLLanguages;
begin
  Result := 'English';

  lrx := TRegExpr.Create;
  try
    lrx.ModifierI := False;
    for sllang in sllanguages do
    begin
      if sllang.NeedsRegexMatching then
      begin
        lrx.Expression := '[\.\-\_](' + sllang.Expression + ')[\.\-\_]';
        if lrx.Exec(aRlsname) then
        begin
          Result := sllang.Language;
          break;
        end;
      end
      else
      begin
        j := Pos(sllang.Expression, aRlsname);
        if (j <> 0) then
        begin
          // make sure its [\.\-\_]language[\.\-\_]
          if (aRlsname[j - 1] in ['.', '-', '_']) and (aRlsname[j + sllang.ExpressionLength] in ['.', '-', '_']) then
          begin
            Result := sllang.Language;
            break;
          end;
        end;
      end;
    end;
  finally
    lrx.Free;
  end;
end;

function _FoundMusicLanguage(const aRlsname, aDelimiter: String; out aLanguage: String): Boolean;
var
  j: integer;
  fRlsnameStripped: String;
  fMusicLanguage: TMusicLanguage;
begin
  Result := False;

  fRlsnameStripped := ReplaceText(aRlsname, '(', '');
  fRlsnameStripped := ReplaceText(fRlsnameStripped, ')', '');

  for fMusicLanguage in slmusiclanguages do
  begin
    j := Pos(aDelimiter + fMusicLanguage.LanguageCode + aDelimiter, fRlsnameStripped);
    if (j > 1) then
    begin
      aLanguage := fMusicLanguage.LanguageCode;
      Result := True;
      break;
    end;
  end;
end;

function FindMusicLanguageOnDirectory(const aRlsname: String): String;
var
  fLanguage: String;
begin
  // language is always -LANGUAGE-
  if _FoundMusicLanguage(aRlsname, '-', fLanguage) then
    Result := fLanguage
  else
    Result := 'EN';
end;

function FindMusicVideoLanguageOnDirectory(const aRlsname: String): String;
var
  fLanguage: String;
begin
  // language can be -LANGUAGE- or .LANGUAGE.
  if _FoundMusicLanguage(aRlsname, '-', fLanguage) then
    Result := fLanguage
  else
  begin
    if _FoundMusicLanguage(aRlsname, '.', fLanguage) then
      Result := fLanguage
    else
      Result := 'EN';
  end;
end;

function VerifyMusicLanguage(const aLanguage: String): boolean;
var
  fMusicLanguage: TMusicLanguage;
begin
  Result := False;

  for fMusicLanguage in slmusiclanguages do
  begin
    if SameText(fMusicLanguage.LanguageCode, aLanguage) then
    begin
      Result := True;
      break;
    end;
  end;
end;

end.
