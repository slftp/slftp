unit sllanguagebase;

interface

uses
  SysUtils, Classes, Contnrs, Generics.Collections;

type
 { @abstract(Class for language and its expression(s) in scene tagging) }
  TSLLanguages = class
  private
    FLanguage: String; //< language (text before equality sign in file)
    FExpression: String; //< lowercased expression(s) which are used in tagging for these language (text after equality sign in file)
    FExpressionLength: Integer; //< String length of @link(FExpression)
    FFileIndex: integer; //< row number from file with the infos
    FIsRegexExpression: Boolean; //< if @true it uses slower regex matching, otherwise it uses fast RTL Pos() matching
  public
    { Just a helper function to create a new @link(TSLLanguages) class
      @param(aLanguage language)
      @param(aExpression expression(s) for these language (automatically lowercased))
      @param(aFileIndex row number in file)
      @param(aIsRegexExpression set to @true if Language should be found via regex matching, otherwise set it to @false ) }
    constructor Create(const aLanguage, aExpression: String; const aFileIndex: integer; const aIsRegexExpression: Boolean);
    property Language: String read FLanguage; //< language
    property Expression: String read FExpression; //< lowercased expression(s) which are used in tagging for these language
    property ExpressionLength: Integer read FExpressionLength; //< String length of @link(Expression)
    property NeedsRegexMatching: Boolean read FIsRegexExpression; //< @true if @link(Expression) is a regex pattern, @false otherwise
  end;

{ Just a helper function to initialize @link(sllanguages) with languages from slftp.languagebase and @link(slmp3languages) with infos from @link(kb.mp3languages) }
procedure SLLanguagesInit;
{ Just a helper function to free @link(sllanguages) and @link(slmp3languages) }
procedure SLLanguagesUninit;
{ Reads mp3languages from kb section in slftp inifile and stores them uppercased in @link(aStringList), @italic(list will be automatically cleared)
  @param(aStringList Stringlist which should be used for storing MP3 languages) }
procedure SLLoadMP3LanguagesFromIniFile(var aStringList: TStringList);
{ Loads current values from files to @link(sllanguages) and @link(slmp3languages)
  @returns(Count of loaded languages and mp3 languages) }
function SLLanguagesReload: String;
{ Iterates through @link(sllanguage) and searches with @link(TSLLanguages.Expression) in @link(aRlsname)
  @param(aRlsname string in which it searches for the language)
  @returns(@link(TSLLanguages.Language) string if found, otherwise default 'English') }
function FindLanguageOnDirectory(const aRlsname: String): String;
{ Iterates through @link(slmp3languages) and searches with each string of it in @link(aRlsname)
  @param(aRlsname string in which it searches for the language)
  @returns(Language string of matched @link(slmp3languages) if found, otherwise default 'EN') }
function FindMusicLanguageOnDirectory(const aRlsname: String): String;

var
  sllanguages: TObjectList<TSLLanguages>; //< Holds @link(TSLLanguages) objects with the expressions/language from slftp.languagebase
  slmp3languages: TStringList; //< Holds mp3 languages from kb section from inifile (same as @link(kb.mp3languages))

implementation

uses
  configunit, mystrings, debugunit, Regexpr, StrUtils;

const
  rsections = 'sllanguagebase';

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

      fIsRegexPattern := {$IFDEF UNICODE}ContainsText{$ELSE}AnsiContainsText{$ENDIF}(SubString(y.Strings[i], '=', 2), '|');

      sllang := TSLLanguages.Create(SubString(y.Strings[i], '=', 1), SubString(y.Strings[i], '=', 2), i, fIsRegexPattern);
      aObjectList.Add(sllang);
    end;
  finally
    y.Free;
  end;
end;

{ TSLLanguages }

constructor TSLLanguages.Create(const aLanguage, aExpression: String; const aFileIndex: integer; const aIsRegexExpression: Boolean);
begin
  FLanguage := aLanguage;
  FExpression := LowerCase(aExpression);
  FExpressionLength := Length(aExpression);
  FFileIndex := aFileIndex;
  FIsRegexExpression := aIsRegexExpression;
end;

procedure SLLanguagesInit;
var
  i: integer;
  y: TStringList;
begin
  Debug(dpSpam, rsections, '-> Loading Language Base');

  sllanguages := TObjectList<TSLLanguages>.Create(True);
  _ReadLanguagesFromIniFile(sllanguages);

  Debug(dpSpam, rsections, Format('<- Ready - Languages loaded: %d', [sllanguages.Count]));

  slmp3languages := TStringList.Create;
  SLLoadMP3LanguagesFromIniFile(slmp3languages);
  Debug(dpSpam, rsections, Format('<- Ready - MP3 Languages loaded: %d', [slmp3languages.Count]));
end;

procedure SLLanguagesUninit;
begin
  Debug(dpSpam, rsections, 'Uninit1');
  sllanguages.Free;
  slmp3languages.Free;
  Debug(dpSpam, rsections, 'Uninit2');
end;

procedure SLLoadMP3LanguagesFromIniFile(var aStringList: TStringList);
begin
  aStringList.Clear;

  aStringList.Delimiter := ' ';
  aStringList.QuoteChar := '"';
  aStringList.CaseSensitive := False;
  aStringList.DelimitedText := UpperCase(config.ReadString('kb', 'mp3languages', ''));
end;

function SLLanguagesReload: String;
var
  i: integer;
  y: TStringList;
begin
  Debug(dpSpam, rsections, '-> Reload Language Base');

  _ReadLanguagesFromIniFile(sllanguages);
  SLLoadMP3LanguagesFromIniFile(slmp3languages);

  Result := Format('Reload done! %d languages and %d mp3 languages loaded.', [sllanguages.Count, slmp3languages.Count]);

  Debug(dpSpam, rsections, '<- Reload Language Base');
end;

function FindLanguageOnDirectory(const aRlsname: String): String;
var
  i, j: integer;
  lrx: TRegexpr;
  sllang: TSLLanguages;
  fRlsnameLowercase: String;
begin
  Result := 'English';
  fRlsnameLowercase := LowerCase(aRlsname);

  lrx := TRegexpr.Create;
  try
    lrx.ModifierI := True;

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
        j := Pos(sllang.Expression, fRlsnameLowercase);
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

function FindMusicLanguageOnDirectory(const aRlsname: String): String;
var
  i, j: integer;
  fRlsnameUppercase: String;
begin
  Result := 'EN';
  fRlsnameUppercase := UpperCase(aRlsname);

  for i := 0 to slmp3languages.Count - 1 do
  begin
    j := Pos(slmp3languages[i], fRlsnameUppercase);
    if (j <> 0) then
    begin
      if (aRlsname[j - 1] = '-') and (aRlsname[j + Length(slmp3languages[i])] = '-') then
      begin
        Result := slmp3languages[i];
        break;
      end;
    end;
  end;
end;

end.
