unit sllanguagebase;

interface

uses SysUtils, Classes, Contnrs, IniFiles;

type

  TSLLanguages = class
  private
    inisection, fexpression, flang: string;
    ffileindex: integer;
  public
    constructor Create(IniSection, Language, Expression: string; fileindex: integer);
    procedure Rehash;
    property Language: string Read flang;
    property Expression: string Read fexpression;
  end;

 //procedure SLLanguagesReload();overload;
 //procedure SLLanguagesReload(out status:string);overload;

function SLLanguagesReload: string;

procedure SLLanguagesRehash(out status: string);

procedure SLLanguages_Init;
procedure SLLanguages_Uninit;

function SLLanguagesFindLanguage(Text: string; mp3language: boolean = False): string;
  overload;
function SLLanguagesFindLanguage(Text: TStringList;
  mp3language: boolean = False): string; overload;

//procedure FindLanguage(text:);

function FindSLLanguage(Language: string; mp3language: boolean = False): TSLLanguages;


function FindLanguageOnDirectory(Text: string; mp3language: boolean = False): string;

//(rls, section: string): string;


var
  sllanguages:    TObjectList;
  //slmp3languages: TObjectList;
  sllanguagefile: TIniFile;

implementation

uses mystrings, debugunit, Regexpr, irc;

const
  rsections = 'sllanguagebase';

{TSLLanguages}

constructor TSLLanguages.Create(IniSection: string; Language: string;
  Expression: string; fileindex: integer);
begin
  inisection := IniSection;
  ffileindex := fileindex;
  flang      := Language;
  fexpression := Expression;
end;

procedure TSLLanguages.Rehash;
begin
  fexpression := sllanguagefile.ReadString(inisection, flang, '');
end;

{SLLanguages Find Utils}

function FindSLLanguage(Language: string; mp3language: boolean = False): TSLLanguages;
var
  i: integer;
begin
  Result := nil;
(*
  if mp3language then
  begin
    for I := 0 to slmp3languages.Count - 1 do
      if TSLLanguages(slmp3languages.Items[i]).Language = Language then begin
        result:=TSLLanguages(slmp3languages.Items[i]);
        break;
      end;
  end else begin
  *)
  for I := 0 to sllanguages.Count - 1 do
    if TSLLanguages(sllanguages.Items[i]).Language = Language then
    begin
      Result := TSLLanguages(sllanguages.Items[i]);
      break;
    end;
  //end;      
end;


function SLLanguagesFindLanguage(Text: string; mp3language: boolean = False): string;
var
  i:      integer;
  lrx:    TRegexpr;
  sllang: TSLLanguages;
begin
  Result := 'English';
  lrx    := TRegexpr.Create;
  lrx.ModifierI := True;
  try
(*
  if mp3language then
  begin
    for I := 0 to slmp3languages.Count - 1 do begin
      sllang:=TSLLanguages(slmp3languages.Items[i]);
      lrx.Expression:='[\-]('+sllang.Expression+')[\-]';
      if lrx.Exec(text) then begin
        result:=sllang.Language;
        break;
      end;
    end;
  end else begin
*)
    for I := 0 to sllanguages.Count - 1 do
    begin
      sllang := TSLLanguages(sllanguages.Items[i]);
      lrx.Expression := '[\.\-\_](' + sllang.Expression + ')[\.\-\_]';
      if lrx.Exec(Text) then
      begin
        Result := sllang.Language;
        break;
      end;
    end;
    //  end;

  finally
    lrx.Free;
  end;

end;

function SLLanguagesFindLanguage(Text: TStringList; mp3language: boolean = False): string;
var
  i: integer;
  s: string;
begin
  Result := 'English';
  for I := 0 to Text.Count - 1 do
  begin
    s := '';
    s := SLLanguagesFindLanguage(Text.Strings[i], mp3language);
    if s <> '' then
    begin
      Result := s;
      break;
    end;
  end;
end;



function FindLanguageOnDirectory(Text: string; mp3language: boolean = False): string;
var
  i:      integer;
  lrx:    TRegexpr;
  sllang: TSLLanguages;
begin
  Result := 'English';
  lrx    := TRegexpr.Create;
  lrx.ModifierI := True;
  try
  (*
  if mp3language then
  begin
    for I := 0 to slmp3languages.Count - 1 do begin
      sllang:=TSLLanguages(slmp3languages.Items[i]);
      lrx.Expression:='[\-]('+sllang.Expression+')[\-]';
      if lrx.Exec(text) then begin
        result:=sllang.Language;
        break;
      end;
    end;
  end else begin
  *)

    for I := 0 to sllanguages.Count - 1 do
    begin
      sllang := TSLLanguages(sllanguages.Items[i]);
      lrx.Expression := '[\.\-\_](' + sllang.Expression + ')[\.\-\_]';
      if lrx.Exec(Text) then
      begin
        Result := sllang.Language;
        break;
      end;
    end;

    //  end;

  finally
    lrx.Free;
  end;

end;

{SLLanguages Common Utils}

procedure SLLanguages_Init;
var
  i: integer;
  y: TStringList;
begin
  Debug(dpSpam, rsections, 'Loading Language Base....');
  sllanguages := TObjectList.Create;
  //should we rename this file to slftp.languagebase ?
  y := TStringList.Create;
  y.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'languagebase.slftp');
  for I := 0 to y.Count - 1 do
  begin

    if ((y.Strings[i][1] = '[') and (y.Strings[i][length(y.Strings[i])] = ']')) then
      Continue;

    sllanguages.Add(TSLLanguages.Create('languages', SubString(y.Strings[i], '=', 1),
      SubString(y.Strings[i], '=', 2), i));
  end;

  y.Free;

  Debug(dpSpam, rsections, 'Done! ' + IntToStr(sllanguages.Count));
end;

procedure SLLanguages_Uninit;
begin
  Debug(dpSpam, rsections, 'Uninit1');
  sllanguages.Free;
  // slmp3languages.Free;
  sllanguagefile.Free;
  Debug(dpSpam, rsections, 'Uninit2');
end;



function SLLanguagesReload: string;
var
  i: integer;
  y: TStringList;
begin
  Debug(dpSpam, rsections, 'Reload Language Base....');
  y := TStringList.Create;
  try
    sllanguages.Clear;
    y.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'languagebase.slftp');
    for I := 0 to y.Count - 1 do
    begin
      sllanguages.Add(TSLLanguages.Create('languages', SubString(y.Strings[i], '=', 1),
        SubString(y.Strings[i], '=', 2), i));
      Result := Format('Reload done! %d languages loaded.', [sllanguages.Count]);
    end;
  finally
    y.Free;
  end;
end;

{
procedure SLLanguagesReload();
var i: Integer; y: TStringList;
begin
sllanguages.Clear;
Debug(dpSpam, rsections, 'Reload Language Base....');
y:= TStringList.Create;
y.LoadFromFile(ExtractFilePath(ParamStr(0))+'languagebase.slftp');
for I := 0 to y.Count - 1 do begin
sllanguages.Add(TSLLanguages.create('languages',SubString(y.Strings[i],'=',1),SubString(y.Strings[i],'=',2),i));
end;
(*
//  slmp3languages.Clear;

//  if sllanguagefile <> nil then sllanguagefile.free;
//  sllanguagefile:= TIniFile.Create(ExtractFilePath(ParamStr(0))+ 'languagebase.slftp');

  x:= TStringList.Create;
  sllanguagefile.ReadSection('languages', x);
  for i:= 0 to x.Count -1 do
  begin
    s:=sllanguagefile.ReadString('languages', x[i], '');
    sllanguages.Add(TSLLanguages.create('languages', x[i],s,i));
  end;
  x.Free;

  x:= TStringList.Create;
  sllanguagefile.ReadSection('mp3languages', x);
  for i:= 0 to x.Count -1 do
  begin
    s:=sllanguagefile.ReadString('mp3languages', x[i], '');
    slmp3languages.Add(TSLLanguages.create('mp3languages', x[i],s,i));
  end;
  *)
  y.Free;
end;

procedure SLLanguagesReload(out status:string);
var i: Integer; y: TStringList;
begin
sllanguages.Clear;
Debug(dpSpam, rsections, 'Reload Language Base....');
y:= TStringList.Create;
y.LoadFromFile(ExtractFilePath(ParamStr(0))+'languagebase.slftp');
for I := 0 to y.Count - 1 do begin
sllanguages.Add(TSLLanguages.create('languages',SubString(y.Strings[i],'=',1),SubString(y.Strings[i],'=',2),i));
end;
(*
//  slmp3languages.Clear;
  if sllanguagefile <> nil then sllanguagefile.free;
  sllanguagefile:= TIniFile.Create(ExtractFilePath(ParamStr(0))+'languagebase.slftp');

  x:= TStringList.Create;
  sllanguagefile.ReadSection('languages', x);
  for i:= 0 to x.Count -1 do
  begin
    s:=sllanguagefile.ReadString('languages', x[i], '');
    sllanguages.Add(TSLLanguages.create('languages', x[i],s,i));
  end;
  x.Free;

  x:= TStringList.Create;
  sllanguagefile.ReadSection('mp3languages', x);
  for i:= 0 to x.Count -1 do
  begin
    s:=sllanguagefile.ReadString('mp3languages', x[i], '');
    slmp3languages.Add(TSLLanguages.create('mp3languages', x[i],s,i));
  end;
*)
  y.Free;

  status:=Format('Reload done! %d languages loaded.',[sllanguages.Count]);
end;

}

procedure SLLanguagesRehash(out status: string);
var
  i: integer;
begin
  for I := 0 to sllanguages.Count - 1 do
    TSLLanguages(sllanguages.Items[i]).Rehash;
  status := Format('Done! Languages count: %d', [sllanguages.Count]);
end;

end.

