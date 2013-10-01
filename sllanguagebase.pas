unit sllanguagebase;

interface

uses sysutils, classes,Contnrs, IniFiles;


type

TSLLanguages = class
private
inisection, fexpression,flang:string;
ffileindex:integer;
public
constructor create(IniSection, Language,Expression:string;fileindex:integer);
procedure Rehash;
property Language:string read flang;
property Expression:string read fexpression;
end;

//procedure SLLanguagesReload();overload;
//procedure SLLanguagesReload(out status:string);overload;

function SLLanguagesReload:string;

procedure SLLanguagesRehash(out status:string);

procedure SLLanguages_Init;
procedure SLLanguages_Uninit;

function SLLanguagesFindLanguage(text:string; mp3language: boolean = false):string;overload;
function SLLanguagesFindLanguage(text:tstringlist; mp3language: boolean = false):string;overload;

//procedure FindLanguage(text:);

function FindSLLanguage(Language:string; mp3language: boolean = false):TSLLanguages;


function FindLanguageOnDirectory(text:string; mp3language: boolean = false):string;

//(rls, section: string): string;


var
sllanguages: TObjectList;
//slmp3languages: TObjectList;
sllanguagefile:TIniFile;

implementation

uses mystrings,debugunit,Regexpr, irc;

const rsections = 'sllanguagebase';

{TSLLanguages}

constructor TSLLanguages.create(IniSection: string; Language: string; Expression: string; fileindex: Integer);
begin
  inisection := IniSection;
  ffileindex:= fileindex;
  flang:=  Language;
  fexpression:= Expression;
end;

procedure TSLLanguages.Rehash;
begin
  fexpression:= sllanguagefile.ReadString(inisection, flang, '');
end;

{SLLanguages Find Utils}

function FindSLLanguage(Language:string; mp3language: boolean = false):TSLLanguages;
var i:integer;
begin
  result:=nil;
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
      if TSLLanguages(sllanguages.Items[i]).Language = Language then begin
        result:=TSLLanguages(sllanguages.Items[i]);
        break;
      end;
//end;      
end;


function SLLanguagesFindLanguage(text:string; mp3language: boolean = false):string;
var i:integer; lrx:TRegexpr; sllang:TSLLanguages;
begin
  result:='English';
  lrx:=TRegexpr.Create;
  lrx.ModifierI:=True;
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
    for I := 0 to sllanguages.Count - 1 do begin
      sllang:=TSLLanguages(sllanguages.Items[i]);
      lrx.Expression:='[\.\-\_]('+sllang.Expression+')[\.\-\_]';
      if lrx.Exec(text) then begin
        result:=sllang.Language;
        break;
      end;
    end;
//  end;

  finally
  lrx.free;
  end;

end;

function SLLanguagesFindLanguage(text:tstringlist; mp3language: boolean = false):string;
var i:integer;s:string;
begin
  result:='English';
  for I := 0 to text.Count - 1 do begin
    s:='';
    s:=SLLanguagesFindLanguage(text.Strings[i], mp3language);
    if s <> '' then begin
      result:=s;
      break;
    end;
  end;
end;



function FindLanguageOnDirectory(text:string; mp3language: boolean = false):string;
var i:integer; lrx:TRegexpr; sllang:TSLLanguages;
begin
  result:='English';
  lrx:=TRegexpr.Create;
  lrx.ModifierI:=True;
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

    for I := 0 to sllanguages.Count - 1 do begin
      sllang:=TSLLanguages(sllanguages.Items[i]);
      lrx.Expression:='[\.\-\_]('+sllang.Expression+')[\.\-\_]';
      if lrx.Exec(text) then begin
        result:=sllang.Language;
        break;
      end;
    end;

//  end;

  finally
  lrx.free;
  end;

end;

{SLLanguages Common Utils}

procedure SLLanguages_Init;
var i: Integer; y: TStringList;
begin
Debug(dpSpam, rsections, 'Loading Language Base....');
sllanguages:= TObjectList.Create;
y:= TStringList.Create;
y.LoadFromFile(ExtractFilePath(ParamStr(0))+'languagebase.slftp');
for I := 0 to y.Count - 1 do begin

if ((y.Strings[i][1] = '[') and (y.Strings[i][length(y.Strings[i])] = ']')) then Continue;

sllanguages.Add(TSLLanguages.create('languages',SubString(y.Strings[i],'=',1),SubString(y.Strings[i],'=',2),i));
end;



(*
  Debug(dpSpam, rsections, 'Loading Language Base....');
  sllanguages:= TObjectList.Create;
  slmp3languages:= TObjectList.Create;
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
  y.free;
  
  Debug(dpSpam, rsections, 'Done! '+inttostr(sllanguages.Count));
end;

procedure SLLanguages_Uninit;
begin
  Debug(dpSpam, rsections, 'Uninit1');
  sllanguages.Free;
 // slmp3languages.Free;
  sllanguagefile.Free;
  Debug(dpSpam, rsections, 'Uninit2');
end;



function SLLanguagesReload:string;
var i: Integer; y: TStringList;
begin
Debug(dpSpam, rsections, 'Reload Language Base....');
y:= TStringList.Create;
try
sllanguages.Clear;
y.LoadFromFile(ExtractFilePath(ParamStr(0))+'languagebase.slftp');
for I := 0 to y.Count - 1 do begin
sllanguages.Add(TSLLanguages.create('languages',SubString(y.Strings[i],'=',1),SubString(y.Strings[i],'=',2),i));
result:=Format('Reload done! %d languages loaded.',[sllanguages.Count]);
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

procedure SLLanguagesRehash(out status:string);
var i: Integer;
begin
  for I := 0 to sllanguages.Count - 1 do
    TSLLanguages(sllanguages.Items[i]).Rehash;
  status:=Format('Done! Languages count: %d',[sllanguages.Count]);
end;

end.
