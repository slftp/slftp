unit mrdohutils;

interface

uses encinifile, slmd5, configunit, irc, debugunit, sysutils;

procedure InitmRdOHConfigFiles;
procedure UninitmRdOHConfigFiles;

//function RehashPreurls:boolean;
//function AddPreURL(url:string;offset:string = '0'):boolean;

function ReadMaxUptimeRecord: Integer;

procedure CheckForNewMaxUpTime;

function CommonFileCheck: AnsiString;

{ # Path functions    #}
function GetBINPath: AnsiString;

function GetETCPath: AnsiString;

function GetUSRPath: AnsiString;
function GetHelpPath: AnsiString;
function GetBACKUPPath: AnsiString;
function GetCBACKUPPath: AnsiString;
function GetDBSPath: AnsiString;
function GetSRULESPath: AnsiString;

function GetVARPath: AnsiString;
function GetLOGSPath: AnsiString;
function GetRLSLOGSPath: AnsiString;

function RandomHEXString(strlength: integer = 16): AnsiString;

var
  preurls: TEncStringlist;
  spamcfg: TEncIniFile;
  last_max_uptime_check: TDateTime;

implementation

uses mainthread, DateUtils, sitesunit, mystrings;

function RandomHEXString(strlength: integer = 16): AnsiString;
var
  temp: integer;
begin
  result := '';
  randomize;
  repeat
    temp := random(2048);
    if temp in [48..57 {0-1}, 65..71 {A-F}, 97..103 {a-f}] then
      result := result + Chr(temp);
  until length(result) = strlength;
end;

function ReadMaxUptimeRecord: Integer;
begin
  result := sitesdat.ReadInteger('default', 'maxuptime', -1);
end;

procedure CheckForNewMaxUpTime;
begin
  if SecondsBetween(now, started) > ReadMaxUptimeRecord then
    sitesdat.WriteInteger('default', 'maxuptime', SecondsBetween(now, started));
  sitesdat.WriteString('default', 'MaxUptimeAsString',
    DateTimeAsString(started));
  sitesdat.UpdateFile;
end;

function CommonFileCheck: AnsiString;
var
  s: AnsiString;
begin
  result := '';
  s := ExtractFilePath(ParamStr(0));
  if not fileexists(s + 'slftp.ini') then
    result := result + 'slftp.ini not found!' + #10#13;
  if not fileexists(s + 'imdbcountrys.nwo') then
    result := result + 'imdbcountrys.nwo not found!' + #10#13;
  if not fileexists(s + 'languagebase.slftp') then
    result := result + 'languagebase.slftp not found!' + #10#13;
  if not fileexists(s + 'slftp.knowngroups') then
    result := result + 'slftp.knowngroups not found!' + #10#13;
  if not fileexists(s + 'slftp.languages') then
    result := result + 'slftp.languages not found!' + #10#13;
  if not fileexists(s + 'slftp.precatcher') then
    result := result + 'slftp.precatcher not found!' + #10#13;
  if not fileexists(s + 'slftp.skip') then
    result := result + 'slftp.skip not found!' + #10#13;
  if not fileexists(s + 'slftp.skipgroups') then
    result := result + 'slftp.skipgroups not found!' + #10#13;
  if not fileexists(s + 'slftp.spamconf') then
    result := result + 'slftp.spamconf not found!' + #10#13;
end;

(*
function RehashPreurls:boolean;
begin
result:=False;
if preurls = nil then Exit;
preurls.Clear;
preurls.LoadFromFile(ExtractFilePath(ParamStr(0))+'slftp.preurls');
// preurls.LoadFromFile(ExtractFilePath(ParamStr(0))+'etc'+PathDelim+'preurls.slftp');

result:=True;
end;

function AddPreURL(url:string;offset:string = '0'):boolean;
begin
result:=False;
try
preurls.Add(url+';'+offset);
finally
result:=True;
end;
end;
*)

{ # Path functions    #}

(*
ExtractFilePath(ParamStr(0))
'etc'
PathDelim
*)

function Getrootpath: AnsiString;
begin
  result := ExtractFilePath(ParamStr(0));
end;

function GetBINPath: AnsiString;
begin
  result := Getrootpath + 'bin' + PathDelim;
end;

function GetETCPath: AnsiString;
begin
  result := Format('%setc%s', [Getrootpath, PathDelim]);
end;

{ --------------------------------------- }

function GetUSRPath: AnsiString;
begin
  result := Format('%susr%s', [Getrootpath, PathDelim]);
end;

function GetDBSPath: AnsiString;
begin
  result := Format('%sdbs%s', [GetUSRPath, PathDelim]);
end;

function GetSRULESPath: AnsiString;
begin
  result := Format('%ssiterules%s', [GetUSRPath, PathDelim]);
end;

function GetHelpPath: AnsiString;
begin
  result := Format('%shelp%s', [GetUSRPath, PathDelim]);
end;

function GetBACKUPPath: AnsiString;
begin
  result := Format('%sbackups%s', [GetUSRPath, PathDelim]);
end;

function GetCBACKUPPath: AnsiString;
begin
  result := Format('%scustom_backups%s', [GetUSRPath, PathDelim]);
end;

function GetVARPath: AnsiString;
begin
  result := Format('%svar%s', [Getrootpath, PathDelim]);
end;

function GetLOGSPath: AnsiString;
begin
  result := Format('%slogs%s', [GetVARPath, PathDelim]);
end;

function GetRLSLOGSPath: AnsiString;
begin
  result := Format('%sreeleaselog%s', [GetVARPath, PathDelim]);
end;

procedure InitmRdOHConfigFiles;
begin
  last_max_uptime_check := now;
  spamcfg := TEncIniFile.Create(ExtractFilePath(ParamStr(0)) + 'slftp.spamconf',
    '');
  //spamcfg:= TEncIniFile.Create(GetETCPath+'slftp.spamconf', '');
end;

procedure UninitmRdOHConfigFiles;
begin
  if spamcfg <> nil then
  begin
    spamcfg.free;
    spamcfg := nil;
  end;
  (*
  if preurls <> nil then begin
  preurls.free;
  preurls:=nil;
  end;
  *)
end;

end.

