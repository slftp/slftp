unit mrdohutils;

interface

uses
  encinifile, slmd5, configunit, irc, debugunit, sysutils;

procedure InitmRdOHConfigFiles;
procedure UninitmRdOHConfigFiles;

//function RehashPreurls:boolean;
//function AddPreURL(url:string;offset:string = '0'):boolean;

function ReadMaxUptimeRecord: Integer;

procedure CheckForNewMaxUpTime;

{ Creates a TFileStream for @value(FileName) and returns the size of it
  @param(FileName Name of the file from which you want to know the size)
  @returns(filesize on success, else -1) }
function GetLocalFileSize(const FileName: AnsiString): Int64;
{ Checks if each file from @link(common.inc) exist and does also some size checks }
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

  {$I common.inc}

implementation

uses
  mainthread, DateUtils, sitesunit, mystrings, Classes, StrUtils;

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

  sitesdat.WriteString('default', 'MaxUptimeAsString', DateTimeAsString(started));

  sitesdat.UpdateFile;
end;


function GetLocalFileSize(const FileName: AnsiString): Int64;
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    try
      Result := FileStream.Size;
    except
      Result := -1; // file not found, better than 0 as 0 could mean empty file
    end;
  finally
    FileStream.Free;
  end;
end;

function CommonFileCheck: AnsiString;
var
  fPath: AnsiString;
  i, fsize: Integer;
  finiFound, fNeeded: boolean;
begin
  Result := '';
  finiFound := false;
  fsize := 0;
  fPath := ExtractFilePath(ParamStr(0));

  for i := 0 to iFileCount do
  begin
    if FileExists(fPath + iniFiles[i]) then
    begin
      fsize := GetLocalFileSize(fPath + iniFiles[i]);
      if (fsize > 0) then
      begin
        finiFound := true;
      end;
    end;
  end;

  if not finiFound then
  begin
    Result := Format('slftp inifile (missing or 0 byte) %s',[#10#13]);
  end;

  for i := 0 to cFilecount do
  begin
    if FileExists(fPath + commonFiles[i]) then
    begin
      fsize := GetLocalFileSize(fPath + commonFiles[i]);
      if (fsize <= 0) then
      begin
        Result := Format('%s %s (0 byte) %s',[Result, commonFiles[i], #10#13]);
      end;
    end
    else
    begin
      if commonFiles[i] = 'mirktrade.conf' then
        continue;

      Result := Format('%s %s (missing) %s',[Result, commonFiles[i], #10#13]);
    end;
  end;

  // no special case for missing files as it will be created when needed
  for i := 0 to gFilecount do
  begin
    if FileExists(fPath + generatedFiles[i]) then
    begin
      fsize := GetLocalFileSize(fPath + generatedFiles[i]);
      if (fsize <= 0) then
      begin
        Result := Format('%s %s (0 byte) %s',[Result, generatedFiles[i], #10#13]);
      end;
    end;
  end;

  if Result <> '' then
  begin
    Result := Format('slFtp can not start with/without following files: %s%s',[#10#13, Result]);
  end;
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
  spamcfg := TEncIniFile.Create(ExtractFilePath(ParamStr(0)) + 'slftp.spamconf', '');
end;

procedure UninitmRdOHConfigFiles;
begin
  if Assigned(spamcfg) then
  begin
    FreeAndNil(spamcfg);
  end;
  (*
  if preurls <> nil then begin
  preurls.free;
  preurls:=nil;
  end;
  *)
end;

end.

