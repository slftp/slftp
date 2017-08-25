unit versioninfo;

interface

function Get_VersionString(const exename: AnsiString): AnsiString; overload;
function Get_VersionString: AnsiString; overload;
function Get_VersionOnlyString: AnsiString;
function cmod_VersionString: AnsiString;


//Change the inc file to mod the version

{$I slftp.inc}

implementation

uses
  configunit, SysUtils, mystrings;

function ShowSLFTPVerison: AnsiString;
begin
  if sl_rev = '' then
    Result := Format('slFtp v%s',[SL_VERSION])
  else
    Result := Format('slFtp v%s (git# %s)',[SL_VERSION, SL_REV]);
end;


function Get_VersionString(const exename: AnsiString): AnsiString;
begin
  result := ShowSLFTPVerison;
end;

function Get_VersionString: AnsiString;
begin
  Result := ShowSLFTPVerison;
end;

function Get_VersionOnlyString: AnsiString;
var
  src: AnsiString;
begin
  src := ShowSLFTPVerison;
  Result := mystrings.RightStr(src, Pos('v', src));
end;

function cmod_VersionString: AnsiString;
var
  s: AnsiString;
begin
  s := config.ReadString('console','customtitle','');
  if s <> '' then
    result := Format('%s %s',[ShowSLFTPVerison,s])
  else
    result := ShowSLFTPVerison;
end;


end.
