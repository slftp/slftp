unit versioninfo;

interface

function Get_VersionString(const exename: String): String; overload;
function Get_VersionString: String; overload;
function Get_VersionOnlyString: String;
function cmod_VersionString: String;


//Change the inc file to mod the version

{$I slftp.inc}

implementation

uses
  configunit, SysUtils, mystrings;

function ShowSLFTPVerison: String;
begin
  if sl_rev = '' then
    Result := Format('slFtp v%s',[SL_VERSION])
  else
    Result := Format('slFtp v%s (git# %s)',[SL_VERSION, SL_REV]);
end;


function Get_VersionString(const exename: String): String;
begin
  result := ShowSLFTPVerison;
end;

function Get_VersionString: String;
begin
  Result := ShowSLFTPVerison;
end;

function Get_VersionOnlyString: String;
var
  src: String;
begin
  src := ShowSLFTPVerison;
  Result := mystrings.RightStr(src, Pos('v', src));
end;

function cmod_VersionString: String;
var
  s: String;
begin
  s := config.ReadString('console','customtitle','');
  if s <> '' then
    result := Format('%s %s',[ShowSLFTPVerison,s])
  else
    result := ShowSLFTPVerison;
end;


end.
