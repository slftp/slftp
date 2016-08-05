unit versioninfo;

interface

function Get_VersionString(exename: AnsiString): AnsiString;overload;
function Get_VersionString: AnsiString;overload;

function cmod_VersionString: AnsiString;


//Change the inc file to mod the version

{$I slftp.inc}

implementation

uses configunit,sysutils;

function ShowSLFTPVerison:AnsiString;
begin
if sl_rev = '' then
Result:=Format('slFtp v%s',[SL_VERSION]) else
Result:=Format('slFtp v%s (git# %s)',[SL_VERSION,sl_rev]);
end;


function Get_VersionString(exename: AnsiString): AnsiString;
begin
//  Result:= 'slftp-'+sl_version;
  result:=ShowSLFTPVerison;
end;

function Get_VersionString: AnsiString;
begin
  Result:= ShowSLFTPVerison;
end;


function cmod_VersionString: AnsiString;
var s:AnsiString;
begin
s:=config.ReadString('console','customtitle','');
if s <> '' then result:= Format('%s %s',[ShowSLFTPVerison,s]) else result:= ShowSLFTPVerison;
end;


end.
