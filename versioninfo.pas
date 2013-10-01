unit versioninfo;

interface

function Get_VersionString(exename: string): string;overload;
function Get_VersionString: string;overload;

function cmod_VersionString: string;


//Change the inc file to mod the version

{$I slftp.inc}

implementation

uses configunit,sysutils;

function ShowSLFTPVerison:string;
begin
if sl_rev = '' then
result:='slFtp v'+SL_VERSION else
result:='slFtp v'+SL_VERSION+' rev'+sl_rev;
end;


function Get_VersionString(exename: string): string;
begin
//  Result:= 'slftp-'+sl_version;
  result:=ShowSLFTPVerison;
end;

function Get_VersionString: string;
begin
  Result:= ShowSLFTPVerison;
end;


function cmod_VersionString: string;
var s:string;
begin
s:=config.ReadString('console','customtitle','');
if s <> '' then result:= Format('%s %s',[ShowSLFTPVerison,s]) else result:= ShowSLFTPVerison;
end;


end.
