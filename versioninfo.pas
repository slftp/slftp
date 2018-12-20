unit versioninfo;

interface

{ Builds a slftp version string in the format 'slFtp v[VERSION.NUMBER]'
  @returns(slftp version string) }
function GetFullVersionString: String; overload;
{ Gets the slftp version string in the format '[VERSION.NUMBER]'
  @returns(slftp version number as string) }
function GetVersionOnlyString: String;
{ Gets the slftp version string in the format '[VERSION.NUMBER]' and appends custom title if set
  @returns(console title string) }
function GetConsoleTitle: String;

implementation

uses
  configunit, SysUtils, mystrings;

{$I slftp.inc}

function ShowSLFTPVerison: String;
begin
  if sl_rev = '' then
    Result := Format('slFtp v%s',[SL_VERSION])
  else
    Result := Format('slFtp v%s (git# %s)',[SL_VERSION, SL_REV]);
end;

function GetFullVersionString: String;
begin
  Result := ShowSLFTPVerison;
end;

function GetVersionOnlyString: String;
var
  src: String;
begin
  src := ShowSLFTPVerison;
  Result := mystrings.RightStr(src, Pos('v', src));
end;

function GetConsoleTitle: String;
var
  s: String;
begin
  s := config.ReadString('console', 'customtitle', '');
  if s <> '' then
    result := Format('%s %s', [ShowSLFTPVerison, s])
  else
    result := ShowSLFTPVerison;
end;

end.