unit versioninfo;

interface

{ Builds a slftp version string in the format 'slFtp v[VERSION.NUMBER]'
  @returns(slftp version string) }
function GetFullVersionString: String;
{ Gets the slftp version string in the format '[VERSION.NUMBER]'
  @returns(slftp version number as string) }
function GetVersionOnlyString: String;
{ Gets the slftp version string in the format '[VERSION.NUMBER]' and appends custom title if set
  @returns(console title string) }
function GetSLConsoleTitle: String;

implementation

uses
  configunit, SysUtils;

{$I slftp.inc}

function GetFormattedSLFTPVersion: String;
var
  fCPU: String;
begin
  {$IFDEF FPC}
    {$IFDEF CPU32}
      fCPU := '32-Bit';
    {$ELSE}
      fCPU := '64-Bit';
    {$ENDIF}
  {$ELSE}
    {$IFDEF MSWINDOWS}
      {$IFDEF WIN32}
        fCPU := '32-Bit';
      {$ELSE}
        fCPU := '64-Bit';
      {$ENDIF}
    {$ELSE}
      {$IFDEF LINUX32}
        fCPU := '32-Bit';
      {$ELSE}
        fCPU := '64-Bit';
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF CPUARM}
    fCPU := fCPU + ' on ARM';
  {$ENDIF}

  if sl_rev = '' then
    Result := Format('slFtp v%s {%s}',[SL_VERSION, fCPU])
  else
    Result := Format('slFtp v%s {%s} (git# %s)',[SL_VERSION, fCPU, SL_REV]);
end;

function GetFullVersionString: String;
begin
  Result := GetFormattedSLFTPVersion;
end;

function GetVersionOnlyString: String;
var
  src: String;
begin
  src := GetFormattedSLFTPVersion;
  Result := Copy(src, Pos('v', src) + 1, Length(src));
end;

function GetSLConsoleTitle: String;
var
  s: String;
begin
  s := config.ReadString('console', 'customtitle', '');
  if s <> '' then
    result := Format('%s %s', [GetFormattedSLFTPVersion, s])
  else
    result := GetFormattedSLFTPVersion;
end;

end.