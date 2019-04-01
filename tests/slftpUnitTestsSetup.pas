unit slftpUnitTestsSetup;

interface

{* calls might depend on each other, so order might be important *}
procedure InitialConfigSetup;
procedure InitialDebugSetup;

implementation

uses
  configunit, debugunit, encinifile;

procedure InitialConfigSetup;
begin
  // works with the default values only
  config := TEncIniFile.Create('test', 'nopw');
end;

procedure InitialDebugSetup;
begin
  DebugInit;
end;

end.