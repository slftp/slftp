unit slftpUnitTestsSetup;

interface

{* calls might depend on each other, so order might be important *}
procedure InitialConfigSetup;
procedure InitialDebugSetup;
procedure InitialKbSetup;
procedure InitialSLLanguagesSetup;

implementation

uses
  configunit, debugunit, encinifile, kb, sllanguagebase;

procedure InitialConfigSetup;
var
  fDefaultPassword: String;
begin
  fDefaultPassword := 'nopw';
  ConfigInit(fDefaultPassword);
end;

procedure InitialDebugSetup;
begin
  DebugInit;
end;

procedure InitialKbSetup;
begin
  kb_Init;
end;

procedure InitialSLLanguagesSetup;
begin
  SLLanguagesInit;
end;

end.