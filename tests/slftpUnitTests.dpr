program slftpUnitTests;


// switch between GUI or CONSOLE (text) test runner
// use .$DEFINE for undef
{$DEFINE TextRunner}


{$if CompilerVersion < 32}
  {$MESSAGE Fatal 'Please upgrade your compiler to at least Delphi 10.2 Tokyo Release 3 (10.2.3)'}
{$endif}

{$IF Defined(TextRunner)}
  {$IFDEF WINDOWS}
    {$APPTYPE CONSOLE}
  {$ENDIF}
{$ELSE}
  {$DEFINE GUIRunner}
  {$IFDEF WINDOWS}
    {$APPTYPE GUI}
  {$ENDIF}
{$ENDIF}

uses
  FastMM4,
  {$IFDEF TextRunner}
    TextTestRunner,
  {$ELSE}
    GUITestRunner,
  {$ENDIF}
  Classes, SysUtils,
  // add all test units below
  Base64OpenSSLTests;

// allow more user mode address space
{$SetPEFlags $20}

begin
  // run all registered tests
{$IFDEF TextRunner}
  // halt on error, means exit code <> 0
  RunRegisteredTests(rxbHaltOnFailures);
{$ELSE}
  RunRegisteredTests;
{$ENDIF}
end.