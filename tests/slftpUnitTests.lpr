program slftpUnitTests;


// switch between GUI or CONSOLE (text) test runner
// use .$DEFINE for undef
{$DEFINE TextRunner}


{$MODE Delphi} //< delphi compatible mode

{$if FPC_FULLVERSION < 30200}
  {$stop Please upgrade your Free Pascal Compiler version to at least 3.2.0 }
{$endif}

{$IFDEF TextRunner}
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
  {$IFDEF UNIX}
    {$IFNDEF CPUARM}
      FastMM4,
    {$ENDIF}
    cthreads,
    cmem,
  {$ENDIF}
  {$IFDEF TextRunner}
    TextTestRunner,
  {$ELSE}
    GUITestRunner,
  {$ENDIF}
  Classes, SysUtils,
  slftpUnitTestsSetup,
  // add all test units below
  Base64OpenSSLTests,
  mystringsTests,
  httpTests,
  ircblowfish.ECBTests,
  ircblowfish.CBCTests;

begin
  {* setup needed internal variables, etc *}
  InitialConfigSetup;
  InitialDebugSetup;


  // seems including other slftp units lets fptest start a new terminal window
  // instead writting to actual terminal not sure why but if you only
  // include Base64OpenSSLTests & mystringsTests it works flawless and results
  // are printed to current terminal window and can be seen after running tests
  // sl units are automatically included due to adding it to uses lists e.g. in slftpUnitTestsSetup


  // run all registered tests
{$IFDEF TextRunner}
  // halt on error, means exit code <> 0
  RunRegisteredTests(rxbHaltOnFailures);
{$ELSE}
  RunRegisteredTests;
{$ENDIF}
end.