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
  mrdohutils,
  slftpUnitTestsSetup,
  // add all test units below
  mystringsTests,
  mystringsTests.Base64,
  httpTests,
  ircblowfish.ECBTests,
  ircblowfish.CBCTests,
  tagsTests,
  ircblowfish.plaintextTests,
  dbtvinfoTests,
  sllanguagebaseTests,
  mygrouphelpersTests,
  globalskipunitTests,
  irccolorunitTests;

var
  filecheck: String;
begin
  filecheck := CommonFileCheck;
  if filecheck <> '' then
  begin
    System.Write(filecheck);
    System.Write('Done. press <Enter> key to quit.');
    System.Readln;
    exit;
  end;

  {* setup needed internal variables, etc *}
  InitialConfigSetup;
  InitialDebugSetup;
  InitialKbSetup;
  InitialSLLanguagesSetup;
  InitialGlobalskiplistSetup;
  InitialTagsSetup;


  // run all registered tests
{$IFDEF TextRunner}
  // halt on error, means exit code <> 0
  RunRegisteredTests(rxbHaltOnFailures);
{$ELSE}
  RunRegisteredTests;
{$ENDIF}
end.