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
    cthreads,
  {$ENDIF}
  {$IFDEF CPUX86_64}
    mormot.core.fpcx64mm,
  {$ELSE}
    cmem,
  {$ENDIF}
  {$IFDEF TextRunner}
    TextTestRunner,
  {$ELSE}
    GUITestRunner,
  {$ENDIF}
  Classes, SysUtils,
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF}
  TestFrameworkProxyIfaces,
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
  irccolorunitTests,
  ircparsingTests,
  slmasksTests,
  dirlist.helpersTests,
  dirlistTests,
  precatcher.helpersTests,
  kb.releaseinfo.MP3Tests,
  kb.releaseinfo.NullDayTests,
  kb.releaseinfo.MVIDTests,
  taskhttpimdbTests,
  slsslTests,
  sitesunitTests,
  precatcherTests,
  slcriticalsection2Tests,
  variantCacheTests,
  sltimerTests;

var
  filecheck: String;
  testresult: ITestResult;
begin
  filecheck := CommonFileCheck;
  if filecheck <> '' then
  begin
    System.Write(filecheck);
    System.WriteLn('Missing config files, tests will not work correctly!');
    halt(2);
  end;

  {* setup needed internal variables, etc *}
  InitialConfigSetup;
  InitialDebugSetup;
  InitialKbSetup;
  InitialSLLanguagesSetup;
  InitialGlobalskiplistSetup;
  InitialTagsSetup;
  InitialDirlistSetup;
  InitialDbAddImdbSetup;
  InitialPrecatcherSetup;
  InitialKnownGroupsSetup;
  InitialSkiplistSetup;
  InitialFakeSetup;


  // run all registered tests
{$IFDEF TextRunner}
  testresult := RunRegisteredTests(rxbContinue);
{$ELSE}
  testresult := RunRegisteredTests;
{$ENDIF}

  // Exit without running unit finalization sections. The tested units keep
  // global state that is never properly uninitialized, and there is a
  // (not yet located) memory corruption which makes the RTL finalization
  // (DoneLocalTime) free an invalid pointer: with mormot.core.fpcx64mm the
  // memory manager then spins forever in LockMediumBlocks and the process
  // never exits (this is the "test runner waits for <Enter>" hang).
  // Everything finalization would clean up here is leaked anyway.
  if (testresult <> nil) and (not testresult.WasSuccessful) then
  begin
  {$IFDEF UNIX}
    BaseUnix.fpExit(testresult.ErrorCount + testresult.FailureCount);
  {$ELSE}
    halt(testresult.ErrorCount + testresult.FailureCount);
  {$ENDIF}
  end;
{$IFDEF UNIX}
  BaseUnix.fpExit(0);
{$ENDIF}
end.
