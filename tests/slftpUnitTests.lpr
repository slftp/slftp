program slftpUnitTests;

{$MODE Delphi} //< delphi compatible mode

{$if FPC_FULLVERSION < 30200}
  {$stop Please upgrade your Free Pascal Compiler version to at least 3.2.0 }
{$endif}

{$IFDEF WINDOWS}
  {$APPTYPE CONSOLE}
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
  consoletestrunner,
  Classes, SysUtils,
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF}
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
  App: TTestRunner;
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

  // run the registered tests, exit code is set by the runner
  // (number of errors + failures, 0 on success);
  // useful command line options: --all --format=plain / --suite=NAME / -l
  App := TTestRunner.Create(nil);
  App.Initialize;
  App.Title := 'slFtp unit tests';
  App.Run;
  App.Free;

  // Exit without running unit finalization sections. The tested units keep
  // global state that is never properly uninitialized, and there is a
  // (not yet located) memory corruption which makes the RTL finalization
  // (DoneLocalTime) free an invalid pointer: with mormot.core.fpcx64mm the
  // memory manager then spins forever in LockMediumBlocks and the process
  // never exits (this is the "test runner waits for <Enter>" hang).
  // Everything finalization would clean up here is leaked anyway.
{$IFDEF UNIX}
  BaseUnix.fpExit(ExitCode);
{$ENDIF}
end.
