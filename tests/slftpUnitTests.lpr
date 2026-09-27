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
  fpcunit, testregistry, plaintestreport,
  slftpUnitTestsJUnitReport,
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

{* returns the value of the --junit=<filename> command line option
   @returns(the file name, or empty string if the option is not present) *}
function GetJUnitFileNameOption: String;
var
  i: Integer;
  fParam: String;
begin
  Result := '';
  for i := 1 to ParamCount do
  begin
    fParam := ParamStr(i);
    if Copy(fParam, 1, 8) = '--junit=' then
      Exit(Copy(fParam, 9, MaxInt));
  end;
end;

{* runs all registered tests in a single run, writing the normal plain
   console output and additionally a JUnit XML report (for the GitLab CI
   test report)
   @param(aJUnitFileName file the JUnit XML report is written to)
   @returns(exit code, same semantics as consoletestrunner: bit 0 set on
     failures, bit 1 set on errors) *}
function RunAllTestsWithJUnitReport(const aJUnitFileName: String): Integer;
var
  fTestResult: TTestResult;
  fPlainWriter: TPlainResultsWriter;
  fJUnitWriter: TslJUnitResultsWriter;
begin
  fTestResult := TTestResult.Create;
  fPlainWriter := TPlainResultsWriter.Create(nil);
  fJUnitWriter := TslJUnitResultsWriter.Create(nil);
  try
    fJUnitWriter.FileName := aJUnitFileName;
    fTestResult.AddListener(fPlainWriter);
    fTestResult.AddListener(fJUnitWriter);
    GetTestRegistry.Run(fTestResult);
    fPlainWriter.WriteResult(fTestResult);
    fJUnitWriter.WriteResult(fTestResult);
    Result := Ord(fTestResult.NumberOfFailures <> 0);
    if fTestResult.NumberOfErrors <> 0 then
      Result := Result or 2;
  finally
    fTestResult.Free;
    fPlainWriter.Free;
    fJUnitWriter.Free;
  end;
end;

var
  filecheck: String;
  junitFileName: String;
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

  junitFileName := GetJUnitFileNameOption;
  if junitFileName <> '' then
  begin
    // --junit=<file> was given: single run of all tests with plain console
    // output plus a JUnit XML report file (used by CI test reports)
    ExitCode := RunAllTestsWithJUnitReport(junitFileName);
  end
  else
  begin
    // run the registered tests, exit code is set by the runner
    // (number of errors + failures, 0 on success);
    // useful command line options: --all --format=plain / --suite=NAME / -l
    App := TTestRunner.Create(nil);
    App.Initialize;
    App.Title := 'slFtp unit tests';
    App.Run;
    App.Free;
  end;

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
