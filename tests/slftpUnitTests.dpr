program slftpUnitTests;


// switch between GUI or CONSOLE (text) test runner
// use .$DEFINE for undef
{$DEFINE TextRunner}


{$if CompilerVersion < 32}
  {$MESSAGE Fatal 'Please upgrade your compiler to at least Delphi 10.2 Tokyo Release 3 (10.2.3)'}
{$endif}

{$IF Defined(TextRunner)}
  {$IFDEF MSWINDOWS}
    {$APPTYPE CONSOLE}
  {$ENDIF}
{$ELSE}
  {$IFDEF MSWINDOWS}
    {$APPTYPE GUI}
  {$ENDIF}
{$ENDIF}

uses
  FastMM4,
  DUnitX.TestFramework,
  {$IFDEF TextRunner}
    DUnitX.Loggers.Console,
  {$ELSE}
    FMX.Forms,
    DUnitX.Loggers.GUIX,
  {$ENDIF}
  Classes, SysUtils,
  mrdohutils,
  slftpUnitTestsSetup,
  // add all test units below
  Base64OpenSSLTests,
  mystringsTests,
  httpTests,
  ircblowfish.ECBTests,
  ircblowfish.CBCTests,
  tagsTests,
  ircblowfish.plaintextTests,
  dbtvinfoTests,
  sllanguagebaseTests;

// allow more user mode address space
{$SetPEFlags $20}

var
  runner : ITestRunner;
  results : IRunResults;
  logger : ITestLogger;
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


  // run all registered tests
  {$IFDEF TextRunner}
    try
      // check for command line commands
      TDUnitX.CheckCommandLine;
      // create runner
      runner := TDUnitX.CreateRunner;
      runner.UseRTTI := True;
      // tell the runner how we will log things
      logger := TDUnitXConsoleLogger.Create(true);
      runner.AddLogger(logger);

      // run tests
      results := runner.Execute;
      if not results.AllPassed then
        System.ExitCode := EXIT_ERRORS;

      if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
      begin
        System.Write('Done. press <Enter> key to quit.');
        System.Readln;
      end;
    except
      on E: Exception do
        System.Writeln(E.ClassName, ': ', E.Message);
    end;
  {$ELSE}
    Application.Initialize;
    Application.CreateForm(TGUIXTestRunner, GUIXTestRunner);
    Application.Run;
  {$ENDIF}
end.