{
 Freeware, Copyright              .
 must be included GLP3            ;    .
 it under the terms of the GNU   01.   .
 General Public License as       01;, .:
 as published by the             0100 11
 Free Software Foundation;       1010 01
 either version 1 or any later   0100 01
 version....                   : 1001 00 :
                               :   `0 01 :
         _______     _         :  _____._:___              _
        /      /    / \___     __/          /             / \________
       /  ,___/____/     /     \    _      / \___________/          /
    ___\____      /     /______/    /_____/             /    _     /
   /__     /     /     /      /     ___/ /____      ___/     /    /
     /    /     /     /      /     /2o!  :   /     /  /     _____/
    /     _____/_____       /__   /     .:  /     /  /__   /
   /__   /          /___   /...\_/....:::: /__   /  .   \_/
      \_.001.  1    .100._/            ...    \_/   .
         01    10      10               ::          :
        `10.11 .0  11.01'                `          :
          1000.  .  .000'                 ........  :
          `'`101. .101'`1.......:.........:      :..'
            . `10100'.:         :         :      :
     --->   :.  `10z.`:  <-- ---+- slFtp -+-     :
            1:   .`10f.         :         `......:
            01  .1  `00r.       :.............'
            00  0:  .100'       :...Legend...:'
            01  01.101'
            10  0101' .   This program is distributed in the hope that it will be useful,
            01  01'  .1   but WITHOUT ANY WARRANTY; without even the implied warranty of
            0:  10   00   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
            :.  1    10   GNU General Public License for more details.
            .   1    0:
                0.    :.
                .     .            http://www.gnu.org/licenses/gpl-3.0.en.html
}

program slftpUnitTests;

{$if CompilerVersion < 33}
  {$MESSAGE Fatal 'Please upgrade your compiler to at least Delphi 10.3 Rio (10.3.x)'}
{$endif}

{$IFNDEF TESTINSIGHT}
  {$APPTYPE CONSOLE}
{$ENDIF}

uses
  FastMM5,
  {$IFDEF TESTINSIGHT}
    TestInsight.DUnitX,
  {$ENDIF}
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework,
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
  irccolorunitTests,
  ircparsingTests,
  slmasksTests,
  dirlist.helpersTests,
  precatcher.helpersTests,
  kb.releaseinfo.MP3Tests;

// allow more user mode address space
{$SetPEFlags $20}

var
  runner : ITestRunner;
  results : IRunResults;
  logger : ITestLogger;
  nunitLogger : ITestLogger;
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
  InitialDirlistSetup;

  {$IFDEF TESTINSIGHT}
    TestInsight.DUnitX.RunRegisteredTests;
    exit;
  {$ENDIF}

  try
    // Check command line options, will exit if invalid
    TDUnitX.CheckCommandLine;
    // Create the test runner
    runner := TDUnitX.CreateRunner;
    // Tell the runner to use RTTI to find Fixtures
    runner.UseRTTI := True;
    // tell the runner how we will log things
    // Log to the console window
    logger := TDUnitXConsoleLogger.Create(true);
    runner.AddLogger(logger);
    // Generate an NUnit compatible XML File
    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(ExtractFilePath(ParamStr(0)) + 'dunitx-results.xml');
    runner.AddLogger(nunitLogger);
    runner.FailsOnNoAsserts := False; //When true, Assertions must be made during tests

    // Run tests
    results := runner.Execute;
    if not results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
end.
