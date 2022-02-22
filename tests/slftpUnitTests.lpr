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
    SynFPCx64MM,
  {$ELSE}
    cmem,
  {$ENDIF}
  {$IFDEF TextRunner}
    TextTestRunner,
  {$ELSE}
    GUITestRunner,
  {$ENDIF}
  Classes, SysUtils,
  mrdohutils,
  SynSQLite3,
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
  imdbDatabaseTests;

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
  sqlite3 := TSQLite3LibraryDynamic.Create({$IFDEF MSWINDOWS}SQLITE_LIBRARY_DEFAULT_NAME{$ELSE}'./libsqlite3.so'{$ENDIF});
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


  // run all registered tests
{$IFDEF TextRunner}
  // halt on error, means exit code <> 0
  RunRegisteredTests(rxbHaltOnFailures);
{$ELSE}
  RunRegisteredTests;
{$ENDIF}
end.
