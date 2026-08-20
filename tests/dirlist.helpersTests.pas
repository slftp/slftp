unit dirlist.helpersTests;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility, DUnitX.Assert;
  {$ENDIF}

type
  TTestDirlistHelpers = class(TTestCase)
  published
    procedure TestIsFtpRushScrewedUpFile1;
    procedure TestIsFtpRushScrewedUpFile2;
    procedure TestIsFtpRushScrewedUpFile3;
    procedure TestIsFtpRushScrewedUpFile4;
    procedure TestIsFtpRushScrewedUpFile5;
    procedure TestIsFtpRushScrewedUpFile6;
    procedure TestIsFtpRushScrewedUpFile7;
    procedure TestParseStatResponseLineGlftpd1;
    procedure TestParseStatResponseLineGlftpd2;
    procedure TestParseStatResponseLineDrftpd1;
    procedure TestParseStatResponseLineDrftpd2;
    procedure TestParseStatResponseLineComplexDirname;
    procedure TestParseStatResponseLineInvalidFilesize;
    procedure TestReleaseContainsNFOOnly;
    procedure TestIsValidFilename1;
    procedure TestIsValidFilename2;
    procedure TestIsValidFilename3;
    procedure TestIsValidDirname1;
    procedure TestIsValidDirname2;
    procedure TestParseStatResponseMultiLine;
  end;

implementation

uses
  SysUtils, Generics.Collections,
  dirlist.helpers;

{ TTestDirlistHelpers }

procedure TTestDirlistHelpers.TestIsFtpRushScrewedUpFile1;
var
  fFilenameStr, fFileExtensionStr: String;
begin
  fFilenameStr := 'sor88bue(1).zip';
  fFileExtensionStr := '.zip';

  CheckTrue(IsFtpRushScrewedUpFile(fFilenameStr, fFileExtensionStr), 'File is fucked up!');
end;

procedure TTestDirlistHelpers.TestIsFtpRushScrewedUpFile2;
var
  fFilenameStr, fFileExtensionStr: String;
begin
  fFilenameStr := '2012.doomsday.2008.100.million.bc.2008.complete.bluray-untouched.r03';
  fFileExtensionStr := '.r03';

  CheckFalse(IsFtpRushScrewedUpFile(fFilenameStr, fFileExtensionStr), 'File is not fucked up!');
end;

procedure TTestDirlistHelpers.TestIsFtpRushScrewedUpFile3;
var
  fFilenameStr, fFileExtensionStr: String;
begin
  fFilenameStr := '06-lemuria-bristles_and_whiskers.flac';
  fFileExtensionStr := '.flac';

  CheckFalse(IsFtpRushScrewedUpFile(fFilenameStr, fFileExtensionStr), 'File is not fucked up!');
end;

procedure TTestDirlistHelpers.TestIsFtpRushScrewedUpFile4;
var
  fFilenameStr, fFileExtensionStr: String;
begin
  fFilenameStr := 'test(1).nfo';
  fFileExtensionStr := '.nfo';

  CheckTrue(IsFtpRushScrewedUpFile(fFilenameStr, fFileExtensionStr), 'File is fucked up!');
end;

procedure TTestDirlistHelpers.TestIsFtpRushScrewedUpFile5;
var
  fFilenameStr, fFileExtensionStr: String;
begin
  fFilenameStr := '00-va-indie_dance_nu_disco_stars_vol_2-(dmrcva009)-web-2020-cover(1).jpg';
  fFileExtensionStr := '.jpg';

  CheckTrue(IsFtpRushScrewedUpFile(fFilenameStr, fFileExtensionStr), 'File is fucked up!');
end;

procedure TTestDirlistHelpers.TestIsFtpRushScrewedUpFile6;
var
  fFilenameStr, fFileExtensionStr: String;
begin
  fFilenameStr := '00-bleached-ride_your_heart-cd-flac-2013-proof(1).jpg';
  fFileExtensionStr := '.jpg';

  CheckTrue(IsFtpRushScrewedUpFile(fFilenameStr, fFileExtensionStr), 'File is fucked up!');
end;

procedure TTestDirlistHelpers.TestIsFtpRushScrewedUpFile7;
var
  fFilenameStr, fFileExtensionStr: String;
begin
  fFilenameStr := 'watchable-swordclaw-watchable-1080p(1975).nfo';
  fFileExtensionStr := '.nfo';

  CheckFalse(IsFtpRushScrewedUpFile(fFilenameStr, fFileExtensionStr), 'File is legit!');
end;

procedure TTestDirlistHelpers.TestParseStatResponseLineGlftpd1;
var
  fTmp: String;
  fDirMask, fUsername, fGroupname, fDatum, fFilename: String;
  fFilesize: Int64;
begin
  fTmp := 'drwxrwxrwx   2 aq11     iND              3 Apr 19 23:14 Sample';
  ParseStatResponseLine(fTmp, fDirMask, fUsername, fGroupname, fFilesize, fDatum, fFilename);

  CheckEquals('drwxrwxrwx', fDirMask);
  CheckEquals('aq11', fUsername);
  CheckEquals('iND', fGroupname);
  CheckEquals(3, fFilesize);
  CheckEquals('Apr 19 23:14', fDatum);
  CheckEquals('Sample', fFilename);
end;

procedure TTestDirlistHelpers.TestParseStatResponseLineGlftpd2;
var
  fTmp: String;
  fDirMask, fUsername, fGroupname, fDatum, fFilename: String;
  fFilesize: Int64;
begin
  fTmp := '-rw-r--r--   1 abc      Friends  100000000 Apr 13 20:14 baby.animals.s01e05.little.hunters.internal.2160p.uhdtv.h265-cbfm.r00';
  ParseStatResponseLine(fTmp, fDirMask, fUsername, fGroupname, fFilesize, fDatum, fFilename);

  CheckEquals('-rw-r--r--', fDirMask);
  CheckEquals('abc', fUsername);
  CheckEquals('Friends', fGroupname);
  CheckEquals(100000000, fFilesize);
  CheckEquals('Apr 13 20:14', fDatum);
  CheckEquals('baby.animals.s01e05.little.hunters.internal.2160p.uhdtv.h265-cbfm.r00', fFilename);
end;

procedure TTestDirlistHelpers.TestParseStatResponseLineDrftpd1;
var
  fTmp: String;
  fDirMask, fUsername, fGroupname, fDatum, fFilename: String;
  fFilesize: Int64;
begin
  fTmp := '-rw-rw-rw- 1   nobody  nogroup      12724352 Feb  6 12:07 01-daniel_kandi-nova_ii_(the_second_journey)_(original_mix).mp3';
  ParseStatResponseLine(fTmp, fDirMask, fUsername, fGroupname, fFilesize, fDatum, fFilename);

  CheckEquals('-rw-rw-rw-', fDirMask);
  CheckEquals('nobody', fUsername);
  CheckEquals('nogroup', fGroupname);
  CheckEquals(12724352, fFilesize);
  CheckEquals('Feb 6 12:07', fDatum);
  CheckEquals('01-daniel_kandi-nova_ii_(the_second_journey)_(original_mix).mp3', fFilename);
end;

procedure TTestDirlistHelpers.TestParseStatResponseLineDrftpd2;
var
  fTmp: String;
  fDirMask, fUsername, fGroupname, fDatum, fFilename: String;
  fFilesize: Int64;
begin
  fTmp := 'drwxrwxrwx 3   nobody  nogroup   27212887049 Apr  7 19:36 Disaster.Report.4.Summer.Memories-CODEX';
  ParseStatResponseLine(fTmp, fDirMask, fUsername, fGroupname, fFilesize, fDatum, fFilename);

  CheckEquals('drwxrwxrwx', fDirMask);
  CheckEquals('nobody', fUsername);
  CheckEquals('nogroup', fGroupname);
  CheckEquals(27212887049, fFilesize);
  CheckEquals('Apr 7 19:36', fDatum);
  CheckEquals('Disaster.Report.4.Summer.Memories-CODEX', fFilename);
end;

procedure TTestDirlistHelpers.TestParseStatResponseLineComplexDirname;
var
  fTmp: String;
  fDirMask, fUsername, fGroupname, fDatum, fFilename: String;
  fFilesize: Int64;
begin
  // Test parsing with complex directory name containing special characters and multiple spaces
  fTmp := 'drwxrwxrwx   2 user1    group1         10 Jan  5 12:20 [::::::::::::::] -   0% Complete - [GRP]';
  ParseStatResponseLine(fTmp, fDirMask, fUsername, fGroupname, fFilesize, fDatum, fFilename);

  CheckEquals('drwxrwxrwx', fDirMask);
  CheckEquals('user1', fUsername);
  CheckEquals('group1', fGroupname);
  CheckEquals(10, fFilesize);
  CheckEquals('Jan 5 12:20', fDatum);
  CheckEquals('[::::::::::::::] -   0% Complete - [GRP]', fFilename);
end;

procedure TTestDirlistHelpers.TestParseStatResponseLineInvalidFilesize;
var
  fTmp: String;
  fDirMask, fUsername, fGroupname, fDatum, fFilename: String;
  fFilesize: Int64;
begin
  // malformed line with non-numeric size must yield the documented -1 sentinel,
  // callers (dirlist.pas) rely on it to skip such lines
  // also covers trailing whitespace being trimmed from the item
  fTmp := 'drwxrwxrwx   2 aq11     iND              garbage Apr 19 23:14 Sample  ';
  ParseStatResponseLine(fTmp, fDirMask, fUsername, fGroupname, fFilesize, fDatum, fFilename);

  CheckEquals('drwxrwxrwx', fDirMask);
  CheckEquals(-1, fFilesize);
  CheckEquals('Sample', fFilename);
end;


procedure TTestDirlistHelpers.TestReleaseContainsNFOOnly;
begin
  CheckTrue(ReleaseOnlyConsistsOfNFO('Test.Rls.DiRFiX.asd-GRP'), 'Only NFO');
  CheckTrue(ReleaseOnlyConsistsOfNFO('Test.Rls.DIRFIX.asd-GRP'), 'Only NFO');
  CheckTrue(ReleaseOnlyConsistsOfNFO('Test.Rls.DiR.FiX.asd-GRP'), 'Only NFO');
  CheckTrue(ReleaseOnlyConsistsOfNFO('Test.Rls.DIR.FIX.asd-GRP'), 'Only NFO');
  CheckTrue(ReleaseOnlyConsistsOfNFO('Test.Rls.NFOFiX.asd-GRP'), 'Only NFO');
  CheckTrue(ReleaseOnlyConsistsOfNFO('Test.Rls.NFOFIX.asd-GRP'), 'Only NFO');
  CheckTrue(ReleaseOnlyConsistsOfNFO('Test.Rls.NFO.FiX.asd-GRP'), 'Only NFO');
  CheckTrue(ReleaseOnlyConsistsOfNFO('Test.Rls.NFO.FIX.asd-GRP'), 'Only NFO');
  CheckTrue(ReleaseOnlyConsistsOfNFO('Test.Rls.PROOFFiX.asd-GRP'), 'Only NFO');
  CheckTrue(ReleaseOnlyConsistsOfNFO('Test.Rls.PROOFFIX.asd-GRP'), 'Only NFO');
  CheckTrue(ReleaseOnlyConsistsOfNFO('Test.Rls.PROOF.FiX.asd-GRP'), 'Only NFO');
  CheckTrue(ReleaseOnlyConsistsOfNFO('Test.Rls.PROOF.FIX.asd-GRP'), 'Only NFO');
  CheckTrue(ReleaseOnlyConsistsOfNFO('Test.Rls.COVERFiX.asd-GRP'), 'Only NFO');
  CheckTrue(ReleaseOnlyConsistsOfNFO('Test.Rls.COVERFIX.asd-GRP'), 'Only NFO');
  CheckTrue(ReleaseOnlyConsistsOfNFO('Test.Rls.COVER.FiX.asd-GRP'), 'Only NFO');
  CheckTrue(ReleaseOnlyConsistsOfNFO('Test.Rls.COVER.FIX.asd-GRP'), 'Only NFO');
  CheckTrue(ReleaseOnlyConsistsOfNFO('Test.Rls.SAMPLEFiX.asd-GRP'), 'Only NFO');
  CheckTrue(ReleaseOnlyConsistsOfNFO('Test.Rls.SAMPLE.FIX.asd-GRP'), 'Only NFO');
  CheckTrue(ReleaseOnlyConsistsOfNFO('Test-RLS-NNN-XX-NFOFIX-1337-GRP'), 'Only NFO');
  CheckFalse(ReleaseOnlyConsistsOfNFO('Test.Rls.asd-GRP'), 'Only NFO');
end;

procedure TTestDirlistHelpers.TestIsValidFilename1;
var
  fFilename: String;
begin
  fFilename := 'Disaster.Report.4.Summer.Memories-CODEX';
  CheckTrue(IsValidFilename(fFilename), 'Input is valid!');

  fFilename := 'German.AVC.Retail.Movie.Ruleset.V1.0-SiGNEDGROUPS';
  CheckTrue(IsValidFilename(fFilename), 'Input is valid!');

  fFilename := 'allistrue20c-dvdr-bfhdvd-sample.vob';
  CheckTrue(IsValidFilename(fFilename), 'Input is valid!');

  fFilename := 'gua-rollerball1975-2160p.r31';
  CheckTrue(IsValidFilename(fFilename), 'Input is valid!');
end;

procedure TTestDirlistHelpers.TestIsValidFilename2;
var
  fFilename: String;
begin
  fFilename := '.';
  CheckFalse(IsValidFilename(fFilename), 'Input is not valid! Only a dot.');

  fFilename := '..';
  CheckFalse(IsValidFilename(fFilename), 'Input is not valid! Only two dots.');

  // from https://github.com/pzs-ng/pzs-ng/blob/master/configGen/config.yaml#L514-L517
  fFilename := '.diz';
  CheckFalse(IsValidFilename(fFilename), 'Input is not valid! Starts with a dot.');

  fFilename := '.debug';
  CheckFalse(IsValidFilename(fFilename), 'Input is not valid! Starts with a dot.');

  fFilename := '.url';
  CheckFalse(IsValidFilename(fFilename), 'Input is not valid! Starts with a dot.');
end;

procedure TTestDirlistHelpers.TestIsValidFilename3;
var
  fFilename: String;
begin
  // global_skip tests
  fFilename := 'file_id.diz';
  CheckFalse(IsValidFilename(fFilename), 'Input is not valid! Globally skipped.');

  fFilename := 'imdb.nfo';
  CheckFalse(IsValidFilename(fFilename), 'Input is not valid! Globally skipped.');

  fFilename := '[IMDB]=-_Score_6.6_-_Action-Sci_-_unknown_Screens_(1975)_-=[IMDB]';
  CheckFalse(IsValidFilename(fFilename), 'Input is not valid! Globally skipped.');
end;

procedure TTestDirlistHelpers.TestIsValidDirname1;
var
  fFilename: String;
begin
  fFilename := '..';
  CheckFalse(IsValidDirname(fFilename), 'Input is not valid! Starts with a dot.');

  fFilename := '[IMDB]=-_Score_6.6_-_Action-Sci_-_unknown_Screens_(1975)_-=[IMDB]';
  CheckFalse(IsValidDirname(fFilename), 'Input is not valid! Globally skipped.');

  fFilename := 'Test.Rls.asdf.XXX-GRP';
  CheckTrue(IsValidDirname(fFilename), 'This is a valid dirname.');

  //don't skip complete tag dirs as we need those
  fFilename := '[xxx] - ( 1337M 22F - COMPLETE ) - [xxx]';
  CheckTrue(IsValidDirname(fFilename), 'This is a valid dirname.');

  fFilename := '[0 of 2 files = 0% complete at 123.4MB]';
  CheckTrue(IsValidDirname(fFilename), 'This is a valid dirname.');
end;

procedure TTestDirlistHelpers.TestIsValidDirname2;
var
  fFilename: String;
begin
  //test some short dir names
  fFilename := 'A';
  CheckTrue(IsValidDirname(fFilename), 'This is a valid dirname.');

  fFilename := '0-9';
  CheckTrue(IsValidDirname(fFilename), 'This is a valid dirname.');

  fFilename := 'CD1';
  CheckTrue(IsValidDirname(fFilename), 'This is a valid dirname.');

  fFilename := 'Subs';
  CheckTrue(IsValidDirname(fFilename), 'This is a valid dirname.');

  fFilename := 'Sample';
  CheckTrue(IsValidDirname(fFilename), 'This is a valid dirname.');

  fFilename := 'Proof';
  CheckTrue(IsValidDirname(fFilename), 'This is a valid dirname.');
end;

procedure TTestDirlistHelpers.TestParseStatResponseMultiLine;
var
  fRawResponse: String;
  fEntries: TObjectList<TParsedDirlistEntry>;
begin
  fRawResponse := 
    'drwxrwxrwx   2 aq11     iND              3 Apr 19 23:14 Sample'#10 +
    '-rw-r--r--   1 abc      Friends  100000000 Apr 13 20:14 file1.rar'#10 +
    '-rw-rw-rw- 1   nobody  nogroup      12724352 Feb  6 12:07 file2.mp3'#10 +
    'drwxrwxrwx 3   nobody  nogroup   27212887049 Apr  7 19:36 Release-GRP';
    
  fEntries := ParseStatResponse(fRawResponse);
  try
    CheckEquals(4, fEntries.Count, 'Should have parsed 4 entries');
    
    // Check first entry (glFTPd style)
    CheckEquals('drwxrwxrwx', fEntries[0].DirMask);
    CheckEquals('aq11', fEntries[0].Username);
    CheckEquals('Sample', fEntries[0].Filename);
    
    // Check second entry
    CheckEquals('100000000', IntToStr(fEntries[1].Filesize));
    CheckEquals('file1.rar', fEntries[1].Filename);
    
    // Check third entry (DrFTPD style)
    CheckEquals('nobody', fEntries[2].Username);
    CheckEquals('nogroup', fEntries[2].Groupname);
    CheckEquals('Feb 6 12:07', fEntries[2].Date);
    CheckEquals('file2.mp3', fEntries[2].Filename);
    
    // Check last entry
    CheckEquals('Release-GRP', fEntries[3].Filename);
  finally
    fEntries.Free;
  end;
end;

initialization
  {$IFDEF FPC}
    RegisterTest('dirlist helpers', TTestDirlistHelpers.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestDirlistHelpers);
  {$ENDIF}
end.
