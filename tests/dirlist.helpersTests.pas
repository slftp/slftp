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
    procedure TestIsValidFilename1;
    procedure TestIsValidFilename2;
    procedure TestIsValidFilename3;
  end;

implementation

uses
  SysUtils,
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

initialization
  {$IFDEF FPC}
    RegisterTest('dirlist helpers', TTestDirlistHelpers.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestDirlistHelpers);
  {$ENDIF}
end.