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

initialization
  {$IFDEF FPC}
    RegisterTest('dirlist helpers', TTestDirlistHelpers.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestDirlistHelpers);
  {$ENDIF}
end.
