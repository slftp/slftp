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

initialization
  {$IFDEF FPC}
    RegisterTest('dirlist helpers', TTestDirlistHelpers.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestDirlistHelpers);
  {$ENDIF}
end.
