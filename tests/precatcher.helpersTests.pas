unit precatcher.helpersTests;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestPrecatcherHelpers = class(TTestCase)
  published
    procedure TestRemoveSpecialCharsAndBareIt1;
    procedure TestRemoveSpecialCharsAndBareIt2;
    procedure TestRemoveSpecialCharsAndBareIt3;
    procedure TestRemoveSpecialCharsAndBareIt4;
    procedure TestStripNoValidChars1;
    procedure TestStripNoValidChars2;
    procedure TestStripNoValidChars3;
    procedure TestStripNoValidChars4;
    procedure TestIsLineCommentedOut1;
    procedure TestIsLineCommentedOut2;
    procedure TestIsLineCommentedOut3;
  end;

implementation

uses
  SysUtils, precatcher.helpers;

{ TTestPrecatcherHelpers }

procedure TTestPrecatcherHelpers.TestRemoveSpecialCharsAndBareIt1;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'NEW  > tv-hd720 < Life.and.Birth.S01E04.1080p.HDTV.x264-FTP by sltrader/BiS0N.';
  fExpectedResultStr := 'NEW  > tv-hd720 < Life.and.Birth.S01E04.1080p.HDTV.x264-FTP by sltrader BiS0N.';

  fOutputStr := RemoveSpecialCharsAndBareIt(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Cleaning failed!');
end;

procedure TTestPrecatcherHelpers.TestRemoveSpecialCharsAndBareIt2;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'NEW in games: Bandit.Point.VR-VREX by poseid0n/iND';
  fExpectedResultStr := 'NEW in games  Bandit.Point.VR-VREX by poseid0n iND';

  fOutputStr := RemoveSpecialCharsAndBareIt(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Cleaning failed!');
end;

procedure TTestPrecatcherHelpers.TestRemoveSpecialCharsAndBareIt3;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := '+ tv-hd-swe > A new car Trolljagarna.S04E08.SWEDiSH.1080p.WEB.H264-EXECUTION discovered on streets';
  fExpectedResultStr := '  tv-hd-swe > A new car Trolljagarna.S04E08.SWEDiSH.1080p.WEB.H264-EXECUTION discovered on streets';

  fOutputStr := RemoveSpecialCharsAndBareIt(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Cleaning failed!');
end;

procedure TTestPrecatcherHelpers.TestRemoveSpecialCharsAndBareIt4;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'NEW in hd-mov: -> Klovn.The.Final.2020.DANISH.1080p.BluRay.x264-CONDITION by winner/SLDev';
  fExpectedResultStr := 'NEW in hd-mov  -> Klovn.The.Final.2020.DANISH.1080p.BluRay.x264-CONDITION by winner SLDev';

  fOutputStr := RemoveSpecialCharsAndBareIt(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Cleaning failed!');
end;

procedure TTestPrecatcherHelpers.TestStripNoValidChars1;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'NEW  > tv-hd720 < Life.and.Birth.S01E04.1080p.HDTV.x264-FTP by sltrader/BiS0N.';
  fExpectedResultStr := 'NEW  > tv-hd720 < Life.and.Birth.S01E04.1080p.HDTV.x264-FTP by sltrader/BiS0N.';

  fOutputStr := StripNoValidChars(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Cleaning failed!');
end;

procedure TTestPrecatcherHelpers.TestStripNoValidChars2;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'NEW in games: Bandit.Point.VR-VREX by poseid0n/iND';
  fExpectedResultStr := 'NEW in games: Bandit.Point.VR-VREX by poseid0n/iND';

  fOutputStr := StripNoValidChars(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Cleaning failed!');
end;

procedure TTestPrecatcherHelpers.TestStripNoValidChars3;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := '+ tv-hd-swe > A new car Trolljagarna.S04E08.SWEDiSH.1080p.WEB.H264-EXECUTION discovered on streets';
  fExpectedResultStr := '+ tv-hd-swe > A new car Trolljagarna.S04E08.SWEDiSH.1080p.WEB.H264-EXECUTION discovered on streets';

  fOutputStr := StripNoValidChars(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Cleaning failed!');
end;

procedure TTestPrecatcherHelpers.TestStripNoValidChars4;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'NEW in hd-mov: -> Klovn.The.Final.2020.DANISH.1080p.BluRay.x264-CONDITION by winner/SLDev';
  fExpectedResultStr := 'NEW in hd-mov: -> Klovn.The.Final.2020.DANISH.1080p.BluRay.x264-CONDITION by winner/SLDev';

  fOutputStr := StripNoValidChars(fInputStr);
  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Cleaning failed!');
end;

procedure TTestPrecatcherHelpers.TestIsLineCommentedOut1;
var
  fInputStr: String;
begin
  fInputStr := '# this is a comment';
  CheckTrue(IsLineCommentedOut(fInputStr), 'It is a comment!');
end;

procedure TTestPrecatcherHelpers.TestIsLineCommentedOut2;
var
  fInputStr: String;
begin
  fInputStr := 'this is a comment';
  CheckFalse(IsLineCommentedOut(fInputStr), 'It is not a comment!');
end;

procedure TTestPrecatcherHelpers.TestIsLineCommentedOut3;
var
  fInputStr: String;
begin
  fInputStr := '//this is a comment';
  CheckTrue(IsLineCommentedOut(fInputStr), 'It is a comment!');
end;

initialization
  {$IFDEF FPC}
    RegisterTest('precatcher helpers', TTestPrecatcherHelpers.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestPrecatcherHelpers);
  {$ENDIF}
end.
