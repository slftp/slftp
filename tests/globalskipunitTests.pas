unit globalskipunitTests;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestGlobalskipunit = class(TTestCase)
  published
    procedure TestCheckIfGlobalSkippedGroup1;
    procedure TestCheckIfGlobalSkippedGroup2;
    procedure TestCheckIfGlobalSkippedGroup3;
    procedure TestCheckIfGlobalSkippedGroup4;
    procedure TestCheckIfGlobalSkippedGroup5;
    procedure TestCheckIfGlobalSkippedGroup6;
    procedure TestCheckIfGlobalSkippedGroup7;
    procedure TestCheckIfGlobalSkippedGroup8;
  end;

implementation

uses
  SysUtils, globalskipunit;

{ TTestGlobalskipunit }

procedure TTestGlobalskipunit.TestCheckIfGlobalSkippedGroup1;
var
  fInputStr: String;
  fOutput, fExpectedResult: Boolean;
begin
  fInputStr := 'American.Ninja.Warrior.S11E01.1080p.WEB.h264-TBS';
  fExpectedResult := False;

  fOutput := CheckIfGlobalSkippedGroup(fInputStr);
  CheckEquals(fExpectedResult, fOutput, 'GlobalSkippedGroup mismatch!');
end;

procedure TTestGlobalskipunit.TestCheckIfGlobalSkippedGroup2;
var
  fInputStr: String;
  fOutput, fExpectedResult: Boolean;
begin
  fInputStr := 'Der.Untergang.German.2004.DVDRiP.PROPER.XViD-DrProper';
  fExpectedResult := True;

  fOutput := CheckIfGlobalSkippedGroup(fInputStr);
  CheckEquals(fExpectedResult, fOutput, 'GlobalSkippedGroup mismatch!');
end;

procedure TTestGlobalskipunit.TestCheckIfGlobalSkippedGroup3;
var
  fInputStr: String;
  fOutput, fExpectedResult: Boolean;
begin
  fInputStr := 'Borderlands.REPACK.RF.XBOX360-BoNkErS';
  fExpectedResult := True;

  fOutput := CheckIfGlobalSkippedGroup(fInputStr);
  CheckEquals(fExpectedResult, fOutput, 'GlobalSkippedGroup mismatch!');
end;

procedure TTestGlobalskipunit.TestCheckIfGlobalSkippedGroup4;
var
  fInputStr: String;
  fOutput, fExpectedResult: Boolean;
begin
  fInputStr := 'Gina.Lisa.Lohfink.Sextape.Part.Two.German.XXX.WEB.FLV-GiNALiSA';
  fExpectedResult := True;

  fOutput := CheckIfGlobalSkippedGroup(fInputStr);
  CheckEquals(fExpectedResult, fOutput, 'GlobalSkippedGroup mismatch!');
end;

procedure TTestGlobalskipunit.TestCheckIfGlobalSkippedGroup5;
var
  fInputStr: String;
  fOutput, fExpectedResult: Boolean;
begin
  fInputStr := 'Can.You.Ever.Forgive.Me.2018.DVDScr.XVID.AC3.SHQ.Hive-CM8';
  fExpectedResult := True;

  fOutput := CheckIfGlobalSkippedGroup(fInputStr);
  CheckEquals(fExpectedResult, fOutput, 'GlobalSkippedGroup mismatch!');
end;

procedure TTestGlobalskipunit.TestCheckIfGlobalSkippedGroup6;
var
  fInputStr: String;
  fOutput, fExpectedResult: Boolean;
begin
  fInputStr := 'Scarf_-_Odysee-(ZMG_073)-WEB-2017-MARiBOR';
  fExpectedResult := False;

  fOutput := CheckIfGlobalSkippedGroup(fInputStr);
  CheckEquals(fExpectedResult, fOutput, 'GlobalSkippedGroup mismatch!');
end;

procedure TTestGlobalskipunit.TestCheckIfGlobalSkippedGroup7;
var
  fInputStr: String;
  fOutput, fExpectedResult: Boolean;
begin
  fInputStr := 'Can.You.Ever.Forgive.Me.2018.DVDScr.XVID.AC3.SHQ.Hive-CM8_INT';
  fExpectedResult := True;

  fOutput := CheckIfGlobalSkippedGroup(fInputStr);
  CheckEquals(fExpectedResult, fOutput, 'GlobalSkippedGroup mismatch!');
end;

procedure TTestGlobalskipunit.TestCheckIfGlobalSkippedGroup8;
var
  fInputStr: String;
  fOutput, fExpectedResult: Boolean;
begin
  fInputStr := 'VA_-_EDM_Deejay_Compilation_2019-(PDM706)-WEB-2019-ZzZz_iNT';
  fExpectedResult := False;

  fOutput := CheckIfGlobalSkippedGroup(fInputStr);
  CheckEquals(fExpectedResult, fOutput, 'GlobalSkippedGroup mismatch!');
end;

initialization
  {$IFDEF FPC}
    RegisterTest('globalskipunit', TTestGlobalskipunit.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestGlobalskipunit);
  {$ENDIF}
end.
