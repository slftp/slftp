unit tagsTests;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestTags = class(TTestCase)
  published
    procedure TestCheckStandardPercentDir;
  end;

implementation

uses
  SysUtils, tags;

{ TTestTags }

procedure TTestTags.TestCheckStandardPercentDir;
begin
  {
    expected to fail because of function expects i > 4 for i := pos('% complete')
  CheckEquals(1, CheckStandardPercentDir('100% complete'));
  CheckEquals(-1, CheckStandardPercentDir('99% complete'));
  CheckEquals(-1, CheckStandardPercentDir('35% complete'));
  }
  CheckEquals(1, CheckStandardPercentDir('[##############] -  100% Complete - [xxx]'));
  CheckEquals(1, CheckStandardPercentDir('[ 23 of 23 files = 100% complete of 335.1MB]'));
  CheckEquals(-1, CheckStandardPercentDir('[##::::::::::::] -  18% Complete - [xxx]'));
  CheckEquals(-1, CheckStandardPercentDir('[8 of 10 files = 80% complete at 366.8MB]'));
  CheckEquals(-1, CheckStandardPercentDir('[::::::::::::::] -   0% Complete - [x]'));
  CheckEquals(0, CheckStandardPercentDir('[ 73%]-[##########::::]-[729mb]'));
  CheckEquals(0, CheckStandardPercentDir('<<SL>> 11M 1F of House from 2017 COMPLETE'));
  CheckEquals(0, CheckStandardPercentDir('-> 8128M 86F - DONE <-'));
  CheckEquals(0, CheckStandardPercentDir('[>>>>>>>>>>>   ] -  83DONE'));
  CheckEquals(0, CheckStandardPercentDir('[Completed! - 1252M 27F]'));
  CheckEquals(0, CheckStandardPercentDir('( 122M 13F - COMPLETE )'));
  CheckEquals(0, CheckStandardPercentDir('[x] - ( 135M 2F - COMPLETE - Dance Hall 1990 ) - [x]'));
  CheckEquals(0, CheckStandardPercentDir('[x] - ( 145M 31F - COMPLETE ) - [x]'));
end;

initialization
  {$IFDEF FPC}
    RegisterTest('tags', TTestTags.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestTags);
  {$ENDIF}
end.
