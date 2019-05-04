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
  CheckEquals(1, CheckStandardPercentDir('100% complete'));
  CheckEquals(1, CheckStandardPercentDir('[##############] -  100% Complete - [xxx]'));
  CheckEquals(-1, CheckStandardPercentDir('99% complete'));
  CheckEquals(-1, CheckStandardPercentDir('35% complete'));
  CheckEquals(-1, CheckStandardPercentDir('[##::::::::::::] -  18% Complete - [xxx]'));
  CheckEquals(0, CheckStandardPercentDir('[ 73%]-[##########::::]-[729mb]'));
  CheckEquals(0, CheckStandardPercentDir('<<SL>> 11M 1F of House from 2017 COMPLETE'));
  CheckEquals(0, CheckStandardPercentDir('-> 8128M 86F - DONE <-'));
  CheckEquals(0, CheckStandardPercentDir('done'));
  CheckEquals(0, CheckStandardPercentDir('35 files complete'));
end;

initialization
  {$IFDEF FPC}
    RegisterTest('tags', TTestTags.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestTags);
  {$ENDIF}
end.
