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
  CheckEquals(-1, CheckStandardPercentDir('99% complete'));
  CheckEquals(-1, CheckStandardPercentDir('35% complete'));
  CheckEquals(-1, CheckStandardPercentDir('done'));
  CheckEquals(-1, CheckStandardPercentDir('35 files complete'));
end;

initialization
  {$IFDEF FPC}
    RegisterTest('mystrings', TTestTags.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestTags);
  {$ENDIF}
end.
