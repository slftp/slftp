unit slmd5Tests;

interface

uses
  {$IFDEF FPC}
    fpcunit, testregistry;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestSLMD5 = class(TTestCase)
  published
    procedure TestEmptyString;
    procedure TestABC;
  end;

implementation

uses
  slmd5;

procedure TTestSLMD5.TestEmptyString;
begin
  CheckEquals('D41D8CD98F00B204E9800998ECF8427E', slMD5DigestToStr(slMD5String('')));
end;

procedure TTestSLMD5.TestABC;
begin
  CheckEquals('900150983CD24FB0D6963F7D28E17F72', slMD5DigestToStr(slMD5String('abc')));
end;

initialization
  {$IFDEF FPC}
    RegisterTest('slmd5', TTestSLMD5.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestSLMD5);
  {$ENDIF}
end.
