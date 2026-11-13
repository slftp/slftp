unit rulehashTests;

interface

uses
  {$IFDEF FPC}
    fpcunit, testregistry;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestRuleHash = class(TTestCase)
  published
    procedure TestCRC32KnownVector;
    procedure TestIgnoreBlanks;
    procedure TestIgnoreCase;
    procedure TestOptionsCanBeDisabled;
  end;

implementation

uses
  rulehash;

procedure TTestRuleHash.TestCRC32KnownVector;
begin
  CheckEquals(Int64($CBF43926), Int64(HashRuleLine('123456789', False, False)));
end;

procedure TTestRuleHash.TestIgnoreBlanks;
begin
  CheckEquals(Int64(HashRuleLine('123', False, False)),
    Int64(HashRuleLine('1 2'#9'3', False, True)));
end;

procedure TTestRuleHash.TestIgnoreCase;
begin
  CheckEquals(Int64(HashRuleLine('abc', False, False)),
    Int64(HashRuleLine('AbC', True, False)));
end;

procedure TTestRuleHash.TestOptionsCanBeDisabled;
begin
  CheckTrue(HashRuleLine('a b', False, False) <> HashRuleLine('ab', False, False));
  CheckTrue(HashRuleLine('AB', False, False) <> HashRuleLine('ab', False, False));
end;

initialization
  {$IFDEF FPC}
    RegisterTest('rulehash', TTestRuleHash.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestRuleHash);
  {$ENDIF}
end.

