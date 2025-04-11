unit slcriticalsection2Tests;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestSLCriticalSection2 = class(TTestCase)
  protected
    {$IFDEF FPC}
      procedure SetUpOnce; override;
      procedure TeardownOnce; override;
    {$ELSE}
      procedure SetUp; override;
      procedure Teardown; override;
    {$ENDIF}
  published
    procedure TestNestedLocks;
  end;

implementation

uses
  slcriticalsection2;

{ TTestSLCriticalSection2 }

procedure TTestSLCriticalSection2.{$IFDEF FPC}SetUpOnce{$ELSE}SetUp{$ENDIF};
begin
  SlCriticalSection2Init(100);
end;

procedure TTestSLCriticalSection2.{$IFDEF FPC}TeardownOnce{$ELSE}Teardown{$ENDIF};
begin
  SlCriticalSection2Uninit;
end;

// tests that the CurrentLockOwnerName returns the correct value in case of
// the same thread entering the same lock multiple times
procedure TTestSLCriticalSection2.TestNestedLocks;
var
  cs: TSlCriticalSection2;
begin
  cs := TSlCriticalSection2.Create('Test');
  try
    cs.Enter('outer lock');
    CheckEqualsString('outer lock', cs.CurrentLockOwnerName);
    cs.Enter('inner lock');
    CheckEqualsString('inner lock', cs.CurrentLockOwnerName);
    cs.Leave;
    CheckEqualsString('outer lock', cs.CurrentLockOwnerName);
    cs.Leave;
    CheckEqualsString('', cs.CurrentLockOwnerName);
  finally
    cs.Free;
  end;
end;

initialization
  {$IFDEF FPC}
    RegisterTest('TTestSLCriticalSection2', TTestSLCriticalSection2.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestSLCriticalSection2);
  {$ENDIF}
end.
