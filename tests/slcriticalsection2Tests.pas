unit slcriticalsection2Tests;

interface

uses
  {$IFDEF FPC}
    fpcunit, testregistry;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestSLCriticalSection2 = class(TTestCase)
  protected
    procedure SetUp; override;
    {$IFNDEF FPC}
      procedure Teardown; override;
    {$ENDIF}
  published
    procedure TestNestedLocks;
    procedure TestGetOrCreate;
  end;

implementation

uses
  slcriticalsection2;

{$IFDEF FPC}
var
  // fpcunit has no SetUpOnce, so guard the setup manually: the critical
  // section registry is process-global anyway
  glSlCriticalSection2SetupDone: Boolean = False;
{$ENDIF}

{ TTestSLCriticalSection2 }

procedure TTestSLCriticalSection2.SetUp;
begin
  {$IFDEF FPC}
  if glSlCriticalSection2SetupDone then
    Exit;
  glSlCriticalSection2SetupDone := True;
  {$ENDIF}

  SlCriticalSection2Init(100, True);
end;

{$IFNDEF FPC}
procedure TTestSLCriticalSection2.Teardown;
begin
  SlCriticalSection2Uninit;
end;
{$ENDIF}

// tests that the CurrentLockOwnerName returns the correct value in case of
// the same thread entering the same lock multiple times
procedure TTestSLCriticalSection2.TestNestedLocks;
var
  cs: TSlCriticalSection2;
begin
  cs := TSlCriticalSection2.Create('Test');
  try
    cs.Enter('outer lock');
    CheckEquals('outer lock', cs.CurrentLockOwnerName);
    cs.Enter('inner lock');
    CheckEquals('inner lock', cs.CurrentLockOwnerName);
    cs.Leave;
    CheckEquals('outer lock', cs.CurrentLockOwnerName);
    cs.Leave;
    CheckEquals('', cs.CurrentLockOwnerName);
  finally
    cs.Free;
  end;
end;

procedure TTestSLCriticalSection2.TestGetOrCreate;
var
  cs1: TSlCriticalSection2;
  cs2: TSlCriticalSection2;
begin
  cs1 := TSlCriticalSection2.GetOrCreate('Test2');
  cs2 := TSlCriticalSection2.GetOrCreate('Test2');
  // we must get the same object from both calls. first call creates the object, second call returns the already existing object.
  CheckTrue(cs1 = cs2);
end;

initialization
  {$IFDEF FPC}
    RegisterTest('TTestSLCriticalSection2', TTestSLCriticalSection2.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestSLCriticalSection2);
  {$ENDIF}
end.
