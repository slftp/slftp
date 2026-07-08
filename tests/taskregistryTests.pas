unit taskregistryTests;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestTaskRegistry = class(TTestCase)
  published
    procedure TestLookupNonExistent;
    procedure TestRegisterAndLookup;
    procedure TestUnregisterRemovesTask;
    procedure TestRegisterSameInstanceTwice;
    procedure TestLookupAfterTaskFreed;
  end;

implementation

uses
  SysUtils, tasksunit, taskregistry;

type
  { Concrete task implementation used only for registry tests. }
  TTestTask = class(TTask)
  public
    function Execute(slot: Pointer): Boolean; override;
    function Name: String; override;
  end;

function TTestTask.Execute(slot: Pointer): Boolean;
begin
  Result := True;
end;

function TTestTask.Name: String;
begin
  Result := 'TestTask';
end;

{ TTestTaskRegistry }

procedure TTestTaskRegistry.TestLookupNonExistent;
begin
  CheckNull(GlTaskRegistry.Lookup(High(UInt64)));
  CheckFalse(GlTaskRegistry.Contains(High(UInt64)));
end;

procedure TTestTaskRegistry.TestRegisterAndLookup;
var
  fTask: TTestTask;
  fFound: TTask;
begin
  fTask := TTestTask.Create('', '', 'nonexistent_site');
  try
    CheckTrue(GlTaskRegistry.Contains(fTask.uid));
    fFound := GlTaskRegistry.Lookup(fTask.uid);
    CheckSame(fTask, fFound);
  finally
    fTask.Free;
  end;
end;

procedure TTestTaskRegistry.TestUnregisterRemovesTask;
var
  fTask: TTestTask;
  fUid: UInt64;
begin
  fTask := TTestTask.Create('', '', 'nonexistent_site');
  fUid := fTask.uid;
  fTask.Free;

  CheckNull(GlTaskRegistry.Lookup(fUid));
  CheckFalse(GlTaskRegistry.Contains(fUid));
end;

procedure TTestTaskRegistry.TestRegisterSameInstanceTwice;
var
  fTask: TTestTask;
begin
  fTask := TTestTask.Create('', '', 'nonexistent_site');
  try
    GlTaskRegistry.RegisterTask(fTask);
    CheckTrue(GlTaskRegistry.Contains(fTask.uid));
  finally
    fTask.Free;
  end;
end;

procedure TTestTaskRegistry.TestLookupAfterTaskFreed;
var
  fTask: TTestTask;
  fUid: UInt64;
  fFound: TTask;
begin
  fTask := TTestTask.Create('', '', 'nonexistent_site');
  fUid := fTask.uid;
  fTask.Free;

  fFound := GlTaskRegistry.Lookup(fUid);
  CheckNull(fFound);
end;

initialization
  {$IFDEF FPC}
    RegisterTest('taskregistry', TTestTaskRegistry.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestTaskRegistry);
  {$ENDIF}

end.
