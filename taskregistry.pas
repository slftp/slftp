unit taskregistry;

interface

uses
  Classes, SysUtils, Generics.Collections, tasksunit, slcriticalsection2;

type
  { Global registry that maps task uids to live task instances.
    It is used to resolve cross-task references (e.g. race/wait pairs) safely
    without keeping raw object pointers that can become dangling. }
  TTaskRegistry = class
  private
    fLock: TSlCriticalSection2;
    fTasks: TDictionary<UInt64, TTask>;
  public
    constructor Create;
    destructor Destroy; override;

    { Registers a task under its uid. No-op if the uid is already registered
      for the same instance; raises if another instance is already registered. }
    procedure RegisterTask(const aTask: TTask);

    { Removes a task from the registry. Safe to call multiple times. }
    procedure UnregisterTask(const aTask: TTask); overload;
    procedure UnregisterTask(const aUid: UInt64); overload;

    { Looks up a task by uid. Returns nil if it is not registered. }
    function Lookup(const aUid: UInt64): TTask;

    { Returns true if a task with the given uid is currently registered. }
    function Contains(const aUid: UInt64): Boolean;

    { Clears the registry. Only used during shutdown. }
    procedure Clear;

    { Returns the number of currently registered tasks. }
    function Count: Integer;

    { Returns a string with counts per task class name. }
    function CountsByType: String;
  end;

var
  GlTaskRegistry: TTaskRegistry;

procedure TaskRegistryInit;
procedure TaskRegistryUninit;

implementation

uses
  debugunit;

const
  section = 'taskregistry';

{ TTaskRegistry }

constructor TTaskRegistry.Create;
begin
  inherited Create;
  fLock := TSlCriticalSection2.Create('TaskRegistry');
  fTasks := TDictionary<UInt64, TTask>.Create;
end;

destructor TTaskRegistry.Destroy;
begin
  FreeAndNil(fTasks);
  FreeAndNil(fLock);
  inherited;
end;

procedure TTaskRegistry.RegisterTask(const aTask: TTask);
var
  fExisting: TTask;
begin
  if aTask = nil then
    Exit;

  fLock.Enter('RegisterTask');
  try
    if fTasks.TryGetValue(aTask.uid, fExisting) then
    begin
      if fExisting <> aTask then
        raise Exception.CreateFmt('TaskRegistry: uid %d is already registered for another task instance', [aTask.uid]);
      Exit;
    end;
    fTasks.Add(aTask.uid, aTask);
  finally
    fLock.Leave;
  end;
end;

procedure TTaskRegistry.UnregisterTask(const aTask: TTask);
begin
  if aTask = nil then
    Exit;
  UnregisterTask(aTask.uid);
end;

procedure TTaskRegistry.UnregisterTask(const aUid: UInt64);
begin
  fLock.Enter('UnregisterTask');
  try
    fTasks.Remove(aUid);
  finally
    fLock.Leave;
  end;
end;

function TTaskRegistry.Lookup(const aUid: UInt64): TTask;
begin
  fLock.Enter('Lookup');
  try
    if not fTasks.TryGetValue(aUid, Result) then
      Result := nil;
  finally
    fLock.Leave;
  end;
end;

function TTaskRegistry.Contains(const aUid: UInt64): Boolean;
begin
  fLock.Enter('Contains');
  try
    Result := fTasks.ContainsKey(aUid);
  finally
    fLock.Leave;
  end;
end;

procedure TTaskRegistry.Clear;
begin
  fLock.Enter('Clear');
  try
    fTasks.Clear;
  finally
    fLock.Leave;
  end;
end;

function TTaskRegistry.Count: Integer;
begin
  fLock.Enter('Count');
  try
    Result := fTasks.Count;
  finally
    fLock.Leave;
  end;
end;

function TTaskRegistry.CountsByType: String;
var
  fPair: TPair<UInt64, TTask>;
  fCounts: TDictionary<String, Integer>;
  fClassName: String;
  fSorted: TStringList;
  fKey: String;
begin
  Result := '';
  fCounts := TDictionary<String, Integer>.Create;
  fSorted := TStringList.Create;
  try
    fLock.Enter('CountsByType');
    try
      for fPair in fTasks do
      begin
        if fPair.Value = nil then
          Continue;
        fClassName := fPair.Value.ClassName;
        if fCounts.ContainsKey(fClassName) then
          fCounts[fClassName] := fCounts[fClassName] + 1
        else
          fCounts.Add(fClassName, 1);
      end;
    finally
      fLock.Leave;
    end;

    for fKey in fCounts.Keys do
      fSorted.Add(Format('%s=%d', [fKey, fCounts[fKey]]));
    fSorted.Sort;
    Result := fSorted.Text;
    // collapse to single line
    Result := StringReplace(Result, #13#10, ' ', [rfReplaceAll]);
    Result := StringReplace(Result, #10, ' ', [rfReplaceAll]);
    Result := Trim(Result);
  finally
    fCounts.Free;
    fSorted.Free;
  end;
end;

{ global init/uninit }

procedure TaskRegistryInit;
begin
  GlTaskRegistry := TTaskRegistry.Create;
end;

procedure TaskRegistryUninit;
begin
  Debug(dpSpam, section, 'Uninit1');
  FreeAndNil(GlTaskRegistry);
  Debug(dpSpam, section, 'Uninit2');
end;

end.
