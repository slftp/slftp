unit pazotaskgraph;

interface

uses
  Classes, SysUtils, Generics.Collections, slcriticalsection2;

type
  { Callback used to wake the site queue(s) of tasks that became ready.
    The receiver should look up the tasks by uid and fire their site queue(s).
    Called outside of the graph lock. }
  TTaskGraphWakeProc = procedure(const aTaskUids: TList<UInt64>) of object;

  { State of a node in the pazo task dependency graph. }
  TTaskGraphState = (
    tgsPending,   //< task is registered but not all dependencies are done yet
    tgsRunning,   //< task has been assigned/is running
    tgsDone,      //< task finished successfully
    tgsError      //< task finished with an error
  );

  { Dependency node for one task inside a pazo. }
  TTaskGraphNode = class
  public
    uid: UInt64;
    state: TTaskGraphState;
    dependencies: TList<UInt64>; //< uids this task depends on
    dependents: TList<UInt64>;   //< uids that depend on this task
    constructor Create(const aUid: UInt64);
    destructor Destroy; override;
  end;

  { Per-pazo dependency graph for pazo tasks (dirlist, mkdir, race, wait).
    Centralises dependency management and can wake dependent tasks automatically. }
  TPazoTaskGraph = class
  private
    fLock: TSlCriticalSection2;
    fNodes: TDictionary<UInt64, TTaskGraphNode>;
    fNodeCount: Integer;
    { Pending dependencies: key is a dependsOnUid that did not exist yet,
      value is the list of uids that depend on it. Resolved when AddTask
      registers the missing uid. }
    fPendingDependencies: TDictionary<UInt64, TList<UInt64>>;
    { UIDs of nodes that finished and were removed from the graph. Kept for a
      short time so that late AddDependency calls on an already-finished task
      can be resolved immediately instead of creating a dangling pending dependency. }
    fFinishedUids: TDictionary<UInt64, TDateTime>;
    fOnWakeTask: TTaskGraphWakeProc;
  public
    { Statistics about the current graph content. }
    type
      TTaskGraphStats = record
        Total, Done, ErrorState, Running, Pending: Integer;
        LeafFinished: Integer;
        AvgDependencies, AvgDependents: Double;
        PendingDependencyKeys: Integer;
        PendingDependencyKeyList: String;
      end;
  private
    procedure InternalMarkState(const aUid: UInt64; const aState: TTaskGraphState; out aReadyDependents: TList<UInt64>);
    procedure DoWakeReadyDependents(var aReadyDependents: TList<UInt64>);
    procedure ResolvePendingDependencies(const aUid: UInt64);
    { Returns true if the node exists, is finished (done/error) and has no dependents.
      Caller must hold the graph lock. }
    function CanRemoveNode(const aUid: UInt64): Boolean;
    { Removes a node from the graph. Caller must hold the graph lock. }
    procedure InternalRemoveTask(const aUid: UInt64);
    { Cleans up entries in fFinishedUids older than the retention window.
      Caller must hold the graph lock. }
    procedure CleanupFinishedUids;
  public
    constructor Create(const aPazoId: Integer);
    destructor Destroy; override;

    { Registers a task in the graph. No-op if already registered. }
    procedure AddTask(const aUid: UInt64);

    { Registers that aUid depends on dependsOnUid. Passing 0 for dependsOnUid
      is a no-op. If dependsOnUid has not been registered yet, the dependency
      is kept pending and applied as soon as it is added. }
    procedure AddDependency(const aUid, dependsOnUid: UInt64);

    { Registers that aUid depends on dependsOnUid, but only if dependsOnUid
      is currently present in the graph. This avoids creating dangling pending
      dependencies when the depended-on task may finish and be removed from the
      graph before the dependency can be registered. }
    procedure AddDependencyIfExists(const aUid, dependsOnUid: UInt64);

    { Marks a task as done or error and returns the uids of all tasks that
      became ready because of this change. If OnWakeTask is assigned, the
      corresponding site queues are fired automatically. }
    procedure MarkDone(const aUid: UInt64);
    procedure MarkError(const aUid: UInt64);

    { Callback used to wake site queues for tasks that became ready.
      Must be assigned by the owner (TPazo). }
    property OnWakeTask: TTaskGraphWakeProc read fOnWakeTask write fOnWakeTask;

    { Marks a task as running. }
    procedure MarkRunning(const aUid: UInt64);

    { Returns true if the task is registered and all its dependencies are done. }
    function IsReady(const aUid: UInt64): Boolean;

    { Returns true if the task is registered. }
    function Contains(const aUid: UInt64): Boolean;

    { Removes a task from the graph. Dependents are not affected; callers
      should use MarkError before removal if dependents should be unblocked. }
    procedure RemoveTask(const aUid: UInt64);

    { Clears the graph. }
    procedure Clear;

    { Returns the number of nodes currently in the graph. }
    function NodeCount: Integer;

    { Returns detailed statistics about the nodes in the graph. }
    function GetStats: TTaskGraphStats;
  end;

var
  { Global counter of all task graph nodes across all pazos. Used for leak diagnostics. }
  GlPazoTaskGraphNodeCount: Int64 = 0;
  { Global list of all living TPazoTaskGraph instances. Used for diagnostics. }
  GlPazoTaskGraphInstances: TThreadList<TPazoTaskGraph>;

{ Returns aggregated statistics across all living pazo task graphs. }
function GetGlobalPazoTaskGraphStats: TPazoTaskGraph.TTaskGraphStats;
{ Returns the number of living pazo task graph instances. }
function GetGlobalPazoTaskGraphInstanceCount: Integer;

implementation

uses
  debugunit, taskregistry, tasksunit, DateUtils;

const
  section = 'pazotaskgraph';

{ TTaskGraphNode }

constructor TTaskGraphNode.Create(const aUid: UInt64);
begin
  inherited Create;
  uid := aUid;
  state := tgsPending;
  dependencies := TList<UInt64>.Create;
  dependents := TList<UInt64>.Create;
end;

destructor TTaskGraphNode.Destroy;
begin
  dependencies.Free;
  dependents.Free;
  inherited;
end;

{ TPazoTaskGraph }

function GetGlobalPazoTaskGraphStats: TPazoTaskGraph.TTaskGraphStats;
var
  fList: TList<TPazoTaskGraph>;
  fGraph: TPazoTaskGraph;
  fLocal: TPazoTaskGraph.TTaskGraphStats;
begin
  FillChar(Result, SizeOf(Result), 0);
  if GlPazoTaskGraphInstances = nil then
    Exit;
  fList := GlPazoTaskGraphInstances.LockList;
  try
    for fGraph in fList do
    begin
      fLocal := fGraph.GetStats;
      Inc(Result.Total, fLocal.Total);
      Inc(Result.Done, fLocal.Done);
      Inc(Result.ErrorState, fLocal.ErrorState);
      Inc(Result.Running, fLocal.Running);
      Inc(Result.Pending, fLocal.Pending);
      Inc(Result.LeafFinished, fLocal.LeafFinished);
      Result.AvgDependencies := Result.AvgDependencies + fLocal.AvgDependencies * fLocal.Total;
      Result.AvgDependents := Result.AvgDependents + fLocal.AvgDependents * fLocal.Total;
      Inc(Result.PendingDependencyKeys, fLocal.PendingDependencyKeys);
      if (Result.PendingDependencyKeyList = '') and (fLocal.PendingDependencyKeyList <> '') then
        Result.PendingDependencyKeyList := fLocal.PendingDependencyKeyList;
    end;
    if Result.Total > 0 then
    begin
      Result.AvgDependencies := Result.AvgDependencies / Result.Total;
      Result.AvgDependents := Result.AvgDependents / Result.Total;
    end;
  finally
    GlPazoTaskGraphInstances.UnlockList;
  end;
end;

function GetGlobalPazoTaskGraphInstanceCount: Integer;
var
  fList: TList<TPazoTaskGraph>;
begin
  if GlPazoTaskGraphInstances = nil then
  begin
    Result := 0;
    Exit;
  end;
  fList := GlPazoTaskGraphInstances.LockList;
  try
    Result := fList.Count;
  finally
    GlPazoTaskGraphInstances.UnlockList;
  end;
end;

constructor TPazoTaskGraph.Create(const aPazoId: Integer);
begin
  inherited Create;
  fLock := TSlCriticalSection2.Create('PazoTaskGraph_' + IntToStr(aPazoId));
  fNodes := TDictionary<UInt64, TTaskGraphNode>.Create;
  fPendingDependencies := TDictionary<UInt64, TList<UInt64>>.Create;
  fFinishedUids := TDictionary<UInt64, TDateTime>.Create;
  if GlPazoTaskGraphInstances <> nil then
    GlPazoTaskGraphInstances.Add(Self);
end;

destructor TPazoTaskGraph.Destroy;
begin
  if GlPazoTaskGraphInstances <> nil then
    GlPazoTaskGraphInstances.Remove(Self);
  if fNodes <> nil then
  begin
    Clear;
    FreeAndNil(fNodes);
  end;
  FreeAndNil(fPendingDependencies);
  FreeAndNil(fFinishedUids);
  FreeAndNil(fLock);
  inherited;
end;

procedure TPazoTaskGraph.InternalMarkState(const aUid: UInt64; const aState: TTaskGraphState; out aReadyDependents: TList<UInt64>);
var
  fNode, fDependentNode, fDependencyNode: TTaskGraphNode;
  fDependentUid: UInt64;
  fAllDone: Boolean;
  fDepUid: UInt64;
begin
  aReadyDependents := nil;
  if not fNodes.TryGetValue(aUid, fNode) then
    Exit;

  if fNode.state = aState then
    Exit;

  fNode.state := aState;

  // check all dependents: if all their dependencies are now done/error, they are ready
  for fDependentUid in fNode.dependents do
  begin
    if not fNodes.TryGetValue(fDependentUid, fDependentNode) then
      Continue;

    fAllDone := True;
    for fDepUid in fDependentNode.dependencies do
    begin
      if not fNodes.TryGetValue(fDepUid, fDependencyNode) then
      begin
        fAllDone := False;
        Break;
      end;
      if not (fDependencyNode.state in [tgsDone, tgsError]) then
      begin
        fAllDone := False;
        Break;
      end;
    end;

    if fAllDone then
    begin
      if aReadyDependents = nil then
        aReadyDependents := TList<UInt64>.Create;
      aReadyDependents.Add(fDependentUid);
    end;
  end;
end;

procedure TPazoTaskGraph.ResolvePendingDependencies(const aUid: UInt64);
var
  fPendingList: TList<UInt64>;
  fDependentUid: UInt64;
  fNode, fDepNode: TTaskGraphNode;
begin
  if not fPendingDependencies.TryGetValue(aUid, fPendingList) then
    exit;

  if fPendingList.Count = 0 then
  begin
    fPendingDependencies.Remove(aUid);
    FreeAndNil(fPendingList);
    exit;
  end;

  if not fNodes.TryGetValue(aUid, fDepNode) then
    exit;

  for fDependentUid in fPendingList do
  begin
    if not fNodes.TryGetValue(fDependentUid, fNode) then
      Continue;

    if fNode.dependencies.IndexOf(aUid) = -1 then
      fNode.dependencies.Add(aUid);
    if fDepNode.dependents.IndexOf(fDependentUid) = -1 then
      fDepNode.dependents.Add(fDependentUid);
  end;

  fPendingDependencies.Remove(aUid);
  FreeAndNil(fPendingList);
end;

procedure TPazoTaskGraph.AddTask(const aUid: UInt64);
var
  fNode: TTaskGraphNode;
begin
  if aUid = 0 then
    Exit;

  fLock.Enter('AddTask');
  try
    if not fNodes.ContainsKey(aUid) then
    begin
      fNode := TTaskGraphNode.Create(aUid);
      fNodes.Add(aUid, fNode);
      Inc(fNodeCount);
      InterlockedIncrement64(GlPazoTaskGraphNodeCount);
    end;
    // apply any dependencies that were registered before this task existed
    ResolvePendingDependencies(aUid);
  finally
    fLock.Leave;
  end;
end;

procedure TPazoTaskGraph.AddDependencyIfExists(const aUid, dependsOnUid: UInt64);
begin
  if (aUid = 0) or (dependsOnUid = 0) or (aUid = dependsOnUid) then
    Exit;

  fLock.Enter('AddDependencyIfExists');
  try
    if not fNodes.ContainsKey(dependsOnUid) then
      Exit;
    AddDependency(aUid, dependsOnUid);
  finally
    fLock.Leave;
  end;
end;

procedure TPazoTaskGraph.AddDependency(const aUid, dependsOnUid: UInt64);
var
  fNode, fDepNode: TTaskGraphNode;
  fPendingList: TList<UInt64>;
  fTask: TTask;
begin
  if (aUid = 0) or (dependsOnUid = 0) or (aUid = dependsOnUid) then
    Exit;

  fLock.Enter('AddDependency');
  try
    if not fNodes.TryGetValue(aUid, fNode) then
    begin
      Debug(dpError, section, Format('[DIAG] AddDependency: uid %d not found when adding dependency on %d', [aUid, dependsOnUid]));
      Exit;
    end;

    if fNodes.TryGetValue(dependsOnUid, fDepNode) then
    begin
      // normal path: both nodes exist
      if fNode.dependencies.IndexOf(dependsOnUid) = -1 then
        fNode.dependencies.Add(dependsOnUid);
      if fDepNode.dependents.IndexOf(aUid) = -1 then
        fDepNode.dependents.Add(aUid);
      Exit;
    end;

    // dependsOnUid is not in the graph. If it recently finished and was removed,
    // the dependency is already satisfied; do NOT re-insert a done ghost node,
    // because that node would never be removed again and would leak memory.
    if fFinishedUids.ContainsKey(dependsOnUid) then
    begin
      Exit;
    end;

    // dependsOnUid is not in the graph. Check whether it is a task that already
    // finished (and was removed from the graph) or one that still exists but has
    // not been registered in the graph yet.
    fTask := nil;
    if (GlTaskRegistry <> nil) and GlTaskRegistry.Contains(dependsOnUid) then
      fTask := GlTaskRegistry.Lookup(dependsOnUid);

    if fTask <> nil then
    begin
      if fTask.ready or fTask.readyerror then
      begin
        // Task already finished outside the graph. The dependency is satisfied;
        // do NOT re-insert a done ghost node that would never be removed again.
        Exit;
      end
      else
      begin
        // Task is still alive but not in the graph yet: register it now so the
        // dependency resolves normally when the task finishes.
        fNodes.Add(dependsOnUid, TTaskGraphNode.Create(dependsOnUid));
        Inc(fNodeCount);
        InterlockedIncrement64(GlPazoTaskGraphNodeCount);
        ResolvePendingDependencies(dependsOnUid);
        if fNodes.TryGetValue(dependsOnUid, fDepNode) then
        begin
          if fNode.dependencies.IndexOf(dependsOnUid) = -1 then
            fNode.dependencies.Add(dependsOnUid);
          if fDepNode.dependents.IndexOf(aUid) = -1 then
            fDepNode.dependents.Add(aUid);
        end;
      end;
      Exit;
    end;

    // Unknown uid: keep pending as before. This should normally not happen;
    // it indicates a dependency was declared on a task that was never created.
    if not fPendingDependencies.TryGetValue(dependsOnUid, fPendingList) then
    begin
      fPendingList := TList<UInt64>.Create;
      fPendingDependencies.Add(dependsOnUid, fPendingList);
    end;
    if fPendingList.IndexOf(aUid) = -1 then
      fPendingList.Add(aUid);
  finally
    fLock.Leave;
  end;
end;

procedure TPazoTaskGraph.DoWakeReadyDependents(var aReadyDependents: TList<UInt64>);
begin
  if (aReadyDependents = nil) or (aReadyDependents.Count = 0) then
  begin
    FreeAndNil(aReadyDependents);
    exit;
  end;
  if not Assigned(fOnWakeTask) then
  begin
    FreeAndNil(aReadyDependents);
    exit;
  end;

  try
    fOnWakeTask(aReadyDependents);
  except
    on e: Exception do
      Debug(dpError, section, Format('[EXCEPTION] DoWakeReadyDependents: %s', [e.Message]));
  end;
  FreeAndNil(aReadyDependents);
end;

procedure TPazoTaskGraph.MarkDone(const aUid: UInt64);
var
  fReadyDependents: TList<UInt64>;
begin
  fReadyDependents := nil;
  fLock.Enter('MarkDone');
  try
    InternalMarkState(aUid, tgsDone, fReadyDependents);
    // Remove the finished node immediately. Because IsReadyToBeExecuted no
    // longer blocks on the graph, keeping finished parent nodes around is not
    // needed and would only leak memory. Dependents that are added later see
    // the uid in fFinishedUids and treat the dependency as satisfied.
    InternalRemoveTask(aUid);
  finally
    fLock.Leave;
  end;
  // wake site queues outside of the graph lock; DoWakeReadyDependents owns the list
  DoWakeReadyDependents(fReadyDependents);
end;

procedure TPazoTaskGraph.MarkError(const aUid: UInt64);
var
  fReadyDependents: TList<UInt64>;
begin
  fReadyDependents := nil;
  fLock.Enter('MarkError');
  try
    InternalMarkState(aUid, tgsError, fReadyDependents);
    // See MarkDone: finished nodes are removed immediately.
    InternalRemoveTask(aUid);
  finally
    fLock.Leave;
  end;
  // wake site queues outside of the graph lock; DoWakeReadyDependents owns the list
  DoWakeReadyDependents(fReadyDependents);
end;

procedure TPazoTaskGraph.MarkRunning(const aUid: UInt64);
var
  fNode: TTaskGraphNode;
begin
  fLock.Enter('MarkRunning');
  try
    if fNodes.TryGetValue(aUid, fNode) then
      if fNode.state = tgsPending then
        fNode.state := tgsRunning;
  finally
    fLock.Leave;
  end;
end;

function TPazoTaskGraph.IsReady(const aUid: UInt64): Boolean;
var
  fNode: TTaskGraphNode;
  fDepUid: UInt64;
  fDepNode: TTaskGraphNode;
begin
  Result := False;
  if aUid = 0 then
    Exit;

  fLock.Enter('IsReady');
  try
    if not fNodes.TryGetValue(aUid, fNode) then
      Exit;

    for fDepUid in fNode.dependencies do
    begin
      if not fNodes.TryGetValue(fDepUid, fDepNode) then
        Exit;
      if not (fDepNode.state in [tgsDone, tgsError]) then
        Exit;
    end;

    Result := True;
  finally
    fLock.Leave;
  end;
end;

function TPazoTaskGraph.Contains(const aUid: UInt64): Boolean;
begin
  fLock.Enter('Contains');
  try
    Result := fNodes.ContainsKey(aUid);
  finally
    fLock.Leave;
  end;
end;

function TPazoTaskGraph.NodeCount: Integer;
begin
  fLock.Enter('NodeCount');
  try
    Result := fNodeCount;
  finally
    fLock.Leave;
  end;
end;

function TPazoTaskGraph.GetStats: TTaskGraphStats;
var
  fNode: TTaskGraphNode;
  fDepCount, fDependentCount: Int64;
  fPendingPair: TPair<UInt64, TList<UInt64>>;
  fFirstKeys: Integer;
begin
  fLock.Enter('GetStats');
  try
    CleanupFinishedUids;
    FillChar(Result, SizeOf(Result), 0);
    fDepCount := 0;
    fDependentCount := 0;
    for fNode in fNodes.Values do
    begin
      Inc(Result.Total);
      case fNode.state of
        tgsDone: Inc(Result.Done);
        tgsError: Inc(Result.ErrorState);
        tgsRunning: Inc(Result.Running);
        tgsPending: Inc(Result.Pending);
      end;
      if (fNode.state in [tgsDone, tgsError]) and (fNode.dependents.Count = 0) then
        Inc(Result.LeafFinished);
      Inc(fDepCount, fNode.dependencies.Count);
      Inc(fDependentCount, fNode.dependents.Count);
    end;
    if Result.Total > 0 then
    begin
      Result.AvgDependencies := fDepCount / Result.Total;
      Result.AvgDependents := fDependentCount / Result.Total;
    end
    else
    begin
      Result.AvgDependencies := 0;
      Result.AvgDependents := 0;
    end;
    Result.PendingDependencyKeys := fPendingDependencies.Count;
    fFirstKeys := 0;
    Result.PendingDependencyKeyList := '';
    for fPendingPair in fPendingDependencies do
    begin
      if fFirstKeys > 0 then
        Result.PendingDependencyKeyList := Result.PendingDependencyKeyList + ',';
      Result.PendingDependencyKeyList := Result.PendingDependencyKeyList + IntToStr(fPendingPair.Key);
      Inc(fFirstKeys);
      if fFirstKeys >= 5 then
        Break;
    end;
  finally
    fLock.Leave;
  end;
end;

function TPazoTaskGraph.CanRemoveNode(const aUid: UInt64): Boolean;
var
  fNode: TTaskGraphNode;
begin
  Result := False;
  if aUid = 0 then
    Exit;
  if not fNodes.TryGetValue(aUid, fNode) then
    Exit;
  if not (fNode.state in [tgsDone, tgsError]) then
    Exit;
  Result := fNode.dependents.Count = 0;
end;

procedure TPazoTaskGraph.InternalRemoveTask(const aUid: UInt64);
var
  fNode: TTaskGraphNode;
  fOther: TTaskGraphNode;
  fDepUid, fDependentUid: UInt64;
  fPendingPair: TPair<UInt64, TList<UInt64>>;
  fPendingList: TList<UInt64>;
begin
  if not fNodes.TryGetValue(aUid, fNode) then
    Exit;

  // remove this uid from its dependencies' dependents lists
  for fDepUid in fNode.dependencies do
  begin
    if fNodes.TryGetValue(fDepUid, fOther) then
      fOther.dependents.Remove(aUid);
  end;

  // remove this uid from its dependents' dependencies lists
  for fDependentUid in fNode.dependents do
  begin
    if fNodes.TryGetValue(fDependentUid, fOther) then
      fOther.dependencies.Remove(aUid);
  end;

  // remove this uid from pending dependency lists
  for fPendingPair in fPendingDependencies do
  begin
    fPendingPair.Value.Remove(aUid);
  end;

  // remove any pending list keyed by this uid
  if fPendingDependencies.TryGetValue(aUid, fPendingList) then
  begin
    fPendingDependencies.Remove(aUid);
    fPendingList.Free;
  end;

  fNodes.Remove(aUid);
  // Remember finished uids for a short while so late AddDependency calls can
  // be resolved immediately instead of leaving dangling pending dependencies.
  if fNode.state in [tgsDone, tgsError] then
    fFinishedUids.AddOrSetValue(aUid, Now);
  fNode.Free;
  Dec(fNodeCount);
  InterlockedDecrement64(GlPazoTaskGraphNodeCount);
end;

procedure TPazoTaskGraph.CleanupFinishedUids;
const
  cRetentionSeconds = 300; // 5 minutes should cover late dependency additions
var
  fCutoff: TDateTime;
  fPair: TPair<UInt64, TDateTime>;
  fToRemove: TList<UInt64>;
  fUid: UInt64;
begin
  fCutoff := IncSecond(Now, -cRetentionSeconds);
  fToRemove := TList<UInt64>.Create;
  try
    for fPair in fFinishedUids do
      if fPair.Value < fCutoff then
        fToRemove.Add(fPair.Key);
    for fUid in fToRemove do
      fFinishedUids.Remove(fUid);
  finally
    fToRemove.Free;
  end;
end;

procedure TPazoTaskGraph.RemoveTask(const aUid: UInt64);
begin
  fLock.Enter('RemoveTask');
  try
    InternalRemoveTask(aUid);
  finally
    fLock.Leave;
  end;
end;

procedure TPazoTaskGraph.Clear;
var
  fNode: TTaskGraphNode;
  fPendingList: TList<UInt64>;
begin
  fLock.Enter('Clear');
  try
    for fNode in fNodes.Values do
      fNode.Free;
    fNodes.Clear;

    for fPendingList in fPendingDependencies.Values do
      fPendingList.Free;
    fPendingDependencies.Clear;
  finally
    fLock.Leave;
  end;
end;

initialization
  GlPazoTaskGraphInstances := TThreadList<TPazoTaskGraph>.Create;

finalization
  FreeAndNil(GlPazoTaskGraphInstances);

end.
