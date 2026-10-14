unit queueunitTests;

{$I ../queuediagnostics.inc}

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  { Checks the production queue event, insertion, deadlines and deferred sorter without FTP. }
  TTestQueueUnit = class(TTestCase)
  private
    FConsoleQueueDisabled: Boolean;
  protected
    {$IFDEF FPC}
      procedure SetUpOnce; override;
      procedure TearDownOnce; override;
    {$ELSE}
      procedure SetUp; override;
      procedure TearDown; override;
    {$ENDIF}
  published
    procedure TestSignalBeforeWaitAndBurstCoalescing;
    procedure TestInsertionSignalsAndExplicitBatch;
    procedure TestEarlierDeadlineInterruptsWait;
    procedure TestDelayedTaskMaturesWithoutPolling;
    procedure TestSortRequestsCoalesce;
    procedure TestWaitTimeoutBounds;
    procedure TestFutureDeadlinesNeverPoll;
    procedure TestDestinationBlockSurvivesUnrelatedWork;
    procedure TestDestinationBlockExpires;
    procedure TestAssignmentExceptionBackoff;
    procedure TestShutdownInterruptsWait;
    procedure TestDestinationSubscriptions;
    {$IFDEF QUEUE_DIAGNOSTICS}
    procedure TestDiagnosticWindowAndReset;
    procedure TestDiagnosticWakeOrigins;
    procedure TestDiagnosticConcurrentSignals;
    procedure TestDiagnosticWaitClassification;
    procedure TestDiagnosticBlockReasonsAndEvents;
    procedure TestDiagnosticPlannedDelayExcluded;
    procedure TestDiagnosticBoundedBlockSamples;
    procedure TestDiagnosticTaskRemovalSnapshot;
    procedure TestDiagnosticDirlistReadiness;
    procedure TestDiagnosticDestinationTimestamp;
    {$ENDIF}
  end;

implementation

uses
  {$IFDEF QUEUE_DIAGNOSTICS}queuediagnostics,{$ENDIF}
  Classes, SysUtils, DateUtils, SyncObjs, queueunit, tasksunit, console, configunit, dirlist;

type
  {$IFDEF QUEUE_DIAGNOSTICS}
  TDiagnosticSignaler = class(TThread)
  private
    FDiagnostics: TQueueDiagnostics;
  public
    constructor Create(aDiagnostics: TQueueDiagnostics);
    procedure Execute; override;
  end;
  {$ENDIF}

  TQueueProbe = class(TQueueThread)
  public
    function WaitForWork(const aMaxWait: Cardinal): TWaitResult;
    function SortNow: Boolean;
    function Promote(const aNow: TDateTime): TDateTime;
    procedure RememberBlock(aDestination: TObject; const aRevision: Int64; const aRetryAt: TDateTime);
    function IsBlocked(aDestination: TObject; const aRevision: Int64; const aNow: TDateTime): Boolean;
    procedure AssignmentFailed(const aTask: TTask);
  end;

  { Synthetic task: no site lookup, credentials, network or global task-ID generator. }
  TProbeTask = class(TTask)
  public
    constructor Create(const aStartAt: TDateTime = 0);
    function Name: String; override;
    function Execute(aSlot: Pointer): Boolean; override;
  end;

  TQueueWorkerProbe = class(TQueueProbe)
  private
    FEntered, FExited: TEvent;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
    property Entered: TEvent read FEntered;
    property Exited: TEvent read FExited;
  end;

  TQueueWaiter = class(TThread)
  private
    FQueue: TQueueProbe;
    FEntered, FDone: TEvent;
    FWaitResult: TWaitResult;
  public
    constructor Create(aQueue: TQueueProbe);
    destructor Destroy; override;
    procedure Execute; override;
    property Entered: TEvent read FEntered;
    property Done: TEvent read FDone;
    property WaitResult: TWaitResult read FWaitResult;
  end;

function TQueueProbe.WaitForWork(const aMaxWait: Cardinal): TWaitResult;
begin
  Result := WaitForWake(aMaxWait);
end;

function TQueueProbe.SortNow: Boolean;
begin
  Result := ApplyPendingSort;
end;

function TQueueProbe.Promote(const aNow: TDateTime): TDateTime;
begin
  main_lock.Enter('Test promote');
  try
    Result := MoveWaitingTasks(aNow);
  finally
    main_lock.Leave;
  end;
end;

procedure TQueueProbe.RememberBlock(aDestination: TObject; const aRevision: Int64;
  const aRetryAt: TDateTime);
begin
  RememberBlockedDestination(aDestination, aRevision, aRetryAt);
end;

function TQueueProbe.IsBlocked(aDestination: TObject; const aRevision: Int64;
  const aNow: TDateTime): Boolean;
begin
  Result := DestinationBlocked(aDestination, aRevision, aNow);
end;

procedure TQueueProbe.AssignmentFailed(const aTask: TTask);
begin
  RecordAssignmentFailure(aTask);
end;

constructor TProbeTask.Create(const aStartAt: TDateTime = 0);
begin
  created := Now;
  startat := aStartAt;
end;

function TProbeTask.Name: String;
begin
  Result := 'synthetic queue work';
end;

function TProbeTask.Execute(aSlot: Pointer): Boolean;
begin
  Result := True;
end;

constructor TQueueWorkerProbe.Create;
begin
  inherited Create('QueueWorkerStopTest');
  FEntered := TEvent.Create(nil, True, False, '');
  FExited := TEvent.Create(nil, True, False, '');
end;

destructor TQueueWorkerProbe.Destroy;
begin
  inherited;
  FEntered.Free;
  FExited.Free;
end;

procedure TQueueWorkerProbe.Execute;
begin
  FEntered.SetEvent;
  while not Terminated do
    WaitForWake;
  FExited.SetEvent;
end;

constructor TQueueWaiter.Create(aQueue: TQueueProbe);
begin
  inherited Create(True);
  FQueue := aQueue;
  FEntered := TEvent.Create(nil, True, False, '');
  FDone := TEvent.Create(nil, True, False, '');
end;

destructor TQueueWaiter.Destroy;
begin
  FQueue.QueueFire;
  WaitFor;
  FEntered.Free;
  FDone.Free;
  inherited;
end;

procedure TQueueWaiter.Execute;
begin
  FEntered.SetEvent;
  FWaitResult := FQueue.WaitForWork(5000);
  FDone.SetEvent;
end;

procedure TTestQueueUnit.{$IFDEF FPC}SetUpOnce{$ELSE}SetUp{$ENDIF};
begin
  FConsoleQueueDisabled := no_console_queue;
  no_console_queue := True;
  QueueInit;
end;

procedure TTestQueueUnit.{$IFDEF FPC}TearDownOnce{$ELSE}TearDown{$ENDIF};
begin
  QueueUninit;
  no_console_queue := FConsoleQueueDisabled;
end;

procedure TTestQueueUnit.TestSignalBeforeWaitAndBurstCoalescing;
var
  fQueue: TQueueProbe;
  i: Integer;
begin
  fQueue := TQueueProbe.Create('QueueSignalTest');
  try
    for i := 1 to 100 do
      fQueue.QueueFire;
    CheckTrue(fQueue.WaitForWork(0) = wrSignaled, 'Signal must survive until the worker waits');
    CheckTrue(fQueue.WaitForWork(10) = wrTimeout, 'A burst must collapse into one retained signal');
    fQueue.QueueFire;
    CheckTrue(fQueue.WaitForWork(0) = wrSignaled, 'A signal during processing must remain pending');
  finally
    fQueue.Free;
  end;
end;

procedure TTestQueueUnit.TestInsertionSignalsAndExplicitBatch;
var
  fQueue: TQueueProbe;
begin
  fQueue := TQueueProbe.Create('QueueInsertionTest');
  try
    fQueue.AddTask(TProbeTask.Create);
    CheckTrue(fQueue.WaitForWork(0) = wrSignaled, 'All due tasks signal by default, including metadata');
    fQueue.AddTask(TProbeTask.Create, False);
    CheckTrue(fQueue.WaitForWork(10) = wrTimeout, 'Batch producer explicitly suppresses insertion signal');
    fQueue.QueueFire;
    CheckTrue(fQueue.WaitForWork(0) = wrSignaled, 'Batch completion wakes the queue');
  finally
    fQueue.Free;
  end;
end;

procedure TTestQueueUnit.TestEarlierDeadlineInterruptsWait;
var
  fQueue: TQueueProbe;
  fWaiter: TQueueWaiter;
  fNow: TDateTime;
begin
  fQueue := TQueueProbe.Create('QueueEarlierTest');
  try
    fNow := Now;
    fQueue.AddTask(TProbeTask.Create(IncSecond(fNow, 30)), False);
    CheckTrue(fQueue.WaitForWork(0) = wrSignaled);
    fWaiter := TQueueWaiter.Create(fQueue);
    try
      fWaiter.Start;
      CheckTrue(fWaiter.Entered.WaitFor(2000) = wrSignaled);
      fQueue.AddTask(TProbeTask.Create(IncSecond(fNow, 10)), False);
      CheckTrue(fWaiter.Done.WaitFor(2000) = wrSignaled, 'Earlier task must interrupt the existing wait');
      CheckTrue(fWaiter.WaitResult = wrSignaled);
    finally
      fWaiter.Free;
    end;
    fQueue.WaitForWork(0); // consume destructor signal
    fQueue.AddTask(TProbeTask.Create(IncSecond(fNow, 60)), False);
    CheckTrue(fQueue.WaitForWork(10) = wrTimeout, 'Later delayed task must not cause another pass');
  finally
    fQueue.Free;
  end;
end;

procedure TTestQueueUnit.TestDelayedTaskMaturesWithoutPolling;
var
  fQueue: TQueueProbe;
  fStartAt: TDateTime;
begin
  fQueue := TQueueProbe.Create('QueueDelayedTest');
  try
    fStartAt := IncMilliSecond(Now, 100);
    fQueue.AddTask(TProbeTask.Create(fStartAt));
    CheckTrue(fQueue.WaitForWork(0) = wrSignaled);
    CheckTrue(fQueue.WaitForWork(2000) = wrTimeout, 'Absolute task deadline must end the wait');
    CheckTrue(Now >= IncMilliSecond(fStartAt, -2), 'Must not wait for the 15-second fallback');
    CheckTrue(fQueue.Promote(IncMilliSecond(fStartAt, 2)) = MaxDateTime, 'Due work must not retain an expired deadline');
    CheckTrue(fQueue.SortNow, 'Promoted tasks request sorting');
    CheckFalse(fQueue.SortNow, 'Unchanged or blocked work must not request another sort');
  finally
    fQueue.Free;
  end;
end;

procedure TTestQueueUnit.TestSortRequestsCoalesce;
var
  fQueue: TQueueProbe;
  i: Integer;
begin
  fQueue := TQueueProbe.Create('QueueSortTest');
  try
    CheckFalse(fQueue.SortNow, 'Empty queue has no pending sort');
    for i := 1 to 100 do
    begin
      fQueue.AddTask(TProbeTask.Create, False);
      fQueue.QueueSort;
    end;
    CheckTrue(fQueue.SortNow);
    CheckFalse(fQueue.SortNow, 'One hundred producer requests require only one sort');
    fQueue.QueueSort;
    CheckTrue(fQueue.SortNow, 'Changed priorities still request another sort');
  finally
    fQueue.Free;
  end;
end;

procedure TTestQueueUnit.TestWaitTimeoutBounds;
var
  fNow: TDateTime;
  fWait: Cardinal;
begin
  fNow := EncodeDate(2026, 10, 11);
  CheckEquals(15000, Integer(QueueWaitMilliseconds(fNow, MaxDateTime, 15000)));
  CheckEquals(15000, Integer(QueueWaitMilliseconds(fNow, IncSecond(fNow, 60), 15000)));
  CheckEquals(0, Integer(QueueWaitMilliseconds(fNow, IncSecond(fNow, -1), 15000)), 'A deadline crossed during processing cannot become an absolute backward delay');
  fWait := QueueWaitMilliseconds(fNow, IncSecond(fNow, 5), 15000);
  CheckTrue((fWait >= 4999) and (fWait <= 5001), 'Cooldown and task deadlines must cap the fallback');
end;

procedure TTestQueueUnit.TestFutureDeadlinesNeverPoll;
var
  fNow, fDeadline: TDateTime;
  i: Integer;
begin
  fNow := EncodeDate(2026, 10, 11);
  for i := 1 to 100 do
  begin
    fDeadline := fNow + (i / 100.0) / MSecsPerDay;
    CheckTrue(QueueWaitMilliseconds(fNow, fDeadline, 15000) >= 1,
      'Every future deadline must wait at least one millisecond');
  end;
  fDeadline := fNow + 1.25 / MSecsPerDay;
  CheckEquals(2, Integer(QueueWaitMilliseconds(fNow, fDeadline, 15000)),
    'Fractional waits must round up rather than wake before the deadline');
  CheckEquals(1, Integer(QueueWaitMilliseconds(fNow, fDeadline, 1)),
    'Rounding must preserve the fallback cap');
  CheckEquals(0, Integer(QueueWaitMilliseconds(fDeadline, fDeadline, 15000)),
    'A due task still runs immediately');
end;

procedure TTestQueueUnit.TestDestinationBlockSurvivesUnrelatedWork;
var
  fQueue: TQueueProbe;
  fTargets: TQueueWakeTargets;
  fDestination: TObject;
  fNow: TDateTime;
  fRevision: Int64;
  i: Integer;
begin
  fQueue := TQueueProbe.Create('QueueBlockedDestinationTest');
  fTargets := TQueueWakeTargets.Create;
  fDestination := TObject.Create;
  try
    fNow := Now;
    fRevision := fTargets.Revision;
    fQueue.RememberBlock(fDestination, fRevision, IncSecond(fNow, 15));
    for i := 1 to 250 do
    begin
      fQueue.AddTask(TProbeTask.Create, False);
      fQueue.QueueFire;
      CheckTrue(fQueue.WaitForWork(0) = wrSignaled);
      CheckTrue(fQueue.IsBlocked(fDestination, fTargets.Revision, fNow),
        'Unrelated task signals must not discard a known destination block');
    end;
    fTargets.RegisterSite('SOURCE');
    CheckEquals(1, Length(fTargets.TakeSites));
    CheckTrue(fTargets.Revision <> fRevision, 'Slot completion advances the capacity revision');
    CheckFalse(fQueue.IsBlocked(fDestination, fTargets.Revision, fNow),
      'Released destination capacity must be reconsidered immediately, without backoff');
  finally
    fQueue.Free;
    fTargets.Free;
    fDestination.Free;
  end;
end;

procedure TTestQueueUnit.TestDestinationBlockExpires;
var
  fQueue: TQueueProbe;
  fDestination: TObject;
  fNow, fRetryAt: TDateTime;
begin
  fQueue := TQueueProbe.Create('QueueDestinationRetryTest');
  fDestination := TObject.Create;
  try
    fNow := Now;
    fRetryAt := IncMilliSecond(fNow, 25);
    fQueue.RememberBlock(fDestination, 0, fRetryAt);
    CheckTrue(fQueue.IsBlocked(fDestination, 0, IncMilliSecond(fNow, 10)),
      'Lock contention waits for the short retry deadline');
    CheckFalse(fQueue.IsBlocked(fDestination, 0, fRetryAt),
      'Retry must expire even without a slot notification');
    fQueue.RememberBlock(fDestination, 0, IncSecond(fNow, 15));
    CheckFalse(fQueue.IsBlocked(fDestination, 0, IncSecond(fNow, 15)),
      'Housekeeping must eventually recheck capacity even if a notification was missed');
  finally
    fQueue.Free;
    fDestination.Free;
  end;
end;

procedure TTestQueueUnit.TestAssignmentExceptionBackoff;
var
  fQueue: TQueueProbe;
  fTask: TProbeTask;
  fMaxAssign, fDelay: Integer;
  fNow: TDateTime;
  i: Integer;
begin
  fQueue := TQueueProbe.Create('QueueAssignmentFailureTest');
  fTask := TProbeTask.Create;
  try
    fMaxAssign := config.ReadInteger('queue', 'maxassign', 200);
    fDelay := config.ReadInteger('queue', 'maxassign_delay', 15);
    if fMaxAssign = 0 then
    begin
      fQueue.AssignmentFailed(fTask);
      CheckEquals(1, fTask.TryToAssign);
      CheckTrue(fTask.startat = 0);
      Exit;
    end;
    for i := 1 to fMaxAssign do
      fQueue.AssignmentFailed(fTask);
    CheckEquals(fMaxAssign, fTask.TryToAssign);
    CheckTrue(fTask.startat = 0, 'Only reaching the limit does not delay the task');
    fNow := Now;
    fQueue.AssignmentFailed(fTask);
    CheckEquals(0, fTask.TryToAssign);
    if fDelay = 0 then
      CheckTrue(fTask.ready, 'Zero configured backoff retains the existing finish action')
    else
    begin
      CheckTrue(fTask.startat >= IncSecond(fNow, fDelay));
      CheckFalse(fTask.ready, 'A real assignment error backs off instead of discarding the task');
    end;
  finally
    fTask.Free;
    fQueue.Free;
  end;
end;

procedure TTestQueueUnit.TestShutdownInterruptsWait;
var
  fQueue: TQueueWorkerProbe;
begin
  fQueue := TQueueWorkerProbe.Create;
  try
    fQueue.Start;
    CheckTrue(fQueue.Entered.WaitFor(2000) = wrSignaled);
    fQueue.QueueStop;
    CheckTrue(fQueue.Exited.WaitFor(0) = wrSignaled, 'Stop must join the actual worker before returning');
    fQueue.QueueStop; // repeated shutdown is safe
  finally
    fQueue.Free;
  end;
end;

procedure TTestQueueUnit.TestDestinationSubscriptions;
var
  fTargets: TQueueWakeTargets;
  fSites: TArray<String>;
  i: Integer;
begin
  fTargets := TQueueWakeTargets.Create;
  try
    for i := 1 to 100 do
      fTargets.RegisterSite('SOURCE-A');
    fTargets.RegisterSite('SOURCE-B');
    fSites := fTargets.TakeSites;
    CheckEquals(2, Length(fSites), 'All waiting sources must wake, across different releases');
    CheckTrue(fSites[0] <> fSites[1], 'Repeated attempts must not multiply subscriptions');
    CheckEquals(0, Length(fTargets.TakeSites), 'Notifications consume subscriptions');
    fTargets.RegisterSite('SOURCE-C');
    fSites := fTargets.TakeSites;
    CheckEquals(1, Length(fSites), 'Registration after a notification must survive for the next completion');
    CheckEqualsString('SOURCE-C', fSites[0]);
  finally
    fTargets.Free;
  end;
end;

{$IFDEF QUEUE_DIAGNOSTICS}
procedure TTestQueueUnit.TestDiagnosticBlockReasonsAndEvents;
var
  fState: TQueueBlockState;
  fText: String;
begin
  fState := Default(TQueueBlockState);
  fState.Observe(qbMkdir, 1000000);
  fState.RecordEvent(900000, 3001000, 'dependency_ready');
  fState.RecordEvent(3000000, 3001000, 'dependency_ready');
  fState.RecordEvent(3000000, 3500000, 'dependency_ready');
  fState.Observe(qbCachedDestination, 3001000, qbDestinationOffline, 15000, 42);
  fState.BeginCheck(6000000);
  CheckFalse(fState.Classified);
  fState.Finish(6000000);
  fText := fState.Detail(6000000, 5000, 0, 'assigned');
  CheckTrue(Pos('reason=destination_cache previous_reason=mkdir_pending', fText) > 0);
  CheckTrue(Pos('cache_reason=destination_no_online_slot', fText) > 0);
  CheckTrue(Pos('destination_revision=42', fText) > 0);
  CheckTrue(Pos('event_kind=dependency_ready event_reason=mkdir_pending event_to_check_ms=1', fText) > 0,
    'A new blocking reason must preserve evidence of the previous readiness transition');
  CheckTrue(Pos('check_to_assign_ms=2999', fText) > 0);
  CheckEquals(Int64(5000000), fState.BlockedUs(9000000), 'Assignment freezes pending time');
  fState.Finish(9000000);
  CheckEquals(Int64(6000000), fState.FinishedAt);
end;

procedure TTestQueueUnit.TestDiagnosticPlannedDelayExcluded;
var
  fState: TQueueBlockState;
begin
  fState := Default(TQueueBlockState);
  fState.Observe(qbDelayed, 1000000);
  CheckEquals(Int64(0), fState.BlockedUs(10000000));
  fState.Observe(qbMkdir, 10000000);
  fState.Observe(qbMkdir, 11000000);
  CheckEquals(Int64(2000000), fState.BlockedUs(12000000), 'Repeated checks do not restart the segment');
  fState.Observe(qbDelayed, 12000000);
  CheckEquals(Int64(2000000), fState.BlockedUs(20000000), 'Backoff is also a planned delay');
  fState.Finish(20000000);
  CheckEquals(Int64(2000000), fState.BlockedUs(30000000));
end;

procedure TTestQueueUnit.TestDiagnosticBoundedBlockSamples;
var
  fDiagnostics: TQueueDiagnostics;
  fState: TQueueBlockState;
  fReport: String;
  i: Integer;
begin
  fDiagnostics := TQueueDiagnostics.Create;
  try
    fState := Default(TQueueBlockState);
    fState.Observe(qbMkdir, 1000000);
    fDiagnostics.SampleBlock(99, 'Probe', 'A', 'B', fState, 2999999, 2000, 0, 1, 1, 'pending');
    for i := 1 to 4 do
    begin
      fState := Default(TQueueBlockState);
      fState.Observe(qbMkdir, i * 1000000);
      fDiagnostics.CountBlocked(qbMkdir);
      fDiagnostics.SampleBlock(i, 'Probe', 'A', 'B', fState, 6000000, 5000, 0, 1, 1, 'pending');
    end;
    fReport := fDiagnostics.Snapshot('A', QueueDiagnosticNow, 4, 0, 1, 4, 5000, True);
    CheckTrue(Pos('block_samples=3 block_samples_omitted=1', fReport) > 0);
    CheckTrue(Pos('blocked_mkdir_pending=4', fReport) > 0, 'Reason summary covers omitted tasks');
    CheckTrue(Pos('task=1 ', fReport) > 0);
    CheckTrue(Pos('task=3 ', fReport) > 0);
    CheckEquals(0, Pos('task=4 ', fReport));
    CheckEquals(0, Pos('task=99 ', fReport), 'Short waits must not produce examples');
    fReport := fDiagnostics.Snapshot('A', QueueDiagnosticNow, 0, 0, 0, 0, 0, True);
    CheckTrue(Pos('block_samples=0 block_samples_omitted=0', fReport) > 0);
    CheckEquals(0, Pos('[QUEUE-BLOCK]', fReport));
  finally
    fDiagnostics.Free;
  end;
end;

procedure TTestQueueUnit.TestDiagnosticTaskRemovalSnapshot;
var
  fTask: TProbeTask;
  fDiagnostics: TQueueDiagnostics;
  fReport: String;
begin
  fDiagnostics := TQueueDiagnostics.Create;
  try
    fTask := TProbeTask.Create;
    try
      fTask.uid := 777;
      fTask.QueueBlockState^.Observe(qbDestinationFull, 1000000);
      fTask.QueueBlockState^.Finish(4000000);
      fDiagnostics.SampleBlock(fTask.uid, fTask.ClassName, 'A', 'B', fTask.QueueBlockState^,
        4000000, 3000, 0, 1, 0, 'removed');
    finally
      fTask.Free;
    end;
    fReport := fDiagnostics.Snapshot('A', QueueDiagnosticNow, 0, 0, 1, 0, 0, True);
    CheckTrue(Pos('[QUEUE-RESUME] task=777 ', fReport) > 0);
    CheckTrue(Pos('outcome=removed', fReport) > 0, 'Reports must survive actual task destruction');
  finally
    fDiagnostics.Free;
  end;
end;

procedure TTestQueueUnit.TestDiagnosticDirlistReadiness;
var
  fDirlist: TDirList;
  fAt: Int64;
begin
  fDirlist := TDirList.Create('QUEUE-DIAG-PROBE', nil, nil, '');
  try
    CheckTrue(fDirlist.need_mkdir);
    CheckEquals(Int64(0), fDirlist.DiagnosticReadyAt);
    fDirlist.need_mkdir := False;
    fAt := fDirlist.DiagnosticReadyAt;
    CheckTrue(fAt > 0);
    fDirlist.need_mkdir := False;
    CheckEquals(fAt, fDirlist.DiagnosticReadyAt, 'Repeated ready writes retain the original transition');
    fDirlist.need_mkdir := True;
    CheckEquals(Int64(0), fDirlist.DiagnosticReadyAt);
    fDirlist.error := True;
    CheckTrue(fDirlist.DiagnosticReadyAt > 0, 'Error also releases the existing dependency predicate');
    fDirlist.error := False;
    CheckEquals(Int64(0), fDirlist.DiagnosticReadyAt);
  finally
    fDirlist.Free;
  end;
end;

procedure TTestQueueUnit.TestDiagnosticDestinationTimestamp;
var
  fTargets: TQueueWakeTargets;
  fBefore, fAfter, fAt: Int64;
begin
  fTargets := TQueueWakeTargets.Create;
  try
    CheckEquals(Int64(0), fTargets.DiagnosticChangedAt);
    fTargets.RegisterSite('A');
    fBefore := QueueDiagnosticNow;
    CheckEquals(1, Length(fTargets.TakeSites));
    fAfter := QueueDiagnosticNow;
    fAt := fTargets.DiagnosticChangedAt;
    CheckTrue((fAt >= fBefore) and (fAt <= fAfter));
    fTargets.RegisterSite('A');
    CheckEquals(fAt, fTargets.DiagnosticChangedAt, 'Registration is not a capacity notification');
  finally
    fTargets.Free;
  end;
end;

constructor TDiagnosticSignaler.Create(aDiagnostics: TQueueDiagnostics);
begin
  inherited Create(True);
  FDiagnostics := aDiagnostics;
end;

procedure TDiagnosticSignaler.Execute;
var
  i: Integer;
begin
  for i := 1 to 5000 do
    FDiagnostics.Signal(qwSlot);
end;

procedure TTestQueueUnit.TestDiagnosticWindowAndReset;
var
  fDiagnostics: TQueueDiagnostics;
  fReport: String;
  fNowUs: Int64;
begin
  fDiagnostics := TQueueDiagnostics.Create;
  try
    fNowUs := QueueDiagnosticNow;
    fDiagnostics.Count(qcSort, 3);
    fDiagnostics.Count(qcCompletedBytes, 5368709120);
    fDiagnostics.Signal(qwDestination);
    fDiagnostics.Measure(qtSort, 20);
    fDiagnostics.Measure(qtSort, 40);
    fDiagnostics.Measure(qtSort, -1);
    CheckEqualsString('', fDiagnostics.Snapshot('PROBE', fNowUs, 3, 0, 1, 2, 100));
    fReport := fDiagnostics.Snapshot('PROBE', fNowUs + 10000000, 3, 0, 1, 2, 100);
    CheckTrue(Pos('sorts=3 ', fReport) > 0, 'Early reads must not reset counters');
    CheckTrue(Pos('completed_declared_bytes=5368709120 ', fReport) > 0, 'Bytes require 64-bit counters');
    CheckTrue(Pos('signal_destination=1 ', fReport) > 0);
    CheckTrue(Pos('sort_n=2 sort_sum_us=60 sort_max_us=40', fReport) > 0);
    fReport := fDiagnostics.Snapshot('PROBE', fNowUs + 10000001, 0, 0, 0, 0, 0, True);
    CheckTrue(Pos('sorts=0 ', fReport) > 0);
    CheckTrue(Pos('signal_destination=0 ', fReport) > 0);
    CheckTrue(Pos('sort_n=0 sort_sum_us=0 sort_max_us=0', fReport) > 0);
  finally
    fDiagnostics.Free;
  end;
end;

procedure TTestQueueUnit.TestDiagnosticWakeOrigins;
var
  fDiagnostics: TQueueDiagnostics;
  fReport: String;
begin
  fDiagnostics := TQueueDiagnostics.Create;
  try
    fDiagnostics.Signal(qwAdminLogin);
    fDiagnostics.Signal(qwMkdirReady);
    fDiagnostics.Signal(qwTaskBatch);
    fReport := fDiagnostics.Snapshot('ORIGIN', QueueDiagnosticNow, 0, 0, 0, 0, 0, True);
    CheckTrue(Pos('signal_external=0 ', fReport) > 0,
      'Classified runtime origins must not be counted as external');
    CheckTrue(Pos('signal_admin_login=1 ', fReport) > 0);
    CheckTrue(Pos('signal_mkdir_ready=1 ', fReport) > 0);
    CheckTrue(Pos('signal_task_batch=1 ', fReport) > 0);
  finally
    fDiagnostics.Free;
  end;
end;

procedure TTestQueueUnit.TestDiagnosticWaitClassification;
var
  fDiagnostics: TQueueDiagnostics;
  fReport: String;
begin
  fDiagnostics := TQueueDiagnostics.Create;
  try
    fDiagnostics.RecordWait(1, 100, wrSignaled, 4000);
    fDiagnostics.RecordWait(0, 0, wrTimeout, 4000);
    fDiagnostics.RecordWait(1, 50, wrTimeout, 4000);
    fDiagnostics.RecordWait(1, 950, wrTimeout, -1);
    fDiagnostics.RecordWait(2, 2500, wrTimeout, 0);
    fDiagnostics.RecordWait(15000, 14995000, wrTimeout, 0);
    fDiagnostics.RecordWait(1, -1, wrTimeout, -1);
    fDiagnostics.RecordWait(5, 10, wrError, -1);
    fReport := fDiagnostics.Snapshot('WAIT', QueueDiagnosticNow, 0, 0, 0, 0, 0, True);
    CheckTrue(Pos('early_timeouts=1 ', fReport) > 0,
      'Signals, zero waits, tolerance and invalid samples must not be counted as early timeouts');
    CheckTrue(Pos('early_timeout_missing_sum_us=950 ', fReport) > 0);
    CheckTrue(Pos('wait_errors=1 ', fReport) > 0);
    CheckTrue(Pos('timeout_requested_n=4 timeout_requested_sum_us=15004000 ', fReport) > 0);
    CheckTrue(Pos('timeout_elapsed_n=4 timeout_elapsed_sum_us=14998500 ', fReport) > 0);
    CheckTrue(Pos('clock_lag_n=5 clock_lag_sum_us=12000 clock_lag_max_us=4000', fReport) > 0);
    fReport := fDiagnostics.Snapshot('WAIT', QueueDiagnosticNow, 0, 0, 0, 0, 0, True);
    CheckTrue(Pos('early_timeouts=0 ', fReport) > 0);
    CheckTrue(Pos('timeout_elapsed_n=0 timeout_elapsed_sum_us=0 ', fReport) > 0);
  finally
    fDiagnostics.Free;
  end;
end;

procedure TTestQueueUnit.TestDiagnosticConcurrentSignals;
var
  fDiagnostics: TQueueDiagnostics;
  fFirst, fSecond: TDiagnosticSignaler;
  fReport: String;
begin
  fDiagnostics := TQueueDiagnostics.Create;
  try
    fFirst := TDiagnosticSignaler.Create(fDiagnostics);
    try
      fSecond := TDiagnosticSignaler.Create(fDiagnostics);
      try
        fFirst.Start;
        fSecond.Start;
        fFirst.WaitFor;
        fSecond.WaitFor;
        fReport := fDiagnostics.Snapshot('PROBE', QueueDiagnosticNow, 0, 0, 0, 0, 0, True);
        CheckTrue(Pos('signal_slot=10000 ', fReport) > 0, 'Concurrent producers must not lose increments');
      finally
        fSecond.Free;
      end;
    finally
      fFirst.Free;
    end;
  finally
    fDiagnostics.Free;
  end;
end;
{$ENDIF}

initialization
  {$IFDEF FPC}
    RegisterTest('queueunit', TTestQueueUnit.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestQueueUnit);
  {$ENDIF}
end.
