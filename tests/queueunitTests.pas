unit queueunitTests;

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
  end;

implementation

uses
  Classes, SysUtils, DateUtils, SyncObjs, queueunit, tasksunit, console, configunit;

type

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


initialization
  {$IFDEF FPC}
    RegisterTest('queueunit', TTestQueueUnit.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestQueueUnit);
  {$ENDIF}
end.
