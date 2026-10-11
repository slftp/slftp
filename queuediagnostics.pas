unit queuediagnostics;

interface

type
  { Signal requests, not individual wakes: auto-reset events coalesce requests. }
  TQueueWakeReason = (qwExternal, qwTask, qwEarlierDeadline, qwSlot, qwDestination, qwCleanup);
  { Per-window counters. Blocked counts describe checks, not unique tasks. }
  TQueueDiagnosticCounter = (qcPass, qcEvent, qcDeadline, qcFallback, qcCappedWait,
    qcFallbackWithWork, qcInserted, qcInsertedAssigned, qcInsertedWait,
    qcPromoted, qcSortRequest, qcSort, qcAttempt, qcAssignedRace,
    qcCompletedRace, qcCompletedBytes, qcRetryDelay, qcSourceFull,
    qcDestinationFull, qcDestinationLock, qcBusyCache, qcSourceLimit,
    qcDestinationLimit, qcActiveFile, qcCooldown, qcDependency, qcDependencyChanged, qcDelayedRace, qcInitialDelayMs);
  { Durations in microseconds, with count/sum/max. Refill starts at slot release. }
  TQueueDiagnosticTiming = (qtPass, qtQueueLock, qtSort, qtCleanup, qtConsole,
    qtSourceRefill, qtDestinationRefill);
  { Queue-owned counters require the existing queue lock. Signal is atomic and
    may be called from any producer. No logging or file I/O happens here. }
  TQueueDiagnostics = class
  private
    FCounters: array[TQueueDiagnosticCounter] of Int64;
    FSignals: array[TQueueWakeReason] of Integer;
    FTimes: array[TQueueDiagnosticTiming] of record
      Count, SumUs, MaxUs: Int64;
    end;
    FWindowStartedUs: Int64;
  public
    { Starts a measurement window using a monotonic clock. }
    constructor Create;
    { Counts one signal request without acquiring a scheduler lock. }
    procedure Signal(const aReason: TQueueWakeReason);
    { Adds to a queue-owned counter while holding the queue lock. }
    procedure Count(const aCounter: TQueueDiagnosticCounter; const aValue: Int64 = 1);
    { Accumulates a nonnegative duration while holding the queue lock. }
    procedure Measure(const aTiming: TQueueDiagnosticTiming; const aElapsedUs: Int64);
    { Returns true at most once per ten seconds, unless explicitly forced. }
    function ReportDue(const aNowUs: Int64): Boolean;
    { Takes and resets the window under the queue lock. Returns an empty string
      before ten seconds unless forced. Emit the result AFTER releasing locks.
      Due work includes tasks blocked on dependencies or destination capacity. }
    function Snapshot(const aSite: String; const aNowUs: Int64;
      const aTasks, aWaiting, aFreeSlots, aDue: Integer; const aOldestDueMs: Int64;
      const aForce: Boolean = False): String;
  end;

{ Returns a system-wide monotonic microsecond timestamp using the bundled runtime. }
function QueueDiagnosticNow: Int64;

implementation

uses
  SysUtils, mormot.core.os;

const
  glCounterNames: array[TQueueDiagnosticCounter] of String = (
    'passes', 'wake_event', 'wake_deadline', 'wake_fallback', 'wake_capped',
    'fallback_with_free_slot_and_due_work', 'inserted', 'inserted_assigned',
    'inserted_wait', 'promoted', 'sort_requests', 'sorts', 'assign_attempts',
    'assigned_races', 'completed_races', 'completed_declared_bytes',
    'retry_delay', 'source_full', 'destination_full', 'destination_lock',
    'busy_cache', 'source_limit', 'destination_limit', 'active_file',
    'cooldown', 'dependency', 'dependency_changed_during_registration',
    'delayed_race_insertions', 'initial_race_delay_sum_ms');
  glWakeNames: array[TQueueWakeReason] of String = (
    'external', 'task', 'earlier_deadline', 'slot', 'destination', 'cleanup');
  glTimingNames: array[TQueueDiagnosticTiming] of String = (
    'pass', 'queue_lock_wait', 'sort', 'cleanup', 'console_add',
    'source_refill', 'destination_refill');

function QueueDiagnosticNow: Int64;
begin
  QueryPerformanceMicroSeconds(Result);
end;

constructor TQueueDiagnostics.Create;
begin
  inherited;
  FWindowStartedUs := QueueDiagnosticNow;
end;

procedure TQueueDiagnostics.Signal(const aReason: TQueueWakeReason);
begin
  {$IFDEF FPC}InterlockedIncrement{$ELSE}AtomicIncrement{$ENDIF}(FSignals[aReason]);
end;

procedure TQueueDiagnostics.Count(const aCounter: TQueueDiagnosticCounter; const aValue: Int64 = 1);
begin
  Inc(FCounters[aCounter], aValue);
end;

procedure TQueueDiagnostics.Measure(const aTiming: TQueueDiagnosticTiming; const aElapsedUs: Int64);
begin
  if aElapsedUs < 0 then
    Exit;
  Inc(FTimes[aTiming].Count);
  Inc(FTimes[aTiming].SumUs, aElapsedUs);
  if aElapsedUs > FTimes[aTiming].MaxUs then
    FTimes[aTiming].MaxUs := aElapsedUs;
end;

function TQueueDiagnostics.ReportDue(const aNowUs: Int64): Boolean;
begin
  Result := aNowUs - FWindowStartedUs >= 10000000;
end;

function TQueueDiagnostics.Snapshot(const aSite: String; const aNowUs: Int64;
  const aTasks, aWaiting, aFreeSlots, aDue: Integer; const aOldestDueMs: Int64;
  const aForce: Boolean = False): String;
var
  fCounter: TQueueDiagnosticCounter;
  fReason: TQueueWakeReason;
  fTiming: TQueueDiagnosticTiming;
  fSignalCount: Integer;
begin
  Result := '';
  if not aForce and not ReportDue(aNowUs) then
    Exit;
  Result := Format('[QUEUE-DIAG] site=%s window_ms=%d tasks=%d waiting=%d free_slots=%d due_unassigned=%d oldest_due_ms=%d',
    [aSite, (aNowUs - FWindowStartedUs) div 1000, aTasks, aWaiting, aFreeSlots, aDue, aOldestDueMs]);
  for fCounter := Low(fCounter) to High(fCounter) do
    Result := Result + ' ' + glCounterNames[fCounter] + '=' + IntToStr(FCounters[fCounter]);
  for fReason := Low(fReason) to High(fReason) do
  begin
    fSignalCount := {$IFDEF FPC}InterlockedExchange{$ELSE}AtomicExchange{$ENDIF}(FSignals[fReason], 0);
    Result := Result + ' signal_' + glWakeNames[fReason] + '=' + IntToStr(fSignalCount);
  end;
  for fTiming := Low(fTiming) to High(fTiming) do
    Result := Result + Format(' %s_n=%d %s_sum_us=%d %s_max_us=%d',
      [glTimingNames[fTiming], FTimes[fTiming].Count,
       glTimingNames[fTiming], FTimes[fTiming].SumUs,
       glTimingNames[fTiming], FTimes[fTiming].MaxUs]);
  FillChar(FCounters, SizeOf(FCounters), 0);
  FillChar(FTimes, SizeOf(FTimes), 0);
  FWindowStartedUs := aNowUs;
end;

end.
