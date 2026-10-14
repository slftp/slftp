unit queuediagnostics;

interface

uses
  SyncObjs;

type
  { First failed scheduler check; not an exhaustive list of simultaneous blockers. }
  TQueueBlockReason = (qbNone, qbNotChecked, qbDelayed, qbMkdir, qbDependency,
    qbSourceFull, qbSourceOffline, qbDestinationFull, qbDestinationOffline,
    qbSourceLimit, qbDestinationLimit, qbCooldown, qbActiveFile,
    qbDestinationLock, qbCachedDestination, qbDirlistLimit, qbWantedSlot, qbUnknown);
  { Inline, queue-owned state. Durations start at observation, never at task creation. }
  TQueueBlockState = record
  private
    FReason, FPreviousReason, FCacheReason, FEventReason: TQueueBlockReason;
    FReasonAt, FCheckedAt, FBlockedUs, FFinishedAt, FEventAt, FEventCheckedAt: Int64;
    FRetryAt, FRevision: Int64;
    FUsed, FLimit, FChanges: Integer;
    FClassified: Boolean;
    FEventKind: String;
  public
    { Begins an actual dispatch attempt, allowing uncovered exits to be labelled. }
    procedure BeginCheck(const aNowUs: Int64);
    { Records the existing check's result, without querying readiness again. }
    procedure Observe(const aReason: TQueueBlockReason; const aNowUs: Int64;
      const aCacheReason: TQueueBlockReason = qbNone; const aRetryMs: Int64 = 0;
      const aRevision: Int64 = 0; const aUsed: Integer = -1; const aLimit: Integer = -1);
    { Associates a relevant producer event with its first subsequent dispatch check.
      A destination notification is not proof that all prerequisites became ready. }
    procedure RecordEvent(const aEventUs, aCheckUs: Int64; const aKind: String);
    { Freezes elapsed time on assignment or removal. Idempotent. }
    procedure Finish(const aNowUs: Int64);
    { Returns observed pending time, excluding planned delays and finished time. }
    function BlockedUs(const aNowUs: Int64): Int64;
    { Formats only an immutable sample; no task pointers or release/file names. }
    function Detail(const aNowUs, aAgeMs, aPlannedMs: Int64; const aOutcome: String): String;
    property Reason: TQueueBlockReason read FReason;
    property Classified: Boolean read FClassified;
    property FinishedAt: Int64 read FFinishedAt;
  end;
  { Borrowed pointer to inline state, valid only while its task remains owned. }
  PQueueBlockState = ^TQueueBlockState;

  { Signal requests, not individual wakes: auto-reset events coalesce requests. }
  TQueueWakeReason = (qwExternal, qwTask, qwEarlierDeadline, qwSlot, qwDestination, qwCleanup);
  { Per-window counters. Blocked counts describe checks, not unique tasks. }
  TQueueDiagnosticCounter = (qcPass, qcEvent, qcDeadline, qcFallback, qcCappedWait,
    qcFallbackWithWork, qcInserted, qcInsertedAssigned, qcInsertedWait,
    qcPromoted, qcSortRequest, qcSort, qcAttempt, qcAssignedRace,
    qcCompletedRace, qcCompletedBytes, qcRetryDelay, qcSourceFull,
    qcDestinationFull, qcDestinationLock, qcBusyCache, qcSourceLimit,
    qcDestinationLimit, qcActiveFile, qcCooldown, qcDependency, qcDependencyChanged, qcDelayedRace, qcInitialDelayMs,
    qcZeroWait, qcFutureZeroWait, qcEarlyTimeout, qcEarlyTimeoutMissingUs, qcWaitError);
  { Durations in microseconds, with count/sum/max. Refill starts at slot release. }
  TQueueDiagnosticTiming = (qtPass, qtQueueLock, qtSort, qtCleanup, qtConsole,
    qtSourceRefill, qtDestinationRefill, qtWait, qtTimeoutRequested,
    qtTimeoutElapsed, qtClockLag);
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
    FBlockedCounts: array[TQueueBlockReason] of Integer;
    FBlockSamples: array[0..2] of record
      FUid: UInt64;
      FDurationUs: Int64;
      FText: String;
    end;
    FBlockSampleCount, FBlockCandidates: Integer;
  public
    { Starts a measurement window using a monotonic clock. }
    constructor Create;
    { Counts one signal request without acquiring a scheduler lock. }
    procedure Signal(const aReason: TQueueWakeReason);
    { Adds to a queue-owned counter while holding the queue lock. }
    procedure Count(const aCounter: TQueueDiagnosticCounter; const aValue: Int64 = 1);
    { Accumulates a nonnegative duration while holding the queue lock. }
    procedure Measure(const aTiming: TQueueDiagnosticTiming; const aElapsedUs: Int64);
    { Records actual event wait time; only positive timeout returns can be early.
      Allows 100 us or 0.1 percent for clock drift and measurement uncertainty.
      A negative clock lag denotes an unavailable measurement. Requires queue lock. }
    procedure RecordWait(const aRequestedMs: Cardinal; const aElapsedUs: Int64;
      const aResult: TWaitResult; const aClockLagUs: Int64);
    { Returns true at most once per ten seconds, unless explicitly forced. }
    function ReportDue(const aNowUs: Int64): Boolean;
    { Adds one task to the current snapshot's reason counts. }
    procedure CountBlocked(const aReason: TQueueBlockReason);
    { Keeps at most three longest pending/completed examples per window. }
    procedure SampleBlock(const aUid: UInt64; const aType, aSource, aTarget: String;
      const aState: TQueueBlockState; const aNowUs, aAgeMs, aPlannedMs: Int64;
      const aSourceFree, aTargetFree: Integer; const aOutcome: String);
    { Takes and resets the window under the queue lock. Returns an empty string
      before ten seconds unless forced. Emit the result AFTER releasing locks.
      Due work includes tasks blocked on dependencies or destination capacity. }
    function Snapshot(const aSite: String; const aNowUs: Int64;
      const aTasks, aWaiting, aFreeSlots, aDue: Integer; const aOldestDueMs: Int64;
      const aForce: Boolean = False): String;
  end;

{ Returns a system-wide monotonic microsecond timestamp using the bundled runtime. }
function QueueDiagnosticNow: Int64;
{ Returns a conservative lower bound on RTL CLOCK_MONOTONIC lag behind libc's
  precise CLOCK_MONOTONIC in microseconds, or -1 outside Linux/FPC or on error. }
function QueueDiagnosticClockLag: Int64;
{ Atomically publish/read producer timestamps across site and queue threads. }
procedure QueueDiagnosticPublish(var aTimestamp: Int64; const aValue: Int64);
{ Reads a producer timestamp without a new scheduler lock. }
function QueueDiagnosticRead(var aTimestamp: Int64): Int64;

implementation

uses
  SysUtils, mormot.core.os
  {$IF Defined(FPC) and Defined(LINUX)}, UnixType, Linux{$IFEND};

const
  glBlockReasonNames: array[TQueueBlockReason] of String = (
    'none', 'not_checked', 'planned_delay', 'mkdir_pending', 'dependency',
    'source_slots_full', 'source_no_online_slot', 'destination_slots_full',
    'destination_no_online_slot', 'source_limit', 'destination_limit', 'cooldown',
    'active_file', 'destination_lock', 'destination_cache', 'dirlist_limit',
    'wanted_slot_unavailable', 'unclassified');
  glCounterNames: array[TQueueDiagnosticCounter] of String = (
    'passes', 'wake_event', 'wake_deadline', 'wake_fallback', 'wake_capped',
    'fallback_with_free_slot_and_due_work', 'inserted', 'inserted_assigned',
    'inserted_wait', 'promoted', 'sort_requests', 'sorts', 'assign_attempts',
    'assigned_races', 'completed_races', 'completed_declared_bytes',
    'retry_delay', 'source_full', 'destination_full', 'destination_lock',
    'busy_cache', 'source_limit', 'destination_limit', 'active_file',
    'cooldown', 'dependency', 'dependency_changed_during_registration',
    'delayed_race_insertions', 'initial_race_delay_sum_ms',
    'zero_waits', 'future_zero_waits', 'early_timeouts',
    'early_timeout_missing_sum_us', 'wait_errors');
  glWakeNames: array[TQueueWakeReason] of String = (
    'external', 'task', 'earlier_deadline', 'slot', 'destination', 'cleanup');
  glTimingNames: array[TQueueDiagnosticTiming] of String = (
    'pass', 'queue_lock_wait', 'sort', 'cleanup', 'console_add',
    'source_refill', 'destination_refill', 'wait', 'timeout_requested',
    'timeout_elapsed', 'clock_lag');

  glClockMode = {$IF Defined(FPC) and Defined(LINUX)}
    {$IFDEF NOPATCHRTL}'nopatchrtl'{$ELSE}'default_rtl'{$ENDIF}
    {$ELSE}'native'{$IFEND};

function QueueDiagnosticNow: Int64;
begin
  QueryPerformanceMicroSeconds(Result);
end;

{$IF Defined(FPC) and Defined(LINUX)}
{ Call libc directly so the mORMot redirection of Linux.clock_gettime is bypassed. }
function _PreciseClockGetTime(aClockId: LongInt; aTime: UnixType.PTimeSpec): LongInt;
  cdecl; external 'c' name 'clock_gettime';
{$IFEND}

function QueueDiagnosticClockLag: Int64;
{$IF Defined(FPC) and Defined(LINUX)}
var
  fPrecise, fRTL: UnixType.TTimeSpec;
{$IFEND}
begin
  Result := -1;
  {$IF Defined(FPC) and Defined(LINUX)}
  if _PreciseClockGetTime(Linux.CLOCK_MONOTONIC, @fPrecise) <> 0 then
    Exit;
  if Linux.clock_gettime(Linux.CLOCK_MONOTONIC, @fRTL) <> 0 then
    Exit;
  Result := (Int64(fPrecise.tv_sec) - Int64(fRTL.tv_sec)) * 1000000 +
    Int64(fPrecise.tv_nsec) div 1000 - Int64(fRTL.tv_nsec) div 1000;
  if Result < 0 then
    Result := 0;
  {$IFEND}
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

procedure QueueDiagnosticPublish(var aTimestamp: Int64; const aValue: Int64);
begin
  {$IFDEF FPC}InterlockedExchange64{$ELSE}AtomicExchange{$ENDIF}(aTimestamp, aValue);
end;

function QueueDiagnosticRead(var aTimestamp: Int64): Int64;
begin
  Result := {$IFDEF FPC}InterlockedCompareExchange64{$ELSE}AtomicCmpExchange{$ENDIF}(aTimestamp, 0, 0);
end;

procedure TQueueBlockState.BeginCheck(const aNowUs: Int64);
begin
  FCheckedAt := aNowUs;
  FClassified := False;
end;

procedure TQueueBlockState.Observe(const aReason: TQueueBlockReason;
  const aNowUs: Int64; const aCacheReason: TQueueBlockReason; const aRetryMs,
  aRevision: Int64; const aUsed, aLimit: Integer);
begin
  if FFinishedAt <> 0 then Exit;
  FClassified := True;
  FCheckedAt := aNowUs;
  if aReason <> FReason then
  begin
    if (FReasonAt > 0) and not (FReason in [qbNone, qbDelayed]) and (aNowUs > FReasonAt) then
      Inc(FBlockedUs, aNowUs - FReasonAt);
    FPreviousReason := FReason;
    FReason := aReason;
    FReasonAt := aNowUs;
    Inc(FChanges);
  end;
  FCacheReason := aCacheReason;
  FRetryAt := aNowUs + aRetryMs * 1000;
  FRevision := aRevision;
  FUsed := aUsed;
  FLimit := aLimit;
end;

procedure TQueueBlockState.RecordEvent(const aEventUs, aCheckUs: Int64; const aKind: String);
begin
  if (aEventUs > FEventAt) and (FReasonAt > 0) and (aEventUs >= FReasonAt) and
    (aEventUs > 0) and (aCheckUs >= aEventUs) then
  begin
    FEventAt := aEventUs;
    FEventCheckedAt := aCheckUs;
    FEventKind := aKind;
    FEventReason := FReason;
  end;
end;

function TQueueBlockState.BlockedUs(const aNowUs: Int64): Int64;
var
  fEnd: Int64;
begin
  Result := FBlockedUs;
  fEnd := aNowUs;
  if FFinishedAt > 0 then fEnd := FFinishedAt;
  if (FReasonAt > 0) and not (FReason in [qbNone, qbDelayed]) and (fEnd > FReasonAt) then
    Inc(Result, fEnd - FReasonAt);
end;

procedure TQueueBlockState.Finish(const aNowUs: Int64);
begin
  if FFinishedAt = 0 then FFinishedAt := aNowUs;
end;

function TQueueBlockState.Detail(const aNowUs, aAgeMs, aPlannedMs: Int64;
  const aOutcome: String): String;
var
  fEventToCheck, fCheckToAssign, fRetryMs, fReasonMs, fCheckAge: Int64;
  fKind: String;
begin
  fEventToCheck := -1;
  fCheckToAssign := -1;
  fKind := 'none';
  if FEventAt > 0 then
  begin
    fKind := FEventKind;
    fEventToCheck := (FEventCheckedAt - FEventAt) div 1000;
    if (aOutcome = 'assigned') and (FFinishedAt >= FEventCheckedAt) then
      fCheckToAssign := (FFinishedAt - FEventCheckedAt) div 1000;
  end;
  fRetryMs := (FRetryAt - aNowUs) div 1000;
  if fRetryMs < 0 then fRetryMs := 0;
  fReasonMs := 0;
  if FReasonAt > 0 then fReasonMs := (aNowUs - FReasonAt) div 1000;
  if fReasonMs < 0 then fReasonMs := 0;
  fCheckAge := -1;
  if FCheckedAt > 0 then
  begin
    fCheckAge := (aNowUs - FCheckedAt) div 1000;
    if fCheckAge < 0 then fCheckAge := 0;
  end;
  Result := Format(' reason=%s previous_reason=%s blocked_ms=%d reason_ms=%d task_age_ms=%d planned_remaining_ms=%d check_age_ms=%d changes=%d cache_reason=%s cache_retry_remaining_ms=%d destination_revision=%d limit_used=%d limit_max=%d event_kind=%s event_reason=%s event_to_check_ms=%d check_to_assign_ms=%d outcome=%s',
    [glBlockReasonNames[FReason], glBlockReasonNames[FPreviousReason], BlockedUs(aNowUs) div 1000,
     fReasonMs, aAgeMs, aPlannedMs, fCheckAge, FChanges, glBlockReasonNames[FCacheReason],
     fRetryMs, FRevision, FUsed, FLimit, fKind, glBlockReasonNames[FEventReason], fEventToCheck, fCheckToAssign, aOutcome]);
end;

procedure TQueueDiagnostics.CountBlocked(const aReason: TQueueBlockReason);
begin
  Inc(FBlockedCounts[aReason]);
end;

procedure TQueueDiagnostics.SampleBlock(const aUid: UInt64;
  const aType, aSource, aTarget: String; const aState: TQueueBlockState;
  const aNowUs, aAgeMs, aPlannedMs: Int64; const aSourceFree, aTargetFree: Integer;
  const aOutcome: String);
var
  fDuration: Int64;
  fIndex, i: Integer;
  fPrefix: String;
begin
  fDuration := aState.BlockedUs(aNowUs);
  if fDuration < 2000000 then Exit;
  fIndex := -1;
  for i := 0 to FBlockSampleCount - 1 do
    if FBlockSamples[i].FUid = aUid then fIndex := i;
  if fIndex < 0 then
  begin
    Inc(FBlockCandidates);
    if FBlockSampleCount < Length(FBlockSamples) then
    begin
      fIndex := FBlockSampleCount;
      Inc(FBlockSampleCount);
    end
    else
    begin
      fIndex := 0;
      for i := 1 to High(FBlockSamples) do
        if FBlockSamples[i].FDurationUs < FBlockSamples[fIndex].FDurationUs then fIndex := i;
      if fDuration <= FBlockSamples[fIndex].FDurationUs then Exit;
    end;
  end;
  FBlockSamples[fIndex].FUid := aUid;
  FBlockSamples[fIndex].FDurationUs := fDuration;
  if aOutcome = 'pending' then fPrefix := '[QUEUE-BLOCK]' else fPrefix := '[QUEUE-RESUME]';
  FBlockSamples[fIndex].FText := Format('%s task=%d type=%s source=%s target=%s source_free_slots=%d destination_free_slots=%d observed_at_us=%d',
    [fPrefix, aUid, aType, aSource, aTarget, aSourceFree, aTargetFree, aNowUs]) +
    aState.Detail(aNowUs, aAgeMs, aPlannedMs, aOutcome);
end;

procedure TQueueDiagnostics.RecordWait(const aRequestedMs: Cardinal;
  const aElapsedUs: Int64; const aResult: TWaitResult; const aClockLagUs: Int64);
var
  fRequestedUs, fToleranceUs: Int64;
begin
  if aElapsedUs < 0 then
    Exit;
  Measure(qtWait, aElapsedUs);
  Measure(qtClockLag, aClockLagUs);
  if (aResult = wrTimeout) and (aRequestedMs > 0) then
  begin
    fRequestedUs := Int64(aRequestedMs) * 1000;
    Measure(qtTimeoutRequested, fRequestedUs);
    Measure(qtTimeoutElapsed, aElapsedUs);
    fToleranceUs := fRequestedUs div 1000;
    if fToleranceUs < 100 then
      fToleranceUs := 100;
    if aElapsedUs < fRequestedUs - fToleranceUs then
    begin
      Count(qcEarlyTimeout);
      Count(qcEarlyTimeoutMissingUs, fRequestedUs - aElapsedUs);
    end;
  end
  else if not (aResult in [wrSignaled, wrTimeout]) then
    Count(qcWaitError);
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
  fBlockReason: TQueueBlockReason;
  i: Integer;
begin
  Result := '';
  if not aForce and not ReportDue(aNowUs) then
    Exit;
  Result := Format('[QUEUE-DIAG] site=%s window_ms=%d tasks=%d waiting=%d free_slots=%d due_unassigned=%d oldest_due_ms=%d',
    [aSite, (aNowUs - FWindowStartedUs) div 1000, aTasks, aWaiting, aFreeSlots, aDue, aOldestDueMs]);
  Result := Result + ' clock_mode=' + glClockMode;
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
  Result := Result + Format(' block_samples=%d block_samples_omitted=%d',
    [FBlockSampleCount, FBlockCandidates - FBlockSampleCount]);
  for fBlockReason := Low(fBlockReason) to High(fBlockReason) do
    Result := Result + ' blocked_' + glBlockReasonNames[fBlockReason] + '=' + IntToStr(FBlockedCounts[fBlockReason]);
  for i := 0 to FBlockSampleCount - 1 do
  begin
    Result := Result + #10 + FBlockSamples[i].FText;
    FBlockSamples[i].FText := '';
  end;
  FBlockSampleCount := 0;
  FBlockCandidates := 0;
  FillChar(FBlockedCounts, SizeOf(FBlockedCounts), 0);
  FillChar(FCounters, SizeOf(FCounters), 0);
  FillChar(FTimes, SizeOf(FTimes), 0);
  FWindowStartedUs := aNowUs;
end;

end.
