unit slcriticalsection2;

interface

uses
  SyncObjs, Generics.Collections, sltimer, Generics.Defaults, mormot.core.os
  {$IFNDEF FPC}
  , Winapi.Windows
  {$ENDIF};

{
  TslCriticalSection
  Provides a possibility for a critical section to have a timeout when trying to enter.
  Supports multiple nested enter calls from the same thread.
  Allows to enable detailed monitoring of wait and hold times.
}


type
  { TslRWLock - shared/exclusive lock built on mORMot2 TRWLock with the same
    naming and Enter/Leave wrapper style as TslCriticalSection2.
    - Enter / Leave             -> exclusive write lock (replaces a critical section)
    - EnterReadOnly / LeaveReadOnly -> shared read lock (multiple readers in parallel)

    Reentrant semantics match TRWLock: WriteLock is reentrant within the same
    thread. ReadOnlyLock CANNOT be acquired while WriteLock is held by the same
    thread (TRWLock.ReadOnlyLock spins on the write bit and would deadlock the
    same thread on itself); EnterReadOnly raises an exception in that case.
    WriteLock-inside-ReadOnlyLock would deadlock per TRWLock's own docs.

    No timeout support (TRWLock spins on wait), so the deadlock-debug timeout
    from TslCriticalSection2 does not apply here. The wrapper does, however,
    track Write-owner thread/count and an outstanding-Read counter so that an
    unbalanced Leave or LeaveReadOnly raises an exception immediately instead
    of silently corrupting the underlying TRWLock Flags counter (which has no
    underflow check and would wrap around to MaxPtrUInt, permanently breaking
    the lock). }
  TslRWLock = class
  private
    FName: string;
    FRWLock: TRWLock;
    FWriteOwner: TThreadID;
    FWriteCount: integer; //< nesting depth of Enter/Leave by FWriteOwner
    FReadCount: integer;  //< total outstanding EnterReadOnly across all threads
  public
    constructor Create(const aName: string);
    destructor Destroy; override;

    { Acquire exclusive write lock. aLockOwnerName is currently informational only
      for parity with TslCriticalSection2.Enter. Returns True (kept for API symmetry). }
    function Enter(const aLockOwnerName: string = ''): boolean;
    { Release exclusive write lock. Raises an exception when called by a thread
      that does not currently hold the write lock or when called more times than
      Enter on the owning thread. }
    procedure Leave;

    { Acquire shared read lock - multiple callers may hold this concurrently.
      Raises an exception when the calling thread already holds the write lock
      (would otherwise spin forever inside TRWLock.ReadOnlyLock). }
    function EnterReadOnly(const aLockOwnerName: string = ''): boolean;
    { Release shared read lock. Raises an exception when there is no outstanding
      EnterReadOnly globally on this lock. }
    procedure LeaveReadOnly;

    property Name: string read FName;
  end;

  TslCriticalSection2 = class
  private
    FInternalCriticalSection: TCriticalSection;
    FEvent: TEvent;
    FLockCount: integer;
    FLockOwningThreadID: TThreadID;
    FName, FCurrentCodeSegmentName: string;
    FUseTimeoutLocking: boolean;
    FLockOwnerNameStack: TStack<string>;
    FHoldTimerStack: TStack<TSLTimer>;
    FWaitTimesDict: TDictionary<string, Double>;
    FHoldTimesDict: TDictionary<string, Double>;
    FLockCountDict: TDictionary<string, Integer>;
    function GetCurrentLockOwnerName: string;
    procedure InitNoTimeoutLocking;
    procedure FreeObjects;
  public
    { Constuctor.
      @param(aName A unique name for this critical section. If another instance with the same name already exists, an exception will be raised.)
      @param(aAlwaysUseTimeoutLocking Set to true, if this instance should always use the timeout locking feature, even if it's not enabled globally.) }
    constructor Create(aName: string; const aAlwaysUseTimeoutLocking: boolean = False);
    destructor Destroy; override;

    { Returns an existing TslCriticalSection2 if there already exists one with the same name. Creates a new TslCriticalSection2 otherwise.
      Use with caution to not produce deadlocks. Only use if you know what you are doing!
      @param(aName The unique name for the critical section.) }
    class function GetOrCreate(const aName: string): TslCriticalSection2;

    { Acquire lock.
      @param(aLockOwnerName A unique name for the code which invokes this function. This is for debugging and performance monitoring purposes.)
      @param(aTimeoutMs The timeout for how long should be waited to acquire the lock.)
      @param(aRaiseExceptionOnFail Raise an exception if the lock could not be acuired within the timeout limit rather than just returning false.)
      @returns(True, if the lock has been acquired, false otherwise. }
    function Enter(const aLockOwnerName: string; const aTimeoutMs: Cardinal; const aRaiseExceptionOnFail: boolean = True): boolean; overload;

    { Acquire lock.
      @param(aLockOwnerName A unique name for the code which invokes this function. This is for debugging and performance monitoring purposes.)
      @returns(True, if the lock has been acquired, false otherwise. }
    function Enter(const aLockOwnerName: string): boolean; overload;

    { Leaves the previously acquired lock }
    procedure Leave;

    { Set an information about which code is currently being executed while holding this lock. This is for (performance) debugging purposes. }
    procedure SetCurrentCodeSegment(const aSegmentName: string);

    { Returns the name of the code part that is currently executing while holding this lock. }
    property CurrentLockOwnerName: string read GetCurrentLockOwnerName;
  end;

  { Initalize this unit.
    @param(aLockingTimeout Set the default locking timout.)
    @param(aUseTimer Set to true, if hold and wait times should be recorded. This might add some performance penalty.) }
  procedure SlCriticalSection2Init(const aLockingTimeout: integer; const aUseTimer: Boolean);

  { Unnitalize this unit - free all resources. }
  procedure SlCriticalSection2Uninit;

  { Returns true, if timeout locking is enabled globally, false otherwise. }
  function GetUseTimeoutLocking: boolean;

  { Returns true, if wait times are being recorded, false otherwise. }
  function GetUseTimer: boolean;

  { Writes all wait and hold times of locks into a log file at the path of slftp executable and returns that path. }
  function WriteCriticalSection2StatsToFile: String;


implementation
  uses
    SysUtils, debugunit, Classes, Math;

  { TslRWLock }

  constructor TslRWLock.Create(const aName: string);
  begin
    inherited Create;
    FName := aName;
    FRWLock.Init;
    FWriteOwner := TThreadID(0);
    FWriteCount := 0;
    FReadCount := 0;
  end;

  destructor TslRWLock.Destroy;
  begin
    if FWriteCount > 0 then
      Debug(dpError, 'slcriticalsection2', Format('TslRWLock(%s) destroyed while held by thread %s (write count=%d)',
        [FName, IntToHex(FWriteOwner, 4), FWriteCount]));
    if FReadCount > 0 then
      Debug(dpError, 'slcriticalsection2', Format('TslRWLock(%s) destroyed with %d outstanding readers',
        [FName, FReadCount]));
    FRWLock.AssertDone;
    inherited;
  end;

  function TslRWLock.Enter(const aLockOwnerName: string): boolean;
  var
    tid: TThreadID;
  begin
    FRWLock.WriteLock;
    // After WriteLock returns, this thread is the exclusive write owner. The
    // increment is safe without an extra atomic primitive because no other
    // thread can be in this branch concurrently; reentrant calls from the same
    // thread serialize through TRWLock's internal LastWriteLockCount.
    tid := GetCurrentThreadId;
    FWriteOwner := tid;
    Inc(FWriteCount);
    Result := True;
  end;

  procedure TslRWLock.Leave;
  var
    tid: TThreadID;
  begin
    tid := GetCurrentThreadId;
    if (FWriteCount <= 0) or (FWriteOwner <> tid) then
      raise Exception.CreateFmt(
        'TslRWLock(%s).Leave by thread %s but lock is not held by this thread (owner=%s count=%d)',
        [FName, IntToHex(tid, 4), IntToHex(FWriteOwner, 4), FWriteCount]);
    Dec(FWriteCount);
    if FWriteCount = 0 then
      FWriteOwner := TThreadID(0);
    FRWLock.WriteUnLock;
  end;

  function TslRWLock.EnterReadOnly(const aLockOwnerName: string): boolean;
  begin
    // Acquiring ReadOnlyLock while this thread already holds WriteLock spins
    // forever inside TRWLock (the writer bit is set by us, ReadOnlyLock waits
    // for it to clear, never happens). Detect and surface as an exception so
    // the caller fails loudly instead of hanging the slot thread.
    if (FWriteCount > 0) and (FWriteOwner = GetCurrentThreadId) then
      raise Exception.CreateFmt(
        'TslRWLock(%s).EnterReadOnly by thread %s while it holds the write lock; ' +
        'use a *Locked helper that assumes the write lock is already held instead',
        [FName, IntToHex(GetCurrentThreadId, 4)]);
    FRWLock.ReadOnlyLock;
    InterLockedIncrement(FReadCount);
    Result := True;
  end;

  procedure TslRWLock.LeaveReadOnly;
  begin
    if FReadCount <= 0 then
      raise Exception.CreateFmt(
        'TslRWLock(%s).LeaveReadOnly with no outstanding readers (count=%d) by thread %s',
        [FName, FReadCount, IntToHex(GetCurrentThreadId, 4)]);
    InterLockedDecrement(FReadCount);
    FRWLock.ReadOnlyUnLock;
  end;

  // these types are used for timer log output
  type
    TEntryData = record
      Name: string;
      WaitSum: Double;
      CriticalSection: TslCriticalSection2;
    end;
    TStrDoublePair = TPair<string, Double>;
    TStrDoublePairList = TList<TStrDoublePair>;

  var
    glUseTimeoutLocking: boolean = True;
    glUseTimer: boolean = True;
    glDefaultLockingTimeout: integer;
    glUsedCriticalSections: TDictionary<string, TslCriticalSection2>;
    glUsedCriticalSectionsLock: TCriticalSection;
    glDebugSection: string = 'slcriticalsection2';
    glIsInitialized: boolean = False;

  procedure SlCriticalSection2Init(const aLockingTimeout: integer; const aUseTimer: Boolean);
  var
    fExistingCs: TSlCriticalSection2;
  begin
    if aLockingTimeout > 0 then
    begin
      glUseTimeoutLocking := True;
      glDefaultLockingTimeout := aLockingTimeout;
    end
    else
      glUseTimeoutLocking := False;

    glUseTimer := aUseTimer;

    if not glIsInitialized then
    begin
      glUsedCriticalSections := TDictionary<string, TslCriticalSection2>.Create;
      glUsedCriticalSectionsLock := TCriticalSection.Create;
      glIsInitialized := True;
    end
    else if aLockingTimeout = 0 then
    begin
      // happens at startup when a TslCriticalSection2 is created before initialization then it will have called init already with timeout locking enabled
      // So if timeout locking is being disabled now, we will change the existing locks to be non timeout locks as well.
      glUsedCriticalSectionsLock.Enter;
      try
        for fExistingCs in glUsedCriticalSections.Values do
        begin
          if fExistingCs.FUseTimeoutLocking then
          begin
            fExistingCs.Enter('ChangeToSimpleLocking'); // no need to leave because we change to normal critical section
            fExistingCs.FreeObjects;
            fExistingCs.InitNoTimeoutLocking;
          end;
        end;
      finally
        glUsedCriticalSectionsLock.Leave;
      end;
    end;
  end;

  procedure SlCriticalSection2Uninit;
  begin
    glUseTimeoutLocking := False;
    FreeAndNil(glUsedCriticalSections);
    FreeAndNil(glUsedCriticalSectionsLock);
    glIsInitialized := False;
  end;

  procedure TslCriticalSection2.InitNoTimeoutLocking;
  begin
    FUseTimeoutLocking := False;
    FInternalCriticalSection := TCriticalSection.Create;
  end;

  constructor TslCriticalSection2.Create(aName: string; const aAlwaysUseTimeoutLocking: boolean = False);
  begin
    if not glIsInitialized then // happens at startup when a TslCriticalSection2 is created before initialization
    begin
      // init with timeout locking enabled and then if it will be initialized again with timeout locking disabled, change the existing instances
      SlCriticalSection2Init(60000, True);
    end;

    aName := aName.Replace('\', '_'); // backslash not allowed on windows

    FName := aName;
    if glUseTimeoutLocking Or aAlwaysUseTimeoutLocking then
    begin
      // make sure a TslCriticalSection2 only exists once with the same name, because of the named mutex
      glUsedCriticalSectionsLock.Enter;
      try
        if glUsedCriticalSections.ContainsKey(aName) then
        begin
          raise Exception.Create(Format('SL Critical section with name %s already exists.', [aName]));
        end;
        glUsedCriticalSections.Add(aName, self);
      finally
        glUsedCriticalSectionsLock.Leave;
      end;

      FUseTimeoutLocking := True;
      FEvent := TEvent.Create(nil, False, True, 'SLFTP_' + aName);
      FLockCount := 0;
      FLockOwningThreadID := 0;
      FCurrentCodeSegmentName := '';
      FLockOwnerNameStack := TStack<string>.Create;
      if glUseTimer then
      begin
        FHoldTimerStack := TStack<TSLTimer>.Create;
        FWaitTimesDict := TDictionary<string, Double>.Create;
        FHoldTimesDict := TDictionary<string, Double>.Create;
        FLockCountDict := TDictionary<string, Integer>.Create;
      end;
    end
    else
    begin
      self.InitNoTimeoutLocking;
    end;
  end;

  class function TslCriticalSection2.GetOrCreate(const aName: string): TslCriticalSection2;
  begin
    if not glIsInitialized then
      raise Exception.Create('TslCriticalSection2 system not initialized!');  // glUsedCriticalSections is not here in that case

    glUsedCriticalSectionsLock.Enter;
    try
      if not glUsedCriticalSections.TryGetValue(aName, Result) then
      begin
        Result := TslCriticalSection2.Create(aName);
      end;
    finally
      glUsedCriticalSectionsLock.Leave;
    end;
  end;

  procedure TSlCriticalSection2.FreeObjects;
  begin
    if FUseTimeoutLocking then
    begin
      FEvent.Free;
      FLockOwnerNameStack.Free;
      if glUseTimer then
      begin
        FreeAndNil(FWaitTimesDict);
        FreeAndNil(FHoldTimesDict);
        FreeAndNil(FLockCountDict);
        FreeAndNil(FHoldTimerStack);
      end;

      if glUsedCriticalSectionsLock <> nil then
      begin
        glUsedCriticalSectionsLock.Enter;
        try
          glUsedCriticalSections.Remove(self.FName);
        finally
          glUsedCriticalSectionsLock.Leave;
        end;
      end;
    end
    else
    begin
      FInternalCriticalSection.Free;
    end;
  end;

  destructor TslCriticalSection2.Destroy;
  begin
    self.FreeObjects;
  end;

  function TslCriticalSection2.Enter(const aLockOwnerName: string): boolean;
  begin
    Result := self.Enter(aLockOwnerName, glDefaultLockingTimeout);
  end;

  function TslCriticalSection2.Enter(const aLockOwnerName: string; const aTimeoutMs: Cardinal; const aRaiseExceptionOnFail: boolean = True): boolean;
  var
    fTimer, fHoldTimer: TSLTimer;
  begin

    if FUseTimeoutLocking then
    begin
      if glUseTimer then
      begin
        fTimer := TSLTimer.Create;
        fTimer.Start;
      end;

      try
        // allow for the same thread to enter multiple times
        if FLockOwningThreadID = GetCurrentThreadId then
        begin
          FLockCount := FLockCount + 1;
          Result := True;
          FLockOwnerNameStack.Push(aLockOwnerName);
        end
        else
        begin
          case FEvent.WaitFor(aTimeoutMs) of
            wrSignaled:
{$IFDEF WINDOWS}
            wrIOCompletion:
{$ENDIF}
              begin
                FLockOwningThreadID := GetCurrentThreadId;
                Result := True;
                FLockOwnerNameStack.Push(aLockOwnerName);
              end;
            wrTimeout:
              begin
                if aRaiseExceptionOnFail then
                begin
                  raise Exception.Create(Format('Unable to acquire lock ''%s'' (%s) by %s thread within %d ms. Lock is held by thread %s (%d) - %s (%s)', [FName, aLockOwnerName, IntToHex(GetCurrentThreadId, 4), aTimeoutMs, IntToHex(FLockOwningThreadID, 4), FLockCount, CurrentLockOwnerName, FCurrentCodeSegmentName]));
                end;
                Result := False;
              end;
            wrAbandoned:
              raise Exception.Create(Format('Mutex abandoned when trying to lock: %s', [aLockOwnerName]));
            wrError:
              raise Exception.Create(Format('Error when trying to lock: %s', [aLockOwnerName]));
          else
            raise Exception.Create(Format('Unknown wait result when trying to lock: %s', [aLockOwnerName]));
          end;
        end;

        if glUseTimer then
        begin
          fTimer.Stop;
          if Result then // maybe timeouts without exception would be interesting too?
          begin
            if not FWaitTimesDict.ContainsKey(aLockOwnerName) then
              FWaitTimesDict.Add(aLockOwnerName, fTimer.ElapsedMilliseconds)
            else
              FWaitTimesDict[aLockOwnerName] := FWaitTimesDict[aLockOwnerName] + fTimer.ElapsedMilliseconds;

            if not FLockCountDict.ContainsKey(aLockOwnerName) then
              FLockCountDict.Add(aLockOwnerName, 1)
            else
              FLockCountDict[aLockOwnerName] := FLockCountDict[aLockOwnerName] + 1;

            fHoldTimer := TSlTimer.Create;
            fHoldTimer.Start;
            FHoldTimerStack.Push(fHoldTimer);
          end;
        end;
      finally
        if glUseTimer then
          FreeAndNil(fTimer);
      end;
    end
    else
    begin
      FInternalCriticalSection.Enter;
      Result := True;
    end;
  end;

  procedure TslCriticalSection2.Leave;
  var
    fLockOwnerName: String;
    fTimer: TSLTimer;

    procedure _handleTimer;
    begin
      if glUseTimer then
      begin
        fTimer := FHoldTimerStack.Pop;
        try
          fTimer.Stop;
          if not FHoldTimesDict.ContainsKey(fLockOwnerName) then
             FHoldTimesDict.Add(fLockOwnerName, fTimer.ElapsedMilliseconds)
          else
            FHoldTimesDict[fLockOwnerName] := FHoldTimesDict[fLockOwnerName] + fTimer.ElapsedMilliseconds;
        finally
          fTimer.Free;
        end;
      end;
    end;

  begin
    if FUseTimeoutLocking then
    begin
      if FLockOwningThreadID = 0 then
        raise Exception.Create(Format('Trying to leave lock by thread %s but it has not been entered before', [IntToHex(GetCurrentThreadId, 4)]));

      if FLockOwningThreadID <> GetCurrentThreadId then
        raise Exception.Create(Format('Trying to leave lock by thread %s but it is held by thread %s (%d) - %s', [IntToHex(GetCurrentThreadId, 4), IntToHex(FLockOwningThreadID, 4), FLockCount, CurrentLockOwnerName]));

      if FLockCount > 0 then
      begin
        FLockCount := FLockCount - 1;
        fLockOwnerName := FLockOwnerNameStack.Pop;
        _handleTimer;
      end
      else
      begin
        FLockOwningThreadID := 0;
        fLockOwnerName := FLockOwnerNameStack.Pop;
        FCurrentCodeSegmentName := '';
        _handleTimer;

        // SetEvent must be the last thing we do because after that the next thread will start working
        FEvent.SetEvent;
      end;

    end
    else
    begin
      FInternalCriticalSection.Leave;
    end;
  end;

  procedure TslCriticalSection2.SetCurrentCodeSegment(const aSegmentName: string);
  begin
    if FUseTimeoutLocking then
    begin
      if FLockOwningThreadID = 0 then
      begin
        Debug(dpError, glDebugSection, Format('Tried to notify code segment ''%s'', but lock is not held by any thread.', [aSegmentName]));
        exit;
      end;

      if FLockOwningThreadID <> GetCurrentThreadId then
      begin
        Debug(dpError, glDebugSection, Format('Tried to notify code segment ''%s'', but lock is by another thread %s (%d) - %s.', [IntToHex(FLockOwningThreadID, 4), FLockCount, CurrentLockOwnerName]));
        exit;
      end;

      FCurrentCodeSegmentName := aSegmentName;
    end;
  end;

  function GetUseTimeoutLocking: boolean;
  begin
    Result := glUseTimeoutLocking;
  end;

  function GetUseTimer: boolean;
  begin
    Result := glUseTimer;
  end;

  function TSlCriticalSection2.GetCurrentLockOwnerName;
  begin
    if FUseTimeoutLocking and (FLockOwnerNameStack.Count > 0) then
      Result := FLockOwnerNameStack.Peek
    else
      Result := '';
  end;

  function _EntryDataSorter({$IFDEF FPC}constref{$ELSE}const{$ENDIF} Left, Right: TEntryData): Integer;
  begin
    Result := CompareValue(Right.WaitSum, Left.WaitSum);
  end;

  function _StrDoublePairSorter({$IFDEF FPC}constref{$ELSE}const{$ENDIF} Left, Right: TStrDoublePair): Integer;
  begin
    Result := CompareValue(Right.Value, Left.Value);
  end;

  function WriteCriticalSection2StatsToFile: String;
  var
    fSortedList: TList<TEntryData>;
    fPair: TPair<string, TslCriticalSection2>;
    fWPair: TPair<string, Double>;
    fEntry: TEntryData;
    fOutput: TStringList;
    fFilename, fNowstr: String;

  procedure SortAndWriteDict(const aHeader: string;
    const aDict: TDictionary<string, Double>;
    const aCountsDict: TDictionary<string, Integer>);
  var
    fSortedSub: TStrDoublePairList;
    fSubPair: TStrDoublePair;
    fCount: Integer;
    fAvg: Double;
  begin
    fOutput.Add('  ' + aHeader + ':');
    fSortedSub := TStrDoublePairList.Create;
    try
      for fSubPair in aDict do
        fSortedSub.Add(fSubPair);

      fSortedSub.Sort(TComparer<TStrDoublePair>.Construct(_StrDoublePairSorter));

      for fSubPair in fSortedSub do
      begin
        if aCountsDict.TryGetValue(fSubPair.Key, fCount) and (fCount > 0) then
          fAvg := fSubPair.Value / fCount
        else
          fAvg := 0;

        fOutput.Add(Format('    %s: total=%.3f, count=%d, avg=%.3f', [fSubPair.Key, fSubPair.Value, fCount, fAvg]));
      end;
    finally
      fSortedSub.Free;
    end;
  end;

begin
  fSortedList := TList<TEntryData>.Create;
  fOutput := TStringList.Create;
  try
    glUsedCriticalSectionsLock.Enter;
    try
      for fPair in glUsedCriticalSections do
      begin
        fEntry.Name := fPair.Key;
        fEntry.CriticalSection := fPair.Value;
        fEntry.WaitSum := 0;
        for fWPair in fEntry.CriticalSection.FWaitTimesDict do
          fEntry.WaitSum := fEntry.WaitSum + fWPair.Value;
        fSortedList.Add(fEntry);
      end;

    finally
      glUsedCriticalSectionsLock.Leave;
    end;

    fSortedList.Sort(TComparer<TEntryData>.Construct(_EntryDataSorter));

    fOutput.Add(Format('Number of Critical Section instances: %d', [glUsedCriticalSections.Count]));
    fOutput.Add('');

    for fEntry in fSortedList do
    begin
      fOutput.Add(Format('Critical Section: %s', [fEntry.Name]));
      fOutput.Add(Format('  Total Wait Time: %.3f', [fEntry.WaitSum]));

      SortAndWriteDict('Wait Times', fEntry.CriticalSection.FWaitTimesDict, fEntry.CriticalSection.FLockCountDict);
      SortAndWriteDict('Hold Times', fEntry.CriticalSection.FHoldTimesDict, fEntry.CriticalSection.FLockCountDict);

      fOutput.Add(''); // Empty line between entries
    end;

    DateTimeToString(fNowstr, 'yyyymmdd_hhnnss_zzz', Now());
    fFilename := ExtractFilePath(ParamStr(0)) + 'lockinfo.' + fNowstr + '.log';
    fOutput.SaveToFile(fFilename);
    Result := fFilename;
  finally
    fSortedList.Free;
    fOutput.Free;
  end;
end;

end.

