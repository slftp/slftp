unit slcriticalsection2;

interface

uses
  SyncObjs, Generics.Collections, sltimer, Generics.Defaults;

{
  TslCriticalSection
  Provides a possibility for a critical section to have a timeout when trying to enter.
  Supports multiple nested enter calls from the same thread.
  Allows to enable detailed monitoring of wait and hold times.
}


type
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
    FContentionCount: Int64;
    FMaxWaitUs: Int64;
    FTotalWaitUs: Int64;
    FMaxHoldUs: Int64;
    FHoldStartUs: Int64;
    FLastWaitOwner: string;
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
    property Name: string read FName; //< name of this critical section
    property ContentionCount: Int64 read FContentionCount; //< number of times a thread had to wait to acquire this lock
    property MaxWaitUs: Int64 read FMaxWaitUs; //< maximum wait time in microseconds to acquire this lock
    property TotalWaitUs: Int64 read FTotalWaitUs; //< total wait time in microseconds across all acquisitions
    property MaxHoldUs: Int64 read FMaxHoldUs; //< maximum duration in microseconds this lock was held
    property LastWaitOwner: string read FLastWaitOwner; //< name of the last code segment that experienced a delay
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

  { Returns a formatted summary string of lock contention statistics across all critical sections
    @returns(Formatted statistics string) }
  function GetCriticalSection2Summary: String;


implementation
  uses
    SysUtils, debugunit, Classes, Math, mormot.core.os;

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
  var
    fDisambigName: string;
    fDupCounter: Integer;
  begin
    if not glIsInitialized then // happens at startup when a TslCriticalSection2 is created before initialization
    begin
      // init with timeout locking enabled and then if it will be initialized again with timeout locking disabled, change the existing instances
      SlCriticalSection2Init(60000, True);
    end;

    aName := aName.Replace('\', '_'); // backslash not allowed on windows

    FName := aName;
    FContentionCount := 0;
    FMaxWaitUs := 0;
    FTotalWaitUs := 0;
    FMaxHoldUs := 0;
    FHoldStartUs := 0;
    FLastWaitOwner := '';

    glUsedCriticalSectionsLock.Enter;
    try
      fDisambigName := aName;
      fDupCounter := 1;
      while glUsedCriticalSections.ContainsKey(fDisambigName) do
      begin
        Inc(fDupCounter);
        fDisambigName := Format('%s#%d', [aName, fDupCounter]);
      end;
      FName := fDisambigName;
      glUsedCriticalSections.Add(fDisambigName, self);
    finally
      glUsedCriticalSectionsLock.Leave;
    end;

    if glUseTimeoutLocking Or aAlwaysUseTimeoutLocking then
    begin
      FUseTimeoutLocking := True;
      FEvent := TEvent.Create(nil, False, True, 'SLFTP_' + FName);
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
    if glUsedCriticalSectionsLock <> nil then
    begin
      glUsedCriticalSectionsLock.Enter;
      try
        if glUsedCriticalSections <> nil then
          glUsedCriticalSections.Remove(self.FName);
      finally
        glUsedCriticalSectionsLock.Leave;
      end;
    end;

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
    tWaitStart, tWaitStop, tWaitUs: Int64;
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
      QueryPerformanceMicroSeconds(tWaitStart);
      FInternalCriticalSection.Enter;
      QueryPerformanceMicroSeconds(tWaitStop);
      tWaitUs := tWaitStop - tWaitStart;
      if tWaitUs > 50 then
      begin
        Inc(FContentionCount);
        Inc(FTotalWaitUs, tWaitUs);
        if tWaitUs > FMaxWaitUs then
          FMaxWaitUs := tWaitUs;
        FLastWaitOwner := aLockOwnerName;
        if (tWaitUs > 200) and (FName <> 'debug_lock') then
          Debug(dpMessage, glDebugSection, Format('[LOCK DELAY] %s (%s): waited %d us', [FName, aLockOwnerName, tWaitUs]));
      end;
      Inc(FLockCount);
      if FLockCount = 1 then
        QueryPerformanceMicroSeconds(FHoldStartUs);
      Result := True;
    end;
  end;

  procedure TslCriticalSection2.Leave;
  var
    fLockOwnerName: String;
    fTimer: TSLTimer;
    tHoldStop, tHoldUs: Int64;

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
      Dec(FLockCount);
      if (FLockCount = 0) and (FHoldStartUs > 0) then
      begin
        QueryPerformanceMicroSeconds(tHoldStop);
        tHoldUs := tHoldStop - FHoldStartUs;
        if tHoldUs > FMaxHoldUs then
          FMaxHoldUs := tHoldUs;
        FHoldStartUs := 0;
      end;
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

function _CriticalSectionStatsSorter({$IFDEF FPC}constref{$ELSE}const{$ENDIF} Left, Right: TslCriticalSection2): Integer;
begin
  Result := CompareValue(Right.MaxWaitUs, Left.MaxWaitUs);
end;

function GetCriticalSection2Summary: String;
var
  fSortedList: TList<TslCriticalSection2>;
  fCs: TslCriticalSection2;
  fOutput: TStringList;
  fAvgWaitUs: Int64;
begin
  fSortedList := TList<TslCriticalSection2>.Create;
  fOutput := TStringList.Create;
  try
    if glUsedCriticalSectionsLock <> nil then
    begin
      glUsedCriticalSectionsLock.Enter;
      try
        if glUsedCriticalSections <> nil then
        begin
          for fCs in glUsedCriticalSections.Values do
          begin
            if (fCs.ContentionCount > 0) or (fCs.MaxHoldUs > 0) or (fCs.MaxWaitUs > 0) then
              fSortedList.Add(fCs);
          end;
        end;
      finally
        glUsedCriticalSectionsLock.Leave;
      end;
    end;

    if fSortedList.Count = 0 then
    begin
      Result := 'Lock contention stats: No lock contention or delays recorded.';
      Exit;
    end;

    fSortedList.Sort(TComparer<TslCriticalSection2>.Construct(_CriticalSectionStatsSorter));

    fOutput.Add(Format('<b>Lock Contention Stats</b> (%d active locks):', [fSortedList.Count]));
    for fCs in fSortedList do
    begin
      if fCs.ContentionCount > 0 then
        fAvgWaitUs := fCs.TotalWaitUs div fCs.ContentionCount
      else
        fAvgWaitUs := 0;

      fOutput.Add(Format('  <b>%s</b>: contentions=%d, max_wait=%dus, avg_wait=%dus, max_hold=%dus (last: %s)',
        [fCs.Name, fCs.ContentionCount, fCs.MaxWaitUs, fAvgWaitUs, fCs.MaxHoldUs, fCs.LastWaitOwner]));
    end;

    Result := fOutput.Text;
  finally
    fSortedList.Free;
    fOutput.Free;
  end;
end;

end.

