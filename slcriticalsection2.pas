unit slcriticalsection2;

interface

uses
  SyncObjs, Generics.Collections, Generics.Defaults;

{
  TslCriticalSection2
  Provides a fast critical section with timeout and deadlock detection, hold-time
  monitoring, and contention metrics.
  Supports multiple nested enter calls from the same thread.
}

type
  TslCriticalSection2 = class
  private
    FInternalCriticalSection: TCriticalSection;
    FLockCount: integer;
    FLockOwningThreadID: TThreadID;
    FName, FCurrentCodeSegmentName: string;
    FUseTimeoutLocking: boolean;

    // Fixed-depth stack for nested locks to avoid any heap allocations on Enter/Leave
    FLockOwnerStack: array[0..31] of string;
    FHoldStartStack: array[0..31] of Int64;

    // Performance and monitoring metrics
    FHoldStartUs: Int64;
    FMaxHoldUs: Int64;
    FTotalHoldUs: Int64;
    FMaxWaitUs: Int64;
    FTotalWaitUs: Int64;
    FContentionCount: Int64;
    FLockCountTotal: Int64;
    FLastWaitOwner: string;

    // Detailed breakdown per caller (populated when glUseTimer is true)
    FWaitTimesDict: TDictionary<string, Double>;
    FHoldTimesDict: TDictionary<string, Double>;
    FLockCountDict: TDictionary<string, Integer>;

    function GetCurrentLockOwnerName: string;
    procedure RecordWaitStats(const aLockOwnerName: string; const aWaitMs: Double);
    procedure RecordHoldStats(const aLockOwnerName: string; const aHoldMs: Double);
    procedure _OnAcquired(const aLockOwnerName: string; const aWaitUs: Int64);
  public
    { Constructor.
      @param(aName A unique name for this critical section. If another instance with the same name already exists, it will be disambiguated with a unique suffix.)
      @param(aAlwaysUseTimeoutLocking Set to true if this instance should always use the timeout locking feature, even if not enabled globally.) }
    constructor Create(aName: string; const aAlwaysUseTimeoutLocking: boolean = False);
    destructor Destroy; override;

    { Returns an existing TslCriticalSection2 if there already exists one with the same name. Creates a new TslCriticalSection2 otherwise.
      Use with caution to not produce deadlocks. Only use if you know what you are doing!
      @param(aName The unique name for the critical section.) }
    class function GetOrCreate(const aName: string): TslCriticalSection2;

    { Acquire lock.
      @param(aLockOwnerName A unique name for the code which invokes this function. This is for debugging and performance monitoring purposes.)
      @param(aTimeoutMs The timeout for how long should be waited to acquire the lock in ms.)
      @param(aRaiseExceptionOnFail Raise an exception if the lock could not be acquired within the timeout limit rather than just returning false.)
      @returns(True if the lock has been acquired, false otherwise.) }
    function Enter(const aLockOwnerName: string; const aTimeoutMs: Cardinal; const aRaiseExceptionOnFail: boolean = True): boolean; overload;

    { Acquire lock with default timeout.
      @param(aLockOwnerName A unique name for the code which invokes this function. This is for debugging and performance monitoring purposes.)
      @returns(True if the lock has been acquired, false otherwise.) }
    function Enter(const aLockOwnerName: string): boolean; overload;

    { Leaves the previously acquired lock }
    procedure Leave;

    { Set information about which code is currently being executed while holding this lock. This is for (performance) debugging purposes. }
    procedure SetCurrentCodeSegment(const aSegmentName: string);

    { Returns the name of the code part that is currently executing while holding this lock. }
    property CurrentLockOwnerName: string read GetCurrentLockOwnerName;
    property Name: string read FName;
    property ContentionCount: Int64 read FContentionCount;
    property MaxWaitUs: Int64 read FMaxWaitUs;
    property TotalWaitUs: Int64 read FTotalWaitUs;
    property MaxHoldUs: Int64 read FMaxHoldUs;
    property TotalHoldUs: Int64 read FTotalHoldUs;
    property LockCountTotal: Int64 read FLockCountTotal;
    property LastWaitOwner: string read FLastWaitOwner;
  end;

  { Initialize this unit.
    @param(aLockingTimeout Set the default locking timeout in ms.)
    @param(aUseTimer Set to true if per-caller hold and wait time breakdowns should be recorded.) }
  procedure SlCriticalSection2Init(const aLockingTimeout: integer; const aUseTimer: Boolean);

  { Uninitialize this unit - free all resources. }
  procedure SlCriticalSection2Uninit;

  { Returns true if timeout locking is enabled globally, false otherwise. }
  function GetUseTimeoutLocking: boolean;

  { Returns true if wait times are being recorded, false otherwise. }
  function GetUseTimer: boolean;

  { Writes all wait and hold times of locks into a log file at the path of slftp executable and returns that path. }
  function WriteCriticalSection2StatsToFile: String;

  { Returns a formatted summary string of the top contended locks for live inspection (e.g. via IRC). }
  function GetCriticalSection2Summary(const aTopN: Integer = 5): String;

implementation

uses
  SysUtils, Classes, Math, mormot.core.os, debugunit;

// These types are used for timer log output
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
  glDefaultLockingTimeout: integer = 60000;
  glUsedCriticalSections: TDictionary<string, TslCriticalSection2>;
  glUsedCriticalSectionsLock: TCriticalSection;
  glDebugSection: string = 'slcriticalsection2';
  glIsInitialized: boolean = False;

procedure SlCriticalSection2Init(const aLockingTimeout: integer; const aUseTimer: Boolean);
var
  fCs: TslCriticalSection2;
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
  else
  begin
    glUsedCriticalSectionsLock.Enter;
    try
      for fCs in glUsedCriticalSections.Values do
      begin
        if glUseTimer and (fCs.FWaitTimesDict = nil) then
        begin
          fCs.FWaitTimesDict := TDictionary<string, Double>.Create;
          fCs.FHoldTimesDict := TDictionary<string, Double>.Create;
          fCs.FLockCountDict := TDictionary<string, Integer>.Create;
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

constructor TslCriticalSection2.Create(aName: string; const aAlwaysUseTimeoutLocking: boolean = False);
var
  fBaseName: string;
  fCounter: Integer;
begin
  if not glIsInitialized then
    SlCriticalSection2Init(60000, True);

  aName := aName.Replace('\', '_'); // backslash not allowed on windows

  glUsedCriticalSectionsLock.Enter;
  try
    if glUsedCriticalSections.ContainsKey(aName) then
    begin
      fBaseName := aName;
      fCounter := 1;
      while glUsedCriticalSections.ContainsKey(aName) do
      begin
        Inc(fCounter);
        aName := Format('%s_%d', [fBaseName, fCounter]);
      end;
      Debug(dpError, glDebugSection, Format('Duplicate critical section name "%s" detected! Auto-disambiguated to "%s"', [fBaseName, aName]));
    end;
    glUsedCriticalSections.Add(aName, self);
  finally
    glUsedCriticalSectionsLock.Leave;
  end;

  FName := aName;
  FInternalCriticalSection := TCriticalSection.Create;
  FUseTimeoutLocking := glUseTimeoutLocking or aAlwaysUseTimeoutLocking;
  FLockCount := 0;
  FLockOwningThreadID := 0;
  FCurrentCodeSegmentName := '';
  FHoldStartUs := 0;
  FMaxHoldUs := 0;
  FTotalHoldUs := 0;
  FMaxWaitUs := 0;
  FTotalWaitUs := 0;
  FContentionCount := 0;
  FLockCountTotal := 0;
  FLastWaitOwner := '';

  if glUseTimer then
  begin
    FWaitTimesDict := TDictionary<string, Double>.Create;
    FHoldTimesDict := TDictionary<string, Double>.Create;
    FLockCountDict := TDictionary<string, Integer>.Create;
  end;
end;

class function TslCriticalSection2.GetOrCreate(const aName: string): TslCriticalSection2;
begin
  if not glIsInitialized then
    raise Exception.Create('TslCriticalSection2 system not initialized!');

  glUsedCriticalSectionsLock.Enter;
  try
    if not glUsedCriticalSections.TryGetValue(aName, Result) then
      Result := TslCriticalSection2.Create(aName);
  finally
    glUsedCriticalSectionsLock.Leave;
  end;
end;

destructor TslCriticalSection2.Destroy;
begin
  if glUsedCriticalSectionsLock <> nil then
  begin
    glUsedCriticalSectionsLock.Enter;
    try
      if glUsedCriticalSections <> nil then
        glUsedCriticalSections.Remove(FName);
    finally
      glUsedCriticalSectionsLock.Leave;
    end;
  end;

  FreeAndNil(FInternalCriticalSection);
  FreeAndNil(FWaitTimesDict);
  FreeAndNil(FHoldTimesDict);
  FreeAndNil(FLockCountDict);

  inherited Destroy;
end;

function TslCriticalSection2.GetCurrentLockOwnerName: string;
begin
  Result := '';
  try
    if FLockCount > 0 then
    begin
      if FLockCount <= 32 then
        Result := FLockOwnerStack[FLockCount - 1]
      else
        Result := FLockOwnerStack[31];
    end;
  except
    Result := '<unknown>';
  end;
end;

procedure TslCriticalSection2.RecordWaitStats(const aLockOwnerName: string; const aWaitMs: Double);
var
  fVal: Double;
  fCnt: Integer;
begin
  if not glUseTimer or (FWaitTimesDict = nil) then
    exit;

  if not FWaitTimesDict.TryGetValue(aLockOwnerName, fVal) then
    FWaitTimesDict.Add(aLockOwnerName, aWaitMs)
  else
    FWaitTimesDict[aLockOwnerName] := fVal + aWaitMs;

  if not FLockCountDict.TryGetValue(aLockOwnerName, fCnt) then
    FLockCountDict.Add(aLockOwnerName, 1)
  else
    FLockCountDict[aLockOwnerName] := fCnt + 1;
end;

procedure TslCriticalSection2.RecordHoldStats(const aLockOwnerName: string; const aHoldMs: Double);
var
  fVal: Double;
begin
  if not glUseTimer or (FHoldTimesDict = nil) then
    exit;

  if not FHoldTimesDict.TryGetValue(aLockOwnerName, fVal) then
    FHoldTimesDict.Add(aLockOwnerName, aHoldMs)
  else
    FHoldTimesDict[aLockOwnerName] := fVal + aHoldMs;
end;

procedure TslCriticalSection2._OnAcquired(const aLockOwnerName: string; const aWaitUs: Int64);
var
  fNowUs: Int64;
  fCurThreadId: TThreadID;
begin
  QueryPerformanceMicroSeconds(fNowUs);
  fCurThreadId := GetCurrentThreadId;

  if (FLockOwningThreadID = 0) or (FLockOwningThreadID <> fCurThreadId) then
  begin
    FLockOwningThreadID := fCurThreadId;
    FLockCount := 1;
    FLockOwnerStack[0] := aLockOwnerName;
    FHoldStartStack[0] := fNowUs;
  end
  else
  begin
    // Recursive enter by the same thread
    if FLockCount < 32 then
    begin
      FLockOwnerStack[FLockCount] := aLockOwnerName;
      FHoldStartStack[FLockCount] := fNowUs;
    end;
    Inc(FLockCount);
  end;

  Inc(FLockCountTotal);

  if aWaitUs > 0 then
  begin
    Inc(FContentionCount);
    Inc(FTotalWaitUs, aWaitUs);
    if aWaitUs > FMaxWaitUs then
    begin
      FMaxWaitUs := aWaitUs;
      FLastWaitOwner := aLockOwnerName;
    end;

    if glUseTimer then
      RecordWaitStats(aLockOwnerName, aWaitUs / 1000.0);

    if (aWaitUs > 50000) and (FName <> 'debug_lock') then
    begin
      Debug(dpMessage, glDebugSection, Format('[LOCK DELAY] %s (%s): waited %d us (%d ms) to acquire lock',
        [FName, aLockOwnerName, aWaitUs, aWaitUs div 1000]));
    end;
  end
  else if glUseTimer then
  begin
    RecordWaitStats(aLockOwnerName, 0);
  end;
end;

function TslCriticalSection2.Enter(const aLockOwnerName: string): boolean;
begin
  Result := self.Enter(aLockOwnerName, glDefaultLockingTimeout);
end;

function TslCriticalSection2.Enter(const aLockOwnerName: string; const aTimeoutMs: Cardinal; const aRaiseExceptionOnFail: boolean = True): boolean;
var
  fStartWaitUs, fNowUs, fWaitUs, fTimeoutUs: Int64;
  fSpinCount: Integer;
  fEffectiveTimeoutMs: Cardinal;
begin
  // Fast path: try to acquire immediately (uncontended: ~10ns)
  if FInternalCriticalSection.TryEnter then
  begin
    _OnAcquired(aLockOwnerName, 0);
    Result := True;
    exit;
  end;

  // Contended path: determine effective timeout
  if aTimeoutMs > 0 then
    fEffectiveTimeoutMs := aTimeoutMs
  else if FUseTimeoutLocking then
    fEffectiveTimeoutMs := glDefaultLockingTimeout
  else
    fEffectiveTimeoutMs := 0;

  if fEffectiveTimeoutMs > 0 then
    fTimeoutUs := Int64(fEffectiveTimeoutMs) * 1000
  else
    fTimeoutUs := 0;

  QueryPerformanceMicroSeconds(fStartWaitUs);
  fSpinCount := 0;

  while True do
  begin
    if fSpinCount < 40 then
    begin
      Inc(fSpinCount);
      {$IFDEF FPC}
      ThreadSwitch;
      {$ELSE}
      TThread.Yield;
      {$ENDIF}
    end
    else
      Sleep(1);

    if FInternalCriticalSection.TryEnter then
    begin
      QueryPerformanceMicroSeconds(fNowUs);
      fWaitUs := fNowUs - fStartWaitUs;
      _OnAcquired(aLockOwnerName, fWaitUs);
      Result := True;
      exit;
    end;

    if fTimeoutUs > 0 then
    begin
      QueryPerformanceMicroSeconds(fNowUs);
      if (fNowUs - fStartWaitUs) >= fTimeoutUs then
      begin
        // Deadlock / Timeout detected!
        if aRaiseExceptionOnFail then
        begin
          raise Exception.Create(Format('Unable to acquire lock ''%s'' (%s) by thread %s within %d ms. Lock is held by thread %s (%d) - %s (%s)',
            [FName, aLockOwnerName, IntToHex(GetCurrentThreadId, 4), fEffectiveTimeoutMs,
             IntToHex(FLockOwningThreadID, 4), FLockCount, CurrentLockOwnerName, FCurrentCodeSegmentName]));
        end;
        Result := False;
        exit;
      end;
    end;
  end;
end;

procedure TslCriticalSection2.Leave;
var
  fLockOwnerName: string;
  fNowUs, fHoldUs, fHoldStart: Int64;
begin
  if FLockOwningThreadID = 0 then
    raise Exception.Create(Format('Trying to leave lock "%s" by thread %s but it has not been entered before', [FName, IntToHex(GetCurrentThreadId, 4)]));

  if FLockOwningThreadID <> GetCurrentThreadId then
    raise Exception.Create(Format('Trying to leave lock "%s" by thread %s but it is held by thread %s (%d) - %s', [FName, IntToHex(GetCurrentThreadId, 4), IntToHex(FLockOwningThreadID, 4), FLockCount, CurrentLockOwnerName]));

  QueryPerformanceMicroSeconds(fNowUs);

  Dec(FLockCount);
  if FLockCount < 32 then
  begin
    fLockOwnerName := FLockOwnerStack[FLockCount];
    FLockOwnerStack[FLockCount] := '';
    fHoldStart := FHoldStartStack[FLockCount];
  end
  else
  begin
    fLockOwnerName := '';
    fHoldStart := fNowUs;
  end;

  if FLockCount = 0 then
  begin
    FLockOwningThreadID := 0;
    FCurrentCodeSegmentName := '';
  end;

  try
    fHoldUs := fNowUs - fHoldStart;
    if fHoldUs > FMaxHoldUs then
      FMaxHoldUs := fHoldUs;
    Inc(FTotalHoldUs, fHoldUs);

    if glUseTimer and (fLockOwnerName <> '') then
      RecordHoldStats(fLockOwnerName, fHoldUs / 1000.0);

    if (fHoldUs > 50000) and (FName <> 'debug_lock') then
      Debug(dpMessage, glDebugSection, Format('[LOCK HELD TOO LONG] %s (%s): held %d us (%d ms)', [FName, fLockOwnerName, fHoldUs, fHoldUs div 1000]));
  finally
    FInternalCriticalSection.Leave;
  end;
end;

procedure TslCriticalSection2.SetCurrentCodeSegment(const aSegmentName: string);
begin
  if FLockOwningThreadID = GetCurrentThreadId then
    FCurrentCodeSegmentName := aSegmentName;
end;

function GetUseTimeoutLocking: boolean;
begin
  Result := glUseTimeoutLocking;
end;

function GetUseTimer: boolean;
begin
  Result := glUseTimer;
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
    if (aDict = nil) or (aCountsDict = nil) then
      exit;

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

        fOutput.Add(Format('    %s: total=%.3f ms, count=%d, avg=%.3f ms', [fSubPair.Key, fSubPair.Value, fCount, fAvg]));
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
        if fEntry.CriticalSection.FWaitTimesDict <> nil then
        begin
          for fWPair in fEntry.CriticalSection.FWaitTimesDict do
            fEntry.WaitSum := fEntry.WaitSum + fWPair.Value;
        end
        else
          fEntry.WaitSum := fEntry.CriticalSection.TotalWaitUs / 1000.0;
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
      fOutput.Add(Format('  Contention Count: %d, Max Wait: %.3f ms, Max Hold: %.3f ms, Total Wait: %.3f ms',
        [fEntry.CriticalSection.ContentionCount, fEntry.CriticalSection.MaxWaitUs / 1000.0,
         fEntry.CriticalSection.MaxHoldUs / 1000.0, fEntry.WaitSum]));

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

function _SummarySorter({$IFDEF FPC}constref{$ELSE}const{$ENDIF} Left, Right: TslCriticalSection2): Integer;
begin
  if Left.TotalWaitUs <> Right.TotalWaitUs then
    Result := CompareValue(Right.TotalWaitUs, Left.TotalWaitUs)
  else if Left.ContentionCount <> Right.ContentionCount then
    Result := CompareValue(Right.ContentionCount, Left.ContentionCount)
  else
    Result := CompareValue(Right.MaxHoldUs, Left.MaxHoldUs);
end;

function GetCriticalSection2Summary(const aTopN: Integer = 5): String;
var
  fList: TList<TslCriticalSection2>;
  fCs: TslCriticalSection2;
  fCount, i, fLimit: Integer;
  fOutput: TStringList;
begin
  Result := '';
  if not glIsInitialized or (glUsedCriticalSections = nil) then
  begin
    Result := 'Critical sections not initialized.';
    exit;
  end;

  fList := TList<TslCriticalSection2>.Create;
  fOutput := TStringList.Create;
  try
    glUsedCriticalSectionsLock.Enter;
    try
      for fCs in glUsedCriticalSections.Values do
        fList.Add(fCs);
      fCount := fList.Count;
    finally
      glUsedCriticalSectionsLock.Leave;
    end;

    fList.Sort(TComparer<TslCriticalSection2>.Construct(_SummarySorter));

    fOutput.Add(Format('Lock Summary: %d active locks.', [fCount]));
    fLimit := Min(aTopN, fList.Count);
    for i := 0 to fLimit - 1 do
    begin
      fCs := fList[i];
      if (fCs.ContentionCount > 0) or (fCs.MaxHoldUs > 10000) then
      begin
        fOutput.Add(Format('#%d %s: contention=%d, max_wait=%.2fms, total_wait=%.2fms, max_hold=%.2fms, last_waiter=%s',
          [i + 1, fCs.Name, fCs.ContentionCount, fCs.MaxWaitUs / 1000.0, fCs.TotalWaitUs / 1000.0, fCs.MaxHoldUs / 1000.0, fCs.LastWaitOwner]));
      end
      else
      begin
        fOutput.Add(Format('#%d %s: contention=0, max_hold=%.2fms, total_locks=%d',
          [i + 1, fCs.Name, fCs.MaxHoldUs / 1000.0, fCs.LockCountTotal]));
      end;
    end;

    Result := fOutput.Text;
  finally
    fList.Free;
    fOutput.Free;
  end;
end;

end.
