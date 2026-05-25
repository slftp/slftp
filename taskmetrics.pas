unit taskmetrics;

interface

uses
  Classes, SysUtils, DateUtils, Math, Generics.Collections, slcriticalsection2;

type
  { @abstract(Task type identifiers for metrics classification) }
  TMetricsTaskType = (mttRace, mttDirlist, mttDirlistFull, mttDirlistSuccess, mttMkdir, mttOther);

  { @abstract(Single timing event stored in the ring buffer) }
  TTaskTimingEvent = packed record
    Timestamp: TDateTime;
    TaskType: TMetricsTaskType;
    Site: array[0..31] of AnsiChar;
    PazoID: Int32;
    DurationMs: Int32;
    QueueWaitMs: Int32;
    Dir: array[0..63] of AnsiChar;
    Error: Boolean;
  end;

  { @abstract(Rolling aggregate statistics per task type + site) }
  TTaskTimingAggregate = record
    Count: UInt64;
    TotalMs: UInt64;
    MinMs: UInt32;
    MaxMs: UInt32;
    TotalQueueWaitMs: UInt64;
    MaxQueueWaitMs: UInt32;
  end;

  { @abstract(Tracks gaps between consecutive dirlist executions per release/site/dir) }
  TDirlistGapTracker = record
    LastEndTime: TDateTime;
    TotalGapMs: UInt64;
    GapCount: UInt32;
    MaxGapMs: UInt32;
    MinGapMs: UInt32;
  end;

  { @abstract(In-memory collector for task execution timing metrics.
    Thread-safe, no file I/O, bounded memory usage. ) }
  TTaskMetricsCollector = class
  private
    FLock: TSlCriticalSection2;
    FRingSize: Integer;
    FRing: array of TTaskTimingEvent;
    FWriteIdx: Integer;
    FAggregates: TDictionary<string, TTaskTimingAggregate>;
    FGaps: TDictionary<string, TDirlistGapTracker>;

    function MakeAggregateKey(const aTaskType: TMetricsTaskType; const aSite: String): String;
    function MakeGapKey(const aPazoID: Integer; const aSite, aDir: String): String;
    procedure UpdateAggregate(const aKey: String; const aDurationMs, aQueueWaitMs: Int32);
    procedure UpdateGap(const aKey: String; const aEndTime: TDateTime);
    procedure RecordGapStart(const aKey: String; const aStartTime: TDateTime);
  public
    constructor Create(const aRingSize: Integer = 8192);
    destructor Destroy; override;

    procedure RecordTaskEvent(const aTaskType: TMetricsTaskType; const aSiteName: String;
      const aPazoID: Integer; const aDir: String; const aDurationMs, aQueueWaitMs: Int32;
      const aError: Boolean);
    procedure RecordDirlistEnd(const aPazoID: Integer; const aSite, aDir: String);
    procedure RecordDirlistStart(const aPazoID: Integer; const aSite, aDir: String);

    function GetAggregate(const aTaskType: TMetricsTaskType; const aSite: String): TTaskTimingAggregate;
    procedure GetAllAggregates(out aList: TArray<TPair<string, TTaskTimingAggregate>>);
    function GetLastEvents(const aCount: Integer): TArray<TTaskTimingEvent>;
    function GetDirlistGap(const aPazoID: Integer; const aSite, aDir: String): TDirlistGapTracker;
    procedure GetAllGaps(out aList: TArray<TPair<string, TDirlistGapTracker>>);

    procedure Reset;
  end;

{ @abstract(Global singleton instance — initialized in initialization section) }
function GetTaskMetrics: TTaskMetricsCollector;

implementation

var
  GlTaskMetrics: TTaskMetricsCollector;

function GetTaskMetrics: TTaskMetricsCollector;
begin
  Result := GlTaskMetrics;
end;

{ TTaskMetricsCollector }

constructor TTaskMetricsCollector.Create(const aRingSize: Integer);
begin
  inherited Create;
  FLock := TSlCriticalSection2.Create('TaskMetrics');
  FRingSize := aRingSize;
  SetLength(FRing, aRingSize);
  FWriteIdx := 0;
  FAggregates := TDictionary<string, TTaskTimingAggregate>.Create;
  FGaps := TDictionary<string, TDirlistGapTracker>.Create;
end;

destructor TTaskMetricsCollector.Destroy;
begin
  FAggregates.Free;
  FGaps.Free;
  FLock.Free;
  inherited Destroy;
end;

function TTaskMetricsCollector.MakeAggregateKey(const aTaskType: TMetricsTaskType;
  const aSite: String): String;
begin
  case aTaskType of
    mttRace:            Result := 'RACE|' + aSite;
    mttDirlist:         Result := 'DIRLIST|' + aSite;
    mttDirlistFull:     Result := 'DIRLISTFULL|' + aSite;
    mttDirlistSuccess:  Result := 'DIRLISTSUCCESS|' + aSite;
    mttMkdir:           Result := 'MKDIR|' + aSite;
    mttOther:           Result := 'OTHER|' + aSite;
  end;
end;

function TTaskMetricsCollector.MakeGapKey(const aPazoID: Integer;
  const aSite, aDir: String): String;
begin
  Result := IntToStr(aPazoID) + '|' + aSite + '|' + aDir;
end;

procedure TTaskMetricsCollector.UpdateAggregate(const aKey: String;
  const aDurationMs, aQueueWaitMs: Int32);
var
  fAgg: TTaskTimingAggregate;
begin
  if not FAggregates.TryGetValue(aKey, fAgg) then
  begin
    FillChar(fAgg, SizeOf(fAgg), 0);
    fAgg.MinMs := High(UInt32);
  end;

  Inc(fAgg.Count);
  fAgg.TotalMs := fAgg.TotalMs + UInt64(aDurationMs);
  fAgg.TotalQueueWaitMs := fAgg.TotalQueueWaitMs + UInt64(aQueueWaitMs);

  if UInt32(aDurationMs) < fAgg.MinMs then
    fAgg.MinMs := UInt32(aDurationMs);
  if UInt32(aDurationMs) > fAgg.MaxMs then
    fAgg.MaxMs := UInt32(aDurationMs);

  if UInt32(aQueueWaitMs) > fAgg.MaxQueueWaitMs then
    fAgg.MaxQueueWaitMs := UInt32(aQueueWaitMs);

  FAggregates.AddOrSetValue(aKey, fAgg);
end;

procedure TTaskMetricsCollector.UpdateGap(const aKey: String;
  const aEndTime: TDateTime);
var
  fGap: TDirlistGapTracker;
  fGapMs: Int64;
begin
  if not FGaps.TryGetValue(aKey, fGap) then
  begin
    FillChar(fGap, SizeOf(fGap), 0);
    fGap.LastEndTime := aEndTime;
    fGap.MinGapMs := High(UInt32);
    FGaps.Add(aKey, fGap);
    Exit;
  end;

  if fGap.LastEndTime > 0 then
  begin
    fGapMs := MilliSecondsBetween(fGap.LastEndTime, aEndTime);
    if fGapMs < 0 then fGapMs := 0;

    Inc(fGap.GapCount);
    fGap.TotalGapMs := fGap.TotalGapMs + UInt64(fGapMs);

    if UInt32(fGapMs) < fGap.MinGapMs then
      fGap.MinGapMs := UInt32(fGapMs);
    if UInt32(fGapMs) > fGap.MaxGapMs then
      fGap.MaxGapMs := UInt32(fGapMs);
  end;

  fGap.LastEndTime := aEndTime;
  FGaps.AddOrSetValue(aKey, fGap);
end;

procedure TTaskMetricsCollector.RecordGapStart(const aKey: String;
  const aStartTime: TDateTime);
var
  fGap: TDirlistGapTracker;
  fGapMs: Int64;
begin
  if not FGaps.TryGetValue(aKey, fGap) then
  begin
    FillChar(fGap, SizeOf(fGap), 0);
    fGap.LastEndTime := aStartTime;
    fGap.MinGapMs := High(UInt32);
    FGaps.Add(aKey, fGap);
    Exit;
  end;

  if fGap.LastEndTime > 0 then
  begin
    fGapMs := MilliSecondsBetween(fGap.LastEndTime, aStartTime);
    if fGapMs < 0 then fGapMs := 0;

    Inc(fGap.GapCount);
    fGap.TotalGapMs := fGap.TotalGapMs + UInt64(fGapMs);

    if UInt32(fGapMs) < fGap.MinGapMs then
      fGap.MinGapMs := UInt32(fGapMs);
    if UInt32(fGapMs) > fGap.MaxGapMs then
      fGap.MaxGapMs := UInt32(fGapMs);
  end;

  fGap.LastEndTime := aStartTime;
  FGaps.AddOrSetValue(aKey, fGap);
end;

procedure TTaskMetricsCollector.RecordTaskEvent(const aTaskType: TMetricsTaskType;
  const aSiteName: String; const aPazoID: Integer; const aDir: String;
  const aDurationMs, aQueueWaitMs: Int32; const aError: Boolean);
var
  fEvent: TTaskTimingEvent;
  fKey: String;
begin
  FillChar(fEvent, SizeOf(fEvent), 0);
  fEvent.Timestamp := Now;
  fEvent.TaskType := aTaskType;
  fEvent.PazoID := aPazoID;
  fEvent.DurationMs := aDurationMs;
  fEvent.QueueWaitMs := aQueueWaitMs;
  fEvent.Error := aError;

  if Length(aSiteName) > 0 then
    Move(aSiteName[1], fEvent.Site[0], Min(Length(aSiteName), 31));
  if Length(aDir) > 0 then
    Move(aDir[1], fEvent.Dir[0], Min(Length(aDir), 63));

  FLock.Enter('RecordTaskEvent');
  try
    FRing[FWriteIdx] := fEvent;
    FWriteIdx := (FWriteIdx + 1) mod FRingSize;

    fKey := MakeAggregateKey(aTaskType, aSiteName);
    UpdateAggregate(fKey, aDurationMs, aQueueWaitMs);
  finally
    FLock.Leave;
  end;
end;

procedure TTaskMetricsCollector.RecordDirlistEnd(const aPazoID: Integer;
  const aSite, aDir: String);
var
  fKey: String;
begin
  fKey := MakeGapKey(aPazoID, aSite, aDir);
  FLock.Enter('RecordDirlistEnd');
  try
    UpdateGap(fKey, Now);
  finally
    FLock.Leave;
  end;
end;

procedure TTaskMetricsCollector.RecordDirlistStart(const aPazoID: Integer;
  const aSite, aDir: String);
var
  fKey: String;
begin
  fKey := MakeGapKey(aPazoID, aSite, aDir);
  FLock.Enter('RecordDirlistStart');
  try
    RecordGapStart(fKey, Now);
  finally
    FLock.Leave;
  end;
end;

function TTaskMetricsCollector.GetAggregate(const aTaskType: TMetricsTaskType;
  const aSite: String): TTaskTimingAggregate;
var
  fKey: String;
begin
  fKey := MakeAggregateKey(aTaskType, aSite);
  FLock.Enter('GetAggregate');
  try
    if not FAggregates.TryGetValue(fKey, Result) then
      FillChar(Result, SizeOf(Result), 0);
  finally
    FLock.Leave;
  end;
end;

procedure TTaskMetricsCollector.GetAllAggregates(
  out aList: TArray<TPair<string, TTaskTimingAggregate>>);
begin
  FLock.Enter('GetAllAggregates');
  try
    aList := FAggregates.ToArray;
  finally
    FLock.Leave;
  end;
end;

function TTaskMetricsCollector.GetLastEvents(const aCount: Integer): TArray<TTaskTimingEvent>;
var
  fResultCount: Integer;
  fI: Integer;
  fSrcIdx: Integer;
begin
  fResultCount := Min(aCount, FRingSize);
  SetLength(Result, fResultCount);

  FLock.Enter('GetLastEvents');
  try
    for fI := 0 to fResultCount - 1 do
    begin
      fSrcIdx := (FWriteIdx - 1 - fI + FRingSize) mod FRingSize;
      Result[fI] := FRing[fSrcIdx];
    end;
  finally
    FLock.Leave;
  end;
end;

function TTaskMetricsCollector.GetDirlistGap(const aPazoID: Integer;
  const aSite, aDir: String): TDirlistGapTracker;
var
  fKey: String;
begin
  fKey := MakeGapKey(aPazoID, aSite, aDir);
  FLock.Enter('GetDirlistGap');
  try
    if not FGaps.TryGetValue(fKey, Result) then
      FillChar(Result, SizeOf(Result), 0);
  finally
    FLock.Leave;
  end;
end;

procedure TTaskMetricsCollector.GetAllGaps(
  out aList: TArray<TPair<string, TDirlistGapTracker>>);
begin
  FLock.Enter('GetAllGaps');
  try
    aList := FGaps.ToArray;
  finally
    FLock.Leave;
  end;
end;

procedure TTaskMetricsCollector.Reset;
begin
  FLock.Enter('Reset');
  try
    FWriteIdx := 0;
    FillChar(FRing[0], Length(FRing) * SizeOf(TTaskTimingEvent), 0);
    FAggregates.Clear;
    FGaps.Clear;
  finally
    FLock.Leave;
  end;
end;

initialization
  GlTaskMetrics := TTaskMetricsCollector.Create(8192);

finalization
  GlTaskMetrics.Free;

end.
