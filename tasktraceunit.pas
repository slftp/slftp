unit tasktraceunit;

interface

uses
  Classes, SysUtils, SyncObjs;

type
  { One traced task execution. }
  TTaskTraceEntry = record
    StartTime: TDateTime;
    EndTime: TDateTime;
    TaskType: String;
    TaskName: String;
    TaskUid: String;
    SiteName: String;
    SlotName: String;
    DstSiteName: String;
    DstSlotName: String;
    Success: Boolean;
  end;

  { In-memory ring buffer for near-real-time task execution tracing. }
  TTaskTraceLog = class
  private
    FBuffer: array of TTaskTraceEntry;
    FCapacity: Integer;
    FCount: Integer;
    FHead: Integer;
    FEnabled: Boolean;
    FLock: TCriticalSection;

    FTotalCounter: Int64;
    FRaceCounter: Int64;
    FDirlistCounter: Int64;
    FMkdirCounter: Int64;
    FWaitCounter: Int64;
    FOtherCounter: Int64;

    function FormatEntry(const aEntry: TTaskTraceEntry): String;
    procedure InternalAdd(const aEntry: TTaskTraceEntry; out aIndex: Integer);
  public
    constructor Create(const aCapacity: Integer);
    destructor Destroy; override;

    property Enabled: Boolean read FEnabled write FEnabled;

    { Classifies a task class name into one of the traced categories. }
    class function TaskTypeFromClassName(const aClassName: String): String;

    { Starts tracing a task. Returns the buffer index used to finish it later. }
    function TraceStart(const aClassName, aTaskName, aTaskUid, aSiteName, aSlotName: String): Integer; overload;
    function TraceStart(const aClassName, aTaskName, aTaskUid, aSiteName, aSlotName, aDstSiteName, aDstSlotName: String): Integer; overload;

    { Finishes tracing a previously started task. }
    procedure TraceFinish(const aIndex: Integer; const aSuccess: Boolean);

    { Returns the most recent entries as a multi-line string (newest last). }
    function GetRecent(const aCount: Integer): String;

    { Returns aggregated counters per task type. }
    function GetStats: String;
  end;

var
  GlTaskTrace: TTaskTraceLog;

implementation

uses
  DateUtils;

{ TTaskTraceLog }

constructor TTaskTraceLog.Create(const aCapacity: Integer);
begin
  inherited Create;
  FCapacity := aCapacity;
  if FCapacity < 1 then
    FCapacity := 1;
  SetLength(FBuffer, FCapacity);
  FCount := 0;
  FHead := 0;
  FEnabled := True;
  FLock := TCriticalSection.Create;

  FTotalCounter := 0;
  FRaceCounter := 0;
  FDirlistCounter := 0;
  FMkdirCounter := 0;
  FWaitCounter := 0;
  FOtherCounter := 0;
end;

destructor TTaskTraceLog.Destroy;
begin
  FLock.Free;
  SetLength(FBuffer, 0);
  inherited Destroy;
end;

class function TTaskTraceLog.TaskTypeFromClassName(const aClassName: String): String;
begin
  if AnsiSameText(aClassName, 'TPazoRaceTask') then
    Result := 'race'
  else if AnsiSameText(aClassName, 'TPazoDirlistTask') or
          AnsiSameText(aClassName, 'TDirlistTask') then
    Result := 'dirlist'
  else if AnsiSameText(aClassName, 'TPazoMkdirTask') then
    Result := 'mkdir'
  else if AnsiSameText(aClassName, 'TWaitTask') then
    Result := 'wait'
  else
    Result := 'other';
end;

function TTaskTraceLog.FormatEntry(const aEntry: TTaskTraceEntry): String;
var
  fDurationMs: Int64;
  fEndStr: String;
begin
  if aEntry.EndTime > 0 then
  begin
    fDurationMs := MilliSecondsBetween(aEntry.StartTime, aEntry.EndTime);
    fEndStr := FormatDateTime('hh:nn:ss.zzz', aEntry.EndTime);
  end
  else
  begin
    fDurationMs := -1;
    fEndStr := 'running';
  end;

  Result := Format('%s-%s [%s] %s/%s',
    [FormatDateTime('hh:nn:ss.zzz', aEntry.StartTime),
     fEndStr,
     aEntry.TaskType,
     aEntry.SiteName,
     aEntry.SlotName]);

  if (aEntry.DstSiteName <> '') or (aEntry.DstSlotName <> '') then
    Result := Result + Format(' -> %s/%s', [aEntry.DstSiteName, aEntry.DstSlotName]);

  Result := Result + Format(' uid=%s ok=%s dur=%dms',
    [aEntry.TaskUid,
     BoolToStr(aEntry.Success, True),
     fDurationMs]);

  if aEntry.TaskName <> '' then
    Result := Result + ' ' + aEntry.TaskName;
end;

procedure TTaskTraceLog.InternalAdd(const aEntry: TTaskTraceEntry; out aIndex: Integer);
begin
  FLock.Enter;
  try
    aIndex := FHead;
    FBuffer[aIndex] := aEntry;
    FHead := (FHead + 1) mod FCapacity;
    if FCount < FCapacity then
      Inc(FCount);

    Inc(FTotalCounter);
    if aEntry.TaskType = 'race' then
      Inc(FRaceCounter)
    else if aEntry.TaskType = 'dirlist' then
      Inc(FDirlistCounter)
    else if aEntry.TaskType = 'mkdir' then
      Inc(FMkdirCounter)
    else if aEntry.TaskType = 'wait' then
      Inc(FWaitCounter)
    else
      Inc(FOtherCounter);
  finally
    FLock.Leave;
  end;
end;

function TTaskTraceLog.TraceStart(const aClassName, aTaskName, aTaskUid, aSiteName, aSlotName: String): Integer;
begin
  Result := TraceStart(aClassName, aTaskName, aTaskUid, aSiteName, aSlotName, '', '');
end;

function TTaskTraceLog.TraceStart(const aClassName, aTaskName, aTaskUid, aSiteName, aSlotName, aDstSiteName, aDstSlotName: String): Integer;
var
  fEntry: TTaskTraceEntry;
  fTaskType: String;
  fSafeTaskName: String;
  fSafeTaskUid: String;
begin
  Result := -1;
  if not FEnabled then
    Exit;

  fSafeTaskName := aTaskName;
  if fSafeTaskName = '' then
    fSafeTaskName := '?';

  fSafeTaskUid := aTaskUid;
  if fSafeTaskUid = '' then
    fSafeTaskUid := '?';

  fTaskType := TaskTypeFromClassName(aClassName);

  fEntry.StartTime := Now;
  fEntry.EndTime := 0;
  fEntry.TaskType := fTaskType;
  fEntry.TaskName := fSafeTaskName;
  fEntry.TaskUid := fSafeTaskUid;
  fEntry.SiteName := aSiteName;
  fEntry.SlotName := aSlotName;
  fEntry.DstSiteName := aDstSiteName;
  fEntry.DstSlotName := aDstSlotName;
  fEntry.Success := False;

  InternalAdd(fEntry, Result);
end;

procedure TTaskTraceLog.TraceFinish(const aIndex: Integer; const aSuccess: Boolean);
begin
  if not FEnabled then
    Exit;
  if (aIndex < 0) or (aIndex >= FCapacity) then
    Exit;

  FLock.Enter;
  try
    // Only update if this slot still belongs to our entry. Because the buffer
    // is a ring, the index may have been overwritten if the buffer is very
    // small and the task ran for a very long time. In that case we simply
    // ignore the finish call.
    if FBuffer[aIndex].EndTime = 0 then
    begin
      FBuffer[aIndex].EndTime := Now;
      FBuffer[aIndex].Success := aSuccess;
    end;
  finally
    FLock.Leave;
  end;
end;

function TTaskTraceLog.GetRecent(const aCount: Integer): String;
var
  fLines: TStringList;
  fRequested: Integer;
  fActualCount: Integer;
  fStartIdx: Integer;
  i: Integer;
  fIdx: Integer;
begin
  Result := '';
  if aCount <= 0 then
    Exit;

  fLines := TStringList.Create;
  try
    FLock.Enter;
    try
      fRequested := aCount;
      if fRequested > FCapacity then
        fRequested := FCapacity;

      fActualCount := FCount;
      if fActualCount > fRequested then
        fActualCount := fRequested;

      if fActualCount <= 0 then
      begin
        Result := 'No task trace entries yet.';
        Exit;
      end;

      // FHead points to the next free slot, so the newest entry is at
      // (FHead - 1) mod FCapacity.
      fStartIdx := (FHead - fActualCount + FCapacity) mod FCapacity;
      for i := 0 to fActualCount - 1 do
      begin
        fIdx := (fStartIdx + i) mod FCapacity;
        fLines.Add(FormatEntry(FBuffer[fIdx]));
      end;
    finally
      FLock.Leave;
    end;

    Result := fLines.Text;
  finally
    fLines.Free;
  end;
end;

function TTaskTraceLog.GetStats: String;
begin
  FLock.Enter;
  try
    Result := Format('Task trace stats: total=%d race=%d dirlist=%d mkdir=%d wait=%d other=%d',
      [FTotalCounter, FRaceCounter, FDirlistCounter, FMkdirCounter, FWaitCounter, FOtherCounter]);
  finally
    FLock.Leave;
  end;
end;

end.
