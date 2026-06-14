unit diagunit;

{
  In-memory diagnostics for queue/slot/WAITTASK analysis.
  This unit is intentionally kept dependency-free from queueunit, sitesunit
  and taskrace to avoid circular uses. Other units call the DiagRecord*
  helpers at the points where state changes happen.
}

interface

uses
  SysUtils, Classes, DateUtils, slcriticalsection2;

type
  { Reasons why TryToAssignRaceSlots / TryToAssignSlots aborted. }
  TDiagAssignAbortReason = (
    darNone,
    darFreeslotsZero,
    darMaxSimUpCooldown,
    darMaxSimDownCooldown,
    darNoSlotAvailable,
    darSiteOffline,
    darTaskNotReady,
    darMaxUpPerRip,
    darOther
  );

  { Reasons why FindBestTask returned nil. }
  TDiagFindBestNilReason = (
    dfbnNone,
    dfbnNoTasks,
    dfbnNoSlots,
    dfbnCooldown,
    dfbnDelayed,
    dfbnNoReadyTask,
    dfbnOther
  );

  TDiagWaitTaskSnapshot = record
    ActiveNow: Integer;
    CreatedTotal: Int64;
    DoneTotal: Int64;
    AvgWaitMs: Int64;
    PeakWaitMs: Int64;
    StuckOver5s: Integer;
    StuckOver30s: Integer;
  end;

  TDiagSlotSnapshot = record
    Online: Integer;
    Offline: Integer;
    Down: Integer;
    MarkedDown: Integer;
    Busy: Integer;
    Free: Integer;
    WaitTaskBusy: Integer;
  end;

  TDiagQueueSnapshot = record
    TotalTasks: Integer;
    RaceTasks: Integer;
    DirlistTasks: Integer;
    AutoTasks: Integer;
    OtherTasks: Integer;
    RaceTasksAssigned: Integer;
    FindBestTaskCalls: Int64;
    FindBestNilNoTasks: Int64;
    FindBestNilNoSlots: Int64;
    FindBestNilCooldown: Int64;
    FindBestNilDelayed: Int64;
    FindBestNilNoReadyTask: Int64;
    FindBestNilOther: Int64;
  end;

  TDiagReasonCounters = record
    FreeslotsZero: Int64;
    MaxSimUpCooldown: Int64;
    MaxSimDownCooldown: Int64;
    NoSlotAvailable: Int64;
    SiteOffline: Int64;
    TaskNotReady: Int64;
    MaxUpPerRip: Int64;
    Other: Int64;
  end;

  { One full snapshot of the diagnostics state. }
  TDiagMetrics = record
    Timestamp: TDateTime;
    WaitTasks: TDiagWaitTaskSnapshot;
    Slots: TDiagSlotSnapshot;
    Queue: TDiagQueueSnapshot;
    AssignRace: TDiagReasonCounters;
    AssignSlots: TDiagReasonCounters;
  end;

  { Simple ring buffer of recent snapshots. }
  TDiagHistory = array[0..119] of TDiagMetrics;

const
  CDiagHistorySize = 120; // 120 * 30s = 1h

var
  GlDiagCS: TslCriticalSection2;
  GlDiagCurrent: TDiagMetrics;
  GlDiagHistory: TDiagHistory;
  GlDiagHistoryIndex: Integer;
  GlDiagHistoryCount: Integer;

procedure DiagInit;
procedure DiagUninit;

{ WAITTASK lifecycle }
procedure DiagRecordWaitTaskCreated;
procedure DiagRecordWaitTaskAssigned;
procedure DiagRecordWaitTaskDone(const aElapsedMs: Int64);

{ Queue scanning / assignment }
procedure DiagRecordFindBestTaskNil(const aReason: TDiagFindBestNilReason);
procedure DiagRecordFindBestTaskCall;
procedure DiagRecordAssignRaceAbort(const aReason: TDiagAssignAbortReason);
procedure DiagRecordAssignSlotsAbort(const aReason: TDiagAssignAbortReason);
procedure DiagRecordRaceTaskAssigned;

{ Snapshots }
procedure DiagUpdateQueueSnapshot(const aTotal, aRace, aDirlist, aAuto, aOther: Integer;
  const aRaceAssigned: Integer);
function DiagGetRaceTasksAssigned: Int64;
procedure DiagUpdateSlotSnapshot(const aOnline, aOffline, aDown, aMarkedDown,
  aBusy, aFree, aWaitTaskBusy: Integer);
procedure DiagTakeSnapshot;

{ Detail: active WAITTASK list }
type
  TDiagWaitTaskDetail = record
    SiteName: String;
    WaitFor: String;
    StartTime: TDateTime;
    Ready: Boolean;
    WaitDone: Boolean;
  end;
  TDiagWaitTaskDetails = array of TDiagWaitTaskDetail;

{ These are populated by taskrace.pas / queueunit.pas when WAITTASKs start/stop.
  Protected by GlDiagCS. }
var
  GlDiagActiveWaitTasks: array of TDiagWaitTaskDetail;

procedure DiagAddActiveWaitTask(const aSiteName, aWaitFor: String; const aStartTime: TDateTime);
procedure DiagUpdateActiveWaitTask(const aSiteName, aWaitFor: String;
  const aReady, aWaitDone: Boolean);
procedure DiagRemoveActiveWaitTask(const aSiteName, aWaitFor: String);

{ Output helpers }
function DiagFormatCurrent: String;
function DiagFormatHistory: String;
function DiagFormatActiveWaitTasks: String;
function DiagSaveToFile(const aFilename: String): Boolean;

implementation

procedure DiagInit;
begin
  GlDiagCS := TslCriticalSection2.Create('diagunit');
  FillChar(GlDiagCurrent, SizeOf(GlDiagCurrent), 0);
  FillChar(GlDiagHistory, SizeOf(GlDiagHistory), 0);
  GlDiagHistoryIndex := 0;
  GlDiagHistoryCount := 0;
  SetLength(GlDiagActiveWaitTasks, 0);
end;

procedure DiagUninit;
begin
  SetLength(GlDiagActiveWaitTasks, 0);
  FreeAndNil(GlDiagCS);
end;

procedure DiagRecordWaitTaskCreated;
begin
  GlDiagCS.Enter('DiagRecordWaitTaskCreated');
  try
    Inc(GlDiagCurrent.WaitTasks.CreatedTotal);
  finally
    GlDiagCS.Leave;
  end;
end;

procedure DiagRecordWaitTaskAssigned;
begin
  GlDiagCS.Enter('DiagRecordWaitTaskAssigned');
  try
    Inc(GlDiagCurrent.WaitTasks.ActiveNow);
  finally
    GlDiagCS.Leave;
  end;
end;

procedure DiagRecordWaitTaskDone(const aElapsedMs: Int64);
begin
  GlDiagCS.Enter('DiagRecordWaitTaskDone');
  try
    Dec(GlDiagCurrent.WaitTasks.ActiveNow);
    Inc(GlDiagCurrent.WaitTasks.DoneTotal);

    if aElapsedMs > GlDiagCurrent.WaitTasks.PeakWaitMs then
      GlDiagCurrent.WaitTasks.PeakWaitMs := aElapsedMs;

    { Running average: avg = (avg * (n-1) + new) / n }
    if GlDiagCurrent.WaitTasks.DoneTotal = 1 then
      GlDiagCurrent.WaitTasks.AvgWaitMs := aElapsedMs
    else
      GlDiagCurrent.WaitTasks.AvgWaitMs :=
        (GlDiagCurrent.WaitTasks.AvgWaitMs * (GlDiagCurrent.WaitTasks.DoneTotal - 1) + aElapsedMs)
        div GlDiagCurrent.WaitTasks.DoneTotal;
  finally
    GlDiagCS.Leave;
  end;
end;

procedure DiagRecordFindBestTaskNil(const aReason: TDiagFindBestNilReason);
begin
  GlDiagCS.Enter('DiagRecordFindBestTaskNil');
  try
    case aReason of
      dfbnNoTasks: Inc(GlDiagCurrent.Queue.FindBestNilNoTasks);
      dfbnNoSlots: Inc(GlDiagCurrent.Queue.FindBestNilNoSlots);
      dfbnCooldown: Inc(GlDiagCurrent.Queue.FindBestNilCooldown);
      dfbnDelayed: Inc(GlDiagCurrent.Queue.FindBestNilDelayed);
      dfbnNoReadyTask: Inc(GlDiagCurrent.Queue.FindBestNilNoReadyTask);
      dfbnOther: Inc(GlDiagCurrent.Queue.FindBestNilOther);
    end;
  finally
    GlDiagCS.Leave;
  end;
end;

procedure DiagRecordFindBestTaskCall;
begin
  GlDiagCS.Enter('DiagRecordFindBestTaskCall');
  try
    Inc(GlDiagCurrent.Queue.FindBestTaskCalls);
  finally
    GlDiagCS.Leave;
  end;
end;

procedure DiagIncReasonCounter(var aCounters: TDiagReasonCounters;
  const aReason: TDiagAssignAbortReason);
begin
  case aReason of
    darFreeslotsZero: Inc(aCounters.FreeslotsZero);
    darMaxSimUpCooldown: Inc(aCounters.MaxSimUpCooldown);
    darMaxSimDownCooldown: Inc(aCounters.MaxSimDownCooldown);
    darNoSlotAvailable: Inc(aCounters.NoSlotAvailable);
    darSiteOffline: Inc(aCounters.SiteOffline);
    darTaskNotReady: Inc(aCounters.TaskNotReady);
    darMaxUpPerRip: Inc(aCounters.MaxUpPerRip);
    darOther: Inc(aCounters.Other);
  end;
end;

procedure DiagRecordAssignRaceAbort(const aReason: TDiagAssignAbortReason);
begin
  if aReason = darNone then Exit;
  GlDiagCS.Enter('DiagRecordAssignRaceAbort');
  try
    DiagIncReasonCounter(GlDiagCurrent.AssignRace, aReason);
  finally
    GlDiagCS.Leave;
  end;
end;

procedure DiagRecordAssignSlotsAbort(const aReason: TDiagAssignAbortReason);
begin
  if aReason = darNone then Exit;
  GlDiagCS.Enter('DiagRecordAssignSlotsAbort');
  try
    DiagIncReasonCounter(GlDiagCurrent.AssignSlots, aReason);
  finally
    GlDiagCS.Leave;
  end;
end;

procedure DiagRecordRaceTaskAssigned;
begin
  GlDiagCS.Enter('DiagRecordRaceTaskAssigned');
  try
    Inc(GlDiagCurrent.Queue.RaceTasksAssigned);
  finally
    GlDiagCS.Leave;
  end;
end;

function DiagGetRaceTasksAssigned: Int64;
begin
  GlDiagCS.Enter('DiagGetRaceTasksAssigned');
  try
    Result := GlDiagCurrent.Queue.RaceTasksAssigned;
  finally
    GlDiagCS.Leave;
  end;
end;

procedure DiagUpdateQueueSnapshot(const aTotal, aRace, aDirlist, aAuto, aOther: Integer;
  const aRaceAssigned: Integer);
begin
  GlDiagCS.Enter('DiagUpdateQueueSnapshot');
  try
    GlDiagCurrent.Queue.TotalTasks := aTotal;
    GlDiagCurrent.Queue.RaceTasks := aRace;
    GlDiagCurrent.Queue.DirlistTasks := aDirlist;
    GlDiagCurrent.Queue.AutoTasks := aAuto;
    GlDiagCurrent.Queue.OtherTasks := aOther;
    GlDiagCurrent.Queue.RaceTasksAssigned := aRaceAssigned;
  finally
    GlDiagCS.Leave;
  end;
end;

procedure DiagUpdateSlotSnapshot(const aOnline, aOffline, aDown, aMarkedDown,
  aBusy, aFree, aWaitTaskBusy: Integer);
begin
  GlDiagCS.Enter('DiagUpdateSlotSnapshot');
  try
    GlDiagCurrent.Slots.Online := aOnline;
    GlDiagCurrent.Slots.Offline := aOffline;
    GlDiagCurrent.Slots.Down := aDown;
    GlDiagCurrent.Slots.MarkedDown := aMarkedDown;
    GlDiagCurrent.Slots.Busy := aBusy;
    GlDiagCurrent.Slots.Free := aFree;
    GlDiagCurrent.Slots.WaitTaskBusy := aWaitTaskBusy;

    { Recalculate stuck counters from active detail list }
    GlDiagCurrent.WaitTasks.StuckOver5s := 0;
    GlDiagCurrent.WaitTasks.StuckOver30s := 0;
    // Note: caller updates stuck counters via DiagUpdateActiveWaitTask if desired.
  finally
    GlDiagCS.Leave;
  end;
end;

procedure DiagTakeSnapshot;
begin
  GlDiagCS.Enter('DiagTakeSnapshot');
  try
    GlDiagCurrent.Timestamp := Now;
    GlDiagHistory[GlDiagHistoryIndex] := GlDiagCurrent;
    GlDiagHistoryIndex := (GlDiagHistoryIndex + 1) mod CDiagHistorySize;
    if GlDiagHistoryCount < CDiagHistorySize then
      Inc(GlDiagHistoryCount);
  finally
    GlDiagCS.Leave;
  end;
end;

{ Active WAITTASK detail list }

procedure DiagAddActiveWaitTask(const aSiteName, aWaitFor: String; const aStartTime: TDateTime);
var
  i: Integer;
begin
  GlDiagCS.Enter('DiagAddActiveWaitTask');
  try
    for i := 0 to High(GlDiagActiveWaitTasks) do
      if (GlDiagActiveWaitTasks[i].SiteName = aSiteName) and
         (GlDiagActiveWaitTasks[i].WaitFor = aWaitFor) then
        Exit; // already tracked

    SetLength(GlDiagActiveWaitTasks, Length(GlDiagActiveWaitTasks) + 1);
    with GlDiagActiveWaitTasks[High(GlDiagActiveWaitTasks)] do
    begin
      SiteName := aSiteName;
      WaitFor := aWaitFor;
      StartTime := aStartTime;
      Ready := False;
      WaitDone := False;
    end;
  finally
    GlDiagCS.Leave;
  end;
end;

procedure DiagUpdateActiveWaitTask(const aSiteName, aWaitFor: String;
  const aReady, aWaitDone: Boolean);
var
  i: Integer;
begin
  GlDiagCS.Enter('DiagUpdateActiveWaitTask');
  try
    for i := 0 to High(GlDiagActiveWaitTasks) do
      if (GlDiagActiveWaitTasks[i].SiteName = aSiteName) and
         (GlDiagActiveWaitTasks[i].WaitFor = aWaitFor) then
      begin
        GlDiagActiveWaitTasks[i].Ready := aReady;
        GlDiagActiveWaitTasks[i].WaitDone := aWaitDone;
        Exit;
      end;
  finally
    GlDiagCS.Leave;
  end;
end;

procedure DiagRemoveActiveWaitTask(const aSiteName, aWaitFor: String);
var
  i, j: Integer;
begin
  GlDiagCS.Enter('DiagRemoveActiveWaitTask');
  try
    for i := 0 to High(GlDiagActiveWaitTasks) do
      if (GlDiagActiveWaitTasks[i].SiteName = aSiteName) and
         (GlDiagActiveWaitTasks[i].WaitFor = aWaitFor) then
      begin
        for j := i to High(GlDiagActiveWaitTasks) - 1 do
          GlDiagActiveWaitTasks[j] := GlDiagActiveWaitTasks[j + 1];
        SetLength(GlDiagActiveWaitTasks, Length(GlDiagActiveWaitTasks) - 1);
        Exit;
      end;
  finally
    GlDiagCS.Leave;
  end;
end;

{ Output helpers }

function DiagFormatCurrent: String;
const
  FMT = '[DIAG] WAITTASKS active=%d created=%d done=%d avg=%dms peak=%dms stuck>5s=%d stuck>30s=%d' + sLineBreak +
        '[DIAG] QUEUE total=%d race=%d dirlist=%d auto=%d other=%d assigned=%d fbt_calls=%d' + sLineBreak +
        '[DIAG] SLOTS online=%d offline=%d down=%d markeddown=%d busy=%d free=%d wait_busy=%d' + sLineBreak +
        '[DIAG] RACE-ABORT freeslots=%d maxsim_up=%d maxsim_down=%d no_slot=%d offline=%d not_ready=%d maxupperrip=%d other=%d' + sLineBreak +
        '[DIAG] SLOT-ABORT freeslots=%d maxsim_up=%d maxsim_down=%d no_slot=%d offline=%d not_ready=%d maxupperrip=%d other=%d' + sLineBreak +
        '[DIAG] FBT-NIL no_tasks=%d no_slots=%d cooldown=%d delayed=%d no_ready=%d other=%d';
var
  m: TDiagMetrics;
begin
  GlDiagCS.Enter('DiagFormatCurrent');
  try
    m := GlDiagCurrent;
    Result := Format(FMT,
      [m.WaitTasks.ActiveNow, m.WaitTasks.CreatedTotal, m.WaitTasks.DoneTotal,
       m.WaitTasks.AvgWaitMs, m.WaitTasks.PeakWaitMs, m.WaitTasks.StuckOver5s,
       m.WaitTasks.StuckOver30s,
       m.Queue.TotalTasks, m.Queue.RaceTasks, m.Queue.DirlistTasks,
       m.Queue.AutoTasks, m.Queue.OtherTasks, m.Queue.RaceTasksAssigned,
       m.Queue.FindBestTaskCalls,
       m.Slots.Online, m.Slots.Offline, m.Slots.Down, m.Slots.MarkedDown,
       m.Slots.Busy, m.Slots.Free, m.Slots.WaitTaskBusy,
       m.AssignRace.FreeslotsZero, m.AssignRace.MaxSimUpCooldown,
       m.AssignRace.MaxSimDownCooldown, m.AssignRace.NoSlotAvailable,
       m.AssignRace.SiteOffline, m.AssignRace.TaskNotReady,
       m.AssignRace.MaxUpPerRip, m.AssignRace.Other,
       m.AssignSlots.FreeslotsZero, m.AssignSlots.MaxSimUpCooldown,
       m.AssignSlots.MaxSimDownCooldown, m.AssignSlots.NoSlotAvailable,
       m.AssignSlots.SiteOffline, m.AssignSlots.TaskNotReady,
       m.AssignSlots.MaxUpPerRip, m.AssignSlots.Other,
       m.Queue.FindBestNilNoTasks, m.Queue.FindBestNilNoSlots,
       m.Queue.FindBestNilCooldown, m.Queue.FindBestNilDelayed,
       m.Queue.FindBestNilNoReadyTask, m.Queue.FindBestNilOther]);
  finally
    GlDiagCS.Leave;
  end;
end;

function DiagFormatHistory: String;
var
  i, idx: Integer;
  m: TDiagMetrics;
begin
  Result := '';
  GlDiagCS.Enter('DiagFormatHistory');
  try
    for i := 0 to GlDiagHistoryCount - 1 do
    begin
      idx := (GlDiagHistoryIndex - GlDiagHistoryCount + i + CDiagHistorySize) mod CDiagHistorySize;
      m := GlDiagHistory[idx];
      Result := Result + Format('[%s] wt=%d/%d slots=%d/%d/%d queue=%d/%d' + sLineBreak,
        [FormatDateTime('hh:nn:ss', m.Timestamp),
         m.WaitTasks.ActiveNow, m.WaitTasks.DoneTotal,
         m.Slots.Online, m.Slots.Busy, m.Slots.Free,
         m.Queue.TotalTasks, m.Queue.RaceTasksAssigned]);
    end;
  finally
    GlDiagCS.Leave;
  end;
end;

function DiagFormatActiveWaitTasks: String;
var
  i: Integer;
  elapsedSec: Double;
begin
  Result := '';
  GlDiagCS.Enter('DiagFormatActiveWaitTasks');
  try
    if Length(GlDiagActiveWaitTasks) = 0 then
    begin
      Result := '[DIAG] no active WAITTASKs';
      Exit;
    end;

    for i := 0 to High(GlDiagActiveWaitTasks) do
    begin
      elapsedSec := MilliSecondsBetween(Now, GlDiagActiveWaitTasks[i].StartTime) / 1000.0;
      Result := Result + Format('[DIAG] WAITTASK #%d site=%s wait_for=%s elapsed=%.1fs ready=%s done=%s' + sLineBreak,
        [i + 1,
         GlDiagActiveWaitTasks[i].SiteName,
         GlDiagActiveWaitTasks[i].WaitFor,
         elapsedSec,
         BoolToStr(GlDiagActiveWaitTasks[i].Ready, True),
         BoolToStr(GlDiagActiveWaitTasks[i].WaitDone, True)]);
    end;
  finally
    GlDiagCS.Leave;
  end;
end;

function DiagSaveToFile(const aFilename: String): Boolean;
var
  f: TextFile;
begin
  Result := False;
  AssignFile(f, aFilename);
  try
    Rewrite(f);
    try
      WriteLn(f, DiagFormatCurrent);
      WriteLn(f, sLineBreak + '--- active WAITTASKs ---');
      WriteLn(f, DiagFormatActiveWaitTasks);
      WriteLn(f, sLineBreak + '--- history ---');
      WriteLn(f, DiagFormatHistory);
      Result := True;
    finally
      CloseFile(f);
    end;
  except
    on E: Exception do
      Result := False;
  end;
end;

end.
