unit diagunit;

{
  In-memory diagnostics for queue/slot/WAITTASK analysis.
  This unit is intentionally kept dependency-free from queueunit, sitesunit
  and taskrace to avoid circular uses. Other units call the DiagRecord*
  helpers at the points where state changes happen.

  Metrics are kept both globally (GlDiagCurrent) and per site so that
  !queuediag <sitename> can show site-specific numbers.
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
    Freeslots: Integer;
    NumUp: Integer;
    MaxUp: Integer;
    NumDn: Integer;
    MaxDn: Integer;
    UpCooldown: Integer;
    DnCooldown: Integer;
  end;

  TDiagQueueSnapshot = record
    TotalTasks: Integer;
    RaceTasks: Integer;
    DirlistTasks: Integer;
    AutoTasks: Integer;
    OtherTasks: Integer;
    RaceTasksAssigned: Integer;
    DirlistsDone: Int64;
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

  { Per-site metrics entry. }
  TDiagSiteEntry = record
    Name: String;
    Metrics: TDiagMetrics;
  end;

  { Simple ring buffer of recent snapshots. }
  TDiagHistory = array[0..119] of TDiagMetrics;

const
  CDiagHistorySize = 120; // 120 * 30s = 1h

var
  GlDiagCS: TslCriticalSection2;
  GlDiagCurrent: TDiagMetrics;
  GlDiagSites: array of TDiagSiteEntry;
  GlDiagHistory: TDiagHistory;
  GlDiagHistoryIndex: Integer;
  GlDiagHistoryCount: Integer;

procedure DiagInit;
procedure DiagUninit;

{ WAITTASK lifecycle. Optional aSiteName updates per-site counters too. }
procedure DiagRecordWaitTaskCreated(const aSiteName: String = '');
procedure DiagRecordWaitTaskAssigned(const aSiteName: String = '');
procedure DiagRecordWaitTaskDone(const aElapsedMs: Int64; const aSiteName: String = '');

{ Queue scanning / assignment. Optional aSiteName updates per-site counters too. }
procedure DiagRecordFindBestTaskNil(const aReason: TDiagFindBestNilReason; const aSiteName: String = '');
procedure DiagRecordFindBestTaskCall(const aSiteName: String = '');
procedure DiagRecordAssignRaceAbort(const aReason: TDiagAssignAbortReason; const aSiteName: String = '');
procedure DiagRecordAssignSlotsAbort(const aReason: TDiagAssignAbortReason; const aSiteName: String = '');
procedure DiagRecordRaceTaskAssigned(const aSiteName: String = '');

{ Snapshots. Optional aSiteName updates per-site snapshot too. }
procedure DiagUpdateQueueSnapshot(const aTotal, aRace, aDirlist, aAuto, aOther: Integer;
  const aRaceAssigned: Integer; const aDirlistsDone: Int64; const aSiteName: String = '');
function DiagGetRaceTasksAssigned(const aSiteName: String = ''): Int64;
procedure DiagUpdateSlotSnapshot(const aOnline, aOffline, aDown, aMarkedDown,
  aBusy, aFree, aWaitTaskBusy, aFreeslots, aNumUp, aMaxUp, aNumDn, aMaxDn,
  aUpCooldown, aDnCooldown: Integer; const aSiteName: String = '');
procedure DiagTakeSnapshot;

{ Per-site lookup helpers }
function DiagSiteIndex(const aSiteName: String): Integer;
function DiagSiteExists(const aSiteName: String): Boolean;
function DiagSiteNames: TStringList;

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
function DiagFormatCurrent(const aSiteName: String = ''): String;
function DiagFormatHistory: String;
function DiagFormatActiveWaitTasks: String;
function DiagSaveToFile(const aFilename: String): Boolean;

implementation

{ Helpers to manage per-site metrics }

function DiagSiteIndex(const aSiteName: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  if aSiteName = '' then
    Exit;
  for i := 0 to High(GlDiagSites) do
    if SameText(GlDiagSites[i].Name, aSiteName) then
    begin
      Result := i;
      Exit;
    end;
end;

function DiagEnsureSiteIndex(const aSiteName: String): Integer;
var
  idx: Integer;
begin
  idx := DiagSiteIndex(aSiteName);
  if idx < 0 then
  begin
    idx := Length(GlDiagSites);
    SetLength(GlDiagSites, idx + 1);
    GlDiagSites[idx].Name := aSiteName;
    FillChar(GlDiagSites[idx].Metrics, SizeOf(GlDiagSites[idx].Metrics), 0);
  end;
  Result := idx;
end;

function DiagSiteExists(const aSiteName: String): Boolean;
begin
  Result := DiagSiteIndex(aSiteName) >= 0;
end;

function DiagSiteNames: TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  GlDiagCS.Enter('DiagSiteNames');
  try
    for i := 0 to High(GlDiagSites) do
      Result.Add(GlDiagSites[i].Name);
  finally
    GlDiagCS.Leave;
  end;
end;

procedure DiagInit;
begin
  GlDiagCS := TslCriticalSection2.Create('diagunit');
  FillChar(GlDiagCurrent, SizeOf(GlDiagCurrent), 0);
  SetLength(GlDiagSites, 0);
  FillChar(GlDiagHistory, SizeOf(GlDiagHistory), 0);
  GlDiagHistoryIndex := 0;
  GlDiagHistoryCount := 0;
  SetLength(GlDiagActiveWaitTasks, 0);
end;

procedure DiagUninit;
begin
  SetLength(GlDiagActiveWaitTasks, 0);
  SetLength(GlDiagSites, 0);
  FreeAndNil(GlDiagCS);
end;

{ WAITTASK lifecycle }

procedure _UpdateWaitTaskDone(var aSnap: TDiagWaitTaskSnapshot; const aElapsedMs: Int64);
begin
  Dec(aSnap.ActiveNow);
  Inc(aSnap.DoneTotal);

  if aElapsedMs > aSnap.PeakWaitMs then
    aSnap.PeakWaitMs := aElapsedMs;

  if aSnap.DoneTotal = 1 then
    aSnap.AvgWaitMs := aElapsedMs
  else
    aSnap.AvgWaitMs :=
      (aSnap.AvgWaitMs * (aSnap.DoneTotal - 1) + aElapsedMs)
      div aSnap.DoneTotal;
end;

procedure DiagRecordWaitTaskCreated(const aSiteName: String = '');
var
  idx: Integer;
begin
  GlDiagCS.Enter('DiagRecordWaitTaskCreated');
  try
    Inc(GlDiagCurrent.WaitTasks.CreatedTotal);
    if aSiteName <> '' then
    begin
      idx := DiagEnsureSiteIndex(aSiteName);
      Inc(GlDiagSites[idx].Metrics.WaitTasks.CreatedTotal);
    end;
  finally
    GlDiagCS.Leave;
  end;
end;

procedure DiagRecordWaitTaskAssigned(const aSiteName: String = '');
var
  idx: Integer;
begin
  GlDiagCS.Enter('DiagRecordWaitTaskAssigned');
  try
    Inc(GlDiagCurrent.WaitTasks.ActiveNow);
    if aSiteName <> '' then
    begin
      idx := DiagEnsureSiteIndex(aSiteName);
      Inc(GlDiagSites[idx].Metrics.WaitTasks.ActiveNow);
    end;
  finally
    GlDiagCS.Leave;
  end;
end;

procedure DiagRecordWaitTaskDone(const aElapsedMs: Int64; const aSiteName: String = '');
var
  idx: Integer;
begin
  GlDiagCS.Enter('DiagRecordWaitTaskDone');
  try
    _UpdateWaitTaskDone(GlDiagCurrent.WaitTasks, aElapsedMs);
    if aSiteName <> '' then
    begin
      idx := DiagEnsureSiteIndex(aSiteName);
      _UpdateWaitTaskDone(GlDiagSites[idx].Metrics.WaitTasks, aElapsedMs);
    end;
  finally
    GlDiagCS.Leave;
  end;
end;

{ Queue scanning / assignment }

procedure _IncFindBestNilReason(var aSnap: TDiagQueueSnapshot; const aReason: TDiagFindBestNilReason);
begin
  case aReason of
    dfbnNoTasks: Inc(aSnap.FindBestNilNoTasks);
    dfbnNoSlots: Inc(aSnap.FindBestNilNoSlots);
    dfbnCooldown: Inc(aSnap.FindBestNilCooldown);
    dfbnDelayed: Inc(aSnap.FindBestNilDelayed);
    dfbnNoReadyTask: Inc(aSnap.FindBestNilNoReadyTask);
    dfbnOther: Inc(aSnap.FindBestNilOther);
  end;
end;

procedure DiagRecordFindBestTaskNil(const aReason: TDiagFindBestNilReason; const aSiteName: String = '');
var
  idx: Integer;
begin
  GlDiagCS.Enter('DiagRecordFindBestTaskNil');
  try
    _IncFindBestNilReason(GlDiagCurrent.Queue, aReason);
    if aSiteName <> '' then
    begin
      idx := DiagEnsureSiteIndex(aSiteName);
      _IncFindBestNilReason(GlDiagSites[idx].Metrics.Queue, aReason);
    end;
  finally
    GlDiagCS.Leave;
  end;
end;

procedure DiagRecordFindBestTaskCall(const aSiteName: String = '');
var
  idx: Integer;
begin
  GlDiagCS.Enter('DiagRecordFindBestTaskCall');
  try
    Inc(GlDiagCurrent.Queue.FindBestTaskCalls);
    if aSiteName <> '' then
    begin
      idx := DiagEnsureSiteIndex(aSiteName);
      Inc(GlDiagSites[idx].Metrics.Queue.FindBestTaskCalls);
    end;
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

procedure DiagRecordAssignRaceAbort(const aReason: TDiagAssignAbortReason; const aSiteName: String = '');
var
  idx: Integer;
begin
  if aReason = darNone then Exit;
  GlDiagCS.Enter('DiagRecordAssignRaceAbort');
  try
    DiagIncReasonCounter(GlDiagCurrent.AssignRace, aReason);
    if aSiteName <> '' then
    begin
      idx := DiagEnsureSiteIndex(aSiteName);
      DiagIncReasonCounter(GlDiagSites[idx].Metrics.AssignRace, aReason);
    end;
  finally
    GlDiagCS.Leave;
  end;
end;

procedure DiagRecordAssignSlotsAbort(const aReason: TDiagAssignAbortReason; const aSiteName: String = '');
var
  idx: Integer;
begin
  if aReason = darNone then Exit;
  GlDiagCS.Enter('DiagRecordAssignSlotsAbort');
  try
    DiagIncReasonCounter(GlDiagCurrent.AssignSlots, aReason);
    if aSiteName <> '' then
    begin
      idx := DiagEnsureSiteIndex(aSiteName);
      DiagIncReasonCounter(GlDiagSites[idx].Metrics.AssignSlots, aReason);
    end;
  finally
    GlDiagCS.Leave;
  end;
end;

procedure DiagRecordRaceTaskAssigned(const aSiteName: String = '');
var
  idx: Integer;
begin
  GlDiagCS.Enter('DiagRecordRaceTaskAssigned');
  try
    Inc(GlDiagCurrent.Queue.RaceTasksAssigned);
    if aSiteName <> '' then
    begin
      idx := DiagEnsureSiteIndex(aSiteName);
      Inc(GlDiagSites[idx].Metrics.Queue.RaceTasksAssigned);
    end;
  finally
    GlDiagCS.Leave;
  end;
end;

function DiagGetRaceTasksAssigned(const aSiteName: String = ''): Int64;
var
  idx: Integer;
begin
  GlDiagCS.Enter('DiagGetRaceTasksAssigned');
  try
    if aSiteName = '' then
      Result := GlDiagCurrent.Queue.RaceTasksAssigned
    else
    begin
      idx := DiagSiteIndex(aSiteName);
      if idx >= 0 then
        Result := GlDiagSites[idx].Metrics.Queue.RaceTasksAssigned
      else
        Result := 0;
    end;
  finally
    GlDiagCS.Leave;
  end;
end;

procedure DiagUpdateQueueSnapshot(const aTotal, aRace, aDirlist, aAuto, aOther: Integer;
  const aRaceAssigned: Integer; const aDirlistsDone: Int64; const aSiteName: String = '');
var
  idx: Integer;
  procedure _Update(var aSnap: TDiagQueueSnapshot);
  begin
    aSnap.TotalTasks := aTotal;
    aSnap.RaceTasks := aRace;
    aSnap.DirlistTasks := aDirlist;
    aSnap.AutoTasks := aAuto;
    aSnap.OtherTasks := aOther;
    aSnap.RaceTasksAssigned := aRaceAssigned;
    aSnap.DirlistsDone := aDirlistsDone;
  end;
begin
  GlDiagCS.Enter('DiagUpdateQueueSnapshot');
  try
    _Update(GlDiagCurrent.Queue);
    if aSiteName <> '' then
    begin
      idx := DiagEnsureSiteIndex(aSiteName);
      _Update(GlDiagSites[idx].Metrics.Queue);
    end;
  finally
    GlDiagCS.Leave;
  end;
end;

procedure DiagUpdateSlotSnapshot(const aOnline, aOffline, aDown, aMarkedDown,
  aBusy, aFree, aWaitTaskBusy, aFreeslots, aNumUp, aMaxUp, aNumDn, aMaxDn,
  aUpCooldown, aDnCooldown: Integer; const aSiteName: String = '');
var
  idx: Integer;
  procedure _Update(var aSnap: TDiagSlotSnapshot);
  begin
    aSnap.Online := aOnline;
    aSnap.Offline := aOffline;
    aSnap.Down := aDown;
    aSnap.MarkedDown := aMarkedDown;
    aSnap.Busy := aBusy;
    aSnap.Free := aFree;
    aSnap.WaitTaskBusy := aWaitTaskBusy;
    aSnap.Freeslots := aFreeslots;
    aSnap.NumUp := aNumUp;
    aSnap.MaxUp := aMaxUp;
    aSnap.NumDn := aNumDn;
    aSnap.MaxDn := aMaxDn;
    aSnap.UpCooldown := aUpCooldown;
    aSnap.DnCooldown := aDnCooldown;
  end;
begin
  GlDiagCS.Enter('DiagUpdateSlotSnapshot');
  try
    _Update(GlDiagCurrent.Slots);
    if aSiteName <> '' then
    begin
      idx := DiagEnsureSiteIndex(aSiteName);
      _Update(GlDiagSites[idx].Metrics.Slots);
    end;
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

function DiagFormatMetrics(const m: TDiagMetrics): String;
const
  FMT = '[DIAG] WAITTASKS active=%d created=%d done=%d avg=%dms peak=%dms stuck>5s=%d stuck>30s=%d' + sLineBreak +
        '[DIAG] QUEUE total=%d race=%d dirlist=%d auto=%d other=%d assigned=%d dirlists_done=%d fbt_calls=%d' + sLineBreak +
        '[DIAG] SLOTS online=%d offline=%d down=%d markeddown=%d busy=%d free=%d wait_busy=%d freeslots=%d up=%d/%d dn=%d/%d up_cd=%ds dn_cd=%ds' + sLineBreak +
        '[DIAG] RACE-ABORT freeslots=%d maxsim_up=%d maxsim_down=%d no_slot=%d offline=%d not_ready=%d maxupperrip=%d other=%d' + sLineBreak +
        '[DIAG] SLOT-ABORT freeslots=%d maxsim_up=%d maxsim_down=%d no_slot=%d offline=%d not_ready=%d maxupperrip=%d other=%d' + sLineBreak +
        '[DIAG] FBT-NIL no_tasks=%d no_slots=%d cooldown=%d delayed=%d no_ready=%d other=%d';
begin
  Result := Format(FMT,
    [m.WaitTasks.ActiveNow, m.WaitTasks.CreatedTotal, m.WaitTasks.DoneTotal,
     m.WaitTasks.AvgWaitMs, m.WaitTasks.PeakWaitMs, m.WaitTasks.StuckOver5s,
     m.WaitTasks.StuckOver30s,
     m.Queue.TotalTasks, m.Queue.RaceTasks, m.Queue.DirlistTasks,
     m.Queue.AutoTasks, m.Queue.OtherTasks, m.Queue.RaceTasksAssigned,
     m.Queue.DirlistsDone, m.Queue.FindBestTaskCalls,
     m.Slots.Online, m.Slots.Offline, m.Slots.Down, m.Slots.MarkedDown,
     m.Slots.Busy, m.Slots.Free, m.Slots.WaitTaskBusy,
     m.Slots.Freeslots, m.Slots.NumUp, m.Slots.MaxUp, m.Slots.NumDn, m.Slots.MaxDn,
     m.Slots.UpCooldown, m.Slots.DnCooldown,
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
end;

function DiagFormatCurrent(const aSiteName: String = ''): String;
var
  m: TDiagMetrics;
  idx: Integer;
begin
  Result := '';
  GlDiagCS.Enter('DiagFormatCurrent');
  try
    if aSiteName = '' then
    begin
      m := GlDiagCurrent;
    end
    else
    begin
      idx := DiagSiteIndex(aSiteName);
      if idx < 0 then
      begin
        Result := Format('[DIAG] No diagnostic data for site %s yet.', [aSiteName]);
        Exit;
      end;
      m := GlDiagSites[idx].Metrics;
    end;
    Result := DiagFormatMetrics(m);
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
  i: Integer;
begin
  Result := False;
  AssignFile(f, aFilename);
  try
    Rewrite(f);
    try
      WriteLn(f, '--- global ---');
      WriteLn(f, DiagFormatCurrent);
      WriteLn(f, sLineBreak + '--- per site ---');
      GlDiagCS.Enter('DiagSaveToFile');
      try
        for i := 0 to High(GlDiagSites) do
        begin
          WriteLn(f, sLineBreak + '--- site: ' + GlDiagSites[i].Name + ' ---');
          WriteLn(f, DiagFormatMetrics(GlDiagSites[i].Metrics));
        end;
      finally
        GlDiagCS.Leave;
      end;
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
