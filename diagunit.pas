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
    darDstMaxUp,
    darSrcMaxDn,
    darSrcNoFreeSlot,
    darDstNoFreeSlot,
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
    dfbnNoReadyTaskMkdir,
    dfbnNoReadyTaskOther,
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
    { FBT-NIL delayed/no_ready broken down by task type for debugging. }
    FindBestNilDelayedRace: Int64;
    FindBestNilDelayedDirlist: Int64;
    FindBestNilDelayedAuto: Int64;
    FindBestNilDelayedOther: Int64;
    FindBestNilNoReadyRace: Int64;
    FindBestNilNoReadyDirlist: Int64;
    FindBestNilNoReadyAuto: Int64;
    FindBestNilNoReadyOther: Int64;
    { Breakdown of no_ready race tasks. }
    FindBestNilNoReadyRaceMkdir: Int64;
    FindBestNilNoReadyRaceOther: Int64;
    { MKDIR task lifecycle counters. }
    MkdirTasksCreated: Int64;
    MkdirTasksDone: Int64;
    MkdirTasksFailed: Int64;
    { Current dirlists with need_mkdir and race tasks blocked by them. }
    NeedMkdirDirlists: Integer;
    RaceTasksWaitingOnMkdir: Integer;
    { need_mkdir lifecycle timing. }
    NeedMkdirClearCount: Int64;
    NeedMkdirClearTotalMs: Int64;
    NeedMkdirClearPeakMs: Int64;
    MkdirUnnecessaryCount: Int64;
  end;

  TDiagReasonCounters = record
    FreeslotsZero: Int64;
    MaxSimUpCooldown: Int64;
    MaxSimDownCooldown: Int64;
    NoSlotAvailable: Int64;
    DstMaxUp: Int64;
    SrcMaxDn: Int64;
    SrcNoFreeSlot: Int64;
    DstNoFreeSlot: Int64;
    SiteOffline: Int64;
    TaskNotReady: Int64;
    MaxUpPerRip: Int64;
    Other: Int64;
  end;

  TDiagDirlistWaitSnapshot = record
    Count: Int64;
    AvgMs: Int64;
    PeakMs: Int64;
  end;

  { One full snapshot of the diagnostics state. }
  TDiagMetrics = record
    Timestamp: TDateTime;
    WaitTasks: TDiagWaitTaskSnapshot;
    Slots: TDiagSlotSnapshot;
    Queue: TDiagQueueSnapshot;
    AssignRace: TDiagReasonCounters;
    AssignSlots: TDiagReasonCounters;
    DirlistWait: TDiagDirlistWaitSnapshot;
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

{ MKDIR task lifecycle. }
procedure DiagRecordMkdirTaskCreated(const aSiteName: String = '');
procedure DiagRecordMkdirTaskDone(const aFailed: Boolean; const aSiteName: String = '');
procedure DiagRecordMkdirUnnecessary(const aSiteName: String = '');

{ need_mkdir lifecycle timing. }
procedure DiagRecordNeedMkdirClear(const aElapsedMs: Int64; const aSiteName: String = '');

{ Queue scanning / assignment. Optional aSiteName updates per-site counters too. }
procedure DiagRecordFindBestTaskNil(const aReason: TDiagFindBestNilReason;
  const aTaskClassName: String = ''; const aSiteName: String = '');
  // aTaskClassName helps distinguish delayed/no_ready counters by task type.
procedure DiagRecordFindBestTaskCall(const aSiteName: String = '');
procedure DiagRecordAssignRaceAbort(const aReason: TDiagAssignAbortReason; const aSiteName: String = '');
procedure DiagRecordAssignSlotsAbort(const aReason: TDiagAssignAbortReason; const aSiteName: String = '');
procedure DiagRecordRaceTaskAssigned(const aSiteName: String = '');
procedure DiagRecordDirlistWait(const aWaitMs: Int64; const aSiteName: String = '');

{ Snapshots. Optional aSiteName updates per-site snapshot too. }
procedure DiagUpdateQueueSnapshot(const aTotal, aRace, aDirlist, aAuto, aOther: Integer;
  const aRaceAssigned: Integer; const aDirlistsDone: Int64; const aSiteName: String = '');
procedure DiagUpdateNeedMkdirStats(const aNeedMkdirDirlists, aRaceTasksWaiting: Integer;
  const aSiteName: String = '');
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

{ Recent abort details for dstmaxup / maxupperrip investigations. }
const
  CDiagDetailBufferSize = 20;

type
  TDiagDstMaxUpDetail = record
    Timestamp: TDateTime;
    SiteName: String;
    NumUp: Integer;
    MaxUp: Integer;
    ActualUploadingSlots: Integer;
    TaskName: String;
  end;

  TDiagMaxUpPerRipDetail = record
    Timestamp: TDateTime;
    SiteName: String;
    MaxUpPerRip: Integer;
    ActiveTransferCount: Integer;
    TaskName: String;
  end;

{ These are populated by taskrace.pas / queueunit.pas when WAITTASKs start/stop.
  Protected by GlDiagCS. }
var
  GlDiagActiveWaitTasks: array of TDiagWaitTaskDetail;

  GlDiagDstMaxUpDetails: array[0..CDiagDetailBufferSize - 1] of TDiagDstMaxUpDetail;
  GlDiagDstMaxUpDetailIndex: Integer;
  GlDiagDstMaxUpDetailCount: Integer;

  GlDiagMaxUpPerRipDetails: array[0..CDiagDetailBufferSize - 1] of TDiagMaxUpPerRipDetail;
  GlDiagMaxUpPerRipDetailIndex: Integer;
  GlDiagMaxUpPerRipDetailCount: Integer;

procedure DiagAddActiveWaitTask(const aSiteName, aWaitFor: String; const aStartTime: TDateTime);
procedure DiagUpdateActiveWaitTask(const aSiteName, aWaitFor: String;
  const aReady, aWaitDone: Boolean);
procedure DiagRemoveActiveWaitTask(const aSiteName, aWaitFor: String);

{ Recent-abort detail recorders. Called from queueunit.pas under main_lock. }
procedure DiagRecordDstMaxUpDetail(const aSiteName: String; const aNumUp, aMaxUp,
  aActualUploadingSlots: Integer; const aTaskName: String);
procedure DiagRecordMaxUpPerRipDetail(const aSiteName: String; const aMaxUpPerRip,
  aActiveTransferCount: Integer; const aTaskName: String);

{ Output helpers }
function DiagFormatCurrent(const aSiteName: String = ''): String;
function DiagFormatHistory: String;
function DiagFormatActiveWaitTasks: String;
function DiagFormatAbortDetails: String;
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
  FillChar(GlDiagDstMaxUpDetails, SizeOf(GlDiagDstMaxUpDetails), 0);
  GlDiagDstMaxUpDetailIndex := 0;
  GlDiagDstMaxUpDetailCount := 0;
  FillChar(GlDiagMaxUpPerRipDetails, SizeOf(GlDiagMaxUpPerRipDetails), 0);
  GlDiagMaxUpPerRipDetailIndex := 0;
  GlDiagMaxUpPerRipDetailCount := 0;
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

{ MKDIR task lifecycle }

procedure DiagRecordMkdirTaskCreated(const aSiteName: String = '');
var
  idx: Integer;
begin
  GlDiagCS.Enter('DiagRecordMkdirTaskCreated');
  try
    Inc(GlDiagCurrent.Queue.MkdirTasksCreated);
    if aSiteName <> '' then
    begin
      idx := DiagEnsureSiteIndex(aSiteName);
      Inc(GlDiagSites[idx].Metrics.Queue.MkdirTasksCreated);
    end;
  finally
    GlDiagCS.Leave;
  end;
end;

procedure DiagRecordMkdirTaskDone(const aFailed: Boolean; const aSiteName: String = '');
var
  idx: Integer;
begin
  GlDiagCS.Enter('DiagRecordMkdirTaskDone');
  try
    Inc(GlDiagCurrent.Queue.MkdirTasksDone);
    if aFailed then
      Inc(GlDiagCurrent.Queue.MkdirTasksFailed);
    if aSiteName <> '' then
    begin
      idx := DiagEnsureSiteIndex(aSiteName);
      Inc(GlDiagSites[idx].Metrics.Queue.MkdirTasksDone);
      if aFailed then
        Inc(GlDiagSites[idx].Metrics.Queue.MkdirTasksFailed);
    end;
  finally
    GlDiagCS.Leave;
  end;
end;

procedure DiagRecordMkdirUnnecessary(const aSiteName: String = '');
var
  idx: Integer;
begin
  GlDiagCS.Enter('DiagRecordMkdirUnnecessary');
  try
    Inc(GlDiagCurrent.Queue.MkdirUnnecessaryCount);
    if aSiteName <> '' then
    begin
      idx := DiagEnsureSiteIndex(aSiteName);
      Inc(GlDiagSites[idx].Metrics.Queue.MkdirUnnecessaryCount);
    end;
  finally
    GlDiagCS.Leave;
  end;
end;

procedure DiagRecordNeedMkdirClear(const aElapsedMs: Int64; const aSiteName: String = '');
var
  idx: Integer;
  procedure _Update(var aSnap: TDiagQueueSnapshot);
  begin
    Inc(aSnap.NeedMkdirClearCount);
    aSnap.NeedMkdirClearTotalMs := aSnap.NeedMkdirClearTotalMs + aElapsedMs;
    if aElapsedMs > aSnap.NeedMkdirClearPeakMs then
      aSnap.NeedMkdirClearPeakMs := aElapsedMs;
  end;
begin
  GlDiagCS.Enter('DiagRecordNeedMkdirClear');
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

{ Queue scanning / assignment }

function _DiagTaskTypeFromClassName(const aClassName: String): Integer;
begin
  { 0=race/wait, 1=dirlist, 2=auto, 3=other }
  if aClassName = 'TPazoRaceTask' then
    Result := 0
  else if aClassName = 'TWaitTask' then
    Result := 0
  else if aClassName = 'TPazoDirlistTask' then
    Result := 1
  else if (aClassName = 'TAutoNukeTask') or (aClassName = 'TAutoDirlistTask') or
          (aClassName = 'TAutoIndexTask') or (aClassName = 'TLoginTask') or
          (aClassName = 'TRulesTask') then
    Result := 2
  else
    Result := 3;
end;

procedure _IncFindBestNilReason(var aSnap: TDiagQueueSnapshot; const aReason: TDiagFindBestNilReason; const aTaskClassName: String);
var
  taskType: Integer;
begin
  case aReason of
    dfbnNoTasks: Inc(aSnap.FindBestNilNoTasks);
    dfbnNoSlots: Inc(aSnap.FindBestNilNoSlots);
    dfbnCooldown: Inc(aSnap.FindBestNilCooldown);
    dfbnDelayed:
      begin
        Inc(aSnap.FindBestNilDelayed);
        taskType := _DiagTaskTypeFromClassName(aTaskClassName);
        case taskType of
          0: Inc(aSnap.FindBestNilDelayedRace);
          1: Inc(aSnap.FindBestNilDelayedDirlist);
          2: Inc(aSnap.FindBestNilDelayedAuto);
        else
          Inc(aSnap.FindBestNilDelayedOther);
        end;
      end;
    dfbnNoReadyTask:
      begin
        Inc(aSnap.FindBestNilNoReadyTask);
        taskType := _DiagTaskTypeFromClassName(aTaskClassName);
        case taskType of
          0: Inc(aSnap.FindBestNilNoReadyRace);
          1: Inc(aSnap.FindBestNilNoReadyDirlist);
          2: Inc(aSnap.FindBestNilNoReadyAuto);
        else
          Inc(aSnap.FindBestNilNoReadyOther);
        end;
      end;
    dfbnNoReadyTaskMkdir:
      begin
        Inc(aSnap.FindBestNilNoReadyTask);
        Inc(aSnap.FindBestNilNoReadyRace);
        Inc(aSnap.FindBestNilNoReadyRaceMkdir);
      end;
    dfbnNoReadyTaskOther:
      begin
        Inc(aSnap.FindBestNilNoReadyTask);
        Inc(aSnap.FindBestNilNoReadyRace);
        Inc(aSnap.FindBestNilNoReadyRaceOther);
      end;
    dfbnOther: Inc(aSnap.FindBestNilOther);
  end;
end;

procedure DiagRecordFindBestTaskNil(const aReason: TDiagFindBestNilReason;
  const aTaskClassName: String; const aSiteName: String);
var
  idx: Integer;
begin
  GlDiagCS.Enter('DiagRecordFindBestTaskNil');
  try
    _IncFindBestNilReason(GlDiagCurrent.Queue, aReason, aTaskClassName);
    if aSiteName <> '' then
    begin
      idx := DiagEnsureSiteIndex(aSiteName);
      _IncFindBestNilReason(GlDiagSites[idx].Metrics.Queue, aReason, aTaskClassName);
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
    darDstMaxUp: Inc(aCounters.DstMaxUp);
    darSrcMaxDn: Inc(aCounters.SrcMaxDn);
    darSrcNoFreeSlot: Inc(aCounters.SrcNoFreeSlot);
    darDstNoFreeSlot: Inc(aCounters.DstNoFreeSlot);
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

procedure _UpdateDirlistWait(var aSnap: TDiagDirlistWaitSnapshot; const aWaitMs: Int64);
begin
  Inc(aSnap.Count);
  if aSnap.Count = 1 then
    aSnap.AvgMs := aWaitMs
  else
    aSnap.AvgMs := (aSnap.AvgMs * (aSnap.Count - 1) + aWaitMs) div aSnap.Count;
  if aWaitMs > aSnap.PeakMs then
    aSnap.PeakMs := aWaitMs;
end;

procedure DiagRecordDirlistWait(const aWaitMs: Int64; const aSiteName: String = '');
var
  idx: Integer;
begin
  GlDiagCS.Enter('DiagRecordDirlistWait');
  try
    _UpdateDirlistWait(GlDiagCurrent.DirlistWait, aWaitMs);
    if aSiteName <> '' then
    begin
      idx := DiagEnsureSiteIndex(aSiteName);
      _UpdateDirlistWait(GlDiagSites[idx].Metrics.DirlistWait, aWaitMs);
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

procedure DiagUpdateNeedMkdirStats(const aNeedMkdirDirlists, aRaceTasksWaiting: Integer;
  const aSiteName: String = '');
var
  idx: Integer;
  fTotalDirlists, fTotalRaceTasks: Integer;
  procedure _Update(var aSnap: TDiagQueueSnapshot);
  begin
    aSnap.NeedMkdirDirlists := aNeedMkdirDirlists;
    aSnap.RaceTasksWaitingOnMkdir := aRaceTasksWaiting;
  end;
begin
  GlDiagCS.Enter('DiagUpdateNeedMkdirStats');
  try
    if aSiteName = '' then
    begin
      _Update(GlDiagCurrent.Queue);
    end
    else
    begin
      idx := DiagEnsureSiteIndex(aSiteName);
      _Update(GlDiagSites[idx].Metrics.Queue);

      // Recalculate global values as the sum of all per-site snapshots.
      fTotalDirlists := 0;
      fTotalRaceTasks := 0;
      for idx := 0 to High(GlDiagSites) do
      begin
        Inc(fTotalDirlists, GlDiagSites[idx].Metrics.Queue.NeedMkdirDirlists);
        Inc(fTotalRaceTasks, GlDiagSites[idx].Metrics.Queue.RaceTasksWaitingOnMkdir);
      end;
      GlDiagCurrent.Queue.NeedMkdirDirlists := fTotalDirlists;
      GlDiagCurrent.Queue.RaceTasksWaitingOnMkdir := fTotalRaceTasks;
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

procedure _UpdateStuckCounters;
var
  i, idx: Integer;
  elapsedMs: Int64;
  siteName: String;
begin
  // Reset global counters.
  GlDiagCurrent.WaitTasks.StuckOver5s := 0;
  GlDiagCurrent.WaitTasks.StuckOver30s := 0;
  // Reset per-site counters.
  for i := 0 to High(GlDiagSites) do
  begin
    GlDiagSites[i].Metrics.WaitTasks.StuckOver5s := 0;
    GlDiagSites[i].Metrics.WaitTasks.StuckOver30s := 0;
  end;

  for i := 0 to High(GlDiagActiveWaitTasks) do
  begin
    try
      elapsedMs := MilliSecondsBetween(Now, GlDiagActiveWaitTasks[i].StartTime);
      siteName := GlDiagActiveWaitTasks[i].SiteName;
      idx := DiagSiteIndex(siteName);

      if elapsedMs > 30000 then
      begin
        Inc(GlDiagCurrent.WaitTasks.StuckOver30s);
        if idx >= 0 then
          Inc(GlDiagSites[idx].Metrics.WaitTasks.StuckOver30s);
      end
      else if elapsedMs > 5000 then
      begin
        Inc(GlDiagCurrent.WaitTasks.StuckOver5s);
        if idx >= 0 then
          Inc(GlDiagSites[idx].Metrics.WaitTasks.StuckOver5s);
      end;
    except
      // Ignore bad StartTime values; do not let stuck-counter calculation fail.
    end;
  end;
end;

procedure DiagTakeSnapshot;
begin
  GlDiagCS.Enter('DiagTakeSnapshot');
  try
    _UpdateStuckCounters;
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

{ Recent-abort detail recorders }

function _FindDstMaxUpDetailIndexByTaskName(const aTaskName: String): Integer;
var
  i, idx: Integer;
begin
  Result := -1;
  for i := 0 to GlDiagDstMaxUpDetailCount - 1 do
  begin
    idx := (GlDiagDstMaxUpDetailIndex - 1 - i + CDiagDetailBufferSize) mod CDiagDetailBufferSize;
    if GlDiagDstMaxUpDetails[idx].TaskName = aTaskName then
    begin
      Result := idx;
      Exit;
    end;
  end;
end;

function _FindMaxUpPerRipDetailIndexByTaskName(const aTaskName: String): Integer;
var
  i, idx: Integer;
begin
  Result := -1;
  for i := 0 to GlDiagMaxUpPerRipDetailCount - 1 do
  begin
    idx := (GlDiagMaxUpPerRipDetailIndex - 1 - i + CDiagDetailBufferSize) mod CDiagDetailBufferSize;
    if GlDiagMaxUpPerRipDetails[idx].TaskName = aTaskName then
    begin
      Result := idx;
      Exit;
    end;
  end;
end;

procedure DiagRecordDstMaxUpDetail(const aSiteName: String; const aNumUp, aMaxUp,
  aActualUploadingSlots: Integer; const aTaskName: String);
var
  idx: Integer;
begin
  GlDiagCS.Enter('DiagRecordDstMaxUpDetail');
  try
    idx := _FindDstMaxUpDetailIndexByTaskName(aTaskName);
    if idx < 0 then
    begin
      idx := GlDiagDstMaxUpDetailIndex;
      GlDiagDstMaxUpDetailIndex := (GlDiagDstMaxUpDetailIndex + 1) mod CDiagDetailBufferSize;
      if GlDiagDstMaxUpDetailCount < CDiagDetailBufferSize then
        Inc(GlDiagDstMaxUpDetailCount);
    end;
    with GlDiagDstMaxUpDetails[idx] do
    begin
      Timestamp := Now;
      SiteName := aSiteName;
      NumUp := aNumUp;
      MaxUp := aMaxUp;
      ActualUploadingSlots := aActualUploadingSlots;
      TaskName := aTaskName;
    end;
  finally
    GlDiagCS.Leave;
  end;
end;

procedure DiagRecordMaxUpPerRipDetail(const aSiteName: String; const aMaxUpPerRip,
  aActiveTransferCount: Integer; const aTaskName: String);
var
  idx: Integer;
begin
  GlDiagCS.Enter('DiagRecordMaxUpPerRipDetail');
  try
    idx := _FindMaxUpPerRipDetailIndexByTaskName(aTaskName);
    if idx < 0 then
    begin
      idx := GlDiagMaxUpPerRipDetailIndex;
      GlDiagMaxUpPerRipDetailIndex := (GlDiagMaxUpPerRipDetailIndex + 1) mod CDiagDetailBufferSize;
      if GlDiagMaxUpPerRipDetailCount < CDiagDetailBufferSize then
        Inc(GlDiagMaxUpPerRipDetailCount);
    end;
    with GlDiagMaxUpPerRipDetails[idx] do
    begin
      Timestamp := Now;
      SiteName := aSiteName;
      MaxUpPerRip := aMaxUpPerRip;
      ActiveTransferCount := aActiveTransferCount;
      TaskName := aTaskName;
    end;
  finally
    GlDiagCS.Leave;
  end;
end;

{ Output helpers }

function DiagFormatMetrics(const m: TDiagMetrics): String;
  function _SafeDiv(const aTotal, aCount: Int64): Int64;
  begin
    if aCount <= 0 then
      Result := 0
    else
      Result := aTotal div aCount;
  end;
const
  FMT = '[DIAG] WAITTASKS active=%d created=%d done=%d avg=%dms peak=%dms stuck>5s=%d stuck>30s=%d' + sLineBreak +
        '[DIAG] QUEUE total=%d race=%d dirlist=%d auto=%d other=%d assigned=%d dirlists_done=%d fbt_calls=%d' + sLineBreak +
        '[DIAG] SLOTS online=%d offline=%d down=%d markeddown=%d busy=%d free=%d wait_busy=%d freeslots=%d up=%d/%d dn=%d/%d up_cd=%ds dn_cd=%ds' + sLineBreak +
        '[DIAG] RACE-ABORT freeslots=%d maxsim_up=%d maxsim_down=%d no_slot=%d dstmaxup=%d srcmaxdn=%d srcnoslot=%d dstnoslot=%d offline=%d not_ready=%d maxupperrip=%d other=%d' + sLineBreak +
        '[DIAG] SLOT-ABORT freeslots=%d maxsim_up=%d maxsim_down=%d no_slot=%d offline=%d not_ready=%d maxupperrip=%d other=%d' + sLineBreak +
        '[DIAG] FBT-NIL no_tasks=%d no_slots=%d cooldown=%d delayed=%d no_ready=%d other=%d' + sLineBreak +
        '[DIAG] FBT-TYPE delayed race=%d dirlist=%d auto=%d other=%d | no_ready race=%d(mkdir=%d|other=%d) dirlist=%d auto=%d other=%d' + sLineBreak +
        '[DIAG] DIRLIST-WAIT count=%d avg=%dms peak=%dms' + sLineBreak +
        '[DIAG] MKDIR created=%d done=%d failed=%d pending=%d unnecessary=%d' + sLineBreak +
        '[DIAG] NEEDMKDIR dirlists=%d race_tasks_waiting=%d' + sLineBreak +
        '[DIAG] NEEDMKDIR-CLEAR count=%d avg=%dms peak=%dms';
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
     m.AssignRace.DstMaxUp, m.AssignRace.SrcMaxDn,
     m.AssignRace.SrcNoFreeSlot, m.AssignRace.DstNoFreeSlot,
     m.AssignRace.SiteOffline, m.AssignRace.TaskNotReady,
     m.AssignRace.MaxUpPerRip, m.AssignRace.Other,
     m.AssignSlots.FreeslotsZero, m.AssignSlots.MaxSimUpCooldown,
     m.AssignSlots.MaxSimDownCooldown, m.AssignSlots.NoSlotAvailable,
     m.AssignSlots.SiteOffline, m.AssignSlots.TaskNotReady,
     m.AssignSlots.MaxUpPerRip, m.AssignSlots.Other,
     m.Queue.FindBestNilNoTasks, m.Queue.FindBestNilNoSlots,
     m.Queue.FindBestNilCooldown, m.Queue.FindBestNilDelayed,
     m.Queue.FindBestNilNoReadyTask, m.Queue.FindBestNilOther,
     m.Queue.FindBestNilDelayedRace, m.Queue.FindBestNilDelayedDirlist,
     m.Queue.FindBestNilDelayedAuto, m.Queue.FindBestNilDelayedOther,
     m.Queue.FindBestNilNoReadyRace, m.Queue.FindBestNilNoReadyRaceMkdir,
     m.Queue.FindBestNilNoReadyRaceOther, m.Queue.FindBestNilNoReadyDirlist,
     m.Queue.FindBestNilNoReadyAuto, m.Queue.FindBestNilNoReadyOther,
     m.DirlistWait.Count, m.DirlistWait.AvgMs, m.DirlistWait.PeakMs,
     m.Queue.MkdirTasksCreated, m.Queue.MkdirTasksDone,
     m.Queue.MkdirTasksFailed,
     m.Queue.MkdirTasksCreated - m.Queue.MkdirTasksDone,
     m.Queue.MkdirUnnecessaryCount,
     m.Queue.NeedMkdirDirlists, m.Queue.RaceTasksWaitingOnMkdir,
     m.Queue.NeedMkdirClearCount,
     _SafeDiv(m.Queue.NeedMkdirClearTotalMs, m.Queue.NeedMkdirClearCount),
     m.Queue.NeedMkdirClearPeakMs]);
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

function DiagFormatAbortDetails: String;
var
  i, idx, n: Integer;
  elapsedSec: Double;
  haveAny: Boolean;
begin
  Result := '';
  haveAny := False;
  GlDiagCS.Enter('DiagFormatAbortDetails');
  try
    if GlDiagDstMaxUpDetailCount > 0 then
    begin
      haveAny := True;
      Result := Result + '[DIAG] recent dstmaxup aborts:' + sLineBreak;
      for i := 0 to GlDiagDstMaxUpDetailCount - 1 do
      begin
        idx := (GlDiagDstMaxUpDetailIndex - 1 - i + CDiagDetailBufferSize) mod CDiagDetailBufferSize;
        elapsedSec := MilliSecondsBetween(Now, GlDiagDstMaxUpDetails[idx].Timestamp) / 1000.0;
        Result := Result + Format('[DIAG]  #%d site=%s num_up=%d max_up=%d actual_up_slots=%d age=%.1fs task=%s' + sLineBreak,
          [i + 1,
           GlDiagDstMaxUpDetails[idx].SiteName,
           GlDiagDstMaxUpDetails[idx].NumUp,
           GlDiagDstMaxUpDetails[idx].MaxUp,
           GlDiagDstMaxUpDetails[idx].ActualUploadingSlots,
           elapsedSec,
           GlDiagDstMaxUpDetails[idx].TaskName]);
      end;
    end;

    if GlDiagMaxUpPerRipDetailCount > 0 then
    begin
      haveAny := True;
      Result := Result + '[DIAG] recent maxupperrip aborts:' + sLineBreak;
      for i := 0 to GlDiagMaxUpPerRipDetailCount - 1 do
      begin
        idx := (GlDiagMaxUpPerRipDetailIndex - 1 - i + CDiagDetailBufferSize) mod CDiagDetailBufferSize;
        elapsedSec := MilliSecondsBetween(Now, GlDiagMaxUpPerRipDetails[idx].Timestamp) / 1000.0;
        Result := Result + Format('[DIAG]  #%d site=%s maxupperrip=%d active_transfer_count=%d age=%.1fs task=%s' + sLineBreak,
          [i + 1,
           GlDiagMaxUpPerRipDetails[idx].SiteName,
           GlDiagMaxUpPerRipDetails[idx].MaxUpPerRip,
           GlDiagMaxUpPerRipDetails[idx].ActiveTransferCount,
           elapsedSec,
           GlDiagMaxUpPerRipDetails[idx].TaskName]);
      end;
    end;

    if not haveAny then
      Result := '[DIAG] no recent abort details';
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
      WriteLn(f, sLineBreak + '--- recent abort details ---');
      WriteLn(f, DiagFormatAbortDetails);
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
