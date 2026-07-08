unit queueunit;

{$DEFINE DEDUP_RACE}   // test deduplication for race tasks
{$DEFINE DEDUP_DIRLIST} // test deduplication for dirlist tasks

interface

uses
  Classes, Contnrs, tasksunit, taskrace, SyncObjs, slcriticalsection2, pazo,
  taskidle, taskquit, tasklogin, RegExpr, taskautoindex, taskrules,
  taskautodirlist, taskautonuke, tasktraceunit, Generics.Collections;


type TQueueStat = class
  private
    FRaceHistory: TList<TDateTime>;
  public
    FRaceTaskCount: integer;
    FDirlistTaskCount: integer;
    FAutoTaskCount: integer;
    FOtherTaskCount: integer;

    // Phase 1 performance metrics
    FTaskWaitMsTotal: Int64;
    FTaskWaitCount: Integer;
    FTasksAssignedThisRun: Integer;
    FTasksSkippedThisRun: Integer;
    FQueueIterateMsTotal: Int64;
    FQueueIterateCount: Integer;
    FQueueSortCount: Integer;
    FQueueSortMsTotal: Int64;
    FSlotIdleMsTotal: Int64;
    FSlotIdleCount: Integer;
    FGraphWakeCount: Integer;
    FBusyDestinationsHitCount: Integer;

    // Phase 2 diagnostics: per-method timing
    FFindBestRaceMsTotal: Int64;
    FFindBestRaceCount: Integer;
    FTryToAssignRaceSlotsMsTotal: Int64;
    FTryToAssignRaceSlotsCount: Integer;
    FQueueStatMsTotal: Int64;
    FQueueStatCount: Integer;

    // Phase 2 diagnostics: task creation / dup counts
    FTasksCreatedRace: Integer;
    FTasksCreatedDirlist: Integer;
    FTasksCreatedMkdir: Integer;
    FTasksCreatedLogin: Integer;
    FTasksCreatedOther: Integer;
    FTasksDupRace: Integer;
    FTasksDupDirlist: Integer;
    FTasksDupMkdir: Integer;
    FTasksDupLogin: Integer;

    // Breakdown of why a race could not be assigned to a destination
    FBusyDestNoFreeSlotsSource: Integer;
    FBusyDestNoFreeSlotsDest: Integer;
    FBusyDestMaxSimUpCooldown: Integer;
    FBusyDestMaxSimDownCooldown: Integer;
    FBusyDestBusyDict: Integer;
    FBusyDestActiveTransferDst: Integer;
    FBusyDestMaxUp: Integer;
    FBusyDestActiveTransferSrc: Integer;
    FBusyDestMaxDn: Integer;
    FBusyDestNoSlotSource: Integer;
    FBusyDestNoSlotDest: Integer;
    FBusyDestMaxUpPerRip: Integer;

    constructor Create;
    destructor Destroy; override;
    procedure CleanupOldRaceHistory;
    procedure AddRaceToHistory;
    function RecentRaceCount: Integer;
end;

{ Origin of a QueueFire call, used for queue-wake instrumentation. }
TQueueFireSource = (qfsSlot, qfsGraphWake, qfsMainThread, qfsConsole, qfsIrc, qfsAddTask, qfsPazo, qfsOther);

procedure IncQueueFireCount(const aSource: TQueueFireSource);

{ In-memory ring buffer for queue diagnostics. }
type TQueueDiagEntry = record
  Timestamp: TDateTime;
  Message: string;
end;

TQueueDiagLog = class
private
  FBuffer: array of TQueueDiagEntry;
  FCapacity: Integer;
  FCount: Integer;
  FHead: Integer;
  FCS: TCriticalSection;
public
  constructor Create(const aCapacity: Integer);
  destructor Destroy; override;
  procedure Add(const aMsg: string);
  function GetRecent(const aCount: Integer): string;
end;

var
  GlQueueDiag: TQueueDiagLog;

procedure QueueDiagLog(const aMsg: string);

type TQueueTask = class
  FFullname: string;
  FType: TClass;
end;

type
  TQueueThread = class(TThread)
    main_lock: TSlCriticalSection2;
    fQueueStat: TQueueStat;
    destructor Destroy; override;
    procedure Execute; override;
    procedure TryToAssignSlots(t: TTask);

  private
  tasks:      TObjectList;
  waiting_tasks: TObjectList;
  queueevent: TEvent;
  fSiteName: String;
  fSite: TObject;
  fBusyDestinations: TDictionary<TObject, integer>;

  // Fast lookup indexes for TaskAlreadyInQueue instead of linear scans.
  // TStringList with Sorted=True gives O(log n) lookup/add/remove and avoids
  // the FPC internal error seen with multiple TDictionary<String,Boolean> fields.
  // Keys are kept in sync with tasks / waiting_tasks under main_lock.
  fRaceTaskSet: TStringList;
  fDirlistTaskSet: TStringList;
  fMkdirTaskSet: TStringList;
  fLoginTaskSet: TStringList;

  queue_last_run: TDateTime;
  queueclean_last_run: TDateTime;
  queue_last_stat_update: TDateTime;
  queue_last_memory_diag: TDateTime;
  fQueueIterateStart: TDateTime;
  fLastSortTime: TDateTime;
  fTasksMovedSinceSort: Integer;

  // Race-check flag: set when a slot becomes free so FindBestRaceTask is only
  // executed when it can actually find a race. Accessed with InterlockedExchange.
  fNeedRaceCheck: Integer;

  // Task currently assigned to a slot and executing. Used to allow exactly one
  // legitimate follow-up task (e.g. a dirlist re-read) with the same key.
  fExecutingTask: TTask;
  fExecutingTaskAllowedDuplicate: Boolean;

  // In-memory diagnostics counters (reset periodically)
  fDiagFbrCalls: Integer;
  fDiagFbrAssigned: Integer;
  fDiagFbrNoCand: Integer;
  fDiagFbrNoCandFreeslots: Integer;
  fDiagFbrNoCandMaxUp: Integer;
  fDiagFbrNoCandMaxDn: Integer;
  fDiagFbrNoCandNoSlotDst: Integer;
  fDiagFbrNoCandBusyDict: Integer;
  fDiagFbrNoCandActiveTransfer: Integer;
  fDiagFbrNoCandNotReady: Integer;
  fDiagFbrNoCandOther: Integer;
  fDiagFbrTriedFailed: Integer;
  fDiagIterCount: Integer;
  fDiagIterAssigned: Integer;
  fDiagIterSkipped: Integer;
  fDiagLastLogTime: TDateTime;

    procedure TryToAssignLoginSlot(t: TLoginTask);
    procedure TryToAssignRaceSlots(t: TPazoRaceTask);
    function HasOnlineFreeSlot(const aSite: TObject): Boolean;
    function HasPendingRaceTasks: Boolean;
    function FindBestRaceTask: Boolean;
    function TaskAlreadyInQueue(t: TTask): boolean;
    function IsExecutingTaskWithKey(const t: TTask; const aKey: String): Boolean;

    // Key helpers and index maintenance for the duplicate lookup.
    function RaceTaskKey(const t: TPazoRaceTask): String;
    function DirlistTaskKey(const t: TPazoDirlistTask): String;
    function MkdirTaskKey(const t: TPazoMkdirTask): String;
    function LoginTaskKey(const t: TLoginTask): String;
    procedure AddTaskToIndex(const t: TTask);
    procedure RemoveTaskFromIndex(const t: TTask);
    procedure QueueStat;
    procedure LogDiagSummary;

public


procedure QueueFire;
procedure IncGraphWakeCount;
procedure QueueStart;
procedure SignalRaceCheck;

procedure AddTask(t: TTask);
procedure QueueEmpty(const sitename: String);
procedure RemovePazoMKDIR(const pazo_id: integer; const dir: String);
procedure RemovePazoSfv(const aPazoID: integer; const aDir: String);
procedure RemovePazoRace(const pazo_id: integer; const dstsite, dir, filename: String);
function IrcKillAll(const netname, channel, params: String): boolean;

{ Fills all tasks of this queue into the given list using a value object of type TQueueTask. }
procedure GetCurrentTasks(const taskLst: Contnrs.TObjectList);

function RemovePazo(const pazo_id: integer; const aForce: boolean = False): boolean;

procedure RemoveRaceTasks(const pazo_id: integer; const sitename: String);
procedure RemovePazoDirTasks(const pazo_id: integer);

procedure QueueSort;

procedure QueueClean(run_now: boolean = False);
constructor Create(const aSiteName: String);

function FetchAutoIndex: TAutoIndexTask;
function FetchAutoBnctest: TLoginTask;
function FetchAutoRules: TRulesTask;
function FetchAutoDirlist: TAutoDirlistTask;
function FetchAutoNuke: TAutoNukeTask;

{ Send the current tasks to the queue console window. }
procedure QueueSendCurrentTasksToConsole;

property QueueLastRun: TDateTime read queue_last_run;
property QueueCleanLastRun: TDateTime read queueclean_last_run;

  end;

{ Wakes the TWaitTask paired with the given TPazoRaceTask (if any). }
procedure SignalPairedWaitTask(const aRaceUid: UInt64);

procedure QueueInit;
procedure QueueUninit;
procedure QueueStatAll;
function QueueStatAllAsString: String;
function QueueStatForSiteAsString(const aSiteName: String): String;

var
  QueueStatUpdateDateTime: TDateTime;

implementation

uses
  SysUtils, Types, irc, DateUtils, debugunit, notify, console, kb, mainthread, Math, configunit, mrdohutils,
  tasktvinfolookup, taskhttpnfo, tasksitenfo, tasksitesfv, sitesunit, taskregistry, pazotaskgraph;

const
  section = 'queue';

var
  // config
  maxassign: integer;
  maxassign_delay: integer;
  sample_dirs_priority: Integer; //< value for priority in queue sorter for sample dirs from slftp.ini
  proof_dirs_priority: Integer; //< value for priority in queue sorter for proof dirs from slftp.ini
  subs_dirs_priority: Integer; //< value for priority in queue sorter for subtitle dirs from slftp.ini
  cover_dirs_priority: Integer; //< value for priority in queue sorter for cover dirs from slftp.ini
  queueclean_unassigned: Integer;
  queueclean_maxrunning: Integer;
  enable_queueclean: boolean;
  queue_recycle_post_to_irc: boolean;

  StatsList: TObjectList<TQueueStat>;
  GlDefaultIterationWaitTimeout: Cardinal = 15 * 1000;
  GlMinQueueWaitTimeout: Cardinal = 100;
  GlQueueFireCounts: array[TQueueFireSource] of Int64;

procedure IncQueueFireCount(const aSource: TQueueFireSource);
begin
  Inc(GlQueueFireCounts[aSource]);
end;

{ TQueueDiagLog }

constructor TQueueDiagLog.Create(const aCapacity: Integer);
begin
  inherited Create;
  FCapacity := aCapacity;
  if FCapacity < 1 then
    FCapacity := 1;
  SetLength(FBuffer, FCapacity);
  FCount := 0;
  FHead := 0;
  FCS := TCriticalSection.Create;
end;

destructor TQueueDiagLog.Destroy;
begin
  FCS.Free;
  SetLength(FBuffer, 0);
  inherited;
end;

procedure TQueueDiagLog.Add(const aMsg: string);
begin
  FCS.Enter;
  try
    FBuffer[FHead].Timestamp := Now;
    FBuffer[FHead].Message := aMsg;
    FHead := (FHead + 1) mod FCapacity;
    if FCount < FCapacity then
      Inc(FCount);
  finally
    FCS.Leave;
  end;
end;

function TQueueDiagLog.GetRecent(const aCount: Integer): string;
var
  fIndex, fStart, fI, fRequested: Integer;
  fEntry: TQueueDiagEntry;
begin
  Result := '';
  fRequested := aCount;
  if fRequested < 1 then
    fRequested := 20;
  if fRequested > FCapacity then
    fRequested := FCapacity;

  FCS.Enter;
  try
    if FCount = 0 then
    begin
      Result := 'No queue diagnostics logged yet.';
      exit;
    end;

    if fRequested > FCount then
      fRequested := FCount;

    // FHead points to next write position; most recent entry is at (FHead - 1)
    fStart := (FHead - 1 + FCapacity) mod FCapacity;
    for fI := 0 to fRequested - 1 do
    begin
      fIndex := (fStart - fI + FCapacity) mod FCapacity;
      fEntry := FBuffer[fIndex];
      if Result <> '' then
        Result := Result + #13#10;
      Result := Result + Format('[%s] %s', [FormatDateTime('hh:nn:ss.zzz', fEntry.Timestamp), fEntry.Message]);
    end;
  finally
    FCS.Leave;
  end;
end;

procedure QueueDiagLog(const aMsg: string);
begin
  if GlQueueDiag <> nil then
    GlQueueDiag.Add(aMsg);
end;

{ TQueueStat }

constructor TQueueStat.Create;
begin
  inherited;
  FRaceHistory := TList<TDateTime>.Create;
end;

destructor TQueueStat.Destroy;
begin
  FRaceHistory.Free;
  inherited;
end;

procedure TQueueStat.CleanupOldRaceHistory;
var
  i: Integer;
  fCutoff: TDateTime;
begin
  fCutoff := IncMilliSecond(Now, -60000);
  for i := FRaceHistory.Count - 1 downto 0 do
  begin
    if FRaceHistory[i] < fCutoff then
      FRaceHistory.Delete(i);
  end;
end;

procedure TQueueStat.AddRaceToHistory;
begin
  FRaceHistory.Add(Now);
  // keep the list bounded even if RecentRaceCount is not called
  CleanupOldRaceHistory;
end;

function TQueueStat.RecentRaceCount: Integer;
begin
  CleanupOldRaceHistory;
  Result := FRaceHistory.Count;
end;

procedure TQueueThread.QueueFire;
begin
  try
    //Debug(dpSpam, section, Format('QueueFire: %s', [(TSite(fSite).Name)]));
    queueevent.SetEvent;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] QueueFire: %s', [e.Message]));
      exit;
    end;
  end;
end;

procedure TQueueThread.IncGraphWakeCount;
begin
  Inc(fQueueStat.FGraphWakeCount);
end;

function QueueSorter(Item1, Item2: Pointer): integer;
var
  i1, i2: TTask;
  tp1, tp2: TPazoTask;
  tpm1, tpm2: TPazoMkdirTask;
  tpr1, tpr2: TPazoRaceTask;
begin
  // compare: -1 Item1 is before Item2
  // compare:  1 Item1 is after Item2
  // ref: https://www.freepascal.org/docs-html/rtl/classes/tstringlist.customsort.html
  try
    i1 := TTask(item1);
    i2 := TTask(item2);

    if (i1 = nil) or (i2 = nil) then
    begin
      Result := 0;
      exit;
    end;

    // Give priority to wait
    if ((i1.ClassType = TWaitTask) and (i2.ClassType = TWaitTask)) then
    begin
      Result := 0;
      exit;
    end;
    if ((i1.ClassType = TWaitTask) and (not (i2.ClassType = TWaitTask))) then
    begin
      Result := -1;
      exit;
    end;
    if ((not (i1.ClassType = TWaitTask)) and (i2.ClassType = TWaitTask)) then
    begin
      Result := 1;
      exit;
    end;

    // Give priority to PazoTasks
    if ((not (i1 is TPazoTask)) and (not (i2 is TPazoTask))) then
    begin
      Result := 0;
      exit;
    end;
    if ((i1 is TPazoTask) and (not (i2 is TPazoTask))) then
    begin
      Result := -1;
      exit;
    end;
    if ((not (i1 is TPazoTask)) and (i2 is TPazoTask)) then
    begin
      Result := 1;
      exit;
    end;

    tp1 := TPazoTask(Item1);
    tp2 := TPazoTask(Item2);

    // Give priority to mkdir
    if ((tp1 is TPazoMkdirTask) and (tp2 is TPazoMkdirTask)) then
    begin
      tpm1 := TPazoMkdirTask(Item1);
      tpm2 := TPazoMkdirTask(Item2);

      if ((tpm1.dir <> '') and (tpm2.dir <> '')) then
      begin
        Result := 0;
        exit;
      end;
      if ((tpm1.dir = '') and (tpm2.dir = '')) then
      begin
        Result := 0;
        exit;
      end;
      // give priority to mkdir tasks that affect maindirs (not a subdir mkdir)
      if ((tpm1.dir = '') and (tpm2.dir <> '')) then
      begin
        Result := -1;
        exit;
      end;
      if ((tpm1.dir <> '') and (tpm2.dir = '')) then
      begin
        Result := 1;
        exit;
      end;
    end;
    if ((tp1 is TPazoMkdirTask) and (not (tp2 is TPazoMkdirTask))) then
    begin
      Result := -1;
      exit;
    end;
    if ((not (tp1 is TPazoMkdirTask)) and (tp2 is TPazoMkdirTask)) then
    begin
      Result := 1;
      exit;
    end;

    // Give priority to RaceTask
    if ((tp1 is TPazoRaceTask) and (tp2 is TPazoRaceTask)) then
    begin
      tpr1 := TPazoRaceTask(Item1);
      tpr2 := TPazoRaceTask(Item2);

      Result := CompareValue(tpr2.rank, tpr1.rank);
      if (Result <> 0) then
        exit;

      // Give priority to sfv
      if ((tpr1.IsSfv) and (not tpr2.IsSfv)) then
      begin
        Result := -1;
        exit;
      end;
      if ((not tpr1.IsSfv) and (tpr2.IsSfv)) then
      begin
        Result := 1;
        exit;
      end;
      if ((tpr1.IsSfv) and (tpr2.IsSfv)) then
      begin
        Result := CompareValue(tpr2.rank, tpr1.rank);
        exit;
      end;

      // Give priority to nfo
      if ((tpr1.IsNfo) and (not tpr2.IsNfo)) then
      begin
        Result := -1;
        exit;
      end;
      if ((not tpr1.IsNfo) and (tpr2.IsNfo)) then
      begin
        Result := 1;
        exit;
      end;
      if ((tpr1.IsNfo) and (tpr2.IsNfo)) then
      begin
        Result := CompareValue(tpr2.rank, tpr1.rank);
        exit;
      end;

      // Sample dir priority
      if (tpr1.IsSample) or (tpr2.IsSample) then
      begin
        if ((tpr1.IsSample) and (not tpr2.IsSample)) then
        begin
          case sample_dirs_priority of
            0: Result := 0;
            1: Result := -1;
            2: Result := 1;
          end;
        end
        else if ((not tpr1.IsSample) and (tpr2.IsSample)) then
        begin
          case sample_dirs_priority of
            0: Result := 0;
            1: Result := 1;
            2: Result := -1;
          end;
        end
        else
          Result := CompareValue(tpr2.rank, tpr1.rank);
      end;

      // Proof priority
      if (tpr1.IsProof) or (tpr2.IsProof) then
      begin
        if ((tpr1.IsProof) and (not tpr2.IsProof)) then
        begin
          case proof_dirs_priority of
            0: Result := 0;
            1: Result := -1;
            2: Result := 1;
          end;
        end
        else if ((not tpr1.IsProof) and (tpr2.IsProof)) then
        begin
          case proof_dirs_priority of
            0: Result := 0;
            1: Result := 1;
            2: Result := -1;
          end;
        end
        else
          Result := CompareValue(tpr2.rank, tpr1.rank);
      end;

      // Subs priority
      if (tpr1.IsSubs) or (tpr2.IsSubs) then
      begin
        if ((tpr1.IsSubs) and (not tpr2.IsSubs)) then
        begin
          case subs_dirs_priority of
            0: Result := 0;
            1: Result := -1;
            2: Result := 1;
          end;
        end
        else if ((not tpr1.IsSubs) and (tpr2.IsSubs)) then
        begin
          case subs_dirs_priority of
            0: Result := 0;
            1: Result := 1;
            2: Result := -1;
          end;
        end
        else
          Result := CompareValue(tpr2.rank, tpr1.rank);
      end;

      // Covers priority
      if (tpr1.IsCovers) or (tpr2.IsCovers) then
      begin
        if ((tpr1.IsCovers) and (not tpr2.IsCovers)) then
        begin
          case cover_dirs_priority of
            0: Result := 0;
            1: Result := -1;
            2: Result := 1;
          end;
        end
        else if ((not tpr1.IsCovers) and (tpr2.IsCovers)) then
        begin
          case cover_dirs_priority of
            0: Result := 0;
            1: Result := 1;
            2: Result := -1;
          end;
        end
        else
          Result := CompareValue(tpr2.rank, tpr1.rank);
      end;

      if (Result = 0) then
        Result := CompareValue(tpr2.filesize, tpr1.filesize);

      exit;
    end;

    if ((tp1 is TPazoRaceTask) and (not (tp2 is TPazoRaceTask))) then
    begin
      Result := -1;
      exit;
    end;
    if ((not (tp1 is TPazoRaceTask)) and (tp2 is TPazoRaceTask)) then
    begin
      Result := 1;
      exit;
    end;

    // All others (Dirlists and so on)
    Result := compareDate(tp1.mainpazo.lastTouch, tp2.mainpazo.lastTouch);
  except
  on e: Exception do
    begin
      Debug(dpError, section, '[EXCEPTION] QueueSorter : %s', [e.Message]);
      Result := 0;
    end;
  end;
end;

procedure TQueueThread.QueueSort;
begin
  try
    Debug(dpSpam, section, 'Sorting queue 1');
    main_lock.Enter('Queue_Sort');
    try
      tasks.Sort(@QueueSorter);
    finally
      main_lock.Leave;
    end;
    Debug(dpSpam, section, 'Sorting queue 2');
  except
    on e: Exception do
    begin
      Debug(dpError, section, '[EXCEPTION] QueueSort : %s', [e.Message]);
    end;
  end;
end;

procedure TQueueThread.QueueStart;
begin
  QueueStatAll;
end;

procedure TQueueThread.SignalRaceCheck;
begin
  InterlockedExchange(fNeedRaceCheck, 1);
end;

procedure SignalPairedWaitTask(const aRaceUid: UInt64);
var
  fRaceTask: TTask;
  fWaitTask: TTask;
  fWait: TWaitTask;
begin
  if aRaceUid = 0 then
    exit;

  if GlTaskRegistry = nil then
    exit;

  fRaceTask := GlTaskRegistry.Lookup(aRaceUid);
  if (fRaceTask = nil) or (fRaceTask.ClassType <> TPazoRaceTask) then
    exit;

  if TPazoRaceTask(fRaceTask).dst_uid = 0 then
    exit;

  fWaitTask := GlTaskRegistry.Lookup(TPazoRaceTask(fRaceTask).dst_uid);
  if (fWaitTask = nil) or (fWaitTask.ClassType <> TWaitTask) then
    exit;

  fWait := TWaitTask(fWaitTask);
  // Wake the wait task so it can finish even if the queue loop misses it.
  fWait.ready := True;
  fWait.event.SetEvent;
end;

constructor TQueueThread.Create(const aSiteName: String);
begin
  main_lock := nil;
  tasks := nil;
  waiting_tasks := nil;
  queueevent := nil;
  fQueueStat := nil;
  fBusyDestinations := nil;
  fRaceTaskSet := nil;
  fDirlistTaskSet := nil;
  fMkdirTaskSet := nil;
  fLoginTaskSet := nil;

  inherited Create(False);
  {$IFDEF DEBUG}
    NameThreadForDebugging('Queue/' + aSiteName, self.ThreadID);
  {$ENDIF}

  try
    main_lock := TSLCriticalSection2.Create('Queue_' + aSiteName);
    tasks := TObjectList.Create(True);
    waiting_tasks := TObjectList.Create(True);
    queueevent := TEvent.Create(nil, False, False, 'SLFTP_queue_event_' + aSiteName);
    queue_last_run := Now;
    queueclean_last_run := Now;
    queue_last_stat_update := Now;
    queue_last_memory_diag := Now;
    fLastSortTime := 0;
    fTasksMovedSinceSort := 0;
    fNeedRaceCheck := 1;
    fExecutingTask := nil;
    fExecutingTaskAllowedDuplicate := False;
    fDiagFbrCalls := 0;
    fDiagFbrAssigned := 0;
    fDiagFbrNoCand := 0;
    fDiagFbrNoCandFreeslots := 0;
    fDiagFbrNoCandMaxUp := 0;
    fDiagFbrNoCandMaxDn := 0;
    fDiagFbrNoCandNoSlotDst := 0;
    fDiagFbrNoCandBusyDict := 0;
    fDiagFbrNoCandActiveTransfer := 0;
    fDiagFbrNoCandNotReady := 0;
    fDiagFbrNoCandOther := 0;
    fDiagFbrTriedFailed := 0;
    fDiagIterCount := 0;
    fDiagIterAssigned := 0;
    fDiagIterSkipped := 0;
    fDiagLastLogTime := Now;
    FreeOnTerminate := True;
    fQueueStat := TQueueStat.Create();
    StatsList.Add(fQueueStat);
    fSiteName := aSiteName;
    fBusyDestinations := TDictionary<TObject, integer>.Create;
    fRaceTaskSet := TStringList.Create;
    fRaceTaskSet.Sorted := True;
    fRaceTaskSet.Duplicates := dupIgnore;
    fDirlistTaskSet := TStringList.Create;
    fDirlistTaskSet.Sorted := True;
    fDirlistTaskSet.Duplicates := dupIgnore;
    fMkdirTaskSet := TStringList.Create;
    fMkdirTaskSet.Sorted := True;
    fMkdirTaskSet.Duplicates := dupIgnore;
    fLoginTaskSet := TStringList.Create;
    fLoginTaskSet.Sorted := True;
    fLoginTaskSet.Duplicates := dupIgnore;
  except
    FreeAndNil(fRaceTaskSet);
    FreeAndNil(fDirlistTaskSet);
    FreeAndNil(fMkdirTaskSet);
    FreeAndNil(fLoginTaskSet);
    FreeAndNil(fBusyDestinations);
    if fQueueStat <> nil then
    begin
      StatsList.Remove(fQueueStat);
      FreeAndNil(fQueueStat);
    end;
    FreeAndNil(queueevent);
    FreeAndNil(waiting_tasks);
    FreeAndNil(tasks);
    FreeAndNil(main_lock);
    raise;
  end;
end;

destructor TQueueThread.Destroy;
begin
  main_lock.Free;
  tasks.Free;
  waiting_tasks.Free;
  queueevent.Free;
  fRaceTaskSet.Free;
  fDirlistTaskSet.Free;
  fMkdirTaskSet.Free;
  fLoginTaskSet.Free;
  inherited;
end;

function TQueueThread.HasOnlineFreeSlot(const aSite: TObject): Boolean;
var
  fSite: TSite;
  fSlot: TSiteSlot;
begin
  Result := False;
  fSite := TSite(aSite);
  for fSlot in fSite.slots do
  begin
    if (fSlot.todotask = nil) and (fSlot.status = ssOnline) then
    begin
      Result := True;
      exit;
    end;
  end;
end;

function TQueueThread.HasPendingRaceTasks: Boolean;
var
  fTask: TTask;
begin
  Result := False;
  for fTask in tasks do
  begin
    if fTask is TPazoRaceTask then
    begin
      Result := True;
      exit;
    end;
  end;
end;

function TQueueThread.FindBestRaceTask: Boolean;
var
  fTaskList: TList;
  fTask: TTask;
  i: integer;
  s1, s2: TSite;
  fRace: TPazoRaceTask;
begin
  Result := False;
  s1 := TSite(self.fSite);

  Inc(fDiagFbrCalls);

  if s1.freeslots = 0 then
  begin
    Inc(fDiagFbrNoCand);
    Inc(fDiagFbrNoCandFreeslots);
    exit;
  end;

  fTaskList := TList.Create;
  try
    for fTask in tasks do
    begin
      if not (fTask is TPazoRaceTask) then
        Continue;
      if fTask.ssite1 <> s1 then
        Continue;
      if fTask.slot1 <> nil then
        Continue;
      if fTask.ready or fTask.readyerror then
        Continue;
      if not fTask.IsReadyToBeExecuted then
      begin
        Inc(fDiagFbrNoCandNotReady);
        Continue;
      end;

      fRace := TPazoRaceTask(fTask);
      s2 := TSite(fRace.ssite2);

      // Hard prerequisites: only consider races that can actually be assigned
      // right now. This prevents the expensive slot-assignment logic from being
      // run for saturated destinations or already-active transfers.
      if s2.freeslots = 0 then
      begin
        Inc(fDiagFbrNoCandFreeslots);
        Continue;
      end;
      if s2.num_up >= s2.max_up then
      begin
        Inc(fDiagFbrNoCandMaxUp);
        Continue;
      end;
      if s2.MaxSimUpCooldownActive then
      begin
        Inc(fDiagFbrNoCandOther);
        Continue;
      end;
      if fBusyDestinations.ContainsKey(s2) then
      begin
        Inc(fDiagFbrNoCandBusyDict);
        Continue;
      end;

      // Source download limits
      if fRace.ps1.StatusRealPreOrShouldPre then
      begin
        if s1.num_dn >= s1.max_pre_dn then
        begin
          Inc(fDiagFbrNoCandMaxDn);
          Continue;
        end;
      end
      else
      begin
        if s1.num_dn >= s1.max_dn then
        begin
          Inc(fDiagFbrNoCandMaxDn);
          Continue;
        end;
      end;

      // Already transferring this file to/from the destination?
      if fRace.ps2.HasActiveTransfer(fRace.dir + fRace.filename) then
      begin
        Inc(fDiagFbrNoCandActiveTransfer);
        Continue;
      end;
      if fRace.ps1.HasActiveTransfer(fRace.dir + fRace.filename, s2.Name) then
      begin
        Inc(fDiagFbrNoCandActiveTransfer);
        Continue;
      end;

      // Destination must have an online, free slot (not just any free slot)
      if not HasOnlineFreeSlot(s2) then
      begin
        Inc(fDiagFbrNoCandNoSlotDst);
        Continue;
      end;

      fTaskList.Add(fTask);
    end;

    if fTaskList.Count = 0 then
    begin
      Inc(fDiagFbrNoCand);
      exit;
    end;

    fTaskList.Sort(@QueueSorter);

    for i := 0 to fTaskList.Count - 1 do
    begin
      fTask := TTask(fTaskList[i]);
      TryToAssignRaceSlots(TPazoRaceTask(fTask));
      if fTask.slot1 <> nil then
      begin
        Inc(fDiagFbrAssigned);
        Result := True;
        exit;
      end;
    end;

    // Candidates existed but none could be assigned (e.g. slot was taken between
    // the cheap filter and the actual slot assignment).
    Inc(fDiagFbrTriedFailed);
  finally
    fTaskList.Free;
  end;
end;

procedure TQueueThread.TryToAssignRaceSlots(t: TPazoRaceTask);
var
  s1, s2: TSite;
  i: integer;
  ss1, ss2, fSiteSlotLoop: TSiteSlot;
  fPartnerTask: TTask;
  fWaitUid: UInt64;
begin
  try
    s1 := TSite(t.ssite1);
    s2 := TSite(t.ssite2);
    if s1.freeslots = 0 then
    begin
      Inc(fQueueStat.FBusyDestNoFreeSlotsSource);
      exit;
    end;
    if s2.freeslots = 0 then
    begin
      Inc(fQueueStat.FBusyDestNoFreeSlotsDest);
      if not fBusyDestinations.ContainsKey(s2) then
        fBusyDestinations.Add(s2, 0);
      exit;
    end;

    if s2.MaxSimUpCooldownActive then
    begin
      if not fBusyDestinations.ContainsKey(s2) then
        fBusyDestinations.Add(s2, 0);
      Inc(fQueueStat.FBusyDestinationsHitCount);
      Inc(fQueueStat.FBusyDestMaxSimUpCooldown);
      Debug(dpSpam, section, '[MAXSIM COOLDOWN] Destination site %s is on MaxSim UP cooldown (%ds remaining), skipping %s',
        [s2.Name, s2.MaxSimUpCooldownRemainingSeconds, t.FullName]);
      exit;
    end;

    if s1.MaxSimDownCooldownActive then
    begin
      Inc(fQueueStat.FBusyDestMaxSimDownCooldown);
      Debug(dpSpam, section, '[MAXSIM COOLDOWN] Source site %s is on MaxSim DOWN cooldown (%ds remaining), skipping %s',
        [s1.Name, s1.MaxSimDownCooldownRemainingSeconds, t.FullName]);
      exit;
    end;

    if fBusyDestinations.ContainsKey(s2) then
    begin
      Inc(fQueueStat.FBusyDestinationsHitCount);
      Inc(fQueueStat.FBusyDestBusyDict);
      Debug(dpSpam, section, 'Destination site %s is busy, skip race task assign from %s', [s2.Name, s1.Name]);
      exit;
    end;

    // first watch if it is not already in process to upload the same file to the same place
    if t.ps2.HasActiveTransfer(t.dir + t.filename) then
    begin
      Inc(fQueueStat.FBusyDestActiveTransferDst);
      exit; // we are already sending this file to the same destination site
    end;

    if s2.num_up >= s2.max_up then
    begin
      Inc(fQueueStat.FBusyDestMaxUp);
      exit;
    end;

    if t.ps1.HasActiveTransfer(t.dir + t.filename, s2.Name) then
    begin
      Inc(fQueueStat.FBusyDestActiveTransferSrc);
      exit; // we are already sending this file the opposite route
    end;

    // or use 'if t.ps1.StatusRealPreOrShouldPre then' from pazo.pas but will also pre true when status = rssShouldPre
    //if t.ps1.status = rssRealPre then
    if t.ps1.StatusRealPreOrShouldPre then
    begin
      if s1.num_dn >= s1.max_pre_dn then
      begin
        Inc(fQueueStat.FBusyDestMaxDn);
        exit;
      end;
    end
    else
    begin
      if s1.num_dn >= s1.max_dn then
      begin
        Inc(fQueueStat.FBusyDestMaxDn);
        exit;
      end;
    end;

    ss1 := nil;
    for i := 0 to s1.slots.Count - 1 do
    begin
      ss1 := TSiteSlot(s1.slots[i]);
      if ss1.todotask = nil then
      begin
        if ss1.status = ssOnline then
        begin
          // siteslot is online and available for a new task
          break;
        end
        else
        begin
          // siteslot is not online
          ss1 := nil;
        end;
      end
      else
      begin
        // siteslot is already busy
        ss1 := nil;
      end;
    end;
    if ss1 = nil then
    begin
      Inc(fQueueStat.FBusyDestNoSlotSource);
      exit;
    end;


    if not s2.AcquireSlotsAssignmentLock(1, 'TryToAssignRaceSlots') then
    begin
      fBusyDestinations.Add(s2, 0);
      Inc(fQueueStat.FBusyDestinationsHitCount);
      Inc(fQueueStat.FBusyDestBusyDict);
      exit;
    end;

    try
      // check again now that we have the lock at the destination
      if s2.num_up >= s2.max_up then
      begin
        Inc(fQueueStat.FBusyDestMaxUp);
        exit;
      end;

      // again check if this file is already being sent to the destination now that we have the slot assignment lock
      if t.ps2.HasActiveTransfer(t.dir + t.filename) then
      begin
        Inc(fQueueStat.FBusyDestActiveTransferDst);
        exit; // we are already sending this file to the same destination site
      end;

      ss2 := nil;
      for fSiteSlotLoop in s2.slots do
      begin
        if (fSiteSlotLoop.todotask = nil) and (fSiteSlotLoop.status = ssOnline) then
        begin
          // available slot we might use
          ss2 := fSiteSlotLoop;
          break;
        end;
      end;
      if ss2 = nil then
      begin
        Inc(fQueueStat.FBusyDestNoSlotDest);
        exit;
      end;

      // now you can relax, just check if you don't abuse your max simultaneous uploads for a rip
      i := ss2.site.MaxUpPerRip;
      if ((i > 0) and (t.ps2.ActiveTransferCount >= i)) then
      begin
        Inc(fQueueStat.FBusyDestMaxUpPerRip);
        Debug(dpSpam, section, 'We shouldnt upload more than maxupperrip value [' + IntToStr(i) + '] for' + ss2.Name);
        exit;
      end;

      Debug(dpSpam, section, 'FOUND SLOTS FOR ' + t.FullName + ': ' + ss1.Name + ' ' + ss2.Name);
      if t.assigned = 0 then
      begin
        t.assigned := Now;
        fQueueStat.FTaskWaitMsTotal := fQueueStat.FTaskWaitMsTotal + MilliSecondsBetween(t.assigned, t.created);
        Inc(fQueueStat.FTaskWaitCount);
      end;
      Inc(fQueueStat.FTasksAssignedThisRun);
      fQueueStat.AddRaceToHistory;
      fPartnerTask := TWaitTask.Create(t.netname, t.channel, t.site2);
      try
        TWaitTask(fPartnerTask).assigned := Now;
        TWaitTask(fPartnerTask).wait_for := t.Name;
        TWaitTask(fPartnerTask).race_task_uid := t.uid;
        TWaitTask(fPartnerTask).mainpazo := t.mainpazo;
        TWaitTask(fPartnerTask).slot1 := ss2;
        fWaitUid := TWaitTask(fPartnerTask).uid;
        AddTask(fPartnerTask);
        // Only link after successful add; AddTask may free duplicates.
        t.dst_uid := fWaitUid;
        if (t.mainpazo <> nil) and (t.mainpazo.TaskGraph <> nil) and
           (GlTaskRegistry <> nil) and (GlTaskRegistry.Lookup(fWaitUid) <> nil) then
        begin
          t.mainpazo.TaskGraph.AddTask(fWaitUid);
          // Use AddDependencyIfExists because the race task may finish and be
          // removed from the graph before we can register the dependency here.
          // The wait task is still woken by the race task via dst_uid/event.
          t.mainpazo.TaskGraph.AddDependencyIfExists(fWaitUid, t.uid);
          Debug(dpMessage, section, Format('[PAZOCASCADE] pazo_id=%d type=WaitCreated race_uid=%d wait_uid=%d',
            [t.mainpazo.pazo_id, t.uid, fWaitUid]));
        end;
      except
        on e: Exception do
        begin
          // If AddTask did not keep it, free it ourselves.
          if GlTaskRegistry.Lookup(TWaitTask(fPartnerTask).uid) = nil then
            FreeAndNil(fPartnerTask)
          else
            fPartnerTask := nil;
          raise;
        end;
      end;
      t.ps2.AddActiveTransfer(t.dir + t.filename, s1.Name);
      t.slot1      := ss1;
      t.slot1name  := ss1.Name;
      t.slot2      := ss2;
      t.slot2name  := ss2.Name;
      ss1.downloadingfrom := True;
      ss2.uploadingto := True;
      ss1.todotask := t;
      ss2.todotask := GlTaskRegistry.Lookup(t.dst_uid);
      Debug(dpMessage, section, '[RACE ASSIGNED] %s (uid=%d) src=%s dst=%s', [t.FullName, t.uid, ss1.Name, ss2.Name]);
      ss2.Fire;
      ss1.Fire;
    finally
      s2.ReleaseSlotsAssignmentLock;
    end;
  except
  on e: Exception do
    begin
      Debug(dpError, section, '[EXCEPTION] TQueueThread.TryToAssignRaceSlots : %s', [e.Message]);
      exit;
    end;
  end;
end;

procedure TQueueThread.TryToAssignLoginSlot(t: TLoginTask);
var
  s:   TSite;
  i:   integer;
  ss:  TSiteSlot;
  bnc: String;
begin
  ss := nil;
  try
    s := TSite(t.ssite1);
    bnc := '';

    if (t.wantedslot <> '') then
    begin
      ss := FindSlotByName(t.wantedslot);
      if (ss = nil) then
        //invalid slot name, should not happen, just exit here
        exit;
      if (ss.todotask <> nil) then
        exit;  //the slot is already in use, cannot assign the login task
    end
    else
    begin
      for i := 0 to s.slots.Count - 1 do
      begin
        try
          if i > s.slots.Count then
            Break;
        except
          Break;
        end;
        ss := TSiteSlot(s.slots[i]);
        if ss.Status = ssOnline then
          bnc := ss.bnc;

        if ss.todotask <> nil then
        begin
          ss := nil;
          Continue;
        end;

        if t.kill then
        begin
          // if we want to kill ghost connections, we would also want to do that on an online slot
          Break;
        end;

        if ss.Status <> ssOnline then
          Break
        else
          ss := nil;
      end;
    end;

    if ss = nil then
    begin
      if t.kill then
      begin
        Debug(dpError, section, 'GhostKill %s: no free slot found, ghost kill skipped', [t.site1]);
        if not t.noannounce then
          irc_Addtext(t, '<c4>Unable to kill ghosts on <b>%s</b>: all slots busy</c>', [t.site1]);
      end
      else if not t.noannounce then
      begin
        if bnc = '' then
          irc_Addtext(t, '<b>%s</b> IS ALREADY BEING TESTED', [t.site1])
        else
          irc_Addtext(t, '<b>%s</b> IS ALREADY UP: %s', [t.site1, bnc]);
      end;
      if not t.kill then
      begin
        s.WorkingStatus := sstUp;
        debug(dpMessage, section, '%s IS UP', [t.site1]);
      end;
      t.ready := True;
      exit;
    end;

    Debug(dpSpam, section, 'FOUND LOGINSLOT FOR ' + t.Name + ': ' + ss.Name);
    t.slot1     := ss;
    t.slot1name := ss.Name;
    t.assigned  := Now;
    ss.todotask := t;
    ss.Fire;
  except
  on e: Exception do
    begin
      Debug(dpError, section, '[EXCEPTION] TQueueThread.TryToAssignLoginSlot : %s', [e.Message]);
      exit;
    end;
  end;
end;

procedure TQueueThread.TryToAssignSlots(t: TTask);
var
  s:   TSite;
  i:   integer;
  ss:  TSiteSlot;
  sst: TSiteSlot;
  actual_count: integer;
begin
   // Debug(dpSpam, section, 'TryToAssignSlots profile '+t.Fullname);

  try
    s := TSite(self.fSite);
    s.AcquireSlotsAssignmentLock('TryToAssignSlots');
    try
    if s.freeslots = 0 then
      exit;

    if t.wanted_up and s.MaxSimUpCooldownActive then
    begin
      Debug(dpSpam, section, '[MAXSIM COOLDOWN] Site %s is on MaxSim UP cooldown (%ds remaining), skip task %s',
        [s.Name, s.MaxSimUpCooldownRemainingSeconds, t.FullName]);
      exit;
    end;

    if t.wanted_dn and s.MaxSimDownCooldownActive then
    begin
      Debug(dpSpam, section, '[MAXSIM COOLDOWN] Site %s is on MaxSim DOWN cooldown (%ds remaining), skip task %s',
        [s.Name, s.MaxSimDownCooldownRemainingSeconds, t.FullName]);
      exit;
    end;

      Inc(t.TryToAssign);
      if ((maxassign <> 0) and (t.TryToAssign > maxassign)) then
      begin
        t.TryToAssign := 0;
        if (maxassign_delay = 0) then
        begin
          t.ready := True;
        end
        else
        begin
          t.startat := IncSecond(Now(), maxassign_delay);
        end;
        exit;
      end;

      if t.ClassType = TPazoRaceTask then
      begin
        TryToAssignRaceSlots(TPazoRaceTask(t));
        exit;
      end;

      if t is TLoginTask then
      begin
        if (t.wantedslot <> '') then
        begin
          TryToAssignLoginSlot(TLoginTask(t));
          exit;
        end;
      end;

      if t.ClassType = TPazoDirlistTask then
      begin
        actual_count := 0;
        for i := 0 to s.slots.Count - 1 do
        begin
          try
            if i > s.slots.Count then
              Break;
          except
            Break;
          end;
          sst := TSiteSlot(s.slots[i]);
          try
          if ((sst.todotask <> nil) and (sst.todotask.ClassType = TPazoDirlistTask)) then
          begin
            Inc(actual_count);
          end;
          except
          on e: Exception do
            begin
              Debug(dpError, section, '[EXCEPTION] This should not happen anymore due to locking at todotask := nil. Else I don''t know why (Remove this if the exception never happens) : %s', [e.Message]);
              try
                Debug(dpError, section, '[DIAG] todotask AV context: site=%s slotidx=%d/%d sst_addr=%p todotask_addr=%p sst_name=%s', [s.Name, i, s.slots.Count, Pointer(sst), Pointer(sst.todotask), sst.Name]);
              except
                on e2: Exception do
                begin
                  Debug(dpError, section, '[DIAG] todotask AV context: site=%s slotidx=%d/%d sst_addr=%p todotask_addr=%p sst_name=INVALID (%s)', [s.Name, i, s.slots.Count, Pointer(sst), Pointer(sst.todotask), e2.Message]);
                end;
              end;
            end;
          end;
        end;
        // only half of the slots for dirlist
        if (actual_count >= Max(s.slots.Count div 2, 1)) then
        begin
          exit;
        end;
      end;

      ss := nil;
      if t.wantedslot <> '' then
      begin
        ss := FindSlotByName(t.wantedslot);
        if (ss = nil) then
        begin
          t.readyerror := True;
          exit;
        end;
        if (ss.todotask <> nil) or (ss.status <> ssOnline) then
          exit;
      end;

      // try to find a free and online slot
      if ss = nil then
      begin
        for sst in s.slots do
        begin
          if (sst.todotask = nil) and ((sst.status = ssOnline) or (t is TLoginTask)) then
          begin
            ss := sst;
            break;
          end;
        end;

        if ss = nil then
          exit;
      end;

      if ((t.wanted_dn) or (t.wanted_up)) then
      begin
        if t.wanted_dn then
        begin

          // or use 'if t.ps1.StatusRealPreOrShouldPre then' from pazo.pas but will also pre true when status = rssShouldPre
          //if t.ps1.status = rssRealPre then
          (*
          *
          * not working right now because we only have access to TSite & TSiteSlot but no chance to get rls by
          * them to call pazosite to get infos about affil or not :(
          *
          if t.ps1.StatusRealPreOrShouldPre then
          begin
            if s.num_dn >= ss.site.max_pre_dn then
              exit;
          end
          else
          begin
            if s.num_dn >= ss.site.max_dn then
              exit;
          end;
          *)

        //OLD CODE before max_pre_dn was added
          if s.num_dn >= ss.site.max_dn then
            exit;


          ss.downloadingfrom := True;

        end
        else
        if t.wanted_up then
        begin
          if s.num_up >= ss.site.max_up then
            exit;
          ss.uploadingto := True;
        end;
      end;

      Debug(dpSpam, section, 'FOUND SLOT FOR ' + t.FullName + ': ' + ss.Name);
      t.slot1     := ss;
      t.slot1name := ss.Name;
      if t.assigned = 0 then
      begin
        t.assigned := Now;
        fQueueStat.FTaskWaitMsTotal := fQueueStat.FTaskWaitMsTotal + MilliSecondsBetween(t.assigned, t.created);
        Inc(fQueueStat.FTaskWaitCount);
      end;
      Inc(fQueueStat.FTasksAssignedThisRun);
      ss.todotask := t;
      fExecutingTask := t;
      fExecutingTaskAllowedDuplicate := True;
      ss.Fire;
    finally
      if t.slot1 = nil then
        Inc(fQueueStat.FTasksSkippedThisRun);
      s.ReleaseSlotsAssignmentLock;
    end;
  except
  on e: Exception do
    begin
      Debug(dpError, section, '[EXCEPTION] TQueueThread.TryToAssignSlots : %s', [e.Message]);
    end;
  end;
end;

procedure AddQuitTask(s: TSiteSlot);
var
  q: TQuitTask;
begin
  try
    q := TQuitTask.Create('', '', s.site.Name);
    q.slot1 := s;
    q.slot1name := s.Name;
    s.todotask := q;
    AddTask(q);
    s.Fire;
  except
    on e: Exception do
    begin
      Debug(dpError, section, '[EXCEPTION] TQueueThread.AddQuitTask : %s', [e.Message]);
    end;
  end;
end;

procedure AddIdleTask(const s: TSiteSlot; const aQueue: TQueueThread);
var
  ti: TIdleTask;
  tt: TTask;
begin
  try
    for tt in aQueue.tasks do
    begin
      if ((tt.ClassType = TIdleTask) and (tt.slot1name = s.Name)) then
      begin
        exit;
      end;
    end;

    ti := TIdleTask.Create('', '', s.site.Name);
    ti.slot1 := s;
    ti.slot1name := s.Name;
    aQueue.fQueueStat.FSlotIdleMsTotal := aQueue.fQueueStat.FSlotIdleMsTotal + MilliSecondsBetween(Now, s.LastNonIdleTaskExecution);
    Inc(aQueue.fQueueStat.FSlotIdleCount);
    s.todotask := ti;
    AddTask(ti);
    s.Fire;
  except
    on e: Exception do
    begin
      Debug(dpError, section, '[EXCEPTION] TQueueThread.AddIdleTask : %s', [e.Message]);
    end;
  end;
end;

function IsSlotReadyForQuitTask(const aSlot: TSiteSlot; const aQueueLastRun: TDateTime): boolean;
begin
  Result := (aSlot.status = ssOnline) and ((aSlot.site.WorkingStatus in [sstMarkedAsDownByUser]) or ((aSlot.site.maxidle <> 0) and
              (MilliSecondsBetween(aQueueLastRun, aSlot.LastNonIdleTaskExecution) >= aSlot.site.maxidle * 1000)));
end;

function IsSlotReadyForIdleTask(const aSlot: TSiteSlot; const aQueueLastRun: TDateTime): boolean;
begin
  Result := ((aSlot.status = ssOnline) or ((aSlot.site.WorkingStatus in [sstUp]) and
              ((aSlot.site.maxidle = 0) or (MilliSecondsBetween(aQueueLastRun, aSlot.LastNonIdleTaskExecution) < aSlot.site.maxidle * 1000))))
              and (MilliSecondsBetween(aQueueLastRun, aSlot.LastIO) > aSlot.site.idleinterval * 1000);
end;

// IT IS ONLY GIVEN TO CALL
procedure TQueueThread.QueueEmpty(const sitename: String);
var
  t: TTask;
  fSetDownPazo: TList<TPazo>;
  fPazo: TPazo;
  fListIndex: Integer;
  fList: TObjectList;
begin
  fSetDownPazo := TList<TPazo>.Create;
  try
    main_lock.Enter('QueueEmpty');
    try
      for fListIndex := 0 to 1 do
      begin
        if fListIndex = 0 then fList := tasks else fList := waiting_tasks;
        for t in fList do
        begin
        if ((not t.ready) and (t.slot1 = nil) and (not t.dontremove) and ((t.site1 = sitename) or (t.site2 = sitename))) then
          t.readyerror := True;

        if (t is TPazoTask) and not fSetDownPazo.Contains(TPazoTask(t).mainpazo) then
          fSetDownPazo.Add(TPazoTask(t).mainpazo);
      end;
      end;
    finally
      main_lock.Leave;
    end;

    for fPazo in fSetDownPazo do
    begin
      fPazo.SiteDown(sitename);
    end;
  finally
    fSetDownPazo.Free;
  end;

  Debug(dpSpam, section, 'QueueEmpty end: ' + sitename);
end;

function TQueueThread.RaceTaskKey(const t: TPazoRaceTask): String;
begin
  Result := IntToStr(t.pazo_id) + #0 + t.site1 + #0 + t.site2 + #0 + t.dir + #0 + t.filename;
end;

function TQueueThread.DirlistTaskKey(const t: TPazoDirlistTask): String;
begin
  Result := IntToStr(t.pazo_id) + #0 + t.site1 + #0 + t.dir;
end;

function TQueueThread.MkdirTaskKey(const t: TPazoMkdirTask): String;
begin
  Result := IntToStr(t.pazo_id) + #0 + t.site1 + #0 + t.dir;
end;

function TQueueThread.LoginTaskKey(const t: TLoginTask): String;
begin
  Result := t.site1 + #0 + t.wantedslot + #0 + BoolToStr(t.readd, True) + #0 + BoolToStr(t.kill, True);
end;

procedure TQueueThread.AddTaskToIndex(const t: TTask);
begin
  if t is TPazoRaceTask then
    fRaceTaskSet.Add(RaceTaskKey(TPazoRaceTask(t)))
  else if t is TPazoDirlistTask then
    fDirlistTaskSet.Add(DirlistTaskKey(TPazoDirlistTask(t)))
  else if t is TPazoMkdirTask then
    fMkdirTaskSet.Add(MkdirTaskKey(TPazoMkdirTask(t)))
  else if t is TLoginTask then
    fLoginTaskSet.Add(LoginTaskKey(TLoginTask(t)));
end;

procedure TQueueThread.RemoveTaskFromIndex(const t: TTask);
var
  fIdx: Integer;
  fKey: String;
begin
  try
    if t is TPazoRaceTask then
    begin
      fKey := RaceTaskKey(TPazoRaceTask(t));
      fIdx := fRaceTaskSet.IndexOf(fKey);
      if fIdx >= 0 then
      begin
        fRaceTaskSet.Delete(fIdx);
        // Trim capacity to avoid a memory leak: TStringList keeps the internal
        // buffer even when all entries are deleted. Trim aggressively so the
        // capacity stays close to the actual count under high churn.
        if fRaceTaskSet.Capacity > fRaceTaskSet.Count * 2 + 16 then
          fRaceTaskSet.Capacity := fRaceTaskSet.Count;
      end;
    end
    else if t is TPazoDirlistTask then
    begin
      fKey := DirlistTaskKey(TPazoDirlistTask(t));
      fIdx := fDirlistTaskSet.IndexOf(fKey);
      if fIdx >= 0 then
      begin
        fDirlistTaskSet.Delete(fIdx);
        if fDirlistTaskSet.Capacity > fDirlistTaskSet.Count * 2 + 16 then
          fDirlistTaskSet.Capacity := fDirlistTaskSet.Count;
      end;
    end
    else if t is TPazoMkdirTask then
    begin
      fKey := MkdirTaskKey(TPazoMkdirTask(t));
      fIdx := fMkdirTaskSet.IndexOf(fKey);
      if fIdx >= 0 then
      begin
        fMkdirTaskSet.Delete(fIdx);
        if fMkdirTaskSet.Capacity > fMkdirTaskSet.Count * 2 + 16 then
          fMkdirTaskSet.Capacity := fMkdirTaskSet.Count;
      end;
    end
    else if t is TLoginTask then
    begin
      fKey := LoginTaskKey(TLoginTask(t));
      fIdx := fLoginTaskSet.IndexOf(fKey);
      if fIdx >= 0 then
      begin
        fLoginTaskSet.Delete(fIdx);
        if fLoginTaskSet.Capacity > fLoginTaskSet.Count * 2 + 16 then
          fLoginTaskSet.Capacity := fLoginTaskSet.Count;
      end;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] RemoveTaskFromIndex: %s', [E.Message]));
    end;
  end;
end;

// Helper to verify that a task with the given index key still exists in a task list.
// Used to detect stale index entries caused by tasks removed without updating the index.
function TQueueThread.IsExecutingTaskWithKey(const t: TTask; const aKey: String): Boolean;
var
  fExecKey: String;
begin
  Result := False;
  if fExecutingTask = nil then
    Exit;
  if fExecutingTask.ClassType <> t.ClassType then
    Exit;

  if t is TPazoRaceTask then
    fExecKey := RaceTaskKey(TPazoRaceTask(fExecutingTask))
  else if t is TPazoDirlistTask then
    fExecKey := DirlistTaskKey(TPazoDirlistTask(fExecutingTask))
  else if t is TPazoMkdirTask then
    fExecKey := MkdirTaskKey(TPazoMkdirTask(fExecutingTask))
  else if t is TLoginTask then
    fExecKey := LoginTaskKey(TLoginTask(fExecutingTask))
  else
    Exit;

  Result := fExecKey = aKey;
end;

function TQueueThread.TaskAlreadyInQueue(t: TTask): boolean;
{$IFDEF DISABLE_DEDUP}
begin
  // TEMPORARY: deduplication disabled for performance root-cause analysis.
  Result := False;
end;
{$ELSE}
var
  fKey: String;
begin
  Result := False;
  try
    if t is TPazoRaceTask then
    begin
      {$IFDEF DEDUP_RACE}
      fKey := RaceTaskKey(TPazoRaceTask(t));
      Result := fRaceTaskSet.IndexOf(fKey) >= 0;
      {$ENDIF}
    end
    else if t is TPazoDirlistTask then
    begin
      {$IFDEF DEDUP_DIRLIST}
      fKey := DirlistTaskKey(TPazoDirlistTask(t));
      Result := fDirlistTaskSet.IndexOf(fKey) >= 0;
      {$ENDIF}
    end
    else if t is TPazoMkdirTask then
    begin
      {$IFDEF DEDUP_MKDIR}
      fKey := MkdirTaskKey(TPazoMkdirTask(t));
      Result := fMkdirTaskSet.IndexOf(fKey) >= 0;
      {$ENDIF}
    end
    else if t is TLoginTask then
    begin
      {$IFDEF DEDUP_LOGIN}
      fKey := LoginTaskKey(TLoginTask(t));
      Result := fLoginTaskSet.IndexOf(fKey) >= 0;
      {$ENDIF}
    end;

    // A task that is currently executing may legitimately create exactly one
    // follow-up task with the same key (e.g. a dirlist re-read). Allow that
    // single duplicate and then block further identical tasks until the
    // executing task has been removed from the queue.
    if Result and (fKey <> '') and fExecutingTaskAllowedDuplicate and
       IsExecutingTaskWithKey(t, fKey) then
    begin
      Result := False;
      fExecutingTaskAllowedDuplicate := False;
    end;

  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TaskAlreadyInQueue index lookup: %s', [e.Message]));
      Result := False;
    end;
  end;
end;
{$ENDIF}

procedure AddTaskToConsole(const aTask: TTask);
var
  fTaskUid, fTaskName: string;
begin
  try
    fTaskUid := aTask.UidText;
    fTaskName := aTask.Name;
  except
    on e: Exception do
    begin
      // it seems this could happen when the task has been freed already (because we are not inside queue lock here).
      Debug(dpSpam, section, Format('[EXCEPTION] AddTaskToConsole task not available : %s', [e.Message]));
      exit;
    end;
  end;
  Console_QueueAdd(fTaskUid, Format('%s', [fTaskName]));
end;

procedure TQueueThread.AddTask(t: TTask);
var
  tname: String;
  fCheckSiteSlotsSite: TSite;
begin
  try
    fCheckSiteSlotsSite := nil;
    tname := t.Name;

    //do this check before the task might have been freed already
    //for races (pazo tasks) the site slots are checked when the site is added to the race,
    //check here for any other tasks that might come along
    if (t.ssite1 <> nil) and
      (((not (t is TPazoPlainTask)) and (not (t is TWaitTask)))

      //if the site has a max idle time, also do the slots check for race/wait tasks.
      //The slots might reach idle time at any time even during a race.
      //The CheckSiteSlots procedure will only login one additional slot for sites with a maxidle setting
      or (TSite(t.ssite1).maxidle <> 0))

      //never do this for login, quit and idle tasks because it doesn't make sense
      and (not (t is TLoginTask)) and (not (t is TQuitTask)) and (not (t is TIdleTask)) then
    begin
      fCheckSiteSlotsSite := t.ssite1;
    end;

    Debug(dpSpam, section, Format('[iNFO] adding : %s', [t.Name]));

    // Phase 2 diagnostics: count task creation by type.
    if t is TPazoRaceTask then
      Inc(fQueueStat.FTasksCreatedRace)
    else if t is TPazoDirlistTask then
      Inc(fQueueStat.FTasksCreatedDirlist)
    else if t is TPazoMkdirTask then
      Inc(fQueueStat.FTasksCreatedMkdir)
    else if t is TLoginTask then
      Inc(fQueueStat.FTasksCreatedLogin)
    else
      Inc(fQueueStat.FTasksCreatedOther);

    main_lock.Enter('AddTask');
    try
      if TaskAlreadyInQueue(t) then
      begin
        // Phase 2 diagnostics: count discarded duplicates by type.
        if t is TPazoRaceTask then
          Inc(fQueueStat.FTasksDupRace)
        else if t is TPazoDirlistTask then
          Inc(fQueueStat.FTasksDupDirlist)
        else if t is TPazoMkdirTask then
          Inc(fQueueStat.FTasksDupMkdir)
        else if t is TLoginTask then
          Inc(fQueueStat.FTasksDupLogin);

        // don't add the task to the queue, just notify and free right away if it's a duplicate
        if t.IsNotifyTask then
          TaskReady(t);

        // If the task was already registered in the pazo dependency graph, remove
        // it there as well so we don't leak pending nodes for discarded duplicates.
        if (t is TPazoTask) and (TPazoTask(t).mainpazo <> nil) and (TPazoTask(t).mainpazo.TaskGraph <> nil) then
          TPazoTask(t).mainpazo.TaskGraph.RemoveTask(t.uid);

        t.Free;
        exit;
      end;

      // Add to waiting_tasks if it starts in the future, else to main tasks queue
      if (t.startat > Now) then
        waiting_tasks.Add(t)
      else
        tasks.Add(t);

      // Keep the O(1) duplicate indexes in sync with the task lists.
      AddTaskToIndex(t);

      // A race task was added; ensure FindBestRaceTask will scan on the next
      // queue iteration even if no slot has been freed yet. If it could not be
      // assigned directly, wake the queue so it retries soon.
      if t is TPazoRaceTask then
      begin
        SignalRaceCheck;
        if t.slot1 = nil then
        begin
          IncQueueFireCount(qfsAddTask);
          QueueFire;
        end;
      end;

      try
        if ((t is TPazoRaceTask) and (not t.ready) and t.IsReadyToBeExecuted and
            (TSite(fSite).freeslots > 0) and
            (TSite(TPazoRaceTask(t).ssite2).freeslots > 0)) then
        begin
          TSite(fSite).AcquireSlotsAssignmentLock('AddTask-Slot');
          try
            if ((not t.ready) and t.IsReadyToBeExecuted) then
            begin
              self.TryToAssignSlots(t);
            end;
          finally
            TSite(fSite).ReleaseSlotsAssignmentLock;
          end;
        end;
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[EXCEPTION] AddTask TryToAssignSlots: %s', [e.Message]));
        end;
      end;

    finally
      main_lock.Leave;
    end;

  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] AddTask tasks.Add: %s', [e.Message]));
      exit;
    end;
  end;

  // check if the race has failed on either source or destination site (in case of race tasks). This can happen when a dirlist task is running and
  // adding new race tasks while the mkdir task on the destination fails at the same time and sets the site failed. This would lead to the
  // dependencies of the race task never be resolved and it would remain and pollute the queue.
  if t is TPazoRaceTask then
  begin
    try
      if TPazoRaceTask(t).ps2.error or
        ((TPazoRaceTask(t).dir <> '') and TPazoRaceTask(t).ps2.dirlist.FindDirList(TPazoRaceTask(t).dir).error) then
      begin
        t.readyerror := true;
        Debug(dpSpam, section, Format('AddTask: race failed on source or destination site: %s', [t.Name]));
        exit;
      end;
    except
      on e: Exception do
      begin
        Debug(dpSpam, section, Format('[EXCEPTION] AddTask check for failed pazo: %s', [e.Message]));
        exit;
      end;
    end;
  end;

  if fCheckSiteSlotsSite <> nil then
  begin
    CheckSiteSlots(fCheckSiteSlotsSite);
  end;
  AddTaskToConsole(t);
end;

procedure TQueueThread.RemoveRaceTasks(const pazo_id: integer; const sitename: String);
var
  ttp: TPazoRaceTask;
  fTask: TTask;
  fListIndex: Integer;
  fList: TObjectList;
begin
  try
    main_lock.Enter('RemoveRaceTasks');
    try
      for fListIndex := 0 to 1 do
      begin
        if fListIndex = 0 then fList := tasks else fList := waiting_tasks;
        for fTask in fList do
        begin
        try
          if (fTask is TPazoRaceTask) then
          begin
            ttp := TPazoRaceTask(fTask);
            if ((ttp.ready = False) and (ttp.readyerror = False) and (ttp.slot1 = nil) and (ttp.pazo_id = pazo_id) and (ttp.site2 = sitename)) then
            begin
              ttp.ready := True;
              SignalPairedWaitTask(ttp.uid);
            end;
          end;
        except
          on E: Exception do
          begin
            Debug(dpError, section, Format('[EXCEPTION] RemoveRaceTasks (loop) : %s', [e.Message]));
          end;
        end;
      end;
      end;
    finally
      main_lock.Leave;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] RemoveRaceTasks : %s', [e.Message]));
      exit;
    end;
  end;
end;

procedure TQueueThread.RemovePazoDirTasks(const pazo_id: integer);
var
  ttp: TPazoTask;
  fTask: TTask;
  fListIndex: Integer;
  fList: TObjectList;
begin
  try
    main_lock.Enter('RemovePazoDirTasks');
    try
      for fListIndex := 0 to 1 do
      begin
        if fListIndex = 0 then fList := tasks else fList := waiting_tasks;
        for fTask in fList do
        begin
        try
          if (fTask is TPazoDirlistTask) or (fTask is TPazoMkdirTask) then
          begin
            ttp := TPazoTask(fTask);
            if ((ttp.ready = False) and (ttp.readyerror = False) and (ttp.slot1 = nil) and (ttp.pazo_id = pazo_id)) then
              ttp.ready := True;
          end;
        except
          on E: Exception do
          begin
            Debug(dpError, section, Format('[EXCEPTION] RemoveDirlistTasks : %s', [e.Message]));
          end;
        end;
      end;
      end;
    finally
      main_lock.Leave;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] RemoveDirlistTasks : %s', [e.Message]));
      exit;
    end;
  end;
end;

function TQueueThread.RemovePazo(const pazo_id: integer; const aForce: boolean = False): boolean;
var
  t: TPazoPlainTask;
  fTask: TTask;
  fSlotsToRebuild: TList<TSiteSlot>;
  fSlot: TSiteSlot;
  fListIndex: Integer;
  fList: TObjectList;
begin
  Result := False;
  fSlotsToRebuild := TList<TSiteSlot>.Create;
  try
    main_lock.Enter('RemovePazo');
    try
      for fListIndex := 0 to 1 do
      begin
        if fListIndex = 0 then fList := tasks else fList := waiting_tasks;
        for fTask in fList do
        begin
        try
          if fTask is TPazoPlainTask then
          begin
            t := TPazoPlainTask(fTask);
            if ((t.pazo_id = pazo_id)) then
            begin
              if t.slot1 = nil then
              begin
                t.readyerror := True;
                if t is TPazoRaceTask then
                  SignalPairedWaitTask(TPazoRaceTask(t).uid);
              end
              else if aForce then
              begin
                Debug(dpMessage, section, Format('RemovePazo: Force removal of assigned task: %s', [t.Name]));
                t.readyerror := True;
                if t is TPazoRaceTask then
                  SignalPairedWaitTask(TPazoRaceTask(t).uid);

                if t.slot1 <> nil then
                begin
                  try
                    // Reset the slot's todotask before detaching the task so that
                    // TryToAssignSlots cannot dereference it after the task object is freed.
                    if TSiteSlot(t.slot1).todotask = t then
                    begin
                      TSiteSlot(t.slot1).site.AcquireSlotsAssignmentLock('RemovePazo reset todotask');
                      try
                        if TSiteSlot(t.slot1).todotask = t then
                          TSiteSlot(t.slot1).todotask := nil;
                      finally
                        TSiteSlot(t.slot1).site.ReleaseSlotsAssignmentLock;
                      end;
                    end;
                  except
                    on E: Exception do
                    begin
                      Debug(dpError, section, Format('[EXCEPTION] RemovePazo (reset todotask): %s', [e.Message]));
                    end;
                  end;

                  // if the site slot actually had this task assigned, we need to rebuild it
                  fSlotsToRebuild.Add(TSiteSlot(t.slot1));
                end;

                t.slot1 := nil;
                t.slot2 := nil;
              end;
            end;
          end;
        except
          on E: Exception do
          begin
            Debug(dpError, section, Format('[EXCEPTION] RemovePazo (loop): %s', [e.Message]));
          end;
        end;
      end;
      end;
    finally
      main_lock.Leave;
    end;

    // now rebuild the slot(s) outside of the queue lock
    for fSlot in fSlotsToRebuild do
    begin
      Debug(dpMessage, section, Format('RemovePazo: Rebuild slot with stuck task: %s', [fSlot.Name]));
      irc_Addadmin('[SITESLOT]: Rebuild slot with stuck task: %s', [fSlot.Name]);
      try
        fSlot.site.RebuildSlot(fSlot.SlotNumber);
      except
        on E: Exception do
        begin
          Debug(dpError, section, Format('[EXCEPTION] RemovePazo (RebuildSlot): %s', [e.Message]));
        end;
      end;
    end;
    fSlotsToRebuild.Free;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] RemovePazo : %s', [e.Message]));
      exit;
    end;
  end;
  Result := True;
end;


procedure TQueueThread.RemovePazoMKDIR(const pazo_id: integer; const dir: String);
var
  ttp: TPazoMkdirTask;
  fTask: TTask;
  fListIndex: Integer;
  fList: TObjectList;
begin
  try
    main_lock.Enter('RemovePazoMKDIR');
    try
      for fListIndex := 0 to 1 do
      begin
        if fListIndex = 0 then fList := tasks else fList := waiting_tasks;
        for fTask in fList do
        try
          if (fTask is TPazoMkdirTask) then
          begin
            ttp := TPazoMkdirTask(fTask);
            if ((ttp.ready = False) and (ttp.readyerror = False) and
              (ttp.slot1 = nil) and (ttp.pazo_id = pazo_id) and
              (ttp.dir = dir)) then
            begin
              ttp.ready := True;
            end;
          end;
        except
          Continue;
        end;
      end;
    finally
      main_lock.Leave;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] RemovePazoMKDIR : %s', [e.Message]));
    end;
  end;
end;

procedure TQueueThread.RemovePazoSfv(const aPazoID: integer; const aDir: String);
var
  fTask: TPazoSiteSfvTask;
  fAbstractTask: TTask;
  fListIndex: Integer;
  fList: TObjectList;
begin
  try
    main_lock.Enter('RemovePazoSfv');
    try
      for fListIndex := 0 to 1 do
      begin
        if fListIndex = 0 then fList := tasks else fList := waiting_tasks;
        for fAbstractTask in fList do
        begin
        if (fAbstractTask is TPazoSiteSfvTask) then
        begin
          fTask := TPazoSiteSfvTask(fAbstractTask);
          if ((fTask.ready = False) and (fTask.readyerror = False) and (fTask.slot1 = nil) and (fTask.pazo_id = aPazoID) and (fTask.dir = aDir)) then
          begin
            fTask.ready := True;
            Debug(dpSpam, 'sfv', Format('Remove SFV task : %s %s %s (%s)', [fTask.mainpazo.rls.rlsname, fTask.dir, fTask.SFVFilename, fTask.site1]));
          end;
        end;
      end;
      end;
    finally
      main_lock.Leave;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] RemovePazoSfv : %s', [e.Message]));
    end;
  end;
end;

procedure TQueueThread.RemovePazoRace(const pazo_id: integer; const dstsite, dir, filename: String);
var
  ttp: TPazoRaceTask;
  fTask: TTask;
  fListIndex: Integer;
  fList: TObjectList;
begin
  try
    main_lock.Enter('RemovePazoRace');
    try
      for fListIndex := 0 to 1 do
      begin
        if fListIndex = 0 then fList := tasks else fList := waiting_tasks;
        for fTask in fList do
        begin
        try
          if (fTask is TPazoRaceTask) then
          begin
            ttp := TPazoRaceTask(fTask);
            if ((ttp.ready = False) and (ttp.readyerror = False) and
              (ttp.slot1 = nil) and (ttp.pazo_id = pazo_id) and (ttp.site2 = dstsite) and
              (ttp.dir = dir) and (ttp.filename = filename)) then
            begin
              ttp.ready := True;
              SignalPairedWaitTask(ttp.uid);
            end;
          end;
        except
          on E: Exception do
          begin
            Debug(dpError, section, Format('[EXCEPTION] RemovePazoRace : %s', [e.Message]));
            Continue;
          end;
        end;
      end;
      end;
    finally
      main_lock.Leave;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] RemovePazoRace : %s', [e.Message]));
    end;
  end;
end;

procedure TQueueThread.Execute;
var
  i: integer;
  fTask:    TTask;
  fPartnerTask: TTask;
  s:    TSiteSlot;
  ss:   String;
  ts:   TSite;
  fBusyDestinationsTmp: TDictionary<TObject, integer>;
  fNextTaskStartAt: TDateTime;
  fWaitTimerTimeout: Cardinal;
  bTasksMoved: Boolean;
  fTasksMovedCount: Integer;
  fListIndex: Integer;
  fList: TObjectList;
  fMethodStart: TDateTime;
begin
  while ((not slshutdown) and (not Terminated)) do
  begin
    queue_last_run := Now();
    fQueueIterateStart := queue_last_run;

    if fSite = nil then
      fSite := FindSiteByName('', fSiteName);

    if fSite = nil then
    begin
      //happens on startup
      Debug(dpSpam, section, 'Queue Iteration: Wait for site]');
      Sleep(1000);
      continue;
    end;

    ss := '';
    ts := TSite(fSite);
    fBusyDestinationsTmp := fBusyDestinations;
    fBusyDestinations := TDictionary<TObject, integer>.Create;
    fNextTaskStartAt := MaxDateTime;
    bTasksMoved := False;
    fTasksMovedCount := 0;
    //Debug(dpSpam, section, 'Queue Iteration begin (%s) [%d tasks]', [ts.Name, tasks.Count]);
    try
      main_lock.Enter('Execute');
      try
        // Move mature tasks from waiting_tasks to main tasks queue
        for i := waiting_tasks.Count - 1 downto 0 do
        begin
          if i < 0 then Break;
          fTask := TTask(waiting_tasks.items[i]);
          if fTask = nil then Continue;
          
          if ((fTask.startat = 0) or (fTask.startat <= queue_last_run)) then
          begin
            waiting_tasks.Extract(fTask);
            tasks.Add(fTask);
            Inc(fTasksMovedCount);
            Inc(fTasksMovedSinceSort);
            bTasksMoved := True;
          end
          else if (fTask.startat > 0) and (fTask.startat < fNextTaskStartAt) then
          begin
            fNextTaskStartAt := fTask.startat;
          end;
        end;

        if bTasksMoved then
        begin
          // Avoid sorting on every single task move. Sort if enough tasks
          // accumulated since the last sort or if the queue hasn't been sorted
          // recently. This cuts overhead when tasks trickle in one by one.
          if (fTasksMovedSinceSort >= 3) or (MilliSecondsBetween(Now, fLastSortTime) >= 500) then
          begin
            Debug(dpMessage, section, '[TASKS MOVED] %s: moved %d tasks from waiting to tasks (waiting left=%d, tasks now=%d)',
              [ts.Name, fTasksMovedCount, waiting_tasks.Count, tasks.Count]);
            Inc(fQueueStat.FQueueSortCount);
            fQueueStat.FQueueSortMsTotal := fQueueStat.FQueueSortMsTotal + MilliSecondsBetween(Now, queue_last_run);
            tasks.Sort(@QueueSorter);
            fLastSortTime := Now;
            fTasksMovedSinceSort := 0;
          end;
        end;

        for fListIndex := 0 to 1 do
        begin
          if fListIndex = 0 then fList := tasks else fList := waiting_tasks;
          for i := fList.Count - 1 downto 0 do
          begin
            if i < 0 then
              Break;

            fTask := TTask(fList.items[i]);

          if fTask = nil then
            Continue;

          try
            if (((fTask.ready) or (fTask.readyerror)) and (fTask.slot1 = nil)) then
            begin
              ss := fTask.uidtext;
              if fTask.IsNotifyTask then
                TaskReady(fTask);

              if (fTask.ClassType = TPazoRaceTask) then
              begin
                with TPazoRaceTask(fTask) do
                begin
                  if dst_uid <> 0 then
                  begin
                    fPartnerTask := GlTaskRegistry.Lookup(dst_uid);
                    if (fPartnerTask <> nil) and (fPartnerTask.ClassType = TWaitTask) then
                    begin
                      fPartnerTask.ready := True;
                      TWaitTask(fPartnerTask).event.SetEvent;
                    end;
                    dst_uid := 0;
                  end;
                end;
              end;

              // update the per-pazo dependency graph when a pazo task is removed
              if (fTask is TPazoPlainTask) then
              begin
                with TPazoPlainTask(fTask) do
                begin
                  if (mainpazo <> nil) and (mainpazo.TaskGraph <> nil) and mainpazo.TaskGraph.Contains(uid) then
                  begin
                    if fTask.readyerror then
                      mainpazo.TaskGraph.MarkError(uid)
                    else
                      mainpazo.TaskGraph.MarkDone(uid);
                  end;
                end;
              end;

              // Remove from the O(1) index BEFORE TObjectList.Remove frees the task.
              RemoveTaskFromIndex(fTask);
              if fTask = fExecutingTask then
              begin
                fExecutingTask := nil;
                fExecutingTaskAllowedDuplicate := False;
              end;
              ts.AcquireSlotsAssignmentLock('Queue remove ready tasks');
              try
                fList.Remove(fTask);
              finally
                ts.ReleaseSlotsAssignmentLock;
              end;
              Console_QueueDel(ss);
            end;
          except
            on e: Exception do
            begin
              Debug(dpError, section, Format('[EXCEPTION] TQueueThread.Execute (RemoveReady): %s', [e.Message]));
              Continue;
            end;
          end;
        end;
        end;

        ts.AcquireSlotsAssignmentLock('Queue iterate');
        try
          // Assign the best race task(s) first, but only when a slot has become
          // free since the last check, there are pending race tasks and the source
          // is not already saturated on all download limits.
          // TEST: disable fNeedRaceCheck optimization to see if it causes low transfer rate.
          if HasPendingRaceTasks then
          begin
            if ((ts.max_dn = 0) or (ts.num_dn < ts.max_dn)) or
               ((ts.max_pre_dn = 0) or (ts.num_dn < ts.max_pre_dn)) then
            begin
              fMethodStart := Now;
              while (ts.freeslots > 0) and FindBestRaceTask do
                ;
              fQueueStat.FFindBestRaceMsTotal := fQueueStat.FFindBestRaceMsTotal + MilliSecondsBetween(Now, fMethodStart);
              Inc(fQueueStat.FFindBestRaceCount);
            end;
          end;

          for fTask in tasks do
          begin
            try
              if ts.freeslots = 0 then
              begin
                //Debug(dpSpam, section, Format('No free slots on %s', [ts.Name]));

                // no need to iterate the queue early if there are no free slots.
                // when a slot becomes free, a queue fire is issued.
                fNextTaskStartAt := MaxDateTime;
                break;
              end;

              if ((fTask.slot1 = nil) and (fTask.slot2 = nil) and (not fTask.ready) and
                (not fTask.readyerror)) then
              begin
                if ((fTask.startat = 0) or (fTask.startat <= queue_last_run)) then
                begin
                  // Skip races whose destination is already saturated. FindBestRaceTask
                  // handles the primary assignment; this avoids redundant work for
                  // leftover race tasks in the main loop.
                  if (fTask is TPazoRaceTask) and (TSite(TPazoRaceTask(fTask).ssite2).freeslots = 0) then
                    Continue;

                  if fTask.IsReadyToBeExecuted then
                    TryToAssignSlots(fTask);
                end
                else if (fTask.startat > 0) and (fTask.startat < fNextTaskStartAt) then
                begin
                  fNextTaskStartAt := fTask.startat;
                end;
              end;
            except
              on e: Exception do
              begin
                Debug(dpError, section, Format('[EXCEPTION] TQueueThread.Execute (TryToASsignSlots) : %s', [e.Message]));
                Continue;
              end;
            end;
          end;
        finally
          ts.ReleaseSlotsAssignmentLock;
        end;
      finally
        main_lock.Leave;
        fBusyDestinationsTmp.Free;
      end;

      // We are looking for idle
      // update per-iteration metrics before QueueStat resets them
      fQueueStat.FQueueIterateMsTotal := fQueueStat.FQueueIterateMsTotal + MilliSecondsBetween(Now, fQueueIterateStart);
      Inc(fQueueStat.FQueueIterateCount);

      // Copy per-iteration counters for the in-memory diagnostic log before
      // QueueStat resets them.
      Inc(fDiagIterCount);
      Inc(fDiagIterAssigned, fQueueStat.FTasksAssignedThisRun);
      Inc(fDiagIterSkipped, fQueueStat.FTasksSkippedThisRun);

      fMethodStart := Now;
      QueueStat;
      fQueueStat.FQueueStatMsTotal := fQueueStat.FQueueStatMsTotal + MilliSecondsBetween(Now, fMethodStart);
      Inc(fQueueStat.FQueueStatCount);
      LogDiagSummary;
        for s in ts.slots do
        begin
          try
            if ((s.todotask = nil) and (s.site.Name <> getAdminSiteName)) then
            begin
              if IsSlotReadyForQuitTask(s, queue_last_run) then
              begin
                main_lock.Enter('QuitTask');
                try
                  // because we directly assign the task to the slot, we need the slot assignment lock
                  ts.AcquireSlotsAssignmentLock('QuitTask');
                  try
                    // check again inside lock if it's still relevant
                    if IsSlotReadyForQuitTask(s, queue_last_run) then
                      AddQuitTask(s);
                  finally
                    ts.ReleaseSlotsAssignmentLock;
                  end;
                finally
                  main_lock.Leave;
                end;
              end
              //we also want idle tasks to relogin slots that are not ssOnline but the sites are in WorkingStatus sstUp
              //at startup only few slots are needed (e.g. autologin), but we want all the slots to be ready for action if
              //an idle interval is configured. also there are several occasions where DestroySocket or Quit are invoked
              //on a slot. the IdleTask will take care to relogin these slots as well.
              else if IsSlotReadyForIdleTask(s, queue_last_run) then
              begin
                main_lock.Enter('IdleTask');
                try
                  // because we directly assign the task to the slot, we need the slot assignment lock
                  ts.AcquireSlotsAssignmentLock('IdleTask');
                  try
                    // check again inside lock if it's still relevant
                    if IsSlotReadyForIdleTask(s, queue_last_run) then
                      AddIdleTask(s, self);
                  finally
                    ts.ReleaseSlotsAssignmentLock;
                  end;
                finally
                  main_lock.Leave;
                end;
              end;
            end;
          except
            on e: Exception do
            begin
              Debug(dpError, section, Format('[EXCEPTION] TQueueThread.Execute (idletask) : %s', [e.Message]));
              Continue;
            end;
          end;
        end;

      //Debug(dpSpam, section, 'Queue Iteration end (%s) [%d tasks]', [ts.Name, tasks.Count]);
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] TQueueThread.Execute : %s', [e.Message]));
      end;
    end;

    // if there is a task with a delayed start time, we will wait exactly that long
    if fNextTaskStartAt = MaxDateTime then
      fWaitTimerTimeout := GlDefaultIterationWaitTimeout
    else
    begin
      if fNextTaskStartAt <= Now then  // can happen ...
      begin
        if ts.freeslots = 0 then
        begin
          // no free slots, so we have to wait for a slot to become free and trigger a queue fire
          fWaitTimerTimeout := GlDefaultIterationWaitTimeout;
        end
        else
        begin
          // The scheduled time is already in the past and there are free slots,
          // but the task was not assigned (likely not ready yet). Sleeping a tiny
          // bit avoids a busy-loop without adding meaningful latency.
          Debug(dpSpam, section, Format('TQueueThread.Execute: skip sleep %s', [ts.Name]));
          fWaitTimerTimeout := GlMinQueueWaitTimeout;
        end;
      end
      else
      begin
        fWaitTimerTimeout := MilliSecondsBetween(Now, fNextTaskStartAt);

        // Enforce a small minimum wait to prevent near-zero timeouts from
        // spinning the queue when startat is only milliseconds in the future.
        if fWaitTimerTimeout < GlMinQueueWaitTimeout then
          fWaitTimerTimeout := GlMinQueueWaitTimeout;

        // don't wait longer than the default wait time if that task is supposed to start later than that
        if fWaitTimerTimeout > GlDefaultIterationWaitTimeout then
          fWaitTimerTimeout := GlDefaultIterationWaitTimeout;
      end;
    end;

    //queueevent.WaitFor($FFFFFFFF);
    case queueevent.WaitFor(fWaitTimerTimeout) of
      wrSignaled: { Event fired. Normal exit. }
      begin
        //Debug(dpSpam, section, Format('[QUEUEFIRE received : %s', [ts.Name]));
      end;
      else { Timeout reach }
      begin
        if fWaitTimerTimeout = GlDefaultIterationWaitTimeout then
        begin
          if queue_recycle_post_to_irc then
            irc_Adderror(Format('TQueueThread.Execute: <c2>Force Leave</c>: TQueueThread Recycle 15s (%s)', [self.fSiteName]));
          Debug(dpMessage, section,
            Format('TQueueThread.Execute: Force Leave: TQueueThread Recycle 15s (%s)', [self.fSiteName]));
        end;
      end;
    end;
  end;
end;

procedure QueueInit;
begin

  // config
  maxassign := config.ReadInteger(section, 'maxassign', 200);
  maxassign_delay := config.ReadInteger(section, 'maxassign_delay', 15);
  sample_dirs_priority := config.ReadInteger(section, 'sample_dirs_priority', 1);
  if not (sample_dirs_priority in [0..2]) then
    sample_dirs_priority := 1;

  proof_dirs_priority := config.ReadInteger(section, 'proof_dirs_priority', 2);
  if not (proof_dirs_priority in [0..2]) then
    proof_dirs_priority := 2;

  subs_dirs_priority := config.ReadInteger(section, 'subs_dirs_priority', 2);
  if not (subs_dirs_priority in [0..2]) then
    subs_dirs_priority := 2;

  cover_dirs_priority := config.ReadInteger(section, 'cover_dirs_priority', 2);
  if not (cover_dirs_priority in [0..2]) then
    cover_dirs_priority := 2;

  queueclean_maxrunning := config.ReadInteger('queue', 'queueclean_maxrunning', 900);
  queueclean_unassigned := config.ReadInteger('queue', 'queueclean_unassigned', 600);
  enable_queueclean := config.ReadBool(section, 'enable_queueclean', False);
  queue_recycle_post_to_irc := spamcfg.readbool(section, 'queue_recycle', True);

  StatsList := TObjectList<TQueueStat>.Create(True);
  GlQueueDiag := TQueueDiagLog.Create(1000);
  GlTaskTrace := TTaskTraceLog.Create(config.ReadInteger(section, 'tasktrace_buffersize', 1000));
  GlTaskTrace.Enabled := config.ReadBool(section, 'enable_tasktrace', True);
end;

procedure QueueUninit;
begin
  StatsList.Free;
  GlQueueDiag.Free;
  GlTaskTrace.Free;
end;

procedure TQueueThread.QueueClean(run_now: boolean = False);
var
  i, tkill_unassigne, tkill_race, tkill_other: integer;
  ss: String;
  t:  TTask;
  fPartnerTask: TTask;
  ts, ts2: TSite;
  fListIndex: Integer;
  fList: TObjectList;
begin

  try

  if not enable_queueclean then
  begin
    queueclean_last_run := Now;
    exit;
  end;

  if fSite = nil then
    exit;

  ts := TSite(fSite);
  ss := '';

  //irc_Addconsole('QueueClean: process begin');
  //Debug(dpMessage, section, 'QueueClean begin %d', [tasks.Count]);
  tkill_unassigne := 0;
  tkill_race      := 0;
  tkill_other     := 0;

  try
    // Check old unassigne task
    main_lock.Enter('QueueClean1');
    for fListIndex := 0 to 1 do
    begin
      if fListIndex = 0 then fList := tasks else fList := waiting_tasks;
      for t in fList do
      begin
      try
        ss := t.UidText;
        if ((t.assigned = 0) and not t.dontremove and ((t.startat = 0) or (t.startat <= queue_last_run)) and
          (SecondsBetween(t.created, Now()) >= queueclean_unassigned)) then
        begin
          try
            t.ready := True;
            Debug(dpMessage, section, Format('QueueClean: Remove Unassigned : %s', [t.Fullname]));
          except
            on e: Exception do
            begin
              Debug(dpError, section,
                Format('[EXCEPTION] QueueClean: Exception Remove Unassigned : %s', [e.Message]));
              Break;
            end;
          end;
          Inc(tkill_unassigne);

          Console_QueueDel(ss);
          Debug(dpSpam, section, Format('[QUEUECLEAN] Clean unassigned task : %s', [t.Fullname]));
        end;
      except
        on e: Exception do
        begin
          Debug(dpError, section,
          Format('[EXCEPTION] QueueClean Clean unassigned: Exception : %s', [e.Message]));
          Break;
        end;
      end;
    end;
    end;
  finally
    main_lock.Leave;
  end;

  // Check old tasks, assigned bu long time wait
  main_lock.Enter('QueueClean2');
  try
    for fListIndex := 0 to 1 do
    begin
      if fListIndex = 0 then fList := tasks else fList := waiting_tasks;
      for i := fList.Count - 1 downto 0 do
      begin
        try
        if i < 0 then
          Break;
        except
          Break;
        end;
        t := TTask(fList[i]);
      if ((t.assigned <> 0) and ((t.startat = 0) or (t.startat <= queue_last_run)) and
        (SecondsBetween(t.assigned, Now()) >= queueclean_maxrunning)) then
      begin
        if (t.ClassType = TPazoRaceTask) then
        begin
          ss := t.UidText;
          ts2 := nil;
          ts.AcquireSlotsAssignmentLock('QueueClean race');
          try
            // Invalidate the paired wait task before we drop the race task.
            if TPazoRaceTask(t).dst_uid <> 0 then
            begin
              fPartnerTask := GlTaskRegistry.Lookup(TPazoRaceTask(t).dst_uid);
              if (fPartnerTask <> nil) and (fPartnerTask.ClassType = TWaitTask) then
              begin
                fPartnerTask.ready := True;
                TWaitTask(fPartnerTask).event.SetEvent;
              end;
              TPazoRaceTask(t).dst_uid := 0;
            end;

            if (t.slot1 <> nil) then
            begin
              try
                TSiteSlot(t.slot1).todotask := nil;
                TSiteSlot(t.slot1).downloadingfrom := False;
                TSiteSlot(t.slot1).uploadingto := False;
                t.slot1 := nil;
                t.slot1name := '';
              except
                on e: Exception do
                begin
                  Debug(dpError, section, Format('[EXCEPTION] slot1 QueueClean: Exception : %s', [e.Message]));
                end;
              end;
            end;

            if (t.slot2 <> nil) then
            begin
              try
                TSiteSlot(t.slot2).site.AcquireSlotsAssignmentLock('QueueClean race destination');
                // we were able to get the slots assignment lock. set the site here to release the lock later.
                ts2 := TSiteSlot(t.slot2).site;

                TSiteSlot(t.slot2).todotask := nil;
                TSiteSlot(t.slot2).downloadingfrom := False;
                TSiteSlot(t.slot2).uploadingto := False;
                t.slot2 := nil;
                t.slot2name := '';
              except
                on e: Exception do
                begin
                  Debug(dpError, section, Format('[EXCEPTION] slot2 QueueClean: Exception : %s', [e.Message]));
                end;
              end;
            end;

            try
              Debug(dpSpam, section, Format('[QUEUECLEAN] Clean race task : %s', [t.Fullname]));

              // Remove the race task from the pazo dependency graph so it does not
              // leak as a pending node when the race is killed by QueueClean.
              if (TPazoRaceTask(t).mainpazo <> nil) and (TPazoRaceTask(t).mainpazo.TaskGraph <> nil) and
                 TPazoRaceTask(t).mainpazo.TaskGraph.Contains(t.uid) then
              begin
                try
                  TPazoRaceTask(t).mainpazo.TaskGraph.MarkError(t.uid);
                except
                  on e: Exception do
                    Debug(dpError, section, Format('[EXCEPTION] QueueClean race MarkError: %s', [e.Message]));
                end;
              end;

              Debug(dpMessage, section, Format('QueueClean: Remove : %s', [t.Fullname]));
              // Remove from the O(1) index BEFORE TObjectList.Remove frees the task.
              RemoveTaskFromIndex(t);
              if t = fExecutingTask then
              begin
                fExecutingTask := nil;
                fExecutingTaskAllowedDuplicate := False;
              end;
              tasks.Remove(t);
            except
              on e: Exception do
              begin
                Debug(dpError, section, Format('[EXCEPTION] QueueClean: Exception Remove : %s', [e.Message]));
              end;
            end;
          finally
            ts.ReleaseSlotsAssignmentLock;
            if ts2 <> nil then
              ts2.ReleaseSlotsAssignmentLock;
          end;
          Inc(tkill_race);

          Console_QueueDel(ss);
          Continue;
        end;

        if (t.ClassType = TWaitTask) then
        begin
          // Tell the paired race task that its wait task is gone.
          if TWaitTask(t).race_task_uid <> 0 then
          begin
            fPartnerTask := GlTaskRegistry.Lookup(TWaitTask(t).race_task_uid);
            if (fPartnerTask <> nil) and (fPartnerTask.ClassType = TPazoRaceTask) then
              TPazoRaceTask(fPartnerTask).dst_uid := 0;
          end;

          with TWaitTask(t) do
          begin
            readyerror := True;
            event.SetEvent;
          end;

          try
            //t := NIL;
            ss := t.UidText;
            Debug(dpSpam, section, Format('[QUEUECLEAN] Clean wait task : %s', [t.Fullname]));
            ts.AcquireSlotsAssignmentLock('QueueClean wait');
            try
              // Remove the wait task from the pazo dependency graph so it does not
              // leak as a pending node when the wait task is killed by QueueClean.
              if (TWaitTask(t).mainpazo <> nil) and (TWaitTask(t).mainpazo.TaskGraph <> nil) and
                 TWaitTask(t).mainpazo.TaskGraph.Contains(t.uid) then
              begin
                try
                  TWaitTask(t).mainpazo.TaskGraph.MarkError(t.uid);
                except
                  on e: Exception do
                    Debug(dpError, section, Format('[EXCEPTION] QueueClean wait MarkError: %s', [e.Message]));
                end;
              end;

              Debug(dpMessage, section, Format('QueueClean: Remove : %s', [t.Fullname]));
              // Remove from the O(1) index BEFORE TObjectList.Remove frees the task.
              RemoveTaskFromIndex(t);
              if t = fExecutingTask then
              begin
                fExecutingTask := nil;
                fExecutingTaskAllowedDuplicate := False;
              end;
              tasks.Remove(t);
            finally
              ts.ReleaseSlotsAssignmentLock;
            end;
          except
            on e: Exception do
            begin
              Debug(dpError, section,
                Format('[EXCEPTION] QueueClean: Exception Remove : %s', [e.Message]));
            end;
          end;
          Inc(tkill_race);

          Console_QueueDel(ss);

          Continue;
        end;

        if (((t.ClassType = TLoginTask) or (t.ClassType = TQuitTask) or
          (t.ClassType = TIdleTask) or (t.ClassType = TPazoMkdirTask)) and
          ((t.startat = 0) or (t.startat <= queue_last_run))) then
        begin
          if (t.slot1 <> nil) then
          begin
            try
              ss := t.UidText;
              ts.AcquireSlotsAssignmentLock('QueueClean login, quit, idle, mkdir');
              try
                TSiteSlot(t.slot1).todotask := nil;
              finally
                ts.ReleaseSlotsAssignmentLock;
              end;
              TSiteSlot(t.slot1).downloadingfrom := False;
              TSiteSlot(t.slot1).uploadingto := False;
              t.slot1     := nil;
              t.slot1name := '';
            except
              on e: Exception do
              begin
                Debug(dpError, section,
                  Format('[EXCEPTION] slot1 QueueClean: Exception : %s', [e.Message]));
              end;
            end;
          end;

          try
            Debug(dpSpam, section, Format('[QUEUECLEAN] Clean other task : %s', [t.Fullname]));
            ts.AcquireSlotsAssignmentLock('QueueClean other');
            try
              Debug(dpMessage, section, Format('QueueClean: Remove : %s', [t.Fullname]));
              // Remove from the O(1) index BEFORE TObjectList.Remove frees the task.
              RemoveTaskFromIndex(t);
              if t = fExecutingTask then
              begin
                fExecutingTask := nil;
                fExecutingTaskAllowedDuplicate := False;
              end;
              tasks.Remove(t);
            finally
              ts.ReleaseSlotsAssignmentLock;
            end;
          except
            on e: Exception do
            begin
              Debug(dpError, section,
                Format('[EXCEPTION] QueueClean: Exception Remove : %s', [e.Message]));
            end;
          end;
          Inc(tkill_other);

          Console_QueueDel(ss);
          Continue;
        end;
      end;
    end;
    end;
  finally
    main_lock.Leave;
  end;


  if (tkill_unassigne <> 0) then
  begin
    irc_Addconsole(Format('QueueClean: Killed : %s unassigned tasks',
      [IntToStr(tkill_unassigne)]));
    Debug(dpMessage, section, Format('QueueClean: Killed : %s unassigned tasks',
      [IntToStr(tkill_unassigne)]));
  end;
  if (tkill_race <> 0) then
  begin
    irc_Addconsole(Format('QueueClean: Killed : %s race tasks', [IntToStr(tkill_race)]));
    irc_Adderror(Format('<c4>[CLEAN]</c> QueueClean: Killed : %s race tasks',
      [IntToStr(tkill_race)]));
    Debug(dpMessage, section, Format('[CLEAN] QueueClean: Killed : %s race tasks',
      [IntToStr(tkill_race)]));
  end;
  if (tkill_other <> 0) then
  begin
    irc_Addconsole(Format('QueueClean: Killed : %s other tasks',
      [IntToStr(tkill_other)]));
    irc_Adderror(Format('<c4>[CLEAN]</c> QueueClean: Killed : %s other tasks',
      [IntToStr(tkill_other)]));
    Debug(dpMessage, section, Format('[CLEAN] QueueClean: Killed : %s other tasks',
      [IntToStr(tkill_other)]));
  end;

  finally
    queueclean_last_run := Now;
  end;

  if (tkill_unassigne > 0) or (tkill_race > 0) or (tkill_other > 0) then
    QueueStat;

  //Debug(dpMessage, section, 'QueueClean end %d', [tasks.Count]);
end;

procedure TQueueThread.QueueStat;
  procedure CountTask(const t: TTask; var t_race, t_dir, t_auto, t_other: integer);
  begin
    if t = nil then exit;
    // Use 'is' instead of ClassType so any descendants are counted as well.
    if (t is TPazoRaceTask) or (t.ClassType = TWaitTask) then
      Inc(t_race)
    else if (t is TPazoDirlistTask) then
      Inc(t_dir)
    else if ((t is TAutoNukeTask) or (t is TAutoDirlistTask) or
      (t is TAutoIndexTask) or (t is TLoginTask) or
      (t is TRulesTask)) then
      Inc(t_auto)
    else
      Inc(t_other);
  end;

var
  t_race, t_dir, t_auto, t_other: integer;
  t_race_in_tasks, t_race_in_waiting, t_race_in_slots: integer;
  fTask: TTask;
  fListIndex: Integer;
  fList: TObjectList;
  fSlot: TSiteSlot;
begin
  if MilliSecondsBetween(queue_last_stat_update, Now) < 1000 then
    exit;

  queue_last_stat_update := Now;
  t_race  := 0;
  t_dir   := 0;
  t_auto  := 0;
  t_other := 0;
  t_race_in_tasks := 0;
  t_race_in_waiting := 0;
  t_race_in_slots := 0;

  main_lock.Enter('QueueStat');
  try
    for fListIndex := 0 to 1 do
    begin
      if fListIndex = 0 then fList := tasks else fList := waiting_tasks;
      for fTask in fList do
      begin
      try
        CountTask(fTask, t_race, t_dir, t_auto, t_other);
        if (fTask <> nil) and (fTask is TPazoRaceTask) then
        begin
          if fListIndex = 0 then
            Inc(t_race_in_tasks)
          else
            Inc(t_race_in_waiting);
        end;
      except
      on e: Exception do
        begin
          Debug(dpError, section, Format('[EXCEPTION] TQueueThread.QueueStat : %s', [e.Message]));
          Continue;
        end;
      end;
      end;
    end;

    // Also count tasks that are currently running in slots so the stats reflect
    // real activity, not just queued-but-not-yet-assigned work.
    if (fSite <> nil) then
    begin
      for fSlot in TSite(fSite).slots do
      begin
        try
          CountTask(fSlot.todotask, t_race, t_dir, t_auto, t_other);
          if (fSlot.todotask <> nil) and (fSlot.todotask is TPazoRaceTask) then
            Inc(t_race_in_slots);
        except
          on e: Exception do
          begin
            Debug(dpError, section, Format('[EXCEPTION] TQueueThread.QueueStat (slot): %s', [e.Message]));
            Continue;
          end;
        end;
      end;
    end;
  finally
    main_lock.Leave;
  end;

  if fSite <> nil then
    Debug(dpMessage, section,
      '[QUEUESTAT] %s: race=%d (tasks=%d waiting=%d slots=%d) dir=%d auto=%d other=%d | tasks=%d waiting=%d slots=%d',
      [TSite(fSite).Name, t_race, t_race_in_tasks, t_race_in_waiting, t_race_in_slots,
       t_dir, t_auto, t_other, tasks.Count, waiting_tasks.Count, TSite(fSite).slots.Count]);

  // Periodic memory diagnostic: log sizes of central data structures that are
  // suspected to leak or accumulate. Only one queue thread logs per minute to
  // avoid log spam.
  if MilliSecondsBetween(queue_last_memory_diag, Now) >= 60000 then
  begin
    queue_last_memory_diag := Now;
    if (GlTaskRegistry <> nil) and (StatsList <> nil) and (fQueueStat <> nil) then
    begin
      Debug(dpMessage, section,
        '[MEMORYDIAG] site=%s taskregistry=%d statslist=%d racehistory=%d pazos=%d graphnodes_global=%d graphstats=T%d/D%d/E%d/R%d/P%d/L%d/Ad%.1f/Ae%.1f/Pd%d/Pk%s tasktypes=%s',
        [TSite(fSite).Name, GlTaskRegistry.Count, StatsList.Count, fQueueStat.FRaceHistory.Count,
         GetGlobalPazoTaskGraphInstanceCount,
         GlPazoTaskGraphNodeCount,
         GetGlobalPazoTaskGraphStats.Total,
         GetGlobalPazoTaskGraphStats.Done,
         GetGlobalPazoTaskGraphStats.ErrorState,
         GetGlobalPazoTaskGraphStats.Running,
         GetGlobalPazoTaskGraphStats.Pending,
         GetGlobalPazoTaskGraphStats.LeafFinished,
         GetGlobalPazoTaskGraphStats.AvgDependencies,
         GetGlobalPazoTaskGraphStats.AvgDependents,
         GetGlobalPazoTaskGraphStats.PendingDependencyKeys,
         GetGlobalPazoTaskGraphStats.PendingDependencyKeyList,
         GlTaskRegistry.CountsByType]);

      // Periodic trim of index capacities to prevent slow memory growth when
      // many transient tasks passed through the queue.
      if fRaceTaskSet.Capacity > fRaceTaskSet.Count then
        fRaceTaskSet.Capacity := fRaceTaskSet.Count;
      if fDirlistTaskSet.Capacity > fDirlistTaskSet.Count then
        fDirlistTaskSet.Capacity := fDirlistTaskSet.Count;
      if fMkdirTaskSet.Capacity > fMkdirTaskSet.Count then
        fMkdirTaskSet.Capacity := fMkdirTaskSet.Count;
      if fLoginTaskSet.Capacity > fLoginTaskSet.Count then
        fLoginTaskSet.Capacity := fLoginTaskSet.Count;
    end;
  end;

  fQueueStat.FRaceTaskCount := t_race;
  fQueueStat.FDirlistTaskCount := t_dir;
  fQueueStat.FAutoTaskCount := t_auto;
  fQueueStat.FOtherTaskCount := t_other;

  // reset per-iteration counters; aggregated totals are kept in the *Total fields
  fQueueStat.FTasksAssignedThisRun := 0;
  fQueueStat.FTasksSkippedThisRun := 0;
end;

procedure TQueueThread.LogDiagSummary;
var
  fSiteName: string;
  fMsg: string;
  fNow: TDateTime;
begin
  fNow := Now;
  if MilliSecondsBetween(fNow, fDiagLastLogTime) < 10000 then
    exit;

  if (fDiagIterCount = 0) and (fDiagFbrCalls = 0) then
  begin
    fDiagLastLogTime := fNow;
    exit;
  end;

  if fSite <> nil then
    fSiteName := TSite(fSite).Name
  else
    fSiteName := fSiteName;

  fMsg := Format('%s DIAG iter=%d assigned=%d skipped=%d fbr=%d fbr_assigned=%d fbr_nocand=%d tried_fail=%d',
    [fSiteName, fDiagIterCount, fDiagIterAssigned, fDiagIterSkipped,
     fDiagFbrCalls, fDiagFbrAssigned, fDiagFbrNoCand, fDiagFbrTriedFailed]);

  if fDiagFbrNoCand > 0 then
    fMsg := fMsg + Format(' | no_cand: freeslots=%d maxup=%d maxdn=%d noslotdst=%d busydict=%d actxfer=%d notready=%d other=%d',
      [fDiagFbrNoCandFreeslots, fDiagFbrNoCandMaxUp, fDiagFbrNoCandMaxDn,
       fDiagFbrNoCandNoSlotDst, fDiagFbrNoCandBusyDict, fDiagFbrNoCandActiveTransfer,
       fDiagFbrNoCandNotReady, fDiagFbrNoCandOther]);

  // Phase 2 diagnostics: method timing + task creation/dup counts.
  fMsg := fMsg + Format(' | times: fbr=%dms/%d qstat=%dms/%d',
    [fQueueStat.FFindBestRaceMsTotal, fQueueStat.FFindBestRaceCount,
     fQueueStat.FQueueStatMsTotal, fQueueStat.FQueueStatCount]);

  fMsg := fMsg + Format(' | created: race=%d dir=%d mkdir=%d login=%d other=%d',
    [fQueueStat.FTasksCreatedRace, fQueueStat.FTasksCreatedDirlist,
     fQueueStat.FTasksCreatedMkdir, fQueueStat.FTasksCreatedLogin,
     fQueueStat.FTasksCreatedOther]);

  fMsg := fMsg + Format(' | dup: race=%d dir=%d mkdir=%d login=%d',
    [fQueueStat.FTasksDupRace, fQueueStat.FTasksDupDirlist,
     fQueueStat.FTasksDupMkdir, fQueueStat.FTasksDupLogin]);

  fMsg := fMsg + Format(' | qfire: slot=%d graphwake=%d main=%d addtask=%d pazo=%d irc=%d console=%d other=%d',
    [GlQueueFireCounts[qfsSlot], GlQueueFireCounts[qfsGraphWake],
     GlQueueFireCounts[qfsMainThread], GlQueueFireCounts[qfsAddTask],
     GlQueueFireCounts[qfsPazo], GlQueueFireCounts[qfsIrc],
     GlQueueFireCounts[qfsConsole], GlQueueFireCounts[qfsOther]]);

  Debug(dpError, section, '[QUEUEDIAG] ' + fMsg);

  fDiagIterCount := 0;
  fDiagIterAssigned := 0;
  fDiagIterSkipped := 0;
  fDiagFbrCalls := 0;
  fDiagFbrAssigned := 0;
  fDiagFbrNoCand := 0;
  fDiagFbrNoCandFreeslots := 0;
  fDiagFbrNoCandMaxUp := 0;
  fDiagFbrNoCandMaxDn := 0;
  fDiagFbrNoCandNoSlotDst := 0;
  fDiagFbrNoCandBusyDict := 0;
  fDiagFbrNoCandActiveTransfer := 0;
  fDiagFbrNoCandNotReady := 0;
  fDiagFbrNoCandOther := 0;
  fDiagFbrTriedFailed := 0;

  // Phase 2 diagnostics: reset per-window counters.
  fQueueStat.FFindBestRaceMsTotal := 0;
  fQueueStat.FFindBestRaceCount := 0;
  fQueueStat.FQueueStatMsTotal := 0;
  fQueueStat.FQueueStatCount := 0;
  fQueueStat.FTasksCreatedRace := 0;
  fQueueStat.FTasksCreatedDirlist := 0;
  fQueueStat.FTasksCreatedMkdir := 0;
  fQueueStat.FTasksCreatedLogin := 0;
  fQueueStat.FTasksCreatedOther := 0;
  fQueueStat.FTasksDupRace := 0;
  fQueueStat.FTasksDupDirlist := 0;
  fQueueStat.FTasksDupMkdir := 0;
  fQueueStat.FTasksDupLogin := 0;

  FillChar(GlQueueFireCounts, SizeOf(GlQueueFireCounts), 0);

  fDiagLastLogTime := fNow;
end;

function FormatQueueStat(const aQueueStat: TQueueStat): String;
begin
  Result := Format('Race:%d(+%d/60s) Dir:%d Auto:%d Other:%d | waited=%d avg=%dms | iter=%d avg=%dms | sort=%d avg=%dms | idle=%d graph_wake=%d busy_dest=%d',
    [aQueueStat.FRaceTaskCount, aQueueStat.RecentRaceCount, aQueueStat.FDirlistTaskCount, aQueueStat.FAutoTaskCount, aQueueStat.FOtherTaskCount,
     aQueueStat.FTaskWaitCount, aQueueStat.FTaskWaitMsTotal div Max(aQueueStat.FTaskWaitCount, 1),
     aQueueStat.FQueueIterateCount, aQueueStat.FQueueIterateMsTotal div Max(aQueueStat.FQueueIterateCount, 1),
     aQueueStat.FQueueSortCount, aQueueStat.FQueueSortMsTotal div Max(aQueueStat.FQueueSortCount, 1),
     aQueueStat.FSlotIdleCount, aQueueStat.FGraphWakeCount, aQueueStat.FBusyDestinationsHitCount]);
end;

function QueueStatForSiteAsString(const aSiteName: String): String;
var
  fSite: TSite;
begin
  Result := '';
  if aSiteName = '' then
    exit;

  fSite := FindSiteByName('', aSiteName);
  if fSite = nil then
  begin
    Result := Format('Site <b>%s</b> not found.', [aSiteName]);
    exit;
  end;

  if fSite.Queue = nil then
  begin
    Result := Format('Site <b>%s</b> has no queue.', [aSiteName]);
    exit;
  end;

  Result := Format('Queue stats for <b>%s</b>: %s', [aSiteName, FormatQueueStat(fSite.Queue.fQueueStat)]);
end;

function QueueStatAllAsString: String;
var
queueStat: TQueueStat;
t_race, t_dir, t_auto, t_other: integer;
t_recent_race: integer;
t_wait_ms, t_wait_count, t_iter_ms, t_iter_count, t_sort_ms, t_sort_count,
t_idle_ms, t_idle_count, t_graph_wake, t_busy_dest: Int64;
t_bd_nosrc, t_bd_nodst, t_bd_maxsimup, t_bd_maxsimdn, t_bd_busydict,
t_bd_actxfer_dst, t_bd_maxup, t_bd_actxfer_src, t_bd_maxdn,
t_bd_noslotsrc, t_bd_noslotdst, t_bd_maxupperrip: Int64;
begin
  t_race  := 0;
  t_dir   := 0;
  t_auto  := 0;
  t_other := 0;
  t_wait_ms := 0;
  t_wait_count := 0;
  t_iter_ms := 0;
  t_iter_count := 0;
  t_sort_ms := 0;
  t_sort_count := 0;
  t_idle_ms := 0;
  t_idle_count := 0;
  t_graph_wake := 0;
  t_busy_dest := 0;

  t_recent_race := 0;

  t_bd_nosrc := 0;
  t_bd_nodst := 0;
  t_bd_maxsimup := 0;
  t_bd_maxsimdn := 0;
  t_bd_busydict := 0;
  t_bd_actxfer_dst := 0;
  t_bd_maxup := 0;
  t_bd_actxfer_src := 0;
  t_bd_maxdn := 0;
  t_bd_noslotsrc := 0;
  t_bd_noslotdst := 0;
  t_bd_maxupperrip := 0;

  for queueStat in StatsList do
  begin
    t_race := t_race + queueStat.FRaceTaskCount;
    t_dir := t_dir + queueStat.FDirlistTaskCount;
    t_auto := t_auto + queueStat.FAutoTaskCount;
    t_other := t_other + queueStat.FOtherTaskCount;

    t_wait_ms := t_wait_ms + queueStat.FTaskWaitMsTotal;
    t_wait_count := t_wait_count + queueStat.FTaskWaitCount;
    t_iter_ms := t_iter_ms + queueStat.FQueueIterateMsTotal;
    t_iter_count := t_iter_count + queueStat.FQueueIterateCount;
    t_sort_ms := t_sort_ms + queueStat.FQueueSortMsTotal;
    t_sort_count := t_sort_count + queueStat.FQueueSortCount;
    t_idle_ms := t_idle_ms + queueStat.FSlotIdleMsTotal;
    t_idle_count := t_idle_count + queueStat.FSlotIdleCount;
    t_graph_wake := t_graph_wake + queueStat.FGraphWakeCount;
    t_busy_dest := t_busy_dest + queueStat.FBusyDestinationsHitCount;
    t_recent_race := t_recent_race + queueStat.RecentRaceCount;

    t_bd_nosrc := t_bd_nosrc + queueStat.FBusyDestNoFreeSlotsSource;
    t_bd_nodst := t_bd_nodst + queueStat.FBusyDestNoFreeSlotsDest;
    t_bd_maxsimup := t_bd_maxsimup + queueStat.FBusyDestMaxSimUpCooldown;
    t_bd_maxsimdn := t_bd_maxsimdn + queueStat.FBusyDestMaxSimDownCooldown;
    t_bd_busydict := t_bd_busydict + queueStat.FBusyDestBusyDict;
    t_bd_actxfer_dst := t_bd_actxfer_dst + queueStat.FBusyDestActiveTransferDst;
    t_bd_maxup := t_bd_maxup + queueStat.FBusyDestMaxUp;
    t_bd_actxfer_src := t_bd_actxfer_src + queueStat.FBusyDestActiveTransferSrc;
    t_bd_maxdn := t_bd_maxdn + queueStat.FBusyDestMaxDn;
    t_bd_noslotsrc := t_bd_noslotsrc + queueStat.FBusyDestNoSlotSource;
    t_bd_noslotdst := t_bd_noslotdst + queueStat.FBusyDestNoSlotDest;
    t_bd_maxupperrip := t_bd_maxupperrip + queueStat.FBusyDestMaxUpPerRip;
  end;

  QueueStatUpdateDateTime := Now;
  Console_QueueStat(t_race + t_dir + t_auto + t_other, t_race, t_dir, t_auto, t_other);

  Result := Format('Queue: %d total (Race:%d(+%d/60s) Dir:%d Auto:%d Other:%d) | waited=%d avg=%dms | iter=%d avg=%dms | sort=%d avg=%dms | idle=%d graph_wake=%d busy_dest=%d | fires: slot=%d graph=%d main=%d add=%d pazo=%d irc=%d console=%d other=%d',
    [t_race + t_dir + t_auto + t_other, t_race, t_recent_race, t_dir, t_auto, t_other,
     t_wait_count, t_wait_ms div Max(t_wait_count, 1),
     t_iter_count, t_iter_ms div Max(t_iter_count, 1),
     t_sort_count, t_sort_ms div Max(t_sort_count, 1),
     t_idle_count, t_graph_wake, t_busy_dest,
     GlQueueFireCounts[qfsSlot], GlQueueFireCounts[qfsGraphWake],
     GlQueueFireCounts[qfsMainThread], GlQueueFireCounts[qfsAddTask],
     GlQueueFireCounts[qfsPazo], GlQueueFireCounts[qfsIrc],
     GlQueueFireCounts[qfsConsole], GlQueueFireCounts[qfsOther]]);

  Result := Result + #13#10 +
    Format('busy_dest breakdown: nosrc=%d nodst=%d maxsimup=%d maxsimdn=%d busydict=%d actxfer_dst=%d maxup=%d actxfer_src=%d maxdn=%d noslotsrc=%d noslotdst=%d maxupperrip=%d',
      [t_bd_nosrc, t_bd_nodst, t_bd_maxsimup, t_bd_maxsimdn, t_bd_busydict,
       t_bd_actxfer_dst, t_bd_maxup, t_bd_actxfer_src, t_bd_maxdn,
       t_bd_noslotsrc, t_bd_noslotdst, t_bd_maxupperrip]);
end;

procedure QueueStatAll;
begin
  // keep the existing console update behaviour; the returned string is intentionally ignored here
  QueueStatAllAsString;
end;

procedure TQueueThread.QueueSendCurrentTasksToConsole;
var
  fTask: TTask;
  fListIndex: Integer;
  fList: TObjectList;
begin
  main_lock.Enter('QueueSendCurrentTasksToConsole');
  try
    for fListIndex := 0 to 1 do
    begin
      if fListIndex = 0 then fList := tasks else fList := waiting_tasks;
      for fTask in fList do
        AddTaskToConsole(fTask);
    end;
  finally
    main_lock.Leave;
  end;
end;

function TQueueThread.FetchAutoIndex: TAutoIndexTask;
var
  fTask: TTask;
  fListIndex: Integer;
  fList: TObjectList;
begin
  Result := nil;
  main_lock.Enter('FetchAutoIndex');
  try
    for fListIndex := 0 to 1 do
    begin
      if fListIndex = 0 then fList := tasks else fList := waiting_tasks;
      for fTask in fList do
      begin
      try
        if (fTask is TAutoIndexTask) then
        begin
          Result := TAutoIndexTask(fTask);
          exit;
        end;
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[EXCEPTION] TSite.FetchAutoIndex: %s', [e.Message]));
        end;
      end;
      end;
    end;
  finally
    main_lock.Leave;
  end;
end;

function TQueueThread.FetchAutoDirlist: TAutoDirlistTask;
var
  fTask: TTask;
  fListIndex: Integer;
  fList: TObjectList;
begin
  Result := nil;
  main_lock.Enter('FetchAutoDirlist');
  try
    for fListIndex := 0 to 1 do
    begin
      if fListIndex = 0 then fList := tasks else fList := waiting_tasks;
      for fTask in fList do
      begin
      try
        if (fTask is TAutoDirlistTask) then
        begin
          Result := TAutoDirlistTask(fTask);
          exit;
        end;
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[EXCEPTION] TSite.FetchAutoDirlist: %s', [e.Message]));
        end;
      end;
      end;
    end;
  finally
    main_lock.Leave;
  end;
end;

function TQueueThread.FetchAutoNuke: TAutoNukeTask;
var
  fTask: TTask;
  fListIndex: Integer;
  fList: TObjectList;
begin
  Result := nil;
  main_lock.Enter('FetchAutoNuke');
  try
    for fListIndex := 0 to 1 do
    begin
      if fListIndex = 0 then fList := tasks else fList := waiting_tasks;
      for fTask in fList do
      begin
      try
        if (fTask is TAutoNukeTask) then
        begin
          Result := TAutoNukeTask(fTask);
          exit;
        end;
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[EXCEPTION] TSite.FetchAutoNuke: %s', [e.Message]));
        end;
      end;
      end;
    end;
  finally
    main_lock.Leave;
  end;
end;

function TQueueThread.FetchAutoBnctest: TLoginTask;
var
  fTask: TTask;
  t: TLoginTask;
  fListIndex: Integer;
  fList: TObjectList;
begin
  Result := nil;
  main_lock.Enter('FetchAutoBnctest');
  try
    for fListIndex := 0 to 1 do
    begin
      if fListIndex = 0 then fList := tasks else fList := waiting_tasks;
      for fTask in fList do
      begin
      try
        if (fTask is TLoginTask) then
        begin
          t := TLoginTask(fTask);
          if t.readd then
          begin
            Result := TLoginTask(fTask);
            exit;
          end;
        end;
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[EXCEPTION] TSite.FetchAutoBnctest: %s', [e.Message]));
        end;
      end;
      end;
    end;
  finally
    main_lock.Leave;
  end;
end;

function TQueueThread.FetchAutoRules: TRulesTask;
var
  fTask: TTask;
  fListIndex: Integer;
  fList: TObjectList;
begin
  Result := nil;
  main_lock.Enter('FetchAutoRules');
  try
    for fListIndex := 0 to 1 do
    begin
      if fListIndex = 0 then fList := tasks else fList := waiting_tasks;
      for fTask in fList do
      begin
      try
        if (fTask is TRulesTask) then
        begin
          Result := TRulesTask(fTask);
          exit;
        end;
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[EXCEPTION] TSite.FetchAutoRules: %s', [e.Message]));
        end;
      end;
      end;
    end;
  finally
    main_lock.Leave;
  end;
end;

function TQueueThread.IrcKillAll(const netname, channel, params: String): boolean;
var
  fTask: TTask;
  rx: TRegExpr;
  i: Int32;
  ts: TSite;
  fListIndex: Integer;
  fList: TObjectList;
begin
  Result := False;

  if fSite = nil then
    exit;

  ts := TSite(fSite);

  rx := TRegExpr.Create;
  try
    rx.ModifierI := False;
    rx.Expression := 'AUTOLOGIN';
    main_lock.Enter('IrcKillAll');

    for fListIndex := 0 to 1 do
    begin
      if fListIndex = 0 then fList := tasks else fList := waiting_tasks;
      for i := fList.Count - 1 downto 0 do
      begin
        try
          if i < 0 then
            Break;
        except
          Break;
        end;

        fTask := TTask(fList.items[i]);
      if not rx.Exec(TPazoTask(fTask).FullName) then
      begin
        irc_Addtext(netname, channel, 'Removing Task -> %s', [TPazoTask(fTask).FullName]);
        try
          // If we are killing a race task, wake its paired wait task first.
          if fTask is TPazoRaceTask then
            SignalPairedWaitTask(TPazoRaceTask(fTask).uid);

          // Remove from the O(1) index BEFORE TObjectList.Remove frees the task.
          RemoveTaskFromIndex(fTask);
          if fTask = fExecutingTask then
          begin
            fExecutingTask := nil;
            fExecutingTaskAllowedDuplicate := False;
          end;
          ts.AcquireSlotsAssignmentLock('killall');
            try
              fList.Remove(TPazoTask(fTask));
            finally
              ts.ReleaseSlotsAssignmentLock;
            end;
        except
          on e: Exception do
            irc_Addtext(netname, channel, '<c4><b>ERROR</c></b>: IrcKillAll.tasks.Remove: %s', [e.Message]);
        end;
      end
      end;
    end;
  finally
    main_lock.Leave;
    rx.Free;
  end;

  Result := True;
end;


  procedure TQueueThread.GetCurrentTasks(const taskLst: Contnrs.TObjectList);
  var
  fTask: TTask;
  fQueueTask: TQueueTask;
  fListIndex: Integer;
  fList: TObjectList;
  begin
    main_lock.Enter('GetCurrentTasks');
    try
      for fListIndex := 0 to 1 do
      begin
        if fListIndex = 0 then fList := tasks else fList := waiting_tasks;
        for fTask in fList do
        begin
          fQueueTask := TQueueTask.Create;
          fQueueTask.FFullname := fTask.Fullname;
          fQueueTask.FType := fTask.ClassType;
          taskLst.Add(fQueueTask);
        end;
      end;
    finally
      main_lock.Leave;
    end;
  end;

end.
