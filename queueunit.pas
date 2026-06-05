unit queueunit;

interface

uses
  Classes, Contnrs, tasksunit, taskrace, SyncObjs, slcriticalsection2, pazo, taskidle, taskquit, tasklogin, RegExpr, taskautoindex, taskrules, taskautodirlist, taskautonuke, Generics.Collections, IdThreadSafe, mormot.core.os;


type TQueueStat = class
    FRaceTaskCount: integer;
    FDirlistTaskCount: integer;
    FAutoTaskCount: integer;
    FOtherTaskCount: integer;
    FActiveTaskCount: integer; //< tasks currently running (status=Running)
end;

type TQueueTask = class
  FFullname: string;
  FType: TClass;
  FTryToAssign: integer;
  FRunnable: boolean;
  FDestinationSite: string;
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
  queueevent: TSynEvent;
  fSiteName: String;
  fSite: TObject;
  fBusyDestinations: TDictionary<TObject, integer>;

  queue_last_run: TDateTime;
  queueclean_last_run: TDateTime;
  queue_last_stat_update: TDateTime;
  fLastDirlistCheckTime: TDateTime;
  fLastLowPriorityLogTime: TDateTime;
  fLastDirlistCount: Integer;

    procedure TryToAssignLoginSlot(t: TLoginTask);
    procedure TryToAssignRaceSlots(t: TPazoRaceTask);
    function TaskAlreadyInQueue(t: TTask): boolean;
    procedure QueueStat;

public
  { Phase 5b: lazy-rebuilt map of destination-site -> pending race task count.
    Rebuilt at the start of every queue iteration under main_lock.
    Read by TSiteSlot.Execute under main_lock to do targeted wakeups. }
  fPendingRaceDestinations: TDictionary<String, Integer>;

  function FindBestTask(aNow: TDateTime; aSkippedTasks: TList<TTask> = nil): TTask;
  procedure QueueFire;
procedure QueueStart;
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
{ @abstract(Returns count of pending race tasks targeting the given destination site) }
function GetPendingRaceTasksToDestination(const aDestinationSiteName: String): integer;

{ Send the current tasks to the queue console window. }
procedure QueueSendCurrentTasksToConsole;

property QueueLastRun: TDateTime read queue_last_run;
property QueueCleanLastRun: TDateTime read queueclean_last_run;

  end;
procedure QueueInit;
procedure QueueUninit;
procedure QueueStatAll;
{ @abstract(Returns total task counts broken down by type across all queue threads) }
procedure GetQueueTotals(out total, race, dirlist, autotasks, other: integer);
{ @abstract(Returns count of pending race tasks targeting the given destination site, across all queues) }
function GetPendingRaceTasksToDestination(const aDestinationSiteName: String): integer;

{ Calculate max allowed dirlist slots for a site based on glMaxDirlistSlots config
  @param(aSlotCount total slot count of the site)
  @returns(max allowed concurrent dirlist tasks, minimum 1)
  Supports absolute values ('1', '3') and percentages ('50%', '25%').
  Empty config falls back to legacy behavior (aSlotCount div 2). }
function _CalcMaxDirlistSlots(const aSlotCount: integer): integer;

{ @abstract(Checks if a race task is low-priority based on dir type and ini settings) }
function _IsLowPriorityRaceTask(const aTask: TPazoRaceTask): Boolean;

{ @abstract(Checks if there are any important non-low-priority tasks waiting in the queue) }
function _HasWaitingNonLowPriorityTasks(const aTasks: Contnrs.TObjectList; const aQueueLastRun: TDateTime; aSkippedTasks: TList<TTask> = nil): Boolean;

{ @abstract(Calculates priority score for a task. Higher score = higher priority.) }
function _ScoreTask(const aTask: TTask): Int64;

var
  QueueStatUpdateDateTime: TDateTime;
  GlDirlistCompletedCounter: TIdThreadSafeInt32;
  GlDirlistRate: Double;
  GlDirlistRateMax: Double;
  { Global list of all queue threads. Used by Phase 5b for targeted wakeups. }
  Queues: TObjectList<TQueueThread>;
  glQueuesLock: TCriticalSection;
  { max dirlist slots config value, e.g. '1', '50%' }
  glMaxDirlistSlots: string;
  { dir type priority values from slftp.ini, used by _ScoreTask and _IsLowPriorityRaceTask }
  sample_dirs_priority: Integer;
  proof_dirs_priority: Integer;
  subs_dirs_priority: Integer;
  cover_dirs_priority: Integer;

implementation

uses
  SysUtils, Types, irc, DateUtils, debugunit, notify, console, kb, mainthread, Math, configunit, mrdohutils,
  tasktvinfolookup, taskhttpnfo, tasksitenfo, tasksitesfv, sitesunit, dirlist;

const
  section = 'queue';

var
  // config
  maxassign: integer;
  maxassign_delay: integer;
  glLastDirlistCheckTime: TDateTime;
  glLastDirlistCount: Integer;
  queueclean_unassigned: Integer;
  queueclean_maxrunning: Integer;
  enable_queueclean: boolean;
  queue_recycle_post_to_irc: boolean;
  StatsList: TObjectList<TQueueStat>;

{ Calculate max allowed dirlist slots for a site based on glMaxDirlistSlots config
  @param(aSlotCount total slot count of the site)
  @returns(max allowed concurrent dirlist tasks, minimum 1)
  Supports absolute values ('1', '3') and percentages ('50%', '25%').
  Empty config falls back to legacy behavior (aSlotCount div 2). }
function _CalcMaxDirlistSlots(const aSlotCount: integer): integer;
var
  fPercentValue: integer;
begin
  if glMaxDirlistSlots = '' then
  begin
    Result := aSlotCount div 2;
    Exit;
  end;

  if glMaxDirlistSlots[Length(glMaxDirlistSlots)] = '%' then
  begin
    fPercentValue := StrToIntDef(Copy(glMaxDirlistSlots, 1, Length(glMaxDirlistSlots) - 1), 50);
    if fPercentValue <= 0 then
      fPercentValue := 1;
    if fPercentValue > 100 then
      fPercentValue := 100;
    Result := Max(Round(aSlotCount * fPercentValue / 100.0), 1);
  end
  else
  begin
    Result := StrToIntDef(glMaxDirlistSlots, aSlotCount div 2);
    if Result < 0 then
      Result := 0;
  end;
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

function _IsLowPriorityRaceTask(const aTask: TPazoRaceTask): Boolean;
begin
  Result := False;

  if aTask = nil then
  begin
    Debug(dpError, section, '_IsLowPriorityRaceTask called with nil task');
    exit;
  end;

  if (aTask.IsSample) and (sample_dirs_priority = 3) then
  begin
    Result := True;
    exit;
  end;

  if (aTask.IsProof) and (proof_dirs_priority = 3) then
  begin
    Result := True;
    exit;
  end;

  if (aTask.IsSubs) and (subs_dirs_priority = 3) then
  begin
    Result := True;
    exit;
  end;

  if (aTask.IsCovers) and (cover_dirs_priority = 3) then
  begin
    Result := True;
    exit;
  end;
end;

function _HasWaitingNonLowPriorityTasks(const aTasks: Contnrs.TObjectList; const aQueueLastRun: TDateTime; aSkippedTasks: TList<TTask> = nil): Boolean;
var
  i: Integer;
  fTask: TTask;
  fRaceTask: TPazoRaceTask;
begin
  Result := False;

  if aTasks = nil then
  begin
    Debug(dpError, section, '_HasWaitingNonLowPriorityTasks called with nil list');
    exit;
  end;

  for i := 0 to aTasks.Count - 1 do
  begin
    fTask := TTask(aTasks.Items[i]);
    if fTask = nil then
    begin
      Debug(dpError, section, '_HasWaitingNonLowPriorityTasks: task at index %d is nil', [i]);
      Continue;
    end;

    // Skip tasks in our skipped/tried list
    if (aSkippedTasks <> nil) and (aSkippedTasks.IndexOf(fTask) >= 0) then
      Continue;

    // Skip tasks that are already running or done
    if ((fTask.slot1 <> nil) or (fTask.slot2 <> nil) or fTask.ready or fTask.readyerror) then
      Continue;

    // Skip tasks that are not yet ready to start
    if ((fTask is TPazoTask) and (TPazoTask(fTask).startat > 0) and
        (TPazoTask(fTask).startat > aQueueLastRun)) then
      Continue;

    // If it's not a race task, it's important (mkdir, dirlist, login, etc.)
    if not (fTask is TPazoRaceTask) then
    begin
      Result := True;
      exit;
    end;

    // If it's a race task but not marked as low priority, it's important
    fRaceTask := TPazoRaceTask(fTask);
    if not _IsLowPriorityRaceTask(fRaceTask) then
    begin
      Result := True;
      exit;
    end;
  end;
end;

function _ScoreTask(const aTask: TTask): Int64;
var
  tpm: TPazoMkdirTask;
  tpr: TPazoRaceTask;
begin
  Result := 0;

  if aTask = nil then
    exit;

  // Wait tasks get absolute top priority
  if (aTask.ClassType = TWaitTask) then
  begin
    Result := 100000000;
  end
  else if (aTask is TPazoMkdirTask) then
  begin
    tpm := TPazoMkdirTask(aTask);
    // Maindir mkdir (dir = '') gets higher priority than subdir mkdir
    if (tpm.dir = '') then
      Result := 90000000
    else
      Result := 70000000;
  end
  else if (aTask is TPazoRaceTask) then
  begin
    tpr := TPazoRaceTask(aTask);
    Result := 80000000;

    // SFV files get highest sub-priority
    if tpr.IsSfv then
      Result := Result + 5000000
    // NFO files next
    else if tpr.IsNfo then
      Result := Result + 4000000
    else
    begin
      // Sample dir priority
      if tpr.IsSample then
      begin
        case sample_dirs_priority of
          0: Result := Result + 0;
          1: Result := Result + 3000000;
          2, 3: Result := Result - 3000000;
        end;
      end;

      // Proof priority
      if tpr.IsProof then
      begin
        case proof_dirs_priority of
          0: Result := Result + 0;
          1: Result := Result + 2500000;
          2, 3: Result := Result - 2500000;
        end;
      end;

      // Subs priority
      if tpr.IsSubs then
      begin
        case subs_dirs_priority of
          0: Result := Result + 0;
          1: Result := Result + 2000000;
          2, 3: Result := Result - 2000000;
        end;
      end;

      // Covers priority
      if tpr.IsCovers then
      begin
        case cover_dirs_priority of
          0: Result := Result + 0;
          1: Result := Result + 1500000;
          2, 3: Result := Result - 1500000;
        end;
      end;
    end;

    // Rank bonus (higher rank = more important)
    Result := Result + tpr.rank * 1000;

    // Filesize bonus (larger files = more important)
    Result := Result + (tpr.filesize div 1000000);
  end
  else if (aTask is TPazoTask) then
  begin
    // Other pazo tasks (dirlist, etc.)
    Result := 60000000;
  end
  else
  begin
    // Non-pazo tasks (login, quit, idle, etc.)
    Result := 50000000;
  end;
end;

function TQueueThread.FindBestTask(aNow: TDateTime; aSkippedTasks: TList<TTask> = nil): TTask;
var
  bestTask: TTask;
  bestScore: Int64;
  t: TTask;
  score: Int64;
  tpr: TPazoRaceTask;
  hasImportantWaiting: Boolean;
begin
  Result := nil;
  bestTask := nil;
  bestScore := Low(Int64);

  // Pre-check: are there any important (non-low-priority) tasks waiting?
  hasImportantWaiting := _HasWaitingNonLowPriorityTasks(tasks, aNow, aSkippedTasks);

  for t in tasks do
  begin
    // Skip tasks in our skipped/tried list
    if (aSkippedTasks <> nil) and (aSkippedTasks.IndexOf(t) >= 0) then
      Continue;

    // Skip tasks that are already assigned, ready, or have errors
    if ((t.slot1 <> nil) or (t.slot2 <> nil) or t.ready or t.readyerror) then
      Continue;

    // Skip delayed tasks
    if (t.startat > 0) and (t.startat > aNow) then
      Continue;

    // Skip tasks not ready to execute
    if not t.IsReadyToBeExecuted then
      Continue;

    // Low-priority race tasks are skipped if important tasks are waiting
    if (t is TPazoRaceTask) then
    begin
      tpr := TPazoRaceTask(t);
      if _IsLowPriorityRaceTask(tpr) and hasImportantWaiting then
        Continue;
    end;

    score := _ScoreTask(t);

    if score > bestScore then
    begin
      bestScore := score;
      bestTask := t;
    end;
  end;

  Result := bestTask;
end;

procedure TQueueThread.QueueSort;
begin
  // QueueSorter removed - tasks are no longer sorted. FindBestTask handles priority.
end;

procedure TQueueThread.QueueStart;
begin
  QueueStatAll;
end;

constructor TQueueThread.Create(const aSiteName: String);
begin
  main_lock := nil;
  tasks := nil;
  queueevent := nil;
  fQueueStat := nil;
  fBusyDestinations := nil;
  fPendingRaceDestinations := nil;

  inherited Create(False);
  {$IFDEF DEBUG}
    NameThreadForDebugging('Queue/' + aSiteName, self.ThreadID);
  {$ENDIF}

  try
    main_lock := TSLCriticalSection2.Create('Queue_' + aSiteName);
    tasks := TObjectList.Create(True);
    queueevent := TSynEvent.Create;
    queue_last_run := Now;
    queueclean_last_run := Now;
    queue_last_stat_update := Now;
    FreeOnTerminate := True;
    fQueueStat := TQueueStat.Create();
    StatsList.Add(fQueueStat);
    if glQueuesLock <> nil then
    begin
      glQueuesLock.Enter;
      try
        Queues.Add(self);
      finally
        glQueuesLock.Leave;
      end;
    end;
    fSiteName := aSiteName;
    fBusyDestinations := TDictionary<TObject, integer>.Create;
    fPendingRaceDestinations := TDictionary<String, Integer>.Create;
  except
    FreeAndNil(fPendingRaceDestinations);
    FreeAndNil(fBusyDestinations);
    if (Queues <> nil) and (glQueuesLock <> nil) then
    begin
      glQueuesLock.Enter;
      try
        Queues.Extract(self);
      finally
        glQueuesLock.Leave;
      end;
    end;
    if fQueueStat <> nil then
    begin
      StatsList.Remove(fQueueStat);
      FreeAndNil(fQueueStat);
    end;
    FreeAndNil(queueevent);
    FreeAndNil(tasks);
    FreeAndNil(main_lock);
    raise;
  end;
end;

destructor TQueueThread.Destroy;
begin
  if (Queues <> nil) and (glQueuesLock <> nil) then
  begin
    glQueuesLock.Enter;
    try
      Queues.Extract(self);
    finally
      glQueuesLock.Leave;
    end;
  end;
  if (fQueueStat <> nil) and (StatsList <> nil) then
  begin
    StatsList.Remove(fQueueStat);
    FreeAndNil(fQueueStat);
  end;
  main_lock.Free;
  tasks.Free;
  queueevent.Free;
  fBusyDestinations.Free;
  fPendingRaceDestinations.Free;
  inherited;
end;

procedure TQueueThread.TryToAssignRaceSlots(t: TPazoRaceTask);
var
  s1, s2: TSite;
  i: integer;
  ss1, ss2, fSiteSlotLoop: TSiteSlot;
begin
  try
    s1 := TSite(t.ssite1);
    s2 := TSite(t.ssite2);
    if s1.freeslots = 0 then
      exit;
    if s2.freeslots = 0 then
      exit;

    if s2.MaxSimUpCooldownActive then
    begin
      if not fBusyDestinations.ContainsKey(s2) then
        fBusyDestinations.Add(s2, 0);
      Debug(dpSpam, section, '[MAXSIM COOLDOWN] Destination site %s is on MaxSim UP cooldown (%ds remaining), skipping %s',
        [s2.Name, s2.MaxSimUpCooldownRemainingSeconds, t.FullName]);
      exit;
    end;

    if s1.MaxSimDownCooldownActive then
    begin
      Debug(dpSpam, section, '[MAXSIM COOLDOWN] Source site %s is on MaxSim DOWN cooldown (%ds remaining), skipping %s',
        [s1.Name, s1.MaxSimDownCooldownRemainingSeconds, t.FullName]);
      exit;
    end;

    if fBusyDestinations.ContainsKey(s2) then
    begin
      Debug(dpSpam, section, 'Destination site %s is busy, skip race task assign from %s', [s2.Name, s1.Name]);
      exit;
    end;

    // Check if the race has already failed on the destination site or dirlist
    if t.ps2.error or
      ((t.dir <> '') and (t.ps2.dirlist <> nil) and (t.ps2.dirlist.FindDirList(t.dir) <> nil) and t.ps2.dirlist.FindDirList(t.dir).error) then
    begin
      t.readyerror := True;
      Debug(dpSpam, section, Format('TryToAssignRaceSlots: race failed on destination site or dirlist: %s', [t.FullName]));
      exit;
    end;

    // first watch if it is not already in process to upload the same file to the same place
    if t.ps2.HasActiveTransfer(t.dir + t.filename) then
      exit; // we are already sending this file to the same destination site

    if s2.num_up >= s2.max_up then
      exit;

    if t.ps1.HasActiveTransfer(t.dir + t.filename, s2.Name) then
      exit; // we are already sending this file the opposite route

    // or use 'if t.ps1.StatusRealPreOrShouldPre then' from pazo.pas but will also pre true when status = rssShouldPre
    //if t.ps1.status = rssRealPre then
    if t.ps1.StatusRealPreOrShouldPre then
    begin
      if s1.num_dn >= s1.max_pre_dn then
        exit;
    end
    else
    begin
      if s1.num_dn >= s1.max_dn then
        exit;
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
      exit;


    if not s2.AcquireSlotsAssignmentLock(1, 'TryToAssignRaceSlots') then
    begin
      fBusyDestinations.Add(s2, 0);
      exit;
    end;

    try
      // check again now that we have the lock at the destination
      if s2.num_up >= s2.max_up then
        exit;

      // again check if this file is already being sent to the destination now that we have the slot assignment lock
      if t.ps2.HasActiveTransfer(t.dir + t.filename) then
        exit; // we are already sending this file to the same destination site

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
        exit;

      // now you can relax, just check if you don't abuse your max simultaneous uploads for a rip
      i := ss2.site.MaxUpPerRip;
      if ((i > 0) and (t.ps2.ActiveTransferCount >= i)) then
      begin
        Debug(dpSpam, section, 'We shouldnt upload more than maxupperrip value [' + IntToStr(i) + '] for' + ss2.Name);
        exit;
      end;

      Debug(dpSpam, section, 'FOUND SLOTS FOR ' + t.FullName + ': ' + ss1.Name + ' ' + ss2.Name);
      t.dst      := TWaitTask.Create(t.netname, t.channel, t.site2);
      t.assigned := Now;
      t.dst.assigned := Now;
      t.dst.wait_for := t.Name;
      t.dst.slot1 := ss2;
      AddTask(t.dst);
      t.ps2.AddActiveTransfer(t.dir + t.filename, s1.Name);
      t.slot1      := ss1;
      t.slot1name  := ss1.Name;
      t.slot2      := ss2;
      t.slot2name  := ss2.Name;
      ss1.downloadingfrom := True;
      ss2.uploadingto := True;
      ss1.todotask := t;
      ss2.todotask := t.dst;
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
        exit;
      if (ss.todotask <> nil) then
        exit;
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
          // Only use slot 0 for ghost kill to avoid disrupting active transfers
          if i > 0 then
            ss := nil;
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
        if (s.fActiveDirlistCount >= _CalcMaxDirlistSlots(s.slots.Count)) then
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
      t.assigned  := Now;
      ss.todotask := t;
      ss.Fire;
    finally
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
begin
  fSetDownPazo := TList<TPazo>.Create;
  try
    main_lock.Enter('QueueEmpty');
    try
        for t in tasks do
        begin
        if ((not t.ready) and (t.slot1 = nil) and (not t.dontremove) and ((t.site1 = sitename) or (t.site2 = sitename))) then
          t.readyerror := True;

        if (t is TPazoTask) and not fSetDownPazo.Contains(TPazoTask(t).mainpazo) then
          fSetDownPazo.Add(TPazoTask(t).mainpazo);
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

function TQueueThread.TaskAlreadyInQueue(t: TTask): boolean;
var
  fTask:    TTask;
  tpr, i_tpr: TPazoRaceTask;
  tpd, i_tpd: TPazoDirlistTask;
  tpm, i_tpm: TPazoMkdirTask;
  tpl, i_tpl: TLoginTask;
begin
  Result := False;

  if (t is TPazoRaceTask) then
  begin
    try
      tpr := TPazoRaceTask(t);
      main_lock.Enter('TaskAlreadyInQueue1');
      try
          for fTask in tasks do
          begin
            try
              if (fTask is TPazoRaceTask) then
              begin
                i_tpr := TPazoRaceTask(fTask);
                if ((i_tpr.ready = False) and (i_tpr.readyerror = False) and
                  (i_tpr.slot1 = nil) and (i_tpr.pazo_id = tpr.pazo_id) and
                  (i_tpr.site1 = tpr.site1) and (i_tpr.site2 = tpr.site2) and
                  (i_tpr.dir = tpr.dir) and (i_tpr.filename = tpr.filename)) then
                begin
                  Result := True;
                  exit;
                end;
              end;
            except
              on E: Exception do
              begin
                Debug(dpError, section, Format('[EXCEPTION] TaskAlreadyInQueue TPazoRaceTask (loop) : %s', [e.Message]));
                continue;
              end;
            end;
        end;
      finally
        main_lock.Leave;
      end;
    except
      on E: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] TaskAlreadyInQueue TPazoRaceTask : %s', [e.Message]));
        Result := False;
        exit;
      end;
    end;
    exit;
  end;

  if (t is TPazoDirlistTask) then
  begin
    try
      tpd := TPazoDirlistTask(t);
      main_lock.Enter('TaskAlreadyInQueue2');
      try
          for fTask in tasks do
          begin
            try
              if (fTask is TPazoDirlistTask) then
              begin
                i_tpd := TPazoDirlistTask(fTask);
                if ((i_tpd.ready = False) and (i_tpd.readyerror = False) and
                  (i_tpd.slot1 = nil) and (i_tpd.pazo_id = tpd.pazo_id) and
                  (i_tpd.site1 = tpd.site1) and (i_tpd.dir = tpd.dir)) then
                begin
                  Result := True;
                  exit;
                end;
              end;
            except
              on E: Exception do
              begin
                Debug(dpError, section, Format('[EXCEPTION] TaskAlreadyInQueue TPazoDirlistTask (loop) : %s', [e.Message]));
                continue;
              end;
            end;
        end;
      finally
        main_lock.Leave;
      end;
    except
      on E: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] TaskAlreadyInQueue TPazoDirlistTask : %s', [e.Message]));
        Result := False;
        exit;
      end;
    end;
    exit;
  end;

  if (t is TPazoMkdirTask) then
  begin
    try
      tpm := TPazoMkdirTask(t);
      main_lock.Enter('TaskAlreadyInQueue3');
      try
          for fTask in tasks do
          begin
            try
              if (fTask is TPazoMkdirTask) then
              begin
                i_tpm := TPazoMkdirTask(fTask);
                if ((i_tpm.ready = False) and (i_tpm.readyerror = False) and
                  (i_tpm.slot1 = nil) and (i_tpm.pazo_id = tpm.pazo_id) and
                  (i_tpm.site1 = tpm.site1) and (i_tpm.dir = tpm.dir)) then
                begin
                  Result := True;
                  exit;
                end;
              end;
            except
              on E: Exception do
              begin
                Debug(dpError, section, Format('[EXCEPTION] TaskAlreadyInQueue TPazoMkdirTask (loop) : %s', [e.Message]));
                continue;
              end;
            end;
        end;
      finally
        main_lock.Leave;
      end;
    except
      on E: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] TaskAlreadyInQueue TPazoMkdirTask : %s', [e.Message]));
        Result := False;
        exit;
      end;
    end;
    exit;
  end;

  if (t is TLoginTask) then
  begin
    try
      tpl := TLoginTask(t);
      main_lock.enter('TaskAlreadyInQueue4');
      try
          for fTask in tasks do
          begin
            if (fTask is TLoginTask) then
            begin
              i_tpl := TLoginTask(fTask);
              if ((i_tpl.ready = False) and (i_tpl.readyerror = False) and
                (i_tpl.slot1 = nil) and (i_tpl.site1 = tpl.site1) and
                (i_tpl.wantedslot = tpl.wantedslot) and (i_tpl.readd = tpl.readd) and (i_tpl.kill = tpl.kill)) then
              begin
                Result := True;
                exit;
              end;
            end;
          end;
      finally
        main_lock.Leave;
      end;
    except
      on E: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] TaskAlreadyInQueue TLoginTask : %s', [e.Message]));
        Result := False;
        exit;
      end;
    end;
    exit;
  end;
end;


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
  step: String;
  fTaskAdded, fTaskAssigned: Boolean;
begin
  fTaskAdded := False;
  fTaskAssigned := False;
  step := 'init';
  try
    fCheckSiteSlotsSite := nil;
    step := 'reading t.Name';
    tname := t.Name;

    step := 'checking ssite1 conditions';
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

    Debug(dpSpam, section, Format('[iNFO] adding : %s', [tname]));

    step := 'entering main_lock';
    main_lock.Enter('AddTask');
    try
      step := 'TaskAlreadyInQueue check';
      if TaskAlreadyInQueue(t) then
      begin
        // don't add the task to the queue, just notify and free right away if it's a duplicate
        if t.IsNotifyTask then
          TaskReady(t);
          
        t.Free;
        exit;
      end;

      step := 'Adding to list';
      // All tasks go to the unified tasks list; delayed tasks are filtered at assignment time
      tasks.Add(t);
      fTaskAdded := True;

      step := 'Race slot checks';
      try
        if ((t is TPazoRaceTask) and (not t.ready) and t.IsReadyToBeExecuted and (TSite(fSite).freeslots > 0)) then
        begin
          TSite(fSite).AcquireSlotsAssignmentLock('AddTask-Slot');
          try
            if ((not t.ready) and t.IsReadyToBeExecuted) then
            begin
              self.TryToAssignSlots(t);
              fTaskAssigned := (t.assigned <> 0);
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
      Debug(dpError, section, Format('[EXCEPTION] AddTask tasks.Add (Step: %s): %s', [step, e.Message]));
      raise; // re-raise
    end;
  end;

  step := 'Checking failed race conditions';
  // check if the race has failed on either source or destination site (in case of race tasks). This can happen when a dirlist task is running and
  // adding new race tasks while the mkdir task on the destination fails at the same time and sets the site failed. This would lead to the
  // dependencies of the race task never be resolved and it would remain and pollute the queue.
  if t is TPazoRaceTask then
  begin
    try
      step := 'Checking ps2 / dirlist';
      if TPazoRaceTask(t).ps2 = nil then
      begin
        // just a safe-check, do nothing
      end
      else if TPazoRaceTask(t).ps2.error or
        ((TPazoRaceTask(t).dir <> '') and (TPazoRaceTask(t).ps2.dirlist <> nil) and (TPazoRaceTask(t).ps2.dirlist.FindDirList(TPazoRaceTask(t).dir) <> nil) and TPazoRaceTask(t).ps2.dirlist.FindDirList(TPazoRaceTask(t).dir).error) then
      begin
        t.readyerror := true;
        Debug(dpSpam, section, Format('AddTask: race failed on source or destination site: %s', [tname]));
        exit;
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] AddTask check for failed pazo (Step: %s): %s', [step, e.Message]));
        exit;
      end;
    end;
  end;

  if fCheckSiteSlotsSite <> nil then
  begin
    step := 'CheckSiteSlots';
    try
      CheckSiteSlots(fCheckSiteSlotsSite);
    except
    end;
  end;
  
  step := 'AddTaskToConsole';
  try
    AddTaskToConsole(t);
  except
    on e: Exception do
      Debug(dpError, section, Format('[EXCEPTION] AddTaskToConsole (Step: %s): %s', [step, e.Message]));
  end;

  // Event-based queue: wake up the queue thread so it can process the new task.
  // Without this, the thread might sleep indefinitely and never assign the task.
  if fTaskAdded and not fTaskAssigned then
    self.QueueFire;
end;

procedure TQueueThread.RemoveRaceTasks(const pazo_id: integer; const sitename: String);
var
  ttp: TPazoRaceTask;
  fTask: TTask;
begin
  try
    main_lock.Enter('RemoveRaceTasks');
    try
        for fTask in tasks do
        begin
        try
          if (fTask is TPazoRaceTask) then
          begin
            ttp := TPazoRaceTask(fTask);
            if ((ttp.ready = False) and (ttp.readyerror = False) and (ttp.slot1 = nil) and (ttp.pazo_id = pazo_id) and (ttp.site2 = sitename)) then
              ttp.ready := True;
          end;
        except
          on E: Exception do
          begin
            Debug(dpError, section, Format('[EXCEPTION] RemoveRaceTasks (loop) : %s', [e.Message]));
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
begin
  try
    main_lock.Enter('RemovePazoDirTasks');
    try
        for fTask in tasks do
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
  fSlotsRebuilt: Boolean;
begin
  Result := False;
  fSlotsRebuilt := False;
  fSlotsToRebuild := TList<TSiteSlot>.Create;
  try
    main_lock.Enter('RemovePazo');
    try
        for fTask in tasks do
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
              end
              else if aForce then
              begin
                Debug(dpMessage, section, Format('RemovePazo: Force removal of assigned task: %s', [t.Name]));
                t.readyerror := True;

                // if the site slot actually has this task assigned, we need to rebuild it
                if TSiteSlot(t.slot1).todotask = t then
                begin
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
        fSlotsRebuilt := True;
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

  // Wake up the queue thread so it can reuse any slots that were rebuilt
  if fSlotsRebuilt then
    self.QueueFire;
end;


procedure TQueueThread.RemovePazoMKDIR(const pazo_id: integer; const dir: String);
var
  ttp: TPazoMkdirTask;
  fTask: TTask;
begin
  try
    main_lock.Enter('RemovePazoMKDIR');
    try
        for fTask in tasks do
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
begin
  try
    main_lock.Enter('RemovePazoSfv');
    try
        for fAbstractTask in tasks do
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
begin
  try
    main_lock.Enter('RemovePazoRace');
    try
        for fTask in tasks do
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
  s:    TSiteSlot;
  ss:   String;
  ts:   TSite;
  fBusyDestinationsTmp: TDictionary<TObject, integer>;
  fNextTaskStartAt: TDateTime;
  fWaitTimerTimeout: Cardinal;
  fCooldownTimeout: Cardinal;
  fPendingCount: Integer;
  fSkippedTasks: TList<TTask>;
begin
  while ((not slshutdown) and (not Terminated)) do
  begin
    queue_last_run := Now();

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
    //Debug(dpSpam, section, 'Queue Iteration begin (%s) [%d tasks]', [ts.Name, tasks.Count]);
    try
      main_lock.Enter('Execute');
      try
        // Phase 5b: Rebuild pending-race-destinations map for targeted wakeups
        fPendingRaceDestinations.Clear;
          for fTask in tasks do
          begin
            if (fTask is TPazoRaceTask) and (fTask.slot1 = nil) then
            begin
              if fPendingRaceDestinations.TryGetValue(TPazoRaceTask(fTask).site2, fPendingCount) then
              begin
                fPendingRaceDestinations.Remove(TPazoRaceTask(fTask).site2);
                fPendingRaceDestinations.Add(TPazoRaceTask(fTask).site2, fPendingCount + 1);
              end
              else
                fPendingRaceDestinations.Add(TPazoRaceTask(fTask).site2, 1);
            end;
          end;

        // Calculate next delayed task wakeup time from tasks with future startat
        for fTask in tasks do
        begin
          if (fTask.startat > 0) and (fTask.startat > queue_last_run) and (fTask.startat < fNextTaskStartAt) then
            fNextTaskStartAt := fTask.startat;
        end;


          for i := tasks.Count - 1 downto 0 do
          begin
            if i < 0 then
              Break;

            fTask := TTask(tasks.items[i]);

          if fTask = nil then
            Continue;

          try
            try
              if fTask.ready then;
            except
              on E: Exception do
              begin
                Debug(dpError, section, Format('[AV-DEBUG] RemoveReady fTask.ready access failed for %s: %s', [fTask.Name, E.Message]));
                Continue;
              end;
            end;

            try
              if fTask.readyerror then;
            except
              on E: Exception do
              begin
                Debug(dpError, section, Format('[AV-DEBUG] RemoveReady fTask.readyerror access failed for %s: %s', [fTask.Name, E.Message]));
                Continue;
              end;
              end;

            if ((fTask.ready) or (fTask.readyerror)) then
            begin
              // Stuck wait tasks: they set ready=True but never freed their slot.
              // Free the slot now so the task can be removed and the slot reused.
              if (fTask.slot1 <> nil) then
              begin
                try
                  if fTask.ClassType = TWaitTask then;
                except
                  on E: Exception do
                  begin
                    Debug(dpError, section, Format('[AV-DEBUG] RemoveReady fTask.ClassType access failed for %s: %s', [fTask.Name, E.Message]));
                    Continue;
                  end;
                end;

                if (fTask.ClassType = TWaitTask) then
                begin
                  try
                    ts.AcquireSlotsAssignmentLock('Queue free stuck wait task');
                    try
                      try
                        if TSiteSlot(fTask.slot1) <> nil then;
                      except
                        on E: Exception do
                        begin
                          Debug(dpError, section, Format('[AV-DEBUG] RemoveReady TSiteSlot(fTask.slot1) cast failed for %s: %s', [fTask.Name, E.Message]));
                          fTask.slot1 := nil;
                        end;
                      end;

                      try
                        if TSiteSlot(fTask.slot1).todotask = fTask then
                          TSiteSlot(fTask.slot1).todotask := nil;
                      except
                        on E: Exception do
                        begin
                          Debug(dpError, section, Format('[AV-DEBUG] RemoveReady todotask access failed for %s: %s', [fTask.Name, E.Message]));
                        end;
                      end;

                      try
                        fTask.slot1 := nil;
                      except
                        on E: Exception do
                        begin
                          Debug(dpError, section, Format('[AV-DEBUG] RemoveReady fTask.slot1 := nil failed for %s: %s', [fTask.Name, E.Message]));
                        end;
                      end;
                    finally
                      ts.ReleaseSlotsAssignmentLock;
                    end;
                  except
                    on E: Exception do
                    begin
                      Debug(dpError, section, Format('[AV-DEBUG] RemoveReady stuck-wait cleanup outer failed for %s: %s — forcing slot1=nil', [fTask.Name, E.Message]));
                      fTask.slot1 := nil;
                    end;
                  end;
                end;
              end;

              if (fTask.slot1 = nil) then
              begin
                try
                  ss := fTask.uidtext;
                except
                  on E: Exception do
                  begin
                    Debug(dpError, section, Format('[AV-DEBUG] RemoveReady fTask.uidtext access failed for %s: %s', [fTask.Name, E.Message]));
                    ss := '';
                  end;
                end;

                try
                  if fTask.IsNotifyTask then
                    TaskReady(fTask);
                except
                  on E: Exception do
                  begin
                    Debug(dpError, section, Format('[AV-DEBUG] RemoveReady TaskReady failed for %s: %s', [fTask.Name, E.Message]));
                  end;
                end;

                if (fTask.ClassType = TPazoRaceTask) then
                begin
                  try
                    with TPazoRaceTask(fTask) do
                      if (dst <> nil) then
                        try
                          dst.event.SetEvent;
                        except
                          on E: Exception do
                          begin
                            Debug(dpError, section, Format('[AV-DEBUG] RemoveReady dst.event.SetEvent failed for %s: %s', [fTask.Name, E.Message]));
                          end;
                        end;
                  except
                    on E: Exception do
                    begin
                      Debug(dpError, section, Format('[AV-DEBUG] RemoveReady dst access failed for %s: %s', [fTask.Name, E.Message]));
                    end;
                  end;
                end;

                try
                  ts.AcquireSlotsAssignmentLock('Queue remove ready tasks');
                  try
                    tasks.Remove(fTask);
                  finally
                    ts.ReleaseSlotsAssignmentLock;
                  end;
                except
                  on E: Exception do
                  begin
                    Debug(dpError, section, Format('[AV-DEBUG] RemoveReady tasks.Remove failed for %s: %s', [fTask.Name, E.Message]));
                  end;
                end;

                try
                  Console_QueueDel(ss);
                except
                  on E: Exception do
                  begin
                    Debug(dpError, section, Format('[AV-DEBUG] RemoveReady Console_QueueDel failed for %s: %s', [fTask.Name, E.Message]));
                  end;
                end;
              end;
            end;
          except
            on e: Exception do
            begin
              Debug(dpError, section, Format('[EXCEPTION] TQueueThread.Execute (RemoveReady fallback): %s', [e.Message]));
              Continue;
            end;
          end;
        end;

        fSkippedTasks := TList<TTask>.Create;
        try
          ts.AcquireSlotsAssignmentLock('Queue iterate');
          try
            // Only scan for best task when slots are actually free
            while ts.freeslots > 0 do
            begin
              fTask := FindBestTask(queue_last_run, fSkippedTasks);
              if fTask = nil then
              begin
                fNextTaskStartAt := MaxDateTime;
                break;
              end;

              try
                TryToAssignSlots(fTask);

                // If assignment failed and task was not delayed, record it in the skipped list
                // to avoid spinning on the same unassignable task, then continue scanning
                if (fTask.slot1 = nil) and (fTask.startat <= queue_last_run) then
                begin
                  fSkippedTasks.Add(fTask);
                end;
              except
                on e: Exception do
                begin
                  Debug(dpError, section, Format('[EXCEPTION] TQueueThread.Execute (TryToAssignSlots) : %s', [e.Message]));
                  fSkippedTasks.Add(fTask);
                end;
              end;
            end;
          finally
            ts.ReleaseSlotsAssignmentLock;
          end;
        finally
          fSkippedTasks.Free;
        end;
      finally
        main_lock.Leave;
        fBusyDestinationsTmp.Free;
      end;

      QueueStat;

      // We are looking for idle
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

    // Event-based waiting: only sleep if we have a known wakeup reason.
    // Reasons to wake up:
    //   1. A delayed task (startat) becomes due  -> sleep until then
    //   2. A site cooldown expires               -> sleep until then
    //   3. QueueFire was called (task added / slot freed)
    //
    // If none apply, wait indefinitely.
    fWaitTimerTimeout := $FFFFFFFF; // INFINITE

    // Reason 1: Delayed tasks
    if fNextTaskStartAt <> MaxDateTime then
    begin
      if fNextTaskStartAt <= Now then
      begin
        if ts.freeslots > 0 then
        begin
          // Task should be assigned immediately; don't sleep at all
          Debug(dpSpam, section, Format('TQueueThread.Execute: skip sleep %s', [ts.Name]));
          continue;
        end;
        // Task is due but no free slots. Wait for other wakeup reasons.
      end
      else
      begin
        fWaitTimerTimeout := MilliSecondsBetween(Now, fNextTaskStartAt);
        // Cap at 60s to avoid extremely long sleeps if clocks drift
        if fWaitTimerTimeout > 60000 then
          fWaitTimerTimeout := 60000;
      end;
    end;

    // Reason 2: Cooldown expiry
    // If the site has active cooldowns, wake up when the earliest one expires
    // so we can retry assigning tasks that were skipped due to the cooldown.
    if ts.MaxSimUpCooldownActive then
    begin
      fCooldownTimeout := ts.MaxSimUpCooldownRemainingSeconds * 1000;
      if fCooldownTimeout < fWaitTimerTimeout then
        fWaitTimerTimeout := fCooldownTimeout;
    end;
    if ts.MaxSimDownCooldownActive then
    begin
      fCooldownTimeout := ts.MaxSimDownCooldownRemainingSeconds * 1000;
      if fCooldownTimeout < fWaitTimerTimeout then
        fWaitTimerTimeout := fCooldownTimeout;
    end;
    if ts.LoginCooldownActive then
    begin
      fCooldownTimeout := ts.LoginCooldownRemainingSeconds * 1000;
      if fCooldownTimeout < fWaitTimerTimeout then
        fWaitTimerTimeout := fCooldownTimeout;
    end;

    if fWaitTimerTimeout = 0 then
    begin
      // Something is immediately ready; don't sleep at all
      continue;
    end;

    if queueevent.WaitFor(fWaitTimerTimeout) then
    begin
      { Event fired. Normal exit. }
      //Debug(dpSpam, section, Format('[QUEUEFIRE received : %s', [ts.Name]));
      queueevent.ResetEvent;
    end
    else { Timeout reached — either startat task or 60s safety cap }
    begin
      // Nothing to log here; timeout is expected for delayed tasks
    end;
  end;
end;

procedure QueueInit;
begin

  // config
  maxassign := config.ReadInteger(section, 'maxassign', 200);
  maxassign_delay := config.ReadInteger(section, 'maxassign_delay', 15);
  sample_dirs_priority := config.ReadInteger(section, 'sample_dirs_priority', 1);
  if not (sample_dirs_priority in [0..3]) then
    sample_dirs_priority := 1;

  proof_dirs_priority := config.ReadInteger(section, 'proof_dirs_priority', 2);
  if not (proof_dirs_priority in [0..3]) then
    proof_dirs_priority := 2;

  subs_dirs_priority := config.ReadInteger(section, 'subs_dirs_priority', 2);
  if not (subs_dirs_priority in [0..3]) then
    subs_dirs_priority := 2;

  cover_dirs_priority := config.ReadInteger(section, 'cover_dirs_priority', 2);
  if not (cover_dirs_priority in [0..3]) then
    cover_dirs_priority := 2;

  queueclean_maxrunning := config.ReadInteger('queue', 'queueclean_maxrunning', 900);
  queueclean_unassigned := config.ReadInteger('queue', 'queueclean_unassigned', 600);
  enable_queueclean := config.ReadBool(section, 'enable_queueclean', False);
  queue_recycle_post_to_irc := spamcfg.readbool(section, 'queue_recycle', True);
  glMaxDirlistSlots := config.ReadString(section, 'max_dirlist_slots', '');

  StatsList := TObjectList<TQueueStat>.Create(True);
  Queues := TObjectList<TQueueThread>.Create(False);
  glQueuesLock := TCriticalSection.Create;
  GlDirlistCompletedCounter := TIdThreadSafeInt32.Create;
  GlDirlistRate := 0;
  GlDirlistRateMax := 0;
  glLastDirlistCheckTime := 0;
  glLastDirlistCount := 0;
end;

procedure QueueUninit;
begin
  FreeAndNil(GlDirlistCompletedCounter);
  FreeAndNil(StatsList);
  FreeAndNil(Queues);
  FreeAndNil(glQueuesLock);
end;

procedure TQueueThread.QueueClean(run_now: boolean = False);
var
  i, tkill_unassigne, tkill_race, tkill_other: integer;
  ss: String;
  t:  TTask;
  ts, ts2: TSite;
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

  // Check old unassigne task
  main_lock.Enter('QueueClean1');
  try
      for i := tasks.Count - 1 downto 0 do
      begin
        try
          if i < 0 then
            Break;
        except
          Break;
        end;
        t := TTask(tasks[i]);
        try
          ss := t.UidText;
          if ((t.assigned = 0) and not t.dontremove and ((t.startat = 0) or (t.startat <= queue_last_run)) and
            (SecondsBetween(t.created, Now()) >= queueclean_unassigned)) then
          begin
            try
              t.ready := True;
              Debug(dpError, section, Format('QueueClean: Remove Unassigned : %s', [t.Fullname]));
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

            // Remove the task from the list so it doesn't linger as a ghost entry
            tasks.Remove(t);
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
  finally
    main_lock.Leave;
  end;

  // Check old tasks, assigned bu long time wait
  main_lock.Enter('QueueClean2');
  try
      for i := tasks.Count - 1 downto 0 do
      begin
        try
        if i < 0 then
          Break;
        except
          Break;
        end;
        t := TTask(tasks[i]);
      if ((t.assigned <> 0) and ((t.startat = 0) or (t.startat <= queue_last_run)) and
        (SecondsBetween(t.assigned, Now()) >= queueclean_maxrunning)) then
      begin
        if (t.ClassType = TPazoRaceTask) then
        begin
          ss := t.UidText;
          ts2 := nil;
          ts.AcquireSlotsAssignmentLock('QueueClean race');
          try
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
              Debug(dpError, section, Format('QueueClean: Remove : %s', [t.Fullname]));
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
          with TWaitTask(t) do
            event.SetEvent;

          try
            //t := NIL;
            ss := t.UidText;
            Debug(dpSpam, section, Format('[QUEUECLEAN] Clean wait task : %s', [t.Fullname]));
            ts.AcquireSlotsAssignmentLock('QueueClean wait');
            try
              Debug(dpError, section, Format('QueueClean: Remove : %s', [t.Fullname]));
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
              Debug(dpError, section, Format('QueueClean: Remove : %s', [t.Fullname]));
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
  finally
    main_lock.Leave;
  end;


  if (tkill_unassigne <> 0) then
  begin
    irc_Addconsole(Format('QueueClean: Killed : %s unassigned tasks',
      [IntToStr(tkill_unassigne)]));
    Debug(dpError, section, Format('QueueClean: Killed : %s unassigned tasks',
      [IntToStr(tkill_unassigne)]));
  end;
  if (tkill_race <> 0) then
  begin
    irc_Addconsole(Format('QueueClean: Killed : %s race tasks', [IntToStr(tkill_race)]));
    irc_Adderror(Format('<c4>[CLEAN]</c> QueueClean: Killed : %s race tasks',
      [IntToStr(tkill_race)]));
    Debug(dpError, section, Format('[CLEAN] QueueClean: Killed : %s race tasks',
      [IntToStr(tkill_race)]));
  end;
  if (tkill_other <> 0) then
  begin
    irc_Addconsole(Format('QueueClean: Killed : %s other tasks',
      [IntToStr(tkill_other)]));
    irc_Adderror(Format('<c4>[CLEAN]</c> QueueClean: Killed : %s other tasks',
      [IntToStr(tkill_other)]));
    Debug(dpError, section, Format('[CLEAN] QueueClean: Killed : %s other tasks',
      [IntToStr(tkill_other)]));
  end;

  finally
    queueclean_last_run := Now;
  end;

  if (tkill_unassigne > 0) or (tkill_race > 0) or (tkill_other > 0) then
  begin
    QueueStat;
    // Wake up the queue thread so it can reuse any slots that were freed
    // by removing long-running assigned tasks.
    self.QueueFire;
  end;

  //Debug(dpMessage, section, 'QueueClean end %d', [tasks.Count]);
end;

procedure TQueueThread.QueueStat;
var
  t_race, t_dir, t_auto, t_other: integer;
  fTask: TTask;
begin
  if MilliSecondsBetween(queue_last_stat_update, Now) < 1000 then
    exit;

  queue_last_stat_update := Now;
  t_race  := 0;
  t_dir   := 0;
  t_auto  := 0;
  t_other := 0;

  main_lock.Enter('QueueStat');
  try
      for fTask in tasks do
      begin
      try
        if ((fTask.ClassType = TPazoRaceTask) or (fTask.ClassType = TWaitTask)) then
          Inc(t_race)
        else if ((fTask.ClassType = TPazoDirlistTask)) then
          Inc(t_dir)
        else if ((fTask.ClassType = TAutoNukeTask) or (fTask.ClassType = TAutoDirlistTask) or
          (fTask.ClassType = TAutoIndexTask) or (fTask.ClassType = TLoginTask) or
          (fTask.ClassType = TRulesTask)) then
          Inc(t_auto)
        else
          Inc(t_other);
      except
      on e: Exception do
        begin
          Debug(dpError, section, Format('[EXCEPTION] TQueueThread.QueueStat : %s', [e.Message]));
          Continue;
        end;
      end;
    end;
  finally
    main_lock.Leave;
  end;

  fQueueStat.FRaceTaskCount := t_race;
  fQueueStat.FDirlistTaskCount := t_dir;
  fQueueStat.FAutoTaskCount := t_auto;
  fQueueStat.FOtherTaskCount := t_other;
end;

procedure QueueStatAll;
var
queueStat: TQueueStat;
t_race, t_dir, t_auto, t_other: integer;
begin
  t_race  := 0;
  t_dir   := 0;
  t_auto  := 0;
  t_other := 0;

  for queueStat in StatsList do
  begin
    t_race := t_race + queueStat.FRaceTaskCount;
    t_dir := t_dir + queueStat.FDirlistTaskCount;
    t_auto := t_auto + queueStat.FAutoTaskCount;
    t_other := t_other + queueStat.FOtherTaskCount;
  end;

  if glLastDirlistCheckTime = 0 then
  begin
    glLastDirlistCheckTime := Now;
    glLastDirlistCount := GlDirlistCompletedCounter.Value;
  end
  else if MilliSecondsBetween(Now, glLastDirlistCheckTime) >= 1000 then
  begin
    GlDirlistRate := (GlDirlistCompletedCounter.Value - glLastDirlistCount) / (MilliSecondsBetween(Now, glLastDirlistCheckTime) / 1000);
    if GlDirlistRate > GlDirlistRateMax then
      GlDirlistRateMax := GlDirlistRate;
    glLastDirlistCount := GlDirlistCompletedCounter.Value;
    glLastDirlistCheckTime := Now;
  end;

  QueueStatUpdateDateTime := Now;
  Console_QueueStat(t_race + t_dir + t_auto + t_other, t_race, t_dir, t_auto, t_other);
end;

procedure GetQueueTotals(out total, race, dirlist, autotasks, other: integer);
var
  fQueueStat: TQueueStat;
begin
  race := 0;
  dirlist := 0;
  autotasks := 0;
  other := 0;

  for fQueueStat in StatsList do
  begin
    race := race + fQueueStat.FRaceTaskCount;
    dirlist := dirlist + fQueueStat.FDirlistTaskCount;
    autotasks := autotasks + fQueueStat.FAutoTaskCount;
    other := other + fQueueStat.FOtherTaskCount;
  end;

  total := race + dirlist + autotasks + other;
end;

function GetPendingRaceTasksToDestination(const aDestinationSiteName: String): integer;
var
  fQueueThread: TQueueThread;
begin
  Result := 0;
  if aDestinationSiteName = '' then
    Exit;
  for fQueueThread in Queues do
    Result := Result + fQueueThread.GetPendingRaceTasksToDestination(aDestinationSiteName);
end;

procedure TQueueThread.QueueSendCurrentTasksToConsole;
var
  fTask: TTask;
begin
  main_lock.Enter('QueueSendCurrentTasksToConsole');
  try
      for fTask in tasks do
        AddTaskToConsole(fTask);
  finally
    main_lock.Leave;
  end;
end;

function TQueueThread.FetchAutoIndex: TAutoIndexTask;
var
  fTask: TTask;
begin
  Result := nil;
  main_lock.Enter('FetchAutoIndex');
  try
      for fTask in tasks do
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
  finally
    main_lock.Leave;
  end;
end;

function TQueueThread.FetchAutoDirlist: TAutoDirlistTask;
var
  fTask: TTask;
begin
  Result := nil;
  main_lock.Enter('FetchAutoDirlist');
  try
      for fTask in tasks do
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
  finally
    main_lock.Leave;
  end;
end;

function TQueueThread.FetchAutoNuke: TAutoNukeTask;
var
  fTask: TTask;
begin
  Result := nil;
  main_lock.Enter('FetchAutoNuke');
  try
      for fTask in tasks do
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
  finally
    main_lock.Leave;
  end;
end;

function TQueueThread.FetchAutoBnctest: TLoginTask;
var
  fTask: TTask;
  t: TLoginTask;
begin
  Result := nil;
  main_lock.Enter('FetchAutoBnctest');
  try
      for fTask in tasks do
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
  finally
    main_lock.Leave;
  end;
end;

function TQueueThread.FetchAutoRules: TRulesTask;
var
  fTask: TTask;
begin
  Result := nil;
  main_lock.Enter('FetchAutoRules');
  try
      for fTask in tasks do
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

      for i := tasks.Count - 1 downto 0 do
      begin
        try
          if i < 0 then
            Break;
        except
          Break;
        end;

        fTask := TTask(tasks.items[i]);
        if not rx.Exec(TPazoTask(fTask).FullName) then
        begin
          irc_Addtext(netname, channel, 'Removing Task -> %s', [TPazoTask(fTask).FullName]);
          try
            ts.AcquireSlotsAssignmentLock('killall');
            try
              tasks.Remove(TPazoTask(fTask));
            finally
              ts.ReleaseSlotsAssignmentLock;
            end;
          except
            on e: Exception do
              irc_Addtext(netname, channel, '<c4><b>ERROR</c></b>: IrcKillAll.tasks.Remove: %s', [e.Message]);
          end;
        end;
      end;
    finally
      main_lock.Leave;
      rx.Free;
    end;

  Result := True;
end;


function TQueueThread.GetPendingRaceTasksToDestination(const aDestinationSiteName: String): integer;
var
  fTask: TTask;
  fRaceTask: TPazoRaceTask;
begin
  Result := 0;
  if aDestinationSiteName = '' then
    Exit;

  main_lock.Enter('GetPendingRaceTasksToDestination');
  try
      for fTask in tasks do
      begin
        if not (fTask is TPazoRaceTask) then
          Continue;
        fRaceTask := TPazoRaceTask(fTask);
        if (not fRaceTask.ready) and (not fRaceTask.readyerror) and (fRaceTask.slot1 = nil) and
          SameText(fRaceTask.site2, aDestinationSiteName) then
        begin
          Inc(Result);
        end;
      end;
  finally
    main_lock.Leave;
  end;
end;

  procedure TQueueThread.GetCurrentTasks(const taskLst: Contnrs.TObjectList);
  var
  fTask: TTask;
  fQueueTask: TQueueTask;
  begin
    main_lock.Enter('GetCurrentTasks');
    try
        for fTask in tasks do
        begin
          fQueueTask := TQueueTask.Create;
          fQueueTask.FFullname := fTask.Fullname;
          fQueueTask.FType := fTask.ClassType;
          taskLst.Add(fQueueTask);
        end;
    finally
      main_lock.Leave;
    end;
  end;

end.
