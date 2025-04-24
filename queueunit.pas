unit queueunit;

interface

uses
  Classes, Contnrs, tasksunit, taskrace, SyncObjs, slcriticalsection2, pazo, taskidle, taskquit, tasklogin, RegExpr, taskautoindex, taskrules, taskautodirlist, taskautonuke, Generics.Collections;


type TQueueStat = class
    FRaceTaskCount: integer;
    FDirlistTaskCount: integer;
    FAutoTaskCount: integer;
    FOtherTaskCount: integer;
end;

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
  queueevent: TEvent;
  fSiteName: String;
  fSite: TObject;
  fBusyDestinations: TDictionary<TObject, integer>;

  queue_last_run: TDateTime;
  queueclean_last_run: TDateTime;

    procedure TryToAssignLoginSlot(t: TLoginTask);
    procedure TryToAssignRaceSlots(t: TPazoRaceTask);
    function TaskAlreadyInQueue(t: TTask): boolean;
    procedure QueueStat;

public


procedure QueueFire;
procedure QueueStart;
procedure AddTask(t: TTask);
procedure QueueEmpty(const sitename: String);
procedure RemovePazoMKDIR(const pazo_id: integer; const dir: String);
procedure RemovePazoSfv(const aPazoID: integer; const aDir: String);
procedure RemovePazoRace(const pazo_id: integer; const dstsite, dir, filename: String);
function IrcKillAll(const netname, channel, params: String): boolean;
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
procedure QueueInit;
procedure QueueUninit;
procedure QueueStatAll;

implementation

uses
  SysUtils, Types, irc, DateUtils, debugunit, notify, console, kb, mainthread, Math, configunit, mrdohutils,
  tasktvinfolookup, taskhttpnfo, tasksitenfo, tasksitesfv, sitesunit;

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

  StatsList: TObjectList<TQueueStat>;
  QueueStatUpdateDateTime: TDateTime;

procedure TQueueThread.QueueFire;
begin
  try
    queueevent.SetEvent;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] QueueFire: %s', [e.Message]));
      exit;
    end;
  end;
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

constructor TQueueThread.Create(const aSiteName: String);
begin
  inherited Create(False);
  {$IFDEF DEBUG}
    NameThreadForDebugging('Queue/' + aSiteName, self.ThreadID);
  {$ENDIF}

  main_lock := TSLCriticalSection2.Create('Queue_' + aSiteName);
  tasks      := TObjectList.Create(True);
  queueevent := TEvent.Create(nil, False, False, 'queue');
  queue_last_run := Now;
  queueclean_last_run := Now;
  FreeOnTerminate := True;
  fQueueStat := TQueueStat.Create();
  StatsList.Add(fQueueStat);
  fSiteName := aSiteName;
  fBusyDestinations := TDictionary<TObject, integer>.Create;
end;

destructor TQueueThread.Destroy;
begin
  main_lock.Free;
  tasks.Free;
  queueevent.Free;
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

    if fBusyDestinations.ContainsKey(s2) then
    begin
      Debug(dpSpam, section, 'Destination site %s is busy, skip race task assign from %s', [s2.Name, s1.Name]);
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
      if i > s1.slots.Count then
      begin
        ss1 := nil;
        Break;
      end;

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
        if ((ss.todotask = nil) and (ss.Status <> ssOnline)) then
          Break
        else
          ss := nil;
      end;
    end;

    if ss = nil then
    begin
      // all slots are busy, which means they are already logged in, we can stop here
      if not t.noannounce then
      begin
        if bnc = '' then
          irc_Addtext(t, '<b>%s</b> IS ALREADY BEING TESTED', [t.site1])
        else
          irc_Addtext(t, '<b>%s</b> IS ALREADY UP: %s', [t.site1, bnc]);
      end;
      s.WorkingStatus := sstUp;
      debug(dpMessage, section, '%s IS UP', [t.site1]);
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
          end;
        end;
      end;
      // only half of the slots for dirlist
      if (actual_count > s.slots.Count div 2) then
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

    main_lock.Enter('AddTask');
    try
      if TaskAlreadyInQueue(t) then
      begin
        TaskReady(t);
        t.Free;
        exit;
      end;

      tasks.Add(t);

      try
        if ((t is TPazoRaceTask) and (not t.ready) and t.IsReadyToBeExecuted and (TSite(fSite).freeslots > 0)) then
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
  try
    if t is TPazoRaceTask and (TPazoRaceTask(t).ps2.error or

      // for subdirs that fail there might only be that dir marked as failed, so if a dir is given, check this as well
      (TPazoRaceTask(t).dir <> '') and TPazoRaceTask(t).ps2.dirlist.FindDirList(TPazoRaceTask(t).dir).error) then
    begin
      t.readyerror := true;
      Debug(dpSpam, section, Format('AddTask: race failed on source or destination site: %s', [t.Name]));
      exit
    end;
  except
    on e: Exception do
    begin
      // expect to get some exceptions because we are outside of the queue lock and accessing a task
      Debug(dpSpam, section, Format('[EXCEPTION] AddTask check for failed pazo: %s', [e.Message]));
      exit;
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
begin
  Result := False;
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
begin
  while ((not slshutdown) and (not Terminated)) do
  begin
    queue_last_run := Now();

    if fSite = nil then
      fSite := FindSiteByName('', fSiteName);

    if fSite = nil then
    begin
      //happens on startup
      Sleep(1000);
      continue;
    end;

    ts := TSite(fSite);
    fBusyDestinationsTmp := fBusyDestinations;
    fBusyDestinations := TDictionary<TObject, integer>.Create;
    //Debug(dpSpam, section, 'Queue Iteration begin [%d tasks]', [tasks.Count]);
    try
      main_lock.Enter('Execute');
      try
        for i := tasks.Count - 1 downto 0 do
        begin
          if i < 0 then
            Break;

          fTask := TTask(tasks.items[i]);

          if fTask = nil then
            Continue;

          try
            if (((fTask.ready) or (fTask.readyerror)) and (fTask.slot1 = nil)) then
            begin
              ss := fTask.uidtext;
              TaskReady(fTask);

              if (fTask.ClassType = TPazoRaceTask) then
              begin
                with TPazoRaceTask(fTask) do
                if (dst <> nil) then
                begin
                  dst.event.SetEvent;
                end;
              end;
              ts.AcquireSlotsAssignmentLock('Queue remove ready tasks');
              try
                //t := NIL;
                tasks.Remove(fTask);
              finally
                ts.ReleaseSlotsAssignmentLock;
               end;
               Console_QueueDel(ss);
               end;
              except
                on e: Exception do
                begin
                  Debug(dpError, section, Format('[EXCEPTION] TQueueThread.Execute: %s', [e.Message]));
                  Continue;
                end;
          end;
        end;

        ts.AcquireSlotsAssignmentLock('Queue iterate');
        try
          for fTask in tasks do
          begin
            try
              if ts.freeslots = 0 then
              begin
                //Debug(dpSpam, section, Format('No free slots on %s', [ts.Name]));
                break;
              end;

              if ((fTask.slot1 = nil) and (fTask.slot2 = nil) and (not fTask.ready) and
                (not fTask.readyerror)) then
              begin
                if ((fTask.startat = 0) or (fTask.startat <= queue_last_run)) then
                begin
                  if fTask.IsReadyToBeExecuted then
                    TryToAssignSlots(fTask);
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

      //Debug(dpSpam, section, 'Queue Iteration end [%d tasks]', [tasks.Count]);
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] TQueueThread.Execute : %s', [e.Message]));
      end;
    end;

    //queueevent.WaitFor($FFFFFFFF);
    case queueevent.WaitFor(15 * 1000) of
      wrSignaled: { Event fired. Normal exit. }
      begin

      end;
      else { Timeout reach }
      begin
        if spamcfg.readbool(section, 'queue_recycle', True) then
          irc_Adderror(Format('TQueueThread.Execute: <c2>Force Leave</c>: TQueueThread Recycle 15s (%s)', [self.fSiteName]));
        Debug(dpMessage, section,
          Format('TQueueThread.Execute: Force Leave: TQueueThread Recycle 15s (%s)', [self.fSiteName]));
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

  StatsList := TObjectList<TQueueStat>.Create(True);
end;

procedure QueueUninit;
begin
  StatsList.Free;
end;

procedure TQueueThread.QueueClean(run_now: boolean = False);
var
  i, tkill_unassigne, tkill_race, tkill_other: integer;
  ss: String;
  t:  TTask;
  ts, ts2: TSite;
begin

  try

  if not config.ReadBool(section, 'enable_queueclean', False) then
  begin
    queueclean_last_run := Now;
    exit;
  end;

  if fSite = nil then
    exit;

  ts := TSite(fSite);

  //irc_Addconsole('QueueClean: process begin');
  //Debug(dpMessage, section, 'QueueClean begin %d', [tasks.Count]);
  tkill_unassigne := 0;
  tkill_race      := 0;
  tkill_other     := 0;

  try
    // Check old unassigne task
    main_lock.Enter('QueueClean1');
    for t in tasks do
    begin
      try
        ss := t.UidText;
        if ((t.assigned = 0) and not t.dontremove and ((t.startat = 0) or (t.startat <= queue_last_run)) and
          (SecondsBetween(t.created, Now()) >= queueclean_unassigned)) then
        begin
          try
            t.ready := True;
            Debug(dpError, section, Format('QueueClean: Remove Unassigned : %s', [t.Name]));
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
              Debug(dpSpam, section, Format('[QUEUECLEAN] Clean race task : %s', [t.FullName]));
              Debug(dpError, section, Format('QueueClean: Remove : %s', [t.Name]));
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
            Debug(dpSpam, section, Format('[QUEUECLEAN] Clean wait task : %s', [t.Fullname]));
            ts.AcquireSlotsAssignmentLock('QueueClean wait');
            try
              Debug(dpError, section, Format('QueueClean: Remove : %s', [t.Name]));
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
            //t := NIL;
            Debug(dpSpam, section, Format('[QUEUECLEAN] Clean other task : %s', [t.Fullname]));
            ts.AcquireSlotsAssignmentLock('QueueClean other');
            try
              Debug(dpError, section, Format('QueueClean: Remove : %s', [t.Name]));
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

  QueueStat;

  //Debug(dpMessage, section, 'QueueClean end %d', [tasks.Count]);
end;

procedure TQueueThread.QueueStat;
var
  t_race, t_dir, t_auto, t_other: integer;
  fTask: TTask;
begin
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
  QueueStatAll;
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

  if MilliSecondsBetween(Now, QueueStatUpdateDateTime) > 500 then
  begin
    QueueStatUpdateDateTime := Now;
    Console_QueueStat(t_race + t_dir + t_auto + t_other, t_race, t_dir, t_auto, t_other);
  end;

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
      end
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
