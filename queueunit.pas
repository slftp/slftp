unit queueunit;

interface

uses
  Classes, Contnrs, tasksunit, taskrace, SyncObjs, slcriticalsection, pazo, taskidle, taskquit, tasklogin, RegExpr, sitesunit;

type
  TQueueThread = class(TThread)
    main_lock: TCriticalSection;
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
    procedure TryToAssignSlots(t: TTask);
  private
    procedure TryToAssignLoginSlot(t: TLoginTask);
    procedure TryToAssignRaceSlots(t: TPazoRaceTask);
    procedure AddIdleTask(s: TSiteSlot);
    procedure AddQuitTask(s: TSiteSlot);
    { Removes a race task if one already exists at the destination with the associated dirname and file of the given race task
       @param(aRaceTask single race task picked from the complete task list by the main TQueueThread execution)
    }
    procedure RemoveActiveTransfer(const aRaceTask: TPazoRaceTask);
  end;

procedure QueueFire;
procedure QueueStart;
procedure AddTask(t: TTask);
procedure QueueEmpty(const sitename: String);
procedure RemovePazoMKDIR(const pazo_id: integer; const sitename, dir: String);
procedure RemovePazoRace(const pazo_id: integer; const dstsite, dir, filename: String);

function RemovePazo(const pazo_id: integer): boolean;

procedure RemoveRaceTasks(const pazo_id: integer; const sitename: String);
procedure RemoveDirlistTasks(const pazo_id: integer; const sitename: String);
procedure QueueInit;
procedure QueueUninit;

procedure QueueSort;

procedure QueueClean(run_now: boolean = False);

procedure QueueStat;
{ Send the current tasks to the queue console window. }
procedure QueueSendCurrentTasksToConsole;

var
  queue_lock: TslCriticalSection;
  tasks:      TObjectList;
  queueth:    TQueueThread;
  queueevent: TEvent;

  queue_last_run: TDateTime;
  queueclean_last_run: TDateTime;
  queue_debug_mode: boolean = False;

implementation

uses
  SysUtils, Types, irc, DateUtils, debugunit, notify, console, kb, mainthread, Math, configunit, mrdohutils, taskautonuke, taskautodirlist, taskautoindex,
  tasktvinfolookup, taskhttpnfo, taskrules, tasksitenfo;

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

procedure QueueFire;
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

procedure QueueSort;
begin
  try
    Debug(dpSpam, section, 'Sorting queue 1');
    queueth.main_lock.Enter();
    try
      tasks.Sort(@QueueSorter);
    finally
      queueth.main_lock.Leave;
    end;
    Debug(dpSpam, section, 'Sorting queue 2');
  except
    on e: Exception do
    begin
      Debug(dpError, section, '[EXCEPTION] QueueSort : %s', [e.Message]);
    end;
  end;
end;

procedure QueueStart;
begin
  QueueStat;
end;

constructor TQueueThread.Create;
begin
  inherited Create(False);
  {$IFDEF DEBUG}
    NameThreadForDebugging('Queue', self.ThreadID);
  {$ENDIF}

  main_lock := TCriticalSection.Create;
end;

destructor TQueueThread.Destroy;
begin
  main_lock.Free;
  inherited;
end;

procedure TQueueThread.TryToAssignRaceSlots(t: TPazoRaceTask);
var
  s1, s2: TSite;
  i: integer;
  ss1, ss2: TSiteSlot;
  tt: TTask;
  tpr: TPazoRaceTask;
  fSlotOnline: Boolean;
begin
  fSlotOnline := False;
  try
    s1 := TSite(t.ssite1);
    s2 := TSite(t.ssite2);
    if s1.freeslots = 0 then
      exit;
    if s2.freeslots = 0 then
      exit;

    // first watch if it is not already in process to upload the same file to the same place
    if t.ps2.activeTransfers.IndexOf(t.dir + t.filename) <> -1 then
      exit; // we are already sending this file to the same destination site

    ss1 := nil;
    for i := 0 to s1.slots.Count - 1 do
    begin
      try
        if i > s1.slots.Count then
        begin
          ss1 := nil;
          Break;
        end;
      except
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

    // or use 'if t.ps1.StatusRealPreOrShouldPre then' from pazo.pas but will also pre true when status = rssShouldPre
    //if t.ps1.status = rssRealPre then
    if t.ps1.StatusRealPreOrShouldPre then
    begin
      if s1.num_dn >= ss1.site.max_pre_dn then
        exit;
    end
    else
    begin
      if s1.num_dn >= ss1.site.max_dn then
        exit;
    end;

    ss2 := nil;
    for i := 0 to s2.slots.Count - 1 do
    begin
      try
        if i > s2.slots.Count then
        begin
          ss2 := nil;
          Break;
        end;
      except
        Break;
      end;

      if TSiteSlot(s2.slots[i]).todotask = nil then
      begin
        // available slot we might use
        if not fSlotOnline then
        begin
          ss2 := TSiteSlot(s2.slots[i]);
          if ss2.status = ssOnline then
          begin
            // slot online and available for a new task
            fSlotOnline := True;
          end;
        end;
      end
      else
      begin
        tt := TSiteSlot(s2.slots[i]).todotask;
        if tt <> nil then
        begin
          // check for already existing tasks to avoid duping ourself
          if tt.ClassType = TPazoRaceTask then
          begin
            tpr := TPazoRaceTask(tt);
            if ((tpr.site2 = t.site2) and (tpr.dir = t.dir) and (tpr.filename = t.filename)) then
            begin
              // already trading the file to that site
              exit;
            end;

            if ((tpr.site2 = t.site1) and (tpr.site1 = t.site2) and (tpr.dir = t.dir) and (tpr.filename = t.filename)) then
            begin
              // already trading the opposite route
              exit;
            end;
          end;
        end;
      end;
    end;
    if ss2 = nil then
      exit;
    if s2.num_up >= ss2.site.max_up then
      exit;

    // now you can relax, just check if you don't abuse your max simultaneous uploads for a rip
    i := ss2.site.MaxUpPerRip;
    if ((i > 0) and (t.ps2.activeTransfers.Count >= i)) then
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
    t.ps2.activeTransfers.AddObject(t.dir + t.filename, t);
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

    if ss = nil then
    begin
      // all slots are busy, which means they are already logged in, we can stop here
      if not t.noannounce then
      begin
        if bnc = '' then
          irc_Addtext(t, '<b>%s</b> IS ALREADY BEING TESTED', [t.site1])
        else
          irc_Addtext(t, '<b>%s</b> IS ALREADY UP: %s', [t.site1, bnc]);

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
  sso: boolean;
  actual_count: integer;
begin
  // Debug(dpSpam, section, 'TryToAssignSlots profile '+t.Fullname);

  try
  s := TSite(t.ssite1);
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
      if not TLoginTask(t).readd then
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
        if ((sst.todotask <> nil) and (sst.todotask.ClassType = TPazoDirlistTask)) then
        begin
          Inc(actual_count);
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
      if (ss.todotask <> nil) then
        exit;
    end;

    if ss = nil then
    begin
      sso := False;
      for i := 0 to s.slots.Count - 1 do
      begin
        try
          if i > s.slots.Count then
            Break;
        except
          Break;
        end;
        if TSiteSlot(s.slots[i]).todotask = nil then
        begin
          if not sso then
          begin
            ss := TSiteSlot(s.slots[i]);
            if ((ss.status = ssOnline) OR (ss.site.Name = 'SLFTP')) then
              sso := True;
          end;
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
  except
  on e: Exception do
    begin
      Debug(dpError, section, '[EXCEPTION] TQueueThread.TryToAssignSlots : %s', [e.Message]);
    end;
  end;
end;

procedure TQueueThread.AddQuitTask(s: TSiteSlot);
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

procedure TQueueThread.AddIdleTask(s: TSiteSlot);
var
  ti: TIdleTask;
  i:  integer;
  tt: TTask;
begin
  try
    for i := 0 to tasks.Count - 1 do
    begin
      try
        if i > tasks.Count then
          Break;
      except
        Break;
      end;
      tt := TTask(tasks[i]);
      try
        if ((tt.ClassType = TIdleTask) and (tt.slot1name = s.Name)) then
        begin
          exit;
        end;
      except
        Break;
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

// IT IS ONLY GIVEN TO CALL
procedure QueueEmpty(const sitename: String);
var
  i: integer;
  t: TTask;
begin
  Debug(dpSpam, section, 'QueueEmpty start: ' + sitename);

  for i := tasks.Count - 1 downto 0 do
  begin
    if i < 0 then
      Break;
    try
      t := TTask(tasks[i]);
      if ((not t.ready) and (t.slot1 = nil) and (not t.dontremove) and ((t.site1 = sitename) or (t.site2 = sitename))) then
        t.readyerror := True;

      if (t is TPazoTask) then
        TPazoTask(t).mainpazo.SiteDown(sitename);
    except
      Continue;
    end;
  end;
  Debug(dpSpam, section, 'QueueEmpty end: ' + sitename);
end;

function TaskAlreadyInQueue(t: TTask): boolean;
var
  i:    integer;
  tpr, i_tpr: TPazoRaceTask;
  tpd, i_tpd: TPazoDirlistTask;
  tpm, i_tpm: TPazoMkdirTask;

begin
  Result := False;

  if (t is TPazoRaceTask) then
  begin
    try
      tpr := TPazoRaceTask(t);
      queueth.main_lock.Enter;
      try
        for i := tasks.Count - 1 downto 0 do
        begin
          try
            if i < 0 then Break;
          except
            Break;
          end;
          try
            if (tasks[i] is TPazoRaceTask) then
            begin
              i_tpr := TPazoRaceTask(tasks[i]);
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
            Continue;
          end;
        end;
      finally
        queueth.main_lock.Leave;
      end;
    except
      on E: Exception do
      begin
        Debug(dpError, 'kb', Format('[EXCEPTION] TaskAlreadyInQueue TPazoRaceTask : %s', [e.Message]));
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
      queueth.main_lock.Enter;
      try
        for i := tasks.Count - 1 downto 0 do
        begin
          try
            if i < 0 then
              Break;
          except
            Break;
          end;
          try
            if (tasks[i] is TPazoDirlistTask) then
            begin
              i_tpd := TPazoDirlistTask(tasks[i]);
              if ((i_tpd.ready = False) and (i_tpd.readyerror = False) and
                (i_tpd.slot1 = nil) and (i_tpd.pazo_id = tpd.pazo_id) and
                (i_tpd.site1 = tpd.site1) and (i_tpd.dir = tpd.dir)) then
              begin
                Result := True;
                exit;
              end;
            end;
          except
            Continue;
          end;
        end;
      finally
        queueth.main_lock.Leave;
      end;
    except
      on E: Exception do
      begin
        Debug(dpError, 'kb', Format('[EXCEPTION] TaskAlreadyInQueue TPazoDirlistTask : %s', [e.Message]));
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
      queueth.main_lock.Enter;
      try
        for i := tasks.Count - 1 downto 0 do
        begin
          try
            if i < 0 then
              Break;
          except
            Break;
          end;
          try
            if (tasks[i] is TPazoMkdirTask) then
            begin
              i_tpm := TPazoMkdirTask(tasks[i]);
              if ((i_tpm.ready = False) and (i_tpm.readyerror = False) and
                (i_tpm.slot1 = nil) and (i_tpm.pazo_id = tpm.pazo_id) and
                (i_tpm.site1 = tpm.site1) and (i_tpm.dir = tpm.dir)) then
              begin
                Result := True;
                exit;
              end;
            end;
          except
            Continue;
          end;
        end;
      finally
        queueth.main_lock.Leave;
      end;
    except
      on E: Exception do
      begin
        Debug(dpError, 'kb', Format('[EXCEPTION] TaskAlreadyInQueue TPazoMkdirTask : %s', [e.Message]));
        Result := False;
        exit;
      end;
    end;
    exit;
  end;
end;

procedure AddTaskToConsole(const aTask: TTask);
begin
  Console_QueueAdd(aTask.UidText, Format('%s', [aTask.Name]));
end;

procedure AddTask(t: TTask);
var
  tname: String;
begin
  try
    tname := t.Name;
    Debug(dpSpam, section, Format('[iNFO] adding : %s', [t.Name]));

    queueth.main_lock.Enter();
    try
      if TaskAlreadyInQueue(t) then
        t.ready := True;

      tasks.Add(t);

      try
        if ((t is TPazoRaceTask) and (not t.ready) and (t.dependencies.Count = 0)) then
        begin
          queueth.TryToAssignSlots(t);
        end;
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[EXCEPTION] AddTask TryToAssignSlots: %s', [e.Message]));
        end;
      end;
    finally
      queueth.main_lock.Leave;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] AddTask tasks.Add: %s', [e.Message]));
      exit;
    end;
  end;

  AddTaskToConsole(t);
end;

procedure RemoveRaceTasks(const pazo_id: integer; const sitename: String);
var
  i:   integer;
  ttp: TPazoRaceTask;
begin
  try
    queueth.main_lock.Enter();
    try
      for i := tasks.Count - 1 downto 0 do
      begin
        try
          if i < 0 then
            Break;
        except
          Break;
        end;
        try
          if (tasks[i] is TPazoRaceTask) then
          begin
            ttp := TPazoRaceTask(tasks[i]);
            if ((ttp.ready = False) and (ttp.readyerror = False) and (ttp.slot1 = nil) and (ttp.pazo_id = pazo_id) and (ttp.site2 = sitename)) then
              ttp.ready := True;
          end;
        except
          Continue;
        end;
      end;
    finally
      queueth.main_lock.Leave;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] RemoveRaceTasks : %s', [e.Message]));
      exit;
    end;
  end;
end;

procedure RemoveDirlistTasks(const pazo_id: integer; const sitename: String);
var
  i:   integer;
  ttp: TPazoDirlistTask;
begin
  try
    queueth.main_lock.Enter();
    try
      for i := tasks.Count - 1 downto 0 do
      begin
        try
          if i < 0 then
            Break;
        except
          Break;
        end;
        try
          if (tasks[i] is TPazoDirlistTask) then
          begin
            ttp := TPazoDirlistTask(tasks[i]);
            if ((ttp.ready = False) and (ttp.readyerror = False) and (ttp.slot1 = nil) and (ttp.pazo_id = pazo_id) and (ttp.site1 = sitename)) then
              ttp.ready := True;
          end;
        except
          Continue;
        end;
      end;
    finally
      queueth.main_lock.Leave;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] RemoveDirlistTasks : %s', [e.Message]));
      exit;
    end;
  end;
end;

function RemovePazo(const pazo_id: integer): boolean;
var
  i: integer;
  t: TPazoTask;
begin
  Result := False;
  try
    queueth.main_lock.Enter();
    try
      for i := tasks.Count - 1 downto 0 do
      begin
        try
          if i < 0 then
            Break;
        except
          Break;
        end;
        try
          if tasks[i] is TPazoTask then
          begin
            t := TPazoTask(tasks[i]);
            if ((t.pazo_id = pazo_id) and (t.slot1 = nil)) then
              t.readyerror := True;
          end;
        except
          Continue;
        end;
      end;
    finally
      queueth.main_lock.Leave;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] RemovePazo : %s', [e.Message]));
      exit;
    end;
  end;
  Result := True;
end;


procedure RemovePazoMKDIR(const pazo_id: integer; const sitename, dir: String);
var
  i:   integer;
  ttp: TPazoMkdirTask;
begin
  try
    queueth.main_lock.Enter();
    try
      for i := tasks.Count - 1 downto 0 do
      begin
        try
          if i < 0 then
            Break;
        except
          Break;
        end;
        try
          if (tasks[i] is TPazoMkdirTask) then
          begin
            ttp := TPazoMkdirTask(tasks[i]);
            if ((ttp.ready = False) and (ttp.readyerror = False) and
              (ttp.slot1 = nil) and (ttp.site1 = sitename) and (ttp.pazo_id = pazo_id) and
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
      queueth.main_lock.Leave;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] RemovePazoMKDIR : %s', [e.Message]));
    end;
  end;
end;

procedure RemovePazoRace(const pazo_id: integer; const dstsite, dir, filename: String);
var
  i:   integer;
  ttp: TPazoRaceTask;
begin
  try
    queueth.main_lock.Enter();
    try
      for i := tasks.Count - 1 downto 0 do
      begin
        try
          if i < 0 then
            Break;
        except
          Break;
        end;
        try
          if (tasks[i] is TPazoRaceTask) then
          begin
            ttp := TPazoRaceTask(tasks[i]);
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
      queueth.main_lock.Leave;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] RemovePazoRace : %s', [e.Message]));
    end;
  end;
end;

procedure RemoveDependencies(t: TTask);
var
  i, j: integer;
  tt: TTask;
begin
  try
    for i := tasks.Count - 1 downto 0 do
    begin
      try
        if i < 0 then
          Break;
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[EXCEPTION] RemoveDependencies (tasks.Count): %s', [e.Message]));
          Break;
        end;
      end;
      try
        tt := TTask(tasks.items[i]);

        if tt = nil then
          Continue;

        j := tt.dependencies.IndexOf(t.UidText);
        if j <> -1 then
        begin
          tt.dependencies.Delete(j);
        end;
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[EXCEPTION] RemoveDependencies (tt.dependencies.Delete): %s', [e.Message]));
          Continue;
        end;
      end;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] RemoveDependencies : %s', [e.Message]));
      exit;
    end;
  end;
end;

procedure TQueueThread.RemoveActiveTransfer(const aRaceTask: TPazoRaceTask);
var
  i: Integer;
begin
  i := aRaceTask.ps2.activeTransfers.IndexOf(aRaceTask.dir + aRaceTask.filename);
  if i <> -1 then
  begin
    aRaceTask.ps2.activeTransfers.Delete(i);
  end;
end;

procedure TQueueThread.Execute;
var
  i, j: integer;
  t:    TTask;
  s:    TSiteSlot;
  ss:   String;
  ts:   TSite;
begin
  while ((not slshutdown) and (not Terminated)) do
  begin
    queue_last_run := Now();
    Debug(dpSpam, section, 'Queue Iteration begin [%d tasks]', [tasks.Count]);
    try
      queueth.main_lock.Enter();
      try
        for i := tasks.Count - 1 downto 0 do
        begin
          try
            if i < 0 then
              Break;
          except
            on e: Exception do
            begin
              Debug(dpError, section, Format('[EXCEPTION] TQueueThread.Execute (tasks.Count) : %s', [e.Message]));
              Break;
            end;
          end;
          try
            t := TTask(tasks.items[i]);
          except
            on e: Exception do
            begin
              Debug(dpError, section, Format('[EXCEPTION] TQueueThread.Execute (t) : %s', [e.Message]));
              Continue;
            end;
          end;

          if t = nil then
            Continue;

          try
            if (((t.ready) or (t.readyerror)) and (t.slot1 = nil)) then
            begin
              ss := t.uidtext;
              TaskReady(t);

              if (t.ClassType = TPazoRaceTask) then
              begin
                with TPazoRaceTask(t) do
                  if (dst <> nil) then
                  begin
                    dst.event.SetEvent;
                  end;
                RemoveActiveTransfer(TPazoRaceTask(t));
              end;
              RemoveDependencies(t);
              tasks.Remove(t);
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

        for i := 0 to tasks.Count - 1 do
        begin
          try
            if i > tasks.Count then
              Break;
          except
            on e: Exception do
            begin
              Debug(dpError, section, Format('[EXCEPTION] TQueueThread.Execute (tasks.Count) : %s', [e.Message]));
              Break;
            end;
          end;

          try
            t := TTask(tasks.items[i]);
          except
            on e: Exception do
            begin
              Debug(dpError, section, Format('[EXCEPTION] TQueueThread.Execute (t) : %s', [e.Message]));
              Continue;
            end;
          end;

          if t = nil then
            Continue;

          try
            if queue_debug_mode then
              Continue;

            if ((t.slot1 = nil) and (t.slot2 = nil) and (not t.ready) and
              (not t.readyerror)) then
            begin
              if ((t.startat = 0) or (t.startat <= queue_last_run)) then
              begin
                if (t.dependencies.Count = 0) then
                  TryToAssignSlots(t);
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
        queueth.main_lock.Leave;
      end;

      QueueStat;

      // We are looking for idle
      for i := 0 to sites.Count - 1 do
      begin
        try
          if i > sites.Count then
            Break;
        except
          on e: Exception do
          begin
            Debug(dpError, section, Format('[EXCEPTION] TQueueThread.Execute (sites.Count) : %s', [e.Message]));
            Break;
          end;
        end;
        try
          ts := TSite(sites[i]);
        except
          on e: Exception do
          begin
            Debug(dpError, section, Format('[EXCEPTION] TQueueThread.Execute (ts) : %s', [e.Message]));
            Continue;
          end;
        end;

        for j := 0 to ts.slots.Count - 1 do
        begin
          try
            if j > ts.slots.Count then
              Break;
          except
            on e: Exception do
            begin
              Debug(dpError, section, Format('[EXCEPTION] TQueueThread.Execute (ts.slots.Count) : %s', [e.Message]));
              Break;
            end;
          end;
          try
            s := TSiteSlot(ts.slots[j]);
          except
            on e: Exception do
            begin
              Debug(dpError, section, Format('[EXCEPTION] TQueueThread.Execute (s) : %s', [e.Message]));
              Continue;
            end;
          end;

          if s = nil then
            Continue;

          try
            if ((s.todotask = nil) and (s.site.Name <> getAdminSiteName)) then
            begin
              if ((s.status = ssOnline) and ((s.site.WorkingStatus in [sstMarkedAsDownByUser]) or ((s.site.maxidle <> 0) and
                (MilliSecondsBetween(queue_last_run, s.LastNonIdleTaskExecution) >= s.site.maxidle * 1000)))) then
              begin
                AddQuitTask(s);
              end
              //we also want idle tasks to relogin slots that are not ssOnline but the sites are in WorkingStatus sstUp
              //at startup only few slots are needed (e.g. autologin), but we want all the slots to be ready for action if
              //an idle interval is configured. also there are several occasions where DestroySocket or Quit are invoked
              //on a slot. the IdleTask will take care to relogin these slots as well.
              else if (((s.status = ssOnline) or ((s.site.WorkingStatus in [sstUp]) and
              ((s.site.maxidle = 0) or (MilliSecondsBetween(queue_last_run, s.LastNonIdleTaskExecution) < s.site.maxidle * 1000))))
              and (MilliSecondsBetween(queue_last_run, s.LastIO) > s.site.idleinterval * 1000)) then
              begin
                AddIdleTask(s);
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
      end;

      Debug(dpSpam, section, 'Queue Iteration end [%d tasks]', [tasks.Count]);
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
          irc_Adderror('TQueueThread.Execute: <c2>Force Leave</c>: TQueueThread Recycle 15s');
        Debug(dpMessage, section,
          'TQueueThread.Execute: Force Leave: TQueueThread Recycle 15s');
      end;
    end;
  end;
end;

procedure QueueInit;
begin
  tasks      := TObjectList.Create(True);
  queue_lock := TslCriticalSection.Create('queue_lock');
  queueevent := TEvent.Create(nil, False, False, 'queue');
  queue_last_run := Now;
  queueclean_last_run := Now;

  queueth := TQueueThread.Create;
  queueth.FreeOnTerminate := True;

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
end;

procedure QueueUninit;
begin
  Debug(dpSpam, section, 'Uninit1');
  tasks.Free;
  kb_FreeList;

  queue_lock.Free;
  queueevent.Free;
  Debug(dpSpam, section, 'Uninit2');
end;

procedure QueueClean(run_now: boolean = False);
var
  i, tkill_unassigne, tkill_race, tkill_other: integer;
  ss: String;
  t:  TTask;
begin

  if not config.ReadBool(section, 'enable_queueclean', False) then
  begin
    queueclean_last_run := Now;
    exit;
  end;

  //irc_Addconsole('QueueClean: process begin');
  Debug(dpMessage, section, 'QueueClean begin %d', [tasks.Count]);
  tkill_unassigne := 0;
  tkill_race      := 0;
  tkill_other     := 0;

  // Check old unassigne task
  for i := tasks.Count - 1 downto 0 do
  begin
    try
      if i < 0 then
        Break;
    except
      Break;
    end;
    try
      ss := TTask(tasks[i]).UidText;
      t  := TTask(tasks[i]);
      if ((t.assigned = 0) and not t.dontremove and ((t.startat = 0) or (t.startat <= queue_last_run)) and
        (SecondsBetween(t.created, Now()) >= config.ReadInteger('queue',
        'queueclean_unassigned', 600))) then
      begin
        try
          t.ready := True;
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
      end;
    except
      Break;
    end;
  end;

  // Check old tasks, assigned bu long time wait
  queueth.main_lock.Enter();
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
        (SecondsBetween(t.assigned, Now()) >= config.ReadInteger('queue',
        'queueclean_maxrunning', 900))) then
      begin
        if (t.ClassType = TPazoRaceTask) then
        begin
          if (t.slot1 <> nil) then
          begin
            try
              TSiteSlot(t.slot1).todotask := nil;
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

          if (t.slot2 <> nil) then
          begin
            try
              TSiteSlot(t.slot2).todotask := nil;
              TSiteSlot(t.slot2).downloadingfrom := False;
              TSiteSlot(t.slot2).uploadingto := False;
              t.slot2     := nil;
              t.slot2name := '';
            except
              on e: Exception do
              begin
                Debug(dpError, section,
                  Format('[EXCEPTION] slot2 QueueClean: Exception : %s', [e.Message]));
              end;
            end;
          end;

          try
            tasks.Remove(t);
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

        if (t.ClassType = TWaitTask) then
        begin
          with TWaitTask(t) do
            event.SetEvent;

          try
            tasks.Remove(t);
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
              TSiteSlot(t.slot1).todotask := nil;
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
            tasks.Remove(t);
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
    queueth.main_lock.Leave;
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
  queueclean_last_run := Now;

  QueueStat;

  Debug(dpMessage, section, 'QueueClean end %d', [tasks.Count]);
end;

procedure QueueStat;
var
  i, t_race, t_dir, t_auto, t_other: integer;
begin
  t_race  := 0;
  t_dir   := 0;
  t_auto  := 0;
  t_other := 0;

  for i := tasks.Count - 1 downto 0 do
  begin
    try
      if i < 0 then
        Break;
    except
      Break;
    end;
    try
      if ((tasks[i].ClassType = TPazoRaceTask) or (tasks[i].ClassType = TWaitTask)) then
        Inc(t_race)
      else if ((tasks[i].ClassType = TPazoDirlistTask)) then
        Inc(t_dir)
      else if ((tasks[i].ClassType = TAutoNukeTask) or (tasks[i].ClassType = TAutoDirlistTask) or
        (tasks[i].ClassType = TAutoIndexTask) or (tasks[i].ClassType = TLoginTask) or
        (tasks[i].ClassType = TRulesTask)) then
        Inc(t_auto)
      else
        Inc(t_other);
    except
      Continue;
    end;
  end;

  Console_QueueStat(tasks.Count, t_race, t_dir, t_auto, t_other);
end;

procedure QueueSendCurrentTasksToConsole;
var
  fTask: TTask;
begin
  queueth.main_lock.Enter;
  try
    for fTask in tasks do
      AddTaskToConsole(fTask);
  finally
    queueth.main_lock.Leave;
  end;
end;

end.

