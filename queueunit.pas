unit queueunit;

{$I queuediagnostics.inc}

interface

uses
  {$IFDEF QUEUE_DIAGNOSTICS}queuediagnostics,{$ENDIF}
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
  { One-shot, site-local subscriptions for queues waiting on destination state.
    Stores names instead of queue pointers so deleted sources are not dereferenced. }
  TQueueWakeTargets = class
  private
    FLock: TCriticalSection;
    FSites: TDictionary<String, Byte>;
    FRevision: Int64;
    {$IFDEF QUEUE_DIAGNOSTICS}FChangedAt: Int64;{$ENDIF}
  public
    { Creates an empty subscription set. }
    constructor Create;
    { Releases the subscription set. }
    destructor Destroy; override;
    { Registers a source once and returns the capacity revision under the same lock. }
    function RegisterSite(const aSiteName: String): Int64;
    { Atomically takes and clears the sources to notify, without holding the lock during callbacks. }
    function TakeSites: TArray<String>;
    { Returns the revision advanced before each destination notification. }
    function Revision: Int64;
    {$IFDEF QUEUE_DIAGNOSTICS}
    { Latest notification timestamp; not a guarantee of destination readiness. }
    function DiagnosticChangedAt: Int64;
    {$ENDIF}
  end;

  { Capacity blocks expire on a destination notification or a bounded safety retry. }
  TQueueDestinationBlock = record
  private
    FRevision: Int64;
    FRetryAt: TDateTime;
    {$IFDEF QUEUE_DIAGNOSTICS}FReason: TQueueBlockReason;{$ENDIF}
  end;

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
  fBusyDestinations: TDictionary<TObject, TQueueDestinationBlock>;
  FSortPending: Boolean;
  FNextTaskAt: TDateTime;
  FAssignmentRetryAt: TDateTime;
  {$IFDEF QUEUE_DIAGNOSTICS}
  FDiagnostics: TQueueDiagnostics;
  {$ENDIF}

  queue_last_run: TDateTime;
  queueclean_last_run: TDateTime;
  queue_last_stat_update: TDateTime;

    procedure TryToAssignLoginSlot(t: TLoginTask);
    procedure TryToAssignRaceSlots(t: TPazoRaceTask);
    function TaskAlreadyInQueue(t: TTask): boolean;
    procedure QueueStat;
    {$IFDEF QUEUE_DIAGNOSTICS}
    procedure ReportDiagnostics;
    procedure DiagnosticBlockTask(aTask: TTask; const aReason: TQueueBlockReason;
      const aCacheReason: TQueueBlockReason = qbNone; const aRetryMs: Int64 = 0;
      const aRevision: Int64 = 0; const aUsed: Integer = -1; const aLimit: Integer = -1);
    procedure DiagnosticBeginTask(aTask: TTask);
    procedure DiagnosticFinishTask(aTask: TTask; const aOutcome: String);
    procedure DiagnosticSampleTask(aTask: TTask; const aNowUs: Int64; const aOutcome: String);
    {$ENDIF}

protected
{ Waits for a retained work signal, the earliest deadline, or housekeeping. }
function WaitForWake(const aMaxWait: Cardinal = High(Cardinal)): TWaitResult;
{ Applies coalesced sort requests. The caller must have a slot available. }
function ApplyPendingSort: Boolean;
{ Moves due tasks into the dispatch list and records only future task deadlines.
  Must be called while holding main_lock. }
function MoveWaitingTasks(const aNow: TDateTime): TDateTime;
{ Records actual assignment exceptions; normal capacity waits do not consume retries. }
procedure RecordAssignmentFailure(const aTask: TTask);
{ Remembers a site-wide capacity block, not release-specific dependencies. }
procedure RememberBlockedDestination(aDestination: TObject; const aRevision: Int64;
  const aRetryAt: TDateTime{$IFDEF QUEUE_DIAGNOSTICS}; const aReason: TQueueBlockReason = qbUnknown{$ENDIF});
{ Checks a retained block against the destination's current revision and retry deadline. }
function DestinationBlocked(aDestination: TObject; const aRevision: Int64;
  const aNow: TDateTime): Boolean;

public

procedure QueueFire({$IFDEF QUEUE_DIAGNOSTICS}const aReason: TQueueWakeReason = qwExternal{$ENDIF});
procedure QueueStart;
{ Stops and joins the worker before site/slot state is released. }
procedure QueueStop;
{ Takes ownership after insertion; due work signals by default, including metadata.
  Batch producers may suppress the signal and fire once when the batch is complete. }
procedure AddTask(t: TTask; const aQueueFire: Boolean = True);
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
{ Returns the bounded wait for an absolute deadline; MaxDateTime means no deadline. }
function QueueWaitMilliseconds(const aNow, aDeadline: TDateTime; const aFallback: Cardinal): Cardinal;
procedure QueueInit;
procedure QueueUninit;
procedure QueueStatAll;

var
  QueueStatUpdateDateTime: TDateTime;

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
  enable_queueclean: boolean;

  StatsList: TObjectList<TQueueStat>;
  glQueueFallbackMs: Cardinal = 15 * 1000;

constructor TQueueWakeTargets.Create;
begin
  inherited;
  FLock := TCriticalSection.Create;
  FSites := TDictionary<String, Byte>.Create;
end;

destructor TQueueWakeTargets.Destroy;
begin
  FSites.Free;
  FLock.Free;
  inherited;
end;

function TQueueWakeTargets.RegisterSite(const aSiteName: String): Int64;
begin
  FLock.Enter;
  try
    FSites.AddOrSetValue(aSiteName, 0);
    Result := FRevision;
  finally
    FLock.Leave;
  end;
end;

function TQueueWakeTargets.TakeSites: TArray<String>;
begin
  FLock.Enter;
  try
    Inc(FRevision);
    {$IFDEF QUEUE_DIAGNOSTICS}FChangedAt := QueueDiagnosticNow;{$ENDIF}
    Result := FSites.Keys.ToArray;
    FSites.Clear;
  finally
    FLock.Leave;
  end;
end;

function TQueueWakeTargets.Revision: Int64;
begin
  FLock.Enter;
  try
    Result := FRevision;
  finally
    FLock.Leave;
  end;
end;

{$IFDEF QUEUE_DIAGNOSTICS}
function TQueueWakeTargets.DiagnosticChangedAt: Int64;
begin
  FLock.Enter;
  try
    Result := FChangedAt;
  finally
    FLock.Leave;
  end;
end;
{$ENDIF}

procedure TQueueThread.QueueFire({$IFDEF QUEUE_DIAGNOSTICS}const aReason: TQueueWakeReason = qwExternal{$ENDIF});
begin
  try
    //Debug(dpSpam, section, Format('QueueFire: %s', [(TSite(fSite).Name)]));
    {$IFDEF QUEUE_DIAGNOSTICS}FDiagnostics.Signal(aReason);{$ENDIF}
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

function QueueWaitMilliseconds(const aNow, aDeadline: TDateTime; const aFallback: Cardinal): Cardinal;
var
  fMilliseconds: Int64;
begin
  Result := aFallback;
  if aDeadline = MaxDateTime then
    Exit;
  if aDeadline <= aNow then
    Exit(0);
  { Round the remaining span UP: timestamp rounding can turn a future deadline
    into zero milliseconds and make WaitFor spin until the wall clock advances. }
  fMilliseconds := Max(Int64(1), Ceil((aDeadline - aNow) * MSecsPerDay));
  if fMilliseconds < aFallback then
    Result := Cardinal(fMilliseconds);
end;

procedure TQueueThread.RecordAssignmentFailure(const aTask: TTask);
begin
  Inc(aTask.TryToAssign);
  if (maxassign <> 0) and (aTask.TryToAssign > maxassign) then
  begin
    {$IFDEF QUEUE_DIAGNOSTICS}FDiagnostics.Count(qcRetryDelay);{$ENDIF}
    aTask.TryToAssign := 0;
    if maxassign_delay = 0 then
      aTask.ready := True
    else
      aTask.startat := IncSecond(Now, maxassign_delay);
  end;
end;

procedure TQueueThread.RememberBlockedDestination(aDestination: TObject;
  const aRevision: Int64; const aRetryAt: TDateTime{$IFDEF QUEUE_DIAGNOSTICS}; const aReason: TQueueBlockReason{$ENDIF});
var
  fBlock: TQueueDestinationBlock;
begin
  fBlock.FRevision := aRevision;
  fBlock.FRetryAt := aRetryAt;
  {$IFDEF QUEUE_DIAGNOSTICS}fBlock.FReason := aReason;{$ENDIF}
  fBusyDestinations.AddOrSetValue(aDestination, fBlock);
  if aRetryAt < FAssignmentRetryAt then
    FAssignmentRetryAt := aRetryAt;
end;

function TQueueThread.DestinationBlocked(aDestination: TObject;
  const aRevision: Int64; const aNow: TDateTime): Boolean;
var
  fBlock: TQueueDestinationBlock;
begin
  Result := fBusyDestinations.TryGetValue(aDestination, fBlock) and
    (fBlock.FRevision = aRevision) and (aNow < fBlock.FRetryAt);
  if Result then
  begin
    { Other work may wake this queue before the block expires. Retain its retry. }
    if fBlock.FRetryAt < FAssignmentRetryAt then
      FAssignmentRetryAt := fBlock.FRetryAt;
  end
  else
    fBusyDestinations.Remove(aDestination);
end;

{ Only future deadlines are retained: already due but blocked tasks wait for a state change. }
procedure _ConsiderQueueDeadline(const aNow, aDeadline: TDateTime; var aNext: TDateTime);
begin
  if (aDeadline > aNow) and (aDeadline < aNext) then
    aNext := aDeadline;
end;

function TQueueThread.WaitForWake(const aMaxWait: Cardinal = High(Cardinal)): TWaitResult;
var
  fTimeout: Cardinal;
  fNow: TDateTime;
  {$IFDEF QUEUE_DIAGNOSTICS}
  fFallbackWait: Boolean;
  fTask: TTask;
  fDiagnosticWaitAt, fDiagnosticWaitUs, fDiagnosticClockLagUs: Int64;
  {$ENDIF}
begin
  main_lock.Enter('Queue wait deadline');
  try
    fNow := Now;
    fTimeout := QueueWaitMilliseconds(fNow, FNextTaskAt, glQueueFallbackMs);
    {$IFDEF QUEUE_DIAGNOSTICS}
    if fTimeout = 0 then
    begin
      FDiagnostics.Count(qcZeroWait);
      if FNextTaskAt > fNow then
        FDiagnostics.Count(qcFutureZeroWait);
    end;
    {$ENDIF}
    if fTimeout > aMaxWait then
      fTimeout := aMaxWait;
    {$IFDEF QUEUE_DIAGNOSTICS}fFallbackWait := (fTimeout = glQueueFallbackMs) and (aMaxWait >= glQueueFallbackMs);{$ENDIF}
  finally
    main_lock.Leave;
  end;
  {$IFDEF QUEUE_DIAGNOSTICS}
  fDiagnosticClockLagUs := QueueDiagnosticClockLag;
  fDiagnosticWaitAt := QueueDiagnosticNow;
  {$ENDIF}
  Result := queueevent.WaitFor(fTimeout);
  {$IFDEF QUEUE_DIAGNOSTICS}
  fDiagnosticWaitUs := QueueDiagnosticNow - fDiagnosticWaitAt;
  main_lock.Enter('Queue diagnostic wake');
  try
    FDiagnostics.RecordWait(fTimeout, fDiagnosticWaitUs, Result, fDiagnosticClockLagUs);
    if Result = wrSignaled then
      FDiagnostics.Count(qcEvent)
    else if Result = wrTimeout then
    begin
      if fFallbackWait then
      begin
        FDiagnostics.Count(qcFallback);
        if (fSite <> nil) and (TSite(fSite).freeslots > 0) then
          for fTask in tasks do
            if (fTask.slot1 = nil) and (fTask.slot2 = nil) and
              not (fTask.ready or fTask.readyerror) and (fTask.startat <= Now) then
            begin
              FDiagnostics.Count(qcFallbackWithWork);
              Break;
            end;
      end
      else if fTimeout = aMaxWait then
        FDiagnostics.Count(qcCappedWait)
      else
        FDiagnostics.Count(qcDeadline);
    end;
  finally
    main_lock.Leave;
  end;
  {$ENDIF}
end;

function TQueueThread.MoveWaitingTasks(const aNow: TDateTime): TDateTime;
var
  i: Integer;
  fTask: TTask;
begin
  Result := MaxDateTime;
  for i := waiting_tasks.Count - 1 downto 0 do
  begin
    fTask := TTask(waiting_tasks[i]);
    if fTask.startat <= aNow then
    begin
      waiting_tasks.Extract(fTask);
      tasks.Add(fTask);
      FSortPending := True;
      {$IFDEF QUEUE_DIAGNOSTICS}FDiagnostics.Count(qcPromoted); DiagnosticBlockTask(fTask, qbNotChecked);{$ENDIF}
    end
    else
      _ConsiderQueueDeadline(aNow, fTask.startat, Result);
  end;
  FNextTaskAt := Result;
end;

procedure TQueueThread.QueueSort;
begin
  main_lock.Enter('Queue sort request');
  try
    {$IFDEF QUEUE_DIAGNOSTICS}FDiagnostics.Count(qcSortRequest);{$ENDIF}
    FSortPending := True;
  finally
    main_lock.Leave;
  end;
end;

function TQueueThread.ApplyPendingSort: Boolean;
{$IFDEF QUEUE_DIAGNOSTICS}
var
  fSortAt: Int64;
  {$ENDIF}
begin
  main_lock.Enter('Queue sort');
  try
    Result := FSortPending;
    if Result then
    begin
      {$IFDEF QUEUE_DIAGNOSTICS}FDiagnostics.Count(qcSort); fSortAt := QueueDiagnosticNow;{$ENDIF}
      tasks.Sort(@QueueSorter);
      {$IFDEF QUEUE_DIAGNOSTICS}FDiagnostics.Measure(qtSort, QueueDiagnosticNow - fSortAt);{$ENDIF}
      FSortPending := False;
    end;
  finally
    main_lock.Leave;
  end;
end;

procedure TQueueThread.QueueStart;
begin
  QueueFire;
  QueueStatAll;
end;

procedure TQueueThread.QueueStop;
begin
  Terminate;
  QueueFire;
  if not Suspended then
    WaitFor;
end;

constructor TQueueThread.Create(const aSiteName: String);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  {$IFDEF DEBUG}
    NameThreadForDebugging('Queue/' + aSiteName, self.ThreadID);
  {$ENDIF}
  main_lock := TSLCriticalSection2.Create('Queue_' + aSiteName);
  tasks := TObjectList.Create(True);
  waiting_tasks := TObjectList.Create(True);
  queueevent := TEvent.Create(nil, False, False, '');
  FNextTaskAt := MaxDateTime;
  FAssignmentRetryAt := MaxDateTime;
  {$IFDEF QUEUE_DIAGNOSTICS}FDiagnostics := TQueueDiagnostics.Create;{$ENDIF}
  queue_last_run := Now;
  queueclean_last_run := Now;
  queue_last_stat_update := Now;
  fSiteName := aSiteName;
  fBusyDestinations := TDictionary<TObject, TQueueDestinationBlock>.Create;
  fQueueStat := TQueueStat.Create;
  StatsList.Add(fQueueStat);
  { AddSite starts the thread only after the site has been fully initialized. }
end;

destructor TQueueThread.Destroy;
begin
  Terminate;
  if queueevent <> nil then
    queueevent.SetEvent;
  { Join before releasing any state that Execute can still access. }
  inherited;
  {$IFDEF QUEUE_DIAGNOSTICS}FDiagnostics.Free;{$ENDIF}
  fBusyDestinations.Free;
  waiting_tasks.Free;
  tasks.Free;
  queueevent.Free;
  main_lock.Free;
end;

procedure TQueueThread.TryToAssignRaceSlots(t: TPazoRaceTask);
var
  s1, s2: TSite;
  i: integer;
  ss1, ss2, fSiteSlotLoop: TSiteSlot;
  fDestinationRevision: Int64;
  fNow: TDateTime;
  {$IFDEF QUEUE_DIAGNOSTICS}fDiagnosticBlock: TQueueDestinationBlock;{$ENDIF}
begin
  try
    s1 := TSite(t.ssite1);
    s2 := TSite(t.ssite2);
    if s1.freeslots = 0 then
    begin
      {$IFDEF QUEUE_DIAGNOSTICS}FDiagnostics.Count(qcSourceFull); DiagnosticBlockTask(t, qbSourceFull);{$ENDIF}
      exit;
    end;
    { Subscribe BEFORE checking destination state so concurrent completion cannot be lost. }
    fDestinationRevision := s2.RegisterQueueWaiter(fSiteName);
    fNow := Now;
    if DestinationBlocked(s2, fDestinationRevision, fNow) then
    begin
      {$IFDEF QUEUE_DIAGNOSTICS}FDiagnostics.Count(qcBusyCache);
      if fBusyDestinations.TryGetValue(s2, fDiagnosticBlock) then
        DiagnosticBlockTask(t, qbCachedDestination, fDiagnosticBlock.FReason,
          Max(Int64(0), MilliSecondsBetween(fDiagnosticBlock.FRetryAt, fNow)), fDiagnosticBlock.FRevision);{$ENDIF}
      Exit;
    end;
    if s2.freeslots = 0 then
    begin
      {$IFDEF QUEUE_DIAGNOSTICS}FDiagnostics.Count(qcDestinationFull); DiagnosticBlockTask(t, qbDestinationFull);{$ENDIF}
      RememberBlockedDestination(s2, fDestinationRevision, IncMilliSecond(fNow, glQueueFallbackMs){$IFDEF QUEUE_DIAGNOSTICS}, qbDestinationFull{$ENDIF});
      exit;
    end;

    if s2.MaxSimUpCooldownActive then
    begin
      {$IFDEF QUEUE_DIAGNOSTICS}FDiagnostics.Count(qcCooldown); DiagnosticBlockTask(t, qbCooldown, qbNone, s2.MaxSimUpCooldownRemainingSeconds * 1000);{$ENDIF}
      RememberBlockedDestination(s2, fDestinationRevision, s2.MaxSimUpCooldownUntil{$IFDEF QUEUE_DIAGNOSTICS}, qbCooldown{$ENDIF});
      Debug(dpSpam, section, '[MAXSIM COOLDOWN] Destination site %s is on MaxSim UP cooldown (%ds remaining), skipping %s',
        [s2.Name, s2.MaxSimUpCooldownRemainingSeconds, t.FullName]);
      exit;
    end;

    if s1.MaxSimDownCooldownActive then
    begin
      {$IFDEF QUEUE_DIAGNOSTICS}FDiagnostics.Count(qcCooldown); DiagnosticBlockTask(t, qbCooldown, qbNone, s1.MaxSimDownCooldownRemainingSeconds * 1000);{$ENDIF}
      Debug(dpSpam, section, '[MAXSIM COOLDOWN] Source site %s is on MaxSim DOWN cooldown (%ds remaining), skipping %s',
        [s1.Name, s1.MaxSimDownCooldownRemainingSeconds, t.FullName]);
      exit;
    end;

    // first watch if it is not already in process to upload the same file to the same place
    if t.ps2.HasActiveTransfer(t.dir + t.filename) then
    begin
      {$IFDEF QUEUE_DIAGNOSTICS}FDiagnostics.Count(qcActiveFile); DiagnosticBlockTask(t, qbActiveFile);{$ENDIF}
      exit;
    end; // we are already sending this file to the same destination site

    if s2.num_up >= s2.max_up then
    begin
      {$IFDEF QUEUE_DIAGNOSTICS}FDiagnostics.Count(qcDestinationLimit); DiagnosticBlockTask(t, qbDestinationLimit, qbNone, 0, 0, s2.num_up, s2.max_up);{$ENDIF}
      RememberBlockedDestination(s2, fDestinationRevision, IncMilliSecond(fNow, glQueueFallbackMs){$IFDEF QUEUE_DIAGNOSTICS}, qbDestinationLimit{$ENDIF});
      exit;
    end;

    if t.ps1.HasActiveTransfer(t.dir + t.filename, s2.Name) then
    begin
      {$IFDEF QUEUE_DIAGNOSTICS}FDiagnostics.Count(qcActiveFile); DiagnosticBlockTask(t, qbActiveFile);{$ENDIF}
      exit;
    end; // we are already sending this file the opposite route

    // or use 'if t.ps1.StatusRealPreOrShouldPre then' from pazo.pas but will also pre true when status = rssShouldPre
    //if t.ps1.status = rssRealPre then
    if t.ps1.StatusRealPreOrShouldPre then
    begin
      if s1.num_dn >= s1.max_pre_dn then
      begin
        {$IFDEF QUEUE_DIAGNOSTICS}FDiagnostics.Count(qcSourceLimit); DiagnosticBlockTask(t, qbSourceLimit, qbNone, 0, 0, s1.num_dn, s1.max_pre_dn);{$ENDIF}
        exit;
      end;
    end
    else
    begin
      if s1.num_dn >= s1.max_dn then
      begin
        {$IFDEF QUEUE_DIAGNOSTICS}FDiagnostics.Count(qcSourceLimit); DiagnosticBlockTask(t, qbSourceLimit, qbNone, 0, 0, s1.num_dn, s1.max_dn);{$ENDIF}
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
      {$IFDEF QUEUE_DIAGNOSTICS}DiagnosticBlockTask(t, qbSourceOffline);{$ENDIF}
      exit;
    end;


    if not s2.AcquireSlotsAssignmentLock(1, 'TryToAssignRaceSlots') then
    begin
      {$IFDEF QUEUE_DIAGNOSTICS}FDiagnostics.Count(qcDestinationLock); DiagnosticBlockTask(t, qbDestinationLock);{$ENDIF}
      RememberBlockedDestination(s2, fDestinationRevision, IncMilliSecond(Now, 25){$IFDEF QUEUE_DIAGNOSTICS}, qbDestinationLock{$ENDIF});
      exit;
    end;

    try
      if t.CancelIfSiteDown then Exit;
      // check again now that we have the lock at the destination
      if s2.num_up >= s2.max_up then
      begin
        {$IFDEF QUEUE_DIAGNOSTICS}FDiagnostics.Count(qcDestinationLimit); DiagnosticBlockTask(t, qbDestinationLimit, qbNone, 0, 0, s2.num_up, s2.max_up);{$ENDIF}
        RememberBlockedDestination(s2, fDestinationRevision, IncMilliSecond(fNow, glQueueFallbackMs){$IFDEF QUEUE_DIAGNOSTICS}, qbDestinationLimit{$ENDIF});
        exit;
      end;

      // again check if this file is already being sent to the destination now that we have the slot assignment lock
      if t.ps2.HasActiveTransfer(t.dir + t.filename) then
      begin
        {$IFDEF QUEUE_DIAGNOSTICS}DiagnosticBlockTask(t, qbActiveFile);{$ENDIF}
        exit;
      end; // we are already sending this file to the same destination site

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
        {$IFDEF QUEUE_DIAGNOSTICS}DiagnosticBlockTask(t, qbDestinationOffline);{$ENDIF}
        RememberBlockedDestination(s2, fDestinationRevision, IncMilliSecond(fNow, glQueueFallbackMs){$IFDEF QUEUE_DIAGNOSTICS}, qbDestinationOffline{$ENDIF});
        exit;
      end;

      // now you can relax, just check if you don't abuse your max simultaneous uploads for a rip
      i := ss2.site.MaxUpPerRip;
      if ((i > 0) and (t.ps2.ActiveTransferCount >= i)) then
      begin
        {$IFDEF QUEUE_DIAGNOSTICS}FDiagnostics.Count(qcDestinationLimit); DiagnosticBlockTask(t, qbDestinationLimit, qbNone, 0, 0, t.ps2.ActiveTransferCount, i);{$ENDIF}
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
      {$IFDEF QUEUE_DIAGNOSTICS}
      FDiagnostics.Count(qcAssignedRace);
      if ss1.QueueDiagnosticFreedAt > 0 then
        FDiagnostics.Measure(qtSourceRefill, QueueDiagnosticNow - ss1.QueueDiagnosticFreedAt);
      if ss2.QueueDiagnosticFreedAt > 0 then
        FDiagnostics.Measure(qtDestinationRefill, QueueDiagnosticNow - ss2.QueueDiagnosticFreedAt);
        {$ENDIF}
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
      RecordAssignmentFailure(t);
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
      RecordAssignmentFailure(t);
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
  {$IFDEF QUEUE_DIAGNOSTICS}DiagnosticBeginTask(t); try{$ENDIF}
   // Debug(dpSpam, section, 'TryToAssignSlots profile '+t.Fullname);
  if t.CancelIfSiteDown then Exit;

  try
    s := TSite(self.fSite);
    s.AcquireSlotsAssignmentLock('TryToAssignSlots');
    try
    if t.CancelIfSiteDown then Exit;
    if s.freeslots = 0 then
    begin
      {$IFDEF QUEUE_DIAGNOSTICS}FDiagnostics.Count(qcSourceFull); DiagnosticBlockTask(t, qbSourceFull);{$ENDIF}
      exit;
    end;

    if t.wanted_up and s.MaxSimUpCooldownActive then
    begin
      {$IFDEF QUEUE_DIAGNOSTICS}FDiagnostics.Count(qcCooldown); DiagnosticBlockTask(t, qbCooldown);{$ENDIF}
      Debug(dpSpam, section, '[MAXSIM COOLDOWN] Site %s is on MaxSim UP cooldown (%ds remaining), skip task %s',
        [s.Name, s.MaxSimUpCooldownRemainingSeconds, t.FullName]);
      exit;
    end;

    if t.wanted_dn and s.MaxSimDownCooldownActive then
    begin
      {$IFDEF QUEUE_DIAGNOSTICS}FDiagnostics.Count(qcCooldown); DiagnosticBlockTask(t, qbCooldown);{$ENDIF}
      Debug(dpSpam, section, '[MAXSIM COOLDOWN] Site %s is on MaxSim DOWN cooldown (%ds remaining), skip task %s',
        [s.Name, s.MaxSimDownCooldownRemainingSeconds, t.FullName]);
      exit;
    end;

      {$IFDEF QUEUE_DIAGNOSTICS}FDiagnostics.Count(qcAttempt);{$ENDIF}

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
        if (actual_count >= Max(s.slots.Count div 2, 1)) then
        begin
          {$IFDEF QUEUE_DIAGNOSTICS}DiagnosticBlockTask(t, qbDirlistLimit, qbNone, 0, 0, actual_count, Max(s.slots.Count div 2, 1));{$ENDIF}
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
        begin
          {$IFDEF QUEUE_DIAGNOSTICS}DiagnosticBlockTask(t, qbWantedSlot);{$ENDIF}
          exit;
        end;
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
        begin
          {$IFDEF QUEUE_DIAGNOSTICS}DiagnosticBlockTask(t, qbSourceOffline);{$ENDIF}
          exit;
        end;
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
          begin
            {$IFDEF QUEUE_DIAGNOSTICS}DiagnosticBlockTask(t, qbSourceLimit, qbNone, 0, 0, s.num_dn, ss.site.max_dn);{$ENDIF}
            exit;
          end;


          ss.downloadingfrom := True;

        end
        else
        if t.wanted_up then
        begin
          if s.num_up >= ss.site.max_up then
          begin
            {$IFDEF QUEUE_DIAGNOSTICS}DiagnosticBlockTask(t, qbSourceLimit, qbNone, 0, 0, s.num_up, ss.site.max_up);{$ENDIF}
            exit;
          end;
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
      RecordAssignmentFailure(t);
    end;
  end;
  {$IFDEF QUEUE_DIAGNOSTICS}
  finally
    if t.slot1 <> nil then DiagnosticFinishTask(t, 'assigned')
    else if t.startat > Now then DiagnosticBlockTask(t, qbDelayed)
    else if not t.QueueBlockState^.Classified then DiagnosticBlockTask(t, qbUnknown);
  end;
  {$ENDIF}
end;

procedure AddQuitTask(s: TSiteSlot);
var
  q: TQuitTask;
begin
  try
    q := TQuitTask.Create('', '', s.site.Name);
    q.slot1 := s;
    q.slot1name := s.Name;
    q.assigned := Now();
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
    ti.assigned := Now();
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

function TQueueThread.TaskAlreadyInQueue(t: TTask): boolean;
var
  fTask:    TTask;
  tpr, i_tpr: TPazoRaceTask;
  tpd, i_tpd: TPazoDirlistTask;
  tpm, i_tpm: TPazoMkdirTask;
  tpl, i_tpl: TLoginTask;
  fListIndex: Integer;
  fList: TObjectList;
begin
  Result := False;

  if (t is TPazoRaceTask) then
  begin
    try
      tpr := TPazoRaceTask(t);
      main_lock.Enter('TaskAlreadyInQueue1');
      try
        for fListIndex := 0 to 1 do
        begin
          if fListIndex = 0 then fList := tasks else fList := waiting_tasks;
          for fTask in fList do
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
        for fListIndex := 0 to 1 do
        begin
          if fListIndex = 0 then fList := tasks else fList := waiting_tasks;
          for fTask in fList do
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
        for fListIndex := 0 to 1 do
        begin
          if fListIndex = 0 then fList := tasks else fList := waiting_tasks;
          for fTask in fList do
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
        for fListIndex := 0 to 1 do
        begin
          if fListIndex = 0 then fList := tasks else fList := waiting_tasks;
          for fTask in fList do
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
      { The caller holds the queue lock while taking this console snapshot. }
      Debug(dpSpam, section, Format('[EXCEPTION] AddTaskToConsole task not available : %s', [e.Message]));
      exit;
    end;
  end;
  Console_QueueAdd(fTaskUid, Format('%s', [fTaskName]));
end;

procedure TQueueThread.AddTask(t: TTask; const aQueueFire: Boolean = True);
var
  fCheckSiteSlotsSite: TSite;
  fWake: Boolean;
  fNow: TDateTime;
  {$IFDEF QUEUE_DIAGNOSTICS}
  fDiagnosticAt: Int64;
  fDiagnosticWake: TQueueWakeReason;
  {$ENDIF}
begin
  fCheckSiteSlotsSite := nil;
  fWake := False;
  {$IFDEF QUEUE_DIAGNOSTICS}fDiagnosticAt := QueueDiagnosticNow;{$ENDIF}
  main_lock.Enter('AddTask');
  try
    {$IFDEF QUEUE_DIAGNOSTICS}FDiagnostics.Measure(qtQueueLock, QueueDiagnosticNow - fDiagnosticAt);{$ENDIF}
    if TaskAlreadyInQueue(t) then
    begin
      if t.IsNotifyTask then
        TaskReady(t);
      t.Free;
      Exit;
    end;

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

    {$IFDEF QUEUE_DIAGNOSTICS}FDiagnostics.Count(qcInserted); fDiagnosticWake := qwTask;{$ENDIF}
    fNow := Now;
    {$IFDEF QUEUE_DIAGNOSTICS}
    if (t.slot1 = nil) and (t.slot2 = nil) and not (t.ready or t.readyerror) then
      if t.startat > fNow then DiagnosticBlockTask(t, qbDelayed)
      else DiagnosticBlockTask(t, qbNotChecked);
    {$ENDIF}
    if t.startat > fNow then
    begin
      waiting_tasks.Add(t);
      {$IFDEF QUEUE_DIAGNOSTICS}
      fDiagnosticWake := qwEarlierDeadline;
      if t is TPazoRaceTask then
      begin
        FDiagnostics.Count(qcDelayedRace);
        FDiagnostics.Count(qcInitialDelayMs, MilliSecondsBetween(t.startat, fNow));
      end;
      {$ENDIF}
      { A newly earlier deadline must interrupt an existing longer wait, even in a batch. }
      fWake := t.startat < FNextTaskAt;
      _ConsiderQueueDeadline(fNow, t.startat, FNextTaskAt);
    end
    else
    begin
      tasks.Add(t);
      FSortPending := True;
      fWake := aQueueFire;
    end;

    if t is TPazoRaceTask then
    begin
      if (TPazoRaceTask(t).ps2 <> nil) and (TPazoRaceTask(t).ps2.error or
        ((TPazoRaceTask(t).dir <> '') and
         TPazoRaceTask(t).ps2.dirlist.FindDirList(TPazoRaceTask(t).dir).error)) then
        t.readyerror := True;
      if (fSite <> nil) and (t.startat <= fNow) and (not t.ready) and
        (not t.readyerror) and t.IsReadyToBeExecuted and (TSite(fSite).freeslots > 0) then
        TryToAssignSlots(t);
    end;
    { Directly assigned tasks already have a worker; completion will signal cleanup. }
    if (t.slot1 <> nil) and not (t.ready or t.readyerror) then
      fWake := False;
    {$IFDEF QUEUE_DIAGNOSTICS}
    if t.slot1 <> nil then
      FDiagnostics.Count(qcInsertedAssigned);
    if t is TWaitTask then
      FDiagnostics.Count(qcInsertedWait);
    fDiagnosticAt := QueueDiagnosticNow;
    {$ENDIF}
    AddTaskToConsole(t);
    {$IFDEF QUEUE_DIAGNOSTICS}FDiagnostics.Measure(qtConsole, QueueDiagnosticNow - fDiagnosticAt);{$ENDIF}
  finally
    main_lock.Leave;
  end;
  { No task reads after unlocking: a worker may have completed and freed it. }
  if fWake then
    QueueFire({$IFDEF QUEUE_DIAGNOSTICS}fDiagnosticWake{$ENDIF});
  if fCheckSiteSlotsSite <> nil then
    CheckSiteSlots(fCheckSiteSlotsSite);
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
              ttp.ready := True;
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
              end
              else if aForce then
              begin
                Debug(dpMessage, section, Format('RemovePazo: Force removal of assigned task: %s', [t.Name]));
                t.readyerror := True;

                // if the site slot actually has this task assigned, we need to rebuild it
                if TSiteSlot(t.slot1).todotask = t then
                begin
                  fSlotsToRebuild.Add(TSiteSlot(t.slot1));
                  // Do NOT clear slot1/slot2 here: the slot thread is still executing
                  // fCurrentTask (= t) inside TSiteSlot.Execute. Clearing slot1 now would
                  // allow RemoveReady to free the task while the slot thread is still using
                  // it -> use-after-free -> AV. The slot thread's cleanup sets
                  // fCurrentTask.slot1 := nil after Execute() returns. RebuildSlot signals
                  // shouldquit=True so the FTP operation aborts quickly.
                end
                else
                begin
                  // Slot has already moved on (todotask != t); safe to clear immediately.
                  t.slot1 := nil;
                  t.slot2 := nil;
                end;
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

{$IFDEF QUEUE_DIAGNOSTICS}
procedure TQueueThread.DiagnosticBlockTask(aTask: TTask; const aReason: TQueueBlockReason;
  const aCacheReason: TQueueBlockReason; const aRetryMs, aRevision: Int64;
  const aUsed, aLimit: Integer);
begin
  aTask.QueueBlockState^.Observe(aReason, QueueDiagnosticNow, aCacheReason,
    aRetryMs, aRevision, aUsed, aLimit);
end;

procedure TQueueThread.DiagnosticBeginTask(aTask: TTask);
var
  fState: PQueueBlockState;
  fAt, fEventAt: Int64;
begin
  fState := aTask.QueueBlockState;
  fAt := QueueDiagnosticNow;
  fState^.BeginCheck(fAt);
  if (fState^.Reason = qbMkdir) and (aTask is TPazoTask) and
    (TPazoTask(aTask).FDependingOnDirlist <> nil) then
  begin
    fEventAt := TPazoTask(aTask).FDependingOnDirlist.DiagnosticReadyAt;
    fState^.RecordEvent(fEventAt, fAt, 'dependency_ready');
  end
  else if (fState^.Reason in [qbDestinationFull, qbDestinationOffline,
    qbDestinationLimit, qbDestinationLock, qbCachedDestination]) and (aTask.ssite2 <> nil) then
    fState^.RecordEvent(TSite(aTask.ssite2).QueueDiagnosticChangedAt, fAt, 'destination_notification')
  else if (fState^.Reason in [qbSourceFull, qbSourceOffline, qbSourceLimit]) and
    (aTask.ssite1 <> nil) then
    fState^.RecordEvent(TSite(aTask.ssite1).QueueDiagnosticChangedAt, fAt, 'source_notification');
end;

procedure TQueueThread.DiagnosticSampleTask(aTask: TTask; const aNowUs: Int64;
  const aOutcome: String);
var
  fSourceFree, fTargetFree: Integer;
  fPlannedMs: Int64;
  fNow: TDateTime;
begin
  if aTask.QueueBlockState^.BlockedUs(aNowUs) < 2000000 then Exit;
  fSourceFree := -1;
  fTargetFree := -1;
  if aTask.ssite1 <> nil then fSourceFree := TSite(aTask.ssite1).freeslots;
  if aTask.ssite2 <> nil then fTargetFree := TSite(aTask.ssite2).freeslots;
  fNow := Now;
  fPlannedMs := 0;
  if aTask.startat > fNow then fPlannedMs := MilliSecondsBetween(aTask.startat, fNow);
  FDiagnostics.SampleBlock(aTask.uid, aTask.ClassName, aTask.site1, aTask.site2,
    aTask.QueueBlockState^, aNowUs, MilliSecondsBetween(fNow, aTask.created),
    fPlannedMs, fSourceFree, fTargetFree, aOutcome);
end;

procedure TQueueThread.DiagnosticFinishTask(aTask: TTask; const aOutcome: String);
var
  fNowUs: Int64;
begin
  if aTask.QueueBlockState^.FinishedAt <> 0 then Exit;
  fNowUs := QueueDiagnosticNow;
  aTask.QueueBlockState^.Finish(fNowUs);
  DiagnosticSampleTask(aTask, fNowUs, aOutcome);
end;

procedure TQueueThread.ReportDiagnostics;
var
  fTask: TTask;
  fDue, fFreeSlots: Integer;
  fOldestDueMs, fAgeMs, fNowUs: Int64;
  fNow: TDateTime;
  fReport: String;
  fList: TObjectList;
  fListIndex, fLineEnd: Integer;
begin
  fNowUs := QueueDiagnosticNow;
  if not FDiagnostics.ReportDue(fNowUs) then
    Exit;
  main_lock.Enter('Queue diagnostic snapshot');
  try
    if not FDiagnostics.ReportDue(fNowUs) then
      Exit;
    fNowUs := QueueDiagnosticNow;
    fDue := 0;
    fOldestDueMs := 0;
    fNow := Now;
    for fListIndex := 0 to 1 do
    begin
    if fListIndex = 0 then fList := tasks else fList := waiting_tasks;
    for fTask in fList do
      if (fTask.slot1 = nil) and (fTask.slot2 = nil) and
        not (fTask.ready or fTask.readyerror) then
      begin
        if fTask.startat > fNow then
        begin
          FDiagnostics.CountBlocked(qbDelayed);
          DiagnosticSampleTask(fTask, fNowUs, 'pending');
          Continue;
        end;
        Inc(fDue);
        fAgeMs := MilliSecondsBetween(fNow, fTask.created);
        if fAgeMs > fOldestDueMs then
          fOldestDueMs := fAgeMs;
        if (fSite <> nil) and (TSite(fSite).freeslots = 0) and
          (fTask.QueueBlockState^.Reason in [qbNone, qbNotChecked, qbSourceFull]) then
          fTask.QueueBlockState^.Observe(qbSourceFull, fNowUs);
        FDiagnostics.CountBlocked(fTask.QueueBlockState^.Reason);
        DiagnosticSampleTask(fTask, fNowUs, 'pending');
      end;
    end;
    fFreeSlots := 0;
    if fSite <> nil then
      fFreeSlots := TSite(fSite).freeslots;
    fReport := FDiagnostics.Snapshot(fSiteName, fNowUs, tasks.Count,
      waiting_tasks.Count, fFreeSlots, fDue, fOldestDueMs);
  finally
    main_lock.Leave;
  end;
  while fReport <> '' do
  begin
    fLineEnd := Pos(#10, fReport);
    if fLineEnd = 0 then fLineEnd := Length(fReport) + 1;
    Debug(dpMessage, 'queue-diag', Copy(fReport, 1, fLineEnd - 1));
    Delete(fReport, 1, fLineEnd);
  end;
end;
{$ENDIF}

procedure TQueueThread.Execute;
var
  i: integer;
  fTask:    TTask;
  s:    TSiteSlot;
  ss:   String;
  ts:   TSite;
  fNextTaskStartAt: TDateTime;

  fListIndex: Integer;
  fList: TObjectList;
  {$IFDEF QUEUE_DIAGNOSTICS}
  fDiagnosticPassAt, fDiagnosticLockAt, fDiagnosticCleanupAt: Int64;
  {$ENDIF}
begin
  while ((not slshutdown) and (not Terminated)) do
  begin
    {$IFDEF QUEUE_DIAGNOSTICS}fDiagnosticPassAt := QueueDiagnosticNow;{$ENDIF}
    queue_last_run := Now();

    if fSite = nil then
      fSite := FindSiteByName('', fSiteName);

    if fSite = nil then
    begin
      //happens on startup
      Debug(dpSpam, section, 'Queue Iteration: Wait for site]');
      WaitForWake(1000);
      continue;
    end;

    ss := '';
    ts := TSite(fSite);
    fNextTaskStartAt := MaxDateTime;
    //Debug(dpSpam, section, 'Queue Iteration begin (%s) [%d tasks]', [ts.Name, tasks.Count]);
    try
      {$IFDEF QUEUE_DIAGNOSTICS}fDiagnosticLockAt := QueueDiagnosticNow;{$ENDIF}
      main_lock.Enter('Execute');
      try
        {$IFDEF QUEUE_DIAGNOSTICS}FDiagnostics.Count(qcPass); FDiagnostics.Measure(qtQueueLock, QueueDiagnosticNow - fDiagnosticLockAt);{$ENDIF}
        FAssignmentRetryAt := MaxDateTime;
        fNextTaskStartAt := MoveWaitingTasks(queue_last_run);

        if ts.freeslots > 0 then
          ApplyPendingSort
        else
          { A slot completion will signal us; task deadlines cannot dispatch work now. }
          fNextTaskStartAt := MaxDateTime;
        {$IFDEF QUEUE_DIAGNOSTICS}fDiagnosticCleanupAt := QueueDiagnosticNow;{$ENDIF}

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
            fTask.CancelIfSiteDown;
            if (((fTask.ready) or (fTask.readyerror)) and (fTask.slot1 = nil)) then
            begin
              {$IFDEF QUEUE_DIAGNOSTICS}
              if (fTask.ClassType = TPazoRaceTask) and TPazoRaceTask(fTask).QueueDiagnosticTransferred then
              begin
                FDiagnostics.Count(qcCompletedRace);
                FDiagnostics.Count(qcCompletedBytes, TPazoRaceTask(fTask).filesize);
              end;
              {$ENDIF}
              ss := fTask.uidtext;
              if fTask.IsNotifyTask then
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
                {$IFDEF QUEUE_DIAGNOSTICS}DiagnosticFinishTask(fTask, 'removed');{$ENDIF}
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

        {$IFDEF QUEUE_DIAGNOSTICS}FDiagnostics.Measure(qtCleanup, QueueDiagnosticNow - fDiagnosticCleanupAt);{$ENDIF}
        ts.AcquireSlotsAssignmentLock('Queue iterate');
        try
          for fTask in tasks do
          begin
            try
              if fTask.CancelIfSiteDown then Continue;
              if ts.freeslots = 0 then
              begin
                //Debug(dpSpam, section, Format('No free slots on %s', [ts.Name]));

                // no need to iterate the queue early if there are no free slots.
                // when a slot becomes free, a queue fire is issued.
                break;
              end;

              if ((fTask.slot1 = nil) and (fTask.slot2 = nil) and (not fTask.ready) and
                (not fTask.readyerror)) then
              begin
                if ((fTask.startat = 0) or (fTask.startat <= queue_last_run)) then
                begin
                  {$IFDEF QUEUE_DIAGNOSTICS}DiagnosticBeginTask(fTask);{$ENDIF}
                  if fTask.IsReadyToBeExecuted then
                  begin
                    TryToAssignSlots(fTask);
                    { Failed assignment can set a later retry time. }
                    _ConsiderQueueDeadline(queue_last_run, fTask.startat, fNextTaskStartAt);
                    if fTask.slot1 = nil then
                    begin
                      if fTask.wanted_up then
                        _ConsiderQueueDeadline(queue_last_run, ts.MaxSimUpCooldownUntil, fNextTaskStartAt);
                      if fTask.wanted_dn then
                        _ConsiderQueueDeadline(queue_last_run, ts.MaxSimDownCooldownUntil, fNextTaskStartAt);
                      if fTask is TPazoRaceTask then
                      begin
                        _ConsiderQueueDeadline(queue_last_run, ts.MaxSimDownCooldownUntil, fNextTaskStartAt);
                        _ConsiderQueueDeadline(queue_last_run, TSite(fTask.ssite2).MaxSimUpCooldownUntil, fNextTaskStartAt);
                      end;
                      if fTask.ready or fTask.readyerror then
                        QueueFire({$IFDEF QUEUE_DIAGNOSTICS}qwCleanup{$ENDIF});
                    end;
                  end
                  else
                  begin
                    {$IFDEF QUEUE_DIAGNOSTICS}
                    FDiagnostics.Count(qcDependency);
                    if fTask is TPazoTask then DiagnosticBlockTask(fTask, qbMkdir)
                    else DiagnosticBlockTask(fTask, qbDependency);
                    {$ENDIF}
                    if (fTask is TPazoRaceTask) and (fTask.ssite2 <> nil) then
                    begin
                      TSite(fTask.ssite2).RegisterQueueWaiter(fSiteName);
                      {$IFDEF QUEUE_DIAGNOSTICS}
                      if fTask.IsReadyToBeExecuted then
                        FDiagnostics.Count(qcDependencyChanged);
                      {$ENDIF}
                    end;
                  end;
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
        _ConsiderQueueDeadline(queue_last_run, FAssignmentRetryAt, fNextTaskStartAt);
        FNextTaskAt := fNextTaskStartAt;
      finally
        main_lock.Leave;
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
              if s.todotask = nil then
              begin
                if (s.status = ssOnline) and (ts.maxidle > 0) then
                  _ConsiderQueueDeadline(queue_last_run, IncSecond(s.LastNonIdleTaskExecution, ts.maxidle), fNextTaskStartAt);
                if (s.status = ssOnline) or ((ts.WorkingStatus = sstUp) and
                  ((ts.maxidle = 0) or (queue_last_run < IncSecond(s.LastNonIdleTaskExecution, ts.maxidle)))) then
                  _ConsiderQueueDeadline(queue_last_run, IncMilliSecond(s.LastIO, Int64(ts.idleinterval) * 1000 + 1), fNextTaskStartAt);
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

    main_lock.Enter('Queue next deadline');
    try
      if fNextTaskStartAt < FNextTaskAt then
        FNextTaskAt := fNextTaskStartAt;
      {$IFDEF QUEUE_DIAGNOSTICS}FDiagnostics.Measure(qtPass, QueueDiagnosticNow - fDiagnosticPassAt);{$ENDIF}
    finally
      main_lock.Leave;
    end;
    {$IFDEF QUEUE_DIAGNOSTICS}
    ReportDiagnostics;
    {$ENDIF}
    { Auto-reset signals persist across processing/wait transitions and coalesce bursts. }
    WaitForWake;

  end;
end;

procedure QueueInit;
var
  fFallbackMs: Integer;
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
  fFallbackMs := config.ReadInteger(section, 'queue_fallback_ms', 15000);
  if (fFallbackMs < 10000) or (fFallbackMs > 15000) then
    fFallbackMs := 15000;
  glQueueFallbackMs := Cardinal(fFallbackMs);

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
            Debug(dpError, section, Format('QueueClean: Remove Unassigned : %s', [t.Fullname]));
          except
            on e: Exception do
            begin
              Debug(dpError, section,
                Format('[EXCEPTION] QueueClean: Exception Remove Unassigned : %s', [e.Message]));
              Break;
            end;
          end;

          // Tasks created by AddIdleTask/AddQuitTask have slot1 set directly
          // but assigned stays 0. The removal loop requires slot1=nil to remove a
          // ready task, so without this cleanup the task is stuck in the queue forever
          // and s.todotask keeps pointing to the dead task, blocking new idle tasks.
          if t.slot1 <> nil then
          begin
            ts.AcquireSlotsAssignmentLock('QueueClean1 unassigned');
            try
              if TSiteSlot(t.slot1).todotask = t then
              begin
                // Slot still holds a reference to this task — it may be actively
                // executing it. Clear todotask to unblock slot assignment for new
                // tasks, but do NOT clear slot1: the slot thread will clear it in
                // its own post-execute cleanup after Execute() returns. Clearing
                // slot1 here makes the task eligible for removal (ready+slot1=nil)
                // while the slot thread still holds fCurrentTask pointing to it,
                // causing a use-after-free crash in the removal loop.
                TSiteSlot(t.slot1).todotask := nil;
              end
              else
              begin
                // Slot has moved on (todotask != this task). Safe to clear slot1
                // because the slot thread no longer accesses this task via fCurrentTask.
                t.slot1 := nil;
                t.slot1name := '';
              end;
            finally
              ts.ReleaseSlotsAssignmentLock;
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

              // Signal the destination WAITTASK to unblock its slot thread,
              // same as RemoveReady does when collecting a completed RACE task.
              if (TPazoRaceTask(t).dst <> nil) then
              begin
                try
                  TPazoRaceTask(t).dst.event.SetEvent;
                except
                  on e: Exception do
                    Debug(dpError, section, Format('[EXCEPTION] QueueClean race: signal dst event : %s', [e.Message]));
                end;
              end;

              {$IFDEF QUEUE_DIAGNOSTICS}DiagnosticFinishTask(t, 'removed');{$ENDIF}
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
          ss := t.UidText;
          Debug(dpSpam, section, Format('[QUEUECLEAN] Clean wait task : %s', [t.Fullname]));

          // Wake up the blocking slot thread so it can complete Execute normally.
          // Do NOT call tasks.Remove here: TWaitTask.Execute blocks in event.WaitFor,
          // so calling tasks.Remove would free the task object while the slot thread
          // is still executing it (use-after-free / AV).
          // The slot thread sets ready := True when Execute returns, clears slot1 in
          // its cleanup block, and RemoveReady will collect the task on the next pass.
          TWaitTask(t).event.SetEvent;

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
              {$IFDEF QUEUE_DIAGNOSTICS}DiagnosticFinishTask(t, 'removed');{$ENDIF}
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
    QueueStat;

  //Debug(dpMessage, section, 'QueueClean end %d', [tasks.Count]);
end;

procedure TQueueThread.QueueStat;
var
  t_race, t_dir, t_auto, t_other: integer;
  fTask: TTask;
  fListIndex: Integer;
  fList: TObjectList;
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
    for fListIndex := 0 to 1 do
    begin
      if fListIndex = 0 then fList := tasks else fList := waiting_tasks;
      for fTask in fList do
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

  QueueStatUpdateDateTime := Now;
  Console_QueueStat(t_race + t_dir + t_auto + t_other, t_race, t_dir, t_auto, t_other);
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
          ts.AcquireSlotsAssignmentLock('killall');
            try
              {$IFDEF QUEUE_DIAGNOSTICS}DiagnosticFinishTask(fTask, 'removed');{$ENDIF}
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
