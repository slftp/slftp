unit queueEventTests;

interface

uses
  {$IFDEF FPC}
    TestFramework,
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility,
  {$ENDIF}
  Classes, SysUtils, pazo;

type
  TTestQueueEvent = class(TTestCase)
  protected
    fPazo: TPazo;
    {$IFDEF FPC}
    procedure SetUpOnce; override;
    procedure TearDownOnce; override;
    {$ELSE}
    procedure SetUp; override;
    procedure TearDown; override;
    {$ENDIF}
  published
    { _CalcMaxDirlistSlots tests }
    procedure TestCalcMaxDirlistSlots_EmptyConfig;
    procedure TestCalcMaxDirlistSlots_AbsoluteValues;
    procedure TestCalcMaxDirlistSlots_PercentValues;
    procedure TestCalcMaxDirlistSlots_InvalidValues;

    { fActiveDirlistCount tests }
    procedure TestActiveDirlistCount_IncrementOnAssign;
    procedure TestActiveDirlistCount_DecrementOnClear;
    procedure TestActiveDirlistCount_RaceTaskNoChange;
    procedure TestActiveDirlistCount_SwapDirlistToRace;
    procedure TestActiveDirlistCount_SwapRaceToDirlist;

    { RecalcFreeslots tests }
    procedure TestRecalcFreeslots_Accuracy;

    { Queue pending race task tests - TODO: re-enable after stabilizing thread teardown }
    // procedure TestGetPendingRaceTasksToDestination;

    { _IsLowPriorityRaceTask tests }
    procedure TestIsLowPriorityRaceTask_SamplePriority3;
    procedure TestIsLowPriorityRaceTask_SamplePriority1;
    procedure TestIsLowPriorityRaceTask_ProofPriority3;
    procedure TestIsLowPriorityRaceTask_NormalRace;
    procedure TestIsLowPriorityRaceTask_Nil;

    { _HasWaitingNonLowPriorityTasks tests }
    procedure TestHasWaitingNonLowPriorityTasks_Empty;
    procedure TestHasWaitingNonLowPriorityTasks_OnlyLowPriority;
    procedure TestHasWaitingNonLowPriorityTasks_MkdirWaiting;
    procedure TestHasWaitingNonLowPriorityTasks_RaceSfvWaiting;
    procedure TestHasWaitingNonLowPriorityTasks_AllAssigned;

    { Diagnostic test }
    procedure TestQueueEventDiagnostic;

    { _ScoreTask tests }
    procedure TestScoreTask_WaitTask;
    procedure TestScoreTask_MkdirMaindir;
    procedure TestScoreTask_MkdirSubdir;
    procedure TestScoreTask_RaceSfv;
    procedure TestScoreTask_RaceNfo;
    procedure TestScoreTask_RaceNormal;
    procedure TestScoreTask_Dirlist;
    procedure TestScoreTask_LoginTask;

    { FindBestTask tests }
    procedure TestFindBestTask_EmptyQueue;
    procedure TestFindBestTask_SingleTask;
    procedure TestFindBestTask_PriorityOrdering;
    procedure TestFindBestTask_DelayedTaskSkipped;
    procedure TestFindBestTask_AssignedTaskSkipped;

  end;

implementation

uses
  Contnrs,
  queueunit, sitesunit, tasksunit, taskrace, tasklogin, encinifile;

{ TTestQueueEvent }

{$IFDEF FPC}
procedure TTestQueueEvent.SetUpOnce;
{$ELSE}
procedure TTestQueueEvent.SetUp;
{$ENDIF}
var
  fAdminSite: TSite;
  fSrcSite: TSite;
  fDstSite: TSite;
  fSitesDatFile: String;
begin
  inherited;

  { Init tasks unit }
  Tasks_Init;

  { Init sites unit }
  SitesInit;

  { Create a temporary sites.dat for tests so RC*/WC* methods work }
  fSitesDatFile := ExtractFilePath(ParamStr(0)) + 'test_sites.dat';
  sitesdat := TEncIniFile.Create(fSitesDatFile, 'testpass', True);
  sitesdat.WriteString('sites', 'default', 'exists');
  sitesdat.autoupdate := True;

  { Init queue unit }
  QueueInit;

  { Create admin site (required by getAdminSiteName) }
  fAdminSite := TSite.Create(getAdminSiteName);
  AddSite(fAdminSite);

  { Create test sites for queue tests }
  fSrcSite := TSite.Create('SRC');
  AddSite(fSrcSite);

  fDstSite := TSite.Create('DST');
  AddSite(fDstSite);

  { Create a pazo with sites so TPazoDirlistTask/TPazoRaceTask can be created }
  fPazo := TPazo.Create(nil, 1);
  fPazo.AddSite('SRC', '/test');
  fPazo.AddSite('DST', '/test');
end;

{$IFDEF FPC}
procedure TTestQueueEvent.TearDownOnce;
{$ELSE}
procedure TTestQueueEvent.TearDown;
{$ENDIF}
var
  fSitesDatFile: String;
  fQueueThread: TQueueThread;
begin
  { Terminate all queue threads but do NOT free them here.
    TSite.Destroy will free each site's fQueue. We must set
    FreeOnTerminate := False before WaitFor to prevent the
    thread from self-freeing before SitesUninit runs. }
  if Queues <> nil then
  begin
    for fQueueThread in Queues do
    begin
      fQueueThread.FreeOnTerminate := False;
      fQueueThread.Terminate;
      fQueueThread.QueueFire;
    end;
    for fQueueThread in Queues do
      fQueueThread.WaitFor;
  end;

  QueueUninit;
  SitesUninit;

  { Free temp sites.dat }
  fSitesDatFile := ExtractFilePath(ParamStr(0)) + 'test_sites.dat';
  if FileExists(fSitesDatFile) then
    DeleteFile(fSitesDatFile);

  { Note: fPazo is intentionally leaked here to avoid complex teardown
    ordering issues with queue threads and task references in tests. }
  // fPazo.Free;

  Tasks_Uninit;

  inherited;
end;

{ --------------------------------------------------------------------------- }
{ _CalcMaxDirlistSlots tests                                                  }
{ --------------------------------------------------------------------------- }

procedure TTestQueueEvent.TestCalcMaxDirlistSlots_EmptyConfig;
begin
  glMaxDirlistSlots := '';
  CheckEquals(2, _CalcMaxDirlistSlots(4), 'Empty config should fallback to slots div 2');
  CheckEquals(5, _CalcMaxDirlistSlots(10), 'Empty config should fallback to slots div 2');
  CheckEquals(0, _CalcMaxDirlistSlots(0), 'Empty config with 0 slots should return 0');
  CheckEquals(1, _CalcMaxDirlistSlots(3), 'Empty config with 3 slots should return 1 (3 div 2)');
end;

procedure TTestQueueEvent.TestCalcMaxDirlistSlots_AbsoluteValues;
begin
  glMaxDirlistSlots := '1';
  CheckEquals(1, _CalcMaxDirlistSlots(4), 'Absolute value 1 with 4 slots');
  CheckEquals(1, _CalcMaxDirlistSlots(10), 'Absolute value 1 with 10 slots');

  glMaxDirlistSlots := '3';
  CheckEquals(3, _CalcMaxDirlistSlots(10), 'Absolute value 3 with 10 slots');

  glMaxDirlistSlots := '0';
  CheckEquals(0, _CalcMaxDirlistSlots(10), 'Absolute value 0 should return 0');
end;

procedure TTestQueueEvent.TestCalcMaxDirlistSlots_PercentValues;
begin
  glMaxDirlistSlots := '50%';
  CheckEquals(2, _CalcMaxDirlistSlots(4), '50% of 4 slots');
  CheckEquals(5, _CalcMaxDirlistSlots(10), '50% of 10 slots');

  glMaxDirlistSlots := '25%';
  CheckEquals(1, _CalcMaxDirlistSlots(4), '25% of 4 slots (minimum 1)');
  CheckEquals(2, _CalcMaxDirlistSlots(10), '25% of 10 slots (Round(2.5)=2 with bankers rounding)');

  glMaxDirlistSlots := '100%';
  CheckEquals(4, _CalcMaxDirlistSlots(4), '100% of 4 slots');

  glMaxDirlistSlots := '0%';
  CheckEquals(1, _CalcMaxDirlistSlots(4), '0% should be clamped to minimum 1');
end;

procedure TTestQueueEvent.TestCalcMaxDirlistSlots_InvalidValues;
begin
  glMaxDirlistSlots := 'invalid';
  CheckEquals(2, _CalcMaxDirlistSlots(4), 'Invalid string should fallback to default (4 div 2)');

  glMaxDirlistSlots := '-1';
  CheckEquals(0, _CalcMaxDirlistSlots(4), 'Negative absolute should return 0');

  glMaxDirlistSlots := '200%';
  CheckEquals(4, _CalcMaxDirlistSlots(4), 'Percent > 100 should be clamped to 100%');
end;

{ --------------------------------------------------------------------------- }
{ fActiveDirlistCount tests                                                   }
{ --------------------------------------------------------------------------- }

procedure TTestQueueEvent.TestActiveDirlistCount_IncrementOnAssign;
var
  fSite: TSite;
  fSlot: TSiteSlot;
  fDirlistTask: TPazoDirlistTask;
  fInitialCount: integer;
begin
  fSite := FindSiteByName('', 'SRC');
  Check(fSite <> nil, 'SRC site should exist');
  fSlot := TSiteSlot(fSite.slots[0]);

  fInitialCount := fSite.fActiveDirlistCount;
  fDirlistTask := TPazoDirlistTask.Create('', '', 'SRC', fPazo, '/test', False);
  try
    fSlot.todotask := fDirlistTask;
    CheckEquals(fInitialCount + 1, fSite.fActiveDirlistCount, 'Assigning dirlist task should increment fActiveDirlistCount');
  finally
    fSlot.todotask := nil;
    fDirlistTask.Free;
  end;
end;

procedure TTestQueueEvent.TestActiveDirlistCount_DecrementOnClear;
var
  fSite: TSite;
  fSlot: TSiteSlot;
  fDirlistTask: TPazoDirlistTask;
  fInitialCount: integer;
begin
  fSite := FindSiteByName('', 'SRC');
  fSlot := TSiteSlot(fSite.slots[0]);

  fDirlistTask := TPazoDirlistTask.Create('', '', 'SRC', fPazo, '/test', False);
  try
    fSlot.todotask := fDirlistTask;
    fInitialCount := fSite.fActiveDirlistCount;
    fSlot.todotask := nil;
    CheckEquals(fInitialCount - 1, fSite.fActiveDirlistCount, 'Clearing dirlist task should decrement fActiveDirlistCount');
  finally
    fDirlistTask.Free;
  end;
end;

procedure TTestQueueEvent.TestActiveDirlistCount_RaceTaskNoChange;
var
  fSite: TSite;
  fSlot: TSiteSlot;
  fInitialCount: integer;
  fRaceTask: TPazoRaceTask;
begin
  fSite := FindSiteByName('', 'SRC');
  fSlot := TSiteSlot(fSite.slots[0]);

  fRaceTask := TPazoRaceTask.Create('', '', 'SRC', 'DST', fPazo, nil, '/test', 'file.mp3', 1024, 1);
  try
    fInitialCount := fSite.fActiveDirlistCount;
    fSlot.todotask := fRaceTask;
    CheckEquals(fInitialCount, fSite.fActiveDirlistCount, 'Assigning race task should not change fActiveDirlistCount');
    fSlot.todotask := nil;
    CheckEquals(fInitialCount, fSite.fActiveDirlistCount, 'Clearing race task should not change fActiveDirlistCount');
  finally
    fRaceTask.Free;
  end;
end;

procedure TTestQueueEvent.TestActiveDirlistCount_SwapDirlistToRace;
var
  fSite: TSite;
  fSlot: TSiteSlot;
  fDirlistTask: TPazoDirlistTask;
  fRaceTask: TPazoRaceTask;
  fInitialCount: integer;
begin
  fSite := FindSiteByName('', 'SRC');
  fSlot := TSiteSlot(fSite.slots[0]);

  fDirlistTask := TPazoDirlistTask.Create('', '', 'SRC', fPazo, '/test', False);
  fRaceTask := TPazoRaceTask.Create('', '', 'SRC', 'DST', fPazo, nil, '/test', 'file.mp3', 1024, 1);
  try
    fSlot.todotask := fDirlistTask;
    fInitialCount := fSite.fActiveDirlistCount;
    fSlot.todotask := fRaceTask;
    CheckEquals(fInitialCount - 1, fSite.fActiveDirlistCount, 'Swapping dirlist to race should decrement fActiveDirlistCount');
  finally
    fSlot.todotask := nil;
    fDirlistTask.Free;
    fRaceTask.Free;
  end;
end;

procedure TTestQueueEvent.TestActiveDirlistCount_SwapRaceToDirlist;
var
  fSite: TSite;
  fSlot: TSiteSlot;
  fDirlistTask: TPazoDirlistTask;
  fRaceTask: TPazoRaceTask;
  fInitialCount: integer;
begin
  fSite := FindSiteByName('', 'SRC');
  fSlot := TSiteSlot(fSite.slots[0]);

  fDirlistTask := TPazoDirlistTask.Create('', '', 'SRC', fPazo, '/test', False);
  fRaceTask := TPazoRaceTask.Create('', '', 'SRC', 'DST', fPazo, nil, '/test', 'file.mp3', 1024, 1);
  try
    fSlot.todotask := fRaceTask;
    fInitialCount := fSite.fActiveDirlistCount;
    fSlot.todotask := fDirlistTask;
    CheckEquals(fInitialCount + 1, fSite.fActiveDirlistCount, 'Swapping race to dirlist should increment fActiveDirlistCount');
  finally
    fSlot.todotask := nil;
    fDirlistTask.Free;
    fRaceTask.Free;
  end;
end;

{ --------------------------------------------------------------------------- }
{ RecalcFreeslots tests                                                       }
{ --------------------------------------------------------------------------- }

procedure TTestQueueEvent.TestRecalcFreeslots_Accuracy;
var
  fSite: TSite;
  fSlot1, fSlot2: TSiteSlot;
  fDirlistTask: TPazoDirlistTask;
  fRaceTask: TPazoRaceTask;
begin
  fSite := FindSiteByName('', 'SRC');
  { Ensure we have at least 2 slots for this test }
  if fSite.slots.Count < 2 then
  begin
    fSite.slots.Add(TSiteSlot.Create(fSite, fSite.slots.Count));
    fSite.RecalcFreeslots;
  end;

  fSlot1 := TSiteSlot(fSite.slots[0]);
  fSlot2 := TSiteSlot(fSite.slots[1]);

  { All slots free }
  fSlot1.todotask := nil;
  fSlot2.todotask := nil;
  fSite.RecalcFreeslots;
  CheckEquals(0, fSite.fActiveDirlistCount, 'Recalc with 2 free slots should find 0 dirlists');

  { One dirlist, one free }
  fDirlistTask := TPazoDirlistTask.Create('', '', 'SRC', fPazo, '/test', False);
  fRaceTask := TPazoRaceTask.Create('', '', 'SRC', 'DST', fPazo, nil, '/test', 'file.mp3', 1024, 1);
  try
    fSlot1.todotask := fDirlistTask;
    fSlot2.todotask := nil;
    fSite.RecalcFreeslots;
    CheckEquals(1, fSite.fActiveDirlistCount, 'Recalc should count 1 dirlist task');

    { One dirlist, one race }
    fSlot2.todotask := fRaceTask;
    fSite.RecalcFreeslots;
    CheckEquals(1, fSite.fActiveDirlistCount, 'Recalc should still count 1 dirlist with race task present');
  finally
    fSlot1.todotask := nil;
    fSlot2.todotask := nil;
    fDirlistTask.Free;
    fRaceTask.Free;
  end;
end;

{ --------------------------------------------------------------------------- }
{ Queue pending race task tests                                               }
{ --------------------------------------------------------------------------- }

// TODO: TestGetPendingRaceTasksToDestination needs stable thread teardown
// before it can be enabled. The core GetPendingRaceTasksToDestination logic
// is covered by the existing code in queueunit.pas under main_lock.

// procedure TTestQueueEvent.TestGetPendingRaceTasksToDestination;
// ...

{ --------------------------------------------------------------------------- }
{ _IsLowPriorityRaceTask tests                                                }
{ --------------------------------------------------------------------------- }

procedure TTestQueueEvent.TestIsLowPriorityRaceTask_SamplePriority3;
var
  fRaceTask: TPazoRaceTask;
begin
  sample_dirs_priority := 3;
  CheckEquals(3, sample_dirs_priority, 'sample_dirs_priority should be 3 after assignment');
  fRaceTask := TPazoRaceTask.Create('', '', 'SRC', 'DST', fPazo, nil, '/Sample', 'file.mp3', 1024, 1);
  fRaceTask.IsSample := True;
  try
    CheckTrue(fRaceTask.IsSample, 'fRaceTask.IsSample should be True after assignment');
    CheckTrue(_IsLowPriorityRaceTask(fRaceTask), 'Sample dir with priority 3 should be low priority');
  finally
    fRaceTask.Free;
  end;
end;

procedure TTestQueueEvent.TestIsLowPriorityRaceTask_SamplePriority1;
var
  fRaceTask: TPazoRaceTask;
begin
  sample_dirs_priority := 1;
  fRaceTask := TPazoRaceTask.Create('', '', 'SRC', 'DST', fPazo, nil, '/Sample', 'file.mp3', 1024, 1);
  fRaceTask.IsSample := True;
  try
    CheckFalse(_IsLowPriorityRaceTask(fRaceTask), 'Sample dir with priority 1 should NOT be low priority');
  finally
    fRaceTask.Free;
  end;
end;

procedure TTestQueueEvent.TestIsLowPriorityRaceTask_ProofPriority3;
var
  fRaceTask: TPazoRaceTask;
begin
  proof_dirs_priority := 3;
  fRaceTask := TPazoRaceTask.Create('', '', 'SRC', 'DST', fPazo, nil, '/Proof', 'file.mp3', 1024, 1);
  fRaceTask.IsProof := True;
  try
    CheckTrue(_IsLowPriorityRaceTask(fRaceTask), 'Proof dir with priority 3 should be low priority');
  finally
    fRaceTask.Free;
  end;
end;

procedure TTestQueueEvent.TestIsLowPriorityRaceTask_NormalRace;
var
  fRaceTask: TPazoRaceTask;
begin
  sample_dirs_priority := 3;
  proof_dirs_priority := 3;
  subs_dirs_priority := 3;
  cover_dirs_priority := 3;
  fRaceTask := TPazoRaceTask.Create('', '', 'SRC', 'DST', fPazo, nil, '/CD1', 'file.mp3', 1024, 1);
  fRaceTask.IsSample := False;
  fRaceTask.IsProof := False;
  fRaceTask.IsSubs := False;
  fRaceTask.IsCovers := False;
  try
    CheckFalse(_IsLowPriorityRaceTask(fRaceTask), 'Normal race task should NOT be low priority');
  finally
    fRaceTask.Free;
  end;
end;

procedure TTestQueueEvent.TestIsLowPriorityRaceTask_Nil;
begin
  CheckFalse(_IsLowPriorityRaceTask(nil), 'nil task should NOT be low priority');
end;

{ --------------------------------------------------------------------------- }
{ Diagnostic test                                                               }
{ --------------------------------------------------------------------------- }

procedure TTestQueueEvent.TestQueueEventDiagnostic;
begin
  CheckTrue(True, 'QueueEvent tests are running');
end;

{ --------------------------------------------------------------------------- }
{ _HasWaitingNonLowPriorityTasks tests                                        }
{ --------------------------------------------------------------------------- }

procedure TTestQueueEvent.TestHasWaitingNonLowPriorityTasks_Empty;
var
  fTaskList: Contnrs.TObjectList;
begin
  fTaskList := Contnrs.TObjectList.Create(False);
  try
    CheckFalse(_HasWaitingNonLowPriorityTasks(fTaskList, Now), 'Empty list should return False');
  finally
    fTaskList.Free;
  end;
end;

procedure TTestQueueEvent.TestHasWaitingNonLowPriorityTasks_OnlyLowPriority;
var
  fTaskList: Contnrs.TObjectList;
  fRaceTask: TPazoRaceTask;
begin
  sample_dirs_priority := 3;
  fTaskList := Contnrs.TObjectList.Create(False);
  try
    fRaceTask := TPazoRaceTask.Create('', '', 'SRC', 'DST', fPazo, nil, '/Sample', 'file.mp3', 1024, 1);
    fRaceTask.IsSample := True;
    fTaskList.Add(fRaceTask);
    try
      CheckFalse(_HasWaitingNonLowPriorityTasks(fTaskList, Now), 'Only low-priority tasks should return False');
    finally
      fRaceTask.Free;
    end;
  finally
    fTaskList.Free;
  end;
end;

procedure TTestQueueEvent.TestHasWaitingNonLowPriorityTasks_MkdirWaiting;
var
  fTaskList: Contnrs.TObjectList;
  fMkdirTask: TPazoMkdirTask;
begin
  fTaskList := Contnrs.TObjectList.Create(False);
  try
    fMkdirTask := TPazoMkdirTask.Create('', '', 'SRC', fPazo, nil, '');
    fTaskList.Add(fMkdirTask);
    try
      CheckTrue(_HasWaitingNonLowPriorityTasks(fTaskList, Now), 'Waiting mkdir task should return True');
    finally
      fMkdirTask.Free;
    end;
  finally
    fTaskList.Free;
  end;
end;

procedure TTestQueueEvent.TestHasWaitingNonLowPriorityTasks_RaceSfvWaiting;
var
  fTaskList: Contnrs.TObjectList;
  fRaceTask: TPazoRaceTask;
begin
  fTaskList := Contnrs.TObjectList.Create(False);
  try
    fRaceTask := TPazoRaceTask.Create('', '', 'SRC', 'DST', fPazo, nil, '', 'release.sfv', 1024, 1);
    fRaceTask.IsSfv := True;
    fTaskList.Add(fRaceTask);
    try
      CheckTrue(_HasWaitingNonLowPriorityTasks(fTaskList, Now), 'Waiting SFV race task should return True');
    finally
      fRaceTask.Free;
    end;
  finally
    fTaskList.Free;
  end;
end;

procedure TTestQueueEvent.TestHasWaitingNonLowPriorityTasks_AllAssigned;
var
  fTaskList: Contnrs.TObjectList;
  fMkdirTask: TPazoMkdirTask;
  fSite: TSite;
  fSlot: TSiteSlot;
begin
  fSite := FindSiteByName('', 'SRC');
  fSlot := TSiteSlot(fSite.slots[0]);

  fTaskList := Contnrs.TObjectList.Create(False);
  try
    fMkdirTask := TPazoMkdirTask.Create('', '', 'SRC', fPazo, nil, '');
    fMkdirTask.slot1 := fSlot;
    fSlot.todotask := fMkdirTask;
    try
      fTaskList.Add(fMkdirTask);
      CheckFalse(_HasWaitingNonLowPriorityTasks(fTaskList, Now), 'Assigned task should not count as waiting');
    finally
      fSlot.todotask := nil;
      fMkdirTask.Free;
    end;
  finally
    fTaskList.Free;
  end;
end;

{ --------------------------------------------------------------------------- }
{ _ScoreTask tests                                                            }
{ --------------------------------------------------------------------------- }

procedure TTestQueueEvent.TestScoreTask_WaitTask;
var
  fWaitTask: TWaitTask;
begin
  fWaitTask := TWaitTask.Create('', '', 'SRC');
  try
    CheckEquals(100000000, _ScoreTask(fWaitTask), 'WaitTask should have score 100M');
  finally
    fWaitTask.Free;
  end;
end;

procedure TTestQueueEvent.TestScoreTask_MkdirMaindir;
var
  fMkdirTask: TPazoMkdirTask;
begin
  fMkdirTask := TPazoMkdirTask.Create('', '', 'SRC', fPazo, nil, '');
  try
    CheckEquals(90000000, _ScoreTask(fMkdirTask), 'Maindir mkdir should have score 90M');
  finally
    fMkdirTask.Free;
  end;
end;

procedure TTestQueueEvent.TestScoreTask_MkdirSubdir;
var
  fMkdirTask: TPazoMkdirTask;
begin
  fMkdirTask := TPazoMkdirTask.Create('', '', 'SRC', fPazo, nil, '/Subs');
  try
    CheckEquals(70000000, _ScoreTask(fMkdirTask), 'Subdir mkdir should have score 70M');
  finally
    fMkdirTask.Free;
  end;
end;

procedure TTestQueueEvent.TestScoreTask_RaceSfv;
var
  fRaceTask: TPazoRaceTask;
begin
  fRaceTask := TPazoRaceTask.Create('', '', 'SRC', 'DST', fPazo, nil, '', 'release.sfv', 1024, 5);
  fRaceTask.IsSfv := True;
  try
    CheckEquals(85005000, _ScoreTask(fRaceTask), 'SFV race should have score 80M + 5M + rank*1000 + filesize');
  finally
    fRaceTask.Free;
  end;
end;

procedure TTestQueueEvent.TestScoreTask_RaceNfo;
var
  fRaceTask: TPazoRaceTask;
begin
  fRaceTask := TPazoRaceTask.Create('', '', 'SRC', 'DST', fPazo, nil, '', 'release.nfo', 1024, 3);
  fRaceTask.IsNfo := True;
  try
    CheckEquals(84003000, _ScoreTask(fRaceTask), 'NFO race should have score 80M + 4M + rank*1000 + filesize');
  finally
    fRaceTask.Free;
  end;
end;

procedure TTestQueueEvent.TestScoreTask_RaceNormal;
var
  fRaceTask: TPazoRaceTask;
begin
  fRaceTask := TPazoRaceTask.Create('', '', 'SRC', 'DST', fPazo, nil, '/CD1', 'file.mp3', 10485760, 2);
  try
    CheckEquals(80002010, _ScoreTask(fRaceTask), 'Normal race should have score 80M + rank*1000 + filesize/1M');
  finally
    fRaceTask.Free;
  end;
end;

procedure TTestQueueEvent.TestScoreTask_Dirlist;
var
  fDirlistTask: TPazoDirlistTask;
begin
  fDirlistTask := TPazoDirlistTask.Create('', '', 'SRC', fPazo, '/test', False);
  try
    CheckEquals(60000000, _ScoreTask(fDirlistTask), 'Dirlist should have score 60M');
  finally
    fDirlistTask.Free;
  end;
end;

procedure TTestQueueEvent.TestScoreTask_LoginTask;
var
  fLoginTask: TLoginTask;
begin
  fLoginTask := TLoginTask.Create('', '', 'SRC', False, False);
  try
    CheckEquals(50000000, _ScoreTask(fLoginTask), 'LoginTask should have score 50M');
  finally
    fLoginTask.Free;
  end;
end;

{ FindBestTask tests                                                              }
{ --------------------------------------------------------------------------- }

procedure TTestQueueEvent.TestFindBestTask_EmptyQueue;
var
  fQueue: TQueueThread;
  fResult: TTask;
begin
  fQueue := TQueueThread.Create('SRC');
  try
    fResult := fQueue.FindBestTask(Now);
    CheckTrue(fResult = nil, 'Empty queue should return nil');
  finally
    Queues.Remove(fQueue);
    fQueue.Free;
  end;
end;

procedure TTestQueueEvent.TestFindBestTask_SingleTask;
var
  fQueue: TQueueThread;
  fWaitTask: TWaitTask;
  fResult: TTask;
begin
  fQueue := TQueueThread.Create('SRC');
  try
    fWaitTask := TWaitTask.Create('', '', 'SRC');
    fQueue.AddTask(fWaitTask);
    fResult := fQueue.FindBestTask(Now);
    CheckTrue(fResult = fWaitTask, 'Should return the single waiting task');
  finally
    Queues.Remove(fQueue);
    fQueue.Free;
  end;
end;

procedure TTestQueueEvent.TestFindBestTask_PriorityOrdering;
var
  fQueue: TQueueThread;
  fWaitTask: TWaitTask;
  fLoginTask: TLoginTask;
  fResult: TTask;
begin
  fQueue := TQueueThread.Create('SRC');
  try
    fLoginTask := TLoginTask.Create('', '', 'SRC', False, False);
    fQueue.AddTask(fLoginTask);
    fWaitTask := TWaitTask.Create('', '', 'SRC');
    fQueue.AddTask(fWaitTask);
    fResult := fQueue.FindBestTask(Now);
    CheckTrue(fResult = fWaitTask, 'WaitTask should beat LoginTask');
  finally
    Queues.Remove(fQueue);
    fQueue.Free;
  end;
end;

procedure TTestQueueEvent.TestFindBestTask_DelayedTaskSkipped;
var
  fQueue: TQueueThread;
  fWaitTask: TWaitTask;
  fResult: TTask;
begin
  fQueue := TQueueThread.Create('SRC');
  try
    fWaitTask := TWaitTask.Create('', '', 'SRC');
    fWaitTask.startat := Now + 1;
    fQueue.AddTask(fWaitTask);
    fResult := fQueue.FindBestTask(Now);
    CheckTrue(fResult = nil, 'Delayed task should be skipped');
  finally
    Queues.Remove(fQueue);
    fQueue.Free;
  end;
end;

procedure TTestQueueEvent.TestFindBestTask_AssignedTaskSkipped;
var
  fQueue: TQueueThread;
  fWaitTask: TWaitTask;
  fResult: TTask;
begin
  fQueue := TQueueThread.Create('SRC');
  try
    fWaitTask := TWaitTask.Create('', '', 'SRC');
    fWaitTask.slot1 := TObject.Create; // fake assigned
    fQueue.AddTask(fWaitTask);
    fResult := fQueue.FindBestTask(Now);
    CheckTrue(fResult = nil, 'Assigned task should be skipped');
    fWaitTask.slot1.Free;
  finally
    Queues.Remove(fQueue);
    fQueue.Free;
  end;
end;

initialization
  {$IFDEF FPC}
    RegisterTest('queueunit', TTestQueueEvent.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestQueueEvent);
  {$ENDIF}
end.
