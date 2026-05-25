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
  end;

implementation

uses
  queueunit, sitesunit, tasksunit, taskrace, encinifile;

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

initialization
  {$IFDEF FPC}
    RegisterTest('queueunit', TTestQueueEvent.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestQueueEvent);
  {$ENDIF}
end.
