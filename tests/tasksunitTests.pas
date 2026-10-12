unit tasksunitTests;

interface

uses
  {$IFDEF FPC}TestFramework;{$ELSE}DUnitX.TestFramework, DUnitX.DUnitCompatibility;{$ENDIF}

type
  { Tests disabled-site policy and task ownership without starting FTP workers. }
  TTestTaskSiteDown = class(TTestCase)
  published
    procedure TestSitePolicyKeepsTemporaryRecovery;
    procedure TestProductionTaskClassification;
    procedure TestInsertionRejectsDisabledTask;
    procedure TestDisabledSourceCancelsPendingTask;
    procedure TestDisabledDestinationCancelsPendingTask;
    procedure TestDownBetweenInsertionAndAssignment;
    procedure TestAssignedTaskRetainsWorkerOwnership;
    procedure TestAdministrativeTaskRemainsAllowed;
    procedure TestWorkerCancellationKeepsSlotsUntilCleanup;
  end;

implementation

uses SysUtils, sitesunit, tasksunit, queueunit, taskrace, tasklogin, taskquit, taskidle, taskautodirlist,
  taskautoindex, taskautonuke, taskrules;

type
  { Synthetic site: no inherited constructor, slots, threads or configuration writes. }
  TPolicySite = class(TSite)
  private
    FStatus: TSiteStatus;
  public
    constructor Create(const aStatus: TSiteStatus);
    destructor Destroy; override;
    function AutomaticTaskDownReason: String; override;
    property Status: TSiteStatus read FStatus write FStatus;
  end;

  { Synthetic task: uses the production cancellation method, without task-ID globals. }
  TPolicyTask = class(TTask)
  private
    FAutomatic: Boolean;
    FDestroyed: PBoolean;
  public
    constructor Create(aSource, aTarget: TSite; const aAutomatic: Boolean = True; aDestroyed: PBoolean = nil);
    destructor Destroy; override;
    function RequiresAvailableSite: Boolean; override;
    function Name: String; override;
    function Execute(aSlot: Pointer): Boolean; override;
  end;

  { No thread is created: only the production pre-assignment rejection path is exercised. }
  TAssignmentProbe = class(TQueueThread)
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    constructor Create;
    destructor Destroy; override;
  end;

constructor TPolicySite.Create(const aStatus: TSiteStatus);
begin
  FStatus := aStatus;
end;

destructor TPolicySite.Destroy;
begin
end;

function TPolicySite.AutomaticTaskDownReason: String;
begin
  Result := GetAutomaticTaskDownReason(FStatus, False);
end;

constructor TPolicyTask.Create(aSource, aTarget: TSite; const aAutomatic: Boolean; aDestroyed: PBoolean);
begin
  ssite1 := aSource;
  ssite2 := aTarget;
  site1 := 'source';
  site2 := 'target';
  FAutomatic := aAutomatic;
  FDestroyed := aDestroyed;
end;

destructor TPolicyTask.Destroy;
begin
  if FDestroyed <> nil then FDestroyed^ := True;
  inherited;
end;

function TPolicyTask.RequiresAvailableSite: Boolean;
begin
  Result := FAutomatic;
end;

function TPolicyTask.Name: String;
begin
  Result := 'Site-policy test';
end;

function TPolicyTask.Execute(aSlot: Pointer): Boolean;
begin
  raise Exception.Create('A cancelled task must never execute');
end;

procedure TAssignmentProbe.AfterConstruction;
begin
end;

procedure TAssignmentProbe.BeforeDestruction;
begin
end;

constructor TAssignmentProbe.Create;
begin
end;

destructor TAssignmentProbe.Destroy;
begin
end;

procedure TTestTaskSiteDown.TestSitePolicyKeepsTemporaryRecovery;
begin
  CheckEqualsString('', GetAutomaticTaskDownReason(sstUp, False));
  CheckEqualsString('', GetAutomaticTaskDownReason(sstUnknown, False));
  CheckEqualsString('', GetAutomaticTaskDownReason(sstTempDown, False));
  CheckEqualsString('site_marked_down', GetAutomaticTaskDownReason(sstMarkedAsDownByUser, False));
  CheckEqualsString('site_down', GetAutomaticTaskDownReason(sstDown, False));
  CheckEqualsString('site_permdown', GetAutomaticTaskDownReason(sstUp, True));
end;

procedure TTestTaskSiteDown.TestProductionTaskClassification;
const
  fAutomaticClasses: array[0..6] of TClass = (TPazoDirlistTask, TPazoRaceTask,
    TPazoMkdirTask, TAutoDirlistTask, TAutoIndexTask, TAutoNukeTask, TRulesTask);
  fAdministrativeClasses: array[0..3] of TClass = (TWaitTask, TQuitTask, TIdleTask, TLoginTask);
var
  fTask: TTask;
  fClass: TClass;
begin
  { Only inspect type policy: unconstructed instances have no workers or pazo ownership. }
  for fClass in fAutomaticClasses do
  begin
    fTask := TTask(fClass.NewInstance);
    try
      CheckTrue(fTask.RequiresAvailableSite, fClass.ClassName);
    finally
      fTask.FreeInstance;
    end;
  end;
  for fClass in fAdministrativeClasses do
  begin
    fTask := TTask(fClass.NewInstance);
    try
      CheckFalse(fTask.RequiresAvailableSite, fClass.ClassName);
      if fTask is TLoginTask then
      begin
        TLoginTask(fTask).readd := True;
        CheckTrue(fTask.RequiresAvailableSite, 'Automatic BNC test is not an explicit login');
      end;
    finally
      fTask.FreeInstance;
    end;
  end;
end;

procedure TTestTaskSiteDown.TestInsertionRejectsDisabledTask;
var
  fSite: TPolicySite;
  fQueue: TAssignmentProbe;
  fTask: TPolicyTask;
  fDestroyed: Boolean;
begin
  fDestroyed := False;
  fSite := TPolicySite.Create(sstMarkedAsDownByUser);
  fQueue := TAssignmentProbe.Create;
  fTask := TPolicyTask.Create(fSite, nil, True, @fDestroyed);
  try
    { No queue storage exists: insertion must reject and release before using it. }
    fQueue.AddTask(fTask);
    CheckTrue(fDestroyed, 'Rejected work must not be stored or leaked');
  finally
    if not fDestroyed then fTask.Free;
    fQueue.Free;
    fSite.Free;
  end;
end;

procedure TTestTaskSiteDown.TestDisabledSourceCancelsPendingTask;
var
  fSite: TPolicySite;
  fTask: TPolicyTask;
begin
  fSite := TPolicySite.Create(sstMarkedAsDownByUser);
  fTask := TPolicyTask.Create(fSite, nil);
  try
    fTask.startat := Now + 1;
    fTask.dontremove := True;
    CheckTrue(fTask.CancelIfSiteDown, 'Delayed automatic work must also be cancelled');
    CheckTrue(fTask.ready);
    CheckFalse(fTask.readyerror, 'Administrative cancellation must not fail the release');
    CheckEqualsString('', fTask.response, 'Cancellation must not replace task result data');
    CheckFalse(fTask.CancelIfSiteDown, 'Cancellation must be idempotent');
  finally
    fTask.Free;
    fSite.Free;
  end;
end;

procedure TTestTaskSiteDown.TestDisabledDestinationCancelsPendingTask;
var
  fSource, fTarget: TPolicySite;
  fTask: TPolicyTask;
begin
  fSource := TPolicySite.Create(sstUp);
  fTarget := TPolicySite.Create(sstMarkedAsDownByUser);
  fTask := TPolicyTask.Create(fSource, fTarget);
  try
    CheckTrue(fTask.CancelIfSiteDown, 'A live source must not bypass the disabled target');
    CheckEqualsString('', fTask.response, 'Cancellation must not replace task result data');
  finally
    fTask.Free;
    fTarget.Free;
    fSource.Free;
  end;
end;

procedure TTestTaskSiteDown.TestDownBetweenInsertionAndAssignment;
var
  fSite: TPolicySite;
  fTask: TPolicyTask;
  fQueue: TAssignmentProbe;
begin
  fSite := TPolicySite.Create(sstUp);
  fTask := TPolicyTask.Create(fSite, nil);
  fQueue := TAssignmentProbe.Create;
  try
    CheckFalse(fTask.CancelIfSiteDown);
    fSite.Status := sstMarkedAsDownByUser;
    fQueue.TryToAssignSlots(fTask);
    CheckTrue(fTask.ready, 'The dispatcher must recheck a later Setdown');
    CheckTrue(fTask.slot1 = nil);
    CheckTrue(fTask.slot2 = nil);
  finally
    fQueue.Free;
    fTask.Free;
    fSite.Free;
  end;
end;

procedure TTestTaskSiteDown.TestAssignedTaskRetainsWorkerOwnership;
var
  fSite: TPolicySite;
  fTask: TPolicyTask;
begin
  fSite := TPolicySite.Create(sstMarkedAsDownByUser);
  fTask := TPolicyTask.Create(fSite, nil);
  try
    fTask.slot1 := Pointer(fSite);
    CheckFalse(fTask.CancelIfSiteDown);
    CheckFalse(fTask.ready, 'An executing task must retain its worker-owned lifetime');
    CheckTrue(fTask.slot1 = Pointer(fSite));
  finally
    fTask.Free;
    fSite.Free;
  end;
end;

procedure TTestTaskSiteDown.TestWorkerCancellationKeepsSlotsUntilCleanup;
var
  fSite: TPolicySite;
  fTask: TPolicyTask;
begin
  fSite := TPolicySite.Create(sstMarkedAsDownByUser);
  fTask := TPolicyTask.Create(fSite, nil);
  try
    fTask.slot1 := Pointer(fSite);
    CheckTrue(fTask.CancelIfSiteDown(True));
    CheckTrue(fTask.ready);
    CheckFalse(fTask.readyerror);
    CheckTrue(fTask.slot1 = Pointer(fSite), 'The worker still owns slot cleanup');
  finally
    fTask.Free;
    fSite.Free;
  end;
end;

procedure TTestTaskSiteDown.TestAdministrativeTaskRemainsAllowed;
var
  fSite: TPolicySite;
  fTask: TPolicyTask;
begin
  fSite := TPolicySite.Create(sstMarkedAsDownByUser);
  fTask := TPolicyTask.Create(fSite, nil, False);
  try
    CheckFalse(fTask.CancelIfSiteDown, 'Login, Quit and WAIT are not automatic trading work');
    CheckFalse(fTask.ready);
  finally
    fTask.Free;
    fSite.Free;
  end;
end;

initialization
  {$IFDEF FPC}RegisterTest('tasksunit', TTestTaskSiteDown.Suite);
  {$ELSE}TDUnitX.RegisterTestFixture(TTestTaskSiteDown);{$ENDIF}
end.
