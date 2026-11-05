unit pazoTests;

interface

uses
  {$IFDEF FPC}
    fpcunit, testregistry;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestPazo = class(TTestCase)
  published
    procedure IsFinishedFalseWithoutUDPBeforeTasksRan;
    procedure IsFinishedTrueWithUDPAndNoQueuedTask;
    procedure IsFinishedFalseWithUDPAndQueuedTask;
    procedure CbftpLatencyAddDropsStaleEntriesAboveLimit;
  end;

implementation

uses
  SysUtils, DateUtils, configunit, pazo;

type
  TUDPConfigBackup = record
    FEnableUDP: String;
    FIP: String;
    FPort: String;
    FHadEnableUDP: Boolean;
    FHadIP: Boolean;
    FHadPort: Boolean;
  end;

function _SetUDPConfig(const aEnabled: Boolean): TUDPConfigBackup;
begin
  Result.FHadEnableUDP := config.ValueExists('UDPConfig', 'EnableUDP');
  Result.FHadIP := config.ValueExists('UDPConfig', 'IP');
  Result.FHadPort := config.ValueExists('UDPConfig', 'Port');
  Result.FEnableUDP := config.ReadString('UDPConfig', 'EnableUDP', '');
  Result.FIP := config.ReadString('UDPConfig', 'IP', '');
  Result.FPort := config.ReadString('UDPConfig', 'Port', '');

  if aEnabled then
    config.WriteString('UDPConfig', 'EnableUDP', '1')
  else
    config.WriteString('UDPConfig', 'EnableUDP', '0');
  config.WriteString('UDPConfig', 'IP', '127.0.0.1');
  config.WriteString('UDPConfig', 'Port', '5696');
end;

procedure _RestoreValue(const aIdent, aValue: String; const aHadValue: Boolean);
begin
  if aHadValue then
    config.WriteString('UDPConfig', aIdent, aValue)
  else
    config.DeleteKey('UDPConfig', aIdent);
end;

procedure _RestoreUDPConfig(const aBackup: TUDPConfigBackup);
begin
  _RestoreValue('EnableUDP', aBackup.FEnableUDP, aBackup.FHadEnableUDP);
  _RestoreValue('IP', aBackup.FIP, aBackup.FHadIP);
  _RestoreValue('Port', aBackup.FPort, aBackup.FHadPort);
end;

procedure TTestPazo.IsFinishedFalseWithoutUDPBeforeTasksRan;
var
  fBackup: TUDPConfigBackup;
  p: TPazo;
begin
  fBackup := _SetUDPConfig(False);
  try
    p := TPazo.Create(nil, 1);
    try
      CheckFalse(p.IsUDPEnabled, 'UDP announce must be disabled');
      CheckFalse(p.IsFinished, 'Without UDP a pazo is only finished once its tasks set it ready');
    finally
      p.Free;
    end;
  finally
    _RestoreUDPConfig(fBackup);
  end;
end;

procedure TTestPazo.IsFinishedTrueWithUDPAndNoQueuedTask;
var
  fBackup: TUDPConfigBackup;
  p: TPazo;
begin
  fBackup := _SetUDPConfig(True);
  try
    p := TPazo.Create(nil, 2);
    try
      CheckTrue(p.IsUDPEnabled, 'UDP announce must be enabled');
      CheckFalse(p.ready, 'No task ran, so ready is never set');
      CheckTrue(p.IsFinished, 'With UDP and no queued task the pazo must count as finished');
    finally
      p.Free;
    end;
  finally
    _RestoreUDPConfig(fBackup);
  end;
end;

procedure TTestPazo.IsFinishedFalseWithUDPAndQueuedTask;
var
  fBackup: TUDPConfigBackup;
  p: TPazo;
begin
  fBackup := _SetUDPConfig(True);
  try
    p := TPazo.Create(nil, 3);
    try
      p.queuenumber.Increase;
      CheckFalse(p.IsFinished, 'A pazo with a queued task must not count as finished');
    finally
      p.Free;
    end;
  finally
    _RestoreUDPConfig(fBackup);
  end;
end;

procedure TTestPazo.CbftpLatencyAddDropsStaleEntriesAboveLimit;
var
  i: Integer;
  fStaleTime: TDateTime;
  fJson: String;
begin
  fStaleTime := IncDay(Now, -2);
  for i := 1 to 1000 do
    CbftpLatencyAdd(Format('Stale.Release.%d-GRP', [i]), fStaleTime, fStaleTime, 10);
  CbftpLatencyAdd('Fresh.Release-GRP', Now, Now, 10);

  fJson := CbftpLatencyGetJson;
  CheckTrue(Pos('"Fresh.Release-GRP"', fJson) > 0, 'The fresh entry must be kept');
  CheckEquals(0, Pos('"Stale.Release.', fJson), 'Entries older than 24 hours must be dropped above the limit');
end;

initialization
  {$IFDEF FPC}
    RegisterTest('pazo', TTestPazo.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestPazo);
  {$ENDIF}
end.
