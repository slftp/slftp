unit slcriticalsection2Tests;

interface

uses
  {$IFDEF FPC}
    fpcunit, testregistry;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestSLCriticalSection2 = class(TTestCase)
  protected
    procedure SetUp; override;
    {$IFNDEF FPC}
      procedure Teardown; override;
    {$ENDIF}
  published
    procedure TestNestedLocks;
    procedure TestGetOrCreate;
    procedure TestTimeoutsAreCounted;
    procedure TestTimeoutMessageNamesCodeSegment;
    procedure TestMaxHoldTime;
    procedure TestLongHoldIsLogged;
  end;

implementation

uses
  Classes, SysUtils, StrUtils, SyncObjs, debugunit, slcriticalsection2;

type
  { Holds a lock in its own thread until it is released, so that the test thread runs into timeouts. }
  TLockHolderThread = class(TThread)
  private
    FLock: TSlCriticalSection2;
    FCodeSegmentName: String;
    FLockedEvent: TEvent;
    FReleaseEvent: TEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(const aLock: TSlCriticalSection2; const aCodeSegmentName: String);
    { Releases the lock and waits until the thread has finished. }
    destructor Destroy; override;
    { @returns(@true if the thread holds the lock, @false if it did not get it within 5 seconds) }
    function WaitUntilLocked: Boolean;
  end;

constructor TLockHolderThread.Create(const aLock: TSlCriticalSection2; const aCodeSegmentName: String);
begin
  FLock := aLock;
  FCodeSegmentName := aCodeSegmentName;
  FLockedEvent := TEvent.Create(nil, True, False, '');
  FReleaseEvent := TEvent.Create(nil, True, False, '');
  inherited Create(False);
end;

destructor TLockHolderThread.Destroy;
begin
  FReleaseEvent.SetEvent;
  inherited; // waits for the thread
  FLockedEvent.Free;
  FReleaseEvent.Free;
end;

procedure TLockHolderThread.Execute;
begin
  FLock.Enter('holder');
  try
    FLock.SetCurrentCodeSegment(FCodeSegmentName);
    FLockedEvent.SetEvent;
    FReleaseEvent.WaitFor($FFFFFFFF);
  finally
    FLock.Leave;
  end;
end;

function TLockHolderThread.WaitUntilLocked: Boolean;
begin
  Result := FLockedEvent.WaitFor(5000) = wrSignaled;
end;

{ Writes the lock stats file, deletes it again and returns the lines of the given lock. }
procedure _GetLockStats(const aLockName: String; const aStats: TStrings);
var
  fFileName: String;
  fLines: TStringList;
  i: Integer;
begin
  aStats.Clear;
  fLines := TStringList.Create;
  try
    fFileName := WriteCriticalSection2StatsToFile;
    try
      fLines.LoadFromFile(fFileName);
    finally
      DeleteFile(fFileName);
    end;

    i := fLines.IndexOf('Critical Section: ' + aLockName);
    if i < 0 then
      exit;

    Inc(i);
    while (i < fLines.Count) and (fLines[i] <> '') do
    begin
      aStats.Add(fLines[i]);
      Inc(i);
    end;
  finally
    fLines.Free;
  end;
end;

{ Returns a value like total or max of a lock owner from the stats of one lock, -1 if not found. }
function _GetTimeValue(const aStats: TStrings; const aSection, aLockOwnerName, aKey: String): Double;
var
  i, fStart, fEnd: Integer;
begin
  Result := -1;

  i := aStats.IndexOf('  ' + aSection + ':');
  if i < 0 then
    exit;

  Inc(i);
  while (i < aStats.Count) and StartsStr('    ', aStats[i]) do
  begin
    if StartsStr('    ' + aLockOwnerName + ': ', aStats[i]) then
    begin
      fStart := Pos(aKey + '=', aStats[i]);
      if fStart = 0 then
        exit;
      Inc(fStart, Length(aKey) + 1);

      // the decimal separator can be a comma, the separator between values is a comma with a space
      fEnd := PosEx(', ', aStats[i], fStart);
      if fEnd = 0 then
        fEnd := Length(aStats[i]) + 1;

      Result := StrToFloat(Copy(aStats[i], fStart, fEnd - fStart));
      exit;
    end;
    Inc(i);
  end;
end;

{$IFDEF FPC}
var
  // fpcunit has no SetUpOnce, so guard the setup manually: the critical
  // section registry is process-global anyway
  glSlCriticalSection2SetupDone: Boolean = False;
{$ENDIF}

{ TTestSLCriticalSection2 }

procedure TTestSLCriticalSection2.SetUp;
begin
  {$IFDEF FPC}
  if glSlCriticalSection2SetupDone then
    Exit;
  glSlCriticalSection2SetupDone := True;
  {$ENDIF}

  SlCriticalSection2Init(100, True);
end;

{$IFNDEF FPC}
procedure TTestSLCriticalSection2.Teardown;
begin
  SlCriticalSection2Uninit;
end;
{$ENDIF}

// tests that the CurrentLockOwnerName returns the correct value in case of
// the same thread entering the same lock multiple times
procedure TTestSLCriticalSection2.TestNestedLocks;
var
  cs: TSlCriticalSection2;
begin
  cs := TSlCriticalSection2.Create('Test');
  try
    cs.Enter('outer lock');
    CheckEquals('outer lock', cs.CurrentLockOwnerName);
    cs.Enter('inner lock');
    CheckEquals('inner lock', cs.CurrentLockOwnerName);
    cs.Leave;
    CheckEquals('outer lock', cs.CurrentLockOwnerName);
    cs.Leave;
    CheckEquals('', cs.CurrentLockOwnerName);
  finally
    cs.Free;
  end;
end;

procedure TTestSLCriticalSection2.TestGetOrCreate;
var
  cs1: TSlCriticalSection2;
  cs2: TSlCriticalSection2;
begin
  cs1 := TSlCriticalSection2.GetOrCreate('Test2');
  cs2 := TSlCriticalSection2.GetOrCreate('Test2');
  // we must get the same object from both calls. first call creates the object, second call returns the already existing object.
  CheckTrue(cs1 = cs2);
end;

// tests that timed out waits are counted per lock owner, with and without exception
procedure TTestSLCriticalSection2.TestTimeoutsAreCounted;
var
  cs: TSlCriticalSection2;
  fHolder: TLockHolderThread;
  fStats: TStringList;
  fRaised: Boolean;
  i: Integer;
begin
  cs := TSlCriticalSection2.Create('TestTimeoutCount');
  fStats := TStringList.Create;
  try
    fHolder := TLockHolderThread.Create(cs, '');
    try
      CheckTrue(fHolder.WaitUntilLocked, 'holder thread did not get the lock');

      CheckFalse(cs.Enter('try waiter', 1, False));
      CheckFalse(cs.Enter('try waiter', 1, False));

      fRaised := False;
      try
        cs.Enter('raising waiter', 1);
      except
        on E: Exception do
          fRaised := True;
      end;
      CheckTrue(fRaised, 'timeout did not raise an exception');
    finally
      fHolder.Free;
    end;

    _GetLockStats('TestTimeoutCount', fStats);
    i := fStats.IndexOf('  Timeouts:');
    CheckTrue(i >= 0, 'no timeouts in stats: ' + fStats.Text);
    CheckTrue(fStats.IndexOf('    try waiter: count=2') > i, 'try waiter: ' + fStats.Text);
    CheckTrue(fStats.IndexOf('    raising waiter: count=1') > i, 'raising waiter: ' + fStats.Text);
  finally
    fStats.Free;
    cs.Free;
  end;
end;

// tests that the timeout message tells what the thread holding the lock is doing
procedure TTestSLCriticalSection2.TestTimeoutMessageNamesCodeSegment;
var
  cs: TSlCriticalSection2;
  fHolder: TLockHolderThread;
  fMessage: String;
begin
  cs := TSlCriticalSection2.Create('TestTimeoutMessage');
  try
    fMessage := '';
    fHolder := TLockHolderThread.Create(cs, 'holder segment');
    try
      CheckTrue(fHolder.WaitUntilLocked, 'holder thread did not get the lock');
      try
        cs.Enter('waiter', 1);
      except
        on E: Exception do
          fMessage := E.Message;
      end;
    finally
      fHolder.Free;
    end;

    CheckTrue(Pos('- holder (holder segment)', fMessage) > 0, 'unexpected message: ' + fMessage);
  finally
    cs.Free;
  end;
end;

// tests that the longest hold of a lock owner is recorded besides the total
procedure TTestSLCriticalSection2.TestMaxHoldTime;
var
  cs: TSlCriticalSection2;
  fStats: TStringList;
  fMax, fTotal: Double;
begin
  cs := TSlCriticalSection2.Create('TestMaxHold');
  fStats := TStringList.Create;
  try
    cs.Enter('sleeper');
    Sleep(50);
    cs.Leave;
    cs.Enter('sleeper');
    Sleep(10);
    cs.Leave;

    _GetLockStats('TestMaxHold', fStats);
    fMax := _GetTimeValue(fStats, 'Hold Times', 'sleeper', 'max');
    fTotal := _GetTimeValue(fStats, 'Hold Times', 'sleeper', 'total');
    CheckTrue(fMax >= 40, Format('max %.3f is shorter than the longer hold: %s', [fMax, fStats.Text]));
    // the shorter hold took at least 10 ms, so max must be clearly below the total
    CheckTrue(fMax < fTotal - 5, Format('max %.3f is not below total %.3f: %s', [fMax, fTotal, fStats.Text]));
  finally
    fStats.Free;
    cs.Free;
  end;
end;

// tests that a hold of at least one second is logged with lock owner and code segment
procedure TTestSLCriticalSection2.TestLongHoldIsLogged;
var
  cs: TSlCriticalSection2;
  fLog: String;
begin
  cs := TSlCriticalSection2.Create('TestLongHold');
  try
    cs.Enter('long holder');
    try
      cs.SetCurrentCodeSegment('long hold segment');
      Sleep(1100);
    finally
      cs.Leave;
    end;

    fLog := LogTail(20);
    CheckTrue(Pos('[WARN] Lock ''TestLongHold'' was held by thread', fLog) > 0, 'not logged: ' + fLog);
    CheckTrue(Pos('- long holder (long hold segment)', fLog) > 0, 'owner or code segment missing: ' + fLog);
  finally
    cs.Free;
  end;
end;

initialization
  {$IFDEF FPC}
    RegisterTest('TTestSLCriticalSection2', TTestSLCriticalSection2.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestSLCriticalSection2);
  {$ENDIF}
end.
