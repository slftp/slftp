unit sltimerTests;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestSLTimer = class(TTestCase)
  protected
    {$IFDEF FPC}
      procedure SetUpOnce; override;
      procedure TeardownOnce; override;
    {$ELSE}
      procedure SetUp; override;
      procedure Teardown; override;
    {$ENDIF}
  published
    procedure TestElapsedTimeInMilliseconds;
    procedure TestElapsedTimeInMicroseconds;
    procedure TestTimerPrecision;
  end;

implementation

uses
  sltimer, SysUtils;

{ TTestSLTimer }

procedure TTestSLTimer.{$IFDEF FPC}SetUpOnce{$ELSE}SetUp{$ENDIF};
begin

end;

procedure TTestSLTimer.{$IFDEF FPC}TeardownOnce{$ELSE}Teardown{$ENDIF};
begin

end;

procedure TTestSLTimer.TestElapsedTimeInMilliseconds;
var
  Timer: TSLTimer;
begin
  Timer := TSLTimer.Create;
  try
    Timer.Start;
    Sleep(100); // sleep 100 ms
    Timer.Stop;
    CheckTrue(Timer.ElapsedMilliseconds >= 100, 'Elapsed time should be >= 100ms');
    CheckTrue(Timer.ElapsedMilliseconds < 200, 'Elapsed time should be < 200ms');
  finally
    Timer.Free;
  end;
end;

procedure TTestSLTimer.TestElapsedTimeInMicroseconds;
var
  Timer: TSLTimer;
begin
  Timer := TSLTimer.Create;
  try
    Timer.Start;
    Sleep(50); // sleep 50 ms
    Timer.Stop;
    CheckTrue(Timer.ElapsedMicroseconds >= 50000, 'Elapsed time should be >= 50000 µs');
    CheckTrue(Timer.ElapsedMicroseconds < 100000, 'Elapsed time should be < 100000 µs');
  finally
    Timer.Free;
  end;
end;

procedure TTestSLTimer.TestTimerPrecision;
var
  Timer: TSLTimer;
begin
  Timer := TSLTimer.Create;
  try
    Timer.Start;
    Timer.Stop;
    CheckTrue(Timer.ElapsedMicroseconds >= 0, 'Timer should report elapsed time >= 0');
  finally
    Timer.Free;
  end;
end;

initialization
  {$IFDEF FPC}
    RegisterTest('TTestSLTimer', TTestSLTimer.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestSLTimer);
  {$ENDIF}
end.
