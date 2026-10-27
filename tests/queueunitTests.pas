unit queueunitTests;

interface

uses
  {$IFDEF FPC}
    fpcunit, testregistry;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestQueueunit = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestQueueUsableAfterShutdown;
  end;

implementation

uses
  SysUtils, mainthread, queueunit;

{ TTestQueueunit }

procedure TTestQueueunit.SetUp;
begin
  QueueInit;
end;

procedure TTestQueueunit.TearDown;
begin
  QueueUninit;
end;

procedure TTestQueueunit.TestQueueUsableAfterShutdown;
var
  fQueue: TQueueThread;
begin
  // on shutdown, the queue thread stops while pazos are still freed and remove their tasks from it
  slshutdown := True;
  try
    fQueue := TQueueThread.Create('TESTSITE');
    try
      fQueue.WaitFor;
      fQueue.QueueFire;
      CheckTrue(fQueue.RemovePazo(1, True), 'RemovePazo after the thread has stopped');
    finally
      fQueue.Free;
    end;
  finally
    slshutdown := False;
  end;
end;

initialization
  {$IFDEF FPC}
    RegisterTest('queueunit', TTestQueueunit.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestQueueunit);
  {$ENDIF}
end.
