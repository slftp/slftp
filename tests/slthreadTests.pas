unit slthreadTests;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestSlThread = class(TTestCase)
  published
    procedure TestRegisterUnregister;
    procedure TestSignalAllTerminatesThreads;
    procedure TestWaitAllReportsStragglers;
  end;

implementation

uses
  slthread, SysUtils, Classes, SyncObjs;

type
  { dummy thread which exits once terminated }
  TDummyThread = class(TSlThread)
  protected
    procedure Execute; override;
  end;

  { dummy thread which simulates being stuck in a long blocking wait:
    SignalStop (Terminate) alone does not wake it, only StopEvent does }
  TSlowThread = class(TSlThread)
  protected
    procedure Execute; override;
  public
    StopEvent: TEvent;
    constructor Create(const aThreadName: String);
    destructor Destroy; override;
  end;

procedure TDummyThread.Execute;
begin
  while not Terminated do
    Sleep(10);
end;

constructor TSlowThread.Create(const aThreadName: String);
begin
  StopEvent := TEvent.Create(nil, False, False, '');
  inherited Create(aThreadName, False);
end;

destructor TSlowThread.Destroy;
begin
  StopEvent.Free;
  inherited;
end;

procedure TSlowThread.Execute;
begin
  StopEvent.WaitFor(30000); // simulates a long blocking wait
end;

{ TTestSlThread }

procedure TTestSlThread.TestRegisterUnregister;
var
  fThread: TDummyThread;
  fStragglers: TStringList;
begin
  fThread := TDummyThread.Create('TestRegisterUnregister', True);
  try
    fStragglers := TSlThread.WaitAll(0);
    try
      CheckTrue(fStragglers.IndexOf('TestRegisterUnregister') >= 0, 'suspended thread should be registered');
    finally
      fStragglers.Free;
    end;
  finally
    fThread.Free;
  end;

  fStragglers := TSlThread.WaitAll(0);
  try
    CheckTrue(fStragglers.IndexOf('TestRegisterUnregister') < 0, 'freed thread should be unregistered');
  finally
    fStragglers.Free;
  end;
end;

procedure TTestSlThread.TestSignalAllTerminatesThreads;
var
  fThread: TDummyThread;
  fStragglers: TStringList;
begin
  // FreeOnTerminate style: the thread frees itself, we must not touch it afterwards
  fThread := TDummyThread.Create('TestSignalAllTerminates', False);
  fThread.FreeOnTerminate := True;

  TSlThread.SignalAll;

  fStragglers := TSlThread.WaitAll(10);
  try
    CheckTrue(fStragglers.IndexOf('TestSignalAllTerminates') < 0, 'thread should have terminated after SignalAll');
  finally
    fStragglers.Free;
  end;
end;

procedure TTestSlThread.TestWaitAllReportsStragglers;
var
  fThread: TSlowThread;
  fStragglers: TStringList;
begin
  fThread := TSlowThread.Create('TestStraggler');
  try
    fStragglers := TSlThread.WaitAll(1);
    try
      CheckTrue(fStragglers.IndexOf('TestStraggler') >= 0, 'stuck thread should be reported after the timeout');
    finally
      fStragglers.Free;
    end;

    fThread.StopEvent.SetEvent;

    fStragglers := TSlThread.WaitAll(10);
    try
      CheckTrue(fStragglers.IndexOf('TestStraggler') < 0, 'woken thread should no longer be reported');
    finally
      fStragglers.Free;
    end;
  finally
    fThread.StopEvent.SetEvent;
    fThread.WaitFor;
    fThread.Free;
  end;
end;

end.
