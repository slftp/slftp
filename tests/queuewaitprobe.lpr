{ Temporary, credential-free event timeout probe. Reports observations without
  treating early timeouts as test failures: compare default and NOPATCHRTL builds. }
program queuewaitprobe;

{$MODE Delphi}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  mormot.core.os, SysUtils, SyncObjs, queuediagnostics;

procedure _ProbeWait(const aTimeoutMs: Cardinal; const aSamples: Integer);
var
  fEvent: TEvent;
  fDiagnostics: TQueueDiagnostics;
  fResult: TWaitResult;
  fStartedUs, fElapsedUs, fClockLagUs: Int64;
  fErrors: Integer;
  i: Integer;
begin
  fErrors := 0;
  fEvent := TEvent.Create(nil, False, False, '');
  try
    fDiagnostics := TQueueDiagnostics.Create;
    try
      for i := 1 to aSamples do
      begin
        fClockLagUs := QueueDiagnosticClockLag;
        fStartedUs := QueueDiagnosticNow;
        fResult := fEvent.WaitFor(aTimeoutMs);
        fElapsedUs := QueueDiagnosticNow - fStartedUs;
        fDiagnostics.RecordWait(aTimeoutMs, fElapsedUs, fResult, fClockLagUs);
        if fResult <> wrTimeout then
          Inc(fErrors);
      end;
      WriteLn(fDiagnostics.Snapshot('PROBE-' + IntToStr(aTimeoutMs) + 'MS',
        QueueDiagnosticNow, 0, 0, 0, 0, 0, True));
    finally
      fDiagnostics.Free;
    end;
  finally
    fEvent.Free;
  end;
  if fErrors > 0 then
    Halt(1);
end;

begin
  _ProbeWait(1, 500);
  _ProbeWait(5, 100);
end.
