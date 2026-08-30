unit watchdogTests;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestWatchdog = class(TTestCase)
  published
    procedure TestParticipantLifecycle;
    procedure TestStaleParticipantMarkedStalled;
    procedure TestGenerateReport;
  end;

implementation

uses
  SysUtils, Classes, Math, slcriticalsection2, watchdog;

// participants must be registered on creation and removed from the registry on release
procedure TTestWatchdog.TestParticipantLifecycle;
var
  fParticipant: TWatchdogParticipant;
begin
  fParticipant := WatchdogNewParticipant('test/lifecycle', 120);
  try
    CheckTrue(fParticipant <> nil);
    CheckEqualsString('test/lifecycle', fParticipant.Name);
    CheckEquals(120, fParticipant.StallSeconds);

    fParticipant.Beat;
    fParticipant.SetInfo('status=testing');
    CheckEqualsString('status=testing', string(fParticipant.Info));
  finally
    WatchdogReleaseParticipant(fParticipant);
  end;

  CheckTrue(fParticipant = nil);
end;

// a participant which does not beat within its threshold must be detected in a generated report
procedure TTestWatchdog.TestStaleParticipantMarkedStalled;
var
  fParticipant: TWatchdogParticipant;
  fReportFile: String;
  fReport: TStringList;
  fStaleLine: String;
  fLine: String;
begin
  fParticipant := WatchdogNewParticipant('test/stale', 1);
  try
    fParticipant.SetInfo('status=frozen');
    Sleep(1500);

    fReportFile := WatchdogGenerateReport('unit test stall');
    try
      CheckTrue(fReportFile <> '');
      CheckTrue(FileExists(fReportFile));

      fReport := TStringList.Create;
      try
        fReport.LoadFromFile(fReportFile);
        fStaleLine := '';
        for fLine in fReport do
          if Pos('test/stale', fLine) > 0 then
            fStaleLine := fLine;

        CheckTrue(fStaleLine <> '', 'report must contain the test/stale participant');
        CheckTrue(Pos('[STALL]', fStaleLine) > 0, 'silent participant must be marked as STALL');
        CheckTrue(Pos('status=frozen', fStaleLine) > 0, 'participant info must appear in the report');
        CheckTrue(Pos('unit test stall', fReport.Text) > 0, 'report must contain the trigger reason');
      finally
        fReport.Free;
      end;
    finally
      DeleteFile(fReportFile);
    end;
  finally
    WatchdogReleaseParticipant(fParticipant);
  end;
end;

// the report must contain the heartbeat and lock sections and only keep the configured amount of files
procedure TTestWatchdog.TestGenerateReport;
var
  fReportFile: String;
  fReport: TStringList;
  fLock: TSlCriticalSection2;
  fSearchRec: TSearchRec;
  fReportCount: integer;
begin
  fLock := TSlCriticalSection2.Create('test/reportlock');
  try
    fLock.Enter('TestGenerateReport');
    try
      fReportFile := WatchdogGenerateReport('unit test report');
      try
        CheckTrue(fReportFile <> '');
        CheckTrue(FileExists(fReportFile));

        fReport := TStringList.Create;
        try
          fReport.LoadFromFile(fReportFile);
          CheckTrue(Pos('[heartbeats]', fReport.Text) > 0, 'report must contain the heartbeat section');
          CheckTrue(Pos('[locks]', fReport.Text) > 0, 'report must contain the lock section');
          CheckTrue(Pos('test/reportlock', fReport.Text) > 0, 'report must contain the test lock state');
        finally
          fReport.Free;
        end;
      finally
        DeleteFile(fReportFile);
      end;
    finally
      fLock.Leave;
    end;
  finally
    fLock.Free;
  end;

  // report pruning: after writing more reports than keep_reports the oldest must be removed
  WatchdogGenerateReport('unit test prune');
  fReportCount := 0;
  if FindFirst(ExtractFilePath(ParamStr(0)) + 'watchdog.*.log', faAnyFile, fSearchRec) = 0 then
  try
    repeat
      Inc(fReportCount);
      DeleteFile(ExtractFilePath(ParamStr(0)) + fSearchRec.Name);
    until FindNext(fSearchRec) <> 0;
  finally
    FindClose(fSearchRec);
  end;
  CheckTrue(fReportCount >= 1, 'at least one report file must have been written');
end;

initialization
  {$IFDEF FPC}
    RegisterTest('TTestWatchdog', TTestWatchdog.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestWatchdog);
  {$ENDIF}
end.
