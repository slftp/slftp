unit raceperfunitTests;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestRacePerf = class(TTestCase)
  published
    procedure TestMarkersAndOutput;
    procedure TestEmptyOutput;
    procedure TestOneShotMarkers;
  end;

implementation

uses
  raceperfunit, SysUtils, Classes;

{ TTestRacePerf }

procedure TTestRacePerf.TestMarkersAndOutput;
var
  fPerf: TRacePerf;
  fLines: TStringList;
  fText: String;
begin
  fPerf := TRacePerf.Create(1000000);
  try
    fPerf.MarkDirlistCreated('SiteA', 'NEWDIR', 1100000);
    fPerf.MarkDirlistCreated('SiteA', 'UPDATE', 1200000);
    fPerf.MarkDirlistParsed('SiteA', 1500000);
    fPerf.MarkMkdirCreated('SiteA', 1600000);
    fPerf.MarkMkdirDone('SiteA', 1800000);
    fPerf.MarkRaceTaskCreated('SiteA', 1750000);
    fPerf.MarkRaceTaskCreated('SiteA', 1900000);
    fPerf.MarkRaceAssigned('SiteA', 2000000);
    fPerf.MarkRaceStarted('SiteA', 2100000);
    fPerf.MarkRaceFinished('SiteA', True);
    fPerf.MarkRaceFinished('SiteA', False);
    fPerf.MarkComplete('SiteA', 12000000);
    fPerf.MarkAllTasksIdle(12500000);

    fLines := fPerf.AsStrings;
    try
      CheckEquals(2, fLines.Count, 'expected global line + one site line');
      fText := fLines.Text;

      CheckTrue(Pos('first dirlist task +100.000 ms', fText) > 0, 'global first dirlist missing: ' + fText);
      CheckTrue(Pos('first race created +750.000 ms', fText) > 0, 'global first race created missing: ' + fText);
      CheckTrue(Pos('all tasks done +11.500 s', fText) > 0, 'global idle missing: ' + fText);

      CheckTrue(Pos('SiteA:', fText) > 0, 'site line missing: ' + fText);
      CheckTrue(Pos('dirlist +100.000 ms via NEWDIR', fText) > 0, 'dirlist created missing: ' + fText);
      CheckTrue(Pos('parsed +500.000 ms', fText) > 0, 'dirlist parsed missing: ' + fText);
      CheckTrue(Pos('2 tasks, 0 err', fText) > 0, 'dirlist counts missing: ' + fText);
      CheckTrue(Pos('mkdir +600.000 ms -> done +800.000 ms (waited 200.000 ms, 0 err)', fText) > 0, 'mkdir missing: ' + fText);
      CheckTrue(Pos('races 2 created (first +750.000 ms)', fText) > 0, 'race count missing: ' + fText);
      CheckTrue(Pos('queue wait 250.000 ms', fText) > 0, 'queue wait missing: ' + fText);
      CheckTrue(Pos('1 ok / 1 err', fText) > 0, 'race results missing: ' + fText);
      CheckTrue(Pos('complete +11.000 s', fText) > 0, 'complete missing: ' + fText);
    finally
      fLines.Free;
    end;
  finally
    fPerf.Free;
  end;
end;

procedure TTestRacePerf.TestEmptyOutput;
var
  fPerf: TRacePerf;
  fLines: TStringList;
begin
  fPerf := TRacePerf.Create(1000000);
  try
    fLines := fPerf.AsStrings;
    try
      CheckEquals(1, fLines.Count, 'expected only the global line');
      CheckTrue(Pos('first dirlist task -', fLines[0]) > 0, 'unset markers should be shown as -: ' + fLines[0]);
    finally
      fLines.Free;
    end;
  finally
    fPerf.Free;
  end;
end;

procedure TTestRacePerf.TestOneShotMarkers;
var
  fPerf: TRacePerf;
  fLines: TStringList;
  fText: String;
begin
  // one-shot markers must keep the first timestamp even when marked again
  fPerf := TRacePerf.Create(1000000);
  try
    fPerf.MarkDirlistCreated('SiteA', '', 1100000);
    fPerf.MarkDirlistCreated('SiteA', '', 9900000);
    fPerf.MarkComplete('SiteA', 2000000);
    fPerf.MarkComplete('SiteA', 9900000);
    fPerf.MarkAllTasksIdle(3000000);
    fPerf.MarkAllTasksIdle(9900000);

    fLines := fPerf.AsStrings;
    try
      fText := fLines.Text;
      CheckTrue(Pos('dirlist +100.000 ms', fText) > 0, 'first dirlist timestamp overwritten: ' + fText);
      CheckTrue(Pos('complete +1000.000 ms', fText) > 0, 'first complete timestamp overwritten: ' + fText);
      CheckTrue(Pos('all tasks done +2000.000 ms', fText) > 0, 'first idle timestamp overwritten: ' + fText);
    finally
      fLines.Free;
    end;
  finally
    fPerf.Free;
  end;
end;

initialization
  {$IFDEF FPC}
    RegisterTest('raceperf', TTestRacePerf.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestRacePerf);
  {$ENDIF}
end.
