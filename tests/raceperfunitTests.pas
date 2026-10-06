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
    procedure TestRuleTimingFirstPass;
    procedure TestEmptyOutput;
    procedure TestOneShotMarkers;
    procedure TestDirectoryMarkers;
    procedure TestBlockedAssignmentsAndDetection;
    procedure TestIndependentTimelines;
    procedure TestDirlistTaskCounts;
    procedure TestCompleteSources;
    procedure TestSlotReasons;
    procedure TestListingCommandsAndReadd;
    procedure TestDirectoryReadinessAndFtpIssue;
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
    fPerf.MarkStartupStage(rpssParseStarted, 1100000);
    fPerf.MarkStartupStage(rpssCandidatesSorted, 1200000);
    fPerf.MarkStartupLockTiming(rpslCandidateScan, 1100000, 1150000, 1180000);
    fPerf.MarkDirlistCreated('SiteA', '', 'NEWDIR', 1100000);
    fPerf.MarkDirlistCreated('SiteA', '', 'UPDATE', 1200000);
    fPerf.MarkDirlistStarted('SiteA', '', 1300000);
    fPerf.MarkDirlistParsed('SiteA', '', 1500000);
    fPerf.MarkMkdirCreated('SiteA', 'tuzelj', 1600000);
    fPerf.MarkMkdirStarted('SiteA', 1700000);
    fPerf.MarkMkdirDone('SiteA', 1800000);
    fPerf.MarkRaceTaskCreated('SiteA', 1750000, 1100000, 1200000, 1250000, 1300000, 1320000, 3, 25);
    fPerf.MarkRaceTaskCreated('SiteA', 1900000);
    fPerf.MarkRaceTaskDupDropped('SiteA');
    fPerf.MarkRaceTaskDupDropped('SiteA');
    fPerf.MarkTuzeljDone(4000);
    fPerf.MarkTuzeljDone(6000);
    fPerf.MarkRaceAssigned('SiteA', 2000000);
    fPerf.MarkRaceStarted('SiteA', 2100000);
    fPerf.MarkRaceFinished('SiteA', True);
    fPerf.MarkRaceFinished('SiteA', False);
    fPerf.MarkComplete('SiteA', 12000000);
    fPerf.MarkAllTasksIdle(12500000);

    fLines := fPerf.AsStrings;
    try
      CheckEquals(5, fLines.Count, 'expected global, startup, first-race path, site and main directory');
      fText := fLines.Text;

      CheckTrue(Pos('Startup path: task started +300.000 ms | parse started +100.000 ms | candidates sorted +200.000 ms | lock candidate-scan 1x wait 50.000 ms (max 50.000 ms), hold 30.000 ms (max 30.000 ms)', fText) > 0, 'startup lock path missing: ' + fText);
      CheckTrue(Pos('First race path: tuzelj +100.000 ms -> destination +200.000 ms (100.000 ms) -> candidate loop +250.000 ms -> ctor +300.000 ms (20.000 ms) -> ready +750.000 ms (500.000 ms after scan), 3 destinations / 25 entries', fText) > 0, 'correlated first-race path missing: ' + fText);
      CheckTrue(Pos('first dirlist task +100.000 ms', fText) > 0, 'global first dirlist missing: ' + fText);
      CheckTrue(Pos('first race created +750.000 ms', fText) > 0, 'global first race created missing: ' + fText);
      CheckTrue(Pos('all tasks done +11.500 s', fText) > 0, 'global idle missing: ' + fText);
      CheckTrue(Pos('tuzelj 2 calls, total 10.000 ms (avg 5.000 ms)', fText) > 0, 'tuzelj stats missing: ' + fText);

      CheckTrue(Pos('SiteA:', fText) > 0, 'site line missing: ' + fText);
      CheckTrue(Pos('dirlist +100.000 ms via NEWDIR (started +300.000 ms, parsed +500.000 ms, 2 created, 1 executed, 0 dup dropped, 0 err)', fText) > 0, 'dirlist line wrong: ' + fText);
      CheckTrue(Pos('mkdir +600.000 ms via tuzelj -> started +700.000 ms -> done +800.000 ms (queue 100.000 ms, exec 100.000 ms, 0 err)', fText) > 0, 'mkdir missing: ' + fText);
      CheckTrue(Pos('races 2 created (first +750.000 ms)', fText) > 0, 'race count missing: ' + fText);
      CheckTrue(Pos('queue wait 250.000 ms', fText) > 0, 'queue wait missing: ' + fText);
      CheckTrue(Pos('1 ok / 1 err', fText) > 0, 'race results missing: ' + fText);
      CheckTrue(Pos('2 dup dropped', fText) > 0, 'dup dropped missing: ' + fText);
      CheckTrue(Pos('complete +11.000 s', fText) > 0, 'complete missing: ' + fText);
    finally
      fLines.Free;
    end;
  finally
    fPerf.Free;
  end;
end;


procedure TTestRacePerf.TestRuleTimingFirstPass;
var
  fPerf: TRacePerf;
  fLines: TStringList;
  fText: String;
begin
  fPerf := TRacePerf.Create(1000000);
  try
    fPerf.MarkRuleStage(rprsSource, 10, 2, 3, 1010000, 1);
    fPerf.MarkRuleStage(rprsSiteAllow, 100, 10, 60, 1020000, 4);
    fPerf.MarkRuleStage(rprsDestinations, 200, 20, 140, 1030000, 5);
    fPerf.MarkRuleStage(rprsDestinations, 50, 5, 30, 1990000, 2);
    fLines := fPerf.AsStrings;
    try
      fText := fLines.Text;
      CheckTrue(Pos('Rules path: source 0.010 ms, done +10.000 ms (1 calls; kb_lock wait 0.002 ms, hold 0.003 ms) | site-allow 0.100 ms, done +20.000 ms (4 calls; kb_lock wait 0.010 ms, hold 0.060 ms) | destinations 0.200 ms, done +30.000 ms (5 calls; kb_lock wait 0.020 ms, hold 0.140 ms)', fText) > 0, fText);
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
    fPerf.MarkDirlistCreated('SiteA', '', '', 1100000);
    fPerf.MarkDirlistCreated('SiteA', '', '', 9900000);
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

procedure TTestRacePerf.TestDirectoryMarkers;
var
  fPerf: TRacePerf;
  fLines: TStringList;
  fText: String;
begin
  fPerf := TRacePerf.Create(1000000);
  try
    { Insert out of order to verify sorting and keep readd markers one-shot. }
    fPerf.MarkDirlistCreated('SiteA', 'Sample', 'subdir', 1300000);
    fPerf.MarkDirlistCreated('SiteA', '', 'NEWDIR', 1100000);
    fPerf.MarkDirlistCreated('sitea', 'Sample', 'readd', 1900000);
    fPerf.MarkDirlistStarted('SiteA', 'Sample', 1400000);
    fPerf.MarkDirlistParsed('SiteA', 'Sample', 1500000);
    fPerf.MarkDirlistStarted('SiteA', 'Sample', 2000000);
    fPerf.MarkDirlistParsed('SiteA', 'Sample', 2100000);
    fPerf.MarkDirlistError('SiteA', 'Sample');
    fLines := fPerf.AsStrings;
    try
      CheckEquals(5, fLines.Count, 'global, startup, site and two directories');
      CheckTrue(Pos('dir /: created +100.000 ms via NEWDIR', fLines[3]) > 0, fLines.Text);
      CheckTrue(Pos('dir Sample: created +300.000 ms via subdir, started +400.000 ms, parsed +500.000 ms, 2 created, 2 executed, 0 dup dropped, 1 err', fLines[4]) > 0, fLines.Text);
      fText := fLines[2];
      CheckTrue(Pos('3 created, 2 executed, 0 dup dropped, 1 err', fText) > 0, fText);
    finally
      fLines.Free;
    end;
  finally
    fPerf.Free;
  end;
end;

procedure TTestRacePerf.TestBlockedAssignmentsAndDetection;
var
  fPerf: TRacePerf;
  fLines: TStringList;
  fReason: TRacePerfBusyReason;
  i: integer;
begin
  fPerf := TRacePerf.Create(1000000, 'ADDPRE');
  try
    CheckEquals('ADDPRE', fPerf.DetectedInfo);
    CheckEquals(Int64(1000000), fPerf.DetectedUs);
    fPerf.MarkAssignBlockedNoSlot('SiteA', rpsrNoFreeSlot, rprSource);
    fPerf.MarkAssignBlockedNoSlot('sitea', rpsrMaxUp, rprDestination);
    for fReason := Low(TRacePerfBusyReason) to High(TRacePerfBusyReason) do
      for i := 0 to Ord(fReason) do
        fPerf.MarkAssignBlockedBusy('SiteA', fReason);
    fPerf.MarkMkdirCreated('SiteA', 'dirlist550', 1100000);
    fPerf.MarkMkdirCreated('SiteA', 'tuzelj', 1200000);
    fLines := fPerf.AsStrings;
    try
      CheckEquals(3, fLines.Count);
      CheckTrue(Pos('assign blocked 2x no slot / 21x busy', fLines.Text) > 0, fLines.Text);
      CheckTrue(Pos('cooldown up 1 / down 2, destination 3, lock 4, active file 5, reverse file 6', fLines.Text) > 0, fLines.Text);
      CheckTrue(Pos('mkdir +100.000 ms via dirlist550', fLines.Text) > 0, fLines.Text);
    finally
      fLines.Free;
    end;
  finally
    fPerf.Free;
  end;
end;

procedure TTestRacePerf.TestIndependentTimelines;
var
  fFirst, fSecond: TRacePerf;
  fLines: TStringList;
begin
  { Timeout locks require distinct names for releases alive at the same time. }
  fFirst := TRacePerf.Create(1000000);
  try
    fSecond := TRacePerf.Create(2000000);
    try
      fFirst.MarkDirlistCreated('SiteA', '', 'NEWDIR', 1100000);
      fSecond.MarkDirlistCreated('SiteB', '', 'ADDPRE', 2200000);
      fLines := fSecond.AsStrings;
      try
        CheckTrue(Pos('SiteB: dirlist +200.000 ms via ADDPRE', fLines.Text) > 0, fLines.Text);
        CheckEquals(0, Pos('SiteA:', fLines.Text));
      finally
        fLines.Free;
      end;
    finally
      fSecond.Free;
    end;
  finally
    fFirst.Free;
  end;
end;

procedure TTestRacePerf.TestDirlistTaskCounts;
var
  fPerf: TRacePerf;
  fLines: TStringList;
begin
  fPerf := TRacePerf.Create(1000000);
  try
    fPerf.MarkDirlistCreated('SiteA', '', 'NEWDIR', 1100000);
    fPerf.MarkDirlistCreated('SiteA', '', 'readd', 1200000);
    fPerf.MarkDirlistCreated('SiteA', 'Sample', 'subdir', 1300000);
    fPerf.MarkDirlistCreated('SiteA', 'Sample', 'readd', 1400000);
    fPerf.MarkDirlistStarted('SiteA', '', 1500000);
    fPerf.MarkDirlistStarted('SiteA', 'Sample', 1600000);
    fPerf.MarkDirlistDupDropped('sitea', '');
    fPerf.MarkDirlistDupDropped('SiteA', 'Sample');
    fLines := fPerf.AsStrings;
    try
      CheckEquals(5, fLines.Count);
      CheckTrue(Pos('4 created, 2 executed, 2 dup dropped, 0 err', fLines[2]) > 0, fLines.Text);
      CheckTrue(Pos('2 created, 1 executed, 1 dup dropped, 0 err', fLines[3]) > 0, fLines.Text);
      CheckTrue(Pos('2 created, 1 executed, 1 dup dropped, 0 err', fLines[4]) > 0, fLines.Text);
      { Execution does not imply that a nonempty listing was processed. }
      CheckTrue(Pos('parsed -', fLines[2]) > 0, fLines.Text);
    finally
      fLines.Free;
    end;
  finally
    fPerf.Free;
  end;
end;

procedure TTestRacePerf.TestCompleteSources;
var
  fPerf: TRacePerf;
  fLines: TStringList;
begin
  fPerf := TRacePerf.Create(1000000);
  try
    { Preserve both time and source of the first completion, in either order. }
    fPerf.MarkComplete('SiteA', 2000000, 'IRC COMPLETE');
    fPerf.MarkComplete('sitea', 3000000, 'dirlist');
    fPerf.MarkComplete('SiteB', 4000000, 'dirlist');
    fPerf.MarkComplete('SiteB', 5000000, 'IRC COMPLETE');
    fPerf.MarkDirlistCreated('SiteC', '', 'NEWDIR', 1100000);
    fLines := fPerf.AsStrings;
    try
      CheckTrue(Pos('complete +1000.000 ms via IRC COMPLETE', fLines.Text) > 0, fLines.Text);
      CheckTrue(Pos('complete +3000.000 ms via dirlist', fLines.Text) > 0, fLines.Text);
      CheckEquals(0, Pos('complete +2000.000 ms', fLines.Text));
      CheckEquals(0, Pos('complete +4000.000 ms', fLines.Text));
      CheckTrue(Pos('complete -', fLines.Text) > 0, fLines.Text);
      CheckEquals(0, Pos('complete - via', fLines.Text));
    finally
      fLines.Free;
    end;
  finally
    fPerf.Free;
  end;
end;

procedure TTestRacePerf.TestSlotReasons;
var
  fPerf: TRacePerf;
  fLines: TStringList;
  fReason: TRacePerfSlotReason;
  i: integer;
begin
  fPerf := TRacePerf.Create(1000000);
  try
    for fReason := Low(TRacePerfSlotReason) to High(TRacePerfSlotReason) do
    begin
      for i := 0 to Ord(fReason) do
        fPerf.MarkAssignBlockedNoSlot('SiteA', fReason, rprSource);
      for i := Ord(fReason) to 5 do
        fPerf.MarkAssignBlockedNoSlot('sitea', fReason, rprDestination);
    end;
    fPerf.MarkRacePrecheckDrop('SiteA');
    fPerf.MarkRacePrecheckDrop('sitea');
    fLines := fPerf.AsStrings;
    try
      CheckTrue(Pos('42x no slot', fLines.Text) > 0, fLines.Text);
      CheckTrue(Pos('source free/online/up/dn/pre/rip 1/2/3/4/5/6; destination 6/5/4/3/2/1', fLines.Text) > 0, fLines.Text);
      CheckTrue(Pos('2 race allocations avoided', fLines.Text) > 0, fLines.Text);
      CheckEquals(0, Pos('races 2 created', fLines.Text), 'precheck must not count an allocated task');
    finally
      fLines.Free;
    end;
  finally
    fPerf.Free;
  end;
end;

procedure TTestRacePerf.TestListingCommandsAndReadd;
var
  fPerf: TRacePerf;
  fLines: TStringList;
begin
  fPerf := TRacePerf.Create(1000000);
  try
    fPerf.MarkDirlistCommandSent('SiteA', '', False);
    fPerf.MarkDirlistCommandSent('sitea', '', False);
    fPerf.MarkDirlistCommandSent('SiteA', 'Sample', True);
    fPerf.MarkDirlistCommandDone('SiteA', '');
    fPerf.MarkDirlistCommandDone('SiteA', '');
    fPerf.MarkDirlistCommandDone('SiteA', 'Sample');
    { A sequential retry increases commands, but must not increase the peak. }
    fPerf.MarkDirlistCommandSent('SiteA', '', False);
    fPerf.MarkDirlistCommandDone('SiteA', '');
    fPerf.MarkDirlistReadd('SiteA', '', 0, 0);
    fPerf.MarkDirlistReadd('SiteA', '', 0, 1000);
    fPerf.MarkDirlistReadd('SiteA', '', 0, 0);
    fPerf.MarkDirlistReadd('SiteA', 'Sample', 20, 2000);
    fLines := fPerf.AsStrings;
    try
      CheckEquals(4, fLines.Count);
      CheckTrue(Pos('sent STAT 3 / LIST 1, active 0, peak 3', fLines[1]) > 0, fLines.Text);
      CheckTrue(Pos('sent STAT 3 / LIST 0, active 0, peak 2', fLines[2]) > 0, fLines.Text);
      CheckTrue(Pos('readd base 0 ms, selected 0..1000 ms', fLines[2]) > 0, fLines.Text);
      CheckTrue(Pos('sent STAT 0 / LIST 1, active 0, peak 1', fLines[3]) > 0, fLines.Text);
      CheckTrue(Pos('readd base 20 ms, selected 2000..2000 ms', fLines[3]) > 0, fLines.Text);
    finally
      fLines.Free;
    end;
  finally
    fPerf.Free;
  end;
end;

procedure TTestRacePerf.TestDirectoryReadinessAndFtpIssue;
var
  fPerf: TRacePerf;
  fLines: TStringList;
begin
  fPerf := TRacePerf.Create(1000000);
  try
    fPerf.MarkMkdirCreated('SiteA', 'tuzelj', 1100000, '');
    fPerf.MarkMkdirStarted('SiteA', 1200000, '');
    fPerf.MarkDirectoryUsable('SiteA', '', 1250000);
    fPerf.MarkDirectoryUsable('SiteA', '', 1300000);
    fPerf.MarkMkdirDone('SiteA', 1400000, '');
    fPerf.MarkMkdirCreated('SiteA', 'tuzelj', 1500000, 'Sample');
    fPerf.MarkMkdirStarted('SiteA', 1600000, 'Sample');
    fPerf.MarkMkdirError('SiteA', 'Sample');
    fPerf.MarkMkdirReply('SiteA', 'Sample', 'MKD', 550, 'old reply');
    fPerf.MarkMkdirReply('SiteA', 'Sample', 'CWD', 550, 'Denied' + #13#10 + '<b>' + #3 + StringOfChar('x', 200));
    fLines := fPerf.AsStrings;
    try
      CheckEquals(4, fLines.Count, 'FTP text cannot introduce extra lines');
      CheckTrue(Pos('mkdir created +100.000 ms via tuzelj, started +200.000 ms, usable +250.000 ms, processing done +400.000 ms, 0 err', fLines[2]) > 0, fLines.Text);
      CheckTrue(Pos('mkdir created +500.000 ms via tuzelj, started +600.000 ms, usable -, processing done -, 1 err', fLines[3]) > 0, fLines.Text);
      CheckTrue(Pos('last FTP issue CWD 550: Denied  [b] ', fLines[3]) > 0, fLines.Text);
      CheckEquals(0, Pos('old reply', fLines.Text));
      CheckEquals(0, Pos('<b>', fLines.Text));
      CheckEquals(0, Pos(#3, fLines.Text));
      CheckEquals(0, Pos(#13, fLines[3]));
      CheckEquals(0, Pos(#10, fLines[3]));
      CheckEquals(0, Pos(StringOfChar('x', 161), fLines.Text));
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
