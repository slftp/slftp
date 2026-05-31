unit commandschedulerTests;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility, DUnitX.Assert;
  {$ENDIF}

type
  TTestCommandScheduler = class(TTestCase)
  published
    procedure TestCreateDestroy;
    procedure TestScheduleDirlist;
    procedure TestScheduleMkdir;
    procedure TestDirlistDeduplication;
    procedure TestMkdirDeduplication;
    procedure TestDirlistPriority;
    procedure TestMkdirPriority;
    procedure TestGetNextDirlistNotReady;
    procedure TestGetNextMkdirWithDependency;
    procedure TestDirlistCap;
    procedure TestCleanup;
    procedure TestRemoveByPazo;
    procedure TestHasDirlist;
    procedure TestHasMkdir;
    procedure TestMixedDirlistAndMkdir;
    procedure TestCompleteDirlist;
    procedure TestCompleteMkdir;
    procedure TestScheduleSfvDownload;
    procedure TestScheduleNfoDownload;
    procedure TestScheduleCwd;
    procedure TestScheduleRaw;
    procedure TestScheduleLogin;
    procedure TestSfvDeduplication;
    procedure TestRawDeduplication;
    procedure TestGetNextCommandPriority;
    procedure TestHasCommand;
    procedure TestCompleteCommand;
    procedure TestOtherCount;
    procedure TestTotalCountWithOther;
    procedure TestRemoveByPazoWithOther;
  end;

implementation

uses
  SysUtils, DateUtils, commandscheduler;

{ TTestCommandScheduler }

procedure TTestCommandScheduler.TestCreateDestroy;
var
  fSched: TCommandScheduler;
begin
  fSched := TCommandScheduler.Create('TestSite');
  try
    CheckEquals(0, fSched.DirlistCount, 'DirlistCount should be 0');
    CheckEquals(0, fSched.MkdirCount, 'MkdirCount should be 0');
    CheckEquals(0, fSched.TotalCount, 'TotalCount should be 0');
  finally
    fSched.Free;
  end;
end;

procedure TTestCommandScheduler.TestScheduleDirlist;
var
  fSched: TCommandScheduler;
  fReq: TCommandRequest;
begin
  fSched := TCommandScheduler.Create('TestSite');
  try
    fReq.Init(1, '/test/dir', 'TestSite', ctDirlist, 0, 'net', 'chan');
    CheckTrue(fSched.ScheduleDirlist(fReq), 'Should schedule dirlist');
    CheckEquals(1, fSched.DirlistCount, 'DirlistCount should be 1');
  finally
    fSched.Free;
  end;
end;

procedure TTestCommandScheduler.TestScheduleMkdir;
var
  fSched: TCommandScheduler;
  fReq: TCommandRequest;
begin
  fSched := TCommandScheduler.Create('TestSite');
  try
    fReq.Init(1, '/test/dir', 'TestSite', ctMkdir, 0, 'net', 'chan');
    CheckTrue(fSched.ScheduleMkdir(fReq), 'Should schedule mkdir');
    CheckEquals(1, fSched.MkdirCount, 'MkdirCount should be 1');
  finally
    fSched.Free;
  end;
end;

procedure TTestCommandScheduler.TestDirlistDeduplication;
var
  fSched: TCommandScheduler;
  fReq1, fReq2: TCommandRequest;
begin
  fSched := TCommandScheduler.Create('TestSite');
  try
    fReq1.Init(1, '/test/dir', 'TestSite', ctDirlist, 0, 'net', 'chan');
    CheckTrue(fSched.ScheduleDirlist(fReq1), 'First should succeed');

    // Same pazo_id, same dir -> duplicate
    fReq2.Init(1, '/test/dir', 'TestSite', ctDirlist, 0, 'net', 'chan');
    CheckFalse(fSched.ScheduleDirlist(fReq2), 'Duplicate should be rejected');
    CheckEquals(1, fSched.DirlistCount, 'DirlistCount should still be 1');

    // Different dir -> ok
    fReq2.Init(1, '/test/dir2', 'TestSite', ctDirlist, 0, 'net', 'chan');
    CheckTrue(fSched.ScheduleDirlist(fReq2), 'Different dir should succeed');
    CheckEquals(2, fSched.DirlistCount, 'DirlistCount should be 2');
  finally
    fSched.Free;
  end;
end;

procedure TTestCommandScheduler.TestMkdirDeduplication;
var
  fSched: TCommandScheduler;
  fReq1, fReq2: TCommandRequest;
begin
  fSched := TCommandScheduler.Create('TestSite');
  try
    fReq1.Init(1, '/test/dir', 'TestSite', ctMkdir, 0, 'net', 'chan');
    CheckTrue(fSched.ScheduleMkdir(fReq1), 'First should succeed');

    fReq2.Init(1, '/test/dir', 'TestSite', ctMkdir, 0, 'net', 'chan');
    CheckFalse(fSched.ScheduleMkdir(fReq2), 'Duplicate should be rejected');
    CheckEquals(1, fSched.MkdirCount, 'MkdirCount should still be 1');
  finally
    fSched.Free;
  end;
end;

procedure TTestCommandScheduler.TestDirlistPriority;
var
  fSched: TCommandScheduler;
  fReq1, fReq2, fReq3, fOut: TCommandRequest;
begin
  fSched := TCommandScheduler.Create('TestSite');
  try
    // Add with different priorities (lower number = higher priority)
    fReq1.Init(1, '/subdir', 'TestSite', ctDirlist, 0, 'net', 'chan');
    fReq1.priority := 5;
    fSched.ScheduleDirlist(fReq1);

    fReq2.Init(1, '/', 'TestSite', ctDirlist, 0, 'net', 'chan');
    fReq2.priority := 0; // root = highest priority
    fSched.ScheduleDirlist(fReq2);

    fReq3.Init(1, '/other', 'TestSite', ctDirlist, 0, 'net', 'chan');
    fReq3.priority := 3;
    fSched.ScheduleDirlist(fReq3);

    // Should get root first (priority 0)
    CheckTrue(fSched.GetNextDirlist(fOut), 'Should get a dirlist');
    CheckEquals('/', fOut.dir, 'Should get root dirlist first');

    // Then /other (priority 3)
    CheckTrue(fSched.GetNextDirlist(fOut), 'Should get second dirlist');
    CheckEquals('/other', fOut.dir, 'Should get /other second');

    // Then /subdir (priority 5)
    CheckTrue(fSched.GetNextDirlist(fOut), 'Should get third dirlist');
    CheckEquals('/subdir', fOut.dir, 'Should get /subdir third');
  finally
    fSched.Free;
  end;
end;

procedure TTestCommandScheduler.TestMkdirPriority;
var
  fSched: TCommandScheduler;
  fReq1, fReq2, fOut: TCommandRequest;
begin
  fSched := TCommandScheduler.Create('TestSite');
  try
    fReq1.Init(1, '/b', 'TestSite', ctMkdir, 0, 'net', 'chan');
    fReq1.priority := 2;
    fSched.ScheduleMkdir(fReq1);

    fReq2.Init(1, '/a', 'TestSite', ctMkdir, 0, 'net', 'chan');
    fReq2.priority := 1;
    fSched.ScheduleMkdir(fReq2);

    CheckTrue(fSched.GetNextMkdir(fOut), 'Should get a mkdir');
    CheckEquals('/a', fOut.dir, 'Should get /a first (priority 1)');
  finally
    fSched.Free;
  end;
end;

procedure TTestCommandScheduler.TestGetNextDirlistNotReady;
var
  fSched: TCommandScheduler;
  fReq, fOut: TCommandRequest;
begin
  fSched := TCommandScheduler.Create('TestSite');
  try
    // Schedule with future startat
    fReq.Init(1, '/test', 'TestSite', ctDirlist, IncSecond(Now(), 10), 'net', 'chan');
    fSched.ScheduleDirlist(fReq);

    CheckFalse(fSched.GetNextDirlist(fOut), 'Should not get dirlist before startat');
  finally
    fSched.Free;
  end;
end;

procedure TTestCommandScheduler.TestGetNextMkdirWithDependency;
var
  fSched: TCommandScheduler;
  fReq, fOut: TCommandRequest;
  fDirList: TDirList;
begin
  fSched := TCommandScheduler.Create('TestSite');
  try
    // Create a dummy dirlist that needs mkdir
    fDirList := TDirList.Create('', nil, nil, '');
    try
      fDirList.need_mkdir := True;
      fDirList.error := False;

      fReq.Init(1, '/test', 'TestSite', ctMkdir, 0, 'net', 'chan', False, False, fDirList);
      fSched.ScheduleMkdir(fReq);

      // Should be blocked because dependency not satisfied
      CheckFalse(fSched.GetNextMkdir(fOut), 'Should not get mkdir with unsatisfied dependency');

      // Now satisfy dependency
      fDirList.need_mkdir := False;
      CheckTrue(fSched.GetNextMkdir(fOut), 'Should get mkdir after dependency satisfied');
    finally
      fDirList.Free;
    end;
  finally
    fSched.Free;
  end;
end;

procedure TTestCommandScheduler.TestDirlistCap;
var
  fSched: TCommandScheduler;
  fReq: TCommandRequest;
  i: Integer;
begin
  fSched := TCommandScheduler.Create('TestSite');
  try
    // Fill up to cap (50)
    for i := 1 to 50 do
    begin
      fReq.Init(1, Format('/dir%d', [i]), 'TestSite', ctDirlist, 0, 'net', 'chan');
      CheckTrue(fSched.ScheduleDirlist(fReq), Format('Should schedule dirlist %d', [i]));
    end;

    // 51st should be rejected
    fReq.Init(1, '/dir51', 'TestSite', ctDirlist, 0, 'net', 'chan');
    CheckFalse(fSched.ScheduleDirlist(fReq), 'Should reject dirlist beyond cap');
    CheckEquals(50, fSched.DirlistCount, 'DirlistCount should be at cap');
  finally
    fSched.Free;
  end;
end;

procedure TTestCommandScheduler.TestCleanup;
var
  fSched: TCommandScheduler;
  fReq: TCommandRequest;
begin
  fSched := TCommandScheduler.Create('TestSite');
  try
    fReq.Init(1, '/test', 'TestSite', ctDirlist, 0, 'net', 'chan');
    fReq.created := IncMinute(Now(), -20); // 20 minutes old
    fSched.ScheduleDirlist(fReq);

    CheckEquals(1, fSched.DirlistCount, 'Should have 1 dirlist');

    fSched.Cleanup(15); // cleanup older than 15 minutes
    CheckEquals(0, fSched.DirlistCount, 'Should be cleaned up');
  finally
    fSched.Free;
  end;
end;

procedure TTestCommandScheduler.TestRemoveByPazo;
var
  fSched: TCommandScheduler;
  fReq: TCommandRequest;
begin
  fSched := TCommandScheduler.Create('TestSite');
  try
    fReq.Init(1, '/test1', 'TestSite', ctDirlist, 0, 'net', 'chan');
    fSched.ScheduleDirlist(fReq);

    fReq.Init(2, '/test2', 'TestSite', ctDirlist, 0, 'net', 'chan');
    fSched.ScheduleDirlist(fReq);

    fReq.Init(1, '/test1', 'TestSite', ctMkdir, 0, 'net', 'chan');
    fSched.ScheduleMkdir(fReq);

    CheckEquals(2, fSched.DirlistCount, 'Should have 2 dirlists');
    CheckEquals(1, fSched.MkdirCount, 'Should have 1 mkdir');

    fSched.RemoveByPazo(1);
    CheckEquals(1, fSched.DirlistCount, 'Should have 1 dirlist after remove');
    CheckEquals(0, fSched.MkdirCount, 'Should have 0 mkdir after remove');
  finally
    fSched.Free;
  end;
end;

procedure TTestCommandScheduler.TestHasDirlist;
var
  fSched: TCommandScheduler;
  fReq: TCommandRequest;
begin
  fSched := TCommandScheduler.Create('TestSite');
  try
    CheckFalse(fSched.HasDirlist(1, '/test', 'TestSite'), 'Should not have dirlist initially');

    fReq.Init(1, '/test', 'TestSite', ctDirlist, 0, 'net', 'chan');
    fSched.ScheduleDirlist(fReq);

    CheckTrue(fSched.HasDirlist(1, '/test', 'TestSite'), 'Should have dirlist after schedule');
    CheckFalse(fSched.HasDirlist(1, '/other', 'TestSite'), 'Should not have different dir');
  finally
    fSched.Free;
  end;
end;

procedure TTestCommandScheduler.TestHasMkdir;
var
  fSched: TCommandScheduler;
  fReq: TCommandRequest;
begin
  fSched := TCommandScheduler.Create('TestSite');
  try
    CheckFalse(fSched.HasMkdir(1, '/test', 'TestSite'), 'Should not have mkdir initially');

    fReq.Init(1, '/test', 'TestSite', ctMkdir, 0, 'net', 'chan');
    fSched.ScheduleMkdir(fReq);

    CheckTrue(fSched.HasMkdir(1, '/test', 'TestSite'), 'Should have mkdir after schedule');
  finally
    fSched.Free;
  end;
end;

procedure TTestCommandScheduler.TestMixedDirlistAndMkdir;
var
  fSched: TCommandScheduler;
  fReq: TCommandRequest;
  fOut: TCommandRequest;
begin
  fSched := TCommandScheduler.Create('TestSite');
  try
    // Add both dirlist and mkdir
    fReq.Init(1, '/test', 'TestSite', ctDirlist, 0, 'net', 'chan');
    fSched.ScheduleDirlist(fReq);

    fReq.Init(1, '/test', 'TestSite', ctMkdir, 0, 'net', 'chan');
    fSched.ScheduleMkdir(fReq);

    CheckEquals(1, fSched.DirlistCount, 'Should have 1 dirlist');
    CheckEquals(1, fSched.MkdirCount, 'Should have 1 mkdir');
    CheckEquals(2, fSched.TotalCount, 'Should have 2 total');

    // GetNextDirlist should only return dirlists
    CheckTrue(fSched.GetNextDirlist(fOut), 'Should get dirlist');
    CheckTrue(fOut.command_type = ctDirlist, 'Should be dirlist type');

    // GetNextMkdir should only return mkdirs
    CheckTrue(fSched.GetNextMkdir(fOut), 'Should get mkdir');
    CheckTrue(fOut.command_type = ctMkdir, 'Should be mkdir type');
  finally
    fSched.Free;
  end;
end;

procedure TTestCommandScheduler.TestCompleteDirlist;
var
  fSched: TCommandScheduler;
  fReq, fOut: TCommandRequest;
begin
  fSched := TCommandScheduler.Create('TestSite');
  try
    fReq.Init(1, '/test', 'TestSite', ctDirlist, 0, 'net', 'chan');
    fSched.ScheduleDirlist(fReq);
    CheckEquals(1, fSched.DirlistCount, 'Should have 1 dirlist');

    // Complete it
    fSched.CompleteDirlist(fReq);
    CheckEquals(0, fSched.DirlistCount, 'Should have 0 dirlists after complete');

    // Should not be able to get it
    CheckFalse(fSched.GetNextDirlist(fOut), 'Should not get completed dirlist');
  finally
    fSched.Free;
  end;
end;

procedure TTestCommandScheduler.TestCompleteMkdir;
var
  fSched: TCommandScheduler;
  fReq, fOut: TCommandRequest;
begin
  fSched := TCommandScheduler.Create('TestSite');
  try
    fReq.Init(1, '/test', 'TestSite', ctMkdir, 0, 'net', 'chan');
    fSched.ScheduleMkdir(fReq);
    CheckEquals(1, fSched.MkdirCount, 'Should have 1 mkdir');

    fSched.CompleteMkdir(fReq);
    CheckEquals(0, fSched.MkdirCount, 'Should have 0 mkdirs after complete');

    CheckFalse(fSched.GetNextMkdir(fOut), 'Should not get completed mkdir');
  finally
    fSched.Free;
  end;
end;

procedure TTestCommandScheduler.TestScheduleSfvDownload;
var
  fSched: TCommandScheduler;
  fReq: TCommandRequest;
begin
  fSched := TCommandScheduler.Create('TestSite');
  try
    fReq.Init(1, '/test/dir', 'TestSite', ctSfvDownload, 0, 'net', 'chan');
    fReq.sfv_filename := 'test.sfv';
    CheckTrue(fSched.ScheduleCommand(fReq), 'Should schedule SFV download');
    CheckEquals(1, fSched.OtherCount, 'OtherCount should be 1');
  finally
    fSched.Free;
  end;
end;

procedure TTestCommandScheduler.TestScheduleNfoDownload;
var
  fSched: TCommandScheduler;
  fReq: TCommandRequest;
begin
  fSched := TCommandScheduler.Create('TestSite');
  try
    fReq.Init(1, '', 'TestSite', ctNfoDownload, 0, 'net', 'chan');
    CheckTrue(fSched.ScheduleCommand(fReq), 'Should schedule NFO download');
    CheckEquals(1, fSched.OtherCount, 'OtherCount should be 1');
  finally
    fSched.Free;
  end;
end;

procedure TTestCommandScheduler.TestScheduleCwd;
var
  fSched: TCommandScheduler;
  fReq: TCommandRequest;
begin
  fSched := TCommandScheduler.Create('TestSite');
  try
    fReq.Init(-1, '/test/dir', 'TestSite', ctCwd, 0, 'net', 'chan');
    CheckTrue(fSched.ScheduleCommand(fReq), 'Should schedule CWD');
    CheckEquals(1, fSched.OtherCount, 'OtherCount should be 1');
  finally
    fSched.Free;
  end;
end;

procedure TTestCommandScheduler.TestScheduleRaw;
var
  fSched: TCommandScheduler;
  fReq: TCommandRequest;
begin
  fSched := TCommandScheduler.Create('TestSite');
  try
    fReq.Init(-1, '/test/dir', 'TestSite', ctRaw, 0, 'net', 'chan');
    fReq.cmd := 'SITE STAT';
    CheckTrue(fSched.ScheduleCommand(fReq), 'Should schedule RAW');
    CheckEquals(1, fSched.OtherCount, 'OtherCount should be 1');
  finally
    fSched.Free;
  end;
end;

procedure TTestCommandScheduler.TestScheduleLogin;
var
  fSched: TCommandScheduler;
  fReq: TCommandRequest;
begin
  fSched := TCommandScheduler.Create('TestSite');
  try
    fReq.Init(-1, '', 'TestSite', ctLogin, 0, 'net', 'chan');
    CheckTrue(fSched.ScheduleCommand(fReq), 'Should schedule login');
    CheckEquals(1, fSched.OtherCount, 'OtherCount should be 1');
  finally
    fSched.Free;
  end;
end;

procedure TTestCommandScheduler.TestSfvDeduplication;
var
  fSched: TCommandScheduler;
  fReq1, fReq2: TCommandRequest;
begin
  fSched := TCommandScheduler.Create('TestSite');
  try
    fReq1.Init(1, '/test/dir', 'TestSite', ctSfvDownload, 0, 'net', 'chan');
    fReq1.sfv_filename := 'test.sfv';
    CheckTrue(fSched.ScheduleCommand(fReq1), 'First SFV should succeed');

    // Same pazo_id, same dir -> duplicate (dedup ignores sfv_filename)
    fReq2.Init(1, '/test/dir', 'TestSite', ctSfvDownload, 0, 'net', 'chan');
    fReq2.sfv_filename := 'other.sfv';
    CheckFalse(fSched.ScheduleCommand(fReq2), 'Duplicate SFV should be rejected');
    CheckEquals(1, fSched.OtherCount, 'OtherCount should still be 1');

    // Different dir -> ok
    fReq2.Init(1, '/test/dir2', 'TestSite', ctSfvDownload, 0, 'net', 'chan');
    fReq2.sfv_filename := 'test.sfv';
    CheckTrue(fSched.ScheduleCommand(fReq2), 'Different dir SFV should succeed');
    CheckEquals(2, fSched.OtherCount, 'OtherCount should be 2');
  finally
    fSched.Free;
  end;
end;

procedure TTestCommandScheduler.TestRawDeduplication;
var
  fSched: TCommandScheduler;
  fReq1, fReq2: TCommandRequest;
begin
  fSched := TCommandScheduler.Create('TestSite');
  try
    fReq1.Init(-1, '/test/dir', 'TestSite', ctRaw, 0, 'net', 'chan');
    fReq1.cmd := 'SITE STAT';
    CheckTrue(fSched.ScheduleCommand(fReq1), 'First RAW should succeed');

    // Same dir, same cmd -> duplicate
    fReq2.Init(-1, '/test/dir', 'TestSite', ctRaw, 0, 'net', 'chan');
    fReq2.cmd := 'SITE STAT';
    CheckFalse(fSched.ScheduleCommand(fReq2), 'Duplicate RAW should be rejected');
    CheckEquals(1, fSched.OtherCount, 'OtherCount should still be 1');

    // Same dir, different cmd -> ok
    fReq2.Init(-1, '/test/dir', 'TestSite', ctRaw, 0, 'net', 'chan');
    fReq2.cmd := 'SITE USER';
    CheckTrue(fSched.ScheduleCommand(fReq2), 'Different cmd RAW should succeed');
    CheckEquals(2, fSched.OtherCount, 'OtherCount should be 2');
  finally
    fSched.Free;
  end;
end;

procedure TTestCommandScheduler.TestGetNextCommandPriority;
var
  fSched: TCommandScheduler;
  fReq1, fReq2, fOut: TCommandRequest;
begin
  fSched := TCommandScheduler.Create('TestSite');
  try
    // Add with different priorities
    fReq1.Init(-1, '/b', 'TestSite', ctCwd, 0, 'net', 'chan');
    fReq1.priority := 2;
    fSched.ScheduleCommand(fReq1);

    fReq2.Init(-1, '/a', 'TestSite', ctCwd, 0, 'net', 'chan');
    fReq2.priority := 1;
    fSched.ScheduleCommand(fReq2);

    CheckTrue(fSched.GetNextCommand(ctCwd, fOut), 'Should get a CWD');
    CheckEquals('/a', fOut.dir, 'Should get /a first (priority 1)');
  finally
    fSched.Free;
  end;
end;

procedure TTestCommandScheduler.TestHasCommand;
var
  fSched: TCommandScheduler;
  fReq: TCommandRequest;
begin
  fSched := TCommandScheduler.Create('TestSite');
  try
    CheckFalse(fSched.HasCommand(ctSfvDownload, 1, '/test'), 'Should not have SFV initially');

    fReq.Init(1, '/test', 'TestSite', ctSfvDownload, 0, 'net', 'chan');
    fReq.sfv_filename := 'test.sfv';
    fSched.ScheduleCommand(fReq);

    CheckTrue(fSched.HasCommand(ctSfvDownload, 1, '/test'), 'Should have SFV after schedule');
    CheckFalse(fSched.HasCommand(ctNfoDownload, 1, '/test'), 'Should not have NFO');
  finally
    fSched.Free;
  end;
end;

procedure TTestCommandScheduler.TestCompleteCommand;
var
  fSched: TCommandScheduler;
  fReq, fOut: TCommandRequest;
begin
  fSched := TCommandScheduler.Create('TestSite');
  try
    fReq.Init(-1, '/test', 'TestSite', ctCwd, 0, 'net', 'chan');
    fSched.ScheduleCommand(fReq);
    CheckEquals(1, fSched.OtherCount, 'Should have 1 other');

    fSched.CompleteCommand(fReq);
    CheckEquals(0, fSched.OtherCount, 'Should have 0 others after complete');

    CheckFalse(fSched.GetNextCommand(ctCwd, fOut), 'Should not get completed CWD');
  finally
    fSched.Free;
  end;
end;

procedure TTestCommandScheduler.TestOtherCount;
var
  fSched: TCommandScheduler;
  fReq: TCommandRequest;
begin
  fSched := TCommandScheduler.Create('TestSite');
  try
    CheckEquals(0, fSched.OtherCount, 'OtherCount should be 0 initially');

    fReq.Init(-1, '/test', 'TestSite', ctCwd, 0, 'net', 'chan');
    fSched.ScheduleCommand(fReq);
    CheckEquals(1, fSched.OtherCount, 'OtherCount should be 1');

    fReq.Init(-1, '/test2', 'TestSite', ctRaw, 0, 'net', 'chan');
    fReq.cmd := 'SITE STAT';
    fSched.ScheduleCommand(fReq);
    CheckEquals(2, fSched.OtherCount, 'OtherCount should be 2');
  finally
    fSched.Free;
  end;
end;

procedure TTestCommandScheduler.TestTotalCountWithOther;
var
  fSched: TCommandScheduler;
  fReq: TCommandRequest;
begin
  fSched := TCommandScheduler.Create('TestSite');
  try
    fReq.Init(1, '/test', 'TestSite', ctDirlist, 0, 'net', 'chan');
    fSched.ScheduleDirlist(fReq);

    fReq.Init(1, '/test', 'TestSite', ctMkdir, 0, 'net', 'chan');
    fSched.ScheduleMkdir(fReq);

    fReq.Init(-1, '/test', 'TestSite', ctCwd, 0, 'net', 'chan');
    fSched.ScheduleCommand(fReq);

    CheckEquals(1, fSched.DirlistCount, 'DirlistCount should be 1');
    CheckEquals(1, fSched.MkdirCount, 'MkdirCount should be 1');
    CheckEquals(1, fSched.OtherCount, 'OtherCount should be 1');
    CheckEquals(3, fSched.TotalCount, 'TotalCount should be 3');
  finally
    fSched.Free;
  end;
end;

procedure TTestCommandScheduler.TestRemoveByPazoWithOther;
var
  fSched: TCommandScheduler;
  fReq: TCommandRequest;
begin
  fSched := TCommandScheduler.Create('TestSite');
  try
    fReq.Init(1, '/test1', 'TestSite', ctDirlist, 0, 'net', 'chan');
    fSched.ScheduleDirlist(fReq);

    fReq.Init(1, '/test1', 'TestSite', ctMkdir, 0, 'net', 'chan');
    fSched.ScheduleMkdir(fReq);

    fReq.Init(1, '/test1', 'TestSite', ctSfvDownload, 0, 'net', 'chan');
    fReq.sfv_filename := 'test.sfv';
    fSched.ScheduleCommand(fReq);

    fReq.Init(2, '/test2', 'TestSite', ctSfvDownload, 0, 'net', 'chan');
    fReq.sfv_filename := 'test2.sfv';
    fSched.ScheduleCommand(fReq);

    CheckEquals(1, fSched.DirlistCount, 'Should have 1 dirlist');
    CheckEquals(1, fSched.MkdirCount, 'Should have 1 mkdir');
    CheckEquals(2, fSched.OtherCount, 'Should have 2 others');

    fSched.RemoveByPazo(1);
    CheckEquals(0, fSched.DirlistCount, 'Should have 0 dirlists after remove');
    CheckEquals(0, fSched.MkdirCount, 'Should have 0 mkdirs after remove');
    CheckEquals(1, fSched.OtherCount, 'Should have 1 other after remove');
  finally
    fSched.Free;
  end;
end;

initialization
  RegisterTest(TTestCommandScheduler.Suite);

end.
