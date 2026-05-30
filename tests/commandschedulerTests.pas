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
    CheckFalse(fSched.HasDirlist(1, '/test'), 'Should not have dirlist initially');

    fReq.Init(1, '/test', 'TestSite', ctDirlist, 0, 'net', 'chan');
    fSched.ScheduleDirlist(fReq);

    CheckTrue(fSched.HasDirlist(1, '/test'), 'Should have dirlist after schedule');
    CheckFalse(fSched.HasDirlist(1, '/other'), 'Should not have different dir');
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
    CheckFalse(fSched.HasMkdir(1, '/test'), 'Should not have mkdir initially');

    fReq.Init(1, '/test', 'TestSite', ctMkdir, 0, 'net', 'chan');
    fSched.ScheduleMkdir(fReq);

    CheckTrue(fSched.HasMkdir(1, '/test'), 'Should have mkdir after schedule');
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

initialization
  RegisterTest(TTestCommandScheduler.Suite);

end.
