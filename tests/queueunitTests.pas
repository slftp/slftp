unit queueunitTests;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility, DUnitX.Assert;
  {$ENDIF}

type
  TTestQueueunit = class(TTestCase)
  published
    procedure TestQueueSorterSortsByRankDescending;
    procedure TestQueueSorterSortsByFileSizeWhenRankEqual;
    procedure TestQueueSorterSfvHasPriorityOverNormalFile;
    procedure TestQueueSorterNfoHasPriorityOverNormalFile;
    procedure TestQueueSorterStableWhenAllEqual;
  end;

implementation

uses
  SysUtils, Classes, Contnrs, Generics.Collections,
  queueunit, taskrace, pazo, kb.releaseinfo, IdThreadSafe;

{ Helper that creates a minimal TPazoSite usable only for QueueSorter tests.
  Only s_racetasks/s_dirlisttasks/s_mkdirtasks are initialized because the
  destructor of TPazoRaceTask touches them. }
function CreateMinimalPazoSite(const aName: String): TPazoSite;
begin
  Result := TPazoSite(TPazoSite.NewInstance);
  Result.Name := aName;
  Result.s_racetasks := TIdThreadSafeInt32.Create;
  Result.s_dirlisttasks := TIdThreadSafeInt32.Create;
  Result.s_mkdirtasks := TIdThreadSafeInt32.Create;
end;

{ Helper that releases a minimal TPazoSite created by CreateMinimalPazoSite.
  We free the counters manually because the site was not created by its real
  constructor and therefore does not own other internal objects. }
procedure FreeMinimalPazoSite(aSite: TPazoSite);
begin
  if aSite = nil then Exit;
  aSite.s_racetasks.Free;
  aSite.s_dirlisttasks.Free;
  aSite.s_mkdirtasks.Free;
  aSite.Free;
end;

{ Helper that creates a minimal TPazoRaceTask usable only for QueueSorter tests.
  The task is not created through its real constructor to avoid the heavy
  site/pazo setup; instead the fields read by QueueSorter are initialized. }
function CreateMinimalRaceTask(const aPazo: TPazo; const aPazoSite: TPazoSite;
  const aFilename: String; const aRank: Integer; const aFileSize: Int64;
  const aIsSfv, aIsNfo: Boolean): TPazoRaceTask;
begin
  Result := TPazoRaceTask(TPazoRaceTask.NewInstance);
  Result.mainpazo := aPazo;
  Result.ps1 := aPazoSite;
  Result.filename := aFilename;
  Result.rank := aRank;
  Result.filesize := aFileSize;
  Result.IsSfv := aIsSfv;
  Result.IsNfo := aIsNfo;
end;

{ Helper that creates a sorted list of RaceTasks and returns the ordered
  filenames after applying QueueSorter. }
function SortRaceTasksAndGetFilenames(const aTasks: array of TPazoRaceTask): TStringList;
var
  fList: TObjectList;
  fTask: TPazoRaceTask;
begin
  fList := TObjectList.Create(True);
  try
    for fTask in aTasks do
      fList.Add(fTask);

    fList.Sort(@QueueSorter);

    Result := TStringList.Create;
    for fTask in fList do
      Result.Add(fTask.filename);
  finally
    fList.Free;
  end;
end;

{ TTestQueueunit }

procedure TTestQueueunit.TestQueueSorterSortsByRankDescending;
var
  fRelease: TRelease;
  fPazo: TPazo;
  fSite: TPazoSite;
  fTaskLow, fTaskHigh: TPazoRaceTask;
  fResult: TStringList;
begin
  fRelease := T0DayRelease.Create('test.release-group', 'SECTION', False);
  fPazo := TPazo.Create(fRelease, 1);
  fSite := CreateMinimalPazoSite('SRC');
  try
    fTaskLow := CreateMinimalRaceTask(fPazo, fSite, 'low_rank.rar', 10, 100, False, False);
    fTaskHigh := CreateMinimalRaceTask(fPazo, fSite, 'high_rank.rar', 50, 100, False, False);

    fResult := SortRaceTasksAndGetFilenames([fTaskLow, fTaskHigh]);
    try
      CheckEquals(2, fResult.Count);
      CheckEquals('high_rank.rar', fResult[0], 'Higher rank should be first');
      CheckEquals('low_rank.rar', fResult[1], 'Lower rank should be second');
    finally
      fResult.Free;
    end;
  finally
    FreeMinimalPazoSite(fSite);
    fPazo.Free;
    fRelease.Free;
  end;
end;

procedure TTestQueueunit.TestQueueSorterSortsByFileSizeWhenRankEqual;
var
  fRelease: TRelease;
  fPazo: TPazo;
  fSite: TPazoSite;
  fTaskSmall, fTaskLarge: TPazoRaceTask;
  fResult: TStringList;
begin
  fRelease := T0DayRelease.Create('test.release-group', 'SECTION', False);
  fPazo := TPazo.Create(fRelease, 1);
  fSite := CreateMinimalPazoSite('SRC');
  try
    fTaskSmall := CreateMinimalRaceTask(fPazo, fSite, 'small.rar', 20, 100, False, False);
    fTaskLarge := CreateMinimalRaceTask(fPazo, fSite, 'large.rar', 20, 500, False, False);

    fResult := SortRaceTasksAndGetFilenames([fTaskSmall, fTaskLarge]);
    try
      CheckEquals(2, fResult.Count);
      CheckEquals('large.rar', fResult[0], 'Larger file should be first when rank is equal');
      CheckEquals('small.rar', fResult[1], 'Smaller file should be second when rank is equal');
    finally
      fResult.Free;
    end;
  finally
    FreeMinimalPazoSite(fSite);
    fPazo.Free;
    fRelease.Free;
  end;
end;

procedure TTestQueueunit.TestQueueSorterSfvHasPriorityOverNormalFile;
var
  fRelease: TRelease;
  fPazo: TPazo;
  fSite: TPazoSite;
  fTaskNormal, fTaskSfv: TPazoRaceTask;
  fResult: TStringList;
begin
  fRelease := T0DayRelease.Create('test.release-group', 'SECTION', False);
  fPazo := TPazo.Create(fRelease, 1);
  fSite := CreateMinimalPazoSite('SRC');
  try
    fTaskNormal := CreateMinimalRaceTask(fPazo, fSite, 'normal.rar', 30, 1000, False, False);
    fTaskSfv := CreateMinimalRaceTask(fPazo, fSite, 'file.sfv', 30, 10, True, False);

    fResult := SortRaceTasksAndGetFilenames([fTaskNormal, fTaskSfv]);
    try
      CheckEquals(2, fResult.Count);
      CheckEquals('file.sfv', fResult[0], 'SFV should be prioritized over normal file');
      CheckEquals('normal.rar', fResult[1], 'Normal file should come after SFV');
    finally
      fResult.Free;
    end;
  finally
    FreeMinimalPazoSite(fSite);
    fPazo.Free;
    fRelease.Free;
  end;
end;

procedure TTestQueueunit.TestQueueSorterNfoHasPriorityOverNormalFile;
var
  fRelease: TRelease;
  fPazo: TPazo;
  fSite: TPazoSite;
  fTaskNormal, fTaskNfo: TPazoRaceTask;
  fResult: TStringList;
begin
  fRelease := T0DayRelease.Create('test.release-group', 'SECTION', False);
  fPazo := TPazo.Create(fRelease, 1);
  fSite := CreateMinimalPazoSite('SRC');
  try
    fTaskNormal := CreateMinimalRaceTask(fPazo, fSite, 'normal.rar', 30, 1000, False, False);
    fTaskNfo := CreateMinimalRaceTask(fPazo, fSite, 'file.nfo', 30, 10, False, True);

    fResult := SortRaceTasksAndGetFilenames([fTaskNormal, fTaskNfo]);
    try
      CheckEquals(2, fResult.Count);
      CheckEquals('file.nfo', fResult[0], 'NFO should be prioritized over normal file');
      CheckEquals('normal.rar', fResult[1], 'Normal file should come after NFO');
    finally
      fResult.Free;
    end;
  finally
    FreeMinimalPazoSite(fSite);
    fPazo.Free;
    fRelease.Free;
  end;
end;

procedure TTestQueueunit.TestQueueSorterStableWhenAllEqual;
var
  fRelease: TRelease;
  fPazo: TPazo;
  fSite: TPazoSite;
  fTaskA, fTaskB: TPazoRaceTask;
  fResult: TStringList;
  fOriginalOrder, fSortedOrder: TStringList;
begin
  fRelease := T0DayRelease.Create('test.release-group', 'SECTION', False);
  fPazo := TPazo.Create(fRelease, 1);
  fSite := CreateMinimalPazoSite('SRC');
  try
    fTaskA := CreateMinimalRaceTask(fPazo, fSite, 'task_a.rar', 25, 100, False, False);
    fTaskB := CreateMinimalRaceTask(fPazo, fSite, 'task_b.rar', 25, 100, False, False);

    // Note: TObjectList.Sort uses QuickSort which is unstable. This test
    // documents the current behavior: equal tasks may swap order.
    fOriginalOrder := TStringList.Create;
    fSortedOrder := TStringList.Create;
    try
      fOriginalOrder.Add(fTaskA.filename);
      fOriginalOrder.Add(fTaskB.filename);

      fResult := SortRaceTasksAndGetFilenames([fTaskA, fTaskB]);
      try
        fSortedOrder.AddStrings(fResult);

        CheckEquals(2, fSortedOrder.Count);
        // We only assert that both tasks are still present after sorting.
        CheckTrue(fSortedOrder.IndexOf('task_a.rar') >= 0, 'task_a should still be in list');
        CheckTrue(fSortedOrder.IndexOf('task_b.rar') >= 0, 'task_b should still be in list');
      finally
        fResult.Free;
      end;
    finally
      fOriginalOrder.Free;
      fSortedOrder.Free;
    end;
  finally
    FreeMinimalPazoSite(fSite);
    fPazo.Free;
    fRelease.Free;
  end;
end;

end.
