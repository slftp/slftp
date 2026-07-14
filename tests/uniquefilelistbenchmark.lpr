program uniquefilelistbenchmark;

{$MODE Delphi}
{$APPTYPE CONSOLE}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF CPUX86_64}
  mormot.core.fpcx64mm,
  {$ENDIF}
  SysUtils, Classes, DateUtils, Generics.Collections,
  mormot.core.base, mormot.core.collections;

const
  NUM_KEYS = 100000;
  NUM_LOOKUPS = 500000;

procedure BenchmarkTDictionary;
var
  fDict: TDictionary<String, Int64>;
  fKey: String;
  fValue: Int64;
  fStart, fStop: TDateTime;
  fI, fIdx: Integer;
  fFound: Integer;
begin
  fDict := TDictionary<String, Int64>.Create;
  try
    WriteLn('=== TDictionary<String, Int64> ===');

    // Fill
    fStart := Now;
    for fI := 0 to NUM_KEYS - 1 do
    begin
      fKey := Format('/Sample/dir%d/file%08d.rar', [fI mod 100, fI]);
      fDict.AddOrSetValue(fKey, Int64(fI) * 1024);
    end;
    fStop := Now;
    WriteLn(Format('Fill %d items: %d ms', [NUM_KEYS, MilliSecondsBetween(fStop, fStart)]));

    // Lookups (50% existing, 50% missing)
    fStart := Now;
    fFound := 0;
    for fI := 0 to NUM_LOOKUPS - 1 do
    begin
      if (fI and 1) = 0 then
        fIdx := fI mod NUM_KEYS
      else
        fIdx := NUM_KEYS + (fI mod NUM_KEYS);
      fKey := Format('/Sample/dir%d/file%08d.rar', [fIdx mod 100, fIdx]);
      if fDict.TryGetValue(fKey, fValue) then
        Inc(fFound);
    end;
    fStop := Now;
    WriteLn(Format('Lookup %d items (%d found): %d ms', [NUM_LOOKUPS, fFound, MilliSecondsBetween(fStop, fStart)]));

    // Update existing
    fStart := Now;
    for fI := 0 to NUM_KEYS - 1 do
    begin
      fKey := Format('/Sample/dir%d/file%08d.rar', [fI mod 100, fI]);
      fDict.AddOrSetValue(fKey, Int64(fI) * 2048);
    end;
    fStop := Now;
    WriteLn(Format('Update %d items: %d ms', [NUM_KEYS, MilliSecondsBetween(fStop, fStart)]));

    // Clear
    fStart := Now;
    fDict.Clear;
    fStop := Now;
    WriteLn(Format('Clear: %d ms', [MilliSecondsBetween(fStop, fStart)]));
  finally
    fDict.Free;
  end;
end;

procedure BenchmarkMormotKeyValue;
var
  fKV: IKeyValue<RawUTF8, Int64>;
  fKey: RawUTF8;
  fValue: Int64;
  fStart, fStop: TDateTime;
  fI, fIdx: Integer;
  fFound: Integer;
begin
  fKV := Collections.NewKeyValue<RawUTF8, Int64>;

  WriteLn('=== IKeyValue<RawUTF8, Int64> (mORMot2) ===');

  // Fill
  fStart := Now;
  for fI := 0 to NUM_KEYS - 1 do
  begin
    fKey := RawUTF8(Format('/Sample/dir%d/file%08d.rar', [fI mod 100, fI]));
    fKV.Items[fKey] := Int64(fI) * 1024;
  end;
  fStop := Now;
  WriteLn(Format('Fill %d items: %d ms', [NUM_KEYS, MilliSecondsBetween(fStop, fStart)]));

  // Lookups (50% existing, 50% missing)
  fStart := Now;
  fFound := 0;
  for fI := 0 to NUM_LOOKUPS - 1 do
  begin
    if (fI and 1) = 0 then
      fIdx := fI mod NUM_KEYS
    else
      fIdx := NUM_KEYS + (fI mod NUM_KEYS);
    fKey := RawUTF8(Format('/Sample/dir%d/file%08d.rar', [fIdx mod 100, fIdx]));
    if fKV.TryGetValue(fKey, fValue) then
      Inc(fFound);
  end;
  fStop := Now;
  WriteLn(Format('Lookup %d items (%d found): %d ms', [NUM_LOOKUPS, fFound, MilliSecondsBetween(fStop, fStart)]));

  // Update existing
  fStart := Now;
  for fI := 0 to NUM_KEYS - 1 do
  begin
    fKey := RawUTF8(Format('/Sample/dir%d/file%08d.rar', [fI mod 100, fI]));
    fKV.Items[fKey] := Int64(fI) * 2048;
  end;
  fStop := Now;
  WriteLn(Format('Update %d items: %d ms', [NUM_KEYS, MilliSecondsBetween(fStop, fStart)]));

  // Clear
  fStart := Now;
  fKV.Clear;
  fStop := Now;
  WriteLn(Format('Clear: %d ms', [MilliSecondsBetween(fStop, fStart)]));
end;

begin
  WriteLn(Format('Benchmark: %d keys, %d lookups', [NUM_KEYS, NUM_LOOKUPS]));
  WriteLn('');

  BenchmarkTDictionary;
  WriteLn('');
  BenchmarkMormotKeyValue;

  WriteLn('');
  WriteLn('Done.');
end.
