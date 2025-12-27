unit variantCacheTests;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestVariantCache = class(TTestCase)
  published
    procedure TestBasicFunction;
  end;

implementation

uses
  SysUtils, variantcache;

{ TTestTags }

procedure TTestVariantCache.TestBasicFunction;
var
  fCache: TVariantCache;
  fVariant: Variant;
begin
  fCache := TVariantCache.Create;
  try
    // Test Set/Get
    fCache.SetValue('Name', 'Alice');
    fCache.SetValue('Age', 42);
    fCache.SetValue('Active', True);

    // Test TryGetValue (existing)
    if not fCache.TryGetValue('Age', fVariant) or (fVariant <> 42) then
      raise Exception.Create('TryGetValue failed for "Age"');

    // Test TryGetValue (missing)
    if fCache.TryGetValue('MissingKey', fVariant) then
      raise Exception.Create('TryGetValue unexpectedly succeeded for "MissingKey"');

    // Test Delete
    fCache.Delete('Age');
    if fCache.TryGetValue('Age', fVariant) then
      raise Exception.Create('Delete failed - key "Age" still exists');

    // if we reach this point, everything worked. Added this to remove warning "No checks executed in TestCase" on FPC
    CheckEquals(0, 0);

  finally
    fCache.Free;
  end;
end;

initialization
  {$IFDEF FPC}
    RegisterTest('variantCache', TTestVariantCache.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestVariantCache);
  {$ENDIF}
end.
