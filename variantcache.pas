unit variantcache;

interface

uses
  SysUtils, SyncObjs,
{$IFDEF FPC}
  fgl, Variants;
{$ELSE}
  System.Generics.Collections, System.Variants;
{$ENDIF}

type
  {$IFDEF FPC}
    TBaseVariantDict = TFPGMap<string, Variant>;
  {$ELSE}
    TBaseVariantDict = TDictionary<string, Variant>;
  {$ENDIF}

  { Allows to cache values with variant types. Currently used to cache sites.dat settings on site level. }
  TVariantCache = class
  private
    FDict: TBaseVariantDict; //< Internal dictionary which stores the data - different for Delphi and FPC
    FLock: TCriticalSection;
    procedure DoSetValue(const aKey: string; const aValue: Variant);
    function DoTryGetValue(const aKey: string; out aValue: Variant): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetValue(const aKey: string; const aValue: Variant);
    function TryGetValue(const aKey: string; out aValue: Variant): Boolean;
    procedure Delete(const aKey: string);
  end;

implementation

{ TVariantCache }

constructor TVariantCache.Create;
begin
  inherited Create;
  FDict := TBaseVariantDict.Create;
  FLock := TCriticalSection.Create;
end;

destructor TVariantCache.Destroy;
begin
  FDict.Free;
  FLock.Free;
  inherited Destroy;
end;

procedure TVariantCache.DoSetValue(const aKey: string; const aValue: Variant);
begin
{$IFDEF FPC}
  FDict.AddOrSetData(aKey, aValue);
{$ELSE}
  FDict.AddOrSetValue(aKey, aValue);
{$ENDIF}
end;

function TVariantCache.DoTryGetValue(const aKey: string; out aValue: Variant): Boolean;
begin
{$IFDEF FPC}
  Result := FDict.TryGetData(aKey, aValue);
{$ELSE}
  Result := FDict.TryGetValue(aKey, aValue);
{$ENDIF}
end;

procedure TVariantCache.SetValue(const aKey: string; const aValue: Variant);
begin
  FLock.Enter;
  try
    self.DoSetValue(aKey, aValue);
  finally
    FLock.Leave;
  end;
end;

function TVariantCache.TryGetValue(const aKey: string; out aValue: Variant): Boolean;
begin
  FLock.Enter;
  try
    Result := self.DoTryGetValue(aKey, aValue);
  finally
    FLock.Leave;
  end;
end;

procedure TVariantCache.Delete(const aKey: string);
begin
  FLock.Enter;
  try
    FDict.Remove(aKey);
  finally
    FLock.Leave;
  end;
end;

end.

