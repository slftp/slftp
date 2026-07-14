unit slmasks;

interface

uses
  SysUtils, SyncObjs, RegExpr, mormot.core.base, mormot.core.search, debugunit;

type
  {
    @abstract(Inbuilt Mask/Regex class with automatic handling of concurrent access)
    Uses mORMot2 TMatch for GLOB patterns (lock-free, fast) and TRegExpr for regex masks.
  }
  TslMask = class
  private
    FMask: String; //< actual mask used for @link(FMatchStore) or @link(rm)
    FIsRegex: Boolean; //< @true if @link(rm) is used, @false if @link(FMatchStore) is used
    FLock: TCriticalSection; //< lock only used for regex to avoid concurrent access
    FMatchStore: TMatchStore; //< mORMot2 TMatch state for GLOB masks (owns pattern copy)
    rm: TRegExpr; //< regex mask
  public
    { Create an object which uses TMask or TRegExpr internally for matching. Regex is identified by use of '/<regex>/' with optional 'i' for case-insensitivity at the end.
      @param(aMask String which is used to create the appropriate mask/regex) }
    constructor Create(const aMask: String);
    { Free the object and all it's internal data }
    destructor Destroy; override;
    { Tests if the Input matches the actual used mask
      @param(aInput String which should be tested against the used mask)
      @returns(@true if mask matches input, @false otherwise) }
    function Matches(const aInput: String): Boolean;

    property mask: String read FMask;
  end;

implementation

const
  ssection = 'slmasks';

{ TslMask }

constructor TslMask.Create(const aMask: String);
var
  fLen: Integer;
begin
  FMask := aMask;
  fLen := Length(aMask);
  FIsRegex := False;

  if fLen = 0 then
    exit;

  if ((aMask[1] = '/') and (aMask[fLen] = '/')) then
  begin
    FIsRegex := True;
    rm := TRegExpr.Create;
    rm.ModifierI := False;
    rm.Expression := Copy(aMask, 2, fLen - 2);
  end
  else
  if ((aMask[1] = '/') and (aMask[fLen - 1] = '/') and (aMask[fLen] = 'i')) then
  begin
    FIsRegex := True;
    rm := TRegExpr.Create;
    rm.ModifierI := True;
    rm.Expression := Copy(aMask, 2, fLen - 3);
  end
  else
  begin
    // GLOB: use mORMot2 TMatch (lock-free, no heap allocation during match)
    FMatchStore.PatternInstance := RawUtf8(aMask);
    FMatchStore.Pattern.Prepare(
      PUtf8Char(FMatchStore.PatternInstance),
      Length(FMatchStore.PatternInstance),
      True,  // case-insensitive, same as DelphiMasks.TMask behavior
      False);
    exit;
  end;

  FLock := TCriticalSection.Create;
end;

destructor TslMask.Destroy;
begin
  if Assigned(rm) then
    FreeAndNil(rm);

  if Assigned(FLock) then
    FLock.Free;

  inherited;
end;

function TslMask.Matches(const aInput: String): Boolean;
begin
  Result := False;

  if FIsRegex then
  begin
    FLock.Enter;
    try
      try
        Result := rm.Exec(aInput);
      except
        on e: Exception do
          Debug(dpError, ssection, 'RegExpr Exception in TslMask.Matches: %s %s', [mask, e.Message]);
      end;
    finally
      FLock.Leave;
    end;
  end
  else
  begin
    Result := FMatchStore.Pattern.MatchString(aInput);
  end;
end;

end.
