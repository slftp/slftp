unit slmasks;

interface

uses
  SyncObjs, DelphiMasks, RegExpr, FLRE;

type
  {
    @abstract(Inbuilt Mask/Regex class)
    Uses TMask for simple wildcard patterns and TFLRE for regex patterns.
    TFLRE is thread-safe (uses per-thread local storage internally),
    so no external locking is required.
  }
  TslMask = class
  private
    FMask: String; //< actual mask used for @link(dm) or @link(flm)
    dm: TMask; //< simple mask (DelphiMasks)
    flm: TFLRE; //< FLRE regex mask (thread-safe)
  public
    { Create an object which uses TMask or TFLRE internally for matching. Regex is identified by use of '/<regex>/' with optional 'i' for case-insensitivity at the end.
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

uses
  SysUtils, debugunit;

const
  ssection = 'slmasks';

{ TslMask }

constructor TslMask.Create(const aMask: String);
var
  fLen: Integer;
  fPattern: String;
  fFlags: TFLREFlags;
begin
  FMask := aMask;
  fLen := Length(aMask);

  if fLen = 0 then
    exit;

  if ((aMask[1] = '/') and (aMask[fLen] = '/')) then
  begin
    fPattern := Copy(aMask, 2, fLen-2);
    flm := TFLRE.Create(RawByteString(fPattern), TFLREFlags([]));
  end
  else
  if ((aMask[1] = '/') and (aMask[fLen-1] = '/') and (aMask[fLen] = 'i')) then
  begin
    fPattern := Copy(aMask, 2, fLen-3);
    flm := TFLRE.Create(RawByteString(fPattern), TFLREFlags([FLRE.rfIGNORECASE]));
  end
  else
    dm := TMask.Create(aMask);
end;

destructor TslMask.Destroy;
begin
  if Assigned(dm) then
    FreeAndNil(dm);

  if Assigned(flm) then
    FreeAndNil(flm);

  inherited;
end;

function TslMask.Matches(const aInput: String): Boolean;
begin
  Result := False;

  if Assigned(dm) then
  begin
    // TMask.Matches is read-only, no lock needed
    Result := dm.Matches(aInput);
  end
  else if Assigned(flm) then
  begin
    // TFLRE is thread-safe (uses per-thread local storage)
    try
      Result := flm.Find(RawByteString(aInput)) <> 0;
    except
      on e: Exception do
        Debug(dpError, ssection, 'FLRE Exception in TslMask.Matches: %s %s', [mask, e.Message]);
    end;
  end;
end;

end.
