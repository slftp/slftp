unit slmasks;

interface

uses
  SyncObjs, RegExpr, mormot.core.search;

type
  {
    @abstract(Inbuilt Mask/Regex class with automatic handling of concurrent access)
  }
  TslMask = class
  private
    FLock: TCriticalSection; //< lock to avoid concurrent access
    FMask: String; //< actual mask used for @link(FMaskMatch) or @link(rm)
    FGlobMask: RawUtf8; //< UTF-8 storage referenced by @link(FMaskMatch)
    FMaskMatch: TMatch; //< compiled glob mask
    rm: TRegExpr; //< regex mask
  public
    { Create an object which uses mORMot TMatch or TRegExpr internally for matching. Regex is identified by use of '/<regex>/' with optional 'i' for case-insensitivity at the end.
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
  SysUtils, debugunit, mormot.core.unicode;

const
  ssection = 'slmasks';

{ TslMask }

constructor TslMask.Create(const aMask: String);
var
  fLen: Integer;
begin
  FMask := aMask;
  FLock := TCriticalSection.Create;
  fLen := Length(aMask);

  if fLen = 0 then
    exit;

  if ((aMask[1] = '/') and (aMask[fLen] = '/')) then
  begin
    rm := TRegExpr.Create;
    rm.ModifierI := False;
    rm.Expression := Copy(aMask, 2, fLen-2);
  end
  else
  if ((aMask[1] = '/') and (aMask[fLen-1] = '/') and (aMask[fLen] = 'i')) then
  begin
    rm := TRegExpr.Create;
    rm.ModifierI := True;
    rm.Expression := Copy(aMask, 2, fLen-3);
  end
  else
  begin
    FGlobMask := StringToUtf8(aMask);
    FMaskMatch.Prepare(FGlobMask, True, True);
  end;
end;

destructor TslMask.Destroy;
begin
  if Assigned(rm) then
    FreeAndNil(rm);

  FLock.Free;
  inherited;
end;

function TslMask.Matches(const aInput: String): Boolean;
begin
  Result := False;

  FLock.Enter;
  try
    if Assigned(rm) then
    begin
      try
        Result := rm.Exec(aInput);
      except
        on e: Exception do
          Debug(dpError, ssection, 'RegExpr Exception in TslMask.Matches: %s %s', [mask, e.Message]);
      end;
    end
    else if FGlobMask <> '' then
      Result := FMaskMatch.MatchString(aInput);
  finally
    FLock.Leave;
  end;
end;

end.
