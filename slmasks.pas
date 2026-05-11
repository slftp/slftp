unit slmasks;

interface

uses
  SyncObjs, DelphiMasks, RegExpr;

type
  {
    @abstract(Inbuilt Mask/Regex class with automatic handling of concurrent access)
  }
  TslMask = class
  private
    FLock: TCriticalSection; //< lock to avoid concurrent access for TRegExpr (not thread-safe)
    FMask: String; //< actual mask used for @link(dm) or @link(rm)
    dm: TMask; //< simple mask (thread-safe, no lock needed)
    rm: TRegExpr; //< regex mask (NOT thread-safe, needs lock around Exec)
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

uses
  SysUtils, debugunit;

const
  ssection = 'slmasks';

{ TslMask }

constructor TslMask.Create(const aMask: String);
var
  fLen: Integer;
begin
  FMask := aMask;
  fLen := Length(aMask);

  if fLen = 0 then
    exit;

  if ((aMask[1] = '/') and (aMask[fLen] = '/')) then
  begin
    rm := TRegExpr.Create;
    rm.ModifierI := False;
    rm.Expression := Copy(aMask, 2, fLen-2);
    FLock := TCriticalSection.Create;
  end
  else
  if ((aMask[1] = '/') and (aMask[fLen-1] = '/') and (aMask[fLen] = 'i')) then
  begin
    rm := TRegExpr.Create;
    rm.ModifierI := True;
    rm.Expression := Copy(aMask, 2, fLen-3);
    FLock := TCriticalSection.Create;
  end
  else
    dm := TMask.Create(aMask);
end;

destructor TslMask.Destroy;
begin
  if Assigned(dm) then
  begin
    FreeAndNil(dm);
  end;

  if Assigned(rm) then
  begin
    FreeAndNil(rm);
  end;

  if Assigned(FLock) then
    FLock.Free;

  inherited;
end;

function TslMask.Matches(const aInput: String): Boolean;
begin
  Result := False;

  // TMask is thread-safe for read-only matching → no lock needed
  if Assigned(dm) then
  begin
    Result := dm.Matches(aInput);
    exit;
  end;

  // TRegExpr is NOT thread-safe: Exec() writes internal state (InputString,
  // MatchPos, MatchLen, parser position, backtracking stack). If two threads
  // call Exec() on the same TRegExpr instance concurrently, they corrupt each
  // other's internal state → wrong results or AVs.
  if Assigned(rm) then
  begin
    FLock.Enter;
    try
      try
        Result := rm.Exec(aInput)
      except
        on e: Exception do
          Debug(dpError, ssection, 'RegExpr Exception in TslMask.Matches: %s %s', [mask, e.Message]);
      end;
    finally
      FLock.Leave;
    end;
  end;
end;

end.
