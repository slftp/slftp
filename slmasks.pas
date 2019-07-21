unit slmasks;

interface

uses
  // TODO: Replace delphimasks file with Masks file from Delphi Rio when FPC has fixed the issues with it...
  {$IFDEF FPC}delphimasks{$ELSE}Masks{$ENDIF}, SyncObjs, RegExpr;

type
  {
    @abstract(Inbuilt Mask/Regex class with automatic handling of concurrent access)
  }
  TslMask = class
  private
    FLock: TCriticalSection; //< lock to avoid concurrent access
    FMask: String; //< actual mask used for @link(dm) or @link(rm)
    dm: TMask; //< simple mask
    rm: TRegExpr; //< regex mask
  public
    function Matches(const s: String): Boolean;
    constructor Create(const mask: String);
    property mask: String read FMask;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils, debugunit;

const
  ssection = 'slmasks';

{ TslMask }

constructor TslMask.Create(const mask: String);
var
  l: Integer;
begin
  FMask := mask;
  l := Length(mask);

  if l = 0 then
    exit;

  if ((mask[1] = '/') and (mask[l] = '/')) then
  begin
    rm := TRegExpr.Create;
    rm.Expression := Copy(mask, 2, l-2);
  end
  else
  if ((mask[1] = '/') and (mask[l-1] = '/') and (mask[l] = 'i')) then
  begin
    rm := TRegExpr.Create;
    rm.ModifierI := True;
    rm.Expression := Copy(mask, 2, l-3);
  end
  else
    dm := TMask.Create(mask);

  FLock := TCriticalSection.Create;
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

  FLock.Free;

  inherited;
end;

function TslMask.Matches(const s: String): Boolean;
begin
  Result := False;

  FLock.Enter;
  try
    if Assigned(dm) then
      Result := dm.Matches(s)
    else if Assigned(rm) then
    begin
      try
        Result := rm.Exec(s)
      except on e: Exception do
        debug(dpError, ssection, 'RegExpr Exception in TslMask.Matches: %s %s', [mask, e.Message]);
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

end.
