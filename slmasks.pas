unit slmasks;

interface

uses delphimasks, RegExpr;

type
  TslMask = class
  private
    fMask: AnsiString;
    dm: TMask;
    rm: TRegExpr;
  public
    function Matches( const s: AnsiString ): Boolean;
    constructor Create( const mask: AnsiString );
    property mask: AnsiString read fMask;
    destructor Destroy; override;
  end;

implementation

uses SysUtils, debugunit;

const ssection = 'slmasks';


{ TslMask }

constructor TslMask.Create(const mask: AnsiString);
var l: Integer;
begin
  fMask := mask;
  l := length(mask);
  if l = 0 then exit;

  if ((mask[1] = '/') and (mask[l]='/')) then
  begin
    rm := TRegExpr.Create;
    rm.Expression := Copy(mask, 2, l-2);
  end
  else
  if ((mask[1] = '/') and (mask[l-1]='/') and (mask[l]='i')) then
  begin
    rm := TRegExpr.Create;
    rm.ModifierI := True;
    rm.Expression := Copy(mask, 2, l-3);
  end
  else
    dm := TMask.Create(mask);
end;

destructor TslMask.Destroy;
begin
  if dm <> nil then
  begin
    dm.Free;
    dm := nil;
  end;
  if rm <> nil then
  begin
    rm.Free;
    rm := nil;
  end;
  inherited;
end;

function TslMask.Matches(const s: AnsiString): Boolean;
begin
  Result := False;
  if dm <> nil then
    Result := dm.Matches(s)
  else
  if rm <> nil then
  begin
    try
      Result := rm.Exec(s)
    except on e: Exception do
      debug(dpError, ssection, 'Regexp exception: %s %s',[fmask, e.Message]);
    end;
  end;
end;

end.
