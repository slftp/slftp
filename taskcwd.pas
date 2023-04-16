unit taskcwd;

interface

uses
  tasksunit;

type
  TCWDTask = class(TTask)
  private
    dir: String;
  public
    constructor Create(const netname, channel, site, dir: String);
    function Execute(slot: Pointer): Boolean; override;
    function Name: String; override;
  end;

implementation

uses
  sitesunit, SysUtils, DebugUnit;

const
  section = 'cwd';

{ TCWDTask }

constructor TCWDTask.Create(const netname, channel, site, dir: String);
begin
  inherited Create(netname, channel, site);
  self.dir := dir;
end;

function TCWDTask.Execute(slot: Pointer): Boolean;
label
  ujra;
var
  s: TSiteSlot;
  fNumTries: integer;
begin
  Result := False;
  s := slot;
  fNumTries := 0;
  response := IntToStr(Ord(False));
  Debug(dpMessage, section, Name);

ujra:
  fNumTries := fNumTries + 1;
  if s.status <> ssOnline then
  begin
    if not s.ReLogin(1) then
    begin
      readyerror := True;
      exit;
    end;
  end;

  if not s.Cwd(dir, True) then
  begin
    if fNumTries > 2 then
    begin
      readyerror := True;
      exit;
    end;
    goto ujra;
  end;

  //if we reach this, CWD has been successful
  response := IntToStr(Ord(True));
  ready := True;
end;

function TCWDTask.Name: String;
begin
  try
    Result := format('CWD %s -> %s', [site1, dir]);
  except
    Result := 'CWD';
  end;
end;

end.