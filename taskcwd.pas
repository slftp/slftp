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
begin
  Result := False;
  s := slot;
  Debug(dpMessage, section, Name);

ujra:
  if s.status <> ssOnline then
  begin
    if not s.ReLogin(1) then
    begin
      readyerror := True;
      exit;
    end;
  end;

  if not s.Cwd(dir, True) then goto ujra;


  // for what is this? never used.
  if s.lastResponseCode = 250 then
  begin
    response := Trim(s.lastResponse);
  end;

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