unit taskcwd;

interface

uses tasksunit;

type TCWDTask = class(TTask)
       dir: AnsiString;
       constructor Create(const netname, channel: AnsiString; site, dir: AnsiString);
       function Execute(slot: Pointer): Boolean; override;
       function Name: AnsiString; override;
     end;

implementation

uses sitesunit, SysUtils, mystrings, DebugUnit;

const section = 'cwd';

{ TLoginTask }
constructor TCWDTask.Create(const netname, channel: AnsiString; site, dir: AnsiString);
begin
  self.dir := dir;
  inherited Create(netname, channel, site);
end;

function TCWDTask.Execute(slot: Pointer): Boolean;
label ujra;
var s: TSiteSlot;
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

function TCWDTask.Name: AnsiString;
begin
  try
    Result := format('CWD %s -> %s', [site1, dir]);
  except
    Result := 'CWD';
  end;
end;

end.

