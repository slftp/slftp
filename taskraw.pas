unit taskraw;

interface

uses
  tasksunit;

type
  TRawTask = class(TTask)
    private
      cmd: String;
      dir: String;
    public
      constructor Create(const netname, channel, site, dir, cmd: String);
      function Execute(slot: Pointer): Boolean; override;
      function Name: String; override;
  end;

implementation

uses
  sitesunit, SysUtils, mystrings, DebugUnit;

const
  section = 'raw';

constructor TRawTask.Create(const netname, channel, site, dir, cmd: String);
begin
  self.cmd := cmd;
  self.dir := dir;
  inherited Create(netname, channel, site);
end;

function TRawTask.Execute(slot: Pointer): Boolean;
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
    if not s.ReLogin(1) then
    begin
      readyerror := True;
      exit;
    end;

  if dir <> '' then
    if (not s.Cwd(dir, true)) then goto ujra;

  if (not s.Send(cmd)) then goto ujra;
  if (not s.Read(cmd)) then goto ujra;

  ido := Now();
  response := s.lastResponse;

  Result := True;
  ready := True;
end;

function TRawTask.Name: String;
begin
  try
    Result := Format('RAW <b>%s</b> -> %s', [site1, cmd]);
  except
    Result := 'RAW';
  end;
end;

end.