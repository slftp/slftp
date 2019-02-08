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

{ TRawTask }

constructor TRawTask.Create(const netname, channel, site, dir, cmd: String);
begin
  inherited Create(netname, channel, site);
  self.cmd := cmd;
  self.dir := dir;
end;

function TRawTask.Execute(slot: Pointer): Boolean;
var
  s: TSiteSlot;
  fNumErrors: Integer;

{ Moves into dir if needed, executes @value(cmd) and reads ftpd output afterwards
  @returns(@true on success, @false if a command execution failed) }
  function SuccessfullyExecuted: Boolean;
  begin
    Result := False;

    if dir <> '' then
    begin
      if (not s.Cwd(dir, true)) then
        exit;
    end;
    if (not s.Send(cmd)) then
      exit;
    if (not s.Read(cmd)) then
      exit;

    Result := True;
  end;

begin
  Result := False;
  Debug(dpMessage, section, '-->' + Name);
  s := slot;

  for fNumErrors := 1 to MaxNumberErrors do
  begin
    if s.status <> ssOnline then
    begin
      if not s.ReLogin(1) then
      begin
        readyerror := True;
        exit;
      end;
    end;

    if SuccessfullyExecuted then
      break;
  end;

  if (fNumErrors = MaxNumberErrors) then
  begin
    readyerror := True;
    exit;
  end;

  ido := Now();
  response := s.lastResponse;
  ready := True;
  Debug(dpSpam, section, '<--' + Name);
  Result := True;
end;

function TRawTask.Name: String;
begin
  try
    Result := Format('RAW %s -> %s', [site1, cmd]);
  except
    Result := 'RAW';
  end;
end;

end.