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
  sitesunit, SysUtils, mystrings, DebugUnit, cbftpclient, mormot.core.unicode;

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
  jsonBody: String;
  jsonResp: String;

{ Moves into dir if needed, executes @link(cmd) and reads ftpd output afterwards
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
  Result := True;
  if GlCbftpClient <> nil then
  begin
    jsonBody := '{"sites":["' + StringToUtf8(site1) + '"],"command":"' + StringToUtf8(cmd) + '"';
    if dir <> '' then
      jsonBody := jsonBody + ',"path_section":"' + StringToUtf8(dir) + '"';
    jsonBody := jsonBody + '}';

    try
      jsonResp := string(GlCbftpClient.SendRawCommand(StringToUtf8(jsonBody)));
      Debug(dpMessage, section, Format('[cbftp] TRawTask executed: %s, response: %s', [Name, jsonResp]));
      ready := True;
      readyerror := False;
      Result := True;
      Exit;
    except
      on E: Exception do
      begin
        DebugException(dpError, section, Format('[cbftp] TRawTask failed: %s', [Name]), E);
        ready := True;
        readyerror := True;
        Result := False;
        Exit;
      end;
    end;
  end;

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

  time := Now();
  response := s.lastResponse;
  ready := True;
  Debug(dpSpam, section, '<--' + Name);
  Result := True;
end;

function TRawTask.Name: String;
begin
  try
    Result := Format('RAW %s (%s, %s)', [site1, dir, cmd]);
  except
    Result := 'RAW';
  end;
end;

end.
