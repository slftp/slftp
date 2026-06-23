unit taskrules;

interface

uses
  Classes, tasksunit;

type
  TRulesTask = class(TTask)
    constructor Create(const netname, channel: String; site: String);
    function Execute(slot: Pointer): Boolean; override;
    function Name: String; override;
  end;

implementation

uses
  sitesunit, SysUtils, DateUtils, DebugUnit, queueunit,
  cbftpclient;

const
  section = 'taskrules';

{ TRulesTask }

constructor TRulesTask.Create(const netname, channel: String; site: String);
begin
  inherited Create(netname, channel, site);
end;

function TRulesTask.Execute(slot: Pointer): Boolean;
label
  ujra;
var
  s: TSiteSlot;
  cmd: String;
  t: TRulesTask;
  i: Integer;
  rules: String;
  numerrors: Integer;
begin
  Result := False;
  numerrors := 0;
  s := slot;
  Debug(dpMessage, section, Name);

  // In cbftp mode SITE RULES is fetched by slftp via cbftp REST (TSite.AutoRulesSync
  // driven by Main_Iter). A native FTP SITE RULES leech would be redundant and
  // would only add queue noise.
  if IsCbftpMode then
  begin
    ready := True;
    Result := True;
    Exit;
  end;

ujra:
  inc(numerrors);
  if numerrors > 3 then
  begin
    readyerror := True;
    exit;
  end;

  if s.status <> ssOnline then
  begin
    if not s.ReLogin(1) then
    begin
      readyerror:= True;
      exit;
    end;
  end;

  cmd := 'SITE RULES';

  if (s.site.sslfxp = srNeeded) then
  begin
    if not s.SendProtP() then goto ujra;
  end
  else
  begin
    if not s.SendProtC() then goto ujra;
  end;

  if (not s.Send(cmd)) then
  begin
    readyerror := True;
    exit;
  end;

  if (not s.Read(cmd, true, true, 60000)) then
  begin
    readyerror := True;
    exit;
  end;

  if (s.lastResponseCode <> 200) then
  begin
    readyerror := True;
    exit;
  end;

  rules := s.lastResponse;

  if not s.site.ProcessSiteRulesDiff(rules) then
  begin
    readyerror := True;
    exit;
  end;

  // re add
  i := s.site.AutoRulesStatus;
  if i > 0 then
  begin
    try
      t := TRulesTask.Create(netname, channel, site1);
      t.startat := IncSecond(Now, i);
      t.dontremove := True;
      AddTask(t);
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] TRulesTask.Execute AddTask: %s', [e.Message]));
      end;
    end;
  end;

  Result := True;
  ready := True;
end;

function TRulesTask.Name: String;
begin
  Result := 'AUTORULES ' + ScheduleText;
end;

end.

