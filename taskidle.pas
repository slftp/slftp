unit taskidle;

interface

uses tasksunit, Math;

type TIdleTask = class(TTask)
  idlecmd: String;
  constructor Create(const netname, channel: String; const site: String);
  function Execute(slot: Pointer): Boolean; override;
  function Name: String; override;
end;

procedure TaskIdleInit;
procedure TaskIdleUninit;

implementation

uses Classes, SysUtils, sitesunit, mystrings, configunit, DebugUnit, irc;

const
  section = 'taskidle';

var
  idlecommands: TStringList;

{ TIdleTask }
constructor TIdleTask.Create(const netname, channel: String; const site: String);
begin
  idlecmd := idlecommands[RandomRange(0, idlecommands.Count - 1)];
  inherited Create(netname, channel, site);
end;

function TIdleTask.Execute(slot: Pointer): Boolean;
label
  ujra;
var
  s: TSiteSlot;
  h: String;
  p: Integer;
  numerrors: Integer;
begin
  Result := False;
  s := slot;
  debugunit.Debug(dpSpam, section, Name);
  numerrors := 0;

ujra:
  inc(numerrors);
  if numerrors > 3 then
  begin
    readyerror := True;
    exit;
  end;

  if s.status <> ssOnline then
  begin
    if not s.ReLogin(1, False, section) then
    begin
      readyerror := True;
      exit;
    end;
  end;

  if s.site.sw = sswDrftpd then
  begin
    // is that really useful ?
    idlecmd := 'CWD .';
  end;

  if (not s.Send(idlecmd)) then goto ujra;
  if (not s.Read(idlecmd)) then goto ujra;

  if ( ( (idlecmd = 'REST 0') and (s.lastResponseCode <> 350) )
     or ( (idlecmd = 'CWD .') and (0 = Pos('250 CWD', s.lastResponse)) and (0 = Pos('250 Directory changed to', s.lastResponse)) )
     or ( (idlecmd = 'PASV') and (not ParsePASVString(s.lastResponse, h, p)) ) )
  then
  begin
    irc_Adderror(Format('<c7>[ERROR idle]</c> %s: %s', [name, s.Name]));
    s.Quit;
  end;

  ready := True;
end;

function TIdleTask.Name: String;
begin
  try
    Result := Format('IDLE <b>%s : %s</b>: %s',[site1, slot1name, idlecmd]);
  except
    Result := 'IDLE';
  end;
end;

procedure TaskIdleInit;
var
  s, ss: String;
  i: Integer;
begin
  idlecommands := TStringList.Create;
  s := config.ReadString(section, 'idlecommands', 'REST 0,STAT -l,PASV,CWD .');
  i := 1;
  while (true) do
  begin
    ss := SubString(s, ',', i);
    if ss = '' then
      Break;
    idlecommands.Add(ss);
    inc(i);
  end;
end;

procedure TaskIdleUninit;
begin
  idlecommands.Free;
end;

end.
