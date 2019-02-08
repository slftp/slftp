unit taskidle;

interface

uses
  tasksunit, Math;

type
  TIdleTask = class(TTask)
  private
    idlecmd: String;
  public
    constructor Create(const netname, channel, site: String);
    function Execute(slot: Pointer): Boolean; override;
    function Name: String; override;
  end;

procedure TaskIdleInit;
procedure TaskIdleUninit;

implementation

uses
  Classes, SysUtils, sitesunit, mystrings, configunit, DebugUnit, irc;

const
  section = 'taskidle';

var
  idlecommands: TStringList;

{ TIdleTask }

constructor TIdleTask.Create(const netname, channel, site: String);
begin
  inherited Create(netname, channel, site);
  idlecmd := idlecommands[RandomRange(0, idlecommands.Count - 1)];
end;

function TIdleTask.Execute(slot: Pointer): Boolean;
var
  s: TSiteSlot;
  h: String;
  p: Integer;
  fNumErrors: Integer;

{ Executes @value(idlecmd) and reads ftpd output afterwards
  @returns(@true on success, @false if a command execution failed) }
  function SuccessfullyExecuted: Boolean;
  begin
    Result := False;

    if (not s.Send(idlecmd)) then
      exit;
    if (not s.Read(idlecmd)) then
      exit;

    if ( ((idlecmd = 'REST 0') and (s.lastResponseCode <> 350))
      or ((idlecmd = 'CWD .') and (0 = Pos('250 CWD', s.lastResponse)) and (0 = Pos('250 Directory changed to', s.lastResponse)))
      or ((idlecmd = 'PASV') and (not ParsePASVString(s.lastResponse, h, p))) )
    then
    begin
      irc_Adderror(Format('<c7>[ERROR idle]</c> %s: %s', [Name, s.Name]));
      s.Quit;
    end;

    Result := True;
  end;

begin
  Result := False;
  Debug(dpSpam, section, '-->' + Name);
  s := slot;

  if s.site.sw = sswDrftpd then
  begin
    // is that really useful ?
    idlecmd := 'CWD .';
  end;

  for fNumErrors := 1 to MaxNumberErrors do
  begin
    if s.status <> ssOnline then
    begin
      if not s.ReLogin(1, False, section) then
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

  ready := True;
  Debug(dpSpam, section, '<--' + Name);
  Result := True;
end;

function TIdleTask.Name: String;
begin
  try
    Result := Format('IDLE %s : %s : %s', [site1, slot1name, idlecmd]);
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