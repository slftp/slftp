unit tasklogin;

interface

uses tasksunit;

type
  TLoginTask = class(TTask)
  private
    kill: Boolean;
  public
    noannounce: Boolean;
    readd: Boolean; //< @true if called from autobnctest, @false otherwise
    constructor Create(const netname, channel, site: String; kill: Boolean; readd: Boolean);
    function Execute(slot: Pointer): Boolean; override;
    function Name: String; override;
  end;

implementation

uses
  sitesunit, queueunit, dateutils, SysUtils, irc, debugunit;

const
  section = 'login';

{ TLoginTask }

constructor TLoginTask.Create(const netname, channel, site: String; kill: Boolean; readd: Boolean);
begin
  inherited Create(netname, channel, site);
  self.kill := kill;
  self.readd := readd;
end;

function TLoginTask.Execute(slot: Pointer): Boolean;
var
  s: TSiteSlot;
  i: Integer;
  l: TLoginTask;
begin
  Result := False;
  Debug(dpSpam, section, '-->' + Name);
  s := slot;
  i := s.site.AutoBncTestInterval;

  // readd is only true if called by autobnctest
  if readd then
  begin
    // if autobnctest for site is disabled we don't need to go further
    if i = 0 then
    begin
      ready := True;
      Result := True;
      exit;
    end;
  end;

  if not ((s.site.WorkingStatus = sstUp) and (readd)) then
  begin
    // site is not up, we have to try to login
    if ((not readd) or (not s.site.markeddown)) then
    begin
      s.Quit;
      Result := s.ReLogin(1, kill, section);
    end;

    if s.Status = ssOnline then
    begin
      // slot is online
      announce := Format('<b>%s</b>: %s', [s.site.Name, s.bnc]);
    end;
  end;

  if readd then
  begin
    try
      l := TLoginTask.Create(netname, channel, site1, kill, readd);
      l.startat := IncSecond(Now, i);
      l.dontremove := True;
      AddTask(l);
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] TLoginTask.Execute.AddTask: %s', [e.Message]));
      end;
    end;
  end;

  ready := True;
  Debug(dpSpam, section, '<--' + Name);
  Result := True;
end;

function TLoginTask.Name: String;
begin
  Result := '';
  try
    if readd then
    begin
      Result := 'AUTO';
    end;

    Result := Result + Format('LOGIN %s %s', [site1, ScheduleText]);
  except
    Result := 'LOGIN';
  end;
end;

end.
