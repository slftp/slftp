unit tasklogin;

interface

uses tasksunit;

type
  TLoginTask = class(TTask)
    private
      kill: Boolean;
    public
      noannounce: Boolean;
      readd: Boolean; // used for autobnctest
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
  self.kill := kill;
  self.readd := readd;
  inherited Create(netname, channel, site);
end;

function TLoginTask.Execute(slot: Pointer): Boolean;
label
  autobnctest;
var
  s: TSiteSlot;
  i: Integer;
  l: TLoginTask;
begin
  Result := False;
  s := slot;
  debugunit.Debug(dpSpam, section, '-->' + Name);

  if readd then
  begin
    // readd is for autobnctest - if autobnctest for site is disabled we don't need to go further
    if s.RCInteger('autobnctest', 0) = 0 then
    begin
      ready := True;
      Result := True;
      exit;
    end;
  end;

  if ((s.site.working = sstUp) and (readd)) then
  begin
    // site is up, we have to try to login
    goto autobnctest;
  end;

  try
    if ((not readd) or (not s.site.markeddown)) then
    begin
      s.Quit;
      Result := s.ReLogin(1, kill, section);
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TLoginTask.s.ReLogin: %s', [e.Message]));
    end;
  end;

  if s.Status = ssOnline then
  begin
    announce := Format('<b>%s</b>: %s',[s.site.name, s.bnc]);
  end;

autobnctest:
  if readd then
  begin
{
 we check this above and if s.RCInteger('autobnctest', 0) = 0 we exit!
 if autobnctest for site is disabled we don't need to go further
       -------------------------------------------------
  generates an high CPU load when a sites has autobnctest value = 0 
}

    i := s.RCInteger('autobnctest', 0);
    if i > 0 then
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
  end;

  debugunit.Debug(dpSpam, section, '<--' + Name);
  ready := True;
end;

function TLoginTask.Name: String;
begin
  Result := '';
  try
    if readd then
    begin
      Result := 'AUTO';
    end;

    Result := Result + Format('LOGIN <b>%s</b> %s', [site1, ScheduleText]);
  except
    Result := 'LOGIN';
  end;
end;

end.
