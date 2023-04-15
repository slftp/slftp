unit tasklogin;

interface

uses tasksunit;

type
  TLoginTask = class(TTask)
  public
    noannounce: Boolean;
    readd: Boolean; //< @true if called from autobnctest, @false otherwise
    kill: Boolean;
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
  fOriginalSlotName: string;
begin
  Result := False;
  Debug(dpSpam, section, '-->' + Name);
  s := slot;
  fOriginalSlotName := s.Name;
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

  if self.wantedslot = '' then
  begin
    if not readd or (not(s.site.WorkingStatus in [sstMarkedAsDownByUser, sstUp])) then
    begin
      if (s.Status <> ssOnline) then
      begin
        // site is not up, we have to try to login
        s.Quit;
        Result := s.ReLogin(1, kill, section);

        if readd and (s.Status = ssOnline) then
        begin
          // slot is online
          announce := Format('<b>%s</b>: %s', [s.site.Name, s.bnc]);
        end;
      end;

      //check all slots if this is not the bnc check. if it's the bnc check and the site might also have an idle
      //timeout set, we don't want to login all the slots
      if not readd and (s.Status = ssOnline) then
      begin
        for s in s.site.slots do
        begin
          if (s.Status <> ssOnline) and (s.Name <> fOriginalSlotName) then
          begin
            l := TLoginTask.Create(netname, channel, site1, False, False);
            l.wantedslot := s.Name;
            AddTask(l);
          end;
        end;
      end;
    end;
  end
  else
  begin
    if (s.Status <> ssOnline) or not(s.site.WorkingStatus in [sstUp, sstMarkedAsDownByUser]) then
    begin
      s.Quit;
      Result := s.ReLogin(1, kill, section);
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
