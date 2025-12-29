unit tasklogin;

interface

uses tasksunit;

type
  TLoginTask = class(TTask)
  public
    noannounce: Boolean;
    readd: Boolean; //< @true if called from autobnctest, @false otherwise
    kill: Boolean;
    isBnctest: Boolean; //< @true if this task should run the full BNC benchmark
    constructor Create(const netname, channel, site: String; kill: Boolean; readd: Boolean; bnctest: Boolean = False);
    function Execute(slot: Pointer): Boolean; override;
    function Name: String; override;
  end;

implementation

uses
  sitesunit, queueunit, dateutils, SysUtils, irc, debugunit;

const
  section = 'login';

{ TLoginTask }

constructor TLoginTask.Create(const netname, channel, site: String; kill: Boolean; readd: Boolean; bnctest: Boolean = False);
begin
  inherited Create(netname, channel, site);
  self.kill := kill;
  self.readd := readd;
  self.isBnctest := bnctest;
end;

function TLoginTask.Execute(slot: Pointer): Boolean;
var
  s: TSiteSlot;
  i, j: Integer;
  l: TLoginTask;
  fOriginalSlotName: string;
  fLoginStartTime: TDateTime;
  fLoginDurationMs: Int64;
  fBncHost: String;
  fBncPort: Integer;
  fReachableBnc: String;
  fBestBncIndex: Integer;
  fBestBncMs: Int64;
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
    // Priority 1: Kill/Force Login
    if kill then
    begin
      s.Quit;
      fLoginStartTime := Now;
      Result := s.ReLogin(1, True, section, readd);
      fLoginDurationMs := MilliSecondsBetween(Now, fLoginStartTime);

      if s.Status = ssOnline then
        announce := Format('<b>%s</b>: %s (%dms)', [s.site.Name, s.bnc, fLoginDurationMs]);
    end
    // Priority 2: Explicit BNC Test (Manual or Auto)
    // Runs the benchmark loop.
    else if isBnctest then
    begin
      // !bnctest should only check reachability, not benchmark or reorder
      Debug(dpMessage, section, '[BNCTEST] Checking BNC reachability for site: %s', [s.site.Name]);

      if s.site.PermDown then
      begin
        Debug(dpMessage, section, '[BNCTEST] Skipping permdown site: %s', [s.site.Name]);
        if netname <> '' then
          irc_addtext(self, '<b>%s</b>: Site is set to permdown, skipping BNC test', [s.site.Name]);
        Result := s.Login(False);
        if Result then
          s.Status := ssOnline;
        ready := True;
        exit;
      end;

      // Send initial progress message (only for manual !bnctest, not for autobnctest)
      if netname <> '' then
        irc_addtext(self, '<b>%s</b>: Testing BNC reachability...', [s.site.Name]);

      Debug(dpMessage, section, '[BNCTEST] Disconnecting current slot for testing: %s', [s.Name]);
      // Disconnect current slot for testing
      s.Quit;
      s.DestroySocket(False);

      Result := False;
      fReachableBnc := '';
      fBestBncIndex := -1;
      fBestBncMs := High(Int64);
      j := 0;
      while True do
      begin
        fBncHost := s.RCString('bnc_host-' + IntToStr(j), '');
        if fBncHost = '' then
          break;

        fBncPort := s.RCInteger('bnc_port-' + IntToStr(j), 0);
        if fBncPort = 0 then
        begin
          Debug(dpError, section, '[BNCTEST] Skipping invalid BNC port for %s (index %d)', [s.site.Name, j]);
          Inc(j);
          continue;
        end;

        Debug(dpMessage, section, '[BNCTEST] Testing BNC index %d: %s:%d', [j, fBncHost, fBncPort]);
        fLoginStartTime := Now;
        if s.LoginBnc(j, False, True) then
        begin
          Debug(dpMessage, section, '[BNCTEST] Login ok for %s:%d', [fBncHost, fBncPort]);
          if s.Send('NOOP') and s.Read('NOOP', False, False, 5000) then
          begin
            fLoginDurationMs := MilliSecondsBetween(Now, fLoginStartTime);
            if fLoginDurationMs < fBestBncMs then
            begin
              fBestBncMs := fLoginDurationMs;
              fBestBncIndex := j;
              fReachableBnc := fBncHost + ':' + IntToStr(fBncPort);
            end;
            Debug(dpMessage, section, '[BNCTEST] Reachable BNC: %s (%dms)', [fBncHost + ':' + IntToStr(fBncPort), fLoginDurationMs]);
          end;

          Debug(dpError, section, '[BNCTEST] NOOP failed for %s:%d', [fBncHost, fBncPort]);
          Debug(dpMessage, section, '[BNCTEST] Disconnecting after test: %s:%d', [fBncHost, fBncPort]);
          s.Quit;
          s.DestroySocket(False);
        end
        else
        begin
          Debug(dpError, section, '[BNCTEST] Login failed for %s:%d', [fBncHost, fBncPort]);
        end;

        Inc(j);
        Sleep(250);
      end;

      Debug(dpMessage, section, '[BNCTEST] Test loop complete. Best index=%d BNC=%s time=%dms', [fBestBncIndex, fReachableBnc, fBestBncMs]);
      if fBestBncIndex >= 0 then
      begin
        Debug(dpMessage, section, '[BNCTEST] Reconnecting to fastest BNC index %d', [fBestBncIndex]);
        Result := s.LoginBnc(fBestBncIndex, False);
        if Result then
          s.Status := ssOnline;
      end;

      if netname <> '' then
      begin
        if Result then
          irc_addtext(self, '<b>%s</b>: Fastest BNC: %s (%dms)', [s.site.Name, fReachableBnc, fBestBncMs])
        else
          irc_addtext(self, '<b>%s</b>: No BNC reachable', [s.site.Name]);

        announce := '';
      end
      else
      begin
        Debug(dpMessage, section, '[BNCTEST] Autobnctest completed for %s (reachable=%s)', [s.site.Name, BoolToStr(Result, True)]);
      end;
    end
    // Priority 3: Generic Login / Wakeup (Site Down or Slot Offline)
    else if (s.Status <> ssOnline) or (not(s.site.WorkingStatus in [sstMarkedAsDownByUser, sstUp])) then
    begin
      // site is not up, we have to try to login
      s.Quit;
      fLoginStartTime := Now;
      Result := s.ReLogin(1, kill, section, readd);
      fLoginDurationMs := MilliSecondsBetween(Now, fLoginStartTime);

      if s.Status = ssOnline then
      begin
        // slot is online - show BNC and response time
        if readd then
          announce := Format('<b>%s</b>: %s (%dms)', [s.site.Name, s.bnc, fLoginDurationMs])
        else
          announce := Format('<b>%s</b>: %s (%dms)', [s.site.Name, s.bnc, fLoginDurationMs]);
      end;
    end
    else
    begin
    end;

    //check all slots if this is not the bnc check. if it's the bnc check and the site might also have an idle
    //timeout set, we don't want to login all the slots
    // Also ensure Result is true (successful login) before triggering other slots
    if not readd and not isBnctest and (s.Status = ssOnline) and Result then
    begin
      for s in s.site.slots do
      begin
        if (s.Status <> ssOnline) and (s.Name <> fOriginalSlotName) then
        begin
          l := TLoginTask.Create(netname, channel, site1, False, False, False);
          l.wantedslot := s.Name;
          AddTask(l);
        end;
      end;
    end;
  end
  else
  begin
    if kill or (s.Status <> ssOnline) or not(s.site.WorkingStatus in [sstUp, sstMarkedAsDownByUser]) then
    begin
      s.Quit;
      fLoginStartTime := Now;
      Result := s.ReLogin(1, kill, section);
      fLoginDurationMs := MilliSecondsBetween(Now, fLoginStartTime);

      if s.Status = ssOnline then
      begin
        announce := Format('<b>%s</b>: %s (%dms)', [s.site.Name, s.bnc, fLoginDurationMs]);
      end;
    end
    else
    begin
    end;
  end;

  if readd then
  begin
    try
      l := TLoginTask.Create(netname, channel, site1, kill, readd, isBnctest);
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
