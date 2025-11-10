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
  sitesunit, queueunit, dateutils, SysUtils, irc, debugunit, Classes, mystrings;

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
  i, j, k: Integer;
  l: TLoginTask;
  fOriginalSlotName: string;
  fLoginStartTime: TDateTime;
  fLoginDurationMs: Int64;
  fBncHost: String;
  fBncPort: Integer;
  fBncTestResults: TStringList;
  fBestBnc: String;
  fBncList, fSplitted: TStringList;
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
    if kill then
    begin
      s.Quit;
      fLoginStartTime := Now;
      Result := s.ReLogin(1, True, section, readd);
      fLoginDurationMs := MilliSecondsBetween(Now, fLoginStartTime);

      if s.Status = ssOnline then
        announce := Format('<b>%s</b>: %s (%dms)', [s.site.Name, s.bnc, fLoginDurationMs]);
    end
    else if not readd or (not(s.site.WorkingStatus in [sstMarkedAsDownByUser, sstUp])) then
    begin
      if (s.Status <> ssOnline) then
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
      else if not readd then
      begin
        // !bnctest on already online site - test ALL BNCs sequentially
        Debug(dpMessage, section, '[BNCTEST] Testing all BNCs for site: %s', [s.site.Name]);

        // Send initial progress message (only for manual !bnctest, not for autobnctest)
        if netname <> '' then
          irc_addtext(self, '<b>%s</b>: Testing BNCs, this may take a moment...', [s.site.Name]);

        fBncTestResults := TStringList.Create;
        try
          // Disconnect current slot for testing
          s.Quit;
          s.DestroySocket(False);

          // Test all configured BNCs
          j := 0;
          while True do
          begin
            fBncHost := s.RCString('bnc_host-' + IntToStr(j), '');
            if fBncHost = '' then
              break;

            fBncPort := s.RCInteger('bnc_port-' + IntToStr(j), 0);
            fBestBnc := fBncHost + ':' + IntToStr(fBncPort);

            Debug(dpSpam, section, '[BNCTEST] Testing BNC #%d: %s', [j, fBestBnc]);

            // Try to login to this BNC and test with NOOP
            // Use skipReorder=True to prevent BNC list reordering during testing
            fLoginStartTime := Now;
            if s.LoginBnc(j, False, True) then
            begin
              // Successfully connected, now test with NOOP
              if s.Send('NOOP') and s.Read('NOOP', False, False, 5000) then
              begin
                fLoginDurationMs := MilliSecondsBetween(Now, fLoginStartTime);
                Debug(dpSpam, section, '[BNCTEST] %s: %dms', [fBestBnc, fLoginDurationMs]);
                fBncTestResults.Add(Format('%.6d|%s', [fLoginDurationMs, fBestBnc]));
              end
              else
              begin
                Debug(dpError, section, '[BNCTEST] %s: NOOP failed', [fBestBnc]);
                fBncTestResults.Add(Format('999998|%s (noop failed)', [fBestBnc]));
              end;

              // Disconnect after test
              s.Quit;
              s.DestroySocket(False);
            end
            else
            begin
              Debug(dpError, section, '[BNCTEST] %s: Login failed', [fBestBnc]);
              fBncTestResults.Add(Format('999999|%s (login failed)', [fBestBnc]));
            end;

            Inc(j);
          end;

          // Sort by response time (fastest first)
          fBncTestResults.Sort;

          // Find the original BNC index of the fastest BNC for reconnect
          i := -1;
          if fBncTestResults.Count > 0 then
          begin
            fBestBnc := Copy(fBncTestResults[0], Pos('|', fBncTestResults[0]) + 1, Length(fBncTestResults[0]));
            // Remove "(login failed)" or "(noop failed)" suffixes if present
            if Pos('(', fBestBnc) > 0 then
              fBestBnc := Trim(Copy(fBestBnc, 1, Pos('(', fBestBnc) - 1));

            // Find which index this BNC was
            j := 0;
            while True do
            begin
              fBncHost := s.RCString('bnc_host-' + IntToStr(j), '');
              if fBncHost = '' then
                break;
              fBncPort := s.RCInteger('bnc_port-' + IntToStr(j), 0);
              if (fBncHost + ':' + IntToStr(fBncPort)) = fBestBnc then
              begin
                i := j;
                Debug(dpMessage, section, '[BNCTEST] Fastest BNC at index %d: %s', [i, fBestBnc]);
                break;
              end;
              Inc(j);
            end;
          end;

          // Reconnect with the fastest BNC using its original index
          if i >= 0 then
          begin
            Result := s.LoginBnc(i, False);
            if Result then
            begin
              s.Status := ssOnline;

              // Only reorder BNC list AFTER successful reconnect
              // This prevents deadlocks from blocking other slots during reorder
              if i <> 0 then
              begin
                fBncList := TStringList.Create;
                try
                  // Extract BNC addresses from sorted results (format: "time|bnc")
                  for k := 0 to fBncTestResults.Count - 1 do
                  begin
                    fBestBnc := Copy(fBncTestResults[k], Pos('|', fBncTestResults[k]) + 1, Length(fBncTestResults[k]));
                    // Remove "(login failed)" or "(noop failed)" suffixes if present
                    if Pos('(', fBestBnc) > 0 then
                      fBestBnc := Trim(Copy(fBestBnc, 1, Pos('(', fBestBnc) - 1));
                    fBncList.Add(fBestBnc);
                    Debug(dpSpam, section, '[BNCTEST] Reorder position %d: %s', [k, fBestBnc]);
                  end;

                  // Reorder the BNC configuration (now after successful connect)
                  s.ReorderBncList(fBncList);
                finally
                  FreeAndNil(fBncList);
                end;
              end;
            end;
          end
          else
          begin
            Debug(dpError, section, '[BNCTEST] Could not find fastest BNC index for %s', [s.site.Name]);
            Result := s.Login(False);  // fallback to normal login
            if Result then
              s.Status := ssOnline;
          end;

          // Send results as separate IRC messages (one per BNC) - only for manual !bnctest
          if netname <> '' then
          begin
            for j := 0 to fBncTestResults.Count - 1 do
            begin
              // Extract time and BNC from "time|bnc" format
              fBestBnc := Copy(fBncTestResults[j], Pos('|', fBncTestResults[j]) + 1, Length(fBncTestResults[j]));
              fLoginDurationMs := StrToIntDef(Copy(fBncTestResults[j], 1, Pos('|', fBncTestResults[j]) - 1), 999999);

              // Check if this BNC is currently in use (with proper parentheses and nil check)
              if (s.Status = ssOnline) and ((fBestBnc = s.bnc) or ((s.bnc <> '') and (Pos(s.bnc, fBestBnc) > 0))) then
              begin
                // This is the active BNC
                if fLoginDurationMs < 999998 then
                  irc_addtext(self, '<b>%s</b>: %d. %s (%dms) <c3>(in use)</c>', [s.site.Name, j + 1, fBestBnc, fLoginDurationMs])
                else
                  irc_addtext(self, '<b>%s</b>: %d. %s <c3>(in use)</c>', [s.site.Name, j + 1, fBestBnc]);
              end
              else
              begin
                // Not the active BNC
                if fLoginDurationMs < 999998 then
                  irc_addtext(self, '<b>%s</b>: %d. %s (%dms)', [s.site.Name, j + 1, fBestBnc, fLoginDurationMs])
                else
                  irc_addtext(self, '<b>%s</b>: %d. %s', [s.site.Name, j + 1, fBestBnc]);
              end;
            end;

            // Set announce to empty since we already sent all messages
            announce := '';
          end
          else
          begin
            // For autobnctest: silent mode, just set a simple announce
            Debug(dpMessage, section, '[BNCTEST] Autobnctest completed for %s', [s.site.Name]);
          end;

        finally
          fBncTestResults.Free;
        end;
      end;

      //check all slots if this is not the bnc check. if it's the bnc check and the site might also have an idle
      //timeout set, we don't want to login all the slots
      // Also ensure Result is true (successful login) before triggering other slots
      if not readd and (s.Status = ssOnline) and Result then
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
