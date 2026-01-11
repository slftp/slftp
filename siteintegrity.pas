unit siteintegrity;

interface

procedure CheckSitesIntegrity;

implementation

uses
  SysUtils, Classes, StrUtils, encinifile, configunit,
  {$IFDEF MSWINDOWS}
    Windows,
  {$ELSE}
    process, baseunix,
  {$ENDIF}
  slblowfish;

type
  TKeyCheckMode = (kcmCaseSensitive, kcmCaseInsensitive);

// --- Password Prompt Logic (Duplicate from commandlineutil) ---
function AskUserForPassword(const aMessagePrompt: String): String;
var
  {$IFDEF MSWINDOWS}
    fConsoleHandle: THANDLE;
    fOldConsoleMode, fNewConsoleMode: DWORD;
  {$ELSE}
    fEchoOff, fEchoOn: TProcess;
  {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    System.Reset(Input);
    fConsoleHandle := TTextRec(Input).Handle;
    if fConsoleHandle = INVALID_HANDLE_VALUE then Halt(1);
    GetConsoleMode(fConsoleHandle, fOldConsoleMode);
    fNewConsoleMode := fOldConsoleMode and (not ENABLE_ECHO_INPUT);
    SetConsoleMode(fConsoleHandle, fNewConsoleMode);
    write(aMessagePrompt);
    ReadLn(Result);
    writeln;
    SetConsoleMode(fConsoleHandle, fOldConsoleMode);
  {$ELSE}
    fEchoOff := TProcess.Create(nil);
    fEchoOn := TProcess.Create(nil);
    try
      fEchoOff.Executable := 'stty';
      fEchoOff.Parameters.add('-echo');
      fEchoOn.Executable := 'stty';
      fEchoOn.Parameters.add('echo');
      fEchoOff.Execute;
      write(aMessagePrompt);
      ReadLn(Result);
      WriteLn;
    finally
      fEchoOn.Execute;
      fEchoOff.Free;
      fEchoOn.Free;
    end;
  {$ENDIF}
end;
// -------------------------------------------------------------

function GetSiteNameFromSection(const aSection: String): String;
begin
  if StartsText('site-', aSection) then
    Result := Copy(aSection, 6, MaxInt)
  else if StartsText('speed-from-', aSection) then
    Result := Copy(aSection, 12, MaxInt)
  else if StartsText('affilspeed-from-', aSection) then
    Result := Copy(aSection, 17, MaxInt)
  else if StartsText('speed-to-', aSection) then
    Result := Copy(aSection, 10, MaxInt)
  else if StartsText('rank-', aSection) then
    Result := Copy(aSection, 6, MaxInt)
  else
    Result := '';
end;

function GetSectionSourceHint(const aSection: String; SplitModeEnabled: Boolean): String;
var
  SiteName: String;
begin
  if SplitModeEnabled and StartsText('site-', aSection) then
  begin
    SiteName := Copy(aSection, 6, MaxInt);
    Result := Format('sites.dat + rtpl/%s.settings', [SiteName]);
  end
  else
    Result := 'sites.dat';
end;

procedure CheckSitesIntegrity;
var
  ini: TEncIniFile;
  SplitModeConfigured: Boolean;
  RedirectKeys: TStringList;
  Password: String;
  FoundIssuesPhase1, FoundIssuesPhase2: Boolean;
  
  procedure Report(const aType, aMsg: String);
  begin
    WriteLn(Format('[%s] %s', [aType, aMsg]));
  end;

  procedure ReportError(const aMsg: String);
  begin
    Report('ERROR', aMsg);
  end;

  procedure ReportWarn(const aMsg: String);
  begin
    Report('WARN', aMsg);
  end;

  procedure ScanIni(aIni: TEncIniFile; CheckSplitConstraints: Boolean; out IssuesFound: Boolean);
  var
    s_i, s_j, s_k: Integer;
    s_Section, s_Key, s_Name: String;
    s_KeysInSection, s_Sites: TStringList;
  begin
    IssuesFound := False;
    s_Sites := TStringList.Create;
    s_KeysInSection := TStringList.Create;
    try
      aIni.ReadSections(s_Sites);
      WriteLn(Format('Scanning %d sections...', [s_Sites.Count]));

      for s_i := 0 to s_Sites.Count - 1 do
      begin
        s_Section := s_Sites[s_i];
        
        // 1. Check Duplicates (Case Insensitive collision)
        s_KeysInSection.Clear;
        try
          aIni.ReadSection(s_Section, s_KeysInSection);
          
          for s_j := 0 to s_KeysInSection.Count - 1 do
            for s_k := s_j + 1 to s_KeysInSection.Count - 1 do
              if AnsiSameText(s_KeysInSection[s_j], s_KeysInSection[s_k]) then
              begin
                if (StartsText('site-', s_Section)) and
                   (s_KeysInSection[s_j] = s_KeysInSection[s_k]) then
                  Continue;
                if (s_KeysInSection[s_j] = '') and (s_KeysInSection[s_k] = '') then
                begin
                  ReportWarn(Format('Section "%s": Duplicate empty key found (blank line). Source: %s',
                    [s_Section, GetSectionSourceHint(s_Section, SplitModeConfigured)]));
                  IssuesFound := True;
                end
                else
                begin
                  ReportError(Format('Section "%s": Duplicate key collision found: "%s" vs "%s". Source: %s',
                    [s_Section, s_KeysInSection[s_j], s_KeysInSection[s_k],
                     GetSectionSourceHint(s_Section, SplitModeConfigured)]));
                  IssuesFound := True;
                end;
              end;
        
          // 2. Check Split Data Constraints (Only if checking raw sites.dat)
          if CheckSplitConstraints and StartsText('site-', s_Section) then
          begin
            for s_j := 0 to s_KeysInSection.Count - 1 do
            begin
              s_Key := s_KeysInSection[s_j];
              if (RedirectKeys.IndexOf(s_Key) = -1) and (not StartsText('bnc_', s_Key)) then
              begin
                ReportWarn(Format('Section "%s": Key "%s" should not be in sites.dat (Split Data Mode).', [s_Section, s_Key]));
                IssuesFound := True;
              end;
            end;
          end;
          
        except
          // ignore read errors for now
        end;
        
        // 3. Orphan Check (Only relevant if we see the orphans)
        if StartsText('speed-from-', s_Section) or 
           StartsText('affilspeed-from-', s_Section) or
           StartsText('speed-to-', s_Section) or
           StartsText('rank-', s_Section) then
        begin
          s_Name := GetSiteNameFromSection(s_Section);
          if (s_Name <> '') and (not aIni.SectionExists('site-' + s_Name)) then
          begin
             ReportWarn(Format('Orphaned Section "%s" found (Site "%s" does not exist).', [s_Section, s_Name]));
             IssuesFound := True;
          end;
        end;

        // 4. Deprecated Section Check
        if StartsText('affilspeed-to-', s_Section) then
        begin
          ReportWarn(Format('Deprecated Section "%s" found. This section type is no longer used and should be removed.', [s_Section]));
          IssuesFound := True;
        end;
      end;
    finally
      s_Sites.Free;
      s_KeysInSection.Free;
    end;
  end;

begin
  WriteLn('Starting sites.dat integrity check...');
  
  if not FileExists('sites.dat') then
  begin
    ReportError('sites.dat not found!');
    Exit;
  end;

  // Initialize Config & Password
  if FileExists('slftp.cini') then
  begin
    WriteLn('Encrypted configuration found (slftp.cini).');
    Password := AskUserForPassword('Please enter decryption password: ');
  end
  else
  begin
    WriteLn('Note: If sites.dat is encrypted, you must provide the password.');
    Password := AskUserForPassword('Password (press Enter for none): ');
  end;

  if not ConfigInit(Password) then
  begin
    ReportError('Failed to initialize configuration (Wrong password?). Aborting.');
    Exit;
  end;

<<<<<<< HEAD
  RedirectKeys := TStringList.Create;
  RedirectKeys.CaseSensitive := False;
  RedirectKeys.Add('redirect');
  RedirectKeys.Add('username');
  RedirectKeys.Add('password');
  RedirectKeys.Add('max_dn');
  RedirectKeys.Add('max_pre_dn');
  RedirectKeys.Add('max_up');
  RedirectKeys.Add('slots');
  RedirectKeys.Add('proxyname');
  RedirectKeys.Add('ircnick');

  try
    SplitModeConfigured := config.ReadBool('sites', 'split_site_data', False);
    
    if SplitModeConfigured then
    begin
      // --- PHASE 2: Full Merged Check ---
      WriteLn('--- Phase 2: Checking full merged configuration ---');
      config.WriteBool('sites', 'split_site_data', True);
      
      try
        if Password = '' then
          ini := TEncIniFile.Create('sites.dat', '', False)
        else
          ini := TEncIniFile.Create('sites.dat', passphrase, False);
        
        ScanIni(ini, False, FoundIssuesPhase2); // Don't check constraints in merged mode (keys are expected)
        
      finally
        // Do NOT free here if we want to use it for fixing... but we need to recreate for consistency
        ini.Free;
      end;
    end
    else
    begin
      // --- PHASE 1: Raw sites.dat Check ---
      WriteLn('--- Phase 1: Checking raw sites.dat content ---');
      config.WriteBool('sites', 'split_site_data', False);
      
      try
        if Password = '' then
          ini := TEncIniFile.Create('sites.dat', '', False)
        else
          ini := TEncIniFile.Create('sites.dat', passphrase, False);
        
        ScanIni(ini, SplitModeConfigured, FoundIssuesPhase1);
        
      finally
        ini.Free;
      end;
    end;

    if FoundIssuesPhase1 or FoundIssuesPhase2 then
    begin
      WriteLn('');
      WriteLn('Issues were found during the integrity check. Please review the errors above.');
    end
    else
    begin
      WriteLn('No issues found.');
    end;

  finally
    RedirectKeys.Free;
  end;
end;

end.
