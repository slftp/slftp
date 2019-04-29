unit irccommands.general;

interface

{ slftp general commands functions }
function IrcHelp(const netname, channel, params: String): boolean;
function IrcDie(const netname, channel, params: String): boolean;
function IrcUptime(const netname, channel, params: String): boolean;
function IrcShowAppStatus(const netname, channel, params: String): boolean;
function IrcHelpV2(const netname, channel, params: String): boolean;
function IrcQueue(const netname, channel, params: String): boolean;
function IrcLastLog(const netname, channel, params: String): boolean;
function IrcSetDebugverbosity(const netname, channel, params: String): boolean;
function IrcCreateBackup(const netname, channel, params: String): boolean;
function IrcAuto(const netname, channel, params: String): boolean;

implementation

uses
  SysUtils, Classes, StrUtils, Math, irccommandsunit, irc, regexpr, statsunit, mainthread, debugunit,
  tasksunit, configunit, sitesunit, news, dbaddpre, dbaddurl, dbaddnfo, dbaddimdb, dbtvinfo, console,
  precatcher, queueunit, kb, mystrings, backupunit, versioninfo, slssl, irccommands.site,
  SynCommons, {$IFDEF MSWINDOWS}Windows, psAPI,{$ELSE}process,{$ENDIF} IdGlobal;

const
  section = 'irccommands.general';

procedure _readHelpTXTFile(const Netname, Channel, filename: String);
var
  s, fn: String;
  f: TextFile;
begin
  fn := 'help' + PathDelim + filename + '.txt';
  if FileExists(fn) then
  begin
    try
      AssignFile(f, fn);
      Reset(f);
      while not EOF(f) do
      begin
        ReadLn(f, s);
        //s := Trim(s);
        if s <> '' then
        begin
          s := ReplaceText(s, '<prefix>', irccmdprefix);
          s := ReplaceText(s, '<cmdprefix>', irccmdprefix);
          s := ReplaceText(s, '<cmd>', irccmdprefix + filename);
          irc_addtext(Netname, Channel, s);
        end;
      end;
    finally
      CloseFile(f);
    end;
  end;
end;

function IrcHelp(const netname, channel, params: String): boolean;
var
  i: integer;
  s: String;
  f: TextFile;
begin
  if params <> '' then
  begin
    i := FindIrcCommand(params);
    if i <> 0 then
    begin
      if FileExists('help' + PathDelim + params + '.txt') then
      begin
        _readHelpTXTFile(Netname, Channel, params);
      end
      else
        irc_addtext(Netname, Channel, Format('<c4>No help available on</c> %s', [params]));
    end
    else
      irc_addtext(Netname, Channel, Format('<b>Command %s not found.</b>', [params]));
  end
  else
  begin
    irc_addtext(Netname, Channel, '<b><u>Available commands are:</u></b>');
    s := '';
    for i := Low(ircCommandsArray) to High(ircCommandsArray) do
    begin
      if AnsiContainsText(ircCommandsArray[i].hlpgrp, '$') then
      begin
        if s <> '' then
          irc_addtext(Netname, Channel, s);

        irc_addtext(Netname, Channel, '.:: <u><c7><b>%s</b></c></u> ::.', [ircCommandsArray[i].cmd]);
        s := '';
      end
      else
      begin
        if s <> '' then
          s := s + ', ';

        if (ircCommandsArray[i].cmd <> '-') then
          s := s + irccmdprefix + ircCommandsArray[i].cmd;
      end;
    end;

    if s <> '' then
      irc_addtext(Netname, Channel, s);

    irc_addtext(Netname, Channel, 'Type <b>%shelp</b> command to get detailed info.', [irccmdprefix]);
  end;

  Result := True;
end;

function IrcDie(const netname, channel, params: String): boolean;
begin
  try
    slshutdown := IrcSetdown(Netname, Channel, '!ALL!');
  finally
    Result := slshutdown;
  end;
end;

function IrcUptime(const netname, channel, params: String): boolean;
var
  fProcessID, fCmdLine, fUsageInfo, fUnit: String;
  {$IFDEF MSWINDOWS}
    fMemCounters: TProcessMemoryCounters;
  {$ENDIF}
  fMemUsage: double;
  rr: TRegexpr;
begin
  fProcessID := IntToStr(IdGlobal.CurrentProcessId);

  {$IFDEF MSWINDOWS}
    fMemCounters.cb := SizeOf(fMemCounters);
    // memory amount returned from Win API always differs from Taskmgr -> see stackoverflow
    if GetProcessMemoryInfo(GetCurrentProcess(), @fMemCounters, SizeOf(fMemCounters)) then
    begin
      fMemUsage := fMemCounters.WorkingSetSize;
    end
    else
      fMemUsage := 0;
    
    RecalcSizeValueAndUnit(fMemUsage, fUnit, 0);
  {$ELSE}
    {$IFDEF UNIX}
      fCmdLine := '/proc/' + fProcessID + '/status';

      if RunCommand('cat', [fCmdLine], fUsageInfo) then
      begin
        rr := TRegexpr.Create;
        try
          rr.ModifierI := True;
          rr.Expression := 'VmRSS\:\s*(\d+)\s*\w+';

          if rr.Exec(fUsageInfo) then
          begin
            fMemUsage := StrToFloatDef(rr.Match[1], 0);
            RecalcSizeValueAndUnit(fMemUsage, fUnit, 1);
          end;
        finally
          rr.Free;
        end;
      end;
    {$ENDIF}
  {$ENDIF}

  irc_addtext(Netname, Channel, '<b>%s</b> [%s] (PID: %s / MEM: %s %s) with OpenSSL %s is up for <c7><b>%s</b></c> [%s]', [GetFullVersionString, GetDelphiCompilerVersion, fProcessID, FloatToStrF(fMemUsage, ffNumber, 15, 2), fUnit, OpenSSLShortVersion, DateTimeAsString(started), DatetimetoStr(started)]);

  Result := True;
end;

function IrcShowAppStatus(const netname, channel, params: String): boolean;
var
  rx: TRegexpr;
  spd, sup, sdn, suk: TStringList;
begin
  IrcUptime(Netname, Channel, '');

  irc_addtext(Netname, Channel, SlftpNewsStatus);

  irc_addtext(Netname, Channel, '<b>Knowledge Base</b>: %d Rip''s in mind', [kb_list.Count]);
  irc_addtext(Netname, Channel, TheTVDbStatus);

  if TPretimeLookupMOde(config.ReadInteger('taskpretime', 'mode', 0)) = plmSQLITE then
    irc_addtext(Netname, Channel, dbaddpre_Status);

  irc_addtext(Netname, Channel, 'Other Stats: %s <b>-</b> %s <b>-</b> %s', [dbaddurl_Status, dbaddimdb_Status, dbaddnfo_Status]);

  rx := TRegexpr.Create;
  sup := TStringList.Create;
  spd := TStringList.Create;
  sdn := TStringList.Create;
  suk := TStringList.Create;
  try
    rx.ModifierI := True;
    SitesWorkingStatusToStringlist(Netname, Channel, sup, sdn, suk, spd);

    irc_addtext(Netname, Channel,
      '<b>Sites count</b>: %d | <b>Online</b> %d - <b>Offline</b> %d - <b>Unknown</b> %d - <b>Permanent offline</b> %d ', [sites.Count - 1, sup.Count, sdn.Count, suk.Count, spd.Count]);

    rx.Expression := 'QUEUE\:\s(\d+)\s\(Race\:(\d+)\sDir\:(\d+)\sAuto\:(\d+)\sOther\:(\d+)\)';
    if rx.Exec(ReadAppQueueCaption) then
      irc_addtext(Netname, Channel,
        '<b>Complete queue count</b>: %s | <b>Racetasks</b> %s - <b>Dirlisttasks</b> %s - <b>Autotasks</b> %s - <b>Other</b> %s', [rx.Match[1], rx.Match[2], rx.Match[3], rx.Match[4], rx.Match[5]]);

  finally
    rx.free;
    sup.Free;
    spd.Free;
    sdn.Free;
    suk.Free;
  end;

  Result := True;
end;

function IrcHelpV2(const netname, channel, params: String): boolean;
var
  i: integer;
  fParams, ss, s: String;
begin
  result := False;
  s := '';
  fParams := params;

  if (fParams = '') then
  begin
    _readHelpTXTFile(Netname, Channel, 'nhelp');
  end;

  // Show all commands
  if ((fParams = '--all') or (fParams = '-all') or (fParams = '--a') or (fParams = '-a')) then
  begin
    irc_addtext(Netname, Channel, '<b><u>Available commands are</b>:</u>');
    for i := Low(ircCommandsArray) to High(ircCommandsArray) do
    begin
      if ((ircCommandsArray[i].cmd[1] = '-') or (AnsiStartsText('$',ircCommandsArray[i].hlpgrp))) then
      begin
        if s <> '' then
          irc_addtext(Netname, Channel, s);
        if (AnsiStartsText('$',ircCommandsArray[i].hlpgrp)) then
          irc_addtext(Netname, Channel, '.:: <u><c7><b>%s</b></c></u> ::.', [ircCommandsArray[i].cmd]);
        s := '';
      end
      else
      begin
        if s <> '' then
          s := s + ', ';
        s := s + irccmdprefix + ircCommandsArray[i].cmd;
      end;
    end;

    if s <> '' then
      IrcLineBreak(Netname, Channel, s, ',', '', 9);

    irc_addtext(Netname, Channel, 'Type <b>%shelp</b> command to get detailed info.', [irccmdprefix]);

    result := True;
    Exit;
  end;

  // Find commands by hlpgrp
  if AnsiStartsText('-', fParams) then
  begin
    ss := fParams;
    Delete(ss, 1, 1);
    if AnsiIndexText(ss, helpCommands) > -1 then
    begin
      for i := Low(ircCommandsArray) to High(ircCommandsArray) do
      begin
        if ((ircCommandsArray[i].hlpgrp = ss) or (ircCommandsArray[i].hlpgrp = '$' + ss)) then
        begin
          if AnsiContainsText(ircCommandsArray[i].hlpgrp, '$') then
            irc_addtext(Netname, Channel, '.:: <u><c7><b>%s</b></c></u> ::.', [ircCommandsArray[i].cmd])
          else
          begin
            if (ircCommandsArray[i].cmd <> '-') then
            begin
              if s <> '' then
                s := s + ', ';
              s := s + irccmdprefix + ircCommandsArray[i].cmd;
            end;
          end;
        end;
      end;
      if s <> '' then
        IrcLineBreak(Netname, Channel, s, ',', '', 12);
      result := True;
      Exit;
    end;

    irc_addtext(Netname, Channel, 'Help group <b>%s</b> not found.', [fParams]);
    result := True;
    Exit;
  end;

  // Display Textfile
  if fParams <> '' then
  begin
    if (1 = Pos(irccmdprefix, fParams)) then
      Delete(fParams, 1, 1);

    i := FindIrcCommand(fParams);
    if i <> 0 then
    begin
      if FileExists('help' + PathDelim + fParams + '.txt') then
      begin
        _readHelpTXTFile(Netname, Channel, fParams);
      end
      else
        irc_addtext(Netname, Channel, '<c4>No help available on</c> ' + fParams);
    end
    else
      irc_addtext(Netname, Channel, 'Command <b>%s</b> not found.', [fParams]);

    result := True;
    Exit;
  end;
end;

function IrcQueue(const netname, channel, params: String): boolean;
var
  i, ii: integer;
  show_tasks: integer;
  show_all: boolean;
  rr: TRegExpr;
begin
  rr := TRegExpr.Create;
  try
    rr.ModifierI := True;

    show_tasks := 10;
    rr.Expression := '-c\:([\d]+)';
    if rr.Exec(params) then
    begin
      show_tasks := StrToIntDef(rr.Match[1], 10);
    end;

    show_all := False;
    rr.Expression := '--all';
    if rr.Exec(params) then
    begin
      show_tasks := tasks.Count;
      show_all := True;
    end;

    ii := 0;
    irc_addtext(Netname, Channel, 'Tasks in queue: %d displaycount: %d', [tasks.Count, Min(show_tasks, tasks.Count)]);

    for i := 0 to tasks.Count - 1 do
    begin
      try

        if show_all then
        begin
          irc_addtext(Netname, Channel, TTask(tasks[i]).Fullname);
          Continue;
          //        Inc(ii);
        end
        else
        begin

          if (ii > show_tasks) then
            break;

          rr.Expression := '(AUTO(LOGIN|INDEX|NUKE|RULES))';
          if ((not rr.Exec(TTask(tasks[i]).Fullname)) and (not TTask(tasks[i]).ready) and (not TTask(tasks[i]).readyerror)) then
          begin
            irc_addtext(Netname, Channel, TTask(tasks[i]).Fullname);
            Inc(ii);
          end;
        end;
      except
        break;
      end;
    end;

  finally
    rr.Free;
  end;

  Result := True;
end;

function IrcLastLog(const netname, channel, params: String): boolean;
var
  lines: integer;
  lastlog: String;
begin
  Result := False;
  lines := StrToIntDef(params, -1);

  if lines >= 100 then
  begin
    irc_Addtext(Netname, Channel, 'Are you okay ? <b>%d</b> is too much lines. Do you want to break the internet ?!', [lines]);
    exit;
  end;

  if lines = -1 then
    lines := 5;

  lastlog := LogTail(lines);
  if lastlog <> '' then
  begin
    irc_Addtext(Netname, Channel, 'Displaying last <b>%d</b> log lines:', [lines]);
    irc_Addtext(Netname, Channel, lastlog);
  end
  else
  begin
    irc_Addtext(Netname, Channel, 'No log entries to display.');
  end;

  Result := True;
end;

function IrcSetDebugverbosity(const netname, channel, params: String): boolean;
var
  val: integer;
begin
  val := StrToIntDef(params, -1);
  if val = -1 then
  begin

    case config.ReadInteger('debug', 'verbosity', 0) of
      0: irc_Addtext(Netname, Channel, 'Only Logging Errors.');
      1: irc_Addtext(Netname, Channel, 'Only Logging Errors and common Messages.');
      2: irc_Addtext(Netname, Channel, 'Only Logging Almost everything.');
      3: irc_Addtext(Netname, Channel, 'Skip Logging...');
    end;
    Result := True;
    Exit;
  end
  else if (val <= 3) then
  begin
    config.WriteInteger('debug', 'verbosity', val);
    config.UpdateFile;
    case config.ReadInteger('debug', 'verbosity', 0) of
      0: irc_Addtext(Netname, Channel, 'Only Logging Errors.');
      1: irc_Addtext(Netname, Channel, 'Only Logging Errors and common Messages.');
      2: irc_Addtext(Netname, Channel, 'Only Logging Almost everything.');
      3: irc_Addtext(Netname, Channel, 'Skip Logging...');
    end;
    Result := True;
    Exit;
  end
  else
  begin
    irc_Addtext(Netname, Channel, '<c4>Syntax error</c>, unknown verbosity.');
    Result := False;
    Exit;
  end;
  Result := True;
end;

function IrcCreateBackup(const netname, channel, params: String): boolean;
var
  error: String;
begin
  Result := ircBackup(error);
  if error <> '' then
    irc_addtext(Netname, Channel, '<b><c4>%s</b></c>', [error]);
end;

function IrcAuto(const netname, channel, params: String): boolean;
begin
  Result := False;

  if params <> '' then
  begin
    if params = '0' then
    begin
      sitesdat.WriteBool('precatcher', 'auto', False);
      irc_addtext(Netname, Channel, Format('Auto is disabled [%s] now!', [IntToStr(integer(precatcher.precatcherauto))]));
    end;

    if params = '1' then
    begin
      sitesdat.WriteBool('precatcher', 'auto', True);
      irc_addtext(Netname, Channel, Format('Auto is enabled [%s] now!', [IntToStr(integer(precatcher.precatcherauto))]));
    end;
  end
  else
  begin
    if precatcher.precatcherauto then
      irc_addtext(Netname, Channel, Format('Precatcher auto is: Enabled [%s]', [IntToStr(integer(precatcher.precatcherauto))]))
    else
      irc_addtext(Netname, Channel, Format('Precatcher auto is: Disabled [%s]', [IntToStr(integer(precatcher.precatcherauto))]));
  end;

  Result := True;
end;

end.