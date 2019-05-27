unit irccommands.misc;

interface

{ slftp misc commands functions }
function IrcRaw(const netname, channel, params: String): boolean;
function IrcManageUser(const netname, channel, params: String): boolean;
function IrcInvite(const netname, channel, params: String): boolean;
function IrcSiteChan(const netname, channel, params: String): boolean;
function IrcSetAutoInvite(const netname, channel, params: String): boolean;
function IrcTweak(const netname, channel, params: String): boolean;
function IrcIdent(const netname, channel, params: String): boolean;
function IrcNoSocks5(const netname, channel, params: String): boolean;
function IrcNoAnnounceSite(const netname, channel, params: String): boolean;
function IrcNoHelp(const netname, channel, params: String): boolean;
function IrcTestLanguageBase(const netname, channel, params: String): boolean;
function IrcKillAll(const netname, channel, params: String): boolean;
function IrcSpamConfig(const netname, channel, params: String): boolean;
function Ircaddknowngroup(const netname, channel, params: String): boolean;

implementation

uses
  SysUtils, Classes, irc, kb, sitesunit, mystrings, mrdohutils, regexpr, debugunit, sllanguagebase,
  taskrace, knowngroups, configunit, queueunit, irccommandsunit;

const
  section = 'irccommands.misc';

function IrcRaw(const netname, channel, params: String): boolean;
var
  command, sitename: String;
  s: TSite;
  i: integer;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  command := mystrings.RightStr(params, length(sitename) + 1);

  if sitename = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      if (TSite(sites.Items[i]).Name = getAdminSiteName) then
        Continue;
      if TSite(sites.Items[i]).PermDown then
        Continue;
      s := TSite(sites.Items[i]);
      if (s.PermDown) then
        Continue;
      RawB(Netname, Channel, s.Name, '', command, True);

    end;
    Result := True;
  end
  else
  begin
    s := FindSiteByName(Netname, sitename);
    if s = nil then
    begin
      irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
      exit;
    end;
    if not s.PermDown then
    begin
      RawB(Netname, Channel, sitename, '', command);
      Result := True;
    end
    else
    begin
      irc_addtext(Netname, Channel, 'Site <b>%s</b> is set perm down.', [sitename]);
      Result := False;
    end;
  end;
end;

function IrcManageUser(const netname, channel, params: String): boolean;
var
  command, username: String;
  s: TSite;
  i, j: integer;
  x, y: TStringList;
begin
  Result := False;
  username := UpperCase(SubString(params, ' ', 1));
  command := mystrings.RightStr(params, length(username) + 1);

  x := TStringList.Create;
  y := TStringList.Create;
  try
    for i := 0 to sites.Count - 1 do
    begin
      s := TSite(sites[i]);
      if s.markeddown then
      begin
        irc_addtext(Netname, Channel, 'Skipping site %s, cause its marked down.', [s.Name]);
        Continue;
      end;
      if (s.PermDown) then
      begin
        irc_addtext(Netname, Channel, 'Skipping site %s, cause its perm down.', [s.Name]);
        Continue;
      end;

      x.DelimitedText := s.leechers;
      if x.IndexOf(username) <> -1 then
      begin
        y.Add(s.Name);
        Continue;
      end;

      x.DelimitedText := s.traders;
      j := x.IndexOf(username);
      if j <> -1 then
      begin
        y.Add(s.Name);
        Continue;
      end;
    end;

    if y.Count = 0 then
    begin
      irc_addtext(Netname, Channel, 'User %s not found on any sites', [username]);
      exit;
    end;

    for i := 0 to y.Count - 1 do
      RawB(Netname, Channel, y[i], '', command);

  finally
    x.Free;
    y.Free;
  end;

  Result := True;
end;

function IrcInvite(const netname, channel, params: String): boolean;
var
  sitename: String;
  s: TSite;
  xl: TStringList;
  i: integer;
begin
  sitename := UpperCase(params);

  xl := TStringList.Create;
  try
    xl.Delimiter := AnsiChar(44);
    xl.DelimitedText := sitename;
    sitename := '';
    for i := 0 to xl.Count - 1 do
    begin
      sitename := xl.Strings[i];
      s := FindSiteByName(Netname, sitename);
      if s = nil then
      begin
        irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
        Continue;
      end;
      if s.PermDown then
      begin
        irc_addtext(Netname, Channel, 'Site <b>%s</b> is set permdown.', [sitename]);
        continue;
      end;
      RawB(Netname, Channel, sitename, '', 'SITE INVITE ' + mynickname);
    end;
  finally
    xl.Free;
  end;

  Result := True;
end;

function IrcSitechan(const netname, channel, params: String): boolean;
var
  sitename, nn: String;
  s: TSite;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  nn := UpperCase(SubString(params, ' ', 2));
  if nn <> '' then
  begin
    if nil = FindIrcnetwork(nn) then
    begin
      irc_addtext(Netname, Channel, 'Cant find network.');
      exit;
    end;
  end;

  s := FindSiteByName(Netname, sitename);
  if nil = s then
  begin
    irc_addtext(Netname, Channel, 'Cant find site.');
    exit;
  end;

  if nn <> '' then
    s.WCString('ircnet', nn)
  else
    s.DeleteKey('ircnet');

  Result := True;
end;

function IrcSetAutoInvite(const netname, channel, params: String): boolean;
var
  sitename, value: String;
  site: TSite;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  value := SubString(params, ' ', 2);

  site := FindSiteByName(Netname, sitename);
  if site = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
    Exit;
  end;

  if site.IRCNick = '' then
  begin
    irc_addtext(Netname, Channel, 'You have to define an IRC Nick for <b>%s</b>.', [sitename]);
    Exit;
  end;

  if value = '' then
  begin
    irc_addtext(Netname, Channel, 'Autoinvite: <b>%s</b>', [BoolToStr(site.UseAutoInvite, True)]);
  end
  else if ((value = '1') or (value = '0')) then
  begin
    if value = '1' then
      site.UseAutoInvite := True;
    if value = '0' then
      site.UseAutoInvite := False;

    irc_addtext(Netname, Channel, 'Autoinvite: <b>%s</b>', [BoolToStr(site.UseAutoInvite, True)]);
  end
  else
  begin
    irc_addtext(Netname, Channel, 'Syntax error. Wrong value parameter!');
    Exit;
  end;

  Result := True;
end;

function IrcTweak(const netname, channel, params: String): boolean;
var
  ss1, ss2, s1, s2, s3: String;
  x: TRegExpr;
begin
  Result := False;
  s1 := SubString(params, ' ', 1);
  s2 := lowercase(SubString(params, ' ', 2));
  s3 := mystrings.RightStr(params, length(s1) + 1 + length(s2) + 1);
  ss1 := SubString(s1, '-', 1);
  ss2 := SubString(s1, '-', 2);

  x := TRegExpr.Create;
  try
    x.ModifierI := True;
    x.Expression := '(site\-|ircnet\-|mysql\-)(.*?)$';
    if not x.Exec(s1) then
    begin
      irc_addtext(Netname, Channel,
        '<c4><b>Syntax error</b>. Need to start with (site\-|ircnet\-|mysql\-).</c>');
      exit;
    end
    else
      s1 := lowercase(ss1) + '-' + uppercase(ss2);

  finally
    x.Free;
  end;

  if s3 = '' then
    irc_addtext(Netname, Channel, 'Value is: ' + sitesdat.ReadString(s1, s2, ''))
  else
  begin
    try
      sitesdat.WriteString(s1, s2, s3);
    except
      on E: Exception do
      begin
        Debug(dpError, section, '[EXCEPTION] IrcTweak : %s', [e.Message]);
        Exit;
      end;
    end;

    irc_addtext(netname, channel, 'New value is: ' + sitesdat.ReadString(s1, s2, ''));
  end;

  Result := True;
end;

function IrcIdent(const netname, channel, params: String): boolean;
var
  ss, sitename: String;
  s: TSite;
  ident: String;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  ident := mystrings.RightStr(params, length(sitename) + 1);

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site %s not found.', [sitename]);
    exit;
  end;

  // TODO: Add property
  if ident <> '' then
    s.WCString('ident', ident)
  else
    s.DeleteKey('ident');

  ss := s.RCString('ident', config.ReadString(section, 'response', 'rsctm'));
  if ss <> '' then
    irc_addtext(Netname, Channel, 'Ident reply for %s is %s', [sitename, ss]);

  Result := True;
end;

function IrcNoSocks5(const netname, channel, params: String): boolean;
var
  sitename: String;
  s: TSite;
  q: integer;
  s2: String;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  s2 := mystrings.RightStr(params, length(sitename) + 1);
  q := StrToIntDef(s2, -1);

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site %s not found.', [sitename]);
    exit;
  end;

  // TODO: Add property
  if q = 1 then
    s.WCInteger('nosocks5', q)
  else if q = 0 then
    s.DeleteKey('nosocks5');
  q := s.RCInteger('nosocks5', 0);

  irc_addtext(Netname, Channel, 'Nosocks5 for %s is %d', [sitename, q]);

  Result := True;
end;

function IrcNoannouncesite(const netname, channel, params: String): boolean;
var
  sitename: String;
  s: TSite;
  cwd: integer;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  cwd := StrToIntDef(SubString(params, ' ', 2), -1);

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
    exit;
  end;

  if cwd = 1 then
    s.noannounce := True
  else if cwd = 0 then
    s.noannounce := False;

  irc_addtext(Netname, Channel, 'Noannounce setting of site <b>%s</b> is %d', [sitename, integer(s.noannounce)]);

  Result := True;
end;

function IrcNoHelp(const netname, channel, params: String): boolean;
var
  Count, i: integer;
begin
  Count := 0;
  for i := Low(ircCommandsArray) to High(ircCommandsArray) do
    if ((length(ircCommandsArray[i].cmd) > 0) and (ircCommandsArray[i].cmd[1] <> '-')) then
      if not FileExists(IncludeTrailingPathDelimiter('help') + ircCommandsArray[i].cmd + '.txt') then
      begin
        irc_addtext(Netname, Channel, 'Command %s has no help yet.', [ircCommandsArray[i].cmd]);
        Inc(Count);
      end;
  if Count = 0 then
    irc_addtext(Netname, Channel, 'No help is missing.');

  Result := True;
end;

function IrcTestLanguageBase(const netname, channel, params: String): boolean;
begin
  Result := False;
  irc_addtext(Netname, Channel, Format('Read language for: %s', [params]));
  irc_addtext(Netname, Channel, Format('language -> %s', [FindLanguageOnDirectory(params)]));
  irc_addtext(Netname, Channel, Format('mp3 language -> %s', [FindMusicLanguageOnDirectory(params)]));
  Result := True;
end;

function IrcKillAll(const netname, channel, params: String): boolean;
var
  i: integer;
  rx: TRegExpr;
begin
  Result := False;

  rx := TRegExpr.Create;
  try
    rx.ModifierI := False;
    rx.Expression := 'AUTOLOGIN';
    for i := 0 to tasks.Count - 1 do
      if not rx.Exec(TPazoTask(tasks[i]).Fullname) then
      begin
        irc_addtext(Netname, Channel, 'Removing Task -> %s', [TPazoTask(tasks[i]).Fullname]);
        try
          tasks.Remove(TPazoTask(tasks[i]));
        except
          on E: Exception do
            Irc_AddText(Netname, Channel, '<c4><b>ERROR</c></b>: IrcKillAll.tasks.Remove: %s', [e.Message]);
        end;

      end
      else
        Continue;
  finally
    rx.Free;
  end;

  Result := True;
end;

function IrcSpamConfig(const netname, channel, params: String): boolean;
var
  vsecs, vsval: TStringList;
  i: integer;
  csec, ckey, cvalue: String;
begin
  Result := False;

  vsecs := TStringList.Create;
  try
    vsval := TStringList.Create;
    try
      if params = '' then
      begin
        spamcfg.ReadSections(vsecs);
        irc_addtext(Netname, Channel, '<b>Sections:</b> %s', [vsecs.commatext]);
        for i := 0 to vsecs.Count - 1 do
        begin
          vsval.Clear;
          spamcfg.ReadSection(vsecs.Strings[i], vsval);
          IrcLineBreak(netname, channel, vsval.CommaText, '"', '<b>' + vsecs.Strings[i] + ':</b> ', 9);
        end;
        Result := True;
        exit;
      end;

      csec := SubString(params, ' ', 1);
      ckey := SubString(params, ' ', 2);
      if ckey = '' then
      begin
        vsval.Clear;
        spamcfg.ReadSection(csec, vsval);
        IrcLineBreak(netname, channel, vsval.CommaText, '"', '<b>valid keys:</b> ', 9);
        Result := True;
        exit;
      end;

      cvalue := SubString(params, ' ', 3);
      if cvalue = '' then
      begin
        if spamcfg.ReadBool(csec, ckey, True) then
          irc_addtext(Netname, Channel, '<b>[%s] %s:</b> = 1 (Announce)', [csec, ckey])
        else
          irc_addtext(Netname, Channel, '<b>[%s] %s:</b> = 0 (Skip)', [csec, ckey]);

        Result := True;
        exit;
      end;
    finally
      vsval.Free;
    end;
  finally
    vsecs.Free;
  end;

  spamcfg.WriteInteger(csec, ckey, StrToIntDef(cvalue, 0));
  spamcfg.UpdateFile;

  if spamcfg.ReadBool(csec, ckey, True) then
    irc_addtext(Netname, Channel, '<b>[%s] %s:</b> = 1 (Announce)', [csec, ckey])
  else
    irc_addtext(Netname, Channel, '<b>[%s] %s:</b> = 0 (Skip)', [csec, ckey]);

  Result := True;
end;

function Ircaddknowngroup(const netname, channel, params: String): boolean;
var
  section, glist: String;
  y, x: TStringList;
  i: integer;
begin
  Result := False;
  section := UpperCase(SubString(params, ' ', 1));
  glist := mystrings.RightStr(params, length(section) + 1);

  if kb_sections.IndexOf(section) = -1 then
  begin
    irc_addtext(Netname, Channel, 'Section <b>%s</b> not found!', [section]);
    exit;
  end;

  x := TStringList.Create;
  y := TStringList.Create;
  try
    x.Delimiter := ' ';
    x.DelimitedText := glist;
    y.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'slftp.knowngroups');

    for i := 0 to x.Count - 1 do
    begin
      if IsKnownGroup(section, x.Strings[i]) <> grp_known then
        y.Values[section] := y.Values[section] + ' ' + x.Strings[i];
    end;

    y.SaveToFile(ExtractFilePath(ParamStr(0)) + 'slftp.knowngroups');
    KnownGroupsStart;
  finally
    x.Free;
    y.Free;
  end;

  Result := True;
end;

end.