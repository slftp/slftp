unit irccommands.irc;

interface

{ slftp irc commands functions }
function IrcStatus(const netname, channel, params: String): boolean;
function IrcSay(const netname, channel, params: String): boolean;
function IrcJump(const netname, channel, params: String): boolean;
function IrcOper(const netname, channel, params: String): boolean;
function IrcNetNoSocks5(const netname, channel, params: String): boolean;
function IrcShowNet(const netname, channel, params: String): boolean;
function IrcAddnet(const netname, channel, params: String): boolean;
function IrcModnet(const netname, channel, params: String): boolean;
function IrcModesNet(const netname, channel, params: String): boolean;
function IrcDelnet(const netname, channel, params: String): boolean;
function IrcNetAddServer(const netname, channel, params: String): boolean;
function IrcNetDelServer(const netname, channel, params: String): boolean;
function IrcNetAddPerform(const netname, channel, params: String): boolean;
function IrcNetDelPerform(const netname, channel, params: String): boolean;
function IrcNetListPerform(const netname, channel, params: String): boolean;
function IrcNetDoPerform(const netname, channel, params: String): boolean;
function IrcChannels(const netname, channel, params: String): boolean;
function IrcChanAdd(const netname, channel, params: String): boolean;
function IrcDelchan(const netname, channel, params: String): boolean;
function IrcSetBlowkey(const netname, channel, params: String): boolean;
function IrcSetChankey(const netname, channel, params: String): boolean;
function IrcSetChanName(const netname, channel, params: String): boolean;
function IrcDelPart(const netname, channel, params: String): boolean;
function IrcSetMYIrcNick(const netname, channel, params: String): boolean;
function IrcInviteMyIRCNICK(const netname, channel, params: String): boolean;

implementation

uses
  SysUtils, Classes, StrUtils, Types, Contnrs, irc, sitesunit, ircchansettings,
  ircblowfish.ECB, ircblowfish.CBC, configunit, mainthread, mystrings, irccommandsunit;

const
  section = 'irccommands.irc';

function IrcStatus(const netname, channel, params: String): boolean;
var
  i: integer;
  th: TMyIrcThread;
begin
  for i := 0 to myIrcThreads.Count - 1 do
  begin
    th := TMyIrcThread(myIrcThreads[i]);
    // channel-FREE-#SM
    irc_addtext(Netname, Channel, format('%s (%s:%d): %s', [th.Netname, th.host, th.port, th.status]));
  end;

  Result := True;
end;

function IrcSay(const netname, channel, params: String): boolean;
var
  nn, blowchannel, tosay: String;
begin
  Result := False;
  nn := UpperCase(SubString(params, ' ', 1));
  blowchannel := SubString(params, ' ', 2);
  tosay := mystrings.RightStr(params, length(nn) + length(blowchannel) + 2);

  if FindIrcChannelSettings(nn, blowchannel) = nil then
  begin
    irc_addtext(Netname, Channel, 'Cant find channel.');
    exit;
  end;

  irc_addtext(nn, blowchannel, tosay);

  Result := True;
end;

function IrcJump(const netname, channel, params: String): boolean;
var
  ircth: TMyIrcThread;
  fParams: String;
begin
  fParams := UpperCase(Trim(params));

  ircth := FindIrcnetwork(fParams);
  if ircth <> nil then
  begin
    ircth.shouldrestart := True;
    myIrcThreads.Remove(ircth);
    myIrcThreads.Add(TMyIrcThread.Create(fParams));
  end;

  Result := True;
end;

function IrcOper(const netname, channel, params: String): boolean;
var
  nn, userid, pass: String;
begin
  Result := False;

  nn := UpperCase(SubString(params, ' ', 1));
  userid := SubString(params, ' ', 2);
  pass := SubString(params, ' ', 3);

  if nil = FindIrcnetwork(nn) then
  begin
    irc_addtext(Netname, Channel, 'IRC Net %s not found!', [nn]);
    exit;
  end;

  if userid = '' then
  begin
    // query mode
    userid := sitesdat.ReadString('ircnet-' + nn, 'oper_userid', '');
    pass := sitesdat.ReadString('ircnet-' + nn, 'oper_password', '');
    if userid <> '' then
      irc_addtext(Netname, Channel, 'IRC oper settings for %s are: %s %s', [nn, userid, pass])
    else
      irc_addtext(Netname, Channel, 'IRC oper settings for %s are turned off.', [nn]);
  end
  else if userid = '-' then
  begin
    // delete mode
    sitesdat.DeleteKey('ircnet-' + nn, 'oper_userid');
    sitesdat.DeleteKey('ircnet-' + nn, 'oper_password');
    irc_addtext(Netname, Channel, 'IRC oper settings for %s are now deleted.', [nn]);
  end
  else
  begin
    // set mode
    sitesdat.WriteString('ircnet-' + nn, 'oper_userid', userid);
    sitesdat.WriteString('ircnet-' + nn, 'oper_password', pass);
  end;

  Result := True;
end;

function IrcNetNoSocks5(const netname, channel, params: String): boolean;
var
  nname, Value: String;
  status: boolean;
begin
  nname := SubString(params, ' ', 1);
  Value := SubString(params, ' ', 2);
  status := boolean(StrToInt(Value));

  sitesdat.WriteBool('ircnet-' + nname, 'nosocks5', status);

  Result := True;
end;

function IrcShownet(const netname, channel, params: String): boolean;
var
  nn, host: String;
  x: TStringList;
  i: integer;
  trigger: String;
begin
  Result := False;
  nn := UpperCase(SubString(params, ' ', 1));
  trigger := SubString(params, ' ', 2);

  if nil = FindIrcnetwork(nn) then
  begin
    irc_addtext(Netname, Channel, 'Network with name %s does not exists!', [nn]);
    exit;
  end;

  irc_addtext(Netname, Channel, 'IRC network: ' + nn);

  x := TStringList.Create;
  try
    sitesdat.ReadSection('ircnet-' + nn, x);
    x.Sort;
    for i := 0 to x.Count - 1 do
    begin
      if Copy(x[i], 1, 3) <> 'bnc' then
      begin
        if ((x[i] = 'password') and (trigger <> '--plain')) then
          Continue;
        irc_addtext(Netname, Channel, ' %s: %s', [x[i], sitesdat.ReadString('ircnet-' + nn,
            x[i], '')]);
      end;
    end;
  finally
    x.Free;
  end;

  i := 0;
  while (not slshutdown) do
  begin
    host := sitesdat.ReadString('ircnet-' + nn, 'bnc_host-' + IntToStr(i), '');
    if host = '' then
      break;
    irc_addtext(Netname, Channel, ' bnc: %s:%d', [host, sitesdat.ReadInteger('ircnet-' + nn, 'bnc_port-' + IntToStr(i), 0)]);
    Inc(i);
  end;

  Result := True;
end;

function IrcAddnet(const netname, channel, params: String): boolean;
var
  nn, host, password, user, ident, nick: String;
  port: integer;
  ssl: integer;
begin
  Result := False;

  nn := UpperCase(SubString(params, ' ', 1));
  if (0 < Pos('-', nn)) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;
  if (nn = 'CONSOLE') then
  begin
    irc_addtext(Netname, Channel, 'You cant add a network called CONSOLE');
    exit;
  end;

  host := SubString(params, ' ', 2);
  port := StrToIntDef(SubString(host, ':', 2), 0);
  if port <= 0 then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;
  host := SubString(host, ':', 1);
  ssl := StrToIntDef(SubString(params, ' ', 3), -1);
  if ((ssl < 0) or (ssl > 1)) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  if nil <> FindIrcnetwork(nn) then
  begin
    irc_addtext(Netname, Channel, 'Network with name %s already exists!', [nn]);
    exit;
  end;

  password := SubString(params, ' ', 4);
  nick := SubString(params, ' ', 5);
  ident := SubString(params, ' ', 6);
  user := SubString(params, ' ', 7);

  if nick = '' then
    nick := config.ReadString('irc', 'nickname', 'slftp');
  if user = '' then
    user := config.ReadString('irc', 'username', 'slftp');
  if ident = '' then
    ident := config.ReadString('irc', 'realname', 'slftp');

  sitesdat.WriteString('ircnet-' + nn, 'host', host);
  sitesdat.WriteInteger('ircnet-' + nn, 'port', port);
  sitesdat.WriteBool('ircnet-' + nn, 'ssl', boolean(ssl));
  sitesdat.WriteString('ircnet-' + nn, 'password', password);
  sitesdat.WriteString('ircnet-' + nn, 'nick', nick);
  sitesdat.WriteString('ircnet-' + nn, 'anick', '_' + nick);
  sitesdat.WriteString('ircnet-' + nn, 'ident', ident + '@soulless.ftp');
  sitesdat.WriteString('ircnet-' + nn, 'username', user);

  myIrcThreads.Add(TMyIrcThread.Create(nn));

  Result := True;
end;

function IrcModnet(const netname, channel, params: String): boolean;
var
  nn, password: String;
  ssl: integer;
begin
  Result := False;

  nn := UpperCase(SubString(params, ' ', 1));
  if (0 < Pos('-', nn)) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  ssl := StrToIntDef(SubString(params, ' ', 2), -1);
  if ((ssl < 0) or (ssl > 1)) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;
  password := SubString(params, ' ', 3);

  if nil = FindIrcnetwork(nn) then
  begin
    irc_addtext(Netname, Channel, 'Network with name %s doesnt exists!', [nn]);
    exit;
  end;

  sitesdat.WriteBool('ircnet-' + nn, 'ssl', boolean(ssl));
  sitesdat.WriteString('ircnet-' + nn, 'password', password);

  IrcJump(Netname, Channel, nn);

  Result := True;
end;

function IrcModesNet(const netname, channel, params: String): boolean;
var
  mode, n_modes, nn: String;
  mlist: TStringlist;
  I: Integer;
  ircn: TMyIrcThread;
begin
  result := False;
  nn := UpperCase(SubString(params, ' ', 1));
  n_modes := mystrings.RightStr(params, length(nn) + 1);

  ircn := FindIrcnetwork(nn);
  if nil = ircn then
  begin
    irc_addtext(Netname, Channel, '<c5><b>ERROR</b></c>: Network with name <b>%s</b> doesnt exists!', [nn]);
    exit;
  end;

  //some more checks to inform the user what slftp have changed?

  mlist := TStringlist.Create;
  try
    mlist.Delimiter := ' ';
    mlist.DelimitedText := n_modes;
    for I := 0 to mlist.Count - 1 do
    begin
      mode := mlist.Strings[i];

      if length(mode) = 2 then
      begin

        if ((mode[1] = '+') and (mode[2] = 'h')) then
          ircn.MangleHost := True;
        if ((mode[1] = '-') and (mode[2] = 'h')) then
          ircn.MangleHost := False;

        if ((mode[1] = '+') and (mode[2] = 'i')) then
          ircn.Invisible := True;
        if ((mode[1] = '-') and (mode[2] = 'i')) then
          ircn.Invisible := False;

      end;
    end;
  finally
    mlist.free;
  end;

  result := True;
end;

function IrcDelnet(const netname, channel, params: String): boolean;
var
  i, ii: integer;
  s: TSite;
  ircth: TMyIrcThread;
  fChanSettings: TIrcChannelSettings;
  x: TStringList;
  fParams: String;
begin
  fParams := UpperCase(Trim(params));
  ircth := FindIrcnetwork(fParams);

  x := TStringList.Create;
  try
    if ircth <> nil then
    begin
      try
        sitesdat.ReadSection('ircnet-' + fParams, x);
        for ii := 0 to x.Count - 1 do
          sitesdat.DeleteKey('ircnet-' + fParams, x.Strings[ii]);
        sitesdat.EraseSection('ircnet-' + fParams);
        myIrcThreads.Remove(ircth);
      except
        on E: Exception do
          irc_addtext(Netname, Channel, 'Erase <b>irc-net</b> failed : %s', [E.Message]);
      end;
    end;

    // now we have to wipe this network out of the sites
    try
      for i := 0 to sites.Count - 1 do
      begin
        s := sites[i] as TSite;
        if s.RCString('ircnet', '') = fParams then
        begin
          s.DeleteKey('ircnet');
        end;
      end;
    except
      on E: Exception do
        irc_addtext(Netname, Channel, 'Erase <b>ircnet from sites</b> failed : %s',[E.Message]);
    end;

    // you need to delete your channel now
    try
      for fChanSettings in IrcChanSettingsList.Values do
      begin
        if fChanSettings.Netname = fParams then
        begin
          sitesdat.EraseSection('channel-' + fChanSettings.Netname + '-' + fChanSettings.Channel);
          IrcChanSettingsList.Remove(fChanSettings.Netname + fChanSettings.Channel);
        end;
      end;
    except
      on E: Exception do
        irc_addtext(Netname, Channel, 'Erase <b>channels</b> failed : %s', [E.Message]);
    end;
  finally
    x.Free;
  end;

  Result := True;
end;

function IrcNetAddServer(const netname, channel, params: String): boolean;
var
  nn: String;
  aktbnc, bnc: String;
  bnchost: String;
  bncport: integer;
  i: integer;
begin
  Result := False;
  nn := UpperCase(SubString(params, ' ', 1));
  bnc := SubString(params, ' ', 2);
  bnchost := SubString(bnc, ':', 1);
  bncport := StrToIntDef(SubString(bnc, ':', 2), 0);

  if (bnchost = '') or (bncport = 0) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  if FindIrcnetwork(nn) = nil then
  begin
    irc_addtext(Netname, Channel, 'IRC net <b>%s</b> not found.', [nn]);
    exit;
  end;

  i := 0;
  while (True) do
  begin
    aktbnc := sitesdat.ReadString('ircnet-' + nn, 'bnc_host-' + IntToStr(i),
      '');
    if (aktbnc = '') then
      break;
    Inc(i);
  end;
  sitesdat.WriteString('ircnet-' + nn, 'bnc_host-' + IntToStr(i), bnchost);
  sitesdat.WriteInteger('ircnet-' + nn, 'bnc_port-' + IntToStr(i), bncport);

  Result := True;
end;

function IrcNetDelServer(const netname, channel, params: String): boolean;
var
  nn: String;
  bnc: String;
  aktbnchost, bnchost: String;
  aktbncport, bncport: integer;
  i: integer;
  megvan: boolean;
begin
  Result := False;
  nn := UpperCase(SubString(params, ' ', 1));
  bnc := SubString(params, ' ', 2);
  bnchost := SubString(bnc, ':', 1);
  bncport := StrToIntDef(SubString(bnc, ':', 2), 0);

  if (bnchost = '') or (bncport = 0) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  if FindIrcnetwork(nn) = nil then
  begin
    irc_addtext(Netname, Channel, 'IRC network <b>%s</b> not found.', [nn]);
    exit;
  end;

  i := 0;
  megvan := False;
  while (True) do
  begin
    aktbnchost := sitesdat.ReadString('ircnet-' + nn, 'bnc_host-' +
      IntToStr(i), '');
    aktbncport := sitesdat.ReadInteger('ircnet-' + nn, 'bnc_port-' +
      IntToStr(i), 0);
    if (aktbnchost = '') then
      break;

    if (not megvan) then
    begin
      if (aktbnchost = bnchost) and (aktbncport = bncport) then
      begin
        megvan := True;
        sitesdat.DeleteKey('ircnet-' + nn, 'bnc_host-' + IntToStr(i));
        sitesdat.DeleteKey('ircnet-' + nn, 'bnc_port-' + IntToStr(i));
      end;
    end
    else
    begin
      sitesdat.DeleteKey('ircnet-' + nn, 'bnc_host-' + IntToStr(i));
      sitesdat.DeleteKey('ircnet-' + nn, 'bnc_port-' + IntToStr(i));
      sitesdat.WriteString('ircnet-' + nn, 'bnc_host-' + IntToStr(i - 1),
        aktbnchost);
      sitesdat.WriteInteger('ircnet-' + nn, 'bnc_port-' + IntToStr(i - 1),
        aktbncport);
    end;

    Inc(i);
  end;
  if (not megvan) then
  begin
    irc_addtext(Netname, Channel, 'Bnc not found.');
    exit;
  end;

  Result := True;
end;

function IrcNetAddPerform(const netname, channel, params: String): boolean;
var
  nn: String;
  aktperform, Perform: String;
  i: integer;
begin
  Result := False;
  nn := UpperCase(SubString(params, ' ', 1));
  Perform := mystrings.RightStr(params, length(nn) + 1);

  if FindIrcnetwork(nn) = nil then
  begin
    irc_addtext(Netname, Channel, 'IRC net <b>%s</b> not found.', [nn]);
    exit;
  end;

  i := 0;
  while (True) do
  begin
    aktperform := sitesdat.ReadString('ircnet-' + nn, 'perform-' + IntToStr(i),
      '');
    if (aktperform = '') then
      break;
    Inc(i);
  end;
  sitesdat.WriteString('ircnet-' + nn, 'perform-' + IntToStr(i), Perform);

  Result := True;
end;

function IrcNetDelPerform(const netname, channel, params: String): boolean;
var
  nn: String;
  aktperform: integer;
  Perform: String;
  i: integer;
  megvan: boolean;
begin
  Result := False;
  nn := UpperCase(SubString(params, ' ', 1));
  aktperform := StrToIntDef(SubString(params, ' ', 2), -1);

  if (aktperform = -1) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  if FindIrcnetwork(nn) = nil then
  begin
    irc_addtext(Netname, Channel, 'IRC network <b>%s</b> not found.', [nn]);
    exit;
  end;

  i := 0;
  megvan := False;
  while (True) do
  begin
    Perform := sitesdat.ReadString('ircnet-' + nn, 'perform-' + IntToStr(i),
      '');
    if (Perform = '') then
      break;

    if (not megvan) then
    begin
      if (aktperform = i) then
      begin
        megvan := True;
        sitesdat.DeleteKey('ircnet-' + nn, 'perform-' + IntToStr(i));
      end;
    end
    else
    begin
      sitesdat.DeleteKey('ircnet-' + nn, 'perform-' + IntToStr(i));
      sitesdat.WriteString('ircnet-' + nn,
        'perform-' + IntToStr(i - 1), Perform);
    end;

    Inc(i);
  end;
  if (not megvan) then
  begin
    irc_addtext(Netname, Channel, 'Perform not found.');
    exit;
  end;

  Result := True;
end;

function IrcNetListPerform(const netname, channel, params: String): boolean;
var
  nn: String;
  aktperform: String;
  i: integer;
begin
  Result := False;
  nn := UpperCase(SubString(params, ' ', 1));

  if FindIrcnetwork(nn) = nil then
  begin
    irc_addtext(Netname, Channel, 'IRC net <b>%s</b> not found.', [nn]);
    exit;
  end;

  i := 0;
  while (True) do
  begin
    aktperform := sitesdat.ReadString('ircnet-' + nn, 'perform-' + IntToStr(i),
      '');
    if (aktperform = '') then
      break;
    irc_addtext(Netname, Channel, 'Perform <b>%s</b>(%d) : %s',
      [nn, i, aktperform]);
    Inc(i);
  end;

  Result := True;
end;

function IrcNetDoPerform(const netname, channel, params: String): boolean;
var
  nn: String;
  nnth: TMyIrcThread;
  aktperform: String;
  i: integer;
begin
  Result := False;
  nn := UpperCase(SubString(params, ' ', 1));

  if FindIrcnetwork(nn) = nil then
  begin
    irc_addtext(Netname, Channel, 'IRC net <b>%s</b> not found.', [nn]);
    exit;
  end;

  nnth := FindIrcnetwork(nn);

  i := 0;
  while (True) do
  begin
    aktperform := sitesdat.ReadString('ircnet-' + nn, 'perform-' + IntToStr(i),
      '');
    if (aktperform = '') then
      break;
    if not nnth.IrcWrite(aktperform) then
    begin
      irc_addtext(Netname, Channel, 'Error Perform <b>%s</b>(%d) : %s',
        [nn, i, aktperform]);
    end
    else
    begin
      irc_addtext(Netname, Channel, 'Perform <b>%s</b>(%d) : %s',
        [nn, i, aktperform]);
    end;
    Inc(i);
  end;

  Result := True;
end;

function IrcChannels(const netname, channel, params: String): boolean;
var
  fChanSettings: TIrcChannelSettings;
  fNetworkName, fMode, fBlowkey: String;
begin
  fNetworkName := UpperCase(Trim(params));

  for fChanSettings in IrcChanSettingsList.Values do
  begin
    if (fNetworkName = '') or (fNetworkName = fChanSettings.Netname) then
    begin
      if UpperCase(fChanSettings.ClassName) = UpperCase('TIrcBlowkeyCBC') then
      begin
        fMode := 'CBC mode';
        fBlowkey := StringOf((fChanSettings as TIrcBlowkeyCBC).Blowkey);
      end
      else if UpperCase(fChanSettings.ClassName) = UpperCase('TIrcBlowkeyECB') then
      begin
        fMode := 'ECB mode';
        fBlowkey := (fChanSettings as TIrcBlowkeyECB).Blowkey;
      end
      else
      begin
        fMode := 'plaintext mode';
        fBlowkey := '';
      end;

      irc_addtext_b(Netname, Channel, Format('%s@%s -> %s blowkey(%s) chankey(%s)',
        [fChanSettings.Channel, fChanSettings.Netname, fMode, fBlowkey, fChanSettings.ChanKey]));
    end;
  end;

  Result := True;
end;

function IrcChanAdd(const netname, channel, params: String): boolean;
var
  nn, blowchannel: String;
  fChanSettings: TIrcChannelSettings;
  ircth: TMyIrcThread;
begin
  Result := False;
  nn := UpperCase(SubString(params, ' ', 1));
  blowchannel := SubString(params, ' ', 2);

  ircth := FindIrcnetwork(nn);
  if ircth = nil then
  begin
    irc_addtext(Netname, Channel, 'Cant find network');
    exit;
  end;

  fChanSettings := FindIrcChannelSettings(nn, blowchannel);
  if fChanSettings = nil then
  begin
    sitesdat.WriteString('channel-' + nn + '-' + blowchannel, 'blowkey', '');
    RegisterChannelSettings(nn, blowchannel, '', '');
    ircth.shouldjoin := True;
  end
  else
    irc_addtext_b(Netname, Channel, format('Channel %s@%s is already added', [blowchannel, Netname]));

  Result := True;
end;

function IrcDelchan(const netname, channel, params: String): boolean;
var
  nn, blowchannel: String;
  fChanSettings: TIrcChannelSettings;
  ircth: TMyIrcThread;
begin
  Result := False;
  nn := UpperCase(SubString(params, ' ', 1));
  blowchannel := SubString(params, ' ', 2);

  ircth := FindIrcnetwork(nn);
  if ircth = nil then
  begin
    irc_addtext(Netname, Channel, 'Network not found.');
    exit;
  end;

  fChanSettings := FindIrcChannelSettings(nn, blowchannel);
  if fChanSettings <> nil then
  begin
    ircth.chanpart(blowchannel, ircth.BotNick);
    IrcChanSettingsList.Remove(fChanSettings.Netname + fChanSettings.Channel);
    sitesdat.EraseSection('channel-' + nn + '-' + blowchannel);
    ircth.shouldjoin := True;
  end
  else
    irc_addtext_b(Netname, Channel, format('Channel %s@%s not found, spelled channel exactly?', [blowchannel, nn]));

  Result := True;
end;

function IrcSetBlowkey(const netname, channel, params: String): boolean;
var
  nn, blowchannel, key, fChankey, fBlowkey, fChanroles, fChannel, fNetname: String;
  fChanSettings: TIrcChannelSettings;
  ircth: TMyIrcThread;
  cbc, fInviteonly: boolean;
begin
  Result := False;
  nn := UpperCase(SubString(params, ' ', 1));
  blowchannel := SubString(params, ' ', 2);
  key := mystrings.RightStr(params, length(nn) + length(blowchannel) + 2);
  cbc := False;

  // for CBC, key must start with 'cbc:' for setup compatibility with other software
  if {$IFDEF UNICODE}StartsText{$ELSE}AnsiStartsText{$ENDIF}('cbc:', key) then
  begin
   Delete(key, 1, 4);
   cbc := True;
  end;

  ircth := FindIrcnetwork(nn);
  if ircth = nil then
  begin
    irc_addtext(Netname, Channel, 'Cant find network');
    exit;
  end;

  fChanSettings := FindIrcChannelSettings(nn, blowchannel);
  if fChanSettings <> nil then
  begin
    fNetname := fChanSettings.Netname;
    fChannel := fChanSettings.Channel;

    sitesdat.WriteString('channel-' + fNetname + '-' + fChannel, 'blowkey', key);
    sitesdat.WriteBool('channel-' + fNetname + '-' + fChannel, 'cbc', cbc);

    // remove entry from list to create proper blowfish class
    IrcChanSettingsList.Remove(fChanSettings.Netname + fChanSettings.Channel);

    fChanroles := sitesdat.ReadString('channel-' + fNetname + '-' + fChannel, 'names', '');
    fBlowkey := sitesdat.ReadString('channel-' + fNetname + '-' + fChannel, 'blowkey', '');
    fChankey := sitesdat.ReadString('channel-' + fNetname + '-' + fChannel, 'chankey', '');
    fInviteonly := sitesdat.ReadBool('channel-' + fNetname + '-' + fChannel, 'inviteonly', False);
    cbc := sitesdat.ReadBool('channel-' + fNetname + '-' + fChannel, 'cbc', False);

    RegisterChannelSettings(fNetname, fChannel, fChanroles, fBlowkey, fChankey, fInviteonly, cbc);
  end
  else
    irc_addtext_b(Netname, Channel, format('Channel %s@%s not found', [blowchannel, nn]));

  Result := True;
end;

function IrcSetChankey(const netname, channel, params: String): boolean;
var
  nn, blowchannel, key: String;
  fChanSettings: TIrcChannelSettings;
  ircth: TMyIrcThread;
begin
  Result := False;
  nn := UpperCase(SubString(params, ' ', 1));
  blowchannel := SubString(params, ' ', 2);
  key := mystrings.RightStr(params, length(nn) + length(blowchannel) + 2);

  ircth := FindIrcnetwork(nn);
  if ircth = nil then
  begin
    irc_addtext(Netname, Channel, 'Cant find network');
    exit;
  end;

  fChanSettings := FindIrcChannelSettings(nn, blowchannel);
  if fChanSettings <> nil then
  begin
    fChanSettings.ChanKey := key;
    sitesdat.WriteString('channel-' + nn + '-' + blowchannel, 'chankey', key);
    ircth.shouldjoin := True;
  end
  else
    irc_addtext_b(Netname, Channel, format('Channel %s@%s not found', [blowchannel, nn]));

  Result := True;
end;

function IrcSetChanName(const netname, channel, params: String): boolean;
var
  nn, blowchannel, Names: String;
  fChanSettings: TIrcChannelSettings;
  ircth: TMyIrcThread;
  y: TStringList;
  i: integer;

  function Check_For_Vailed_Chanrole(const Name: String): boolean;
  var
    i: integer;
  begin
    Result := False;
    for i := 0 to integer(irc_chanroleindex) do
      if irc_chanroles[i] = Name then
      begin
        Result := True;
        break;
      end;
  end;

begin
  Result := False;
  nn := UpperCase(SubString(params, ' ', 1));
  blowchannel := SubString(params, ' ', 2);
  Names := UpperCase(mystrings.RightStr(params, length(nn) + length(blowchannel) + 2));

  ircth := FindIrcnetwork(nn);
  if ircth = nil then
  begin
    irc_addtext(Netname, Channel, '<c4><b>ERROR</c>:</b> Cant find network %s', [nn]);
    exit;
  end;

  fChanSettings := FindIrcChannelSettings(nn, blowchannel);
  if fChanSettings = nil then
  begin
    irc_addtext_b(Netname, Channel, format('Channel %s@%s not found.', [blowchannel, nn]));
    exit;
  end;

  if Names = '' then
  begin
    irc_addtext_b(Netname, Channel, format('Channel name(s): %s', [fChanSettings.ChanRoles]));
    Result := True;
    exit;
  end;

  if Names = '-' then
  begin
    fChanSettings.ChanRoles := '';
    sitesdat.DeleteKey('channel-' + nn + '-' + blowchannel, 'names');
    Result := True;
    exit;
  end;

  y := TStringList.Create;
  try
    y.Delimiter := ' ';
    y.DelimitedText := Names;

    for i := 0 to y.Count - 1 do
      if not Check_For_Vailed_Chanrole(y.Strings[i]) then
      begin
        irc_addtext(Netname, Channel, '<c4><b>ERROR</c>:</b> %s is not a valid chanrole.', [y.Strings[i]]);
        Result := False;
        exit;
      end;

    if Names = '' then
    begin
      irc_addtext_b(Netname, Channel, format('Channel name(s): %s', [fChanSettings.ChanRoles]));
    end
    else if Names = '-' then
    begin
      fChanSettings.ChanRoles := '';
      sitesdat.DeleteKey('channel-' + nn + '-' + blowchannel, 'names');
    end
    else
    begin
      fChanSettings.ChanRoles := Names;
      sitesdat.WriteString('channel-' + nn + '-' + blowchannel, 'names', Names);
    end;
  finally
    y.Free;
  end;

  Result := True;
end;

function IrcDelPart(const netname, channel, params: String): boolean;
var
  nn, blowchannel: String;
  fChanSettings: TIrcChannelSettings;
  ircth: TMyIrcThread;
begin
  Result := False;
  nn := UpperCase(SubString(params, ' ', 1));
  blowchannel := SubString(params, ' ', 2);

  ircth := FindIrcnetwork(nn);
  if ircth = nil then
  begin
    irc_addtext(Netname, Channel, 'Network not found.');
    exit;
  end;

  fChanSettings := FindIrcChannelSettings(nn, blowchannel);
  if fChanSettings <> nil then
  begin
    ircth.chanpart(blowchannel, ircth.BotNick);
    IrcChanSettingsList.Remove(fChanSettings.Netname + fChanSettings.Channel);
    ircth.shouldjoin := True;
  end
  else
    irc_addtext_b(Netname, Channel, format('Channel %s@%s not found', [blowchannel, nn]));

  Result := True;
end;

function IrcSetMYIrcNick(const netname, channel, params: String): boolean;
var
  ircnick, sname: String;
  s: TSite;
begin
  Result := False;
  sname := UpperCase(SubString(params, ' ', 1));
  ircnick := SubString(params, ' ', 2);

  s := FindSiteByName('', sname);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Error</c></b>: Site %s not found!', [sname]);
    exit;
  end;
  s.ircnick := ircnick;

  Result := True;
end;

function IrcInviteMyIRCNICK(const netname, channel, params: String): boolean;
var
  s: TSite;
  x: TStringList;
  i: Integer;
begin
  Result := False;

  if params = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      s := TSite(sites.Items[i]);
      if s = nil then
        Continue;
      if Uppercase(s.Name) = getAdminSiteName then
        Continue;
      if s.PermDown then
        Continue;

      if s.IRCNick = '' then
      begin
        Irc_AddText(Netname, Channel, 'Please set your irc nick first! Maybe %sircnick %s %s', [irccmdprefix, s.Name, s.UserName]);
      end
      else
      begin
        Irc_AddText(Netname, Channel, 'Invitation sent inquiry to %s with irc nick %s', [s.Name, s.IRCNick]);
        RawB(Netname, Channel, s.Name, '/', Format('SITE INVITE %s', [s.IRCNick]));
      end
    end;
    Result := True;
  end
  else
  begin
    x := TStringList.Create;
    try
      x.Delimiter := ' ';
      x.DelimitedText := UpperCase(params);
      if x.Count > 0 then
      begin

        for i := 0 to x.Count - 1 do
        begin
          s := FindSiteByName(Netname, x[i]);
          if s = nil then
          begin
            irc_addtext(Netname, Channel, '<c4><b>Error</c></b>: Site %s not found!', [x[i]]);
            Continue;
          end
          else
          begin
            if Uppercase(s.Name) = getAdminSiteName then
              Continue;
            if s.PermDown then
              Continue;

            if s.IRCNick = '' then
            begin
              Irc_AddText(Netname, Channel, 'Please set your irc nick first! Maybe %sircnick %s %s', [irccmdprefix, s.Name, s.UserName]);
            end
            else
            begin
              Irc_AddText(Netname, Channel, 'Invitation sent inquiry to %s with irc nick %s', [s.Name, s.IRCNick]);
              RawB(Netname, Channel, s.Name, '/', Format('SITE INVITE %s', [s.IRCNick]));
            end;
          end;

        end;
        irc_addtext(Netname, Channel, 'All Done...');
      end;
    finally
      x.Free;
    end;
    Result := True;
  end;
end;

end.