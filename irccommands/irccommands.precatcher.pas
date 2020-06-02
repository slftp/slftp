unit irccommands.precatcher;

interface

{ slftp precatcher commands functions }
function IrcPrelist(const netname, channel, params: String): boolean;
function IrcPreadd(const netname, channel, params: String): boolean;
function IrcPredel(const netname, channel, params: String): boolean;
function IrcPreCatchtest(const netname, channel, params: String): boolean;
function IrcCatchMod(const netname, channel, params: String): boolean;
function IrcPreCatchDebug(const netname, channel, params: String): boolean;
function IrcDisplayMappings(const netname, channel, params: String): boolean;

implementation

uses
  SysUtils, Classes, Contnrs, precatcher, irc, ircchansettings, sitesunit, mystrings, kb, kb.release;

const
  section = 'irccommands.precatcher';

function IrcPrelist(const netname, channel, params: String): boolean;
var
  i: integer;
  s1, s2: String;
  mehetki: boolean;
  nn, aktchannel, sitename, nick, event, words, section: String;
begin
  Result := False;
  s1 := UpperCase(SubString(params, ' ', 1));
  s2 := SubString(params, ' ', 2);

  if ((s1 <> '') and (s2 <> '')) then
  begin
    if FindIrcChannelSettings(s1, s2) = nil then
    begin
      irc_addtext(Netname, Channel, Format('Cant find channel %s on net %s.', [s2, s1]));
      exit;
    end;
  end
  else if (s1 <> '') then
  begin
    if nil = FindSiteByName(Netname, s1) then
    begin
      irc_addtext(Netname, Channel, 'Cant find site.');
      exit;
    end;
  end;

  for i := 0 to catcherFile.Count - 1 do
  begin
    nn := SubString(catcherFile[i], ';', 1);
    aktchannel := SubString(catcherFile[i], ';', 2);
    nick := SubString(catcherFile[i], ';', 3);
    sitename := SubString(catcherFile[i], ';', 4);
    event := SubString(catcherFile[i], ';', 5);
    words := SubString(catcherFile[i], ';', 6);
    section := SubString(catcherFile[i], ';', 7);

    mehetki := False;
    if ((s1 <> '') and (s2 <> '')) then
    begin
      if ((s1 = nn) and (s2 = aktchannel)) then
        mehetki := True;
    end
    else if (s1 <> '') then
    begin
      if sitename = s1 then
        mehetki := True;
    end
    else
      mehetki := True;

    if mehetki then
      irc_addtext(Netname, Channel, '#%d %s-%s-%s <%s> [%s] {%s} (%s)', [i, sitename, nn, aktchannel, nick, event, words, section]);
  end;

  Result := True;
end;

function IrcPreadd(const netname, channel, params: String): boolean;
var
  sitename, nn, channelname, botnicks, event, words, section: String;
  kb_event: TKBEventType;
begin
  Result := False;

  sitename := UpperCase(SubString(params, ' ', 1));
  nn := UpperCase(SubString(params, ' ', 2));
  channelname := SubString(params, ' ', 3);
  botnicks := SubString(params, ' ', 4);
  event := UpperCase(SubString(params, ' ', 5));
  words := SubString(params, ' ', 6);
  section := SubString(params, ' ', 7);

  kb_event := EventStringToTKBEventType(event);

  if (not (kb_event in [kbePRE, kbeCOMPLETE, kbeNEWDIR, kbeNUKE, kbeREQUEST])) then
  begin
    irc_addtext(Netname, Channel, 'Syntax error, unknown event: ' + event);
    exit;
  end;

  if nil = FindSiteByName(Netname, sitename) then
  begin
    irc_addtext(Netname, Channel, Format('Site %s not found', [sitename]));
    exit;
  end;

  if FindIrcChannelSettings(nn, channelname) = nil then
  begin
    irc_addtext(Netname, Channel, Format('Channel %s not found on net %s.', [channelname, nn]));
    exit;
  end;

  catcherFile.Add(format('%s;%s;%s;%s;%s;%s;%s', [nn, channelname, botnicks, sitename, event, words, section]));
  PrecatcherRebuild;

  Result := True;
end;

function IrcPredel(const netname, channel, params: String): boolean;
var
  i: integer;
begin
  Result := False;
  i := StrToIntDef(params, -1);
  if i < 0 then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  if catcherFile.Count > i then
    catcherFile.Delete(i);

  PrecatcherRebuild();

  Result := True;
end;

function IrcPrecatchtest(const netname, channel, params: String): boolean;
var
  net, chan, nick, rest: String;
begin
  Result := False;

  net := UpperCase(SubString(params, ' ', 1));
  chan := SubString(params, ' ', 2);
  nick := SubString(params, ' ', 3);
  rest := mystrings.RightStr(params, length(net) + length(chan) + length(nick) + 3);

  if FindIrcChannelSettings(net, chan) = nil then
  begin
    irc_addtext(Netname, Channel, 'Syntax error: %s@%s not found', [net, chan]);
    exit;
  end;

  precatcher_debug := True;
  precatcher_debug_netname := Netname;
  precatcher_debug_channel := Channel;
  PrecatcherProcessB(net, chan, nick, rest);
  precatcher_debug := False;

  Result := True;
end;

function IrcCatchMod(const netname, channel, params: String): boolean;
var
  index, sitename, nn, channelname, botnicks, event, words, section: String;
  kb_event: TKBEventType;
begin
  Result := False;
  index := UpperCase(SubString(params, ' ', 1));
  sitename := UpperCase(SubString(params, ' ', 2));
  nn := UpperCase(SubString(params, ' ', 3));
  channelname := SubString(params, ' ', 4);
  botnicks := SubString(params, ' ', 5);
  event := UpperCase(SubString(params, ' ', 6));
  words := SubString(params, ' ', 7);
  section := SubString(params, ' ', 8);

  if ((index = '') or (StrToIntDef(index, -1) = -1)) then
  begin
    irc_addtext(Netname, Channel, 'Syntax error, index: ' + index);
    Exit;
  end;

  kb_event := EventStringToTKBEventType(event);

  if (not (kb_event in [kbePRE, kbeCOMPLETE, kbeNEWDIR, kbeNUKE, kbeREQUEST])) then
  begin
    irc_addtext(Netname, Channel, 'Syntax error, unknown event: ' + event);
    exit;
  end;

  if nil = FindSiteByName(Netname, sitename) then
  begin
    irc_addtext(Netname, Channel, 'Site not found');
    exit;
  end;

  if FindIrcChannelSettings(nn, channelname) = nil then
  begin
    irc_addtext(Netname, Channel, Format('Channel %s not found on net %s.', [channelname, nn]));
    exit;
  end;
  try
    catcherFile.Delete(StrToInt(index));
  except
    on E: Exception do
    begin
      irc_AddAdmin(format('<c4>[Exception]</c> in IrcCatchMod.catcherFile.Delete: %s',
        [E.Message]));
      Exit;
    end;
  end;

  try
    catcherfile.Insert(StrToInt(index), format('%s;%s;%s;%s;%s;%s;%s',
      [nn, channelname, botnicks, sitename, event, words, section]));
  except
    on E: Exception do
    begin
      irc_AddAdmin(format('<c4>[Exception]</c> in IrcCatchMod.catcherfile.Insert: %s',
        [E.Message]));
      Exit;
    end;
  end;

  try
    PrecatcherRebuild();
  except
    on E: Exception do
    begin
      irc_AddAdmin(format('<c4>[Exception]</c> in IrcCatchMod.catcherfile.Insert: %s',
        [E.Message]));
      Exit;

    end;
  end;

  Result := True;
end;

function IrcPreCatchDebug(const netname, channel, params: String): boolean;
begin
  if params <> '' then
  begin
    if params = '0' then
      precatcher_ircdebug := False;

    if params = '1' then
      precatcher_ircdebug := True;

    irc_addtext(Netname, Channel, 'CatchDebug is: ' + BoolToStr(precatcher_ircdebug, True));
  end
  else
    irc_addtext(Netname, Channel, 'CatchDebug is: ' + BoolToStr(precatcher_ircdebug, True));

  Result := True;
end;

function IrcDisplayMappings(const netname, channel, params: String): boolean;
var
  i: integer;
begin
  irc_addtext(Netname, Channel, 'Listing %d entries...', [mappingslist.Count]);
  try
    for i := 0 to mappingslist.Count - 1 do
      irc_addtext(Netname, Channel, '%s -> %s <c4>if</c> %s ',
        [TMap(mappingslist.Items[i]).origsection, TMap(mappingslist.Items[i])
        .newsection, TMap(mappingslist.Items[i]).mask.mask]);
  except
    on E: Exception do
      irc_addtext(Netname, Channel, '<c4><b>ERROR</c></b>: %s', [E.Message]);
  end;
  Result := True;
end;

end.
