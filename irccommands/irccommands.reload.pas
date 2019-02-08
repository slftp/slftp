unit irccommands.reload;

interface

{ slftp reload commands functions }
function IrcPrereload(const netname, channel, params: String): boolean;
function IrcSkipReload(const netname, channel, params: String): boolean;
function IrcLanguageBaseReload(const netname, channel, params: String): boolean;
function IrcRehashSocks5(const netname, channel, params: String): boolean;
function IrcFakeReload(const netname, channel, params: String): boolean;
function IrcRulesReload(const netname, channel, params: String): boolean;
function IrcReloadGlobalSkipGrouplist(const netname, channel, params: String): boolean;
function IrcKnowngroups(const netname, channel, params: String): boolean;

implementation

uses
  SysUtils, Classes, precatcher, skiplists, sllanguagebase, mslproxys, fake, rulesunit, globalskipunit, knowngroups, irc;

const
  section = 'irccommands.reload';

function IrcPrereload(const netname, channel, params: String): boolean;
var
  vs: String;
begin
  vs := PrecatcherReload;
  if vs <> '' then
    irc_addtext(Netname, Channel, vs);
  Result := True;
end;

function IrcSkipReload(const netname, channel, params: String): boolean;
begin
  try
    Result := SkiplistRehash;
  finally
    irc_addtext(Netname, Channel, 'Skiplist reloaded... (%d entries)', [SkiplistCount]);
  end;
end;

function IrcLanguageBaseReload(const netname, channel, params: String): boolean;
begin
  irc_addtext(Netname, Channel, SLLanguagesReload);
  Result := True;
end;

function IrcRehashSocks5(const netname, channel, params: String): boolean;
begin
  Result := RehashProxys;
end;

function IrcFakeReload(const netname, channel, params: String): boolean;
begin
  Result := FakesRehash;
end;

function IrcRulesReload(const netname, channel, params: String): boolean;
begin
  RulesReload;
  Result := True;
end;

function IrcReloadGlobalSkipGrouplist(const netname, channel, params: String): boolean;
begin
  Result := Rehashglobalskiplist;
  if Result then
    irc_addtext(Netname, Channel, '%d groups in list.', [globalgroupskip.Count]);
end;

function IrcKnowngroups(const netname, channel, params: String): boolean;
begin
  KnownGroupsStart();
  Result := True;
end;

end.