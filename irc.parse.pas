{ @abstract(Parse IRC lines)
  Used to parse IRC lines and split the relevant values into variables }
unit irc.parse;

interface

uses
  SysUtils;

  { Parses a invite line from the IRCd
    @param(aIrcdLine the line sent by the IRCd)
    @param(aChan Channelname)
    @param(aNick Nickname)
    @returns(parsed Channel and Nick) }
  procedure parseIrcINVITE(const aIrcdLine: String; out aChan, aNick: String);
  { Parse channel name from "JOIN :#chan" OR "JOIN #chan"
    @param(aIrcdLine the line sent by the IRCd)
    @param(aChan Channelname)
    @param(aNick Nickname)
    @returns(parsed Channel and Nick) }
  procedure parseIrcJOIN(const aIrcdLine: String; out aChan, aNick: String);
  { Parse the channel name and nick that was kicked
    @param(aIrcdLine the line sent by the IRCd)
    @param(aChan Channelname)
    @param(aNick Nickname)
    @returns(parsed Channel and Nick) }
  procedure parseIrcKICK(const aIrcdLine: String; out aChan, aNick: String);
  { Parse the persons new nick
    @param(aIrcdLine the line sent by the IRCd)
    @param(aNick Nickname)
    @returns(parsed Nick) }
  procedure parseIrcNICK(const aIrcdLine: String; out aNick: String);
  { Parse the channel name and nick that parted
    @param(aIrcdLine the line sent by the IRCd)
    @param(aChan Channelname)
    @param(aNick Nickname)
    @returns(parsed Channel and Nick) }
  procedure parseIrcPART(const aIrcdLine: String; out aChan, aNick: String);
   { Parse the quit reason
    @param(aIrcdLine the line sent by the IRCd)
    @param(aQuit Quit Reason)
    @returns(parsed Quit Reason) } 
  procedure parseIrcQUIT(const aIrcdLine: String; out aQuit: String);
  { Parse the new topic of channel
    @param(aIrcdLine the line sent by the IRCd)
    @param(aChan Channelname)
    @param(aTopic Channel topic)
    @returns(parsed Channel and Topic) }  
  procedure parseIrcTOPIC(const aIrcdLine: String; out aChan, aTopic: String);

implementation

uses
  mystrings;


procedure parseIrcINVITE(const aIrcdLine: String; out aChan, aNick: String);
begin
  // :invitee!ident@ircserver.local INVITE slnick :#test1
  aChan := Copy(SubString(aIrcdLine, ' ', 4), 2, 1000);
  aNick := Copy(SubString(SubString(aIrcdLine, ' ', 1), '!', 1), 2, 100);
end;

procedure parseIrcJOIN(const aIrcdLine: String; out aChan, aNick: String);
begin
  // IRCv3 https://github.com/unrealircd/unrealircd/commit/5c7d89a642782e008dd9ce8576a4a0cc6a7cad35
  // :sltest!ident@ircserver.local JOIN #test1
  // :sltest!ident@LinkNet-v893610cae.isp.this.that JOIN :#test1
  aChan := SubString(aIrcdLine, ' ', 3).TrimLeft([':']);
  aNick := Copy(aIrcdLine, 2, Pos('!', aIrcdLine) - 2);
end;

procedure parseIrcKICK(const aIrcdLine: String; out aChan, aNick: String);
begin
  // :rsc!rsctm@coctail.sda.bme.hu KICK #femforgacs rsctm :no reason
  // :nick!ident@ircserver.local KICK #test1 sltest :sltest
  aChan := SubString(aIrcdLine, ' ', 3);
  aNick := SubString(aIrcdLine, ' ', 4);
end;

procedure parseIrcNICK(const aIrcdLine: String; out aNick: String);
begin
  // :oldnick!ident@ircserver.local NICK :newnick
  aNick := Copy(aIrcdLine, 2, Pos('!', aIrcdLine) - 2);
end;

procedure parseIrcPART(const aIrcdLine: String; out aChan, aNick: String);
begin
  // :sltest!ident@ircserver.local PART #test1
  aChan := SubString(aIrcdLine, ' ', 3);
  aNick := Copy(aIrcdLine, 2, Pos('!', aIrcdLine) - 2);
end;

procedure parseIrcQUIT(const aIrcdLine: String; out aQuit: String);
begin
  // :user!ident@ircserver.local QUIT :Quit: Reason
  aQuit := Copy(aIrcdLine, RPos(':', aIrcdLine) + 1, 1000);
  aQuit := aQuit.TrimLeft([' ']);
end;

procedure parseIrcTOPIC(const aIrcdLine: String; out aChan, aTopic: String);
begin
  // :user!ident@ircserver.local TOPIC #test1 :newtopic
  aTopic := Copy(aIrcdLine, Pos(' :', aIrcdLine), MaxInt);
  aTopic := aTopic.TrimLeft([':', ' ']);
  aChan := SubString(aIrcdLine, ' ', 3);
end;


end.