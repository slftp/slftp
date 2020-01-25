unit irccommands.windows;

interface

{ slftp windows commands functions }
function IrcShowWindow(const netname, channel, params: String): boolean;
function IrcShowWindows(const netname, channel, params: String): boolean;
function IrcDelWindow(const netname, channel, params: String): boolean;
function IrcIrcNames(const netname, channel, params: String): boolean;
function IrcRepaint(const netname, channel, params: String): boolean;

implementation

uses
  SysUtils, Classes, console, irc, ircchansettings, mystrings;

const
  section = 'irccommands.windows';

function IrcShowWindow(const netname, channel, params: String): boolean;
begin
  Result := console_showwindow(params);
end;

function IrcShowWindows(const netname, channel, params: String): boolean;
var
  Windows, s: String;
begin
  Windows := console_windows;
  while (True) do
  begin
    s := GetFirstLineFromTextViaNewlineIndicators(Windows);
    if s = '' then
      break;

    irc_addtext(Netname, Channel, s);
  end;
  Result := True;
end;

function IrcDelWindow(const netname, channel, params: String): boolean;
begin
  if uppercase(params) = uppercase('ADMIN') then
  begin
    irc_addtext(Netname, Channel, 'Smartass.');
    Result := False;
    exit;
  end;
  console_delwindow(params);
  Result := True;
end;

function IrcIrcNames(const netname, channel, params: String): boolean;
var
  th: TMyIrcThread;
  nn, ch: String;
  i: integer;
  s: String;
  x: TStringList;
begin
  Result := False;

  nn := UpperCase(SubString(params, ' ', 1));
  ch := SubString(params, ' ', 2);
  th := FindIrcnetwork(nn);

  if ((FindIrcChannelSettings(nn, ch) = nil) or (th = nil)) then
  begin
    irc_addtext(Netname, Channel, 'Channel %s@%s not found.', [ch, nn]);
    exit;
  end;

  x := TStringList.Create;
  try
    x.DelimitedText := th.ChanNicks(ch);
    s := '';
    for i := 0 to x.Count - 1 do
    begin
      if (i + 1) mod 10 = 0 then
      begin
        irc_addtext(Netname, Channel, s);
        s := '';
      end;
      if s <> '' then
        s := s + ', ';
      s := s + x[i];
    end;
    if s <> '' then
      irc_addtext(Netname, Channel, s);
  finally
    x.Free;
  end;

  Result := True;
end;

function IrcRepaint(const netname, channel, params: String): boolean;
begin
  console_repaint;
  Result := True;
end;

end.
