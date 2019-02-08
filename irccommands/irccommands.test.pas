unit irccommands.test;

interface

{ slftp test commands functions }
function IrcTestColors(const netname, channel, params: String): boolean;

implementation

uses
  SysUtils, Classes, irc;

const
  section = 'irccommands.test';

function IrcTestColors(const netname, channel, params: String): boolean;
var
  i, colorscount: integer;
  colors: String;
begin
  colorscount := 15;
  colors := '';

  for i := 0 to colorscount do
  begin
    colors := colors + Format('<c%d>c%d</c> ', [i, i, i]);
  end;
  irc_addtext(Netname, Channel, 'Color test: %s', [colors]);

  Result := True;
end;

end.