unit irccommands.section;

interface

{ slftp sections commands functions }
function IrcSections(const netname, channel, params: String): boolean;

implementation

uses
  SysUtils, Classes, Contnrs, irccommandsunit, sitesunit, kb, irc, mystrings;

const
  section = 'irccommands.section';

function IrcSections(const netname, channel, params: String): boolean;
var
  ss, sitename, secs: String;
  s: TSite;
  i: integer;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  secs := UpperCase(mystrings.RightStr(params, length(sitename) + 1));

  if ((sitename = '') and (secs = '')) then
  begin
    IrcLineBreak(Netname, Channel, kb_sections.commatext, AnsiChar('"'), '<b>Global Sections</b>: ');
    Result := True;
    exit;
  end;

  if kb_sections.IndexOf(sitename) > -1 then
  begin
    ss := '';
    for i := 0 to sites.Count - 1 do
    begin
      s := TSite(sites.Items[i]);
      if s.IsSection(sitename) then
        ss := ss + s.Name + ',';
    end;
    delete(ss, length(ss), 1);
    Irc_AddText(Netname, channel, 'Sites with section %s', [sitename]);
    IrcLineBreak(Netname, Channel, ss, '"', '<b>' + sitename + '</b>: ', 9);
    Result := true;
    exit;
  end;

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site %s not found.', [sitename]);
    exit;
  end;

  ss := s.SetSections(secs, True);
  if ss <> '' then
    IrcLineBreak(Netname, Channel, ss, AnsiChar('"'), '<b>' + sitename + ' Sections</b>: ');

  Result := True;
end;

end.