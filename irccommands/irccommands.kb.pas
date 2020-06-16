unit irccommands.kb;

interface

{ slftp kb commands functions }
function IrcKbShow(const netname, channel, params: String): boolean;
function IrcKbList(const netname, channel, params: String): boolean;
function IrcKbExtra(const netname, channel, params: String): boolean;
function IrcKbAdd(const netname, channel, params: String): boolean;

implementation

uses
  SysUtils, Classes, kb, kb.releaseinfo, pazo, precatcher, irc, mystrings;

const
  section = 'irccommands.kb';

function IrcKbShow(const netname, channel, params: String): boolean;
var
  section, rls: String;
  p: TPazo;
  i: integer;
  s, ss: String;
begin
  Result := False;
  section := UpperCase(SubString(params, ' ', 1));
  rls := SubString(params, ' ', 2);

  i := kb_list.IndexOf(section + '-' + rls);
  if i <> -1 then
  begin
    p := TPazo(kb_list.Objects[i]);
    s := p.AsText;
    for i := 1 to 1000 do
    begin
      ss := SubString(s, #13#10, i);
      if ss = '' then
        break;
      irc_addtext(Netname, Channel, '%s', [ss]);
    end;
  end
  else
    irc_addtext(Netname, Channel, Format('Can not find any knowledge base entry for %s %s', [section, rls]));

  Result := True;
end;

function IrcKbList(const netname, channel, params: String): boolean;
var
  p: TPazo;
  i, db: integer;
  section: String;
  hits: integer;
begin
  section := SubString(params, ' ', 1);

  if kb_list.Count <= 0 then
  begin
    irc_addtext(Netname, Channel, 'No Infos in knowledge base!');
    Result := True;
    exit;
  end;

  if section <> '' then
  begin
    i := kb_sections.IndexOf(section);
    if i <> -1 then
      section := kb_sections[i]
    else
      section := '';
  end;

  if section <> '' then
    hits := StrToIntDef(SubString(params, ' ', 2), 10)
  else
    hits := StrToIntDef(SubString(params, ' ', 1), 10);

  db := 0;
  for i := kb_list.Count - 1 downto 0 do
  begin
    if (db > hits) then
      break;

    p := TPazo(kb_list.Objects[i]);
    if p <> nil then
    begin
      if ((section = '') or (p.rls.section = section)) then
      begin
        irc_addtext(Netname, Channel, '#%d %s %s [QueueNumber: %d (Race:%d Dirlist:%d Mkdir:%d)]',
          [p.pazo_id, p.rls.section, p.rls.rlsname, p.queuenumber.Value, p.racetasks.Value,
          p.dirlisttasks.Value, p.mkdirtasks.Value]);

        Inc(db);
      end;
    end
    else
    begin
      irc_addtext(Netname, Channel, 'Whops, Pazo is nil! Anything screwed up!');
    end;
  end;

  Result := True;
end;

function IrcKbExtra(const netname, channel, params: String): boolean;
var
  section, rls, extra: String;
begin
  section := UpperCase(SubString(params, ' ', 1));
  rls := SubString(params, ' ', 2);
  extra := mystrings.RightStr(params, length(section) + length(rls) + 2);
  kb_Add(Netname, Channel, '', section, extra, kbeNEWDIR, rls, '', True);

  Result := True;
end;

function IrcKbAdd(const netname, channel, params: String): boolean;
var
  sitename, event, section, rls_section, rls: String;
  kb_event: TKBEventType;
begin
  sitename := UpperCase(SubString(params, ' ', 1));
  event := UpperCase(SubString(params, ' ', 2));
  section := UpperCase(SubString(params, ' ', 3));
  rls := SubString(params, ' ', 4);

  // section correct
  section := ProcessDoReplace(section);
  rls_section := '';
  rls_section := KibontasSection(' ' + section + ' ', '');
  rls_section := PrecatcherSectionMapping(rls, rls_section);
  if ((rls_section = '') or (rls_section = 'TRASH')) then
  begin
    irc_addtext(Netname, Channel, 'No valid section found (%s)', [rls_section]);
    Result := False;
    exit;
  end;

  // add to kb
  kb_event := EventStringToTKBEventType(event);
  kb_Add(Netname, Channel, sitename, rls_section, '', kb_event, rls, '');

  case kb_event of
    kbeNEWDIR:
      begin
        irc_addtext(Netname, Channel, format('<c2>-> [KB]</c> %s %s %s @ <b>%s</b>',
          [event, rls_section, rls, sitename]));
      end;
    kbePRE:
      begin
        irc_addtext(Netname, Channel, format('<c3>-> [KB]</c> %s %s %s @ <b>%s</b>',
          [event, rls_section, rls, sitename]));
      end;
    kbeSPREAD:
      begin
        irc_addtext(Netname, Channel, format('<c5>-> [KB]</c> %s %s %s @ <b>%s</b>',
          [event, rls_section, rls, sitename]));
      end;
    kbeADDPRE:
      begin
        irc_addtext(Netname, Channel, format('<c3>-> [KB]</c> %s %s %s @ <b>%s</b>',
          [event, rls_section, rls, sitename]));
      end;
    kbeCOMPLETE:
      begin
        irc_addtext(Netname, Channel, format('<c7><- [KB]</c> %s %s %s @ <b>%s</b>',
          [event, rls_section, rls, sitename]));
      end;
    kbeNUKE:
      begin
        irc_addtext(Netname, Channel, format('<c4>-- [KB]</c> %s %s %s @ <b>%s</b>',
          [event, rls_section, rls, sitename]));
      end;
  end;

  Result := True;
end;

end.
