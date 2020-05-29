unit irccommands.pretime;

interface

{ slftp pretime commands functions }
function IrcSetupPretimeMode(const netname, channel, params: String): boolean;
function IrcSetupPretimeMode2(const netname, channel, params: String): boolean;
function IrcSetupADDPreMode(const netname, channel, params: String): boolean;
function IrcFindPretime(const netname, channel, params: String): boolean;
function IrcSetPretime(const netname, channel, params: String): boolean;

implementation

uses
  SysUtils, Classes, DateUtils, Contnrs, dbaddpre, irc, configunit, sitesunit, RegExpr, mystrings;

const
  section = 'irccommands.pretime';

function IrcSetupPretimeMode(const netname, channel, params: String): boolean;
var
  pmode: integer;
begin
  Result := False;
  pmode := StrToIntDef(params, -1);

  if (pmode >= 0) and (pmode <= Ord(High(TPretimeLookupMOde))) then
  begin
    irc_addtext(Netname, Channel, 'Will change Pretimemode from %s to %s', [pretimeModeToString(TPretimeLookupMOde(config.ReadInteger('taskpretime', 'mode', 0))), pretimeModeToString(TPretimeLookupMOde(pmode))]);
    config.WriteInteger('taskpretime', 'mode', pmode);
    setPretimeMode_One(TPretimeLookupMOde(pmode));
    config.UpdateFile;
  end;

  irc_addtext(Netname, Channel, 'Pretimemode: <b>%d</b> (%s)', [config.ReadInteger('taskpretime', 'mode', 0), pretimeModeToString(TPretimeLookupMOde(config.ReadInteger('taskpretime', 'mode', 0)))]);
  Result := True;
end;

function IrcSetupPretimeMode2(const netname, channel, params: String): boolean;
var
  pmode: integer;
begin
  Result := False;
  pmode := StrToIntDef(params, -1);

  if (pmode >= 0) and (pmode <= Ord(High(TPretimeLookupMOde))) then
  begin
    irc_addtext(Netname, Channel, 'Will change Pretimemode from %s to %s', [pretimeModeToString(TPretimeLookupMOde(config.ReadInteger('taskpretime', 'mode_2', 0))), pretimeModeToString(TPretimeLookupMOde(pmode))]);
    config.WriteInteger('taskpretime', 'mode_2', pmode);
    setPretimeMode_Two(TPretimeLookupMOde(pmode));
    config.UpdateFile;
  end;

  irc_addtext(Netname, Channel, 'Pretimemode: <b>%d</b> (%s)', [config.ReadInteger('taskpretime', 'mode_2', 0), pretimeModeToString(TPretimeLookupMOde(config.ReadInteger('taskpretime', 'mode_2', 0)))]);
  Result := True;
end;

function IrcSetupADDPreMode(const netname, channel, params: String): boolean;
var
  pmode: integer;
begin
  Result := False;

  pmode := StrToIntDef(params, -1);

  if (pmode >= 0) and (pmode <= Ord(High(TAddPreMode))) then
  begin
    irc_addtext(Netname, Channel, 'Will change Pretimemode from %s to %s', [addPreModeToString(TAddPreMode(config.ReadInteger('dbaddpre', 'mode', 0))), addPreModeToString(TAddPreMode(pmode))]);
    config.WriteInteger('dbaddpre', 'mode', pmode);
    setAddPretimeMode(TAddPreMode(pmode));
    config.UpdateFile;
  end;
  irc_addtext(Netname, Channel, 'Pretimemode: <b>%d</b> (%s)', [config.ReadInteger('dbaddpre', 'mode', 0), addPreModeToString(TAddPreMode(config.ReadInteger('dbaddpre', 'mode', 0)))]);

  Result := True;
end;

function IrcFindPretime(const netname, channel, params: String): boolean;
var
  pt: TDateTime;
  resu: TPretimeResult;
begin

  if config.ReadInteger('taskpretime', 'mode', 0) = 0 then
  begin
    Irc_AddText(Netname, Channel, '<c15>INFO</c>: Pretime_mode is set to None!');
    Result := True;
    Exit;
  end;

  resu := getPretime(params);
  pt := UnixToDateTime(resu.pretime, False);
  if resu.pretime > 15 then
    irc_addtext(Netname, Channel, 'PRETIME %s ~ %s ~ %s (%s)', [params,
      dbaddpre_GetPreduration(resu.pretime), FormatDateTime('yyyy-mm-dd hh:nn:ss', pt), resu.mode])
  else
  begin
    irc_addtext(Netname, Channel, 'No valid pretime -> ' +
      IntToStr(resu.pretime));
  end;
  Result := True;
end;

function IrcSetPretime(const netname, channel, params: String): boolean;
var
  sitename: String;
  section: String;
  s_pretime: String;
  pretime: integer;
  site: TSite;
  i: integer;
  x: TStringList;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  section := UpperCase(SubString(params, ' ', 2));
  s_pretime := SubString(params, ' ', 3);

  if s_pretime = '-' then
    pretime := -10
  else
    pretime := StrToIntDef(s_pretime, -1);

  if sitename = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      if (TSite(sites.Items[i]).Name = getAdminSiteName)
        then
        Continue;
      if ((pretime = -10) or (pretime >= 0)) then
        TSite(sites.Items[i]).sectionpretime[section] := pretime;
      if (TSite(sites.Items[i]).sectionpretime[section] <> -1) then
      begin
        irc_addtext(Netname, Channel, 'Pretime for <b>%s</b> in %s is<c7> %d</c>',
          [TSite(sites.Items[i]).Name, section,
          TSite(sites.Items[i]).sectionpretime[section]]);
      end
      else
      begin
        irc_addtext(Netname, Channel, 'Pretime for <b>%s</b> in %s is not set',
          [TSite(sites.Items[i]).Name, section]);
      end;

    end;
  end
  else
  begin
    x := TStringList.Create;
    try
      x.commatext := sitename;

      for i := 0 to x.Count - 1 do
      begin
        site := FindSiteByName(Netname, x.Strings[i]);
        if site = nil then
        begin
          irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [x.Strings[i]]);
          Continue;
        end;
        if ((pretime = -10) or (pretime >= 0)) then
          site.sectionpretime[section] := pretime;
        if (site.sectionpretime[section] <> -1) then
        begin
          irc_addtext(Netname, Channel, 'Pretime for <b>%s</b> in %s is<c7> %d</c>', [sitename,
            section, site.sectionpretime[section]]);
        end
        else
        begin
          irc_addtext(Netname, Channel, 'Pretime for <b>%s</b> in %s is not set', [sitename,
            section]);
        end;

      end;
    finally
      x.Free;
    end;
  end;

  Result := True;
end;

end.
