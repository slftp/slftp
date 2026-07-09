unit irccommands.route;

interface

{ slftp route commands functions }
function IrcSpeeds(const netname, channel, params: String): boolean;
function IrcSetSpeed(const netname, channel, params: String): boolean;
function IrcInroutes(const netname, channel, params: String): boolean;
function IrcOutroutes(const netname, channel, params: String): boolean;
function IrcSpeedStats(const netname, channel, params: String): boolean;
function IrcSpeedRecalc(const netname, channel, params: String): boolean;

implementation

uses
  SysUtils, Classes, StrUtils, Contnrs, irc, debugunit, speedstatsunit, sitesunit,
  rcmdline, mystrings, irccommandsunit, routeconfig, Generics.Collections;

const
  section = 'irccommands.route';

type
  {
  @value(dRoutesIn To be used when incoming routes should be shown (-> 'to'))
  @value(dRoutesOut To be used when outgoing routes should be shown (-> 'from'))
  }
  TRoutesToShow = (dRoutesIn, dRoutesOut);

{$I common.inc}

function _IrcSetRoute(const netname, channel, params: String): boolean;
var
  source, dest, admin_site: String;
  rcmd: TCommandLineReader;
  c1, c2, sw1, sw2: String;
  i,j: integer;
  DoIt: Boolean;
  backtext: String;
  apply, back, fAffilOnly, fNoAffil, lock: Boolean;
  source_sites, dest_sites: TStringList;
  site: TSite;
  speed: integer;
  fSpeedInfo: TSpeedFromRouteInfo;
begin
  Result := False;

  // Parse the params
  rcmd := TCommandLineReader.create();
  try
    try
      rcmd.allowDOSStyle := True;
      rcmd.automaticalShowError := False;
      rcmd.declareString('c1','','');
      rcmd.declareString('c2','','');
      rcmd.declareString('sw1','','');
      rcmd.declareString('sw2','','');
      rcmd.declareFlag('apply','Apply changes');
      rcmd.addAbbreviation('a', 'apply');
      rcmd.declareFlag('back','Also add back route');
      rcmd.addAbbreviation('b', 'back');
      rcmd.declareFlag('affil','Only use pres from affils');
      rcmd.addAbbreviation('f', 'affil');
      rcmd.declareFlag('noaffil','Only use when not from affil');
      rcmd.addAbbreviation('n', 'noaffil');
      rcmd.declareFlag('lock','Lock the given speed rank');
      rcmd.addAbbreviation('l', 'lock');
      rcmd.parse(params);
    except
      on e: Exception do
      begin
        irc_addtext(Netname, Channel, '<c4><b>%s</b></c>', [e.Message]);
        Debug(dpError, section, '[EXCEPTION] IrcSetSpeed(rcmd.parse): %s', [e.Message]);
        exit;
      end;
    end;

    source := AnsiUpperCase(rcmd.readNamelessString()[0]);
    dest := AnsiUpperCase(rcmd.readNamelessString()[1]);
    speed := StrToIntDef(rcmd.readNamelessString()[2], -1);
    c1 := stringreplace(rcmd.readString('c1'), '.', '', [rfReplaceAll]);
    c2 := stringreplace(rcmd.readString('c2'), '.', '', [rfReplaceAll]);
    sw1 := rcmd.readString('sw1');
    sw2 := rcmd.readString('sw2');
    apply := rcmd.readFlag('apply');
    back := rcmd.readFlag('back');
    fAffilOnly := rcmd.readFlag('affil');
    fNoAffil := rcmd.readFlag('noaffil');
    lock := rcmd.readFlag('lock');

    fSpeedInfo.Speed := speed;
    fSpeedInfo.Locked := lock;
    fSpeedInfo.AffilOnly := fAffilOnly;
    fSpeedInfo.NoAffil := fNoAffil;
  finally
    rcmd.Free;
  end;

  admin_site := getAdminSiteName;

  // basic sanity check for first args
  if (source = admin_site) or (dest = admin_site) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>You can not use admin site with this function</b>.</c>');
    exit;
  end;
  if (speed > 9) or (speed < 0) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Third argument must be a speed between 0 and 9</b>.</c>');
    exit;
  end;

  // additional checks for optional filters
  if (c1 <> '') and (AnsiIndexText(c1, CountryCodes) = -1) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Sorry bro, %s is not a valid country code.</b>. Check https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2#Officially_assigned_code_elements</c>', [c1]);
    exit;
  end;
  if (c2 <> '') and (AnsiIndexText(c2, CountryCodes) = -1) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Sorry bro, %s is not a valid country code.</b>. Check https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2#Officially_assigned_code_elements</c>', [c2]);
    exit;
  end;
  if (sw1 <> '') and (StringToSiteSoftWare(sw1) = sswUnknown) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Hey dude, %s is not a valid ftp server software.</b>. Must be one of GLFTPD, IOFTPD, DRFTPD.</c>', [sw1]);
    exit;
  end;
  if (sw2 <> '') and (StringToSiteSoftWare(sw2) = sswUnknown) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Hey dude, %s is not a valid ftp server software.</b>. Must be one of GLFTPD, IOFTPD, DRFTPD.</c>', [sw2]);
    exit;
  end;

  if fAffilOnly and fNoAffil then
  begin
    irc_addtext(Netname, Channel, '<c4><b>You can''t use the affil only and the no affil options at the same time</b>.</c>');
    exit;
  end;

  // by default do not apply changes if we are using the overpowered wildcards stuff
  if ((source = '*') or (dest = '*')) and (not Apply)then
    doit := False
  else
    doit := True;

  // here we go with the real stuff
  source_sites := TStringList.Create;
  dest_sites := TStringList.Create;
  site := nil;
  try
    // lookup source site(s)
    if source = '*' then
    begin
      for i := 0 to sites.Count - 1 do
      begin
        site := nil;
        site := TSite(sites[i]);

        // site not found (shouldn't happen)
        if site = nil then
          Continue;

        // Admin site
        if site.Name = admin_site then
          Continue;

        // filters handling
        if (c1 <> '') and (AnsiLowerCase(site.Country) <> ('.' + AnsiLowerCase(c1))) then
          Continue;
        if (sw1 <> '') and (StringToSiteSoftWare(sw1) <> site.sw) then begin
          Continue;
        end;
        // add site to the source sites list
        source_sites.Add(site.Name);
      end;
    end
    else
    begin
      site := FindSiteByName(Netname, source);
      if site = nil then
      begin
        irc_addtext(Netname, Channel, 'Source site <b>%s</b> not found.', [source]);
        exit;
      end;
      source_sites.Add(site.Name);
    end;

    // lookup destination site(s)
    if dest = '*' then
    begin
      for i := 0 to sites.Count - 1 do
      begin
        site := nil;
        site := TSite(sites[i]);

        // site not found (shouldn't happen)
        if site = nil then
          Continue;
        if site.Name = admin_site then
          Continue;

        // filters handling
        if (c2 <> '') and (AnsiLowerCase(site.Country) <> ('.' + AnsiLowerCase(c2))) then
          Continue;
        if (sw2 <> '') and (StringToSiteSoftWare(sw2) <> site.sw) then
          Continue;

        // add site to the destination sites list
        dest_sites.Add(site.Name);
      end;
    end
    else
    begin
      site := FindSiteByName(Netname, dest);
      if site = nil then
      begin
        irc_addtext(Netname, Channel, 'Destination site <b>%s</b> not found.', [dest]);
        exit;
      end;
      dest_sites.Add(site.Name);
    end;

    // Check if we have work to do
    if source_sites.Count < 1 then
    begin
      irc_addtext(Netname, Channel, 'No source site match your criterias.');
      exit;
    end;
    if dest_sites.Count < 1 then
    begin
      irc_addtext(Netname, Channel, 'No destination site match your criterias.');
      exit;
    end;

    for i := 0 to source_sites.Count - 1 do
    begin
      for j := 0 to dest_sites.Count - 1 do
      begin
        // Source and dest is the same. Skipping.
        if (source_sites[i] = dest_sites[j]) then
          Continue;

        // announce what we're doing
        backtext := '';
        if back then
          backtext := ' (and backroute)';
        if fAffilOnly then
          backtext := backtext + ' (affil only)';
        if fNoAffil then
          backtext := backtext + ' (no affil)';
        if lock then
          backtext := backtext + ' (locked)';
        if speed > 0 then
        begin
          irc_addtext(Netname, Channel, 'Route from <b>%s</b> to <b>%s</b> set to %d%s', [source_sites[i], dest_sites[j], speed, backtext]);
        end
        else
        begin
          irc_addtext(Netname, Channel, 'Route from <b>%s</b> to <b>%s</b> removed', [source_sites[i], dest_sites[j]]);
        end;

        // When using wildcards apply changes only if --apply has been specified (to avoid unwanted changes)
        if DoIt then
        begin
          if speed > 0 then
          begin
            sitesdat.WriteString('site-' + source_sites[i], 'speed-from-' + dest_sites[j], fSpeedInfo.ToConfigString);
            if back then
            begin
              sitesdat.WriteString('site-' + dest_sites[j], 'speed-from-' + source_sites[i], fSpeedInfo.ToConfigString);
            end;
          end
          else
          begin
            sitesdat.DeleteKey('site-' + source_sites[i], 'speed-from-' + dest_sites[j]);
            if back then
            begin
              sitesdat.DeleteKey('site-' + dest_sites[j], 'speed-from-' + source_sites[i]);
            end;
          end;

          if back then
          begin
            site := FindSiteByName(Netname, dest_sites[j]);
            if site <> nil then
              site.UpdateSpeedFromCache;
          end;
        end;
      end;

      if DoIt then
      begin
        site := FindSiteByName(Netname, source_sites[i]);
        site.UpdateSpeedFromCache;
      end;
    end;

    if not DoIt then
      irc_addtext(Netname, Channel, 'Route were not really added. Check if you are satisfied and add --apply to the command.');

  finally
    FreeAndNil(source_sites);
    FreeAndNil(dest_sites);
  end;

  Result := True;
end;

{ Shows the incoming or outgoing routes depending on @link(aRoutesToShow)
  @param(Netname netname)
  @param(Channel channel name)
  @param(sitename sitename of routes which should be shown)
  @param(aRoutesToShow TRoutesToShow type to identify between incoming or outgoing routes)
}
procedure _ReadAndShowRoutesB(const Netname, Channel, sitename: String; aRoutesToShow: TRoutesToShow);
const
  RoutesDirectionIdentifier: array[0..1] of String = ('to', 'from');
  ArrowDirection: array[0..1] of String = ('<-', '->');
var
  fSitesAndRouteInfo: TStringList;
  ii, i: integer;
  ss, fArrowDirection: String;
  fSite: TSite;
  fSpeedFromListIterator: TSpeedFromRouteList;
  fSpeedFromItem: TSpeedFromRouteInfo;
begin
  case aRoutesToShow of
    dRoutesIn:
      begin
        fArrowDirection := ArrowDirection[0];
      end;
    dRoutesOut:
      begin
        fArrowDirection := ArrowDirection[1];
      end;
  end;

  fSitesAndRouteInfo := TStringList.Create;
  try
    fSitesAndRouteInfo.Sorted := True;
    fSitesAndRouteInfo.OwnsObjects := True;

    // if we want to show the incoming routes, we need to check the outgoing routes from all sites
    if aRoutesToShow = dRoutesIn then
    begin
      fArrowDirection := ArrowDirection[0];
      for fSite in sites do
      begin
        fSpeedFromListIterator := fSite.Speed_From;
        for fSpeedFromItem in fSpeedFromListIterator.Routes do
        begin
          if fSpeedFromItem.sitename = sitename then
          begin
            fSitesAndRouteInfo.AddObject(fSite.Name, TSpeedFromRouteInfoObjectWrapper.Create(fSpeedFromItem));
            break;
          end;
        end;
      end;
    end
    else if aRoutesToShow = dRoutesOut then
    begin
      // to show the outgoing routes, we can just use the site's Speed_From information
      fSite := FindSiteByName(Netname, sitename);
      fSpeedFromListIterator := fSite.Speed_From;
      for fSpeedFromItem in fSpeedFromListIterator.Routes do
      begin
        fSitesAndRouteInfo.AddObject(fSpeedFromItem.Sitename, TSpeedFromRouteInfoObjectWrapper.Create(fSpeedFromItem));
      end;
    end;

    ss := '';
    for i := 0 to fSitesAndRouteInfo.Count - 1 do
    begin
      if ss <> '' then
        ss := ss + ', ';

      ss := ss + '"' + fSitesAndRouteInfo[i] + ' ' + TSpeedFromRouteInfoObjectWrapper(fSitesAndRouteInfo.Objects[i]).SpeedInfo.ToString + '"';
    end;
    if ss <> '' then
      IrcLineBreak(Netname, Channel, ss, AnsiChar('"'), Format('<b>%s (%d)</b> %s ', [sitename, fSitesAndRouteInfo.Count, fArrowDirection]));

  finally
    fSitesAndRouteInfo.Free;
  end;
end;

function IrcSpeeds(const netname, channel, params: String): boolean;
var
  sitename: String;
  s: TSite;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
    exit;
  end;

  _ReadAndShowRoutesB(Netname, Channel, sitename, dRoutesOut);
  _ReadAndShowRoutesB(Netname, Channel, sitename, dRoutesIn);

  Result := True;
end;

function IrcSetSpeed(const netname, channel, params: String): boolean;
begin
  Result := False;

  if (_IrcSetRoute(Netname, Channel, params)) then
    Result := True;
end;

function IrcInroutes(const netname, channel, params: String): boolean;
var
  s: TSite;
  i: integer;
begin
  if params <> '' then
  begin
    s := FindSiteByName('', UpperCase(params));
    if s <> nil then
    begin
      _ReadAndShowRoutesB(Netname, Channel, s.Name, dRoutesIn);
    end
    else
      irc_addtext(Netname, Channel, '<c4>Site: <b>%s</b> not found!</c>', [params]);
    Result := True;
    exit;
  end;
  for i := 0 to sites.Count - 1 do
  begin
    s := TSite(sites[i]);
    _ReadAndShowRoutesB(Netname, Channel, s.Name, dRoutesIn);
  end;
  Result := True;
end;

function IrcOutroutes(const netname, channel, params: String): boolean;
var
  s: TSite;
  i: integer;
begin
  if params <> '' then
  begin
    s := FindSiteByName('', UpperCase(params));
    if s <> nil then
    begin
      _ReadAndShowRoutesB(Netname, Channel, s.Name, dRoutesOut);
    end
    else
      irc_addtext(Netname, Channel, '<c4>Site: <b>%s</b> not found!</c>', [params]);

    Result := True;
    exit;
  end;

  for i := 0 to sites.Count - 1 do
  begin
    s := TSite(sites[i]);
    _ReadAndShowRoutesB(Netname, Channel, s.Name, dRoutesOut);
  end;

  Result := True;
end;

function IrcSpeedStats(const netname, channel, params: String): boolean;
var
  sitename, section, rip: String;
  s: TSite;
begin
  Result := False;

  sitename := UpperCase(SubString(params, ' ', 1));
  section := UpperCase(SubString(params, ' ', 2));
  rip := SubString(params, ' ', 3);

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
    exit;
  end;

  SpeedStatsShowStats(Netname, Channel, sitename, section, rip);

  Result := True;
end;

function IrcSpeedRecalc(const netname, channel, params: String): boolean;
begin
  // SpeedStatsRecalc(netname, channel);
  SpeedStatsRecalc('CONSOLE', 'ADMIN');
  Result := True;
end;

end.
