unit irccommands.route;

interface

{ slftp route commands functions }
function IrcSpeeds(const netname, channel, params: String): boolean;
function IrcSetSpeed(const netname, channel, params: String): boolean;
function IrcLockSpeed(const netname, channel, params: String): boolean;
function IrcInroutes(const netname, channel, params: String): boolean;
function IrcOutroutes(const netname, channel, params: String): boolean;
function IrcSpeedStats(const netname, channel, params: String): boolean;
function IrcSpeedRecalc(const netname, channel, params: String): boolean;

implementation

uses
  SysUtils, Classes, StrUtils, irc, debugunit, speedstatsunit, sitesunit, rcmdline, mystrings,
  irccommandsunit;

const
  section = 'irccommands.route';

type
  {
  @value(dRoutesIn To be used when incoming routes should be shown (-> 'to'))
  @value(dRoutesOut To be used when outgoing routes should be shown (-> 'from'))
  }
  TRoutesToShow = (dRoutesIn, dRoutesOut);

{$I common.inc}

function _IrcSetRoute(const netname, channel, params: String; lock: boolean = False): boolean;
var
  source, dest, admin_site: String;
  rcmd: TCommandLineReader;
  c1, c2, sw1, sw2: String;
  i,j: integer;
  DoIt: Boolean;
  backtext: String;
  apply, back: Boolean;
  source_sites, dest_sites: TStringList;
  site: TSite;
  speed: integer;
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
        if speed > 0 then
        begin
          if lock then
            irc_addtext(Netname, Channel, 'Routelock from <b>%s</b> to <b>%s</b> set to %d%s', [source_sites[i], dest_sites[j], speed, backtext])
          else
            irc_addtext(Netname, Channel, 'Route from <b>%s</b> to <b>%s</b> set to %d%s', [source_sites[i], dest_sites[j], speed, backtext]);
        end
        else
        begin
          if lock then
            irc_addtext(Netname, Channel, 'Routelock from <b>%s</b> to <b>%s</b> removed%s', [source_sites[i], dest_sites[j], backtext])
          else
            irc_addtext(Netname, Channel, 'Route from <b>%s</b> to <b>%s</b> removed%s', [source_sites[i], dest_sites[j], backtext]);
        end;

        // When using wildcards apply changes only if -apply has been specified (to avoid unwanted changes)
        if DoIt then
        begin
          if speed > 0 then
          begin
            // normal route
            sitesdat.WriteInteger('speed-from-' + source_sites[i], dest_sites[j], speed);
            sitesdat.WriteInteger('speed-to-' + dest_sites[j], source_sites[i], speed);
            if back then
            begin
              sitesdat.WriteInteger('speed-from-' + dest_sites[j], source_sites[i], speed);
              sitesdat.WriteInteger('speed-to-' + source_sites[i], dest_sites[j], speed);
            end;

            // locked route
            if lock then
            begin
              sitesdat.WriteInteger('speedlock-from-' + source_sites[i], dest_sites[j], speed);
              sitesdat.WriteInteger('speedlock-to-' + dest_sites[j], source_sites[i], speed);
              if back then
              begin
                sitesdat.WriteInteger('speedlock-from-' + dest_sites[j], source_sites[i], speed);
                sitesdat.WriteInteger('speedlock-to-' + source_sites[i], dest_sites[j], speed);
              end;
            end;
          end
          else
          begin
            if not lock then
            begin
              // normal route
              sitesdat.DeleteKey('speed-from-' + source_sites[i], dest_sites[j]);
              sitesdat.DeleteKey('speed-to-' + dest_sites[j], source_sites[i]);
              if back then
              begin
                sitesdat.DeleteKey('speed-from-' + dest_sites[j], source_sites[i]);
                sitesdat.DeleteKey('speed-to-' + source_sites[i], dest_sites[j]);
              end;
            end;

            // locked route
            if lock then
            begin
              sitesdat.DeleteKey('speedlock-from-' + source_sites[i], dest_sites[j]);
              sitesdat.DeleteKey('speedlock-to-' + dest_sites[j], source_sites[i]);
              if back then
              begin
                sitesdat.DeleteKey('speedlock-from-' + dest_sites[j], source_sites[i]);
                sitesdat.DeleteKey('speedlock-to-' + source_sites[i], dest_sites[j]);
              end;
            end;
          end;
        end;
      end;
    end;

    if not DoIt then
      irc_addtext(Netname, Channel, 'Route were not really added. Check if you are satisfied and add -apply to the command.');

  finally
    FreeAndNil(source_sites);
    FreeAndNil(dest_sites);
  end;

  Result := True;
end;

{ Shows the incoming or outgoing routes depending on @value(aRoutesToShow)
  @param(Netname netname)
  @param(Channel channel name)
  @param(sitename sitename of routes which should be shown)
  @param(aRoutesToShow TRoutesToShow type to identify between incoming or outgoing routes)
}
procedure _ReadAndShowRoutesB(const Netname, Channel, sitename: String; aRoutesToShow: TRoutesToShow);
const
  RoutesDirectionIdentifier: array[0..1] of String = ('to', 'from');
var
  x: TStringList;
  ii, i: integer;
  ss, fIdentifier: String;
begin
  case aRoutesToShow of
    dRoutesIn: fIdentifier := RoutesDirectionIdentifier[0];
    dRoutesOut: fIdentifier := RoutesDirectionIdentifier[1];
  end;

  x := TStringList.Create;
  try
    x.Sorted := True;
    sitesdat.ReadSection('speed-' + fIdentifier +'-' + sitename, x);
    ss := '';
    ii := x.Count;
    for i := 0 to x.Count - 1 do
    begin
      if ss <> '' then
        ss := ss + ', ';
      if (sitesdat.ReadString('speedlock-' + fIdentifier + '-' + sitename, x[i], '') <> '') then
      begin
        ss := ss + '"' + x[i] + ' ' + sitesdat.ReadString('speedlock-' + fIdentifier + '-' + sitename, x[i], '') + '(L)' + '"';
      end
      else
      begin
        ss := ss + '"' + x[i] + ' ' + sitesdat.ReadString('speed-' + fIdentifier + '-' + sitename, x[i], '') + '"';
      end;
    end;
  finally
    x.Free;
  end;
  if ss <> '' then
    IrcLineBreak(Netname, Channel, ss, AnsiChar('"'), format('<b>%s (%d)</b> -> ', [sitename, ii]));
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

  if (_IrcSetRoute(Netname, Channel, params, False)) then
    Result := True;
end;

function IrcLockSpeed(const netname, channel, params: String): boolean;
begin
  Result := False;

  if (_IrcSetRoute(Netname, Channel, params, True)) then
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