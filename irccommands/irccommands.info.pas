unit irccommands.info;

interface

{ slftp info commands functions }
function IrcInfo(const netname, channel, params: String): boolean;
function IrcName(const netname, channel, params: String): boolean;
function IrcLink(const netname, channel, params: String): boolean;
function IrcAffils(const netname, channel, params: String): boolean;
function IrcSetAffils(const netname, channel, params: String): boolean;
function IrcSize(const netname, channel, params: String): boolean;
function IrcCountry(const netname, channel, params: String): boolean;
function IrcNotes(const netname, channel, params: String): boolean;
function IrcUsers(const netname, channel, params: String): boolean;
function IrcLeechers(const netname, channel, params: String): boolean;
function IrcTraders(const netname, channel, params: String): boolean;
function IrcUserslots(const netname, channel, params: String): boolean;
function IrcFreeslots(const netname, channel, params: String): boolean;
function IrcFindAffil(const netname, channel, params: String): boolean;
function IrcFindCountry(const netname, channel, params: String): boolean;
function IrcFindSection(const netname, channel, params: String): boolean;
function IrcFindUser(const netname, channel, params: String): boolean;

implementation

uses
  SysUtils, Classes, StrUtils, Contnrs, irc, sitesunit, mystrings, globals, irccommandsunit;

const
  section = 'irccommands.info';

{$I common.inc}

function IrcInfo(const netname, channel, params: String): boolean;
var
  i: integer;
  s: TSite;
  sitename: String;
  x: TStringList;
begin
  Result := False;
  sitename := UpperCase(params);

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
    exit;
  end;

  // TODO: Rework this and improve helpfile

  x := TStringList.Create;
  try
    irc_addtext(Netname, Channel, '<b>Site</b> %s:', [s.Name]);
    irc_addtext(Netname, Channel, ' name/speed/location/size: %s / %s / %s / %s', [s.RCString('name', '??'), s.RCString('link', '??'), s.Country, s.RCString('size', '??')]);
    irc_addtext(Netname, Channel, ' sections: %s', [s.sections]);

    sitesdat.ReadSection('site-' + sitename, x);
    x.Sort;
    for i := 0 to x.Count - 1 do
    begin
      if x[i] = 'affils-PRE' then
        Continue;

      if 1 = Pos('affils-', x[i]) then
        irc_addtext(Netname, Channel, ' %s: %s', [x[i], s.RCString(x[i], '')]);
    end;

    x.DelimitedText := s.leechers;
    irc_addtext(Netname, Channel, ' leechers (%d/%d):B %s', [x.Count, s.RCInteger('maxleechers', -1), x.DelimitedText]);
    x.DelimitedText := s.traders;
    irc_addtext(Netname, Channel, ' traders (%d/%d):B %s', [x.Count, s.RCInteger('maxtraders', -1), x.DelimitedText]);

    if s.RCString('notes', '') <> '' then
      irc_addtext(Netname, Channel, ' notes: ' + s.RCString('notes', ''));

  finally
    x.Free;
  end;

  Result := True;
end;

function IrcName(const netname, channel, params: String): boolean;
var
  sitename, Name: String;
  s: TSite;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  Name := mystrings.RightStr(params, length(sitename) + 1);

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site %s not found.', [sitename]);
    exit;
  end;
  s.WCString('name', Name); // TODO: Add property
  irc_addtext(Netname, Channel, Name);

  Result := True;
end;

function IrcLink(const netname, channel, params: String): boolean;
var
  sitename, link: String;
  s: TSite;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  link := mystrings.RightStr(params, length(sitename) + 1);

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site %s not found.', [sitename]);
    exit;
  end;
  s.WCString('link', link); // TODO: Add property
  irc_addtext(Netname, Channel, link);

  Result := True;
end;

function IrcAffils(const netname, channel, params: String): boolean;
var
  affils_new, affillist, sitename: String;
  s: TSite;
  TStringList_affils_new, TStringList_affils_old: TStringList;
  i: integer;
begin
  Result := False;

  sitename := UpperCase(SubString(params, ' ', 1));
  affils_new := mystrings.RightStr(params, length(sitename) + 1);

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, '<b><c4>ERROR</c></b>: Site <b>%s</b> not found.', [sitename]);
    exit;
  end;

  if affils_new <> '' then
  begin
    TStringList_affils_new := TStringList.Create;
    TStringList_affils_old := TStringList.Create;
    try
      TStringList_affils_new.Delimiter := ' ';
      TStringList_affils_new.CaseSensitive := False;
      TStringList_affils_new.Sorted := True;
      TStringList_affils_new.Duplicates := dupIgnore;
      TStringList_affils_new.DelimitedText := affils_new;

      TStringList_affils_old.Delimiter := ' ';
      TStringList_affils_old.CaseSensitive := False;
      TStringList_affils_old.Sorted := True;
      TStringList_affils_old.Duplicates := dupIgnore;
      TStringList_affils_old.DelimitedText := s.SiteAffils;

      for i := 0 to TStringList_affils_new.Count - 1 do
      begin
        if (TStringList_affils_old.IndexOf(TStringList_affils_new[i]) <> -1) then
        Begin
          TStringList_affils_old.Delete(TStringList_affils_old.IndexOf(TStringList_affils_new[i]));
        End
        else
        begin
          TStringList_affils_old.Add(TStringList_affils_new[i]);
        end
      end;

      s.SiteAffils := TStringList_affils_old.DelimitedText;
    finally
      TStringList_affils_new.Free;
      TStringList_affils_old.Free;
    end;
  end;

  affillist := s.SiteAffils;

  if affillist <> '' then
    IrcLineBreak(Netname, Channel, affillist, ' ', Format('<b>%s</b>@%s : ', ['', sitename]), 12)
  else
    irc_addText(Netname, Channel, 'No affils available.');

  Result := True;
end;

function IrcSetAffils(const netname, channel, params: String): boolean;
var
  affils, ss, sitename: String;
  s: TSite;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  affils := mystrings.RightStr(params, length(sitename) + 1);

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, '<b><c4>ERROR</c></b>: Site <b>%s</b> not found.', [sitename]);
    exit;
  end;

  if affils <> '' then
  begin
    s.siteAffils := affils;
  end;

  ss := s.SiteAffils;

  if ss <> '' then
    IrcLineBreak(Netname, Channel, ss, ' ', Format('<b>%s</b>@%s : ', ['', sitename]), 12)
  else
    irc_addText(Netname, Channel, 'No affils available.');

  Result := True;
end;

function IrcSize(const netname, channel, params: String): boolean;
var
  sitename, size: String;
  s: TSite;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  size := mystrings.RightStr(params, length(sitename) + 1);

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site %s not found.', [sitename]);
    exit;
  end;
  s.WCString('size', size); // TODO: Add property to TSite
  irc_addtext(Netname, Channel, size);

  Result := True;
end;

function IrcCountry(const netname, channel, params: String): boolean;
var
  sitename, country: String;
  i: Integer;
  s: TSite;
begin
  Result := False;
  sitename := AnsiUpperCase(SubString(params, ' ', 1));
  country := AnsiUpperCase(stringreplace(mystrings.RightStr(params, length(sitename) + 1), '.', '', [rfReplaceAll]));

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site %s not found.', [sitename]);
    exit;
  end;

  i := AnsiIndexText(country, CountryCodes);
  if not (i > -1) then
  begin
    irc_addtext(Netname, Channel, 'Country .%s is not a valid country! Check https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2#Officially_assigned_code_elements', [country]);
    exit;
  end;

  s.Country := '.' + country;
  irc_addtext(Netname, Channel, 'Country for %s set to %s (%s)', [sitename, s.Country, CountryNames[i]]);

  Result := True;
end;

function IrcNotes(const netname, channel, params: String): boolean;
var
  sitename, notes: String;
  s: TSite;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  notes := mystrings.RightStr(params, length(sitename) + 1);

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site %s not found.', [sitename]);
    exit;
  end;
  s.WCString('notes', notes);
  irc_addtext(Netname, Channel, notes);

  Result := True;
end;

function IrcUsers(const netname, channel, params: String): boolean;
var
  ss, sitename: String;
  s: TSite;
  x, y: TStringList;
  i, j: integer;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));

  if sitename <> '' then
  begin
    s := FindSiteByName(Netname, sitename);
    if s = nil then
    begin
      irc_addtext(Netname, Channel, 'Site %s not found.', [sitename]);
      exit;
    end;
    ss := s.users;
  end
  else
  begin
    x := TStringList.Create;
    y := TStringList.Create;
    try
      for i := 0 to sites.Count - 1 do
      begin
        s := TSite(sites[i]);
        x.DelimitedText := s.leechers;
        for j := 0 to x.Count - 1 do
          if y.IndexOf(x[j]) = -1 then
            y.Add(x[j]);
        x.DelimitedText := s.traders;
        for j := 0 to x.Count - 1 do
          if y.IndexOf(x[j]) = -1 then
            y.Add(x[j]);
      end;

      y.Sort;

      ss := '';
      for i := 0 to y.Count - 1 do
      begin
        if ss <> '' then
          ss := ss + ' ';
        if (((i + 1) mod 10) = 0) then
        begin
          irc_addtext(Netname, Channel, ss);
          ss := '';
        end;

        ss := ss + y[i];
      end;

    finally
      x.Free;
      y.Free;
    end;

  end;
  if (trim(ss) <> '') then
    irc_addtext(Netname, Channel, ss);

  Result := True;
end;

function IrcLeechers(const netname, channel, params: String): boolean;
var
  leecherlist, sitename, users: String;
  i: Integer;
  s: TSite;
  x: TStringList;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  users := mystrings.RightStr(params, length(sitename) + 1);

  x := TStringList.Create;
  try
    x.commatext := sitename;

    for i := 0 to x.Count - 1 do
    begin
      leecherlist := '';

      s := FindSiteByName(Netname, x.Strings[i]);
      if s = nil then
      begin
        irc_addtext(Netname, Channel, 'Site %s not found.', [x.Strings[i]]);
        Continue;
      end;

      leecherlist := s.SetLeechers(users, True);
      if leecherlist <> '' then
      begin
        irc_addText(Netname, Channel, 'Leecher list for <b>%s</b>: %s', [s.Name, leecherlist]);
      end;

    end;
  finally
    x.Free;
  end;

  Result := True;
end;

function IrcTraders(const netname, channel, params: String): boolean;
var
  ss, sitename, users: String;
  s: TSite;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  users := mystrings.RightStr(params, length(sitename) + 1);

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site %s not found.', [sitename]);
    exit;
  end;
  ss := s.SetTraders(users, True);
  if ss <> '' then
    irc_addtext(Netname, Channel, ss);

  Result := True;
end;

function IrcUserslots(const netname, channel, params: String): boolean;
var
  sitename: String;
  s: TSite;
  leechslots, ratioslots: integer;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  leechslots := StrToIntDef(SubString(params, ' ', 2), -1);
  ratioslots := StrToIntDef(SubString(params, ' ', 3), -1);

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site %s not found.', [sitename]);
    exit;
  end;

  // TODO: Add property to TSite
  s.WCInteger('maxleechers', leechslots);
  s.WCInteger('maxtraders', ratioslots);

  Result := True;
end;

function IrcFreeslots(const netname, channel, params: String): boolean;
var
  s: TSite;
  i: integer;
  db: integer;
  ss: String;
begin
  Result := False;

  db := 0;
  ss := '';
  for i := 0 to sites.Count - 1 do
  begin
    s := TSite(sites[i]);

    if (TSite(sites.Items[i]).Name = getAdminSiteName) then
      Continue;

    ss := ss + format('<b>%s</b> (%d/%d) ', [s.Name, s.FreeTraderSlots, s.FreeLeechSlots]);
    Inc(db);

    if db >= 5 then
    begin
      irc_addtext(Netname, Channel, ss);
      ss := '';
      db := 0;
    end;
  end;

  if ss <> '' then
    irc_addtext(Netname, Channel, ss);

  Result := True;
end;

function IrcFindAffil(const netname, channel, params: String): boolean;
var
  s: TSite;
  db, i: integer;
  ss: String;
  affil: String;
  found: Boolean;
begin
  Result := False;
  affil := Uppercase(SubString(params, ' ', 1));

  ss := 'Sites with Affilgroup <b>' + affil + '</b> are: ';
  db := 0;
  found := False;
  for i := 0 to sites.Count - 1 do
  begin
    s := TSite(sites[i]);

    if s.IsAffil(affil) then
    begin
      found := True;

      if s.PermDown then
      begin
        ss := ss + Format('<%s><b>%s</b></c> ', [globals.SiteColorPermdown, s.Name]);
      end
      else
      begin
        case s.WorkingStatus of
          sstUp: ss := ss + Format('<%s><b>%s</b></c> ', [globals.SiteColorOnline, s.Name]);
          sstDown, sstTempDown, sstMarkedAsDownByUser: ss := ss + Format('<%s><b>%s</b></c> ', [globals.SiteColorOffline, s.Name]);
          sstUnknown: ss := ss + Format('<%s><b>%s</b></c> ', [globals.SiteColorUnknown, s.Name]);
        end;
      end;

      Inc(db);
      if db >= 10 then
      begin
        irc_addtext(Netname, Channel, ss);
        db := 0;
        ss := '';
      end;
    end;
  end;

  if found then
    irc_addtext(Netname, Channel, ss)
  else
    irc_addtext(Netname, Channel, Format('No Site with %s as Affilgroup found!', [affil]));

  Result := True;
end;

function IrcFindCountry(const netname, channel, params: String): boolean;
var
  s: TSite;
  i: integer;
  site_found: boolean;
  country, ss: String;
begin
  country := AnsiUpperCase(stringreplace(SubString(params, ' ', 1), '.', '', [rfReplaceAll]));
  site_found := False;
  ss := format('Site(s) with Country .%s:', [country]);

  for i := 0 to sites.Count - 1 do
  begin
    s := TSite(sites[i]);

    if '.' + country = s.Country then
    begin
      ss := ss + format(' <b>%s</b>', [s.Name]);
      site_found := True;
    end;

  end;

  if site_found then
  begin
    irc_addtext(Netname, Channel, ss);
  end
  else
  begin
    irc_addtext(Netname, Channel, 'No sites in country .%s found!', [country]);
  end;

  Result := True;
end;

function IrcFindSection(const netname, channel, params: String): boolean;
var
  s: TSite;
  i: integer;
  section, fSitesList: String;
begin
  section := UpperCase(SubString(params, ' ', 1));
  fSitesList := '';

  for i := 0 to sites.Count - 1 do
  begin
    s := TSite(sites[i]);
    if (s.Name = getAdminSiteName) then
      Continue;
    if (s.PermDown) then
      Continue;

    if s.IsSection(section) then
      fSitesList := fSitesList + ' ' + s.Name;
  end;

  if (fSitesList <> '') then
    irc_addtext(Netname, Channel, 'Section <b>%s</b> added on: %s', [section, fSitesList]);

  Result := True;
end;

function IrcFindUser(const netname, channel, params: String): boolean;
var
  s: TSite;
  i: integer;
  user: String;
  leech_up: String;
  leech_dn: String;
  ratio_up: String;
  ratio_dn: String;
begin
  user := SubString(params, ' ', 1);

  leech_up := '';
  leech_dn := '';
  ratio_up := '';
  ratio_dn := '';

  for i := 0 to sites.Count - 1 do
  begin
    s := TSite(sites[i]);

    if s.IsLeecher(user) then
    begin
      if s.WorkingStatus = sstUp then
        leech_up := leech_up + format('<b>%s</b> (%d/%d) ',
          [s.Name, s.FreeTraderSlots, s.FreeLeechSlots])
      else
        leech_dn := leech_dn + format('<b>%s</b> (%d/%d) ',
          [s.Name, s.FreeTraderSlots, s.FreeLeechSlots]);
    end
    else if s.IsTrader(user) then
    begin
      if s.WorkingStatus = sstUp then
        ratio_up := ratio_up + format('<b>%s</b> (%d/%d) ',
          [s.Name, s.FreeTraderSlots, s.FreeLeechSlots])
      else
        ratio_dn := ratio_dn + format('<b>%s</b> (%d/%d) ',
          [s.Name, s.FreeTraderSlots, s.FreeLeechSlots]);
    end;

  end;

  if leech_up <> '' then
    irc_addtext(Netname, Channel, 'Leech up: %s', [leech_up]);
  if leech_dn <> '' then
    irc_addtext(Netname, Channel, 'Leech dn: %s', [leech_dn]);
  if ratio_up <> '' then
    irc_addtext(Netname, Channel, 'Ratio up: %s', [ratio_up]);
  if ratio_dn <> '' then
    irc_addtext(Netname, Channel, 'Ratio dn: %s', [ratio_dn]);

  Result := True;
end;

end.