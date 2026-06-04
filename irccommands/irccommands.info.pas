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
function IrcFindAffil(const netname, channel, params: String): boolean;
function IrcFindCountry(const netname, channel, params: String): boolean;
function IrcFindSection(const netname, channel, params: String): boolean;

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

  x := TStringList.Create;
  try
    irc_addtext(Netname, Channel, '<b>Site</b> %s:', [s.Name]);
    irc_addtext(Netname, Channel, ' username/irc nick/proxyname: %s / %s / %s', [s.UserName, s.IRCNick, s.ProxyName]);
    irc_addtext(Netname, Channel, ' name/link speed/location/size: %s / %s / %s / %s', [s.SiteFullName, s.SiteLinkSpeed, s.Country, s.SiteSize]);
    irc_addtext(Netname, Channel, ' notes: %s', [s.SiteNotes]);
    irc_addtext(Netname, Channel, ' infos: %s', [s.SiteInfos]);
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
  s.SiteFullName := Name;
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
  s.SiteLinkSpeed := link;
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

  if Pos(',', affils) <> 0 then
  begin
    irc_addtext(Netname, Channel, '<b><c4>ERROR</c></b>: Only whitespace allowed as delimiter.');
    exit;
  end;

  s.siteAffils := affils;

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
  s.SiteSize := size;
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
  s.SiteNotes := notes;
  irc_addtext(Netname, Channel, notes);

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

end.
