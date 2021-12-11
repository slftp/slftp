unit irccommands.site;

interface

{ slftp site commands functions }
function IrcSites(const netname, channel, params: String): boolean;
function IrcSite(const netname, channel, params: String): boolean;
function IrcAddSite(const netname, channel, params: String): boolean;
function IrcDelsite(const netname, channel, params: String): boolean;
function IrcAddBnc(const netname, channel, params: String): boolean;
function IrcDelBnc(const netname, channel, params: String): boolean;
function IrcSiteUser(const netname, channel, params: String): boolean;
function IrcSitePass(const netname, channel, params: String): boolean;
function IrcSetdown(const netname, channel, params: String): boolean;
function IrcSetSitePermdown(const netname, channel, params: String): boolean;
function IrcSetDir(const netname, channel, params: String): boolean;
function IrcSlots(const netname, channel, params: String): boolean;
function IrcMaxUpDn(const netname, channel, params: String): boolean;
function IrcMaxUpPerRip(const netname, channel, params: String): boolean;
function IrcMaxIdle(const netname, channel, params: String): boolean;
function IrcTimeout(const netname, channel, params: String): boolean;
function IrcSslfxp(const netname, channel, params: String): boolean;
function IrcSslmethod(const netname, channel, params: String): boolean;
function IrcLegacyCwd(const netname, channel, params: String): boolean;
function IrcSkipBeingUploadedFiles(const netname, channel, params: String): boolean;
function IrcSiteUserFetch(const netname, channel, params: String): boolean;
function IrcUseForNFOdownload(const netname, channel, params: String): boolean;
function IrcAutoLogin(const netname, channel, params: String): boolean;
function IrcAutoBncTest(const netname, channel, params: String): boolean;
function IrcShowCredits(const netname, channel, params: String): boolean;
function IrcAddSiteInfos(const netname, channel, params: String): boolean;
function IrcSlotsShow(const netname, channel, params: String): boolean;
function IrcBnc(const netname, channel, params: String): boolean;
function IrcBnctest(const netname, channel, params: String): boolean;
function IrcKill(const netname, channel, params: String): boolean;
function IrcRebuildSlot(const netname, channel, params: String): boolean;
function IrcRecalcFreeslots(const netname, channel, params: String): boolean;
function IrcSetDownOnOutOfSpace(const netname, channel, params: String): boolean;
function IrcSetReverseFxp(const netname, channel, params: String): boolean;
function IrcUseSiteSearchOnReqfill(const netname, channel, params: String): boolean;
function IrcReducedSpeedstatWeight(const netname, channel, params: String): boolean;

implementation

uses
  SysUtils, Classes, StrUtils, Contnrs, irc, sitesunit, queueunit, mystrings, notify, taskraw, RegExpr,
  globals, indexer, ranksunit, kb, configunit, precatcher, speedstatsunit, statsunit, rulesunit,
  mainthread, tasklogin, irccommandsunit;

const
  section = 'irccommands.site';

{$I common.inc}

function _Bnctest(const Netname, Channel: String; s: TSite; tn: TTaskNotify; kill: boolean = False): boolean;
var
  l: TLoginTask;
begin
  l := TLoginTask.Create(Netname, Channel, s.Name, kill, False);
  if tn <> nil then
    tn.tasks.Add(l);

  l.startat := GiveSiteLastStart;
  AddTask(l);

  Result := True;
end;



function IrcSites(const netname, channel, params: String): boolean;
var
  spd, sup, sdn, suk: TStringList;
  scount: integer;
begin
  scount := sites.Count - 1;

  sup := TStringList.Create;
  spd := TStringList.Create;
  sdn := TStringList.Create;
  suk := TStringList.Create;
  try
    SitesWorkingStatusToStringlist(Netname, Channel, sup, sdn, suk, spd);

    // make it alphabetically
    sup.Sort;
    spd.Sort;
    sdn.Sort;
    suk.Sort;

    IrcLineBreak(Netname, Channel, sup.commatext, AnsiChar('"'), '<' + globals.SiteColorOnline + '>UP</c>(' + IntToStr(sup.Count) + '/' + IntToStr(scount) + '): ');
    IrcLineBreak(Netname, Channel, sdn.commatext, AnsiChar('"'), '<' + globals.SiteColorOffline + '>DN</c>(' + IntToStr(sdn.Count) + '/' + IntToStr(scount) + '): ');
    IrcLineBreak(Netname, Channel, suk.commatext, AnsiChar('"'), '<' + globals.SiteColorUnknown + '>??</c>(' + IntToStr(suk.Count) + '/' + IntToStr(scount) + '): ');
    IrcLineBreak(Netname, Channel, spd.commatext, AnsiChar('"'), '<' + globals.SiteColorPermdown + '>PD</c>(' + IntToStr(spd.Count) + '/' + IntToStr(scount) + '): ');
  finally
    sup.Free;
    spd.Free;
    sdn.Free;
    suk.Free;
  end;

  Result := True;
end;

function IrcSite(const netname, channel, params: String): boolean;
var
  i, i_sec, j_sec: integer;
  s: TSite;
  host, sitename: String;
  x: TStringList;
  s_section, s_sections: String;
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
    sitesdat.ReadSection('site-' + sitename, x);
    x.Sort;

    irc_addtext(Netname, Channel, 'Site <b>%s</b>:', [sitename]);
    for i := 0 to x.Count - 1 do
    begin
      if x[i] = 'password' then
        Continue;
      if (Copy(x[i], 1, 3) = 'bnc') then
        Continue;

      if x.Strings[i] = 'sslmethod' then
      begin
        irc_addtext(Netname, Channel, ' %s: %s (%s)', [x[i], s.RCString(x[i], ''), sslMethodToSTring(s)]);
        Continue;
      end;

      if x.Strings[i] = 'sw' then
      begin
        irc_addtext(Netname, Channel, ' %s: %s (%s)', [x[i], s.RCString(x[i], ''), SiteSoftWareToSTring(s)]);
        Continue;
      end;

      if (StartsStr('pretime', x.Strings[i]) or (x.Strings[i] = 'pretime-*')) then
      begin
        if s.RCInteger(x.Strings[i], 120) > 60 then
        begin
          case round(s.RCInteger(x.Strings[i], 120) / 60) of
            1:
              irc_addtext(Netname, Channel, ' %s: %s seconds (%d minute)', [x[i],
                s.RCString(x[i], ''), round(s.RCInteger(x.Strings[i], 120) / 60)]);
          else
            irc_addtext(Netname, Channel, ' %s: %s seconds (%d minutes)', [x[i],
              s.RCString(x[i], ''), round(s.RCInteger(x.Strings[i], 120) / 60)]);
          end;
        end
        else
          irc_addtext(Netname, Channel, ' %s: %s seconds', [x[i], s.RCString(x[i], '')]);
        Continue;
      end;

      if x.Strings[i] = 'affils' then
      begin
        IrcLineBreak(Netname, Channel, s.RCString(x.Strings[i], ''), ' ', ' affils: ');
        Continue;
      end;

      if x.Strings[i] = 'country' then
      begin
        if (s.RCString(x[i], '')[1] <> '.') then
        begin
          irc_addtext(Netname, Channel, ' %s: %s', [x[i], s.RCString(x[i], '')]);
          continue;
        end
        else
        begin
          //we can use j_sec because it's set to 0 below when used there - so no need to create a new integer variable!
          j_sec := AnsiIndexText(copy(s.RCString(x[i], ''), 2, length(s.RCString(x[i], ''))), CountryCodes);
          irc_addtext(Netname, Channel, ' %s: %s (%s)', [x[i], s.RCString(x[i], ''), CountryNames[j_sec]]);
          continue;
        end;
      end
      else
      begin
        if ((x.Strings[i] = 'sections') or (x.Strings[i] = 'autoindexsections')) then
        begin
          j_sec := 0;
          s_sections := '';
          for i_sec := 1 to 1000 do
          begin
            s_section := SubString(s.RCString(x[i], ''), ' ', i_sec);
            if s_section = '' then
              break;

            if s_sections <> '' then
            begin
              s_sections := s_sections + ', ' + s_section;
            end
            else
            begin
              s_sections := s_section;
            end;
            Inc(j_sec);

            if (j_sec >= 10) then
            begin
              IrcLineBreak(Netname, Channel, s_sections, ',', ' ' + x.Strings[i] + ': ', 9);
              j_sec := 0;
              s_sections := '';
            end;
          end;
          if s_sections <> '' then
          begin
            IrcLineBreak(Netname, Channel, s_sections, ',', ' ' + x.Strings[i] + ': ', 9);
          end;
        end
        else
        begin
          irc_addtext(Netname, Channel, ' %s: %s', [x[i], s.RCString(x[i], '')]);
        end;
      end;
    end;

  finally
    x.Free;
  end;

  i := 0;
  while (not slshutdown) do
  begin
    host := s.RCString('bnc_host-' + IntToStr(i), '');
    if host = '' then
      break;

    irc_addtext(Netname, Channel, ' bnc: %s:%d', [host, s.RCInteger('bnc_port-' + IntToStr(i), 0)]);
    Inc(i);
  end;

  Result := True;
end;

function IrcAddSite(const netname, channel, params: String): boolean;
var
  sitename, username, password: String;
  s: TSite;
  bnc: String;
  bnchost: String;
  bncport: integer;
  i: integer;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  username := SubString(params, ' ', 2);
  password := SubString(params, ' ', 3);
  bnc := SubString(params, ' ', 4);
  bnchost := SubString(bnc, ':', 1);
  bncport := StrToIntDef(SubString(bnc, ':', 2), 0);

  if (sitename = '*') then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  if (0 < Pos('-', sitename)) then
  begin
    irc_addtext(Netname, Channel, 'Sitename cant contain -.');
    exit;
  end;

  if (username = '') or (password = '') then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  if (bnchost = '') or (bncport = 0) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  s := FindSiteByName(Netname, sitename);
  if s <> nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> already added.', [sitename]);
    exit;
  end;

  i := 4;
  while (True) do
  begin
    bnc := SubString(params, ' ', i);
    bnchost := SubString(bnc, ':', 1);
    bncport := StrToIntDef(SubString(bnc, ':', 2), 0);
    if ((bnchost = '') or (bncport = 0)) then
      break;

    sitesdat.WriteString('site-' + sitename, 'bnc_host-' + IntToStr(i - 4), bnchost);
    sitesdat.WriteInteger('site-' + sitename, 'bnc_port-' + IntToStr(i - 4), bncport);

    Inc(i);
  end;

  sites.Add(TSite.Create(sitename));

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Adding Site <b>%s</b> failed.', [sitename]);
    exit;
  end;

  s.UserName := username;
  s.PassWord := password;

  Result := True;
end;

function IrcDelsite(const netname, channel, params: String): boolean;
var
  sitename: String;
  s: TSite;
  i: integer;
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

  s.Stop;

  try
    try
      s.DeleteKey('autodirlist');
      s.DeleteKey('autodirlistsections');
      s.DeleteKey('nextautodirlist');
      s.RemoveAutoDirlist;
    except
      on E: Exception do
        irc_addtext(Netname, Channel, 'Remove <b>autodirlist</b> failed : %s', [E.Message]);
    end;

    try
      s.DeleteKey('autonuke');
      s.DeleteKey('nextautonuke');
      s.RemoveAutoNuke;
    except
      on E: Exception do
        irc_addtext(Netname, Channel, 'Remove <b>autonuke</b> failed : %s', [E.Message]);
    end;

    try
      s.DeleteKey('autobnctest');
      s.RemoveAutoBnctest;
    except
      on E: Exception do
        irc_addtext(Netname, Channel, 'Remove <b>autobnctest</b> failed : %s', [E.Message]);
    end;

    try
      s.DeleteKey('autorules');
      s.RemoveAutoRules;
    except
      on E: Exception do
        irc_addtext(Netname, Channel, 'Remove <b>autorules</b> failed : %s', [E.Message]);
    end;

    try
      s.DeleteKey('autoindex');
      s.DeleteKey('autoindexsections');
      s.DeleteKey('nextautoindex');
      s.RemoveAutoIndex;
      indexerRemoveSiteSection(s.Name, '');
    except
      on E: Exception do
        irc_addtext(Netname, Channel, 'Remove <b>autoindex</b> failed : %s', [E.Message]);
    end;

    try
      x := TStringList.Create;
      try
        sitesdat.ReadSection('site-' + sitename, x);
        for i := 0 to x.Count - 1 do
          sitesdat.DeleteKey('site-' + sitename, x.Strings[i]);
      finally
        x.Free;
      end;
    except
      on E: Exception do
        irc_addtext(Netname, Channel, 'Wipeing section SITE-%s failed : %s', [sitename, E.Message]);
    end;

    try
      sitesdat.EraseSection('speed-from-' + sitename);
      sitesdat.EraseSection('speed-to-' + sitename);

      for i := 0 to sites.Count - 1 do
      begin
        sitesdat.DeleteKey('speed-from-' + TSite(sites.Items[i]).Name, sitename);
        sitesdat.DeleteKey('speed-to-' + TSite(sites.Items[i]).Name, sitename);
      end;

    except
      on E: Exception do
        irc_addtext(Netname, Channel, 'Remove <b>routes</b> failed : %s', [E.Message]);
    end;

    try
      RulesRemove(sitename, '');
      RulesSave;
    except
      on E: Exception do
        irc_addtext(Netname, Channel, '<b>Rules remove</b> failed : %s', [E.Message]);
    end;

    try
      RemoveRanks(sitename);
      RanksSave;
      RanksReload;
    except
      on E: Exception do
        irc_addtext(Netname, Channel, 'Remove <b>ranks</b> failed : %s', [E.Message]);
    end;

    try
      Precatcher_DelSiteChans(sitename);
      PrecatcherRebuild;
    except
      on E: Exception do
        irc_addtext(Netname, Channel, 'Remove <b>catches</b> failed : %s', [E.Message]);
    end;

    if not RemoveStats(sitename) then
      irc_addtext(Netname, Channel, '<b>Stats info remove</b> failed');

    try
      sitesdat.EraseSection('site-' + sitename);
    except
      on E: Exception do
        irc_addtext(Netname, Channel, 'Erase <b>site section</b> failed : %s', [E.Message]);
    end;

    try
      sites.Delete(sites.IndexOf(s));
    except
      on E: Exception do
        irc_addtext(Netname, Channel, 'Remove <b>TSite Object</b> failed : %s', [E.Message]);
    end;
  finally
    sitesdat.UpdateFile;
  end;

  Result := True;
end;

function IrcAddBnc(const netname, channel, params: String): boolean;
var
  sitename: String;
  s: TSite;
  aktbnc, bnc: String;
  bnchost: String;
  bncport: integer;
  i: integer;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  bnc := SubString(params, ' ', 2);
  bnchost := SubString(bnc, ':', 1);
  bncport := StrToIntDef(SubString(bnc, ':', 2), 0);

  if (bnchost = '') or (bncport = 0) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
    exit;
  end;

  i := 0;
  while (True) do
  begin
    aktbnc := s.RCString('bnc_host-' + IntToStr(i), '');
    if (aktbnc = '') then
      break;
    Inc(i);
  end;
  s.WCString('bnc_host-' + IntToStr(i), bnchost);
  s.WCInteger('bnc_port-' + IntToStr(i), bncport);

  Result := True;
end;

function IrcDelBnc(const netname, channel, params: String): boolean;
var
  sitename: String;
  s: TSite;
  bnc: String;
  aktbnchost, bnchost: String;
  aktbncport, bncport: integer;
  i: integer;
  megvan: boolean;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  bnc := SubString(params, ' ', 2);
  bnchost := SubString(bnc, ':', 1);
  bncport := StrToIntDef(SubString(bnc, ':', 2), 0);

  if (bnchost = '') or (bncport = 0) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
    exit;
  end;

  i := 0;
  megvan := False;
  while (True) do
  begin
    aktbnchost := s.RCString('bnc_host-' + IntToStr(i), '');
    aktbncport := s.RCInteger('bnc_port-' + IntToStr(i), 0);
    if (aktbnchost = '') then
      break;

    if (not megvan) then
    begin
      if (aktbnchost = bnchost) and (aktbncport = bncport) then
      begin
        megvan := True;
        sitesdat.DeleteKey('site-' + sitename, 'bnc_host-' + IntToStr(i));
        sitesdat.DeleteKey('site-' + sitename, 'bnc_port-' + IntToStr(i));
      end;
    end
    else
    begin
      sitesdat.DeleteKey('site-' + sitename, 'bnc_host-' + IntToStr(i));
      sitesdat.DeleteKey('site-' + sitename, 'bnc_port-' + IntToStr(i));
      s.WCString('bnc_host-' + IntToStr(i - 1), aktbnchost);
      s.WCInteger('bnc_port-' + IntToStr(i - 1), aktbncport);
    end;
    Inc(i);
  end;

  if (not megvan) then
  begin
    irc_addtext(Netname, Channel, 'Bnc not found.');
    exit;
  end;

  Result := True;
end;

function IrcSiteUser(const netname, channel, params: String): boolean;
var
  username, sname: String;
  s: TSite;
begin
  Result := False;

  // parse parameters
  sname := UpperCase(SubString(params, ' ', 1));
  username := SubString(params, ' ', 2);

  // lookup site
  s := FindSiteByName('', sname);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Error</b></c>: Site %s not found!', [sname]);
    exit;
  end;

  // not for admin site
  if s.Name = getAdminSiteName then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Error</b></c>: This command is not allowed on internal site <b>%s</b>!', [sname]);
    exit;
  end;

  // no username has been specified
  if Trim(username) = '' then
  begin
    irc_addtext(Netname, Channel, 'Current username on <b>%s</b>: <b>%s</b>', [sname, s.username]);
    Result := True;
    exit;
  end;

  // set the new username
  s.username := username;
  irc_addtext(Netname, Channel, 'Username on <b>%s</b> set to: <b>%s</b>', [sname, s.username]);

  Result := True;
end;

function IrcSitePass(const netname, channel, params: String): boolean;
var
  password, sname: String;
  s: TSite;
begin
  Result := False;

  // parse parameters
  sname := UpperCase(SubString(params, ' ', 1));
  password := SubString(params, ' ', 2);

  // lookup site
  s := FindSiteByName('', sname);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Error</b></c>: Site %s not found!', [sname]);
    exit;
  end;

  // not for admin site
  if s.Name = getAdminSiteName then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Error</b></c>: This command is not allowed on internal site <b>%s</b>!', [sname]);
    exit;
  end;

  // no password has been specified
  if Trim(password) = '' then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Error</b></c>: No password specified!');
    exit;
  end;

  // set the new password
  s.password := password;
  irc_addtext(Netname, Channel, 'Password on <b>%s</b> set to: <b>%s</b>', [sname, '<censored>']);

  Result := True;
end;

function IrcSetdown(const netname, channel, params: String): boolean;
var
  s: TSite;
  i: integer;
  x: TStringList;
begin
  Result := False;
  x := TStringList.Create;
  try
    x.DelimitedText := UpperCase(params);

    if (x.Strings[0] = '!ALL!') or (x.Strings[0] = '*') then
    begin
      for i := 0 to sites.Count - 1 do
      begin
        if (TSite(sites.Items[i]).Name = getAdminSiteName) then
          Continue;
        if (TSite(sites.Items[i]).PermDown) then
          Continue;

        s := TSite(sites[i]);
        s.WorkingStatus := sstMarkedAsDownByUser;
      end;
    end
    else
    begin
      for i := 0 to x.Count - 1 do
      begin
        s := FindSiteByName(Netname, x.Strings[i]);

        if s = nil then
        begin
          irc_addtext(Netname, Channel, '<c4><b>ERROR</c></b>: Site <b>%s</b> not found.', [x.Strings[i]]);
          Continue;
        end;

        if (s.Name = getAdminSiteName) then
          Continue;
        if (s.PermDown) then
          Continue;

        s.WorkingStatus := sstMarkedAsDownByUser;
      end;
    end;
  finally
    x.Free;
  end;

  QueueFire; //to remove entries from queue

  Result := True;
end;

function IrcSetSitePermdown(const netname, channel, params: String): boolean;
var
  s: TSite;
  sname: String;
  svalue: String;
  ivalue: integer;
begin
  Result := False;
  sname := UpperCase(SubString(params, ' ', 1));
  svalue := UpperCase(SubString(params, ' ', 2));

  if svalue = '' then
  begin
    ivalue := 1 // default value if no parameter is given
  end
  else
  begin
    ivalue := StrToIntDef(svalue, 0);
  end;

  if ((ivalue > 1) or (ivalue < 0)) then
  begin
    irc_AddText(Netname, Channel, '<c4><b>Syntax Error!</b></c> %d is not valid, 1 or 0', [ivalue]);
    exit;
  end;

  s := FindSiteByName('', sname);

  if s = nil then
  begin
    irc_AddText(Netname, Channel, '<c4><b>Site not found</b></c> with name: %s', [sname]);
    exit;
  end;

  if boolean(ivalue) then
  begin
    try
      s.RemoveAutoIndex;
      s.RemoveAutoBnctest;
      s.RemoveAutoRules;
      s.RemoveAutoNuke;
      s.RemoveAutoDirlist;
    except
      on E: Exception do
        irc_AddText(Netname, Channel, '<c4>[Exception]</c> in remove auto tasks: %s', [E.Message]);
    end;

    try
      s.WorkingStatus := sstDown;
    except
      on E: Exception do
        irc_AddText(Netname, Channel, '<c4>[Exception]</c> in mark as down: %s', [E.Message]);
    end;

    try
      QueueEmpty(s.Name);
    except on E: Exception do
        irc_AddText(Netname, Channel, '<c4>[Exception]</c> in QueueEmpty: %s', [E.Message]);
    end;
    try
      // rewrite config value
      s.WCInteger('disabled_autonuke', s.AutoNukeInterval);
      s.WCInteger('disabled_autoindex', s.AutoIndexInterval);
      s.WCInteger('disabled_autobnctest', s.AutoBncTestInterval);
      s.WCInteger('disabled_autorules', s.AutoRulesStatus);
      s.WCInteger('disabled_autodirlist', s.AutoDirlistInterval);
      // s.WCInteger('disabled_autologin',s.RCInteger('autologin',0));
    except
      on E: Exception do
        irc_AddText(Netname, Channel, Format('<c4>[Exception]</c> in rewrite value: %s', [E.Message]));
    end;

    try
      sitesdat.DeleteKey('site-' + s.Name, 'autonuke');
      sitesdat.DeleteKey('site-' + s.Name, 'autoindex');
      sitesdat.DeleteKey('site-' + s.Name, 'autobnctest');
      sitesdat.DeleteKey('site-' + s.Name, 'autorules');
      sitesdat.DeleteKey('site-' + s.Name, 'autodirlist');
      // sitesdat.DeleteKey('site-'+s.name,'autologin');
      // sitesdat.UpdateFile;
    except
      on E: Exception do
        irc_AddText(Netname, Channel, '<c4>[Exception]</c> in delete old value: %s', [E.Message]);
    end;
  end
  else
  begin

    try
      // rewrite config value
      s.AutoNukeInterval := s.RCInteger('disabled_autonuke', 0);
      s.AutoIndexInterval := s.RCInteger('disabled_autoindex', 0);
      s.AutoBncTestInterval := s.RCInteger('disabled_autobnctest', 0);
      s.AutoRulesStatus := s.RCInteger('disabled_autorules', 0);
      s.AutoDirlistInterval := s.RCInteger('disabled_autodirlist', 0);
      // s.WCInteger('autologin',s.RCInteger('disabled_autologin',0));
    except
      on E: Exception do
        irc_AddText(Netname, Channel, '<c4>[Exception]</c> in rewrite orig. value: %s', [E.Message]);
    end;

    try
      sitesdat.DeleteKey('site-' + s.Name, 'disabled_autonuke');
      sitesdat.DeleteKey('site-' + s.Name, 'disabled_autoindex');
      sitesdat.DeleteKey('site-' + s.Name, 'disabled_autobnctest');
      sitesdat.DeleteKey('site-' + s.Name, 'disabled_autorules');
      sitesdat.DeleteKey('site-' + s.Name, 'disabled_autodirlist');
      // sitesdat.DeleteKey('site-'+s.name,'autologin');
      // sitesdat.UpdateFile;
    except
      on E: Exception do
        irc_AddText(Netname, Channel, '<c4>[Exception]</c> in delete disabled value: %s', [E.Message]);
    end;

    try
      s.AutoIndex;
      s.AutoBnctest;
      s.AutoRules;
      s.AutoNuke;
      s.AutoDirlist;
    except
      on E: Exception do
        irc_AddText(Netname, Channel, Format('<c4>[Exception]</c> in start auto tasks: %s', [E.Message]));
    end;
  end;

  s.PermDown := boolean(ivalue);

  Result := True;
end;

function IrcSetDir(const netname, channel, params: String): boolean;
var
  sitename, section: String;
  s: TSite;
  dir: String;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  section := UpperCase(SubString(params, ' ', 2));
  dir := mystrings.RightStr(params, length(sitename) + length(section) + 2);

  if ((section = '*') or (section = '')) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  if ((dir <> '') and (dir[1] <> '/')) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  if section = 'REQUESTS' then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b></c>, use REQUEST as section name.');
    exit;
  end;

  if section = 'SPEEDTESTS' then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b></c>, use SPEEDTEST as section name.');
    exit;
  end;

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, '<b><c4>Error</c></b>: Site <b>%s</b> not found.', [sitename]);
    exit;
  end;

  if (section <> 'SPEEDTEST') and (section <> 'REQUEST') and (not section.StartsWith('ARCH-')) and (not section.StartsWith('PRE')) then
  begin
    if (kb_sections.IndexOf(section) = -1) and (dir <> '') then
    begin
      irc_addtext(Netname, Channel,
        '<b><c4>Error</c></b>: Section <b>%s</b> not found. Hint: Section <b>%s</b> must be in your <b>slftp.precatcher</b> file at [sections] and/or [mappings].',
        [section, section]);
      exit;
    end;
  end;

  // if empty, it can be removed
  if dir = '' then
  begin
    s.SetSections(section, True);
    s.sectiondir[section] := '';
    s.sectionpretime[section] := -10;
    s.SetRankLock(section, 0);
    RulesRemove(sitename, section);
    RemoveRanks(sitename, section);
    // stats are not removed by section, only when site is deleted
    RemoveSpeedStats(sitename, section);
    irc_addtext(Netname, Channel, 'Section <b>%s</b> removed from site <b>%s</b>', [section, s.Name]);
  end
  else
  begin
    s.sectiondir[section] := dir;
    s.SetSections(section, False);
    irc_addtext(Netname, Channel, 'Section <b>%s</b> dir on site <b>%s</b> set to <b>%s</b>', [section, s.Name, dir]);
  end;

  Result := True;
end;

function IrcSlots(const netname, channel, params: String): boolean;
var
  sitename: String;
  ss: TStringList;
  s: TSite;
  oldslots, newslots: integer;
  ii, i: integer;
  fDoOutputOnly: boolean;
begin
  Result := False;

  fDoOutputOnly := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  newslots := StrToIntDef(SubString(params, ' ', 2), 0);

  if newslots <= 0 then
  begin
    fDoOutputOnly := True;
  end;

  if sitename = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      s := TSite(sites.Items[i]);
      if (s.Name = getAdminSiteName) then
        Continue;
      if s.PermDown then
        Continue;

      oldslots := s.slots.Count;

      if fDoOutputOnly then
      begin
         irc_addtext(Netname, Channel, Format('Site: %s - Slots count: %d', [s.Name, oldslots]));
         Continue;
      end;

      sitesdat.WriteInteger('site-' + s.Name, 'slots', newslots);
      if oldslots > newslots then
      begin
        // you have to remove some slots
        for ii := 1 to oldslots - newslots do
        begin
          TSiteSlot(s.slots[s.slots.Count - 1]).Stop;
          s.slots.Delete(s.slots.Count - 1);
        end;
      end
      else if oldslots < newslots then
      begin
        // new slots have to be added
        for ii := 1 to newslots - oldslots do
        begin
          s.slots.Add(TSiteSlot.Create(s, s.slots.Count));
        end;
      end;

      s.RecalcFreeslots;
    end;
  end
  else
  begin
    ss := TStringList.Create;
    try
      ss.commatext := sitename;

      for i := 0 to ss.Count - 1 do
      begin
        s := FindSiteByName(Netname, ss.Strings[i]);
        if s = nil then
        begin
          irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [ss.Strings[i]]);
          Continue;
        end;

        oldslots := s.slots.Count;

        if fDoOutputOnly then
        begin
          irc_addtext(Netname, Channel, Format('Site: %s - Slots count: %d', [s.Name, oldslots]));
          Continue;
        end;

        sitesdat.WriteInteger('site-' + s.Name, 'slots', newslots);
        if oldslots > newslots then
        begin
          // you have to remove some slots
          for ii := 1 to oldslots - newslots do
          begin
            TSiteSlot(s.slots[s.slots.Count - 1]).Stop;
            s.slots.Delete(s.slots.Count - 1);
          end;
        end
        else if oldslots < newslots then
        begin
          // new slots have to be added
          for ii := 1 to newslots - oldslots do
          begin
            s.slots.Add(TSiteSlot.Create(s, s.slots.Count));
          end;
        end;

        s.RecalcFreeslots;
      end;
    finally
      ss.Free;
    end;
  end;

  Result := True;
end;

function IrcMaxUpDn(const netname, channel, params: String): boolean;
var
  sitename: String;
  x: TStringList;
  s: TSite;
  up, dn, pre_dn: integer;
  i: integer;
begin
  Result := False;

  sitename := UpperCase(SubString(params, ' ', 1));
  up := StrToIntDef(SubString(params, ' ', 2), 0);
  dn := StrToIntDef(SubString(params, ' ', 3), 0);
  // optional setting, if empty it well be set to dn
  pre_dn := StrToIntDef(SubString(params, ' ', 4), 0);

  if (up < 0) or (dn < 0) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  if (pre_dn = 0) then
    pre_dn := dn;

  if sitename = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      if (TSite(sites.Items[i]).Name = getAdminSiteName) then
        Continue;
      if TSite(sites.Items[i]).PermDown then
        Continue;

      TSite(sites.Items[i]).max_dn := dn;
      TSite(sites.Items[i]).max_pre_dn := pre_dn;
      TSite(sites.Items[i]).max_up := up;
    end;
  end
  else
  begin
    x := TStringList.Create;
    try
      x.commatext := sitename;
      for i := 0 to x.Count - 1 do
      begin
        s := FindSiteByName(Netname, x.Strings[i]);
        if s = nil then
        begin
          irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [x.Strings[i]]);
          Continue;
        end;
        s.max_dn := dn;
        s.max_pre_dn := pre_dn;
        s.max_up := up;
      end;
    finally
      x.Free;
    end;
  end;

  Result := True;
end;

function IrcMaxUpPerRip(const netname, channel, params: String): boolean;
var
  sitename: String;
  s: TSite;
  upperrip: integer;
  i: integer;
  x: TStringList;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  upperrip := StrToIntDef(SubString(params, ' ', 2), -1);

  if (upperrip < -1) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  if sitename = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      if (TSite(sites.Items[i]).Name = getAdminSiteName) then
        Continue;
      if TSite(sites.Items[i]).PermDown then
        Continue;

      if (upperrip = -1) then
        irc_addtext(Netname, Channel, 'Site <b>%s</b> max. up per rip value: %d', [TSite(sites.Items[i]).Name, TSite(sites.Items[i]).MaxUpPerRip])
      else
        TSite(sites.Items[i]).MaxUpPerRip := upperrip;
    end;
  end
  else
  begin
    x := TStringList.Create;
    try
      x.commatext := sitename;
      for i := 0 to x.Count - 1 do
      begin
        s := FindSiteByName(Netname, x.Strings[i]);
        if s = nil then
        begin
          irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [x.Strings[i]]);
          Continue;
        end;
        if s.PermDown then
          Continue;

        if (upperrip = -1) then
          irc_addtext(Netname, Channel, 'Site <b>%s</b> max. up per rip value: %d', [s.Name, s.MaxUpPerRip])
        else
          s.MaxUpPerRip := upperrip;
      end;
    finally
      x.Free;
    end;
  end;

  Result := True;
end;

function IrcMaxIdle(const netname, channel, params: String): boolean;
var
  sitename: String;
  s: TSite;
  maxidle, idleinterval: integer;
  i: integer;
  x: TStringList;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  maxidle := StrToIntDef(SubString(params, ' ', 2), -1);
  idleinterval := StrToIntDef(SubString(params, ' ', 3), 0);

  if (maxidle = -1) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  if sitename = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      if (TSite(sites.Items[i]).Name = getAdminSiteName)
        then
        Continue;
      if TSite(sites.Items[i]).PermDown then
        Continue;

      TSite(sites.Items[i]).maxidle := maxidle;
      if idleinterval <> 0 then
        TSite(sites.Items[i]).idleinterval := idleinterval;
    end;
  end
  else
  begin
    x := TStringList.Create;
    try
      x.commatext := sitename;
      for i := 0 to x.Count - 1 do
      begin
        s := FindSiteByName(Netname, x.Strings[i]);
        if s = nil then
        begin
          irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [x.Strings[i]]);
          Continue;
        end;
        s.maxidle := maxidle;
        if idleinterval <> 0 then
          s.idleinterval := idleinterval;
      end;
    finally
      x.Free;
    end;
  end;

  Result := True;
end;

function IrcTimeout(const netname, channel, params: String): boolean;
var
  sitename: String;
  s: TSite;
  iotimeout, connnecttimeout: integer;
  i: integer;
  x: TStringList;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  connnecttimeout := StrToIntDef(SubString(params, ' ', 2), 0);
  iotimeout := StrToIntDef(SubString(params, ' ', 3), 0);

  if (connnecttimeout = 0) or (iotimeout = 0) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  if sitename = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      if (TSite(sites.Items[i]).Name = getAdminSiteName)
        then
        Continue;
      if TSite(sites.Items[i]).PermDown then
        Continue;
      TSite(sites.Items[i]).io_timeout := iotimeout;
      TSite(sites.Items[i]).connect_timeout := connnecttimeout;
    end;
  end
  else
  begin
    x := TStringList.Create;
    try
      x.commatext := sitename;
      for i := 0 to x.Count - 1 do
      begin
        s := FindSiteByName(Netname, x.Strings[i]);
        if s = nil then
        begin
          irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [x.Strings[i]]);
          Continue;
        end;

        s.io_timeout := iotimeout;
        s.connect_timeout := connnecttimeout;
      end;
    finally
      x.Free;
    end;
  end;

  Result := True;
end;

function IrcSslfxp(const netname, channel, params: String): boolean;
var
  s: String;
  sname: String;
  site: TSite;
  sslfxp: TSSLReq;
  i: integer;
  x: TStringList;
begin
  Result := False;

  sname := UpperCase(SubString(params, ' ', 1));
  s := SubString(params, ' ', 2);
  sslfxp := TSSLReq(StrToIntDef(s, 0));

  if sname = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      site := TSite(sites.Items[i]);
      if (site.Name = getAdminSiteName) then
        Continue;
      if s <> '' then
        site.sslfxp := sslfxp;
      if site.sslfxp = srNone then
        irc_addtext(Netname, Channel, '%s SSLFXP: False', [site.Name]);
      if site.sslfxp = srNeeded then
        irc_addtext(Netname, Channel, '%s SSLFXP: True', [site.Name]);
      if site.sslfxp = srUnsupported then
        irc_addtext(Netname, Channel, '%s SSLFXP: Unsupported', [site.Name]);
    end;
  end
  else
  begin
    x := TStringList.Create;
    try
      x.commatext := sname;
      for i := 0 to x.Count - 1 do
      begin
        site := FindSiteByName('', x.Strings[i]);
        if site = nil then
        begin
          irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [x.Strings[i]]);
          Continue;
        end;
        if s <> '' then
          site.sslfxp := sslfxp;
        if site.sslfxp = srNone then
          irc_addtext(Netname, Channel, '%s SSLFXP: False', [site.Name]);
        if site.sslfxp = srNeeded then
          irc_addtext(Netname, Channel, '%s SSLFXP: True', [site.Name]);
        if site.sslfxp = srUnsupported then
          irc_addtext(Netname, Channel, '%s SSLFXP: Unsupported', [site.Name]);
      end;
    finally
      x.Free;
    end;
  end;

  Result := True;
end;

function IrcSslmethod(const netname, channel, params: String): boolean;
var
  method, sitename: String;
  s: TSite;
  i: integer;
  x: TStringList;
begin
  sitename := UpperCase(SubString(params, ' ', 1));
  method := SubString(params, ' ', 2);
  i := StrToIntDef(method, -1);

  if ((method <> '') and ((i < 0) or (i > Integer(High(TSSLMethods))))) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</c></b>: %s is not valid SSL method.', [method]);
    Exit;
  end;

  if sitename = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      s := TSite(sites.Items[i]);
      if (s.Name = getAdminSiteName) then
        Continue;
      if s.PermDown then
        Continue;
      if method <> '' then
        s.sslmethod := TSSLMethods(StrToIntDef(method, integer(s.sslmethod)));

      irc_addText(Netname, Channel, 'SSL method for <b>%s</b>: %s', [sitename, sslMethodToSTring(s)]);
    end;
  end
  else
  begin
    x := TStringList.Create;
    try
      x.commatext := sitename;
      for i := 0 to x.Count - 1 do
      begin
        s := FindSiteByName(Netname, x.Strings[i]);
        if s = nil then
        begin
          irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [x.Strings[i]]);
          Continue;
        end;

        if method <> '' then
          s.sslmethod := TSSLMethods(StrToIntDef(method, integer(s.sslmethod)));

        irc_addText(Netname, Channel, 'SSL method for <b>%s</b>: %s', [sitename, sslMethodToSTring(s)]);
      end;
    finally
      x.Free;
    end;
  end;

  Result := True;
end;

function IrcLegacycwd(const netname, channel, params: String): boolean;
var
  sitename: String;
  s: TSite;
  cwd: integer;
  i: integer;
  x: TStringList;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  cwd := StrToIntDef(SubString(params, ' ', 2), -1);

  if sitename = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      if (TSite(sites.Items[i]).Name = getAdminSiteName) then
        Continue;
      if TSite(sites.Items[i]).PermDown then
        Continue;
      if cwd = 1 then
        TSite(sites.Items[i]).legacydirlist := True;
      if cwd = 0 then
        TSite(sites.Items[i]).legacydirlist := False;
      irc_addtext(Netname, Channel, 'Legacy dirlisting of site <b>%s</b> is %d', [TSite(sites.Items[i]).Name, integer(TSite(sites.Items[i]).legacydirlist)]);
    end;
  end
  else
  begin
    x := TStringList.Create;
    try
      x.commatext := sitename;
      for i := 0 to x.Count - 1 do
      begin
        s := FindSiteByName(Netname, x.Strings[i]);
        if s = nil then
        begin
          irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [x.Strings[i]]);
          Continue;
        end;
        if cwd = 1 then
          s.legacydirlist := True;
        if cwd = 0 then
          s.legacydirlist := False;

        irc_addtext(Netname, Channel, 'Legacy dirlisting of site <b>%s</b> is %d', [sitename, integer(s.legacydirlist)]);
      end;
    finally
      x.Free;
    end;
  end;

  Result := True;
end;

function IrcSkipBeingUploadedFiles(const netname, channel, params: String): boolean;
var
  svalue, sname: String;
  ss: TSite;
  i, fValueInt: integer;

  procedure _AnnounceConfigValue;
  begin
    irc_addtext(Netname, Channel, '%s skip incomplete files: %d (%s)', [ss.Name, Ord(ss.SkipBeingUploadedFiles), TEnum<TSkipBeingUploaded>.ToString(ss.SkipBeingUploadedFiles)]);
  end;
begin
  Result := False;
  sname := UpperCase(SubString(params, ' ', 1));
  sValue := SubString(params, ' ', 2);
  fValueInt := StrToIntDef(sValue, -1);

  //if svalue is empty that means the current setting should be displayed, so that is valid too
  if ((svalue <> '') and ((fValueInt < Ord(Low(TSkipBeingUploaded))) or (fValueInt > Ord(High(TSkipBeingUploaded))))) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</c></b>: %s is not valid.', [svalue]);
    Exit;
  end;

  if sname = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      ss := TSite(sites.Items[i]);
      if (ss.Name = getAdminSiteName) then
        Continue;

      if svalue = '' then
        _AnnounceConfigValue
      else
      begin
        ss.SkipBeingUploadedFiles := TSkipBeingUploaded(fValueInt);
        _AnnounceConfigValue;
      end
    end;
  end
  else
  begin
    ss := FindSiteByName('', sname);
    if ss = nil then
    begin
      irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [ss.Name]);
      Result := True;
      exit;
    end;

    if svalue = '' then
      _AnnounceConfigValue
    else
    begin
      ss.SkipBeingUploadedFiles := TSkipBeingUploaded(fValueInt);
      _AnnounceConfigValue;
    end
  end;

  Result := True;
end;

function IrcSiteUserFetch(const netname, channel, params: String): boolean;
var
  i: integer;
  s: TSite;
  r: TRawTask;
  tn: TTaskNotify;
  x: TRegExpr;
  sitename, response, username: String;
  logins, maxdn, maxup: String;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  username := SubString(params, ' ', 2);

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
    exit;
  end;

  if (s.Name = getAdminSiteName) then
  begin
    exit;
  end;

  // username can be omitted or you specify another name (e.g. if your a siteop)
  if (username = '') then
    username := s.username;

  tn := AddNotify;
  try
    r := TRawTask.Create(Netname, Channel, s.Name, '', 'SITE USER ' + username);
    tn.tasks.Add(r);
    AddTask(r);
    QueueFire;
    tn.event.WaitFor($FFFFFFFF);
  except
  on E: Exception do
    begin
      RemoveTN(tn);
      irc_addtext(Netname, Channel, '<c4><b>ERROR</c></b>: %s', [e.Message]);
      Exit;
    end;
  end;

  for i := 0 to tn.responses.Count - 1 do
  begin
    response := TSiteResponse(tn.responses[i]).response;

    if ((0 <> Pos('You do not have access', response)) or (0 <> Pos('Access denied', response))) then
    begin
      irc_addtext(Netname, Channel, '<c4><b>ERROR</c></b>: You do not have access to this command.');
      break;
    end;

    if (0 <> Pos('does not exist', response)) then
    begin
      irc_addtext(Netname, Channel, Format('<c4><b>ERROR</c></b>: User %s does not exist.', [username]));
      break;
    end;

    x := TRegExpr.Create;
    try
      x.ModifierI := True;

      x.Expression := 'Max Logins\: (\d+|Unlimited)';
      if x.Exec(response) then
      begin
        //irc_addtext(Netname, Channel, 'Max Logins: %s', [x.Match[1]]);
        logins := IfThen(x.Match[1] = 'Unlimited', '999', x.Match[1]);
      end;

      x.Expression := 'Max Sim Uploads\: (\d+|Unlimited)';
      if x.Exec(response) then
      begin
        //irc_addtext(Netname, Channel, 'Max Sim Uploads: %s', [x.Match[1]]);
        maxup := IfThen(x.Match[1] = 'Unlimited', '999', x.Match[1]);
      end;

      x.Expression := 'Max Sim Downloads\: (\d+|Unlimited)';
      if x.Exec(response) then
      begin
        //irc_addtext(Netname, Channel, 'Max Sim Downloads: %s', [x.Match[1]]);
        maxdn := IfThen(x.Match[1] = 'Unlimited', '999', x.Match[1]);
      end;

    finally
      x.free;
    end;
  end;

  RemoveTN(tn);

  if (logins <> '') and (maxup <> '') and (maxdn <> '') then
  begin
    irc_addtext(Netname, Channel, 'Slots:');
    irc_addtext(Netname, Channel, '!slots %s %s', [s.Name, logins]);
    irc_addtext(Netname, Channel, 'Max. number of slots for uploading/downloading:');
    irc_addtext(Netname, Channel, '!maxupdn %s %s %s', [s.Name, maxup, maxdn]);
  end;

  Result := True;
end;

function IrcUseForNFOdownload(const netname, channel, params: String): boolean;
var
  sname: String;
  svalue: integer;
  ss: TSite;
  i: integer;
begin
  Result := False;
  sname := UpperCase(SubString(params, ' ', 1));
  svalue := StrToIntDef(SubString(params, ' ', 2), -1);

  if sname = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      ss := TSite(sites.Items[i]);
      if (ss.Name = getAdminSiteName) then
        Continue;
      if TSite(sites.Items[i]).PermDown then
        Continue;

      if svalue = -1 then
      begin
        irc_addtext(Netname, Channel, '%s use for NFO download: %d (%s)', [ss.Name, Ord(ss.UseForNFOdownload), ReplaceText(TEnum<TUseForNfoDownload>.ToString(ss.UseForNFOdownload), 'ufn', '')]);
      end
      else if ((svalue = 1) or (svalue = 0)) then
      begin
        ss.UseForNFOdownload := TUseForNfoDownload(svalue);
        irc_addtext(Netname, Channel, '%s use for NFO download: %d (%s)', [ss.Name, Ord(ss.UseForNFOdownload), ReplaceText(TEnum<TUseForNfoDownload>.ToString(ss.UseForNFOdownload), 'ufn', '')]);
      end
      else
        irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c> Only 0 and 1 as value allowed!');
    end;
  end
  else
  begin
    ss := FindSiteByName('', sname);
    if ss = nil then
    begin
      irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sname]);
      Result := True;
      exit;
    end;

    if svalue = -1 then
    begin
      irc_addtext(Netname, Channel, '%s use for NFO download: %d (%s)', [ss.Name, Ord(ss.UseForNFOdownload), ReplaceText(TEnum<TUseForNfoDownload>.ToString(ss.UseForNFOdownload), 'ufn', '')]);
    end
    else if ((svalue = 1) or (svalue = 0)) then
    begin
      ss.UseForNFOdownload := TUseForNfoDownload(svalue);
      irc_addtext(Netname, Channel, '%s use for NFO download: %d (%s)', [ss.Name, Ord(ss.UseForNFOdownload), ReplaceText(TEnum<TUseForNfoDownload>.ToString(ss.UseForNFOdownload), 'ufn', '')]);
    end
    else
      irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c> Only 0 and 1 as value allowed!');
  end;

  Result := True;
end;

function IrcSetReverseFxp(const netname, channel, params: String): boolean;
var
  fSiteName, fDirection: String;
  fDestinationMode: boolean;
  fValue: integer;
  fSite: TSite;
begin
  Result := False;

  //get the site
  fSiteName := UpperCase(SubString(params, ' ', 1));
  if fSiteName = getAdminSiteName then
  begin
    irc_addtext(Netname, Channel, '<c4><b>You can not use admin site with this function</b>.</c>');
    exit;
  end;

  fSite := FindSiteByName('', fSiteName);
  if fSite = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [fSiteName]);
    exit;
  end;

  //get the direction for which reverse fxp should be set (when site is used as source or as destination?)
  fDirection := LowerCase(SubString(params, ' ', 2));

  //if only site parameter is given, show current value for both directions
  if fDirection.IsEmpty then
  begin
    irc_addtext(Netname, Channel, '%s use reverse FXP when destination: %d', [fSite.Name, ord(fSite.UseReverseFxpDestination)]);
    irc_addtext(Netname, Channel, '%s use reverse FXP when source: %d', [fSite.Name, ord(fSite.UseReverseFxpSource)]);
    Result := True;
    exit;
  end;

  if (fDirection = '--source') or (fDirection = '-s') then
    fDestinationMode := False
  else if (fDirection = '--destination') or (fDirection = '-d') then
    fDestinationMode := True
  else
  begin
    irc_addtext(Netname, Channel, 'Unknown parameter <b>%s</b>. Use either --source (-s) or --destination (-d).', [fDirection]);
    exit;
  end;

  //get the value (1 - enable, 0 - disable)
  fValue := StrToIntDef(SubString(params, ' ', 3), -1);

  //if the value parameter is missing, display the currently set value for the site
  if fValue = -1 then
  begin
  if fDestinationMode then
    irc_addtext(Netname, Channel, '%s use reverse FXP when destination: %d', [fSite.Name, ord(fSite.UseReverseFxpDestination)])
  else
    irc_addtext(Netname, Channel, '%s use reverse FXP when source: %d', [fSite.Name, ord(fSite.UseReverseFxpSource)]);
  end

  //if a value is given, set it to the site
  else if ((fValue = 1) or (fValue = 0)) then
  begin
    if fDestinationMode then
    begin
      fSite.UseReverseFxpDestination := boolean(fValue);
      irc_addtext(Netname, Channel, '%s use reverse FXP when destination: %d', [fSite.Name, ord(fSite.UseReverseFxpDestination)]);
    end
    else
    begin
      fSite.UseReverseFxpSource := boolean(fValue);
      irc_addtext(Netname, Channel, '%s use reverse FXP when source: %d', [fSite.Name, ord(fSite.UseReverseFxpSource)]);
    end;
  end
  else
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c> Only 0 and 1 as value allowed!');
    exit;
  end;

  Result := True;
end;

function IrcAutoLogin(const netname, channel, params: String): boolean;
var
  sitename: String;
  status: integer;
  s: TSite;
  i: integer;
  x: TStringList;
begin
  Result := True;
  sitename := UpperCase(SubString(params, ' ', 1));
  status := StrToIntDef(SubString(params, ' ', 2), -1);

  if sitename = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      if (TSite(sites.Items[i]).Name = getAdminSiteName) then
        Continue;
      if (TSite(sites.Items[i]).PermDown) then
        Continue;

      if status > -1 then
      begin
        TSite(sites.Items[i]).WCInteger('autologin', status);
      end;

      irc_addtext(Netname, Channel, 'Autologin of %s is: %d', [TSite(sites.Items[i]).Name, integer(TSite(sites.Items[i]).RCBool('autologin', False))]);
    end;
  end
  else
  begin
    x := TStringList.Create;
    try
      x.commatext := sitename;
      for i := 0 to x.Count - 1 do
      begin
        s := FindSiteByName(Netname, x.Strings[i]);

        if s = nil then
        begin
          irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [x.Strings[i]]);
          Continue;
        end;

        if (s.PermDown) then
        begin
          irc_addtext(Netname, Channel, 'Site <b>%s</b> is set to PermDown.', [x.Strings[i]]);
          Continue;
        end;

        if status > -1 then
        begin
          s.WCInteger('autologin', status);
        end;

        irc_addtext(Netname, Channel, 'Autologin of %s is: %d', [sitename, integer(s.RCBool('autologin', False))]);
      end;
    finally
      x.Free;
    end;
  end;

  Result := True;
end;

function IrcAutoBnctest(const netname, channel, params: String): boolean;
var
  sitename: String;
  status: integer;
  s: TSite;
  enableAutobnctest: boolean;
  i: integer;
  x: TStringList;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  status := StrToIntDef(SubString(params, ' ', 2), -1);

  if sitename = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      if (TSite(sites.Items[i]).Name = getAdminSiteName) then
        Continue;
      if (TSite(sites.Items[i]).PermDown) then
        Continue;

      enableAutobnctest := False;
      if status > -1 then
      begin
        if status <> 0 then
        begin
          if TSite(sites.Items[i]).AutoBncTestInterval <= 0 then
            enableAutobnctest := True;

          TSite(sites.Items[i]).AutoBncTestInterval := status;
        end
        else
        begin
          TSite(sites.Items[i]).DeleteKey('autobnctest');
          TSite(sites.Items[i]).RemoveAutoBnctest;
        end;
      end;

      irc_addtext(Netname, Channel, 'Autobnctest of %s is: %d', [TSite(sites.Items[i]).Name, TSite(sites.Items[i]).AutoBncTestInterval]);

      if enableAutobnctest then
        TSite(sites.Items[i]).AutoBnctest;
    end;
  end
  else
  begin
    x := TStringList.Create;
    try
      x.commatext := sitename;
      for i := 0 to x.Count - 1 do
      begin
        s := FindSiteByName(Netname, x.Strings[i]);
        if s = nil then
        begin
          irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [x.Strings[i]]);
          Continue;
        end;

        if (s.PermDown) then
        begin
          irc_addtext(Netname, Channel, 'Site <b>%s</b> is set to PermDown.', [x.Strings[i]]);
          Continue;
        end;

        enableAutobnctest := False;
        if status > -1 then
        begin
          if status <> 0 then
          begin
            if s.AutoBncTestInterval <= 0 then
              enableAutobnctest := True;

            s.AutoBncTestInterval := status;
          end
          else
          begin
            s.DeleteKey('autobnctest');
            s.RemoveAutoBnctest;
          end;
        end;

        irc_addtext(Netname, Channel, 'Autobnctest of %s is: %d', [sitename, s.AutoBncTestInterval]);

        if enableAutobnctest then
          s.AutoBnctest;
      end;
    finally
      x.Free;
    end;
  end;

  Result := True;
end;

function IrcShowCredits(const netname, channel, params: String): boolean;
var
  i: integer;
  sitename: String;
  sitesList : TStringList;
  site: TSite;

  procedure _ShowCredits(const Netname, Channel: String; s: TSite);
  var
    r: TRawTask;
    tn: TTaskNotify;
    fCredits, fRatio: String;
  begin
    if ((s = nil) or (s.Name = getAdminSiteName)) then
      exit;
    if (s.PermDown) then
    begin
      irc_addtext(Netname, Channel, '<c4><b>Site %s is set permdown! </c></b>', [s.Name]);
      exit;
    end;
    if not s.IsUp then
    begin
      irc_addtext(Netname, Channel, '<c4><b>Site %s is temporarily offline! </c></b>', [s.Name]);
      exit;
    end;

    tn := AddNotify;
    try
      try
        r := TRawTask.Create(Netname, Channel, s.Name, '', 'SITE STAT');
        tn.tasks.Add(r);
        AddTask(r);
        QueueFire;
        tn.event.WaitFor($FFFFFFFF);
      except on E: Exception do
        begin
          RemoveTN(tn);
          irc_addtext(Netname, Channel, '<c4><b>ERROR</c></b>: %s', [e.Message]);
          exit;
        end;
      end;

      ParseSTATLine(TSiteResponse(tn.responses[0]).response, fCredits, fRatio);
      // if negative, set color accordingly
      if AnsiContainsText(fCredits, '-') then
      begin
        fCredits := Format('<c4> %s </c>', [fCredits]);
      end
      else
      begin
        fCredits := Format('<c3> %s </c>', [fCredits]);
      end;

      fCredits := StringReplace(fCredits, ',', '.', [rfReplaceAll, rfIgnoreCase]);
      irc_addtext(Netname, Channel, Format('Credits on <b>%s</b>: %s (%s)', [s.Name, fCredits, fRatio]));
    finally
      RemoveTN(tn);
    end;
  end;

begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));

  if sitename = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      _ShowCredits(Netname, channel, TSite(sites.Items[i]));
    end;
  end
  else
  begin
    sitesList := TStringList.Create;
    try
      sitesList.Delimiter := ' ';
      sitesList.DelimitedText := UpperCase(params);

      for i := 0 to sitesList.Count - 1 do
      begin
        site := FindSiteByName(Netname, sitesList[i]);
        _ShowCredits(Netname, channel, site);
      end;

    finally
      sitesList.Free;
    end;
  end;

  Result := True;
end;

function IrcAddSiteInfos(const netname, channel, params: String): boolean;
var
  s: TSite;
  Text, sitename: String;
begin
  sitename := SubString(params, ' ', 1);
  Text := mystrings.RightStr(params, length(sitename) + 2);

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
    Result := False;
    exit;
  end;

  if Text = '' then
  begin
    irc_addtext(Netname, Channel, '<b>Info%ss for Site</b> %s:', [Chr(39), s.Name]);
    irc_addtext(Netname, Channel, '<b>Info%ss for Site</b> %s:', [Chr(39), s.SiteInfos]);
  end
  else
  begin
    s.SiteInfos := Text;
    irc_addtext(Netname, Channel, '<b>Info%ss for Site</b> %s:', [Chr(39), s.Name]);
    irc_addtext(Netname, Channel, '<b>Info%ss for Site</b> %s:', [Chr(39), s.SiteInfos]);
  end;

  Result := True;
end;

function IrcSlotsShow(const netname, channel, params: String): boolean;
var
  sitename: String;
  s: TSite;
  ss: TSiteSlot;
  i: integer;
begin
  Result := False;

  sitename := UpperCase(SubString(params, ' ', 1));

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
    exit;
  end;

  irc_addtext(Netname, Channel, Format('Slots for %s : Total: %d Free: %d Dn/MaxDn: %d/%d Up/MaxUp: %d/%d',
    [sitename, s.slots.Count, s.freeslots, s.num_dn, s.max_dn, s.num_up, s.max_up]));

  for i := 0 to s.slots.Count - 1 do
  begin
    try
      ss := TSiteSlot(s.slots[i]);
      if ((s.slots[i] = nil) or (ss = nil)) then
      begin
        irc_addtext(Netname, Channel, Format('%s/%d : ERROR while getting infos', [sitename, i]));
      end
      else
      begin
        if ss.todotask = nil then
        begin
          irc_addtext(Netname, Channel, Format('%s : no assigned task', [ss.Name]));
        end
        else
        begin
          irc_addtext(Netname, Channel, Format('%s : %s', [ss.Name, ss.todotask.Name]));
        end;

        irc_addtext(Netname, Channel, Format('%s : Last execution times - Task: %s, Non-Idle Task: %s, I/O: %s',
            [ss.Name, TimeToStr(ss.LastTaskExecution), TimeToStr(ss.LastNonIdleTaskExecution), TimeToStr(ss.LastIO)]));
      end;
    except
      on E: Exception do
      begin
        irc_addtext(Netname, Channel, Format('%s/%d : FATAL ERROR', [sitename, i]));
        Continue;
      end;
    end;
  end;

  Result := True;
end;

function IrcBnc(const netname, channel, params: String): boolean;
var
  i: integer;
  s: TSite;
  host, sitename: String;
begin
  Result := False;
  sitename := UpperCase(params);

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Error:</b> </c>Site <b>%s</b> not found.', [sitename]);
    exit;
  end;

  irc_addtext(Netname, Channel, 'Site <b>%s</b>:', [sitename]);

  i := 0;
  while (not slshutdown) do
  begin
    host := s.RCString('bnc_host-' + IntToStr(i), '');
    if host = '' then
      break;

    irc_addtext(Netname, Channel, ' bnc: %s:%d', [host, s.RCInteger('bnc_port-' + IntToStr(i), 0)]);
    Inc(i);
  end;

  Result := True;
end;

function IrcBnctest(const netname, channel, params: String): boolean;
var
  s: TSite;
  x: TStringList;
  tn: TTaskNotify;
  added: boolean;
  i: integer;
  db: integer;
begin
  Result := False;
  added := False;
  s := nil;
  db := 0;
  x := TStringList.Create;
  try
    x.Delimiter := ' ';
    x.DelimitedText := UpperCase(params);

    if x.Count > 0 then
    begin
      db := x.Count;

      for i := 0 to x.Count - 1 do
      begin
        s := FindSiteByName(Netname, x[i]);
        if s = nil then
        begin
          irc_addtext(Netname, Channel, 'Site %s not found', [x[i]]);
          exit;
        end;
        if (s.Name = getAdminSiteName) then
        begin
          Continue;
        end;
      end;

      tn := AddNotify;
      for i := 0 to x.Count - 1 do
      begin
        s := FindSiteByName(Netname, x[i]);
        if s.PermDown then
          Continue;
        if _Bnctest(Netname, Channel, s, tn) then
          added := True;
      end;
    end
    else
    begin
      tn := AddNotify;
      for i := 0 to sites.Count - 1 do
      begin
        s := TSite(sites[i]);
        if (s.Name = getAdminSiteName) then
        begin
          Continue;
        end;
        Inc(db);

        if s.PermDown then
          Continue;

        if _Bnctest(Netname, Channel, s, tn) then
          added := True;
      end;
    end;

  finally
    x.Free;
  end;

  if added then
    QueueFire;

  if added then
    tn.event.WaitFor($FFFFFFFF);

  if (db > 1) then
    IrcSites(Netname, Channel, 'IrcBnctest');

  s.RemoveAutoIndex;
  s.RemoveAutoBnctest;
  s.RemoveAutoNuke;
  s.RemoveAutoDirlist;
  s.RemoveAutoRules;

  if s.AutoNukeInterval <> 0 then
    s.AutoNuke;
  if s.AutoIndexInterval <> 0 then
    s.AutoIndex;
  if s.AutoRulesStatus <> 0 then
    s.AutoRules;
  // if s.RCString('autologin','-1') <> '-1' then
  if s.AutoBncTestInterval <> 0 then
    s.AutoBnctest;

  RemoveTN(tn);

  Result := True;
end;

function IrcKill(const netname, channel, params: String): boolean;
var
  sitename: String;
  s: TSite;
begin
  Result := False;

  sitename := UpperCase(params);
  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
    exit;
  end;

  if (s.Name = getAdminSiteName) then
  begin
    exit;
  end;

  if _Bnctest(Netname, Channel, s, nil, True) then
    QueueFire;

  Result := True;
end;

function IrcRebuildSlot(const netname, channel, params: String): boolean;
var
  sitename: String;
  s_slot: String;
  slot: integer;
  site: TSite;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  s_slot := SubString(params, ' ', 2);
  slot := StrToIntDef(s_slot, -1);

  site := FindSiteByName(Netname, sitename);
  if site = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
    exit;
  end;

  if slot < 0 then
  begin
    irc_addtext(Netname, Channel, 'Slot %s/<b>%s</b> not found.', [sitename, s_slot]);
    exit;
  end;

  try
    if ((site.slots[slot] = nil) or (TSiteSlot(site.slots[slot]) = nil)) then
    begin
      irc_addtext(Netname, Channel, 'Slot %s/<b>%s</b> not found.', [sitename, s_slot]);
      exit;
    end;
    site.slots[slot] := nil;
    site.slots[slot] := TSiteSlot.Create(site, slot);
  except
    on E: Exception do
    begin
      irc_addtext(Netname, Channel, 'Exception : %s', [E.Message]);
    end;
  end;

  Result := True;
end;

function IrcRecalcFreeslots(const netname, channel, params: String): boolean;
var
  sitename: String;
  site: TSite;
  i: integer;
  x: TStringList;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));

  if sitename = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      if (TSite(sites.Items[i]).Name = getAdminSiteName) then
        Continue;
      TSite(sites.Items[i]).RecalcFreeslots;
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
        site.RecalcFreeslots;
      end;
    finally
      x.Free;
    end;
  end;

  Result := True;
end;

function IrcSetDownOnOutOfSpace(const netname, channel, params: String): boolean;
var
  sitename: String;
  s: TSite;
  i: integer;
  x: TStringList;
  fSetDown: integer;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  fSetDown := StrToIntDef(SubString(params, ' ', 2), -1);

  if sitename = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      s := TSite(sites.Items[i]);
      if (s.Name = getAdminSiteName) then
        Continue;
      if s.PermDown then
        Continue;

      // only allow 0 and 1 as valid values
      if ((fSetDown < 0) or (fSetDown > 1)) then
        irc_addtext(Netname, Channel, 'Site <b>%s</b> value for set down out of space is: %d', [s.Name, s.SetDownOnOutOfSpace])
      else
        s.SetDownOnOutOfSpace := boolean(fSetDown);
    end;
  end
  else
  begin
    x := TStringList.Create;
    try
      x.CommaText := sitename;
      for i := 0 to x.Count - 1 do
      begin
        s := FindSiteByName(Netname, x.Strings[i]);
        if s = nil then
        begin
          irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [x.Strings[i]]);
          Continue;
        end;
        if s.PermDown then
          Continue;

        // only allow 0 and 1 as valid values
        if ((fSetDown < 0) or (fSetDown > 1)) then
          irc_addtext(Netname, Channel, 'Site <b>%s</b> value for set down out of space is: %d', [s.Name, s.SetDownOnOutOfSpace])
        else
          s.SetDownOnOutOfSpace := boolean(fSetDown);
      end;
    finally
      x.Free;
    end;
  end;

  Result := True;
end;

function IrcUseSiteSearchOnReqfill(const netname, channel, params: String): boolean;
var
  fSiteName: String;
  fUseSiteSearch: Integer;
  fSite: TSite;
begin
  Result := False;
  fSiteName := UpperCase(SubString(params, ' ', 1));
  fUseSiteSearch := StrToIntDef(SubString(params, ' ', 2), -1);
  fSite := FindSiteByName(Netname, fSiteName);
  if fSite = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [fSiteName]);
    exit;
  end;

  // only allow 0 and 1 as valid values
  if ((fUseSiteSearch < 0) or (fUseSiteSearch > 1)) then
    irc_addtext(Netname, Channel, 'Site <b>%s</b> value for use site search on req fill is: %d', [fSite.Name, ord(fSite.UseSiteSearchOnReqFill)])
  else
    fSite.UseSiteSearchOnReqFill := boolean(fUseSiteSearch);

  Result := True;
end;

function IrcReducedSpeedstatWeight(const netname, channel, params: String): boolean;
var
  fSiteName: String;
  fReducedSpeedstatWeight: Integer;
  fSite: TSite;
begin
  Result := False;
  fSiteName := UpperCase(SubString(params, ' ', 1));
  fReducedSpeedstatWeight := StrToIntDef(SubString(params, ' ', 2), -1);
  fSite := FindSiteByName(Netname, fSiteName);
  if fSite = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [fSiteName]);
    exit;
  end;

  // only allow 0 and 1 as valid values
  if ((fReducedSpeedstatWeight < 0) or (fReducedSpeedstatWeight > 1)) then
    irc_addtext(Netname, Channel, 'Site <b>%s</b> value for reduced speedstat weight is: %d', [fSite.Name, ord(fSite.ReducedSpeedstatWeight)])
  else
    fSite.ReducedSpeedstatWeight := boolean(fReducedSpeedstatWeight);

  Result := True;
end;

end.
