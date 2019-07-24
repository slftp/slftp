unit irccommands.slots;

interface

{ slftp slots commands functions }
function IrcDelayLeech(const netname, channel, params: String): boolean;
function IrcDelayUpload(const netname, channel, params: String): boolean;

implementation

uses
  SysUtils, Classes, Contnrs, irc, mystrings, sitesunit, IdGlobal;

const
  section = 'irccommands.slots';

procedure _DisplayDelay(const aNetname, aChannel: String; const aSite: TSite; const aTipus, aSection: String);
var
  minv, maxv: integer;
begin
  if (aTipus = 'leech') then
  begin
    minv := aSite.GetDelayLeechMin(aSection);
    maxv := aSite.GetDelayLeechMax(aSection);
  end
  else if (aTipus = 'upload') then
  begin
    minv := aSite.GetDelayUploadMin(aSection);
    maxv := aSite.GetDelayUploadMax(aSection);
  end
  else
  begin
    irc_addtext(aNetname, aChannel, 'Cannot get delay%s values for section <b>%s</b>', [aTipus, aSection]);
    exit;
  end;

  if (maxv = 0) then
    irc_addtext(aNetname, aChannel, 'Delaying %s feature is disabled for site %s, section: %s', [aTipus, aSite.Name, aSection])
  else
    irc_addtext(aNetname, aChannel, 'Delaying %s on site %s for section %s: min delay %d, max delay: %d', [aTipus, aSite.Name, aSection, minv, maxv]);
end;

procedure _DisplayAllDelay(const aNetname, aChannel: String; const aSite: TSite; const aTipus: String);
var
  x: TStringList;
  minv, maxv: integer;
  i: integer;
  s, fSection: String;
begin
  x := TStringList.Create;
  try
    sitesdat.ReadSection('site-' + aSite.Name, x);
    for i := 0 to x.Count - 1 do
    begin
      if ((1 = Pos('delay' + aTipus, x[i])) and (0 <> Pos('-min', x[i]))) then
      begin
        s := x[i];
        Fetch(s, '-', True, False);
        fSection := Fetch(s, '-', True, False);

        if (aTipus = 'leech') then
        begin
          minv := aSite.GetDelayLeechMin(fSection);
          maxv := aSite.GetDelayLeechMax(fSection);
        end
        else if (aTipus = 'upload') then
        begin
          minv := aSite.GetDelayUploadMin(fSection);
          maxv := aSite.GetDelayUploadMax(fSection);
        end;

        if (maxv = 0) then
          irc_addtext(aNetname, aChannel, 'Delaying %s feature is disabled for site %s, section: %s', [aTipus, aSite.Name, fSection])
        else
          irc_addtext(aNetname, aChannel, 'Delaying %s on site %s for section %s: min delay %d, max delay: %d', [aTipus, aSite.Name, fSection, minv, maxv]);
      end;
    end;
  finally
    x.Free;
  end;
end;

procedure _DeleteDelay(const aNetname, aChannel: String; const aSite: TSite; const aTipus, aSection: String);
begin
  if (aTipus = 'leech') then
  begin
    aSite.SetDelayLeechMin(aSection, 0);
    aSite.SetDelayLeechMax(aSection, 0);
  end
  else if (aTipus = 'upload') then
  begin
    aSite.SetDelayUploadMin(aSection, 0);
    aSite.SetDelayUploadMax(aSection, 0);
  end
  else
  begin
    irc_addtext(aNetname, aChannel, 'Cannot delete delay%s for section <b>%s</b>', [aTipus, aSection]);
    exit;
  end;

  irc_addtext(aNetname, aChannel, '%s %s delay is deleted on site %s', [aSection, aTipus, aSite.Name]);
end;

procedure _SpecifyDelay(const aNetname, aChannel: String; aSite: TSite; const aTipus, aSection: String; minv, maxv: integer);
begin
  if minv < 0 then
  begin
    irc_addtext(aNetname, aChannel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;
  if maxv < minv then
  begin
    irc_addtext(aNetname, aChannel, 'Max is smaller than min.');
    exit;
  end;

  if (aTipus = 'leech') then
  begin
    aSite.SetDelayLeechMin(aSection, minv);
    aSite.SetDelayLeechMax(aSection, maxv);
  end
  else if (aTipus = 'upload') then
  begin
    aSite.SetDelayUploadMin(aSection, minv);
    aSite.SetDelayUploadMax(aSection, maxv);
  end
  else
  begin
    irc_addtext(aNetname, aChannel, 'Cannot set delay%s for section <b>%s</b>', [aTipus, aSection]);
    exit;
  end;
end;

function IrcDelayLeech(const netname, channel, params: String): boolean;
const
  tipus = 'leech';
var
  sitename, section, s: String;
  minv, maxv: integer;
  i: integer;
  site: TSite;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  if sitename <> '*' then
  begin
    site := FindSiteByName(Netname, sitename);
    if site = nil then
    begin
      irc_addtext(Netname, Channel, 'Site %s not found.', [sitename]);
      exit;
    end;
  end;

  section := UpperCase(SubString(params, ' ', 2));
  if section = '-' then
  begin
    // delete value
    if sitename = '*' then
    begin
      for i := 0 to sites.Count - 1 do
      begin
        site := TSite(sites.Items[i]);
        if (site.Name = getAdminSiteName) then
          Continue;
        if site.PermDown then
          Continue;

        _DeleteDelay(Netname, Channel, site, tipus, 'global');
      end;
    end
    else
    begin
      _DeleteDelay(Netname, Channel, site, tipus, 'global');
    end;
  end
  else if section = '' then
  begin
    // display value
    if sitename = '*' then
    begin
      for i := 0 to sites.Count - 1 do
      begin
        site := TSite(sites.Items[i]);
        if (site.Name = getAdminSiteName) then
          Continue;
        if site.PermDown then
          Continue;

        _DisplayAllDelay(Netname, Channel, site, tipus);
      end;
    end
    else
    begin
      _DisplayAllDelay(Netname, Channel, site, tipus);
    end;
  end
  else
  begin
    minv := StrToIntDef(section, -1);
    if minv > 0 then
    begin
      // specify global
      section := 'global';
      maxv := StrToIntDef(SubString(params, ' ', 3), -1);
      if sitename = '*' then
      begin
        for i := 0 to sites.Count - 1 do
        begin
          site := TSite(sites.Items[i]);
          if (site.Name = getAdminSiteName) then
            Continue;
          if site.PermDown then
            Continue;

          _SpecifyDelay(Netname, Channel, site, tipus, section, minv, maxv);
        end;
      end
      else
        _SpecifyDelay(Netname, Channel, site, tipus, section, minv, maxv);
    end
    else
    begin
      // section specified.
      s := SubString(params, ' ', 3);
      if s = '-' then
      begin
        if sitename = '*' then
        begin
          for i := 0 to sites.Count - 1 do
          begin
            site := TSite(sites.Items[i]);
            if (site.Name = getAdminSiteName) then
              Continue;
            if site.PermDown then
              Continue;

            _DeleteDelay(Netname, Channel, site, tipus, section);
          end;
        end
        else
        begin
          _DeleteDelay(Netname, Channel, site, tipus, section);
        end;
      end
      else if s = '' then
      begin
        if sitename = '*' then
        begin
          for i := 0 to sites.Count - 1 do
          begin
            site := TSite(sites.Items[i]);
            if (site.Name = getAdminSiteName) then
              Continue;
            if site.PermDown then
              Continue;

            _DisplayDelay(Netname, Channel, site, tipus, section);
          end;
        end
        else
        begin
          _DisplayDelay(Netname, Channel, site, tipus, section);
        end;
      end
      else
      begin
        // set
        minv := StrToIntDef(s, -1);
        maxv := StrToIntDef(SubString(params, ' ', 4), -1);
        if sitename = '*' then
        begin
          for i := 0 to sites.Count - 1 do
          begin
            site := TSite(sites.Items[i]);
            if (site.Name = getAdminSiteName) then
              Continue;
            if site.PermDown then
              Continue;

            _SpecifyDelay(Netname, Channel, site, tipus, section, minv, maxv);
          end;
        end
        else
        begin
          _SpecifyDelay(Netname, Channel, site, tipus, section, minv, maxv);
        end;
      end;
    end;
  end;

  Result := True;
end;

function IrcDelayUpload(const netname, channel, params: String): boolean;
const
  tipus = 'upload';
var
  sitename, section, s: String;
  minv, maxv: integer;
  i: integer;
  site: TSite;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  if sitename <> '*' then
  begin
    site := FindSiteByName(Netname, sitename);
    if site = nil then
    begin
      irc_addtext(Netname, Channel, 'Site %s not found.', [sitename]);
      exit;
    end;
  end;

  section := UpperCase(SubString(params, ' ', 2));
  if section = '-' then
  begin
    if sitename = '*' then
    begin
      for i := 0 to sites.Count - 1 do
      begin
        site := TSite(sites.Items[i]);
        if (site.Name = getAdminSiteName) then
          Continue;
        if site.PermDown then
          Continue;

        _DeleteDelay(Netname, Channel, site, tipus, 'global');
      end;
    end
    else
    begin
      _DeleteDelay(Netname, Channel, site, tipus, 'global');
    end;
  end
  else if section = '' then
  begin
    if sitename = '*' then
    begin
      for i := 0 to sites.Count - 1 do
      begin
        site := TSite(sites.Items[i]);
        if (site.Name = getAdminSiteName) then
          Continue;
        if site.PermDown then
          Continue;

        _DisplayAllDelay(Netname, Channel, site, tipus);
      end;
    end
    else
    begin
      _DisplayAllDelay(Netname, Channel, site, tipus);
    end;
  end
  else
  begin
    minv := StrToIntDef(section, -1);
    if minv > 0 then
    begin
      // specify global
      section := 'global';
      maxv := StrToIntDef(SubString(params, ' ', 3), -1);
      if sitename = '*' then
      begin
        for i := 0 to sites.Count - 1 do
        begin
          site := TSite(sites.Items[i]);
          if (site.Name = getAdminSiteName) then
            Continue;
          if site.PermDown then
            Continue;

          _SpecifyDelay(Netname, Channel, site, tipus, section, minv, maxv);
        end;
      end
      else
      begin
        _SpecifyDelay(Netname, Channel, site, tipus, section, minv, maxv);
      end;
    end
    else
    begin
      // section specified.
      s := SubString(params, ' ', 3);
      if s = '-' then
      begin
        if sitename = '*' then
        begin
          for i := 0 to sites.Count - 1 do
          begin
            site := TSite(sites.Items[i]);
            if (site.Name = getAdminSiteName) then
              Continue;
            if site.PermDown then
              Continue;

            _DeleteDelay(Netname, Channel, site, tipus, section);
          end;
        end
        else
        begin
          _DeleteDelay(Netname, Channel, site, tipus, section);
        end;
      end
      else if s = '' then
      begin
        if sitename = '*' then
        begin
          for i := 0 to sites.Count - 1 do
          begin
            site := TSite(sites.Items[i]);
            if (site.Name = getAdminSiteName) then
              Continue;
            if site.PermDown then
              Continue;

            _DisplayDelay(Netname, Channel, site, tipus, section);
          end;
        end
        else
        begin
          _DisplayDelay(Netname, Channel, site, tipus, section);
        end;
      end
      else
      begin
        // set
        minv := StrToIntDef(s, -1);
        maxv := StrToIntDef(SubString(params, ' ', 4), -1);

        if sitename = '*' then
        begin
          for i := 0 to sites.Count - 1 do
          begin
            site := TSite(sites.Items[i]);
            if (site.Name = getAdminSiteName) then
              Continue;
            if site.PermDown then
              Continue;

            _SpecifyDelay(Netname, Channel, site, tipus, section, minv, maxv);
          end;
        end
        else
        begin
          _SpecifyDelay(Netname, Channel, site, tipus, section, minv, maxv);
        end;
      end;
    end;
  end;

  Result := True;
end;

end.