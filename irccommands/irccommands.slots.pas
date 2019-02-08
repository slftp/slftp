unit irccommands.slots;

interface

{ slftp slots commands functions }
function IrcDelayLeech(const netname, channel, params: String): boolean;
function IrcDelayUpload(const netname, channel, params: String): boolean;

implementation

uses
  SysUtils, Classes, irc, mystrings, sitesunit, IdGlobal;

const
  section = 'irccommands.slots';

procedure _DisplayDelay(Netname, Channel, s1, s2, s3: String);
var
  minv, maxv: integer;
begin
  minv := sitesdat.ReadInteger('site-' + s1, 'delay' + s2 + '-' + s3 + '-min',
    0);
  maxv := sitesdat.ReadInteger('site-' + s1, 'delay' + s2 + '-' + s3 + '-max',
    0);
  if (minv = 0) or (maxv = 0) then
    irc_addtext(Netname, Channel, 'Delaying ' + s2 + ' feature is disabled for site %s, section: %s', [s1, s3])
  else
    irc_addtext(Netname, Channel, 'Delay ' + s2 + ' on site %s, section %s: min delay %d, max delay: %d', [s1, s3, minv, maxv]);

end;

procedure _DisplayAllDelay(Netname, Channel, s1, s2: String);
var
  x: TStringList;
  minv, maxv: integer;
  i: integer;
  s, s3: String;
begin
  x := TStringList.Create;
  try
    sitesdat.ReadSection('site-' + s1, x);
    for i := 0 to x.Count - 1 do
    begin
      if ((1 = Pos('delay' + s2, x[i])) and (0 <> Pos('-min', x[i]))) then
      begin
        s := x[i];
        Fetch(s, '-', True, False);
        s3 := Fetch(s, '-', True, False);
        minv := sitesdat.ReadInteger('site-' + s1, 'delay' + s2 + '-' + s3 + '-min', 0);
        maxv := sitesdat.ReadInteger('site-' + s1, 'delay' + s2 + '-' + s3 + '-max', 0);

        if (minv = 0) or (maxv = 0) then
          irc_addtext(Netname, Channel, 'Delaying ' + s2 + ' feature is disabled for site %s, section: %s', [s1, s3])
        else
          irc_addtext(Netname, Channel, 'Delay ' + s2 + ' on site %s, section %s: min delay %d, max delay: %d', [s1, s3, minv, maxv]);
      end;
    end;
  finally
    x.Free;
  end;
end;

procedure _DeleteDelay(Netname, Channel, s1, s2, s3: String);
begin
  sitesdat.DeleteKey('site-' + s1, 'delay' + s2 + '-' + s3 + '-min');
  sitesdat.DeleteKey('site-' + s1, 'delay' + s2 + '-' + s3 + '-max');
  irc_addtext(Netname, Channel, s3 + ' ' + s2 + ' delay is deleted on site ' + s1 + '.');
end;

procedure _SpecifyDelay(Netname, Channel, s1, s2, s3: String; minv, maxv: integer);
begin
  if minv < 0 then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;
  if maxv < minv then
  begin
    irc_addtext(Netname, Channel, 'Max is smaller than min.');
    exit;
  end;

  sitesdat.WriteInteger('site-' + s1, 'delay' + s2 + '-' + s3 + '-min', minv);
  sitesdat.WriteInteger('site-' + s1, 'delay' + s2 + '-' + s3 + '-max', maxv);
end;

function IrcDelayLeech(const netname, channel, params: String): boolean;
const
  tipus = 'leech';
var
  sitename, section, s: String;
  minv, maxv: integer;
  i: integer;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  if sitename <> '*' then
    if nil = FindSiteByName(Netname, sitename) then
    begin
      irc_addtext(Netname, Channel, 'Site %s not found.', [sitename]);
      exit;
    end;

  section := UpperCase(SubString(params, ' ', 2));
  if section = '-' then
  begin
    if sitename = '*' then
    begin
      for i := 0 to sites.Count - 1 do
      begin
        if (TSite(sites.Items[i]).Name = getAdminSiteName) then
          Continue;
        if TSite(sites.Items[i]).PermDown then
          Continue;
        _DeleteDelay(Netname, Channel, TSite(sites.Items[i]).Name, tipus, 'global');
      end;
    end
    else
      _DeleteDelay(Netname, Channel, sitename, tipus, 'global');
  end
  else if section = '' then
  begin
    if sitename = '*' then
    begin
      for i := 0 to sites.Count - 1 do
      begin
        if (TSite(sites.Items[i]).Name = getAdminSiteName) then
          Continue;
        if TSite(sites.Items[i]).PermDown then
          Continue;
        _DisplayAllDelay(Netname, Channel, TSite(sites.Items[i]).Name, tipus);
      end;
    end
    else
      _DisplayAllDelay(Netname, Channel, sitename, tipus);
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
          if (TSite(sites.Items[i]).Name = getAdminSiteName) then
            Continue;
          if TSite(sites.Items[i]).PermDown then
            Continue;
          _SpecifyDelay(Netname, Channel, TSite(sites.Items[i]).Name, tipus,
            section, minv, maxv);
        end;
      end
      else
        _SpecifyDelay(Netname, Channel, sitename, tipus, section, minv, maxv);
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
            if (TSite(sites.Items[i]).Name = getAdminSiteName) then
              Continue;
            if TSite(sites.Items[i]).PermDown then
              Continue;
            _DeleteDelay(Netname, Channel, TSite(sites.Items[i]).Name,
              tipus, section);
          end;
        end
        else
          _DeleteDelay(Netname, Channel, sitename, tipus, section);
      end
      else if s = '' then
      begin
        if sitename = '*' then
        begin
          for i := 0 to sites.Count - 1 do
          begin
            if (TSite(sites.Items[i]).Name = getAdminSiteName) then
              Continue;
            if TSite(sites.Items[i]).PermDown then
              Continue;
            _DisplayDelay(Netname, Channel, TSite(sites.Items[i]).Name,
              tipus, section);
          end;
        end
        else
          _DisplayDelay(Netname, Channel, sitename, tipus, section);
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
            if (TSite(sites.Items[i]).Name = getAdminSiteName) then
              Continue;
            if TSite(sites.Items[i]).PermDown then
              Continue;
            _SpecifyDelay(Netname, Channel, TSite(sites.Items[i]).Name, tipus,
              section, minv, maxv);
          end;
        end
        else
          _SpecifyDelay(Netname, Channel, sitename, tipus, section, minv, maxv);
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
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  if sitename <> '*' then
    if nil = FindSiteByName(Netname, sitename) then
    begin
      irc_addtext(Netname, Channel, 'Site %s not found.', [sitename]);
      exit;
    end;

  section := UpperCase(SubString(params, ' ', 2));
  if section = '-' then
  begin
    if sitename = '*' then
    begin
      for i := 0 to sites.Count - 1 do
      begin
        if (TSite(sites.Items[i]).Name = getAdminSiteName) then
          Continue;
        if TSite(sites.Items[i]).PermDown then
          Continue;
        _DeleteDelay(Netname, Channel, TSite(sites.Items[i]).Name, tipus,
          'global');
      end;
    end
    else
      _DeleteDelay(Netname, Channel, sitename, tipus, 'global');
  end
  else if section = '' then
  begin

    if sitename = '*' then
    begin
      for i := 0 to sites.Count - 1 do
      begin
        if (TSite(sites.Items[i]).Name = getAdminSiteName) then
          Continue;
        if TSite(sites.Items[i]).PermDown then
          Continue;
        _DisplayAllDelay(Netname, Channel, TSite(sites.Items[i]).Name, tipus);
      end;
    end
    else
      _DisplayAllDelay(Netname, Channel, sitename, tipus);
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
          if (TSite(sites.Items[i]).Name = getAdminSiteName) then
            Continue;
          if TSite(sites.Items[i]).PermDown then
            Continue;
          _SpecifyDelay(Netname, Channel, TSite(sites.Items[i]).Name, tipus,
            section, minv, maxv);
        end;
      end
      else
        _SpecifyDelay(Netname, Channel, sitename, tipus, section, minv, maxv);
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
            if (TSite(sites.Items[i]).Name = getAdminSiteName) then
              Continue;
            if TSite(sites.Items[i]).PermDown then
              Continue;
            _DeleteDelay(Netname, Channel, TSite(sites.Items[i]).Name,
              tipus, section);
          end;
        end
        else
          _DeleteDelay(Netname, Channel, sitename, tipus, section);
      end
      else if s = '' then
      begin
        if sitename = '*' then
        begin
          for i := 0 to sites.Count - 1 do
          begin
            if (TSite(sites.Items[i]).Name = getAdminSiteName) then
              Continue;
            if TSite(sites.Items[i]).PermDown then
              Continue;
            _DisplayDelay(Netname, Channel, TSite(sites.Items[i]).Name,
              tipus, section);
          end;
        end
        else
          _DisplayDelay(Netname, Channel, sitename, tipus, section);
      end
      else
      begin
        // set
        minv := StrToIntDef(s, -1);
        maxv := StrToIntDef(SubString(params, ' ', 4), -1);

        if sitename = '*' then
        begin
          for i := 0 to sites.Count - 1 do
            _SpecifyDelay(Netname, Channel, TSite(sites.Items[i]).Name, tipus,
              section, minv, maxv);
        end
        else
          _SpecifyDelay(Netname, Channel, sitename, tipus, section, minv, maxv);
      end;

    end;

  end;

  Result := True;
end;

end.