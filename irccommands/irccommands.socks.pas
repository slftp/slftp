unit irccommands.socks;

interface

{ slftp socks commands functions }
function IrcAddSocks5(const netname, channel, params: String): boolean;
function IrcDelSocks5(const netname, channel, params: String): boolean;
function IrcDisplaySocks5(const netname, channel, params: String): boolean;
function IrcTweakSocks5(const netname, channel, params: String): boolean;
function IrcSetSocks5(const netname, channel, params: String): boolean;

implementation

uses
  SysUtils, Classes, irc, sitesunit, mslproxys, regexpr, configunit, mystrings;

const
  section = 'irccommands.socks';

function IrcAddSocks5(const netname, channel, params: String): boolean;
var
  fhostport, fhost, fuser, fpass, fname: String;
  fport, fstatus: integer;
begin
  Result := False;
  fname := UpperCase(SubString(params, ' ', 1));

  if FindProxyByName(fname) <> nil then
  begin
    irc_addtext(Netname, Channel, 'Proxy with name %s already exists!', [fname]);
    Result := True;
    exit;
  end;

  fhostport := SubString(params, ' ', 2);
  fhost := SubString(fhostport, ':', 1);
  fport := StrToIntDef(SubString(fhostport, ':', 2), 0);
  fuser := SubString(params, ' ', 3);
  fstatus := StrToIntDef(fuser, -1);

  if fstatus = -1 then
  begin
    fpass := SubString(params, ' ', 4);
    fstatus := StrToIntDef(SubString(params, ' ', 5), 0);
  end
  else
  begin
    fuser := '';
    fpass := '';
    fstatus := StrToIntDef(SubString(params, ' ', 3), 0);
  end;

  if not AddNewProxy(fname, fhost, fuser, fpass, fport, boolean(fstatus)) then
  begin
    Irc_AddText(Netname, Channel, '<c4><b>ERROR</c></b>: Adding a new Proxy failed.');
  end;

  Result := True;
end;

function IrcDelSocks5(const netname, channel, params: String): boolean;
var
  trigger, Value: String;
  rx: TRegExpr;
begin
  Result := False;

  rx := TRegExpr.Create;
  try
    rx.ModifierI := True;
    rx.Expression := '[\-]{1,2}(name|index) ([^\s]+)';
    if rx.Exec(params) then
    begin
      trigger := AnsiUpperCase(rx.Match[1]);
      Value := rx.Match[2];
    end
    else
    begin
      irc_addtext(Netname, Channel, 'dOH! something wrong!');
      exit;
    end;
  finally
    rx.Free;
  end;

  if ((trigger <> 'NAME') and (trigger <> 'INDEX')) then
  begin
    irc_addtext(Netname, Channel, 'Use delsocks5 --NAME <socks5 name> OR --INDEX <# in listsocks5>');
    exit;
  end;

  if trigger = 'NAME' then
    Result := RemoveProxy(Value);
  if trigger = 'INDEX' then
    Result := RemoveProxy(StrToInt(Value));

  Result := True;
end;

function IrcDisplaySocks5(const netname, channel, params: String): boolean;
var
  i, fProxyCount: integer;
begin
  fProxyCount := GetTotalProxyCount;
  irc_addtext(Netname, Channel, 'Listing all %d Proxys:', [fProxyCount]);
  for i := 0 to fProxyCount - 1 do
  begin
    irc_addtext(Netname, Channel, GetFormattedProxyInfo(i));
  end;
  Result := True;
end;

function IrcTweakSocks5(const netname, channel, params: String): boolean;
var
  fname, ftrigger, fvalue: String;
  s5: TmSLSocks5;
begin
  Result := False;

  fname := SubString(params, ' ', 1);
  ftrigger := AnsiLowerCase(SubString(params, ' ', 2));
  fvalue := SubString(params, ' ', 3);

  s5 := FindProxyByName(fname);
  if s5 = nil then
  begin
    irc_addtext(Netname, Channel, '<c4><b>ERROR</c></b>: Cant find Proxy with name %s!', [fname]);
    exit;
  end;

  try
    if ftrigger = 'host' then
    begin
      s5.Host := fvalue;
    end
    else if ftrigger = 'port' then
    begin
      s5.Port := StrToInt(fvalue);
    end
    else if ftrigger = 'user' then
    begin
      s5.Username := fvalue;
    end
    else if ftrigger = 'password' then
    begin
      s5.Password := fvalue;
    end
    else if ftrigger = 'status' then
    begin
      s5.Enabled := StrToBool(fvalue);
    end
    else
    begin
      irc_addtext(Netname, Channel, '<c4><b>ERROR</c></b>: Unknown trigger: %s!', [ftrigger]);
      exit;
    end;
  except
    on e: Exception do
    begin
      Irc_AddText(Netname, Channel, '<c4><b>ERROR</c></b>: IrcTweakSocks5 saving value %s', [e.Message]);
      exit;
    end;
  end;

  Result := True;
end;

function IrcSetSocks5(const netname, channel, params: String): boolean;
var
  vname, vvalue, vtrigger: String;
  virc: TMyIrcThread;
  vsite: TSite;
  vsocks: TmSLSocks5;
begin
  Result := False;
  vsocks := nil;

  vtrigger := UpperCase(SubString(params, ' ', 1));
  if vtrigger = 'HTTP' then
  begin
    vvalue := UpperCase(SubString(params, ' ', 2));
  end
  else
  begin
    vname := UpperCase(SubString(params, ' ', 2));
    vvalue := UpperCase(SubString(params, ' ', 3));

    if (vname = '') or (vvalue = '') then
    begin
      irc_addtext(Netname, Channel, '<c4><b>ERROR</c></b>: Wrong input parameters.');
      exit;
    end;
  end;

  if vtrigger = 'SITE' then
  begin
    vsite := FindSiteByName('', vname);
    if vsite = nil then
    begin
      irc_addtext(Netname, Channel, '<c4><b>ERROR</c></b>: Cant find Site with name %s!', [vname]);
      exit;
    end;

    if vvalue = '-1' then
    begin
      vsite.ProxyName := '!!NOIN!!'; //means Proxy usage removed
      Result := True;
      exit;
    end
    else
    begin
      vsocks := FindProxyByName(vvalue);
      if vsocks = nil then
      begin
        irc_addtext(Netname, Channel, '<c4><b>ERROR</c></b>: Cant find Proxy with name %s!', [vvalue]);
        exit;
      end;
      vsite.ProxyName := vvalue;
    end;
  end
  else if vtrigger = 'IRC' then
  begin
    virc := FindIrcnetwork(vname);
    if virc = nil then
    begin
      irc_addtext(Netname, Channel, '<c4><b>ERROR</c></b>: Cant find IRCNetwork with name %s!', [vname]);
      exit;
    end;

    if vvalue = '-1' then
    begin
      virc.ProxyName := '!!NOIN!!'; //means Proxy usage removed
      Result := True;
      exit;
    end
    else
    begin
      vsocks := FindProxyByName(vvalue);
      if vsocks = nil then
      begin
        irc_addtext(Netname, Channel, '<c4><b>ERROR</c></b>: Cant find Proxy with name %s!', [vvalue]);
        exit;
      end;
      virc.ProxyName := vvalue;
    end;
  end
  else if vtrigger = 'HTTP' then
  begin
    if vvalue = '-1' then
    begin
      config.WriteInteger('http', 'enabled', 0);
      config.WriteString('http', 'proxyname', '');
    end
    else
    begin
      vsocks := FindProxyByName(vvalue);
      if vsocks = nil then
      begin
        irc_addtext(Netname, Channel, '<c4><b>ERROR</c></b>: Cant find Proxy with name %s!', [vvalue]);
        exit;
      end;

      config.WriteInteger('http', 'enabled', 1);
      config.WriteString('http', 'proxyname', vvalue);
    end;

    config.UpdateFile;
  end;

  Result := True;
end;

end.