unit socks5;

interface

uses
  sltcp, mslproxys;

procedure Socks5Init;
procedure SetupSocks5(c: TslTCPSocket; usesocks5: Boolean);

procedure mSLSetupSocks5(const proxyname: String; c: TslTCPSocket; usesocks5: Boolean);

implementation

uses configunit;

const
  section = 'socks5';

procedure Socks5Init;
begin
  slDefaultSocks5.Enabled := config.ReadBool(section, 'enabled', False);
  slDefaultSocks5.Host := config.ReadString(section, 'host', '127.0.0.1');
  slDefaultSocks5.Port := config.ReadInteger(section, 'port', 11101);
  slDefaultSocks5.Username := config.ReadString(section, 'username', 'user');
  slDefaultSocks5.Password := config.ReadString(section, 'password', 'pw');
end;

procedure SetupSocks5(c: TslTCPSocket; usesocks5: Boolean);
begin
  if not usesocks5 then
  begin
    c.socks5.enabled := False;
    c.socks5.Host := '';
    c.socks5.Port := 0;
    c.socks5.Username := '';
    c.socks5.Password := '';
  end
  else
    c.socks5.enabled := True;
end;

procedure mSLSetupSocks5(const proxyname: String; c: TslTCPSocket; usesocks5: Boolean);
var
  so5: TmSLSocks5;
begin
  so5 := FindProxyByName(proxyname);
  if so5 = nil then
  begin
    c.socks5.enabled := False;
  end
  else
  begin
    c.socks5.enabled := so5.Enabled;
    c.socks5.Host := so5.host;
    c.socks5.Port := so5.port;
    c.socks5.Username := so5.username;
    c.socks5.Password := so5.password;
  end;
end;

end.
