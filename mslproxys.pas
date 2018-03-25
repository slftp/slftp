{*******************************************************************************
 *  THIS IS A PART OF SLFTP BLABLABLA NEED COOL MESSAGE ;)                     *
 *                                                                             *
 *                                                                             *
 *                                                                             *
 *                                                                             *
 **************************(mod.done.by.kraut)**********************************}
unit mslproxys;

interface

uses
  Classes, Contnrs, SyncObjs
  {$IFDEF FPC}
    {$IFNDEF MSWINDOWS}
      , baseunix
    {$ENDIF}
  {$ENDIF};

procedure InitProxys;
procedure UninitProxys;
function StartProxys: boolean;
function RehashProxys: boolean;

type
  TmSLSocks5 = class
    Name: AnsiString;
  private
    // ini read/write functions
    procedure WStr(const key, Value: AnsiString);
    function RStr(const key, default: AnsiString): AnsiString;
    procedure WInt(const key: AnsiString; const Value: integer);
    function RInt(const key: AnsiString; const default: integer): integer;
    procedure WBool(const key: AnsiString; Value: boolean);
    function RBool(const key: AnsiString; default: boolean): boolean;
    // property stuff
    function GetHost: AnsiString;
    procedure SetHost(const Value: AnsiString);
    function GetPort: integer;
    procedure SetPort(const Value: integer);
    function GetUsername: AnsiString;
    procedure SetUsername(const Value: AnsiString);
    function GetPassword: AnsiString;
    procedure SetPassword(const Value: AnsiString);
    function GetStatus: boolean;
    procedure SetStatus(Value: boolean);
  public
    constructor Create(const bncname: AnsiString);

    property Host: AnsiString Read GetHost Write SetHost;
    property Port: integer Read GetPort Write SetPort;
    property Username: AnsiString Read GetUsername Write SetUsername;
    property Password: AnsiString Read GetPassword Write SetPassword;
    property Enabled: boolean Read GetStatus Write SetStatus; //! Usage status of proxy, for enabling/disabling proxy
  end;

{ Adds a new proxy to list and writes settings to socksini
  @param(aName proxyname)
  @param(aHost socks hostadress)
  @param(aUsername username to login)
  @param(aPassword password for login)
  @param(aPort socks port)
  @param(aEnabled status of socks, if it should be used or not)
  @returns(@true on success, @false otherwise) }
function AddNewProxy(const aName, aHost, aUsername, aPassword: AnsiString; const aPort: Integer; aEnabled: boolean): boolean;

{ Search proxy with given Name and return it's object
  @param(Name Name of proxy)
  @returns(TmSLSocks5 if found, nil otherwise) }
function FindProxyByName(const Name: AnsiString): TmSLSocks5;

{ Remove proxy by ID
  @param(index list entry of proxy) }
function RemoveProxy(const index: integer): boolean; overload;

{ Remove proxy by Name
  @param(Name name of proxy) }
function RemoveProxy(const Name: AnsiString): boolean; overload;

{ Get the total amount of added proxys
  @returns(Number of added Proxys) }
function GetTotalProxyCount: Integer;

{ Get the total amount of added proxys
  @param(aIndex list entry of proxy)
  @returns(Formatted string with some proxy infos) }
function GetFormattedProxyInfo(const aIndex: integer): String;

implementation

uses
  debugunit, configunit, SysUtils, StrUtils, encinifile;

var
  proxys: TObjectList = nil;
  socksini: TEncinifile;

{###   Init Jobs   ###}

procedure InitProxys;
begin
  proxys   := TObjectList.Create;
  socksini := TEncinifile.Create(ExtractFilePath(ParamStr(0)) + 'slftp.socks5', passphrase);
end;

procedure UninitProxys;
begin
  Debug(dpSpam, 'Proxys', 'Uninit1');
  if proxys <> nil then
  begin
    proxys.Clear;
    FreeAndNil(proxys);
  end;
  Debug(dpSpam, 'Proxys', 'Uninit2');
end;

function StartProxys: boolean;
var
  I:  integer;
  xs: TStringList;
begin
  Result := False;
  Debug(dpSpam, 'Proxys', 'Loadlist');
  xs := TStringList.Create;
  try
    socksini.ReadSections(xs);
    Debug(dpSpam, 'Proxys', IntToStr(xs.Count) + ' proxys in list...');
    for I := 0 to xs.Count - 1 do
      proxys.Add(TmSLSocks5.Create(xs.Strings[i]));
  finally
    xs.Free;
  end;
  Debug(dpSpam, 'Proxys', 'Proxys added: ' + IntToStr(proxys.Count));
  Result := True;
end;

function RehashProxys: boolean;
begin
  proxys.Clear;
  socksini.Free;
  socksini := TEncinifile.Create(ExtractFilePath(ParamStr(0)) +'slftp.socks5', passphrase);
  Result := StartProxys;
end;

{###   Socks5 Class   ###}

constructor TmSLSocks5.Create(const bncname: AnsiString);
begin
  self.Name := bncname;
end;

procedure TmSLSocks5.WStr(const key: AnsiString; const Value: AnsiString);
begin
  socksini.WriteString(Name, key, Value);
  socksini.UpdateFile;
end;

procedure TmSLSocks5.WInt(const key: AnsiString; const Value: integer);
begin
  socksini.WriteInteger(Name, key, Value);
  socksini.UpdateFile;
end;

procedure TmSLSocks5.WBool(const key: AnsiString; Value: boolean);
begin
  socksini.WriteBool(Name, key, Value);
  socksini.UpdateFile;
end;

function TmSLSocks5.RStr(const key: AnsiString; const default: AnsiString): AnsiString;
begin
  Result := socksini.ReadString(Name, key, default);
end;

function TmSLSocks5.RInt(const key: AnsiString; const default: integer): integer;
begin
  Result := socksini.ReadInteger(Name, key, default);
end;

function TmSLSocks5.RBool(const key: AnsiString; default: boolean): boolean;
begin
  Result := socksini.ReadBool(Name, key, default);
end;

procedure TmSLSocks5.SetHost(const Value: AnsiString);
begin
  WStr('Host', Value);
end;

function TmSLSocks5.GetHost: AnsiString;
begin
  Result := RStr('Host', '');
end;

procedure TmSLSocks5.SetPort(const Value: integer);
begin
  WInt('Port', Value);
end;

function TmSLSocks5.GetPort: integer;
begin
  Result := RInt('Port', -1);
end;

procedure TmSLSocks5.SetUsername(const Value: AnsiString);
begin
  WStr('Username', Value);
end;

function TmSLSocks5.GetUsername: AnsiString;
begin
  Result := RStr('Username', '');
end;

procedure TmSLSocks5.SetPassword(const Value: AnsiString);
begin
  WStr('Password', Value);
end;

function TmSLSocks5.GetPassword: AnsiString;
begin
  Result := RStr('Password', '');
end;

procedure TmSLSocks5.SetStatus(Value: boolean);
begin
  WBool('Enabled', Value);
end;

function TmSLSocks5.GetStatus: boolean;
begin
  Result := RBool('Enabled', False);
end;

{###   Controling Jobs   ###}

function AddNewProxy(const aName, aHost, aUsername, aPassword: AnsiString; const aPort: Integer; aEnabled: boolean): boolean;
var
  fProxyObj: TmSLSocks5;
begin
  Result := False;

  // create new proxy
  try
    proxys.Add(TmSLSocks5.Create(aName));
  except
    on E: Exception do
    begin
      Debug(dpError, 'Proxys', Format('Exception in AddNewProxy: %s', [e.Message]));
      exit;
    end;
  end;

  fProxyObj := FindProxyByName(aName);
  // set config values for proxy
  fProxyObj.Host := aHost;
  fProxyObj.Port := aPort;
  fProxyObj.Username := aUsername;
  fProxyObj.Password := aPassword;
  fProxyObj.Enabled := aEnabled;

  Result := True;
end;

function FindProxyByName(const Name: AnsiString): TmSLSocks5;
var
  I: integer;
begin
  Result := nil;
  for I := 0 to proxys.Count - 1 do
  begin
    if TmSLSocks5(proxys.Items[i]).Name = Name then
    begin
      Result := TmSLSocks5(proxys.Items[i]);
      break;
    end;
  end;
end;

function RemoveProxy(const Name: AnsiString): boolean;
begin
  if proxys.Remove(FindProxyByName(Name)) > -1 then
  begin
    socksini.EraseSection(Name);
    socksini.UpdateFile;
    Result := True;
  end
  else
    Result := False;
end;

function RemoveProxy(const index: integer): boolean;
var
  Name: AnsiString;
begin
  Name := TmSLSocks5(proxys.Items[index]).Name;
  if proxys.Remove(proxys.Items[index]) > -1 then
  begin
    socksini.EraseSection(Name);
    socksini.UpdateFile;
    Result := True;
  end
  else
    Result := False;
end;

function GetTotalProxyCount: Integer;
begin
  Result := proxys.Count;
end;

function GetFormattedProxyInfo(const aIndex: integer): String;
var
  fColorEnabled: String;
begin
  fColorEnabled := IfThen(TmSLSocks5(proxys.Items[aIndex]).Enabled, '<c3>enabled</c>', '<c4>disabled</c>');

  Result := Format('%d) %s %s:%d %s (%s)',
    [aIndex, TmSLSocks5(proxys.Items[aIndex]).Name, TmSLSocks5(proxys.Items[aIndex]).host, TmSLSocks5(proxys.Items[aIndex]).port, TmSLSocks5(proxys.Items[aIndex]).username, fColorEnabled]);
end;

end.

