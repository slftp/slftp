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
  Classes, Contnrs, SyncObjs, encinifile, slstack, slssl
{$IFDEF FPC}
{$IFNDEF MSWINDOWS}
  , baseunix
{$ENDIF}
{$ENDIF}  ;

const
  slDefaultTimeout = 30000; // default timeout is 30 seconds
  slDefaultBacklog = 30;
  slBufferSize = 16384;
  slLF  = #10;
  slCR  = #13;
  slEOL = slCR + slLF;

procedure InitProxys;
procedure UninitProxys;
function StartProxys: boolean;
function RehashProxys: boolean;

type
  //  TslSSLMethod = (slSSLv23, slTLSv1);

  TmSLSocks5 = class
    Name: AnsiString;
  private
    function GetHost: AnsiString;
    procedure SetHost(Value: AnsiString);
    function GetPort: integer;
    procedure SetPort(Value: integer);
    function GetUsername: AnsiString;
    procedure SetUsername(Value: AnsiString);
    function GetPassword: AnsiString;
    procedure SetPassword(Value: AnsiString);
    function GetStatus: boolean;
    procedure SetStatus(Value: boolean);
  public
    constructor Create(bncname: AnsiString);
    procedure WStr(key, Value: AnsiString);
    function RStr(key, default: AnsiString): AnsiString;
    procedure WInt(key: AnsiString; Value: integer);
    function RInt(key: AnsiString; default: integer): integer;
    procedure WBool(key: AnsiString; Value: boolean);
    function RBool(key: AnsiString; default: boolean): boolean;

    property Host: AnsiString Read GetHost Write SetHost;
    property Port: integer Read GetPort Write SetPort;
    property Username: AnsiString Read GetUsername Write SetUsername;
    property Password: AnsiString Read GetPassword Write SetPassword;
    property Enabled: boolean Read GetStatus Write SetStatus;
  end;


var
  proxys:   TObjectList = nil;
  socksini: TEncinifile;

function FindProxyByName(Name: AnsiString): TmSLSocks5;
function RemoveProxy(index: integer): boolean; overload;
function RemoveProxy(Name: AnsiString): boolean; overload;

implementation

uses debugunit, configunit, SysUtils;

{###    Socks5 Class... ###}

constructor TmSLSocks5.Create(bncname: AnsiString);
begin
  self.Name := bncname;
end;

procedure TmSLSocks5.WStr(key: AnsiString; Value: AnsiString);
begin
  socksini.WriteString(Name, key, Value);
  socksini.UpdateFile;
end;

procedure TmSLSocks5.WInt(key: AnsiString; Value: integer);
begin
  socksini.WriteInteger(Name, key, Value);
  socksini.UpdateFile;
end;

procedure TmSLSocks5.WBool(key: AnsiString; Value: boolean);
begin
  socksini.WriteBool(Name, key, Value);
  socksini.UpdateFile;
end;

function TmSLSocks5.RStr(key: AnsiString; default: AnsiString): AnsiString;
begin
  Result := socksini.ReadString(Name, key, default);
end;

function TmSLSocks5.RInt(key: AnsiString; default: integer): integer;
begin
  Result := socksini.ReadInteger(Name, key, default);
end;


function TmSLSocks5.RBool(key: AnsiString; default: boolean): boolean;
begin
  Result := socksini.ReadBool(Name, key, default);
end;

procedure TmSLSocks5.SetHost(Value: AnsiString);
begin
  WStr('Host', Value);
end;

function TmSLSocks5.GetHost: AnsiString;
begin
  Result := RStr('Host', '');
end;

procedure TmSLSocks5.SetPort(Value: integer);
begin
  WInt('Port', Value);
end;

function TmSLSocks5.GetPort: integer;
begin
  Result := RInt('Port', -1);
end;

procedure TmSLSocks5.SetUsername(Value: AnsiString);
begin
  WStr('Username', Value);
end;

function TmSLSocks5.GetUsername: AnsiString;
begin
  Result := RStr('Username', '');
end;

procedure TmSLSocks5.SetPassword(Value: AnsiString);
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

{###    Controling Jobs... ###}


function FindProxyByName(Name: AnsiString): TmSLSocks5;
var
  I: integer;
begin
  Result := nil;
  for I := 0 to proxys.Count - 1 do
    if TmSLSocks5(proxys.Items[i]).Name = Name then
    begin
      Result := TmSLSocks5(proxys.Items[i]);
      break;
    end;
end;


procedure InitProxys;
begin
  proxys   := TObjectList.Create;
  socksini := TEncinifile.Create(ExtractFilePath(ParamStr(0)) +
    'slftp.socks5', passphrase);
end;

procedure UninitProxys;
begin
  Debug(dpSpam, 'Proxys', 'Uninit1');
  if proxys <> nil then
  begin
    proxys.Free;
    proxys := nil;
  end;
  Debug(dpSpam, 'Proxys', 'Uninit2');
end;

function StartProxys: boolean;
var
  I:  integer;
  xs: TStringList;
begin
  Debug(dpSpam, 'Proxys', 'Loadlist');
  xs     := TStringList.Create;
  try
    Result := False;
    socksini.ReadSections(xs);
    Debug(dpSpam, 'Proxys', IntToStr(xs.Count) + ' proxys in list...');
    for I := 0 to xs.Count - 1 do
      proxys.Add(TmSLSocks5.Create(xs.Strings[i]));
    Result := True;
  finally
    xs.Free;
  end;
  Debug(dpSpam, 'Proxys', 'Proxys added: ' + IntToStr(proxys.Count));
end;

function RehashProxys: boolean;
begin
  proxys.Clear;
  socksini.Free;
  socksini := TEncinifile.Create(ExtractFilePath(ParamStr(0)) +'slftp.socks5', passphrase);
  Result := StartProxys;
end;



function RemoveProxy(Name: AnsiString): boolean;
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

function RemoveProxy(index: integer): boolean;
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

end.

