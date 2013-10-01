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
  Classes, Contnrs, SyncObjs,encinifile, slstack, slssl
{$IFDEF FPC}
{$IFNDEF MSWINDOWS}
  , baseunix
{$ENDIF}
{$ENDIF}

  ;

const
  slDefaultTimeout = 30000; // default timeout is 30 seconds
  slDefaultBacklog = 30;
  slBufferSize = 16384;
  slLF = #10;
  slCR = #13;
  slEOL = slCR + slLF;

procedure InitProxys;
procedure UninitProxys;
function StartProxys:boolean;
function RehashProxys:boolean;

type
//  TslSSLMethod = (slSSLv23, slTLSv1);

  TmSLSocks5 = class
  name:string;
  private
  function GetHost:string;
  procedure SetHost(value:string);
  function GetPort:Integer;
  procedure SetPort(value:integer);
  function GetUsername:string;
  procedure SetUsername(value:string);
  function GetPassword:string;
  procedure SetPassword(value:string);
  function GetStatus:boolean;
  procedure SetStatus(value:boolean);
    public
    constructor create(bncname:string);
    procedure WStr(key,value:string);
    function RStr(key,default:string):string;
    procedure WInt(key:string;value:integer);
    function RInt(key:string;default:integer):integer;
    procedure WBool(key:string;value:boolean);
    function RBool(key:string;default:boolean):boolean;

    property Host:string read GetHost write SetHost;
    property Port:Integer read GetPort Write SetPort;
    property Username:string read GetUsername write SetUsername;
    property Password:string read GetPassword write SetPassword;
    property Enabled:boolean read GetStatus write SetStatus;
  end;


var
proxys:TObjectlist =nil;
socksini:TEncinifile;

function FindProxyByName(name:string):TmSLSocks5;
function RemoveProxy(index:integer):boolean;overload;
function RemoveProxy(name:string):boolean;overload;

implementation

uses debugunit,configunit,SysUtils;

{###    Socks5 Class... ###}

constructor TmSLSocks5.create(bncname: string);
begin
  self.name:=bncname;
end;

procedure TmSLSocks5.WStr(key: string; value: string);
begin
  socksini.WriteString(name,key,value);
  socksini.UpdateFile;
end;

procedure TmSLSocks5.WInt(key: string; value: Integer);
begin
  socksini.WriteInteger(name,key,value);
  socksini.UpdateFile;
end;

procedure TmSLSocks5.WBool(key: string; value: Boolean);
begin
  socksini.WriteBool(name,key,value);
  socksini.UpdateFile;
end;

function TmSLSocks5.RStr(key: string; default: string):string;
begin
result:=socksini.ReadString(name,key,default);
end;

function TmSLSocks5.RInt(key: string; default: integer):Integer;
begin
result:=socksini.ReadInteger(name,key,default);
end;


function TmSLSocks5.RBool(key: string; default: boolean):boolean;
begin
result:=socksini.ReadBool(name,key,default);
end;

procedure TmSLSocks5.SetHost(value: string);
begin
  WStr('Host',Value);
end;

function TmSLSocks5.GetHost:string;
begin
result:=RStr('Host','');
end;

procedure TmSLSocks5.SetPort(value: Integer);
begin
  WInt('Port',Value);
end;

function TmSLSocks5.GetPort:integer;
begin
result:=RInt('Port',-1);
end;

procedure TmSLSocks5.SetUsername(value: string);
begin
  WStr('Username',Value);
end;

function TmSLSocks5.GetUsername:string;
begin
 result:=RStr('Username','');
end;

procedure TmSLSocks5.SetPassword(value: string);
begin
  WStr('Password',Value);
end;

function TmSLSocks5.GetPassword:string;
begin
result:=RStr('Password','');
end;


procedure TmSLSocks5.SetStatus(value: Boolean);
begin
  WBool('Enabled',Value);
end;

function TmSLSocks5.GetStatus:boolean;
begin
result:=RBool('Enabled',False);
end;

{###    Controling Jobs... ###}


function FindProxyByName(name:string):TmSLSocks5;
var
  I: Integer;
begin
result:=nil;
for I := 0 to proxys.Count - 1 do
if TmSLSocks5(proxys.Items[i]).name = name then begin
result:=TmSLSocks5(proxys.Items[i]);
break;
end;
end;


procedure InitProxys;
begin
proxys:=TObjectlist.Create;
socksini:=TEncinifile.Create(ExtractFilePath(ParamStr(0))+'slftp.socks5', passphrase);
end;

procedure UninitProxys;
begin
Debug(dpSpam, 'Proxys', 'Uninit1');
  if proxys <> nil then begin
proxys.free;
proxys:=nil;
  end;
Debug(dpSpam, 'Proxys', 'Uninit2');
end;

function StartProxys:boolean;
var
  I: Integer;   xs:TStringlist;
begin
Debug(dpSpam, 'Proxys', 'Loadlist');
result:=False;
xs:=TStringlist.Create;
try
socksini.ReadSections(xs);
Debug(dpSpam, 'Proxys', inttostr(xs.Count)+' proxys in list...');
for I := 0 to xs.Count - 1 do
proxys.Add(TmSLSocks5.create(xs.Strings[i]));
finally
result:=True;
Debug(dpSpam, 'Proxys', 'Proxys added: '+inttostr(proxys.Count));
end;
xs.free;
end;

function RehashProxys:boolean;
begin
  result:=False;
  proxys.Clear;
  socksini.free;
  try
  socksini:=TEncinifile.Create(ExtractFilePath(ParamStr(0))+'slftp.socks5', passphrase);
  finally
    result:=StartProxys;
  end;
end;



function RemoveProxy(name:string):boolean;
begin
if proxys.Remove(FindProxyByName(name)) > -1 then begin
  socksini.EraseSection(name);
  socksini.UpdateFile;
  result:=True;
end else result:=False;
end;

function RemoveProxy(index:integer):boolean;
var
  name: String;
begin
  name:=TmSLSocks5(proxys.Items[index]).name;
if proxys.Remove(proxys.Items[index]) > -1 then begin
  socksini.EraseSection(name);
  socksini.UpdateFile;
  result:=True;
end else result:=False;
end;

end.
