unit sltcp;

interface

uses
  Classes, Contnrs, Types, SyncObjs,
  {$IFDEF FPC}
    {$IFNDEF MSWINDOWS}
      baseunix,
    {$ENDIF}
  {$ENDIF}
  mslproxys, slstack, debugunit, slssl, IdOpenSSLHeaders_ossl_typ, IdOpenSSLHeaders_ssl, IdOpenSSLHeaders_err;

const
  slDefaultTimeout = 10000; // default timeout is 10 seconds
  slDefaultBacklog = 30;
  slBufferSize = 16384;
  slLF = #10;
  slCR = #13;
  slEOL = slCR + slLF;


type

  TslSocks5 = class
    enabled: Boolean;
    username: {$IFDEF UNICODE}RawByteString{$ELSE}AnsiString{$ENDIF};
    password: {$IFDEF UNICODE}RawByteString{$ELSE}AnsiString{$ENDIF};
    host: String;
    port: Integer;
  end;

  TslTCPSocket = class; // forward
  TWaitingForSocketEvent = procedure(socket: TslTCPSocket; var ShouldQuit: Boolean) of object;
  TslTCPSocket = class
  private
    fss: TStringStream;
    fSSL: PSSL;
    fSSLCTX: PSSL_CTX;
    fBindIp: String;
    fBindPort: Integer;
    fOnWaitingforSocket: TWaitingforsocketEvent;
    socksextra: {$IFDEF UNICODE}RawByteString{$ELSE}AnsiString{$ENDIF};
    readlnsession: Boolean;
    function ConnectSocks5(timeout: Integer): Boolean;
    function ConnectB(host: String; port: Integer; timeout: Integer; udp: Boolean): Boolean;
    procedure DisconnectSSL;
    procedure ClearSocket;
    function IsFireing(timeout: Integer; shouldread, shouldwrite: Boolean): Boolean;
    function ShouldQuit: Boolean;
    function Reuse(b: Integer): Boolean;
  protected
    procedure SetSSLContext();
  public
    slSocket: TslSocket;
    Host: {$IFDEF UNICODE}RawByteString{$ELSE}AnsiString{$ENDIF};
    Port: Integer;
    error: String;

    socks5: TslSocks5;

    function IsWriteAble(timeout: Integer): Boolean;
    function IsReadAble(timeout: Integer): Boolean;
    function connected: Boolean;
    function SendBuffer(b: Integer): Boolean;
    function GetSocket(udp: Boolean = False; lReuse: Boolean = False): Boolean;
    function Accept(var newSocket: TslSocket): Boolean;
    function TurnToAccept: Boolean;
    function BindHost(): String; overload;
    function BindHost(host: String): Boolean; overload;
    function Connect(timeout: Integer = slDefaultTimeout; udp: Boolean = False): Boolean;
    function Write(s: {$IFDEF UNICODE}RawByteString{$ELSE}AnsiString{$ENDIF}; timeout: Integer = slDefaultTimeout): Boolean;
    function WriteLn(s: String; timeout: Integer = slDefaultTimeout): Boolean;
    function WriteBuffer(var Buf; BufSize: Integer; timeout: Integer = slDefaultTimeout): Boolean;
    function WriteStream(s: TStream; timeout: Integer = slDefaultTimeout; maxsend: Int64=0): Boolean;
    function SendStream(s: TStream; timeout: Integer = slDefaultTimeout; maxsend: Int64 = 0): Boolean;
    function Read(var s: String; timeout: Integer = slDefaultTimeout; maxolvasas: Integer = 0; untilDisconnects: Boolean = False): Boolean; overload;
    function Read(s: TStream; timeout: Integer = slDefaultTimeout; maxolvasas: Integer = 0; untilDisconnects: Boolean = False): Boolean; overload;
    function ReadLn(var line, alllines: String; timeout: Integer = slDefaultTimeout): Boolean;

    function Disconnect: Boolean;
    constructor Create; overload;
    constructor Create(proxyname:String); overload;
    procedure SetupSocket(c: TslSocket);
    destructor Destroy; override;
    function Listen(backlog: Integer): Boolean;

    function TurnToSSL(timeout: Integer = slDefaultTimeout): Boolean; overload;
    function TurnToSSL(sslctx: PSSL_CTX; timeout: Integer = slDefaultTimeout): Boolean; overload;
    function AcceptSSL(sslctx: PSSL_CTX; timeout: Integer = slDefaultTimeout): Boolean;
  published
    property BindPort: Integer read fBindPort write fBindPort;
    property OnWaitingforSocket: TWaitingForSocketEvent read fOnWaitingforSocket write fOnWaitingforSocket;
  end;

  TslTCPThread = class;
  TslCCHThread = class(TThread)
  private
    fTH: TslTCPThread;
  public
    constructor Create(owner: TslTCPThread; createSuspended: Boolean = True);
    destructor Destroy; override;
    procedure Execute; override;
  end;
  TslTCPThread = class(TslTCPSocket)
  private
    thread_running: Boolean;
    connectionThread: TslCCHThread;
  protected
    procedure WFS(socket: TslTCPSocket; var ShouldQuit: Boolean); virtual;
  public
    shouldquit: Boolean;
    procedure Execute; virtual; abstract;
    constructor Create(createSuspended: Boolean = True); overload;
    constructor Create(const aThreadName: String; createSuspended: Boolean = True); overload;
    destructor Destroy; override;
    procedure Start;
    procedure Stop; virtual;
  end;

type
  TWaitingForSocket = function(socket: TslTCPSocket): Boolean;

{$IFDEF FPC}
  {$IFDEF LINUX}
    const
      clib = 'c';

    function sendfile64(__out_fd:longint; __in_fd:longint; offset:Pointer; __count:size_t):ssize_t; cdecl; external clib name 'sendfile64';
  {$ENDIF}
{$ENDIF}

var
  slDefaultSocks5: TslSocks5;
  sltcp_LocalAddresses: TStringList;
  sltcp_error: String; //< Holds the last occured TCP Error
  sltcp_inited: Boolean = False;
  sltcp_onwaitingforsocket: TWaitingForSocket = nil;

implementation

uses SysUtils, slhelper, Math, DateUtils;


var sltcp_lock: TCriticalSection;


procedure sltcp_Init;
begin
  if not slStackInit(sltcp_error) then
    exit;

  sltcp_LocalAddresses := TStringList.Create;
  if not PopulateLocalAddresses(sltcp_LocalAddresses, sltcp_error) then
    exit;

  sltcp_lock := TCriticalSection.Create;
  sltcp_inited := True;

  slDefaultSocks5 := TslSocks5.Create;
  slDefaultSocks5.enabled := False;
  slDefaultSocks5.username := '';
  slDefaultSocks5.password := '';
  slDefaultSocks5.host := '';
  slDefaultSocks5.port := 0;
end;

procedure sltcp_UnInit;
begin
  if sltcp_inited then
    slStackUninit;

  sltcp_LocalAddresses.Free;
  sltcp_lock.Free;
  slDefaultSocks5.Free;

  sltcp_inited := False;
end;


{ TslTCPSocket }

constructor TslTCPSocket.Create;
begin
  if not sltcp_inited then
    raise Exception.Create('sltcp is not inited');

  ClearSocket;

  fSSLCTX := GetOpenSSLConnectionContext;

  socks5:= TslSocks5.Create;
  socks5.username:= slDefaultSocks5.username;
  socks5.password:= slDefaultSocks5.password;
  socks5.host:= slDefaultSocks5.host;
  socks5.port:= slDefaultSocks5.port;
  socks5.enabled:= ((slDefaultSocks5.enabled) and (socks5.host <> ''));

  fss:= TStringStream.Create('');

  inherited Create;
end;

constructor TslTCPSocket.Create(proxyname: String);
var sok5:TmSLSocks5;
begin
sok5:=FindProxyByName(proxyname);
if sok5 = nil then begin
Create;
exit;
end;
  if not sltcp_inited then
    raise Exception.Create('sltcp is not inited');

  ClearSocket;

  fSSLCTX := GetOpenSSLConnectionContext;

  socks5:= TslSocks5.Create;
  socks5.username:= sok5.username;
  socks5.password:= sok5.password;
  socks5.host:= sok5.host;
  socks5.port:= sok5.port;
  socks5.enabled:= ((sok5.enabled) and (sok5.host <> ''));

  fss:= TStringStream.Create('');

  inherited Create;
end;

procedure TslTCPSocket.ClearSocket;
begin
    slsocket.socket:= slSocketError;
(*
    slsocket.peerip:= '';
    slsocket.peerport:= 0;
    slsocket.localip:= '';
    slsocket.localport:= 0;
*)
end;
destructor TslTCPSocket.Destroy;
begin
  Disconnect;
  socks5.Free;
  fss.Free;

  inherited;
end;

procedure TslTCPSocket.DisconnectSSL;
begin
  if fSSL <> nil then
  begin
    SSL_shutdown(fSSL);

    SSL_free(fSSL);
    fSSL:= nil;
  end;

end;

function TslTCPSocket.Disconnect: Boolean;
begin
  Result:= False;
  if slSocket.socket = slSocketError then
    exit;

  DisconnectSSL;

  slShutdown(slSocket);
  slClose(slSocket);
  ClearSocket;
  Result:= True;
end;

function TslTCPSocket.ShouldQuit: Boolean;
begin
  Result:= False;
        if Assigned(fOnWaitingforSocket) then
          fOnWaitingforSocket(self, Result);
        if ((not Result) and (Assigned(sltcp_onwaitingforsocket))) then
          Result:= sltcp_onwaitingforsocket(self);

        if Result then
        begin
          // Debug(dpError, 'Should quit');
          Disconnect;
          error:= 'shouldquit';
          exit;
        end;
end;

function TslTCPSocket.IsFireing(timeout: Integer; shouldread, shouldwrite: Boolean): Boolean;
const gyakorisag = 100;
var i: Integer;
begin
  Result:= False;
  if ShouldQuit then exit;

  if slsocket.socket = slsocketerror then exit;

  try
    i:= 0;
    while(true) do
    begin
      error:= '';
      if not slSelect(slSocket, gyakorisag, shouldread, shouldwrite, error) then
      begin

        slDebug('nem slselect');

        if error = 'timeout' then
        begin
          if i * gyakorisag >= timeout then
            exit;

          if ShouldQuit then exit;

          inc(i);
          Continue;
        end;

        exit;
      end;

      slDebug('igen slselect');
      Break;
    end;
    Result:= True;
  except
    on e: Exception do
    begin
      Debug(dpError, 'sltcp', Format('[EXCEPTION] TslTCPSocket.IsFireing: %s', [e.Message]));
      Result:= False;
    end;
  end;
end;

function TslTCPSocket.GetSocket(udp: Boolean = False; lReuse: Boolean= False): Boolean;
begin
  Result:= False;
  sltcp_lock.Enter;
  try
    // kell kerni egy uj socketet
    // Obtain a new socket
    if not slGetSocket(slSocket, udp, error) then
    begin
      Disconnect;
      exit;
    end;

    if(lReuse)then
      reuse(1);


    // kell bindelni
    //be ?bindelni?
    if ((fBindIp <> '') or (fBindPort <> 0)) then
    begin
      if not slBind(slSocket, fBindIp, fBindPort, error) then
      begin
        Disconnect;
        exit;
      end;
    end;
    Result:= True;
  finally
    sltcp_lock.Leave;

  end;
end;

function TslTCPSocket.ConnectB(host: String; port: Integer; timeout: Integer; udp: Boolean): Boolean;
var rc: Integer;
begin
  Result:= False;

  slDebug('getsocket elott');

  if not GetSocket(udp) then
  begin
    Disconnect;
    exit;
  end;

  if not slSetnonblocking(slSocket, error) then
  begin
    Disconnect;
    exit;
  end;

  slDebug('connect elott');

  // and now go to connect
  rc:= slConnect(slSocket, host, port, error);
  if rc < 0 then
  begin
    // error already set!
    Disconnect;
    exit;
  end;

  slDebug('connect utan');

  if not IsWriteAble(timeout) then
  begin
    Disconnect;
    exit;
  end;

  slDebug('soerror elott');

  if(slSoError(slSocket, error) <> '') then
  begin
    Disconnect;
    exit;
  end;

  slDebug('soerror utan');

  if((not udp and (not slSetKeepalive(slSocket,True, error)))) then
  begin
    Disconnect;
    exit;
  end;

  if not slSetblocking(slSocket, error) then
  begin
    Disconnect;
    exit;
  end;

  if not slGetSockName(slSocket) then
  begin
    Disconnect;
    exit;
  end;

  error:= '';

  Result:= True;
end;

function TslTCPSocket.ConnectSocks5(timeout: Integer): Boolean;
var
  tempBuffer: array [0..255] of AnsiChar;
  s: String;
  pos: Integer;
  tempPort: Word;
begin
  Result:= False;

  if not ConnectB(socks5.host, socks5.port, timeout, False) then exit;

  // defined in rfc 1928
  if socks5.username = '' then begin
    tempBuffer[2] := #0   // No authentication
  end else begin
    tempBuffer[2] := #2;  // Username password authentication
  end;

  tempBuffer[0] := #5;     // socks version
  tempBuffer[1] := #1;     // number of possible authentication methods

  if not WriteBuffer(tempBuffer, 3) then exit;
  if not Read(s, timeout) then exit;

  if ((AnsiChar(s[2]) <> tempBuffer[2]) or (s[2] = #255)) then
  begin
    error:= 'Authentication method is not supported by the socks5 server';
    exit;
  end;

  // Authentication process
  if socks5.username <> '' then
  begin
    tempBuffer[0] := #1; // version of subnegotiation
    tempBuffer[1] := AnsiChar(Chr(Length(socks5.Username)));
    pos := 2;
    if Length(socks5.Username) > 0 then begin
      Move(socks5.Username[1], tempBuffer[pos], Length(socks5.Username));
    end;
    pos := pos + Length(socks5.Username);
    tempBuffer[pos] := AnsiChar(Chr(Length(socks5.Password)));
    pos := pos + 1;
    if Length(socks5.Password) > 0 then begin
      Move(socks5.Password[1], tempBuffer[pos], Length(socks5.Password));
    end;
    pos := pos + Length(socks5.Password);

    if not WriteBuffer(tempBuffer, pos) then exit;
    if not Read(s, timeout) then exit;

    if s[2] <> #0 then
    begin
      error:= 'Invalid username/password for the socks5 server';
      exit;
    end;
  end;

  // Connection process
  tempBuffer[0] := #5;   // socks version
  tempBuffer[1] := #1;   // connect method
  tempBuffer[2] := #0;   // reserved
  // for now we stick with domain name, must ask Chad how to detect
  // address type
  tempBuffer[3] := #3;   // address type: IP V4 address: X'01'    {Do not Localize}
                         //               DOMAINNAME:    X'03'    {Do not Localize}
                         //               IP V6 address: X'04'    {Do not Localize}
  // host name
  if 0 = System.Pos('.', Host) then begin
    Host := slConvertIp(Host);
  end;

  tempBuffer[4] := AnsiChar(Chr(Length(Host)));
  pos := 5;
  if Length(Host) > 0 then begin
    Move(Host[1], tempBuffer[pos], Length(Host));
  end;
  pos := pos + Length(Host);
  // port
  tempPort := slHToNs(Port);
  Move(tempPort, tempBuffer[pos], SizeOf(tempPort));
  pos := pos + 2;

  if not WriteBuffer(tempBuffer, pos) then exit;
  if not Read(s, timeout) then exit;


  case s[2] of
    #0: ;// success, do nothing
    #1: error:= 'socks5 error: general';
    #2: error:= 'socks5 error: permission';
    #3: error:= 'socks5 error: network unreachable';
    #4: error:= 'socks5 error: host unreachable';
    #5: error:= 'socks5 error: connection refused';
    #6: error:= 'socks5 error: ttl expired';
    #7: error:= 'socks5 error: command';
    #8: error:= 'socks5 error: address';
    else
       error:= 'socks5 error: unknown';
  end;

  if error <> '' then exit;

  socksextra:= Copy(s, 11, Length(s)-11);

  Result:= True;
end;

function TslTCPSocket.Connect(timeout: Integer = slDefaultTimeout; udp: Boolean = False): Boolean;
begin
  Result:= False;
  try
    if slSocket.socket <> slSocketError then
    begin
      error:= 'already connected';
      exit;
    end;

    if udp and socks5.enabled then
    begin
      error:= 'udp cant be used with socks5';
      exit;
    end;

    slDebug('disconnect elott');

    Disconnect;
    error:= '';

    if socks5.enabled then
      Result:= ConnectSocks5(timeout)
    else
      Result:= ConnectB(Host, Port, timeout, udp);
  except
    on e: Exception do
    begin
      Debug(dpError, 'sltcp', Format('[EXCEPTION] TslTCPSocket.Connect: %s', [e.Message]));
      Result:= False;
    end;
  end;
end;



function TslTCPSocket.TurnToSSL(timeout: Integer = slDefaultTimeout): Boolean;
begin
  try
    Result:= TurnToSSL(fsslctx, timeout);
  except
    on e: Exception do
    begin
      Debug(dpError, 'sltcp', Format('[EXCEPTION] TslTCPSocket.TurnToSSL: %s', [e.Message]));
      Result:= False;
    end;
  end;
end;

function TslTCPSocket.TurnToSSL(sslctx: PSSL_CTX; timeout: Integer = slDefaultTimeout): Boolean;
var er: String;
    sslerr, err, i: Integer;
    shouldquit: Boolean;
begin
  shouldquit:= False;
  Result:= False;
  try
    setlength(er, 512);

    if slSocket.socket = slSocketError then
    begin
      error:= 'not connected';
      exit;
    end;

    if (fSSL <> nil) then
    begin
      error:= 'ssl connection is already negotiated';
      exit;
    end;

    if not slSetNonblocking(slSocket, error) then exit;


    sltcp_lock.Enter;
    try
      fSSL:= nil;
      fssl:= SSL_new(sslctx);
    finally
      sltcp_lock.Leave;
    end;

    if (fSSL = nil) then
    begin
      ERR_error_string(ERR_get_error(), @er[1]);
      er:= AnsiString(PAnsiChar(er));
      error:= 'Cant create new ssl: '+er;
      exit;
    end;

    if SSL_set_fd(fSSL, slSocket.socket) = 0 then
    begin
      DisconnectSSL;
      error:= 'ssl set fd returned false';
      exit;
    end;

    // try for 10 seconds
    i:= 0;
    while(true)do
    begin
      if i* 100 > timeout then
      begin
        error:= 'timeout';
        ERR_error_string(ERR_get_error(), @er[1]);
        er:= AnsiString(PAnsiChar(er));
        error:= 'ssl failed '+er;
        DisconnectSSL;
        slSetBlocking(slSocket,er);
        exit;
      end;

      err:= SSL_connect(fssl);

      if(err = 1) then Break;

      sslerr:= SSL_get_error(fssl, err);
      if((sslerr = SSL_ERROR_WANT_READ) or (sslerr = SSL_ERROR_WANT_WRITE) or (sslerr = SSL_ERROR_WANT_X509_LOOKUP)) then
      begin
        if Assigned(fOnWaitingforSocket) then
          fOnWaitingforSocket(self, shouldquit);
        if ((not shouldquit) and (Assigned(sltcp_onwaitingforsocket))) then
          shouldquit:= sltcp_onwaitingforsocket(self);

        if shouldquit then
        begin
          DisconnectSSL;
          slSetBlocking(slSocket,er);
          error:= 'shouldquit';
          exit;
        end;

        Sleep(100);
        inc(i);
      end
      else
      begin
        ERR_error_string(ERR_get_error(), @er[1]);
        er:= AnsiString(PAnsiChar(er));
        error:= 'ssl failed '+er;
        DisconnectSSL;
        slSetBlocking(slSocket,er);
        exit;
      end;

    end;


    if not slSetBlocking(slSocket,error) then exit;

    Result:= True;
  except
    on e: Exception do
    begin
      Debug(dpError, 'sltcp', Format('[EXCEPTION] TslTCPSocket.TurnToSSL: %s', [e.Message]));
      error:= 'exception';
      Result:= False;
    end;
  end;
end;

function TslTCPSocket.AcceptSSL(sslctx: PSSL_CTX; timeout: Integer = slDefaultTimeout): Boolean;
var er: String;
    sslerr, err, i: Integer;
    shouldquit: Boolean;
begin
  shouldquit:= False;
  Result:= False;
  try
    setlength(er, 512);

    if slSocket.socket = slSocketError then
    begin
      error:= 'not connected';
      exit;
    end;

    if (fSSL <> nil) then
    begin
      error:= 'ssl connection is already negotiated';
      exit;
    end;

    if not slSetNonblocking(slSocket, error) then exit;


    sltcp_lock.Enter;
    try
      fssl:= SSL_new(sslctx);
    finally
      sltcp_lock.Leave;
    end;

    if (fSSL = nil) then
    begin
      ERR_error_string(ERR_get_error(), @er[1]);
      er:= AnsiString(PAnsiChar(er));
      error:= 'Cant create new ssl: '+er;
      exit;
    end;

    if SSL_set_fd(fSSL, slSocket.socket) = 0 then
    begin
      DisconnectSSL;
      error:= 'ssl set fd returned false';
      exit;
    end;

    // try for 10 seconds
    i:= 0;
    while(true)do
    begin
      if i* 100 > timeout then
      begin
        error:= 'timeout';
        ERR_error_string(ERR_get_error(), @er[1]);
        er:= AnsiString(PAnsiChar(er));
        error:= 'ssl failed '+er;
        DisconnectSSL;
        slSetBlocking(slSocket,er);
        exit;
      end;

      err:= SSL_accept(fssl);

      if(err = 1) then Break;

      sslerr:= SSL_get_error(fssl, err);
      if((sslerr = SSL_ERROR_WANT_READ) or (sslerr = SSL_ERROR_WANT_WRITE) or (sslerr = SSL_ERROR_WANT_X509_LOOKUP)) then
      begin
        if Assigned(fOnWaitingforSocket) then
          fOnWaitingforSocket(self, shouldquit);
        if ((not shouldquit) and (Assigned(sltcp_onwaitingforsocket))) then
          shouldquit:= sltcp_onwaitingforsocket(self);

        if shouldquit then
        begin
          DisconnectSSL;
          slSetBlocking(slSocket,er);
          error:= 'shouldquit';
          exit;
        end;

        Sleep(100);
        inc(i);
      end
      else
      begin
        ERR_error_string(ERR_get_error(), @er[1]);
        er:= AnsiString(PAnsiChar(er));
        error:= 'ssl failed '+er;
        DisconnectSSL;
        slSetBlocking(slSocket,er);
        exit;
      end;

    end;


    if not slSetBlocking(slSocket,error) then exit;

    Result:= True;
  except
    on e: Exception do
    begin
      Debug(dpError, 'sltcp', Format('[EXCEPTION] TslTCPSocket.AcceptSSL: %s', [e.Message]));
      error:= 'exception';
      Result:= False;
    end;
  end;
end;


function TslTCPSocket.Write(s: {$IFDEF UNICODE}RawByteString{$ELSE}AnsiString{$ENDIF}; timeout: Integer = slDefaultTimeout): Boolean;
begin
  try
    Result := WriteBuffer(s[1], Length(s), timeout);
  except
    on e: Exception do
    begin
      Debug(dpError, 'sltcp', Format('[EXCEPTION] TslTCPSocket.Write: %s', [e.Message]));
      error:= 'exception';
      Result:= False;
    end;
  end;
end;

function TslTCPSocket.WriteBuffer(var Buf; BufSize: Integer; timeout: Integer = slDefaultTimeout): Boolean;
begin
  Result:= False;

  try
    if slSocket.socket = slSocketError then
    begin
      error:= 'Socket not initialized';
      exit;
    end;
    error:= '';

    if not IsWriteAble(timeout) then
    begin
      Disconnect;
      exit;
    end;

    if fSSL <> nil then
      Result:= slSend(fSSL, Buf, BufSize, error)
    else
      Result:= slSend(slSocket, Buf, BufSize, error);
  except
    on e: Exception do
    begin
      Debug(dpError, 'sltcp', Format('[EXCEPTION] TslTCPSocket.WriteBuffer: %s', [e.Message]));
      error:= 'exception';
      Result:= False;
    end;
  end;
end;

function TslTCPSocket.WriteStream(s: TStream; timeout: Integer = slDefaultTimeout; maxsend: Int64=0): Boolean;
var buf: array[0..65535] of byte;
    ennyit, r: Integer;
begin
  Result:= False;

  try
    if maxsend = 0 then
      maxsend:= s.Size - s.Position;
    while maxsend > 0 do
    begin
      ennyit:= Min(SizeOf(buf), maxsend);
      r:= s.Read(buf, ennyit);
      if r > 0 then
      begin
        dec(maxsend, r);
        Result:= WriteBuffer(buf, r, timeout);
        if not result then exit;
      end else
        exit;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, 'sltcp', Format('[EXCEPTION] TslTCPSocket.WriteStream: %s', [e.Message]));
      error:= 'exception';
      Result:= False;
    end;
  end;
end;
function TslTCPSocket.SendStream(s: TStream; timeout: Integer = slDefaultTimeout; maxsend: Int64 = 0): Boolean;
{$IFDEF FPC}
var fh: THandleStream;
{$ENDIF}
begin
  try
{$IFDEF LINUX}
    if ((s is THandleStream) and (fSSL <> nil)) then
    begin
      Result:= False;
      fh:= THandleStream(s);
      if maxsend = 0 then
        maxsend:= s.Size - s.Position;

      if iswriteable(timeout) then
        Result:= sendfile64(slSocket.socket, fh.Handle, nil, maxsend) > 0;
    end
    else
      Result:= WriteStream(s, timeout, maxsend);
{$ELSE}
  Result:= WriteStream(s, timeout, maxsend);
{$ENDIF}
  except
    on e: Exception do
    begin
      Debug(dpError, 'sltcp', Format('[EXCEPTION] TslTCPSocket.SendStream: %s', [e.Message]));
      error:= 'exception';
      Result:= False;
    end;
  end;
end;


function TslTCPSocket.ReadLn(var line, alllines: String; timeout: Integer = slDefaultTimeout): Boolean;
begin
  Result:= False;
  try
    if not readlnsession then
    begin
      if not Read(alllines, timeout) then
        exit;
      readlnsession:= True;
    end;

    if alllines = '' then
    begin
      readlnsession:= False;
      exit;
    end;

    line:= ElsoSor(alllines);
    Result:= True;
  except
    on e: Exception do
    begin
      Debug(dpError, 'sltcp', Format('[EXCEPTION] TslTCPSocket.ReadLn: %s', [e.Message]));
      error:= 'exception';
      Result:= False;
    end;
  end;
end;

function TslTCPSocket.WriteLn(s: String; timeout: Integer = slDefaultTimeout): Boolean;
begin
  try
    Result:= Write(s+slEOL, timeout);
  except
    on e: Exception do
    begin
      Debug(dpError, 'sltcp', Format('[EXCEPTION] TslTCPSocket.WriteLn: %s', [e.Message]));
      error:= 'exception';
      Result:= False;
    end;
  end;
end;

function TslTCPSocket.BindHost(host: String): Boolean;
var ip: String;
begin
  Result:= False;
  error:= '';
  if host = '' then
  begin
    Result:= True;
    fBindIp:= '';
    exit;
  end;
  ip:= slResolve(host, error);
  if ip = '' then exit;
(*
  if sltcp_LocalAddresses.IndexOf(ip) = -1 then
  begin
    error:= 'Invalid local IP';
    exit;
  end;
*)
  fBindIp:= ip;

  Result:= True;
end;

function TslTCPSocket.BindHost: String;
begin
  Result:= fBindIp;
end;

function TslTCPSocket.Read(var s: String; timeout: Integer = slDefaultTimeout; maxolvasas: Integer = 0; untilDisconnects: Boolean = False): Boolean;
begin
  try
    fss.Size:= 0;
    Result:= Read(fss, timeout, maxolvasas, untilDisconnects);
    s:= fss.DataString;
  except
    on e: Exception do
    begin
      Debug(dpError, 'sltcp', Format('[EXCEPTION] TslTCPSocket.Read: %s', [e.Message]));
      error:= 'exception';
      Result:= False;
    end;
  end;
end;

function TslTCPSocket.Read(s: TStream; timeout: Integer = slDefaultTimeout; maxolvasas: Integer = 0; untilDisconnects: Boolean = False): Boolean;
var buffer: array[1..slBufferSize] of AnsiChar;
    aktolvasas, osszesolvasva, olvasva: Integer;
begin
  Result:= False;
  try
    if slSocket.socket = slsocketerror then
    begin
      error:= 'not connected';
      exit;
    end;

    if socksextra <> '' then
    begin
      s.WriteBuffer(socksextra[1], Length(socksextra));
      socksextra:= '';
      Result:= True;
      exit;
    end;

    error:= '';
    osszesolvasva:= 0;
  //debug(dpSpam, 'isreadable elott');
    while (IsReadAble(timeout)) do
    begin
      aktolvasas:= slBufferSize;
      if ((maxolvasas > 0) and (osszesolvasva + aktolvasas > maxolvasas)) then
        aktolvasas:= maxolvasas - osszesolvasva;

  //debug(dpSpam, 'slrecv elott');

      if fSSL <> nil then
        olvasva:= slRecv(fSSL, buffer, aktolvasas, error)
      else
        olvasva:= slRecv(slSocket, buffer, aktolvasas, error);

  //debug(dpSpam, 'slrecv utan, olvasva %d', [olvasva]);

      if olvasva <= 0 then
      begin
        if 0 <> Pos('syscall', error) then
          error:= 'Connection broken';
        Disconnect;
        if osszesolvasva = 0 then
          Result:= False;
        exit;
      end;

      if ShouldQuit then
      begin
        Result:= False;
        exit;
      end;

      Result:= True;

      s.WriteBuffer(buffer, olvasva);

      inc(osszesolvasva, olvasva);
      if ((osszesolvasva >= maxolvasas) and (maxolvasas > 0)) then exit;

      if not untilDisconnects then exit;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, 'sltcp', Format('[EXCEPTION] TslTCPSocket.Read: %s', [e.Message]));
      error:= 'exception';
      Result:= False;
    end;
  end;
end;

function TslTCPSocket.IsReadAble(timeout: Integer): Boolean;
begin
  try
    Result:= IsFireing(timeout, True, False);
  except
    on e: Exception do
    begin
      Debug(dpError, 'sltcp', Format('[EXCEPTION] TslTCPSocket.IsReadAble: %s', [e.Message]));
      error:= 'exception';
      Result:= False;
    end;
  end;
end;

function TslTCPSocket.IsWriteAble(timeout: Integer): Boolean;
begin
  try
    Result:= IsFireing(timeout, False, True);
  except
    on e: Exception do
    begin
      Debug(dpError, 'sltcp', Format('[EXCEPTION] TslTCPSocket.IsWriteAble: %s', [e.Message]));
      error:= 'exception';
      Result:= False;
    end;
  end;
end;

function TslTCPSocket.Reuse(b: Integer): Boolean;
const SO_REUSEADDR = 2;
begin
  Result:= False;
  try
    if not slSetSockopt(slSocket, SO_REUSEADDR, b, error) then exit;
    Result:= True;
  except
    on e: Exception do
    begin
      Debug(dpError, 'sltcp', Format('[EXCEPTION] TslTCPSocket.Reuse: %s', [e.Message]));
      error:= 'exception';
      Result:= False;
    end;
  end;
end;

function TslTCPSocket.Accept(var newSocket: TslSocket): Boolean;
begin
  Result:= False;
  try
    if slSocket.socket = slSocketError then
    begin
      error:= 'socket not initialized';
      exit;
    end;

    error:= '';
    Result:= slAccept(slSocket, newSocket, error);
  except
    on e: Exception do
    begin
      Debug(dpError, 'sltcp', Format('[EXCEPTION] TslTCPSocket.Accept: %s', [e.Message]));
      error:= 'exception';
      Result:= False;
    end;
  end;
end;

function TslTCPSocket.TurnToAccept(): Boolean;
var newSocket: TslSocket;
begin
  Result:= False;
  if slSocket.socket = slSocketError then
  begin
    error:= 'socket not initialized';
    exit;
  end;

  error:= '';
  Result:= slAccept(slSocket, newSocket, error);
  Disconnect;
  if Result then
  begin
    slSocket.socket:= newSocket.socket;
    slSocket.peerip:= newSocket.peerip;
    slSocket.peerport:= newSocket.peerport;
    slSocket.localip:= newSocket.localip;
    slSocket.localport:= newSocket.localport;
  end;
end;


procedure TslTCPSocket.SetupSocket(c: TslSocket);
begin
//  slSocket:= c;
  slSocket.socket:= c.Socket;
  slSocket.peerip:= c.peerip;
  slSocket.peerport:= c.peerport;
  slSocket.localip:= c.localip;
  slSocket.localport:= c.localport;

end;

function TslTCPSocket.Listen(backlog: Integer): Boolean;
begin
  Result:= slListen(slSocket, backlog, error);
end;

function TslTCPSocket.SendBuffer(b: Integer): Boolean;
begin
  Result:= False;
  if not slSetSockopt(slsocket, $1001, b, error) then exit;
  (*
  b:= 0;
  if not slGetSockopt(slsocket, $1001, b, error) then exit;
  System.writeln(IntToStr(b));
  *)
  Result:= True;
end;

procedure TslTCPSocket.SetSSLContext();
begin
  fSSLCTX := GetOpenSSLConnectionContext;
end;

function TslTCPSocket.connected: Boolean;
begin
  Result:= slSocket.socket <> slSocketError;
end;

{ TslTCPThread }

constructor TslTCPThread.Create(createSuspended: Boolean = True);
begin
  inherited Create;
  OnWaitingforSocket := WFS;
  connectionThread := TslCCHThread.Create(self, createSuspended);
end;

constructor TslTCPThread.Create(const aThreadName: String; createSuspended: Boolean = True);
begin
  inherited Create;
  OnWaitingforSocket := WFS;
  connectionThread := TslCCHThread.Create(self, createSuspended);

  {$IFDEF DEBUG}
    connectionThread.NameThreadForDebugging(aThreadName, connectionThread.ThreadID);
  {$ENDIF}
end;

destructor TslTCPThread.Destroy;
begin
  Stop;
  inherited;
end;

procedure TslTCPThread.Start;
begin
  {$IFDEF MSWINDOWS}
    connectionThread.Resume;
  {$ELSE}
    connectionThread.Start;
  {$ENDIF}
end;

procedure TslTCPThread.Stop;
begin
  shouldquit:= True;
  while thread_running do Sleep(100);
end;

procedure TslTCPThread.WFS(socket: TslTCPSocket; var ShouldQuit: Boolean);
begin
  Shouldquit := self.shouldquit;
end;

{ TslCCHThread }

constructor TslCCHThread.Create(owner: TslTCPThread; createSuspended: Boolean = True);
begin
  inherited Create(createSuspended);
  fTh := owner;
  FreeOnTerminate := True;
end;

destructor TslCCHThread.Destroy;
begin
  fTh.thread_running := False;
  inherited;
end;

procedure TslCCHThread.Execute;
begin
  fTh.thread_running := True;
  fTh.Execute();
end;


initialization
  sltcp_Init;
finalization
  sltcp_Uninit;
end.

