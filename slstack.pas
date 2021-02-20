unit slstack;

interface


uses
  Classes, SysUtils,
  slssl, IdOpenSSLHeaders_ssl, IdOpenSSLHeaders_ossl_typ, debugunit, mystrings,
{$IFDEF FPC}
  sockets
  {$IFDEF MSWINDOWS}
  , winsock2
  {$ELSE}
  , baseunix, unix
  {$ENDIF}
{$ELSE}
  {$IFDEF MSWINDOWS}
  Windows, IdWinsock2
  {$ELSE}
    Libc
  {$ENDIF}
{$ENDIF}
;

// needed for below TslSocket, else SOCKET_ERROR gives a compiler error in windows
type
  TSocket = Integer;

type
  TslSocket = record
    socket: TSocket;
    peerip: String;
    peerport: Integer;
    localip: String;
    localport: Integer;
  end;


const
{$IFDEF FPC}
  slSocketError = 0;
  {$IFNDEF MSWINDOWS}
    sd_receive = $00;
    sd_send    = $01;
    sd_both    = $02;

  {$ENDIF}
{$ELSE}
  {$IFDEF MSWINDOWS}
    slSocketError = SOCKET_ERROR;
  {$ELSE}
    slSocketError = -1;

    sd_receive = $00;
    sd_send    = $01;
    sd_both    = $02;
  {$ENDIF}
{$ENDIF}

function slStackInit(var error: String): Boolean;
procedure slStackUninit;
function slShutDown( var s: TslSocket ): Integer;
function slClose( var s: TslSocket ): Integer;
function slSetnonblocking(s: TslSocket; var error: String): Boolean;
function slSetblocking(s: TslSocket; var error: String): Boolean;
function slGetHostByName(AHostName: String; var error: String): String; overload;
function slResolve(host: String; var error: String): String;
function slConvertIp(host: String): String;
function slBind(var slSocket: TslSocket; ip: String; port: Word; var error: String): Boolean;
function slGetHostName: {$IFDEF UNICODE}RawByteString{$ELSE}AnsiString{$ENDIF};
function PopulateLocalAddresses(l: TStringList; var error: String): Boolean;
function slGetSocket(var slSocket: TslSocket; udp: Boolean; var error: String): Boolean;
function slConnect(socket: TslSocket; host: String; port: Integer; var error: String): Integer;
function slGetSockopt(slSocket: TslSocket; i2: Integer; var ret: Integer; var error: String): Boolean;
function slSoError(slSocket: TslSocket; var error: String): String;
function slSetSockOpt(slSocket: TslSocket; i: Integer; rc: Integer; var error: String): Boolean;
function slSetKeepalive(slSocket: TslSocket; alive: Boolean; var error: String): Boolean;
function slGetKeepalive(slSocket: TslSocket; var alive: Boolean;var error: String): Boolean;
function slSelect(var slSocket: TslSocket; timeout: Integer; shouldread, shouldwrite: Boolean; var error: String): Boolean; overload;
function slSelect(var slSocket1, slSocket2: TslSocket; timeout: Integer; shouldread, shouldwrite: Boolean; var error: String): Integer; overload;
function slLastError: String; overload;
function slLastError(err: Integer): String; overload;
function slLastErrno: Integer;
function slRecv(slSocket: TslSocket; var buffer; bufsize: Integer; var error: String): Integer; overload;
function slRecv(ssl: PSSL; var buffer; bufsize: Integer; var error: String): Integer; overload;
function slSend(ssl: PSSL; var buffer; bufsize: Integer; var error: String): Boolean; overload;

function slSend(slSocket: TslSocket; var buffer; bufsize: Integer; var error: String): Boolean; overload;
function slListen(slSocket: TslSocket; backlog: Integer; var error: String): Boolean;
function slAccept(var listenSocket, newSocket: TslSocket; var error: String): Boolean;
function slHToNs(port: Integer): Integer;
function slGetSockName(var socket: TslSocket): Boolean;
procedure slDebug(s: String);

implementation

uses slhelper, StrUtils;

function slStackInit(var error: String): Boolean;
begin
  Result := False;

  {$IFDEF MSWINDOWS}
    try
      InitializeWinSock;
    except
      on e : Exception do
      begin
        error := e.Message;
        exit;
      end;
      on e : EIdWinsockStubError do
      begin
        error := e.Message + ' -- ' + e.Win32ErrorMessage;
        exit;
      end;
    end;
  {$ELSE}
    // nothing to do on linux
  {$ENDIF}

  Result := True;
end;

{$IFDEF MSWINDOWS}
function slSetnonblocking(s: TslSocket; var error: String): Boolean;
var iMode: Cardinal;
    r: Integer;
begin
  Result:= False;

//-------------------------
// Set the socket I/O mode: In this case FIONBIO
// enables or disables the blocking mode for the
// socket based on the numerical value of iMode.
// If iMode = 0, blocking is enabled;
// If iMode != 0, non-blocking mode is enabled.
  r:= ioctlsocket(s.socket, FIONBIO, iMode);
  if(r<>0) then
  begin
    error:= 'setnonblocking set flags failed '+IntToStr(r);
    exit;
  end;
  Result:= True;
end;

function slSetblocking(s: TslSocket; var error: String): Boolean;
var iMode: Cardinal;
    r: Integer;
begin
  Result:= False;

//-------------------------
// Set the socket I/O mode: In this case FIONBIO
// enables or disables the blocking mode for the
// socket based on the numerical value of iMode.
// If iMode = 0, blocking is enabled;
// If iMode != 0, non-blocking mode is enabled.
  iMode:= 0;
  r:= ioctlsocket(s.socket, FIONBIO, iMode);
  if(r<>0) then
  begin
    error:= 'setblocking set flags failed '+IntToStr(r);
    exit;
  end;
  Result:= True;
end;
{$ELSE}
function slSetnonblocking(s: TslSocket; var error: String): Boolean;
var flags: Integer;
begin
  Result:= False;

{$IFDEF FPC}
  flags:= fpfcntl(s.socket, F_GETFL, 0);
{$ELSE}
  flags:= fcntl(s.socket, F_GETFL, 0);
{$ENDIF}
  if(flags = -1) then
  begin
    error:= 'setnonblocking get flags failed';
    exit;
  end;
  flags := flags or O_NONBLOCK;
{$IFDEF FPC}
  if (fpfcntl(s.socket, F_SETFL, flags) = -1) then
{$ELSE}
        if (fcntl(s.socket, F_SETFL, flags) = -1) then
{$ENDIF}
  begin
    error:= 'setnonblocking set flags failed';
    exit;
  end;

  Result:= True;
end;
function slSetblocking(s: TslSocket; var error: String): Boolean;
var flags: Integer;
begin
  Result:= False;

{$IFDEF FPC}
  flags:= fpfcntl(s.socket, F_GETFL, 0);
{$ELSE}
  flags:= fcntl(s.socket, F_GETFL, 0);
{$ENDIF}
  if(flags = -1) then
  begin
    error:= 'setblocking get flags failed';
    exit;
  end;
  flags := flags and not O_NONBLOCK;
{$IFDEF FPC}
        if (fpfcntl(s.socket, F_SETFL, flags) = -1) then
{$ELSE}
        if (fcntl(s.socket, F_SETFL, flags) = -1) then
{$ENDIF}
  begin
    error:= 'setblocking set flags failed';
    exit;
  end;

  Result:= True;
end;

{$ENDIF}

procedure slStackUninit;
begin
{$IFDEF MSWINDOWS}
  WSACleanup();
{$ENDIF}
end;


function slShutDown( var s: TslSocket ): Integer;
begin
{$IFDEF FPC}
  Result:= fpshutdown(s.socket, sd_both );
{$ELSE}
  Result:= shutdown(s.socket, sd_both );
{$ENDIF}
end;

function slClose( var s: TslSocket ): Integer;
begin
{$IFDEF FPC}
  Result:= closesocket(s.socket);

{$ELSE}
{$IFDEF MSWINDOWS}
  Result:= closesocket(s.socket);
{$ELSE}
  Result:= __close(s.socket);
{$ENDIF}
{$ENDIF}
end;



{$IFDEF FPC}
  {$IFNDEF MSWINDOWS}
  const
    { Net type }
    socklib = 'c';

  Type

    { THostEnt Object }
    THostEnt = record
      H_Name     : PAnsiChar;   { Official name }
      H_Aliases  : ppansichar;  { Null-terminated list of aliases}
      H_Addrtype : longint;   { Host address type }
      H_length  : longint;   { Length of address }
      H_Addr_list : ppansichar;    { null-terminated list of adresses }
    end;
    PHostEntry = ^THostEnt;
    PHostEnt = PHostEntry;

  function gethostbyname ( Name : PAnsiChar) : PHostEntry; cdecl; external socklib;
  {$ENDIF}
{$ENDIF}



// Delphi/Kylix
function slGetHostByName(AHostName: String; var error: String): String; overload;
var
  pa: PAnsiChar;
  sa: TInAddr;
  Host: PHostEnt;
begin
  Result:= '';
  {$IFDEF UNICODE}
    Host := GetHostByName(PAnsiChar(RawByteString(AHostName)));
  {$ELSE}
    Host := GetHostByName(PAnsiChar(AHostName));
  {$ENDIF}
  if Host <> nil then
  begin
  {$IFDEF MSWINDOWS}
    {$IFDEF FPC}
    pa := Host^.h_addr_list^;
    {$ELSE}
    pa := Host^.h_address_list^;
    {$ENDIF}
  {$ELSE}
    pa := Host^.h_addr_list^;
  {$ENDIF}

  {$IFDEF FPC}
    {$IFDEF MSWINDOWS}
    sa.S_un_b.s_b1 := pa[0];
    sa.S_un_b.s_b2 := pa[1];
    sa.S_un_b.s_b3 := pa[2];
    sa.S_un_b.s_b4 := pa[3];
    {$ELSE}
    sa.s_bytes[1] := Ord(pa[0]);
    sa.s_bytes[2] := Ord(pa[1]);
    sa.s_bytes[3] := Ord(pa[2]);
    sa.s_bytes[4] := Ord(pa[3]);
    {$ENDIF}
  {$ELSE}
    sa.S_un_b.s_b1 := Ord(pa[0]);
    sa.S_un_b.s_b2 := Ord(pa[1]);
    sa.S_un_b.s_b3 := Ord(pa[2]);
    sa.S_un_b.s_b4 := Ord(pa[3]);
  {$ENDIF}
    result := TInAddrToString(sa);
  end else
    error:= 'Cant resolve '+AHostName;
end;


function slResolve(host: String; var error: String): String;
begin
  // Sometimes 95 forgets who localhost is
  if AnsiSameText(host, 'LOCALHOST') then begin    {Do not Localize}
    result := '127.0.0.1';    {Do not Localize}
  end else if IsIP(host) then begin
    result := host;
  end else begin
    if 0 = Pos('.', host) then begin
      result := slConvertIp(host);
    end else begin
      result := slGetHostByName(host, error);
    end;
  end;
end;

function slConvertIp(host: String): String;
var lip:LongWord;
begin
  host:=ReplaceText(host, '0x', '$'); // if the string is Hex we need to replace the 0x with $
  lip:=StrToInt64Def(host, -1);
  Result := Format('%d.%d.%d.%d', [(lip shr 24), (lip shr 16) and $FF,(lip shr 8) and $FF, lip and $FF]);
end;

function slGetHostName: {$IFDEF UNICODE}RawByteString{$ELSE}AnsiString{$ENDIF};
begin
  {$IFDEF MSWINDOWS}
    SetLength(result, 250);
    GetHostName(PAnsiChar(result), Length(result));
    Result := PAnsiChar(result);
  {$ELSE}
    Result := GetHostName;
  {$ENDIF}
end;

function PopulateLocalAddresses(l: TStringList; var error: String): Boolean;
type
  TaPInAddr = Array[0..250] of PInAddr;
  PaPInAddr = ^TaPInAddr;
var
  i: integer;
  AHost: PHostEnt;
  PAdrPtr: PaPInAddr;
begin
  Result := True; //TODO: shouldn't this be false? And what about returning an error?
  l.Clear ;
  AHost := GetHostByName(PAnsiChar(slGetHostName));
  if AHost <> nil then
  begin
    {$IFDEF FPC}
      PAdrPtr := PAPInAddr(AHost^.h_addr_list);
    {$ELSE}
      {$IFDEF MSWINDOWS}
      PAdrPtr := PAPInAddr(AHost^.h_address_list);
      {$ELSE}
      PAdrPtr := PAPInAddr(AHost^.h_addr_list);
      {$ENDIF}
    {$ENDIF}
    i := 0;
    while PAdrPtr^[i] <> nil do
    begin
      l.Add(TInAddrToString(PAdrPtr^[I]^));
      Inc(I);
    end;
    l.Add('127.0.0.1');
    Result := True;
  end;
// else
//    error:= 'Cant query local addresses';
end;




function slBind(var slSocket: TslSocket; ip: String; port: Word; var error: String): Boolean;
var i: Integer;
    Addr: SockAddr;
begin
  Result:= False;

  Addr.sin_family := AF_INET;
  if ip = '' then
    Addr.sin_addr.s_addr := INADDR_ANY
  else
    TranslateStringToTInAddr(ip, Addr.sin_addr);

  Addr.sin_port:= htons(port);

{$IFDEF FPC}
  i:= fpbind(slSocket.socket, @Addr, SizeOf(Addr));
{$ELSE}
{$IFDEF MSWINDOWS}
  i:= bind(slSocket.socket, @Addr, SizeOf(Addr));
{$ELSE}
  i:= bind(slSocket.socket, Addr, SizeOf(Addr));
{$ENDIF}
{$ENDIF}
  if 0 <> i then
  begin
    error:= 'bind to '+ip+':'+IntToStr(port)+' failed: '+IntToStr(i);
    exit;
  end;

  Result:= True;
end;

function slGetSocket(var slSocket: TslSocket; udp: Boolean; var error: String): Boolean;
begin
  Result:= False;

{$IFDEF FPC}
  if udp then
    slSocket.socket:= fpsocket(AF_INET, SOCK_DGRAM, 0)
  else
    slSocket.socket:= fpsocket(AF_INET, SOCK_STREAM, 0);
{$ELSE}
  if udp then
    slSocket.socket:= socket(AF_INET, SOCK_DGRAM, 0)
  else
    slSocket.socket:= socket(AF_INET, SOCK_STREAM, 0);
{$ENDIF}

  if slSocket.socket = slSocketError then
  begin
    error:= 'cant get a new socket?! '+slLastError;
    exit;
  end;

  Result:= True;
end;


{$IFDEF FPC}
{$IFNDEF MSWINDOWS}
const EINPROGRESS = ESYSEINPROGRESS;
{$ENDIF}
{$ENDIF}

function slConnect(socket: TslSocket; host: String; port: Integer; var error: String): Integer;
var
{$IFDEF MSWINDOWS}
  Addr: TSockAddrIn;
{$ELSE}
  Addr: SockAddr;
{$ENDIF}
  rc: Integer;
begin
  Result:= -1; // -1 vegleges hiba, 0 siker, 1 selectelni kell meg!
  socket.peerip:= slResolve(host, error);
  if error <> '' then exit;

  Addr.sin_family := AF_INET;
  TranslateStringToTInAddr(socket.peerip, Addr.sin_addr);
  Addr.sin_port := HToNS(port);


{$IFDEF FPC}
  rc := fpconnect(socket.socket, @addr, sizeof(addr));
{$ELSE}
{$IFDEF MSWINDOWS}
  rc := connect(socket.socket, @addr, sizeof(addr));
{$ELSE}
  rc := connect(socket.socket, addr, sizeof(addr));
{$ENDIF}
{$ENDIF}
  if rc < 0 then
  begin
    error:= slLastError;
{$IFDEF MSWINDOWS}
    rc:= WSAGetLastError();
    if rc = 10035 then
      Result:= 1;
{$ELSE}
    rc:= errno;
    if rc = EINPROGRESS then
    begin
      slDebug('connect returned einprogress');
      Result:= 1;
    end;
{$ENDIF}
  end
  else if rc = 0 then
    Result:= 0
  else
    Result:= 1;

end;

function slLastError(err: Integer): String;
begin
{$IFDEF MSWINDOWS}
  Result:= SysUtils.SysErrorMessage(err);
{$ELSE}
  {$IFDEF FPC}
    Result:= SysErrorMessage(err);
  {$ELSE}
    Result:= StrPas(strerror(err));
  {$ENDIF}
{$ENDIF}
end;

function slLastError: String;
begin
  Result:= slLastError(slLastErrno);
end;
function slLastErrno: Integer;
begin
{$IFDEF MSWINDOWS}
  Result:= WSAGetLastError();
{$ELSE}
  Result:= errno;
{$ENDIF}
end;


function slGetSockopt(slSocket: TslSocket; i2: Integer; var ret: Integer; var error: String): Boolean;
var
{$IFDEF MSWINDOWS}
l: Integer;
{$ELSE}
l: Cardinal;
{$ENDIF}
  rc: Integer;
begin
  Result:= False;
  l:= sizeof(Integer);
{$IFDEF FPC}
  if (0 > fpgetsockopt(slSocket.socket, SOL_SOCKET, i2, PAnsiChar(@rc), @l)) then
{$ELSE}
  if (0 > getsockopt(slSocket.socket, SOL_SOCKET, i2, PAnsiChar(@rc), l)) then
{$ENDIF}
  begin
    error:= 'getsockopt failed: '+slLastError;
    exit;
  end;
  slDebug('getsockopt utan, '+IntToStr(rc));
  ret:= rc;
  Result:= True;
end;

function slSoError(slSocket: TslSocket; var error: String): String;
var rc: Integer;
begin
  error:= '';
  slGetSockopt(slSocket,SO_ERROR,rc,error);
  if rc <> 0 then
    error:= slLastError(rc);
  Result:= error;
end;

function slSetSockOpt(slSocket: TslSocket; i: Integer; rc: Integer; var error: String): Boolean;
begin
  Result:= False;

{$IFDEF FPC}
  if (0 <> fpsetsockopt(slSocket.socket, SOL_SOCKET, i, @rc, SizeOf(Integer))) then
{$ELSE}
{$IFDEF MSWINDOWS}
  if (0 <> setsockopt(slSocket.socket, SOL_SOCKET, i, PAnsiChar(@rc), SizeOf(Integer))) then
{$ELSE}
  if (0 <> setsockopt(slSocket.socket, SOL_SOCKET, i, @rc, SizeOf(Integer))) then
{$ENDIF}
{$ENDIF}
  begin
    error:= 'setsockopt failed';
    exit;
  end;
  Result:= True;
end;

function slSetKeepalive(slSocket: TslSocket; alive: Boolean;var error: String): Boolean;
begin
  Result:= slSetSockOpt(slSocket, so_keepalive, Integer(alive), error);
end;

function slGetKeepalive(slSocket: TslSocket; var alive: Boolean;var error: String): Boolean;
var ret: Integer;
begin
  Result:= slGetSockOpt(slSocket, so_keepalive, ret, error);
  alive:= Boolean(ret);
end;

{$IFDEF FPC}
{$IFNDEF MSWINDOWS}
function slSelect(var slSocket: TslSocket; timeout: Integer; shouldread, shouldwrite: Boolean; var error: String): Boolean;
var writefds,readfds,exceptfds: TFDSet;
    tv: TTimeVal;
    rc: Integer;
begin
  Result:= False;
  try
    if slsocket.socket = slsocketerror then exit;

    fpFD_ZERO(writefds);
    fpFD_ZERO(readfds);
    fpFD_ZERO(exceptfds);
    if shouldwrite then
    begin
      fpFD_SET(slSocket.socket, writefds);
      fpFD_SET(slSocket.socket, exceptfds);
    end;
    if shouldread then
      fpFD_SET(slSocket.socket, readfds);


    //timeout in milliseconds.
    tv.tv_sec:= timeout div 1000;
    tv.tv_usec:= (timeout - tv.tv_sec * 1000) * 1000;
    rc:= fpselect(slSocket.socket+1, @readfds, @writefds, @exceptfds, @tv);
    if (0 > rc) then
    begin
      error:= 'select returned error: '+slLastError();
      exit;
    end;

    if (0 < fpFD_ISSET(slSocket.socket,writefds)) then
      slDebug('writefds');
    if (0 < fpFD_ISSET(slSocket.socket,readfds)) then
      slDebug('readfds');
    if (0 < fpFD_ISSET(slSocket.socket,exceptfds)) then
      slDebug('exceptfds');

    Result:= True;
    if ((shouldread) and (0 = fpFD_ISSET(slSocket.socket,readfds))) then
      Result:= False;
    if ((shouldwrite) and (0 = fpFD_ISSET(slSocket.socket,writefds))) then
      Result:= False;

    if ((not Result) and (0 < fpfd_isset(slSocket.socket, exceptfds))) then
    begin
      error:= slSoError(slSocket, error);
      if error <> '' then
        exit;
    end;

    if not Result then
      error:= 'timeout'
    else
      error:= '';
  except
    on e: Exception do
    begin
      Debug(dpError, 'slstack', Format('[EXCEPTION] slSelect: %s', [e.Message]));
      Result:= False;
    end;
  end;
end;

function slSelect(var slSocket1, slSocket2: TslSocket; timeout: Integer; shouldread, shouldwrite: Boolean; var error: String): Integer;
var writefds,readfds,exceptfds: TFDSet;
    tv: TTimeVal;
    rc: Integer;
    m: TSocket;
begin
  Result:= 0;
  try
    if slsocket1.socket = slsocketerror then exit;
    if slsocket2.socket = slsocketerror then exit;

    fpFD_ZERO(writefds);
    fpFD_ZERO(readfds);
    fpFD_ZERO(exceptfds);
    if shouldwrite then
    begin
      fpFD_SET(slSocket1.socket, writefds);
      fpFD_SET(slSocket2.socket, writefds);
    end;
    if shouldread then
    begin
      fpFD_SET(slSocket1.socket, readfds);
      fpFD_SET(slSocket2.socket, readfds);
    end;
    fpFD_SET(slSocket1.socket, exceptfds);
    fpFD_SET(slSocket2.socket, exceptfds);

    m:= slSocket1.socket;
    if slSocket2.socket > m then
      m:= slSocket2.socket;

    //timeout in milliseconds.
    tv.tv_sec:= timeout div 1000;
    tv.tv_usec:= (timeout - tv.tv_sec * 1000) * 1000;
    rc:= fpselect(m+1, @readfds, @writefds, @exceptfds, @tv);
    if (0 > rc) then
    begin
      error:= 'select returned error: '+slLastError();
      exit;
    end;

    if (0 < fpFD_ISSET(slSocket1.socket,writefds)) then
      slDebug('writefds 1');
    if (0 < fpFD_ISSET(slSocket1.socket,readfds)) then
      slDebug('readfds 1');
    if (0 < fpFD_ISSET(slSocket1.socket,exceptfds)) then
      slDebug('exceptfds 1');

    if (0 < fpFD_ISSET(slSocket2.socket,writefds)) then
      slDebug('writefds 2');
    if (0 < fpFD_ISSET(slSocket2.socket,readfds)) then
      slDebug('readfds 2');
    if (0 < fpFD_ISSET(slSocket2.socket,exceptfds)) then
      slDebug('exceptfds 2');

    if 0 < fpfd_isset(slSocket1.socket, exceptfds) then
    begin
      error:= slSoError(slSocket1, error);
      exit;
    end;
    if 0 < fpfd_isset(slSocket2.socket, exceptfds) then
    begin
      error:= slSoError(slSocket2, error);
      exit;
    end;

    inc(Result);
    if ((shouldread) and (0= fpFD_ISSET(slSocket1.socket,readfds))) then
      dec(Result);
    if ((shouldwrite) and (0= fpFD_ISSET(slSocket1.socket,writefds))) then
      dec(Result);

    inc(Result, 2);
    if ((shouldread) and (0= fpFD_ISSET(slSocket2.socket,readfds))) then
      dec(Result, 2);
    if ((shouldwrite) and (0= fpFD_ISSET(slSocket2.socket,writefds))) then
      dec(Result, 2);

    if Result = 0 then
      error:= 'timeout'
    else
      error:= '';
  except
    on e: Exception do
    begin
      Debug(dpError, 'slstack', Format('[EXCEPTION] slSelect: %s', [e.Message]));
      Result:= 0;
    end;
  end;
end;

{$ELSE}
function slSelect(var slSocket: TslSocket; timeout: Integer; shouldread, shouldwrite: Boolean; var error: String): Boolean;
var writefds,readfds,exceptfds: TFDSet;
    tv: TTimeVal;
    rc: Integer;
begin
  Result:= False;
  try
    if slsocket.socket = slsocketerror then exit;

    FD_ZERO(writefds);
    FD_ZERO(readfds);
    FD_ZERO(exceptfds);
    if shouldwrite then
    begin
      FD_SET(slSocket.socket, writefds);
      FD_SET(slSocket.socket, exceptfds);
    end;
    if shouldread then
      FD_SET(slSocket.socket, readfds);


    //timeout in milliseconds.
    tv.tv_sec:= timeout div 1000;
    tv.tv_usec:= (timeout - tv.tv_sec * 1000) * 1000;
    rc:= select(slSocket.socket+1, @readfds, @writefds, @exceptfds, @tv);
    if (0 > rc) then
    begin
      error:= 'select returned error: '+slLastError();
      exit;
    end;

    if (FD_ISSET(slSocket.socket,writefds)) then
      slDebug('writefds');
    if (FD_ISSET(slSocket.socket,readfds)) then
      slDebug('readfds');
    if (FD_ISSET(slSocket.socket,exceptfds)) then
      slDebug('exceptfds');

    Result:= True;
    if ((shouldread) and (not FD_ISSET(slSocket.socket,readfds))) then
      Result:= False;
    if ((shouldwrite) and (not FD_ISSET(slSocket.socket,writefds))) then
      Result:= False;

    if ((not Result) and (fd_isset(slSocket.socket, exceptfds))) then
    begin
      error:= slSoError(slSocket, error);
      if error <> '' then
        exit;
    end;

    if not Result then
      error:= 'timeout'
    else
      error:= '';
  except
    on e: Exception do
    begin
      Debug(dpError, 'slstack', Format('[EXCEPTION] slSelect: %s', [e.Message]));
      Result:= False;
    end;
  end;
end;

function slSelect(var slSocket1, slSocket2: TslSocket; timeout: Integer; shouldread, shouldwrite: Boolean; var error: String): Integer;
var writefds,readfds,exceptfds: TFDSet;
    tv: TTimeVal;
    rc: Integer;
    m: TSocket;
begin
  Result:= 0;
  try
    if slsocket1.socket = slsocketerror then exit;
    if slsocket2.socket = slsocketerror then exit;

    FD_ZERO(writefds);
    FD_ZERO(readfds);
    FD_ZERO(exceptfds);
    if shouldwrite then
    begin
      FD_SET(slSocket1.socket, writefds);
      FD_SET(slSocket2.socket, writefds);
    end;
    if shouldread then
    begin
      FD_SET(slSocket1.socket, readfds);
      FD_SET(slSocket2.socket, readfds);
    end;
    FD_SET(slSocket1.socket, exceptfds);
    FD_SET(slSocket2.socket, exceptfds);

    m:= slSocket1.socket;
    if slSocket2.socket > m then
      m:= slSocket2.socket;

    //timeout in milliseconds.
    tv.tv_sec:= timeout div 1000;
    tv.tv_usec:= (timeout - tv.tv_sec * 1000) * 1000;
    rc:= select(m+1, @readfds, @writefds, @exceptfds, @tv);
    if (0 > rc) then
    begin
      error:= 'select returned error: '+slLastError();
      exit;
    end;

    if (FD_ISSET(slSocket1.socket,writefds)) then
      slDebug('writefds 1');
    if (FD_ISSET(slSocket1.socket,readfds)) then
      slDebug('readfds 1');
    if (FD_ISSET(slSocket1.socket,exceptfds)) then
      slDebug('exceptfds 1');

    if (FD_ISSET(slSocket2.socket,writefds)) then
      slDebug('writefds 2');
    if (FD_ISSET(slSocket2.socket,readfds)) then
      slDebug('readfds 2');
    if (FD_ISSET(slSocket2.socket,exceptfds)) then
      slDebug('exceptfds 2');

    if fd_isset(slSocket1.socket, exceptfds) then
    begin
      error:= slSoError(slSocket1, error);
      exit;
    end;
    if fd_isset(slSocket2.socket, exceptfds) then
    begin
      error:= slSoError(slSocket2, error);
      exit;
    end;

    inc(Result);
    if ((shouldread) and (not FD_ISSET(slSocket1.socket,readfds))) then
      dec(Result);
    if ((shouldwrite) and (not FD_ISSET(slSocket1.socket,writefds))) then
      dec(Result);

    inc(Result, 2);
    if ((shouldread) and (not FD_ISSET(slSocket2.socket,readfds))) then
      dec(Result, 2);
    if ((shouldwrite) and (not FD_ISSET(slSocket2.socket,writefds))) then
      dec(Result, 2);

    if Result = 0 then
      error:= 'timeout'
    else
      error:= '';
  except
    on e: Exception do
    begin
      Debug(dpError, 'slstack', Format('[EXCEPTION] slSelect: %s', [e.Message]));
      Result:= 0;
    end;
  end;
end;

{$ENDIF}
{$ELSE}
function slSelect(var slSocket: TslSocket; timeout: Integer; shouldread, shouldwrite: Boolean; var error: String): Boolean;
var writefds,readfds,exceptfds: TFDSet;
    tv: TTimeVal;
    rc: Integer;
begin
  Result:= False;
  try
    if slsocket.socket = slsocketerror then exit;

    FD_ZERO(writefds);
    FD_ZERO(readfds);
    FD_ZERO(exceptfds);
    if shouldwrite then
    begin
      FD_SET(slSocket.socket, writefds);
      FD_SET(slSocket.socket, exceptfds);
    end;
    if shouldread then
      FD_SET(slSocket.socket, readfds);


    //timeout in milliseconds.
    tv.tv_sec:= timeout div 1000;
    tv.tv_usec:= (timeout - tv.tv_sec * 1000) * 1000;
    rc:= select(slSocket.socket+1, @readfds, @writefds, @exceptfds, @tv); //@exceptfds
    if (0 > rc) then
    begin
      error:= 'select returned error: '+slLastError();
      exit;
    end;

    if (FD_ISSET(slSocket.socket,writefds)) then
      slDebug('writefds');
    if (FD_ISSET(slSocket.socket,readfds)) then
      slDebug('readfds');
    if (FD_ISSET(slSocket.socket,exceptfds)) then
      slDebug('exceptfds');


    Result:= True;
    if ((shouldread) and (not FD_ISSET(slSocket.socket,readfds))) then
      Result:= False;
    if ((shouldwrite) and (not FD_ISSET(slSocket.socket,writefds))) then
      Result:= False;

    if ((not Result) and (fd_isset(slSocket.socket, exceptfds))) then
    begin
      error:= slSoError(slSocket, error);
      if error <> '' then
        exit;
    end;

    if not Result then
      error:= 'timeout'
    else
      error:= '';
  except
    on e: Exception do
    begin
      Debug(dpError, 'slstack', Format('[EXCEPTION] slSelect: %s', [e.Message]));
      Result:= False;
    end;
  end;
end;

function slSelect(var slSocket1, slSocket2: TslSocket; timeout: Integer; shouldread, shouldwrite: Boolean; var error: String): Integer;
var writefds,readfds,exceptfds: TFDSet;
    tv: TTimeVal;
    rc: Integer;
    m: TSocket;
begin
  Result:= 0;
  try
    if slsocket1.socket = slsocketerror then exit;
    if slsocket2.socket = slsocketerror then exit;

    FD_ZERO(writefds);
    FD_ZERO(readfds);
    FD_ZERO(exceptfds);
    if shouldwrite then
    begin
      FD_SET(slSocket1.socket, writefds);
      FD_SET(slSocket2.socket, writefds);
    end;
    if shouldread then
    begin
      FD_SET(slSocket1.socket, readfds);
      FD_SET(slSocket2.socket, readfds);
    end;
    FD_SET(slSocket1.socket, exceptfds);
    FD_SET(slSocket2.socket, exceptfds);

    m:= slSocket1.socket;
    if slSocket2.socket > m then
      m:= slSocket2.socket;

    //timeout in milliseconds.
    tv.tv_sec:= timeout div 1000;
    tv.tv_usec:= (timeout - tv.tv_sec * 1000) * 1000;
    rc:= select(m+1, @readfds, @writefds, @exceptfds, @tv);
    if (0 > rc) then
    begin
      error:= 'select returned error: '+slLastError();
      exit;
    end;

    if (FD_ISSET(slSocket1.socket,writefds)) then
      slDebug('writefds 1');
    if (FD_ISSET(slSocket1.socket,readfds)) then
      slDebug('readfds 1');
    if (FD_ISSET(slSocket1.socket,exceptfds)) then
      slDebug('exceptfds 1');

    if (FD_ISSET(slSocket2.socket,writefds)) then
      slDebug('writefds 2');
    if (FD_ISSET(slSocket2.socket,readfds)) then
      slDebug('readfds 2');
    if (FD_ISSET(slSocket2.socket,exceptfds)) then
      slDebug('exceptfds 2');

    if fd_isset(slSocket1.socket, exceptfds) then
    begin
      error:= slSoError(slSocket1, error);
      exit;
    end;
    if fd_isset(slSocket2.socket, exceptfds) then
    begin
      error:= slSoError(slSocket2, error);
      exit;
    end;

    inc(Result);
    if ((shouldread) and (not FD_ISSET(slSocket1.socket,readfds))) then
      dec(Result);
    if ((shouldwrite) and (not FD_ISSET(slSocket1.socket,writefds))) then
      dec(Result);

    inc(Result, 2);
    if ((shouldread) and (not FD_ISSET(slSocket2.socket,readfds))) then
      dec(Result, 2);
    if ((shouldwrite) and (not FD_ISSET(slSocket2.socket,writefds))) then
      dec(Result, 2);

    if Result = 0 then
      error:= 'timeout'
    else
      error:= '';
  except
    on e: Exception do
    begin
      Debug(dpError, 'slstack', Format('[EXCEPTION] slSelect: %s', [e.Message]));
      Result:= 0;
    end;
  end;
end;
{$ENDIF}

function slRecv(slSocket: TslSocket; var buffer; bufsize: Integer; var error: String): Integer;
begin
  Result := -1;
  try
    if slsocket.socket = slsocketerror then
      exit;

    {$IFDEF FPC}
      Result := fprecv(slSocket.socket, @buffer, bufsize, {$IFDEF MSWINDOWS}0{$ELSE}MSG_NOSIGNAL{$ENDIF});
    {$ELSE}
      Result := recv(slSocket.socket, buffer, bufsize, {$IFDEF MSWINDOWS}0{$ELSE}MSG_NOSIGNAL{$ENDIF});
    {$ENDIF}
    if Result <= 0 then
    begin
      error := 'recv failed: ' + slLastError;
      if 0 < Pos('Operation now in progress', error) then
        error := 'Connection lost';
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, 'slstack', Format('[EXCEPTION] slRecv: %s', [e.Message]));
      Result := -1;
    end;
  end;
end;

function slRecv(ssl: PSSL; var buffer; bufsize: Integer; var error: String): Integer;
begin
  try
    Result := SSL_read(ssl, PAnsiChar(@buffer), bufsize);
    if Result <= 0 then
    begin
      error := 'sslread failed: ' + slSSL_LastError(ssl, Result);
      if 0 < Pos('zero return', error) then
        error := 'Connection lost';
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, 'slstack', Format('[EXCEPTION] slRecv: %s', [e.Message]));
      Result := -1;
    end;
  end;
end;

function slSend(ssl: PSSL; var buffer; bufsize: Integer; var error: String): Boolean;
var
  rc: Integer;
begin
  Result := True;
  try
    rc := SSL_write(ssl, PAnsiChar(@buffer), bufsize);
    if rc <= 0 then
    begin
      error := 'sslwrite failed: ' + slSSL_LastError(ssl, rc);
      Result := False;
    end
    else if rc < bufsize then
    begin
      error := 'Couldnt send all the data';
      Result := False;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, 'slstack', Format('[EXCEPTION] slSend: %s', [e.Message]));
      Result := False;
    end;
  end;
end;

function slSend(slSocket: TslSocket; var buffer; bufsize: Integer; var error: String): Boolean;
var
  rc: Integer;
begin
  Result := False;
  try
    if slsocket.socket = slsocketerror then
      exit;

    Result := True;
    {$IFDEF FPC}
      rc := fpsend(slSocket.socket, @buffer, bufsize, {$IFDEF MSWINDOWS}0{$ELSE}MSG_NOSIGNAL{$ENDIF});
    {$ELSE}
      rc := send(slSocket.socket, buffer, bufsize, {$IFDEF MSWINDOWS}0{$ELSE}MSG_NOSIGNAL{$ENDIF});
    {$ENDIF}
    if rc <= 0 then
    begin
      error := 'send failed: '+ slLastError;
      Result := False;
    end
    else if rc < bufsize then
    begin
      error := 'Couldnt send all the data';
      Result := False;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, 'slstack', Format('[EXCEPTION] slSend: %s', [e.Message]));
      Result := False;
    end;
  end;
end;


function slListen(slSocket: TslSocket; backlog: Integer; var error: String): Boolean;
begin
  Result:= False;
{$IFDEF FPC}
  if 0 > fpListen(slSocket.socket, backlog) then
{$ELSE}
  if 0 > Listen(slSocket.socket, backlog) then
{$ENDIF}
  begin
    error:= slLastError;
    exit;
  end;
  Result:= True;
end;

function slAccept(var listenSocket, newSocket: TslSocket; var error: String): Boolean;
var
  i: Integer;
  Addr: TSockAddr;
begin
  Result:= False;
  i := SizeOf(addr);
{$IFDEF FPC}
  newSocket.socket := fpAccept(listenSocket.socket, @addr, @i);
{$ELSE}
  newSocket.socket := Accept(listenSocket.socket, @addr, @i);
{$ENDIF}
  if newSocket.socket > 0 then
  begin
    newSocket.peerip := TInAddrToString(Addr.sin_addr);
    newSocket.peerport := NToHs(Addr.sin_port);
    slGetSockName(newSocket);
    Result:= True;
  end else
    error:= slLastError;
end;

function slHToNs(port: Integer): Integer;
begin
  Result:= htons(port);
end;

function slGetSockName(var socket: TslSocket): Boolean;
var
{$IFDEF MSWINDOWS}
  i: Integer;
{$ELSE}
  i: Cardinal;
{$ENDIF}
{$IFDEF FPC}
  LAddr: TSockAddr;
{$ELSE}
  LAddr: TSockAddrIn;
{$ENDIF}
begin
  i := SizeOf(LAddr);
{$IFDEF FPC}
    Result:= 0 = fpGetSockName(socket.socket, @LAddr, @i);
{$ELSE}
  {$IFDEF MSWINDOWS}
    Result:= 0 = GetSockName(socket.socket, @LAddr, i);
  {$ELSE}
    Result:= 0 = GetSockName(socket.socket, LAddr, i);
  {$ENDIF}
{$ENDIF}

  socket.localip := TInAddrToString(LAddr.sin_addr);
  socket.localport := Ntohs(LAddr.sin_port);
end;

procedure slDebug(s: String);
begin
//   WriteLn(s);
end;

{$IFNDEF MSWINDOWS}

procedure slPipeHandler(sig :longint);cdecl;
begin
// dummy.
end;

procedure  InstallSigPipeHandler;
begin
{$IFDEF FPC}
  fpSignal(SIGPIPE, @slPipeHandler);
{$ELSE}
  Signal(SIGPIPE, @slPipeHandler);
{$ENDIF}
end;
{$ENDIF}

initialization
{$IFNDEF MSWINDOWS}
  InstallSigPipeHandler;
{$ENDIF}
end.
