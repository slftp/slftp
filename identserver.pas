unit identserver;

interface

uses
  Classes;

(*
  Ident Server (RFC 1413) implementation using mORMot2 sockets.

  A request:  56646,13307
  B answer:   56646,13307 : USERID : UNIX : slftpuser
*)

type
  { @abstract(Ident server thread for replying to ident requests on port 113) }
  TIdentServerThread = class(TThread)
  private
    FDefaultIdentResponse: String; //< default ident response from inifile
    FPort: Integer; //< port to listen on (default 113)
    FActive: Boolean; //< flag to indicate if server is running

    { Tries to find the site's ident by iterating all siteslots to find the appropriate connection by IP and Port
      @param(aPeerIP IP address from server who requests ident)
      @param(aPeerPort Port from server who requests ident)
      @returns(Sites ident string if configured, otherwise default ident from config) }
    function FindSiteIdent(const aPeerIP: String; const aPeerPort: Integer): String;

    { Parse the ident request and extract server and client ports
      @param(aRequest The raw request line)
      @param(aServerPort Output: server port)
      @param(aClientPort Output: client port)
      @returns(True if parsing was successful) }
    function ParseIdentRequest(const aRequest: String; out aServerPort, aClientPort: Integer): Boolean;

    { Build the ident response string
      @param(aServerPort Server port from request)
      @param(aClientPort Client port from request)
      @param(aIdentReply The ident username to return)
      @returns(Formatted ident response) }
    function BuildIdentResponse(aServerPort, aClientPort: Integer; const aIdentReply: String): String;
  protected
    procedure Execute; override;
  public
    { Creates an ident server thread on specified port (default 113) }
    constructor Create(aPort: Integer = 113);

    property DefaultIdentResponse: String read FDefaultIdentResponse;
    property Port: Integer read FPort;
    property Active: Boolean read FActive;
  end;

{ Just a helper function to initialize @link(IdentServer) if enabled in config
  @returns(Exception string on failure, otherwise empty string) }
function IdentServerInit: String;
{ Just a helper function to free @link(IdentServer) }
procedure IdentServerStop;

implementation

uses
  SysUtils, configunit, sitesunit, debugunit,
  mormot.core.base, mormot.net.sock;

const
  section = 'ident';
  IDENT_TIMEOUT_MS = 5000; // 5 second timeout for ident connections

var
  glMyIdentServer: TIdentServerThread = nil;

{ TIdentServerThread }

constructor TIdentServerThread.Create(aPort: Integer = 113);
begin
  inherited Create(True); // create suspended to avoid race condition
  FreeOnTerminate := False;
  FPort := aPort;
  FActive := False;
  FDefaultIdentResponse := config.ReadString(section, 'response', 'slftpuser');
  Start; // now start the thread with all fields properly initialized
end;

function TIdentServerThread.FindSiteIdent(const aPeerIP: String; const aPeerPort: Integer): String;
var
  i, j: Integer;
  s: TSite;
  ss: TSiteSlot;
begin
  Result := FDefaultIdentResponse;
  for i := 0 to sites.Count - 1 do
  begin
    s := TSite(sites[i]);
    for j := 0 to s.slots.Count - 1 do
    begin
      ss := TSiteSlot(s.slots[j]);
      if ((ss.peerport = aPeerPort) and (ss.peerip = aPeerIP)) then
      begin
        Result := ss.GetIdentReply;
        exit;
      end;
    end;
  end;
end;

function TIdentServerThread.ParseIdentRequest(const aRequest: String; out aServerPort, aClientPort: Integer): Boolean;
var
  fCommaPos: Integer;
  fServerPortStr, fClientPortStr: String;
begin
  Result := False;
  aServerPort := 0;
  aClientPort := 0;

  // Request format: "serverport,clientport" or "serverport, clientport"
  fCommaPos := Pos(',', aRequest);
  if fCommaPos = 0 then
    exit;

  fServerPortStr := Trim(Copy(aRequest, 1, fCommaPos - 1));
  fClientPortStr := Trim(Copy(aRequest, fCommaPos + 1, Length(aRequest)));

  // Remove any trailing CR/LF
  fClientPortStr := Trim(fClientPortStr);

  aServerPort := StrToIntDef(fServerPortStr, 0);
  aClientPort := StrToIntDef(fClientPortStr, 0);

  Result := (aServerPort > 0) and (aServerPort <= 65535) and
            (aClientPort > 0) and (aClientPort <= 65535);
end;

function TIdentServerThread.BuildIdentResponse(aServerPort, aClientPort: Integer; const aIdentReply: String): String;
begin
  // Response format: "serverport, clientport : USERID : UNIX : username"
  Result := Format('%d, %d : USERID : UNIX : %s'#13#10, [aServerPort, aClientPort, aIdentReply]);
end;

procedure TIdentServerThread.Execute;
var
  fListenSock: TCrtSocket;
  fClientSock: TNetSocket;
  fClientAddr: TNetAddr;
  fBuf: array[0..255] of AnsiChar;
  fBufLen, fSendLen: Integer;
  fRequest: String;
  fServerPort, fClientPort: Integer;
  fPeerIP: RawUtf8;
  fIdentReply: String;
  fResponse: AnsiString;
  fResult: TNetResult;
  fEvents: TNetEvents;
  fSockHandle: PtrInt;
begin
  Debug(dpError, section, Format('IDENT DEBUG: Execute starting, FPort=%d, FDefaultIdentResponse="%s"', [FPort, FDefaultIdentResponse]));

  fListenSock := nil;
  try
    // Create listening socket on ident port
    fListenSock := TCrtSocket.Bind(RawUtf8(IntToStr(FPort)), nlTcp, IDENT_TIMEOUT_MS, True);
    fSockHandle := fListenSock.Sock.Socket;
    FActive := True;
    Debug(dpError, section, Format('IDENT DEBUG: Bind successful, listening on port %d, socket fd=%d', [FPort, fSockHandle]));

    while not Terminated do
    begin
      // Accept incoming connection with timeout
      fResult := fListenSock.Sock.Accept(fClientSock, fClientAddr, {async=}False);

      if Terminated then
        Break;

      if fResult = nrRetry then
        Continue;

      if fResult <> nrOk then
      begin
        Debug(dpError, section, Format('IDENT DEBUG: Accept error: %d', [Ord(fResult)]));
        Continue;
      end;

      // Get peer IP from the accepted address
      fClientAddr.IP(fPeerIP);
      Debug(dpError, section, Format('IDENT DEBUG: Accepted connection from %s, client fd=%d', [fPeerIP, fClientSock.Socket]));

      try
        try
          // Wait for data to be available (timeout 5s)
          fEvents := fClientSock.WaitFor(IDENT_TIMEOUT_MS, [neRead, neError]);
          if neError in fEvents then
          begin
            Debug(dpError, section, Format('IDENT DEBUG: WaitFor returned error for %s', [fPeerIP]));
            Continue;
          end;
          if not (neRead in fEvents) then
          begin
            Debug(dpError, section, Format('IDENT DEBUG: WaitFor timeout (no data) from %s', [fPeerIP]));
            Continue;
          end;

          // Read request directly from the raw socket
          fBufLen := SizeOf(fBuf);
          fResult := fClientSock.Recv(@fBuf, fBufLen);
          Debug(dpError, section, Format('IDENT DEBUG: Recv result=%d, bytes=%d from %s', [Ord(fResult), fBufLen, fPeerIP]));

          if (fResult <> nrOk) or (fBufLen <= 0) then
          begin
            Debug(dpError, section, Format('IDENT DEBUG: Recv failed from %s', [fPeerIP]));
            Continue;
          end;

          // Convert buffer to string, strip CR/LF
          SetString(fRequest, PAnsiChar(@fBuf), fBufLen);
          fRequest := Trim(fRequest);
          Debug(dpError, section, Format('IDENT DEBUG: Raw request from %s: "%s" (%d bytes)', [fPeerIP, fRequest, fBufLen]));

          if ParseIdentRequest(fRequest, fServerPort, fClientPort) then
          begin
            Debug(dpError, section, Format('IDENT DEBUG: Parsed ports %d,%d from %s', [fServerPort, fClientPort, fPeerIP]));

            // Find the appropriate ident response
            fIdentReply := FindSiteIdent(string(fPeerIP), fServerPort);
            Debug(dpError, section, Format('IDENT DEBUG: FindSiteIdent returned "%s" for %s:%d', [fIdentReply, fPeerIP, fServerPort]));

            // Build and send response directly via raw socket
            fResponse := AnsiString(BuildIdentResponse(fServerPort, fClientPort, fIdentReply));
            fSendLen := Length(fResponse);
            Debug(dpError, section, Format('IDENT DEBUG: Sending response (%d bytes): "%s"', [fSendLen, Trim(string(fResponse))]));

            fResult := fClientSock.Send(pointer(fResponse), fSendLen);
            Debug(dpError, section, Format('IDENT DEBUG: Send result=%d, sent=%d bytes', [Ord(fResult), fSendLen]));
          end
          else
          begin
            Debug(dpError, section, Format('IDENT DEBUG: ParseIdentRequest FAILED for "%s" from %s', [fRequest, fPeerIP]));
          end;
        except
          on e: Exception do
            Debug(dpError, section, Format('IDENT DEBUG: EXCEPTION from %s: %s', [fPeerIP, e.Message]));
        end;
      finally
        fClientSock.ShutdownAndClose({rdwr=}false);
        Debug(dpError, section, Format('IDENT DEBUG: Connection closed for %s', [fPeerIP]));
      end;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('IDENT DEBUG: FATAL server error: %s', [e.Message]));
    end;
  end;

  FActive := False;
  if fListenSock <> nil then
    fListenSock.Free;

  Debug(dpError, section, 'IDENT DEBUG: Server stopped');
end;

function IdentServerInit: String;
var
  fEnabled: Boolean;
begin
  Result := '';
  Debug(dpError, section, 'IDENT DEBUG: IdentServerInit called');
  try
    fEnabled := config.ReadBool(section, 'enabled', False);
    Debug(dpError, section, Format('IDENT DEBUG: config [ident] enabled = %s', [BoolToStr(fEnabled, True)]));

    if fEnabled then
    begin
      Debug(dpError, section, Format('IDENT DEBUG: Creating thread on port 113, default response = "%s"',
        [config.ReadString(section, 'response', 'slftpuser')]));
      glMyIdentServer := TIdentServerThread.Create(113);
      // Wait briefly for the server to start
      Sleep(100);
      if glMyIdentServer.Active then
        Debug(dpError, section, 'IDENT DEBUG: Server is ACTIVE after 100ms')
      else
        Debug(dpError, section, 'IDENT DEBUG: Server is NOT ACTIVE after 100ms');
    end
    else
      Debug(dpError, section, 'IDENT DEBUG: Ident server is DISABLED in config');
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('IDENT DEBUG: IdentServerInit EXCEPTION: %s', [e.Message]));
      Result := e.Message;
      if glMyIdentServer <> nil then
      begin
        glMyIdentServer.Terminate;
        glMyIdentServer.WaitFor;
        FreeAndNil(glMyIdentServer);
      end;
    end;
  end;
end;

procedure IdentServerStop;
begin
  if Assigned(glMyIdentServer) then
  begin
    glMyIdentServer.Terminate;
    glMyIdentServer.WaitFor;
    FreeAndNil(glMyIdentServer);
  end;
end;

end.
