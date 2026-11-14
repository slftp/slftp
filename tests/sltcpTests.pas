unit sltcpTests;

interface

uses
  {$IFDEF FPC}
    fpcunit, testregistry,
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility,
  {$ENDIF}
  slftpUnitTestsSetupIndyOpenSSL, sltcp;

type
  TTestSlTCP = class(TTestIndyOpenSSL)
  private
    FServer, FClient, FPeer: TslTCPSocket;
    { Writes aData from the peer and reads it with the client until all of it arrived }
    function SendAndRead(const aData: String): String;
    procedure CancelHandshake(aSocket: TslTCPSocket; var aShouldQuit: Boolean);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestReadReturnsOnlyCurrentData;
    procedure TestReadAfterBigRead;
    procedure TestTLSHandshakeTimeoutIsMeasured;
    procedure TestTLSHandshakeCancellationIsMeasured;
    procedure TestTLSHandshakeErrorIsMeasured;
  end;

implementation

uses
  SysUtils, slstack, sltlsstats, mormot.lib.openssl11;

{ TTestSlTCP }

procedure TTestSlTCP.SetUp;
var
  fAccepted: TslSocket;
begin
  inherited;
  CheckTrue(sltcp_inited, 'sltcp init failed: ' + sltcp_error);

  // loopback connection: listen on a free port, connect to it and accept
  FServer := TslTCPSocket.Create;
  CheckTrue(FServer.BindHost('127.0.0.1'), 'BindHost: ' + FServer.error);
  CheckTrue(FServer.GetSocket, 'GetSocket: ' + FServer.error);
  CheckTrue(FServer.Listen(1), 'Listen: ' + FServer.error);
  CheckTrue(slGetSockName(FServer.slSocket), 'slGetSockName failed');

  FClient := TslTCPSocket.Create;
  FClient.Host := '127.0.0.1';
  FClient.Port := FServer.slSocket.localport;
  CheckTrue(FClient.Connect, 'Connect: ' + FClient.error);

  CheckTrue(FServer.Accept(fAccepted), 'Accept: ' + FServer.error);
  FPeer := TslTCPSocket.Create;
  FPeer.SetupSocket(fAccepted);
end;

procedure TTestSlTCP.TearDown;
begin
  FPeer.Free;
  FClient.Free;
  FServer.Free;
  inherited;
end;

function TTestSlTCP.SendAndRead(const aData: String): String;
var
  fChunk: String;
begin
  CheckTrue(FPeer.Write(RawByteString(aData)), 'Write: ' + FPeer.error);
  Result := '';
  // a single Read returns at most slBufferSize bytes and may return less
  while Length(Result) < Length(aData) do
  begin
    CheckTrue(FClient.Read(fChunk, 5000), 'Read: ' + FClient.error);
    Result := Result + fChunk;
  end;
end;

procedure TTestSlTCP.TestReadReturnsOnlyCurrentData;
begin
  CheckEquals('230 User logged in, proceed.', SendAndRead('230 User logged in, proceed.'));
  // shorter than the previous response: nothing of it may be left over
  CheckEquals('200 OK', SendAndRead('200 OK'));
end;

procedure TTestSlTCP.TestReadAfterBigRead;
begin
  CheckEquals(StringOfChar('x', slBufferSize), SendAndRead(StringOfChar('x', slBufferSize)));
  CheckEquals('226 Done', SendAndRead('226 Done'));
end;

procedure TTestSlTCP.CancelHandshake(aSocket: TslTCPSocket; var aShouldQuit: Boolean);
begin
  aShouldQuit := True;
end;

procedure TTestSlTCP.TestTLSHandshakeTimeoutIsMeasured;
var
  fContext: PSSL_CTX;
begin
  fContext := SSL_CTX_new(TLS_client_method());
  CheckTrue(fContext <> nil);
  try
    // The accepted peer stays silent, so SSL_connect must wait and time out.
    CheckFalse(FClient.TurnToSSL(fContext, 30));
    CheckEquals(Ord(thoTimeout), Ord(FClient.LastTLSHandshake.Outcome));
    CheckTrue(FClient.LastTLSHandshake.ElapsedMs >= 30);
  finally
    SSL_CTX_free(fContext);
  end;
end;

procedure TTestSlTCP.TestTLSHandshakeCancellationIsMeasured;
var
  fContext: PSSL_CTX;
begin
  fContext := SSL_CTX_new(TLS_client_method());
  CheckTrue(fContext <> nil);
  try
    FClient.OnWaitingforSocket := CancelHandshake;
    CheckFalse(FClient.TurnToSSL(fContext, 5000));
    CheckEquals(Ord(thoCancelled), Ord(FClient.LastTLSHandshake.Outcome));
    CheckEquals('shouldquit', FClient.error);
  finally
    SSL_CTX_free(fContext);
  end;
end;

procedure TTestSlTCP.TestTLSHandshakeErrorIsMeasured;
var
  fContext: PSSL_CTX;
begin
  fContext := SSL_CTX_new(TLS_client_method());
  CheckTrue(fContext <> nil);
  try
    CheckTrue(FPeer.Write('HTTP/1.1 400 Bad Request' + #13#10#13#10));
    CheckFalse(FClient.TurnToSSL(fContext, 5000));
    CheckEquals(Ord(thoError), Ord(FClient.LastTLSHandshake.Outcome));
    CheckTrue(FClient.LastTLSHandshake.ElapsedMs >= 0);
  finally
    SSL_CTX_free(fContext);
  end;
end;

initialization
  {$IFDEF FPC}
    RegisterTest('sltcp', TTestSlTCP.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestSlTCP);
  {$ENDIF}
end.
