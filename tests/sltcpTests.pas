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
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestReadReturnsOnlyCurrentData;
    procedure TestReadAfterBigRead;
  end;

implementation

uses
  SysUtils, slstack;

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

initialization
  {$IFDEF FPC}
    RegisterTest('sltcp', TTestSlTCP.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestSlTCP);
  {$ENDIF}
end.
