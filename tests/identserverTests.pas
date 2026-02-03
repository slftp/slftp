unit identserverTests;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestIdentServer = class(TTestCase)
  published
    // ParseIdentRequest tests
    procedure TestParseIdentRequest_ValidRequest;
    procedure TestParseIdentRequest_ValidRequestWithSpaces;
    procedure TestParseIdentRequest_ValidRequestWithCRLF;
    procedure TestParseIdentRequest_InvalidNoComma;
    procedure TestParseIdentRequest_InvalidPortZero;
    procedure TestParseIdentRequest_InvalidPortTooHigh;
    procedure TestParseIdentRequest_InvalidNonNumeric;
    procedure TestParseIdentRequest_EmptyString;

    // BuildIdentResponse tests
    procedure TestBuildIdentResponse_Standard;
    procedure TestBuildIdentResponse_CustomUser;
    procedure TestBuildIdentResponse_MaxPorts;
  end;

implementation

uses
  SysUtils, identserver;

{ TTestIdentServer }

procedure TTestIdentServer.TestParseIdentRequest_ValidRequest;
var
  fServerPort, fClientPort: Integer;
  fResult: Boolean;
begin
  fResult := ParseIdentRequest('56646,13307', fServerPort, fClientPort);
  CheckTrue(fResult, 'ParseIdentRequest should return true for valid request');
  CheckEquals(56646, fServerPort, 'Server port should be 56646');
  CheckEquals(13307, fClientPort, 'Client port should be 13307');
end;

procedure TTestIdentServer.TestParseIdentRequest_ValidRequestWithSpaces;
var
  fServerPort, fClientPort: Integer;
  fResult: Boolean;
begin
  fResult := ParseIdentRequest('56646, 13307', fServerPort, fClientPort);
  CheckTrue(fResult, 'ParseIdentRequest should handle spaces');
  CheckEquals(56646, fServerPort, 'Server port should be 56646');
  CheckEquals(13307, fClientPort, 'Client port should be 13307');

  fResult := ParseIdentRequest('  56646  ,  13307  ', fServerPort, fClientPort);
  CheckTrue(fResult, 'ParseIdentRequest should handle extra spaces');
  CheckEquals(56646, fServerPort, 'Server port should be 56646');
  CheckEquals(13307, fClientPort, 'Client port should be 13307');
end;

procedure TTestIdentServer.TestParseIdentRequest_ValidRequestWithCRLF;
var
  fServerPort, fClientPort: Integer;
  fResult: Boolean;
begin
  fResult := ParseIdentRequest('56646,13307'#13#10, fServerPort, fClientPort);
  CheckTrue(fResult, 'ParseIdentRequest should handle CRLF');
  CheckEquals(56646, fServerPort, 'Server port should be 56646');
  CheckEquals(13307, fClientPort, 'Client port should be 13307');

  fResult := ParseIdentRequest('56646,13307'#10, fServerPort, fClientPort);
  CheckTrue(fResult, 'ParseIdentRequest should handle LF only');
  CheckEquals(56646, fServerPort, 'Server port should be 56646');
  CheckEquals(13307, fClientPort, 'Client port should be 13307');
end;

procedure TTestIdentServer.TestParseIdentRequest_InvalidNoComma;
var
  fServerPort, fClientPort: Integer;
  fResult: Boolean;
begin
  fResult := ParseIdentRequest('5664613307', fServerPort, fClientPort);
  CheckFalse(fResult, 'ParseIdentRequest should return false when no comma');
end;

procedure TTestIdentServer.TestParseIdentRequest_InvalidPortZero;
var
  fServerPort, fClientPort: Integer;
  fResult: Boolean;
begin
  fResult := ParseIdentRequest('0,13307', fServerPort, fClientPort);
  CheckFalse(fResult, 'ParseIdentRequest should reject port 0');

  fResult := ParseIdentRequest('56646,0', fServerPort, fClientPort);
  CheckFalse(fResult, 'ParseIdentRequest should reject port 0');
end;

procedure TTestIdentServer.TestParseIdentRequest_InvalidPortTooHigh;
var
  fServerPort, fClientPort: Integer;
  fResult: Boolean;
begin
  fResult := ParseIdentRequest('65536,13307', fServerPort, fClientPort);
  CheckFalse(fResult, 'ParseIdentRequest should reject port > 65535');

  fResult := ParseIdentRequest('56646,70000', fServerPort, fClientPort);
  CheckFalse(fResult, 'ParseIdentRequest should reject port > 65535');
end;

procedure TTestIdentServer.TestParseIdentRequest_InvalidNonNumeric;
var
  fServerPort, fClientPort: Integer;
  fResult: Boolean;
begin
  fResult := ParseIdentRequest('abc,13307', fServerPort, fClientPort);
  CheckFalse(fResult, 'ParseIdentRequest should reject non-numeric port');

  fResult := ParseIdentRequest('56646,xyz', fServerPort, fClientPort);
  CheckFalse(fResult, 'ParseIdentRequest should reject non-numeric port');
end;

procedure TTestIdentServer.TestParseIdentRequest_EmptyString;
var
  fServerPort, fClientPort: Integer;
  fResult: Boolean;
begin
  fResult := ParseIdentRequest('', fServerPort, fClientPort);
  CheckFalse(fResult, 'ParseIdentRequest should reject empty string');
end;

procedure TTestIdentServer.TestBuildIdentResponse_Standard;
var
  fResponse: String;
begin
  fResponse := BuildIdentResponse(56646, 13307, 'slftpuser');
  CheckEquals('56646, 13307 : USERID : UNIX : slftpuser'#13#10, fResponse, 'Response format mismatch');
end;

procedure TTestIdentServer.TestBuildIdentResponse_CustomUser;
var
  fResponse: String;
begin
  fResponse := BuildIdentResponse(21, 45678, 'customident');
  CheckEquals('21, 45678 : USERID : UNIX : customident'#13#10, fResponse, 'Response format mismatch');
end;

procedure TTestIdentServer.TestBuildIdentResponse_MaxPorts;
var
  fResponse: String;
begin
  fResponse := BuildIdentResponse(65535, 65535, 'maxport');
  CheckEquals('65535, 65535 : USERID : UNIX : maxport'#13#10, fResponse, 'Response format mismatch');
end;

initialization
  {$IFDEF FPC}
    RegisterTest(TTestIdentServer);
  {$ENDIF}

end.
