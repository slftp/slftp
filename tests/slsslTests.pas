unit slsslTests;

interface

uses
  {$IFDEF FPC}
    TestFramework,
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility,
  {$ENDIF}
  slftpUnitTestsSetupIndyOpenSSL;

type
  TTestSlSSL = class(TTestIndyOpenSSL)
  published
    procedure TestGetOpenSSLVersion;
  end;

implementation

uses
  SysUtils, slssl;

{ TTestSlSSL }

procedure TTestSlSSL.TestGetOpenSSLVersion;
var
  fExpectedStr, fVersion: String;
begin
  //fExpectedStr := 'OpenSSL 3.5.0';
  fVersion := GetOpenSSLVersion; // e.g. OpenSSL 1.1.1f  31 Mar 2020 compiler: gcc -fPIC -pthread -m64 -Wa,--noexecstack ...
  CheckTrue(GetOpenSSLAvailable, 'OpenSSL version is wrong. Epected: ' + fExpectedStr + ' - loaded: ' + fVersion);
  // should also be quite long...
  //CheckTrue(Length(fVersion) > 35, 'OpenSSL version return seems wrong');
end;

initialization
  {$IFDEF FPC}
    RegisterTest('slssl', TTestSlSSL.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestSlSSL);
  {$ENDIF}
end.
