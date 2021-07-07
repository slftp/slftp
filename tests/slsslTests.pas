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
    procedure TestGetOpenSSLShortVersion;
  end;

implementation

uses
  SysUtils, slssl;

{ TTestSlSSL }

procedure TTestSlSSL.TestGetOpenSSLVersion;
var
  fExpectedStr, fVersion: String;
begin
  fExpectedStr := 'OpenSSL 1.1.1';
  fVersion := GetOpenSSLVersion; // e.g. OpenSSL 1.1.1f  31 Mar 2020 compiler: gcc -fPIC -pthread -m64 -Wa,--noexecstack ...
  CheckTrue(fVersion.Contains(fExpectedStr), 'OpenSSL version is wrong');
  // should also be quite long...
  CheckTrue(Length(fVersion) > 35, 'OpenSSL version return seems wrong');
end;

procedure TTestSlSSL.TestGetOpenSSLShortVersion;
var
  fExpectedStr, fShortVersion: String;
begin
  fExpectedStr := '1.1.1';
  fShortVersion := GetOpenSSLShortVersion; // e.g. OpenSSL 1.1.1f
  // remove letter from version
  SetLength(fShortVersion, Length(fShortVersion) - 1);
  CheckEqualsString(fExpectedStr, fShortVersion, 'OpenSSL version is wrong');
end;

initialization
  {$IFDEF FPC}
    RegisterTest('slssl', TTestSlSSL.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestSlSSL);
  {$ENDIF}
end.
