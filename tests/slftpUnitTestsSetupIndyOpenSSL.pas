﻿unit slftpUnitTestsSetupIndyOpenSSL;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility, DUnitX.Assert;
  {$ENDIF}

type
  // base class which should be used whenever the Indy OpenSSL is needed
  TTestIndyOpenSSL = class(TTestCase)
  protected
    {$IFDEF FPC}
      procedure SetUpOnce; override;
      procedure TeardownOnce; override;
    {$ELSE}
      procedure SetUp; override;
      procedure Teardown; override;
    {$ENDIF}
  published
    procedure OpenSSLVersion;
  end;

implementation

uses
  SysUtils, IdOpenSSLLoader, IdOpenSSLHeaders_crypto;

{ TTestIndyOpenSSL }

procedure TTestIndyOpenSSL.{$IFDEF FPC}SetUpOnce{$ELSE}SetUp{$ENDIF};
var
  fSslLoader: IOpenSSLLoader;
begin
  fSslLoader := IdOpenSSLLoader.GetOpenSSLLoader;
  // Tell Indy OpenSSL to load libs from current dir
  fSslLoader.OpenSSLPath := '.';

  try
    CheckTrue(fSslLoader.Load, 'IdOpenSSLLoader.Load failed failed: ' + fSslLoader.FailedToLoad.CommaText);
  except
    on e: Exception do
    begin
      {$IFNDEF FPC}DUnitX.Assert.Assert.{$ENDIF}Fail(Format('[EXCEPTION] Unexpected error while loading OpenSSL: %s%s %s%s', [sLineBreak, e.ClassName, sLineBreak, e.Message]));
    end;
  end;

  // check if we at least have OpenSSL 1.1.0
  CheckTrue(Assigned(OpenSSL_version), 'OpenSSL needs to be at least OpenSSL 1.1.0');
end;

procedure TTestIndyOpenSSL.{$IFDEF FPC}TeardownOnce{$ELSE}Teardown{$ENDIF};
var
  fSslLoader: IOpenSSLLoader;
begin
  try
    fSslLoader := IdOpenSSLLoader.GetOpenSSLLoader;
    fSslLoader.Unload;
  except
    on e: Exception do
    begin
      {$IFNDEF FPC}DUnitX.Assert.Assert.{$ENDIF}Fail(Format('Failed to unload OpenSSL: %s %s', [sLineBreak, e.Message]));
    end;
  end;
end;

procedure TTestIndyOpenSSL.OpenSSLVersion;
var
  fExpectedResultStr, fShortVersion: String;
  {$I slftp.inc}
begin
  fExpectedResultStr := OpenSSL_version(OPENSSL_VERSION_CONST); // e.g. OpenSSL 1.0.2n  7 Dec 2017
  fShortVersion := Copy(fExpectedResultStr, 9, 5);
  CheckEqualsString(lib_OpenSSL, fShortVersion, 'OpenSSL version is wrong');
  SetLength(fExpectedResultStr, 13);
  CheckEqualsString('OpenSSL ' + lib_OpenSSL, fExpectedResultStr, 'OpenSSL version string is wrong');
end;

initialization
  {$IFDEF FPC}
    RegisterTest('Indy OpenSSL', TTestIndyOpenSSL.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestIndyOpenSSL);
  {$ENDIF}
end.
