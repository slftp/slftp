unit slftpUnitTestsSetupIndyOpenSSL;

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
    {$ELSE}
      procedure SetUp; override;
    {$ENDIF}
  end;

implementation

uses
  SysUtils, mormot.lib.openssl11, mormot.core.os, slssl;

{ TTestIndyOpenSSL }

procedure TTestIndyOpenSSL.{$IFDEF FPC}SetUpOnce{$ELSE}SetUp{$ENDIF};
var 
  fError: String;
  fInitResult: Boolean;
begin
  fError := '';
  fInitResult := InitOpenSSL(fError);
  CheckTrue(fInitResult, 'Mormotssl initOpenSsl returned false: ' + fError);

  try
    CheckTrue(OpenSslIsAvailable, 'Mormotssl failed: ');
  except
    on e: Exception do
    begin
      {$IFNDEF FPC}DUnitX.Assert.Assert.{$ENDIF}Fail(Format('[EXCEPTION] Unexpected error while loading OpenSSL: %s%s %s%s', [sLineBreak, e.ClassName, sLineBreak, e.Message]));
    end;
  end;
end;

initialization
  {$IFDEF FPC}
    RegisterTest('Indy OpenSSL', TTestIndyOpenSSL.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestIndyOpenSSL);
  {$ENDIF}
end.
