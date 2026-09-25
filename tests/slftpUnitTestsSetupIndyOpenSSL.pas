unit slftpUnitTestsSetupIndyOpenSSL;

interface

uses
  {$IFDEF FPC}
    fpcunit, testregistry;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility, DUnitX.Assert;
  {$ENDIF}

type
  // base class which should be used whenever the Indy OpenSSL is needed
  TTestIndyOpenSSL = class(TTestCase)
  protected
    procedure SetUp; override;
  end;

implementation

uses
  SysUtils, mormot.lib.openssl11, mormot.core.os, slssl;

{$IFDEF FPC}
var
  // fpcunit has no SetUpOnce, so guard the setup manually: OpenSSL loading
  // is process-global anyway
  glOpenSSLSetupDone: Boolean = False;
{$ENDIF}

{ TTestIndyOpenSSL }

procedure TTestIndyOpenSSL.SetUp;
var
  fError: String;
  fInitResult: Boolean;
begin
  {$IFDEF FPC}
  if glOpenSSLSetupDone then
    Exit;
  glOpenSSLSetupDone := True;
  {$ENDIF}

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

end.
