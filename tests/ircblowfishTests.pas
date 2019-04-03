unit ircblowfishTests;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestIRCblowfishSetup = class(TTestCase)
  protected
    {$IFDEF FPC}
      procedure SetUpOnce; override;
      procedure TeardownOnce; override;
    {$ELSE}
      procedure SetUp; override;
      procedure Teardown; override;
    {$ENDIF}
  end;

  TTestIRCblowfish = class(TTestIRCblowfishSetup)
  published
    procedure TestECB_irc_encrypt;
    procedure TestCBC_irc_encrypt;
  end;

implementation

uses
  SysUtils, ircblowfish;

{ TTestIRCblowfishSetup }

procedure TTestIRCblowfishSetup.{$IFDEF FPC}SetUpOnce{$ELSE}SetUp{$ENDIF};
var
  b: TIrcBlowKey;
begin
  // init
  IrcBlowfishInit;

  { add some channel keys }
  // ECB //
  b := irc_RegisterChannel('linknet', '#slftp-blowfish', 'ThisIsNOTsecure', '', False, False);
  b.names := ' ' + 'nothing' + ' ';
  b := irc_RegisterChannel('efnet', '#blowfishuser', 'googleme', 'securechannel', False, False);
  b.names := ' ' + 'Googling' + ' ';
  // CBC //
  b := irc_RegisterChannel('LinkNET', '#testsl', 'asdf1234', '', False, True);
  b.names := ' ' + 'ln' + ' ';
  b := irc_RegisterChannel('efNET', '#sltesting', 'graycode', '', False, True);
  b.names := ' ' + 'ef' + ' ';

  CheckEquals(4, chankeys.Count, 'Should have 4 chankeys/blowfish');
end;

procedure TTestIRCblowfishSetup.{$IFDEF FPC}TeardownOnce{$ELSE}Teardown{$ENDIF};
begin
  try
    IrcBlowfishUninit;
  except
    on e: Exception do
    begin
      {$IFNDEF FPC}DUnitX.Assert.Assert.{$ENDIF}Fail(Format('Failed to unload OpenSSL: %s %s', [sLineBreak, e.Message]));
    end;
  end;
end;

{ TTestIRCblowfish }

procedure TTestIRCblowfish.TestECB_irc_encrypt;
var
  fResult: String;
begin
  fResult := irc_encrypt('linknet', '#slftp-blowfish', 'Hello guys!');
  CheckNotEquals(0, Length(fResult), 'Length of encrypted text should be longer than 0');
  CheckEqualsString('LoL', fResult, 'Encrypted text does not match');

  fResult := irc_encrypt('efnet', '#blowfishuser', 'please call me noob');
  CheckNotEquals(0, Length(fResult), 'Length of encrypted text should be longer than 0');
  CheckEqualsString('omfg', fResult, 'Encrypted text does not match');
end;

procedure TTestIRCblowfish.TestCBC_irc_encrypt;
var
  fResult: String;
begin
  fResult := irc_encrypt('LinkNET', '#testsl', 'I chat more securely with CBC');
  CheckNotEquals(0, Length(fResult), 'Length of encrypted text should be longer than 0');
  CheckEqualsString('differs always', fResult, 'Encrypted text does not match');

  fResult := irc_encrypt('efNET', '#sltesting', 'I only use CBC');
  CheckNotEquals(0, Length(fResult), 'Length of encrypted text should be longer than 0');
  CheckEqualsString('always different', fResult, 'Encrypted text does not match');
end;

initialization
  {$IFDEF FPC}
    RegisterTest('IRC Blowfish Tests', TTestIRCblowfish.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestIRCblowfish);
  {$ENDIF}
end.