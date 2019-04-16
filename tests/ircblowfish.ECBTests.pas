unit ircblowfish.ECBTests;

interface

uses
  ircchansettingsTests,
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestIrcBlowkeyECB = class(TTestIrcChannelSettingsSetup)
  published
    procedure TestEncryptMessage;
    procedure TestDecryptMessage;
  end;

implementation

uses
  SysUtils, ircchansettings;

{ TTestIrcBlowkeyECB }

type
  TItem = record
    _dText: string;
    _eText: string;
  end;

var
  TestValues: array [0..2] of TItem = (
    (_dText:'Hello guys!' ; _eText:'+OK asdf'),
    (_dText:'please call me noob' ; _eText:'+OK lolol'),
    (_dText:'Im a frénch gúy :)' ; _eText:'+OK qwerty')
  );

procedure TTestIrcBlowkeyECB.TestEncryptMessage;
var
  fChanSettingsObj: TIrcChannelSettings;
  fInputStr, fResult: String;
  i: Integer;
begin
  for i := Low(TestValues) to High(TestValues) do
  begin
    for fChanSettingsObj in IrcChanSettingsList.Values do
    begin
      fResult := fChanSettingsObj.EncryptMessage(TestValues[i]._dText);
      CheckNotEquals(0, Length(fResult), 'Length of encrypted text should be longer than 0');
      CheckEqualsString(TestValues[i]._eText, fResult, 'Encrypted text does not match');
    end;
  end;
end;

procedure TTestIrcBlowkeyECB.TestDecryptMessage;
//var
//  fResult: String;
begin
{
  fResult := irc_encrypt('LinkNET', '#testsl', 'I chat more securely with CBC');
  CheckNotEquals(0, Length(fResult), 'Length of encrypted text should be longer than 0');
  CheckEqualsString('differs always', fResult, 'Encrypted text does not match');

  fResult := irc_encrypt('efNET', '#sltesting', 'I only use CBC');
  CheckNotEquals(0, Length(fResult), 'Length of encrypted text should be longer than 0');
  CheckEqualsString('always different', fResult, 'Encrypted text does not match');
}
end;

initialization
  {$IFDEF FPC}
    RegisterTest('IRC Blowfish ECB Tests', TTestIrcBlowkeyECB.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestIrcBlowkeyECB);
  {$ENDIF}
end.