unit ircblowfish.plaintextTests;

interface

uses
  ircchansettingsTests,
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestIrcBlowkeyPlaintext = class(TTestIrcChannelSettingsSetup)
  published
    procedure TestEncryptMessage;
    procedure TestDecryptMessage;
  end;

implementation

uses
  SysUtils, ircchansettings, ircblowfish.plaintext;

{ TTestIrcBlowkeyPlaintext }

type
  TItem = record
    _dText: string;
    _eText: string;
  end;

var
  // #insecure
  TestValues1: array [0..2] of TItem = (
    (_dText:'Hello guys!' ; _eText:'Hello guys!'),
    (_dText:'please call me noob' ; _eText:'please call me noob'),
    (_dText:'Im a frénch gúy :]' ; _eText:'Im a frénch gúy :]')
  );
  // #plainy
  TestValues2: array [0..2] of TItem = (
    (_dText:'Hello guys!' ; _eText:'Hello guys!'),
    (_dText:'please call me noob' ; _eText:'please call me noob'),
    (_dText:'Im a frénch gúy :]' ; _eText:'Im a frénch gúy :]')
  );

procedure TTestIrcBlowkeyPlaintext.TestEncryptMessage;
var
  fChanSettingsObj: TIrcChannelSettings;
  fInputStr, fResult: String;
  i: Integer;
begin
  for fChanSettingsObj in IrcChanSettingsList.Values do
  begin
    if (fChanSettingsObj is TIrcBlowkeyPlaintext) then
    begin
      if (fChanSettingsObj.Channel = '#insecure') then
      begin
        for i := Low(TestValues1) to High(TestValues1) do
        begin
          fResult := fChanSettingsObj.EncryptMessage(TestValues1[i]._dText);
          CheckNotEquals(0, Length(fResult), 'Length of encrypted text should be longer than 0 for chan ' + fChanSettingsObj.Channel);
          CheckEqualsString(TestValues1[i]._eText, fResult, 'Encrypted text does not match for chan ' + fChanSettingsObj.Channel);
        end;
      end
      else if (fChanSettingsObj.Channel = '#plainy') then
      begin
        for i := Low(TestValues2) to High(TestValues2) do
        begin
          fResult := fChanSettingsObj.EncryptMessage(TestValues2[i]._dText);
          CheckNotEquals(0, Length(fResult), 'Length of encrypted text should be longer than 0 for chan ' + fChanSettingsObj.Channel);
          CheckEqualsString(TestValues2[i]._eText, fResult, 'Encrypted text does not match for chan ' + fChanSettingsObj.Channel);
        end;
      end;
    end;
  end;
end;

procedure TTestIrcBlowkeyPlaintext.TestDecryptMessage;
var
  fChanSettingsObj: TIrcChannelSettings;
  fInputStr, fResult: String;
  i: Integer;
begin
  for fChanSettingsObj in IrcChanSettingsList.Values do
  begin
    if (fChanSettingsObj is TIrcBlowkeyPlaintext) then
    begin
      if (fChanSettingsObj.Channel = '#insecure') then
      begin
        for i := Low(TestValues1) to High(TestValues1) do
        begin
          fResult := fChanSettingsObj.DecryptMessage(TestValues1[i]._eText);
          CheckNotEquals(0, Length(fResult), 'Length of decrypted text should be longer than 0 for chan ' + fChanSettingsObj.Channel);
          CheckEqualsString(TestValues1[i]._dText, fResult, 'Decrypted text does not match for chan ' + fChanSettingsObj.Channel);
        end;
      end
      else if (fChanSettingsObj.Channel = '#plainy') then
      begin
        for i := Low(TestValues2) to High(TestValues2) do
        begin
          fResult := fChanSettingsObj.DecryptMessage(TestValues2[i]._eText);
          CheckNotEquals(0, Length(fResult), 'Length of decrypted text should be longer than 0 for chan ' + fChanSettingsObj.Channel);
          CheckEqualsString(TestValues2[i]._dText, fResult, 'Decrypted text does not match for chan ' + fChanSettingsObj.Channel);
        end;
      end;
    end;
  end;
end;

initialization
  {$IFDEF FPC}
    RegisterTest('IRC Blowfish PlainText Tests', TTestIrcBlowkeyPlaintext.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestIrcBlowkeyPlaintext);
  {$ENDIF}
end.
