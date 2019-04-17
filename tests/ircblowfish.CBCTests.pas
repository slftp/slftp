unit ircblowfish.CBCTests;

interface

uses
  ircchansettingsTests,
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestIrcBlowkeyCBC = class(TTestIrcChannelSettingsSetup)
  published
    procedure TestEncryptMessage;
    procedure TestDecryptMessage;
  end;

implementation

uses
  SysUtils, ircchansettings, ircblowfish.CBC, IdSSLOpenSSL;

{ TTestIrcBlowkeyCBC }

type
  TItem = record
    _dText: string;
    _eText: string;
  end;

var
  // #testsl
  TestValues1: array [0..2] of TItem = (
    (_dText:'Hello guys!' ; _eText:'+OK *CXafm5eIlo8mQ12vArFSS6GLQYmTaGGF'),
    (_dText:'please call me noob' ; _eText:'+OK *KI4brpXJZaMbE+tDkerjjQ2F86KVgNohQAwSjNqWT20='),
    (_dText:'Im a frénch gúy :]' ; _eText:'+OK *b8wj0Tj76xrndXAgMjzSdR4/lApZfQdUbdq3zkpVxrY=')
  );
  // #sltesting
  TestValues2: array [0..2] of TItem = (
    (_dText:'Hello guys!' ; _eText:'+OK *LNO+lAO4d5KTIfGV2nuCFtU+FirnW5hN'),
    (_dText:'please call me noob' ; _eText:'+OK *i/PCJijQodq2IbxisvlvwK6k1s/Dw+lCrvbq9sM09A0='),
    (_dText:'Im a frénch gúy :]' ; _eText:'+OK */VOHZwapNbV7tMml1zf4TWSwxjG6VV6419VnVkRU3sM=')
  );

procedure TTestIrcBlowkeyCBC.TestEncryptMessage;
var
  fChanSettingsObj: TIrcChannelSettings;
  fInputStr, fResult: String;
  i: Integer;
begin
  CheckTrue(IdSSLOpenSSL.LoadOpenSSLLibrary, 'IdSSLOpenSSL.LoadOpenSSLLibrary loaded');

  for fChanSettingsObj in IrcChanSettingsList.Values do
  begin
    if (fChanSettingsObj is TIrcBlowkeyCBC) then
    begin
      if (fChanSettingsObj.Channel = '#testsl') then
      begin
        for i := Low(TestValues1) to High(TestValues1) do
        begin
          fResult := fChanSettingsObj.EncryptMessage(TestValues1[i]._dText);
          CheckNotEquals(0, Length(fResult), 'Length of encrypted text should be longer than 0 for chan ' + fChanSettingsObj.Channel);
          // message always vary for CBC, so decrypt it and check if we get the same result 
          fResult := fChanSettingsObj.DecryptMessage(fResult.Remove(0, 5));
          CheckEqualsString(TestValues1[i]._dText, fResult, 'Resulting decrypted text does not match previous encrypted text for chan ' + fChanSettingsObj.Channel);
        end;
      end
      else if (fChanSettingsObj.Channel = '#sltesting') then
      begin
        for i := Low(TestValues2) to High(TestValues2) do
        begin
          fResult := fChanSettingsObj.EncryptMessage(TestValues2[i]._dText);
          CheckNotEquals(0, Length(fResult), 'Length of encrypted text should be longer than 0 for chan ' + fChanSettingsObj.Channel);
          // message always vary for CBC, so decrypt it and check if we get the same result 
          fResult := fChanSettingsObj.DecryptMessage(fResult.Remove(0, 5));
          CheckEqualsString(TestValues2[i]._dText, fResult, 'Resulting decrypted text does not match previous encrypted text for chan ' + fChanSettingsObj.Channel);
        end;
      end;
    end;
  end;
end;

procedure TTestIrcBlowkeyCBC.TestDecryptMessage;
var
  fChanSettingsObj: TIrcChannelSettings;
  fInputStr, fResult: String;
  i: Integer;
begin
  CheckTrue(IdSSLOpenSSL.LoadOpenSSLLibrary, 'IdSSLOpenSSL.LoadOpenSSLLibrary loaded');

  for fChanSettingsObj in IrcChanSettingsList.Values do
  begin
    if (fChanSettingsObj is TIrcBlowkeyCBC) then
    begin
      if (fChanSettingsObj.Channel = '#testsl') then
      begin
        for i := Low(TestValues1) to High(TestValues1) do
        begin
          fResult := fChanSettingsObj.DecryptMessage(TestValues1[i]._eText.Remove(0, 5));
          CheckNotEquals(0, Length(fResult), 'Length of decrypted text should be longer than 0 for chan ' + fChanSettingsObj.Channel);
          CheckEqualsString(TestValues1[i]._dText, fResult, 'Decrypted text does not match for chan ' + fChanSettingsObj.Channel);
        end;
      end
      else if (fChanSettingsObj.Channel = '#sltesting') then
      begin
        for i := Low(TestValues2) to High(TestValues2) do
        begin
          fResult := fChanSettingsObj.DecryptMessage(TestValues2[i]._eText.Remove(0, 5));
          CheckNotEquals(0, Length(fResult), 'Length of decrypted text should be longer than 0 for chan ' + fChanSettingsObj.Channel);
          CheckEqualsString(TestValues2[i]._dText, fResult, 'Decrypted text does not match for chan ' + fChanSettingsObj.Channel);
        end;
      end;
    end;
  end;
end;

initialization
  {$IFDEF FPC}
    RegisterTest('IRC Blowfish CBC Tests', TTestIrcBlowkeyCBC.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestIrcBlowkeyCBC);
  {$ENDIF}
end.