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
  SysUtils, ircchansettings;

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
    (_dText:'Im a frénch gúy :)' ; _eText:'+OK *rg0iG4Jyyeho4J2hRUt95ByAw3EhnDlxg4mszMI4XHQ=')
  );
  // #sltesting
  TestValues2: array [0..2] of TItem = (
    (_dText:'Hello guys!' ; _eText:'+OK *LNO+lAO4d5KTIfGV2nuCFtU+FirnW5hN'),
    (_dText:'please call me noob' ; _eText:'+OK *i/PCJijQodq2IbxisvlvwK6k1s/Dw+lCrvbq9sM09A0='),
    (_dText:'Im a frénch gúy :)' ; _eText:'+OK *2BjXD7fb+EM52ZmNKeKCowX79ntFNZPYFzEEHvcZac8=')
  );

procedure TTestIrcBlowkeyCBC.TestEncryptMessage;
var
  fChanSettingsObj: TIrcChannelSettings;
  fInputStr, fResult: String;
  i: Integer;
begin
{
  for i := Low(TestValues) to High(TestValues) do
  begin
    for fChanSettingsObj in IrcChanSettingsList.Values do
    begin
      fResult := fChanSettingsObj.EncryptMessage(TestValues[i]._dText);
      CheckNotEquals(0, Length(fResult), 'Length of encrypted text should be longer than 0');
      CheckEqualsString(TestValues[i]._eText, fResult, 'Encrypted text does not match');
    end;
  end;
}
end;

procedure TTestIrcBlowkeyCBC.TestDecryptMessage;
var
  fChanSettingsObj: TIrcChannelSettings;
  fInputStr, fResult: String;
  i: Integer;
begin
  for fChanSettingsObj in IrcChanSettingsList.Values do
  begin
    if (fChanSettingsObj is TIrcBlowkeyCBC) then
    begin
      if (fChanSettingsObj.Channel = '#testsl') then
      begin
        for i := Low(TestValues1) to High(TestValues1) do
        begin
          fResult := fChanSettingsObj.DecryptMessage(TestValues1[i]._eText);
          CheckNotEquals(0, Length(fResult), 'Length of encrypted text should be longer than 0');
          CheckEqualsString(TestValues1[i]._dText, fResult, 'Encrypted text does not match');
        end;
      end
      else if (fChanSettingsObj.Channel = '#sltesting') then
      begin
        for i := Low(TestValues2) to High(TestValues2) do
        begin
          fResult := fChanSettingsObj.DecryptMessage(TestValues2[i]._eText);
          CheckNotEquals(0, Length(fResult), 'Length of encrypted text should be longer than 0');
          CheckEqualsString(TestValues2[i]._dText, fResult, 'Encrypted text does not match');
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