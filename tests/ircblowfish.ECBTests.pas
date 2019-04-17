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
  SysUtils, ircchansettings, ircblowfish.ECB;

{ TTestIrcBlowkeyECB }

type
  TItem = record
    _dText: string;
    _eText: string;
  end;

var
  // #slftp-blowfish
  TestValues1: array [0..2] of TItem = (
    (_dText:'Hello guys!' ; _eText:'+OK zzPVJ0.imLT1rN4oA.CbFjU1'),
    (_dText:'please call me noob' ; _eText:'+OK Hqywo1KBhKz0XSB/U17DvWd/lED26/nQUfB1'),
    (_dText:'Im a frénch gúy :]' ; _eText:'+OK pLJzN/xHT5y04HZAo.rflC9/8QcJE1rbRJ/.')
  );
  // #blowfishuser
  TestValues2: array [0..2] of TItem = (
    (_dText:'Hello guys!' ; _eText:'+OK di93b.objmT0YE/FY0FeOe5.'),
    (_dText:'please call me noob' ; _eText:'+OK yOCAR.8sXH./lEAyP/LOvkf/V4.cb/KrDXR/'),
    (_dText:'Im a frénch gúy :]' ; _eText:'+OK OIXm3.mwbJx0.8oZh.D0tbo/jcTAR.nUiBg1')
  );

procedure TTestIrcBlowkeyECB.TestEncryptMessage;
var
  fChanSettingsObj: TIrcChannelSettings;
  fInputStr, fResult: String;
  i: Integer;
begin
  for fChanSettingsObj in IrcChanSettingsList.Values do
  begin
    if (fChanSettingsObj is TIrcBlowkeyECB) then
    begin
      if (fChanSettingsObj.Channel = '#slftp-blowfish') then
      begin
        for i := Low(TestValues1) to High(TestValues1) do
        begin
          fResult := fChanSettingsObj.EncryptMessage(TestValues1[i]._dText);
          CheckNotEquals(0, Length(fResult), 'Length of encrypted text should be longer than 0');
          CheckEqualsString(TestValues1[i]._eText, fResult, 'Encrypted text does not match');
        end;
      end
      else if (fChanSettingsObj.Channel = '#blowfishuser') then
      begin
        for i := Low(TestValues2) to High(TestValues2) do
        begin
          fResult := fChanSettingsObj.EncryptMessage(TestValues2[i]._dText);
          CheckNotEquals(0, Length(fResult), 'Length of encrypted text should be longer than 0');
          CheckEqualsString(TestValues2[i]._eText, fResult, 'Encrypted text does not match');
        end;
      end;
    end;
  end;
end;

procedure TTestIrcBlowkeyECB.TestDecryptMessage;
var
  fChanSettingsObj: TIrcChannelSettings;
  fInputStr, fResult: String;
  i: Integer;
begin
  for fChanSettingsObj in IrcChanSettingsList.Values do
  begin
    if (fChanSettingsObj is TIrcBlowkeyECB) then
    begin
      if (fChanSettingsObj.Channel = '#slftp-blowfish') then
      begin
        for i := Low(TestValues1) to High(TestValues1) do
        begin
          fResult := fChanSettingsObj.DecryptMessage(TestValues1[i]._eText);
          CheckNotEquals(0, Length(fResult), 'Length of encrypted text should be longer than 0');
          CheckEqualsString(TestValues1[i]._dText, fResult, 'Encrypted text does not match');
        end;
      end
      else if (fChanSettingsObj.Channel = '#blowfishuser') then
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
    RegisterTest('IRC Blowfish ECB Tests', TTestIrcBlowkeyECB.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestIrcBlowkeyECB);
  {$ENDIF}
end.