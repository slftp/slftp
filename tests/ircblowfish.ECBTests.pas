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
  TestValues1: array [0..3] of TItem = (
    (_dText:'Hello guys!' ; _eText:'+OK zzPVJ0.imLT1rN4oA.CbFjU1'),
    (_dText:'please call me noob' ; _eText:'+OK Hqywo1KBhKz0XSB/U17DvWd/lED26/nQUfB1'),
    (_dText:'Im a frénch gúy :]' ; _eText:'+OK pLJzN/xHT5y04HZAo.rflC9/8QcJE1rbRJ/.'),
    (_dText:'[SKIP] : MP3 Najoua_Belyzel_-_Rendez-Vous_(De_La_Lune_Au_Soleil)-WEB-FR-2019-ZzZz @ XXXX "XXXX * if foreign && not language = German || mp3foreign && mp3language != DE then DROP" (COMPLETE)' ; _eText:'+OK 6zyoQ.j1Is10m6vhl0yaEwj1K2D3G.OHB.D0ffCJ/0f2Zs..4PzXw.OOH7k1iFPaf.VXGtK10IEr71fgsEF0NO/b./OEc7a.WNuM70IQoVI0nc9ZI1jtsvH1CDR.a1arSd80bDC37.YIx7P02B6f3//9CWP.H2.tQ0RCWvE11' + 'WkSu1pncvr0leecu00QrLo0aeVFB13FMtn/nlpUu1V.H6f/BgfqP/owhK1.Wlmr30EIWTo/XOFPm0yp5Xe1kQyF30Ee2cV0quB6/0U0oJ4.AF542.m4EE5.')
  );
  // #blowfishuser
  TestValues2: array [0..3] of TItem = (
    (_dText:'Hello guys!' ; _eText:'+OK di93b.objmT0YE/FY0FeOe5.'),
    (_dText:'please call me noob' ; _eText:'+OK yOCAR.8sXH./lEAyP/LOvkf/V4.cb/KrDXR/'),
    (_dText:'Im a frénch gúy :]' ; _eText:'+OK OIXm3.mwbJx0.8oZh.D0tbo/jcTAR.nUiBg1'),
    (_dText:'[SKIP] : MP3 Najoua_Belyzel_-_Rendez-Vous_(De_La_Lune_Au_Soleil)-WEB-FR-2019-ZzZz @ XXXX "XXXX * if foreign && not language = German || mp3foreign && mp3language != DE then DROP" (UPDATE)' ; _eText:'+OK EATXl1OPfey.J3eKA/ie/R7.ik.gW/ndWhI11JmeF0VWb24/.kauk.pCV58.s9cvK/4mHhr1CKJ5e/cjulL.G0jqM/oOL601Hp..M.NqPFk/oLJT2.lVRtq0.7Rn7/YwwoU.VRcL30hwDFB11TEf0/15.oZ0eP1bd1VKTjt.FX' + 'oB./kjHZb/FMEJJ1.W1O117bv7g19ardq.V2HKu0a19aO/vfPar.Sg2nO16F5M9.zV.GN0POItR1oLxjx1NWdP3.W2sXQ.MtHL80gJaKQ.TkHnr/PJXc.0')
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
          CheckNotEquals(0, Length(fResult), 'Length of encrypted text should be longer than 0 for chan ' + fChanSettingsObj.Channel);
          CheckEqualsString(TestValues1[i]._eText, fResult, 'Encrypted text does not match for chan ' + fChanSettingsObj.Channel);
        end;
      end
      else if (fChanSettingsObj.Channel = '#blowfishuser') then
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
          fResult := fChanSettingsObj.DecryptMessage(TestValues1[i]._eText.Remove(0, 4));
          CheckNotEquals(0, Length(fResult), 'Length of decrypted text should be longer than 0 for chan ' + fChanSettingsObj.Channel);
          CheckEqualsString(TestValues1[i]._dText, fResult, 'Decrypted text does not match for chan ' + fChanSettingsObj.Channel);
        end;
      end
      else if (fChanSettingsObj.Channel = '#blowfishuser') then
      begin
        for i := Low(TestValues2) to High(TestValues2) do
        begin
          fResult := fChanSettingsObj.DecryptMessage(TestValues2[i]._eText.Remove(0, 4));
          CheckNotEquals(0, Length(fResult), 'Length of decrypted text should be longer than 0 for chan ' + fChanSettingsObj.Channel);
          CheckEqualsString(TestValues2[i]._dText, fResult, 'Decrypted text does not match for chan ' + fChanSettingsObj.Channel);
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
