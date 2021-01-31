unit ircblowfish.CBCTests;

interface

uses
  ircchansettingsTests,
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility, DUnitX.Assert;
  {$ENDIF}

type
  TTestIrcBlowkeyCBC = class(TTestIrcChannelSettingsSetup)
  private
    // copy of slftpUnitTestsSetupIndyOpenSSL unit because it cannot be called if not inherited from TTestIndyOpenSSL
    procedure LoadIndyOpenSSL;
  published
    procedure TestEncryptMessage;
    procedure TestDecryptMessage;
  end;

implementation

uses
  SysUtils, ircchansettings, ircblowfish.CBC, IdOpenSSLLoader, IdSSLOpenSSLHeaders;

{ TTestIrcBlowkeyCBC }

type
  TItem = record
    _dText: string;
    _eText: string;
  end;

var
  // #testsl
  TestValues1: array [0..3] of TItem = (
    (_dText:'Hello guys!' ; _eText:'+OK *CXafm5eIlo8mQ12vArFSS6GLQYmTaGGF'),
    (_dText:'please call me noob' ; _eText:'+OK *KI4brpXJZaMbE+tDkerjjQ2F86KVgNohQAwSjNqWT20='),
    (_dText:'Im a frénch gúy :]' ; _eText:'+OK *b8wj0Tj76xrndXAgMjzSdR4/lApZfQdUbdq3zkpVxrY='),
    (_dText:'[SKIP] : MP3 Najoua_Belyzel_-_Rendez-Vous_(De_La_Lune_Au_Soleil)-WEB-FR-2019-ZzZz @ XXXX "XXXX * if foreign && not language = German || mp3foreign && mp3language != DE then DROP" (UPDATE)' ; _eText:'+OK *jpmvB6OQwtebe9MKz+a/Rhe8IiifP5+S/DxR9+LLgRe7dECBBn/DuANkcjaYhHCnn37u9zob4Y+yjTBMUJJuKvpzLiQRoOukr7iBDOyrRNPAmqphtXWIVGFmaac7leXbbdXOViCPtyhUjuXOcL3q9Uol8r86jPvCuLD9tCWLOdnO73rWe8hYOmTRPNzIFdcrF7' + 'mrpPdzLtNZEXp/Hqgeg7W4YwCx9zfUjfhCBHokvPHCV6tZHuYIBCDSEMvdoRaSYuZikjKshOI=')
  );
  // #sltesting
  TestValues2: array [0..3] of TItem = (
    (_dText:'Hello guys!' ; _eText:'+OK *LNO+lAO4d5KTIfGV2nuCFtU+FirnW5hN'),
    (_dText:'please call me noob' ; _eText:'+OK *i/PCJijQodq2IbxisvlvwK6k1s/Dw+lCrvbq9sM09A0='),
    (_dText:'Im a frénch gúy :]' ; _eText:'+OK */VOHZwapNbV7tMml1zf4TWSwxjG6VV6419VnVkRU3sM='),
    (_dText:'[SKIP] : MP3 Najoua_Belyzel_-_Rendez-Vous_(De_La_Lune_Au_Soleil)-WEB-FR-2019-ZzZz @ XXXX "XXXX * if foreign && not language = German || mp3foreign && mp3language != DE then DROP" (COMPLETE)' ; _eText:'+OK *F0VWajKu7KLXb5SJbQpNLPhAdo8pb1tlwOT+Q5OLD6/Oj64ETCXeXEdO0zBMq+svj7nkbp6z1fi1PdDQ/U0q0jNbFeQ+hyrv1ByNz+GHMTMmXJQPiW57PQkzV5qQ9+BLwgkhOnbMF8RiQ1wu7ksGPBhveYtrR9kaGDim/9mIvyQKcJekf/ANp6gBsRLABfjcB' + '1q1lAGCpUU6EgttCygeg1Q0F08zWEIpdi1bsSXljOznBO0uojAebltLQmusQ1JkC6H2CMs0dkE=')
  );

procedure TTestIrcBlowkeyCBC.LoadIndyOpenSSL;
var
  fSslLoader: IOpenSSLLoader;
begin
  fSslLoader := IdOpenSSLLoader.GetOpenSSLLoader;
  // Tell Indy OpenSSL to load libs from current dir
  fSslLoader.OpenSSLPath := '.';

  {$IFDEF UNIX}
    // do not try to load sym links first
    IdOpenSSLSetLoadSymLinksFirst(False);
  {$ENDIF}

  try
    CheckTrue(fSslLoader.Load, 'IdOpenSSLLoader.Load failed: ' + fSslLoader.FailedToLoad.CommaText);
  except
    on e: Exception do
    begin
      {$IFNDEF FPC}DUnitX.Assert.Assert.{$ENDIF}Fail(Format('[EXCEPTION] Unexpected error while loading OpenSSL: %s%s %s%s', [sLineBreak, e.ClassName, sLineBreak, e.Message]));
    end;
  end;
end;

procedure TTestIrcBlowkeyCBC.TestEncryptMessage;
var
  fChanSettingsObj: TIrcChannelSettings;
  fInputStr, fResult: String;
  i: Integer;
begin
  LoadIndyOpenSSL;

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
  LoadIndyOpenSSL;

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