unit ircnetworkconfigTests;

interface

uses
  {$IFDEF FPC}
    fpcunit, testregistry;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestIrcNetworkConfig = class(TTestCase)
  published
    procedure TestWritesCanonicalPrimaryServerKeys;
  end;

implementation

uses
  encinifile, irc, sitesunit;

procedure TTestIrcNetworkConfig.TestWritesCanonicalPrimaryServerKeys;
var
  previousSitesDat: TEncIniFile;
  testSitesDat: TEncIniFile;
begin
  previousSitesDat := sitesdat;
  testSitesDat := TEncIniFile.Create('', '', False);
  sitesdat := testSitesDat;
  try
    IrcWritePrimaryServerConfig('TESTNET', 'irc.example.org', 6697);

    CheckEquals('irc.example.org', sitesdat.ReadString('ircnet-TESTNET', 'bnc_host-0', ''));
    CheckEquals(6697, sitesdat.ReadInteger('ircnet-TESTNET', 'bnc_port-0', 0));
    CheckEquals('', sitesdat.ReadString('ircnet-TESTNET', 'host', ''));
    CheckEquals(0, sitesdat.ReadInteger('ircnet-TESTNET', 'port', 0));
  finally
    sitesdat := previousSitesDat;
    testSitesDat.Free;
  end;
end;

initialization
  {$IFDEF FPC}
    RegisterTest('irc network config', TTestIrcNetworkConfig.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestIrcNetworkConfig);
  {$ENDIF}

end.
