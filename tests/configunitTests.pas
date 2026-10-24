unit configunitTests;

interface

uses
  {$IFDEF FPC}
    fpcunit, testregistry;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestConfigCache = class(TTestCase)
  published
    procedure TestCachedValuesMatchLoadedConfig;
  end;

implementation

uses
  configunit, mrdohutils;

procedure TTestConfigCache.TestCachedValuesMatchLoadedConfig;
begin
  CheckEquals(Ord(config.ReadBool('console', 'show_infos', False)),
    Ord(config_cached_bool('console', 'show_infos', False)));
  CheckEquals(config.ReadInteger('config', 'connect', 20),
    config_cached_integer('config', 'connect', 20));
  CheckEquals(config.ReadString('ident', 'response', 'slftpuser'),
    config_cached_string('ident', 'response', 'slftpuser'));
  CheckEquals(Ord(spamcfg.ReadBool('kb', 'new_rls', True)),
    Ord(spam_config_cached_bool('kb', 'new_rls', True)));
end;

initialization
  {$IFDEF FPC}
    RegisterTest('configunit', TTestConfigCache.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestConfigCache);
  {$ENDIF}
end.
