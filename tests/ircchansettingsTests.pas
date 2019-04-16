unit ircchansettingsTests;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility, DUnitX.Assert;
  {$ENDIF}

type
  TTestIrcChannelSettingsSetup = class(TTestCase)
  protected
    {$IFDEF FPC}
      procedure SetUpOnce; override;
      procedure TeardownOnce; override;
    {$ELSE}
      procedure SetUp; override;
      procedure Teardown; override;
    {$ENDIF}
  end;

implementation

uses
  SysUtils, ircchansettings;

{ TTestIrcChannelSettingsSetup }

procedure TTestIrcChannelSettingsSetup.{$IFDEF FPC}SetUpOnce{$ELSE}SetUp{$ENDIF};
begin
  // init
  IrcChannelSettingsInit;

  { ECB channels }
  RegisterChannelSettings('linknet', '#slftp-blowfish', 'ADMIN', 'ThisIsNOTsecure', '', False, False);
  RegisterChannelSettings('efnet', '#blowfishuser', 'googleme', 'STATS INFO', 'lesssecurechannel', False, False);

  { CBC channels }
  RegisterChannelSettings('LinkNET', '#testsl', 'asdf1234', 'ADMIN', '', False, True);
  RegisterChannelSettings('efNET', '#sltesting', 'graycode', 'STATS INFO', '', False, True);

  CheckEquals(4, IrcChanSettingsList.Count, 'Should have 4 chan settings');
end;

procedure TTestIrcChannelSettingsSetup.{$IFDEF FPC}TeardownOnce{$ELSE}Teardown{$ENDIF};
begin
  try
    IrcChannelSettingsUninit;
  except
    on e: Exception do
    begin
      {$IFNDEF FPC}DUnitX.Assert.Assert.{$ENDIF}Fail(Format('Failed to unload IRC Channel Settings: %s %s', [sLineBreak, e.Message]));
    end;
  end;
end;

end.