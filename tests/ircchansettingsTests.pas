unit ircchansettingsTests;

interface

uses
  {$IFDEF FPC}
    fpcunit, testregistry;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility, DUnitX.Assert;
  {$ENDIF}

type
  TTestIrcChannelSettingsSetup = class(TTestCase)
  protected
    procedure SetUp; override;
    {$IFNDEF FPC}
      procedure Teardown; override;
    {$ENDIF}
  end;

implementation

uses
  SysUtils, ircchansettings;

{$IFDEF FPC}
var
  // fpcunit has no SetUpOnce, so guard the setup manually: the registered
  // channel settings are identical for all suites inheriting this fixture
  glIrcChannelSettingsSetupDone: Boolean = False;
{$ENDIF}

{ TTestIrcChannelSettingsSetup }

procedure TTestIrcChannelSettingsSetup.SetUp;
begin
  {$IFDEF FPC}
  if glIrcChannelSettingsSetupDone then
    Exit;
  glIrcChannelSettingsSetupDone := True;
  {$ENDIF}

  // init
  IrcChannelSettingsInit;

  { ECB channels }
  RegisterChannelSettings('linknet', '#slftp-blowfish', 'ADMIN', 'ThisIsNOTsecure', '', True, False);
  RegisterChannelSettings('efnet', '#blowfishuser', 'STATS INFO', 'googleme', 'securechannel', False, False);

  { CBC channels }
  RegisterChannelSettings('LinkNET', '#testsl', 'ADMIN', 'asdf1234', '', True, True);
  RegisterChannelSettings('efNET', '#sltesting', 'STATS INFO', 'graycodefishing', 'moresecure', False, True);

  { PlainText channels }
  RegisterChannelSettings('LINKnet', '#insecure', 'ADMIN', '', '', True, False);
  RegisterChannelSettings('EfNet', '#plainy', 'STATS INFO', '', 'moresecure', False, False);

  CheckEquals(6, IrcChanSettingsList.Count, 'Should have 6 chan settings');
end;

{$IFNDEF FPC}
procedure TTestIrcChannelSettingsSetup.Teardown;
begin
  try
    IrcChannelSettingsUninit;
  except
    on e: Exception do
    begin
      DUnitX.Assert.Assert.Fail(Format('Failed to unload IRC Channel Settings: %s %s', [sLineBreak, e.Message]));
    end;
  end;
end;
{$ENDIF}

end.
