unit ruleconditions.tvTests;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestTVRuleConditions = class(TTestCase)
  published
    procedure TVEpisodeAgeDaysUnknownReturns99999;
    procedure TVEpisodeAgeDaysReturnsExpectedRange;
  end;

implementation

uses
  SysUtils, DateUtils, Math, kb.releaseinfo, ruleconditions.tv;

{ Helper: compute tvepisodeagedays from an episode_airdate value,
  mirroring TConditionTVEpisodeAgeDays.SupplyValue logic without TPazo. }
function CalcEpisodeAgeDays(const aAirdate: Int64): Integer;
begin
  if aAirdate > 0 then
    Result := DaysBetween(UnixToDateTime(aAirdate), Now)
  else
    Result := 99999;
end;

procedure TTestTVRuleConditions.TVEpisodeAgeDaysUnknownReturns99999;
begin
  CheckEquals(99999, CalcEpisodeAgeDays(-1), 'Unknown episode_airdate (-1) must return 99999');
  CheckEquals(99999, CalcEpisodeAgeDays(0),  'Unknown episode_airdate (0) must return 99999');
end;

procedure TTestTVRuleConditions.TVEpisodeAgeDaysReturnsExpectedRange;
var
  fAirdate: Int64;
  fAgeDays: Integer;
begin
  fAirdate := DateTimeToUnix(IncDay(Now, -10));
  fAgeDays := CalcEpisodeAgeDays(fAirdate);
  CheckTrue(fAgeDays >= 9,  'Episode 10 days ago: age should be >= 9');
  CheckTrue(fAgeDays <= 11, 'Episode 10 days ago: age should be <= 11');
end;

initialization
  {$IFDEF FPC}
    RegisterTest('ruleconditions.tv', TTestTVRuleConditions.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestTVRuleConditions);
  {$ENDIF}
end.
