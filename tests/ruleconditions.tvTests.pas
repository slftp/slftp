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
    procedure TVEpisodeAgeDaysUnknownReturnsMinusOne;
    procedure TVEpisodeAgeDaysReturnsExpectedRange;
  end;

implementation

uses
  SysUtils, DateUtils, kb.releaseinfo, pazo, ruleconditions.tv;

type
  TConditionTVEpisodeAgeDaysForTest = class(TConditionTVEpisodeAgeDays)
    function AtConditionName: String; override;
  end;

function TConditionTVEpisodeAgeDaysForTest.AtConditionName: String;
begin
  Result := Name;
end;

procedure TTestTVRuleConditions.TVEpisodeAgeDaysUnknownReturnsMinusOne;
var
  p: TPazo;
  c: TConditionTVEpisodeAgeDaysForTest;
begin
  p := TPazo.Create(nil, 1);
  p.rls := TTVRelease.Create('Example.Show.S01E01.720p.HDTV.x264-TEST', 'TV');
  c := TConditionTVEpisodeAgeDaysForTest.Create(nil);
  try
    CheckEquals(-1, c.SupplyValue(p), 'Unknown episode airdate must return -1');
  finally
    c.Free;
    p.Free;
  end;
end;

procedure TTestTVRuleConditions.TVEpisodeAgeDaysReturnsExpectedRange;
var
  p: TPazo;
  c: TConditionTVEpisodeAgeDaysForTest;
  rls: TTVRelease;
  ageDays: Integer;
begin
  p := TPazo.Create(nil, 2);
  p.rls := TTVRelease.Create('Example.Show.S01E02.720p.HDTV.x264-TEST', 'TV');
  c := TConditionTVEpisodeAgeDaysForTest.Create(nil);
  try
    rls := TTVRelease(p.rls);
    rls.episode_airdate := DateTimeToUnix(IncDay(Now, -10));
    ageDays := c.SupplyValue(p);

    CheckTrue(ageDays >= 9, 'Episode age should be at least 9 days');
    CheckTrue(ageDays <= 11, 'Episode age should be at most 11 days');
  finally
    c.Free;
    p.Free;
  end;
end;

initialization
  {$IFDEF FPC}
    RegisterTest('ruleconditions.tv', TTestTVRuleConditions.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestTVRuleConditions);
  {$ENDIF}
end.
