unit pazo.rankTests;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  { Tests the shared source and destination rank ordering. }
  TTestPazoRanks = class(TTestCase)
  published
    procedure TestDescendingRankOrder;
    procedure TestEqualRanks;
    procedure TestExtremeRanks;
  end;

implementation

uses
  pazo, Generics.Collections, Generics.Defaults;

procedure TTestPazoRanks.TestDescendingRankOrder;
var
  fSites: TList<TSiteRank>;
begin
  fSites := TList<TSiteRank>.Create(TComparer<TSiteRank>.Construct(CompareSiteRanks));
  try
    fSites.Add(TSiteRank.Create(nil, 5));
    fSites.Add(TSiteRank.Create(nil, -20));
    fSites.Add(TSiteRank.Create(nil, 101));
    fSites.Add(TSiteRank.Create(nil, 0));
    fSites.Sort;

    CheckEquals(101, fSites[0].Rank);
    CheckEquals(5, fSites[1].Rank);
    CheckEquals(0, fSites[2].Rank);
    CheckEquals(-20, fSites[3].Rank);
  finally
    fSites.Free;
  end;
end;

procedure TTestPazoRanks.TestEqualRanks;
begin
  CheckEquals(0, CompareSiteRanks(TSiteRank.Create(nil, 5), TSiteRank.Create(nil, 5)));
end;

procedure TTestPazoRanks.TestExtremeRanks;
begin
  CheckTrue(CompareSiteRanks(TSiteRank.Create(nil, High(Integer)),
    TSiteRank.Create(nil, Low(Integer))) < 0);
  CheckTrue(CompareSiteRanks(TSiteRank.Create(nil, Low(Integer)),
    TSiteRank.Create(nil, High(Integer))) > 0);
end;

initialization
  {$IFDEF FPC}
    RegisterTest('pazo.ranks', TTestPazoRanks.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestPazoRanks);
  {$ENDIF}
end.
