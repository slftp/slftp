unit indexerTests;

interface

uses
  {$IFDEF FPC}
    fpcunit, testregistry;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestIndexer = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AddReleasesAndQuery;
    procedure AddReleasesEmpty;
  end;

implementation

uses
  SysUtils, indexer;

const
  cSite = 'INDEXERTEST';

{ TTestIndexer }

procedure TTestIndexer.SetUp;
begin
  indexerInit;
  indexerStart;
  // the database file stays after a run, remove leftovers of previous runs
  indexerRemoveSiteSection(cSite, '');
end;

procedure TTestIndexer.TearDown;
begin
  indexerRemoveSiteSection(cSite, '');
  indexerUninit;
end;

procedure TTestIndexer.AddReleasesAndQuery;
var
  fReleases: TArray<String>;
  fRls: String;
  i: Integer;
begin
  fReleases := ['Indexer.Test.One-GRP', 'Indexer.Test.Two-GRP', 'Indexer.Test.Three-GRP'];
  indexerAddReleases(fReleases, cSite, 'TV', '/tv/');

  for i := 0 to High(fReleases) do
  begin
    fRls := fReleases[i];
    CheckEquals(cSite + '-TV=/tv/' + #13#10, indexerQuery(fRls), fReleases[i]);
  end;
end;

procedure TTestIndexer.AddReleasesEmpty;
var
  fRls: String;
begin
  indexerAddReleases([], cSite, 'TV', '/tv/');

  fRls := 'Indexer.Test.One-GRP';
  CheckEquals('', indexerQuery(fRls));
end;

initialization
  {$IFDEF FPC}
    RegisterTest('TTestIndexer', TTestIndexer.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestIndexer);
  {$ENDIF}
end.
