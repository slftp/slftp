unit sltlsstatsTests;

interface

uses
  {$IFDEF FPC}
    fpcunit, testregistry;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestTLSStats = class(TTestCase)
  published
    procedure OutcomesAreDisjointAndSnapshotsDoNotReset;
    procedure DownloadsAndRetriesStaySeparate;
    procedure ClassifiesMetadataWithoutKeepingNames;
  end;

implementation

uses SysUtils, sltlsstats;

procedure TTestTLSStats.OutcomesAreDisjointAndSnapshotsDoNotReset;
var
  fStats: TSiteTLSStats;
  fSample: TTLSHandshakeSample;
  fSnapshot, fAgain: TTLSStatsSnapshot;
  fOutcome: TTLSHandshakeOutcome;
begin
  fStats := TSiteTLSStats.Create;
  try
    for fOutcome := Low(TTLSHandshakeOutcome) to High(TTLSHandshakeOutcome) do
    begin
      fSample.Outcome := fOutcome;
      fSample.ElapsedMs := 10 + Ord(fOutcome);
      fStats.AddHandshake(tskLogin, fSample);
    end;
    fSnapshot := fStats.Snapshot;
    fAgain := fStats.Snapshot;
    CheckEquals(Int64(4), fSnapshot.Counters[tskLogin].Handshakes);
    CheckEquals(Int64(1), fSnapshot.Counters[tskLogin].Successes);
    CheckEquals(Int64(1), fSnapshot.Counters[tskLogin].Errors);
    CheckEquals(Int64(1), fSnapshot.Counters[tskLogin].Timeouts);
    CheckEquals(Int64(1), fSnapshot.Counters[tskLogin].Cancelled);
    CheckEquals(Int64(46), fSnapshot.Counters[tskLogin].HandshakeMs);
    CheckEquals(Int64(13), fSnapshot.Counters[tskLogin].HandshakeMaxMs);
    CheckEquals(Int64(0), fSnapshot.Counters[tskSFV].Handshakes);
    CheckEquals(FormatTLSStats(tskLogin, fSnapshot.Counters[tskLogin]),
      FormatTLSStats(tskLogin, fAgain.Counters[tskLogin]));
    CheckTrue(fAgain.WindowMs >= fSnapshot.WindowMs);
  finally
    fStats.Free;
  end;
end;

procedure TTestTLSStats.DownloadsAndRetriesStaySeparate;
var
  fStats: TSiteTLSStats;
  fSnapshot: TTLSStatsSnapshot;
  fText: String;
begin
  fStats := TSiteTLSStats.Create;
  try
    fStats.AddDownload(tskSFV, True, 50);
    fStats.AddDownload(tskSFV, False, 100);
    fStats.AddRetry(tskSFV, True);
    fStats.AddRetry(tskNFO, False);
    fSnapshot := fStats.Snapshot;
    CheckEquals(Int64(2), fSnapshot.Counters[tskSFV].Downloads);
    CheckEquals(Int64(1), fSnapshot.Counters[tskSFV].DownloadSuccesses);
    CheckEquals(Int64(1), fSnapshot.Counters[tskSFV].DownloadErrors);
    CheckEquals(Int64(150), fSnapshot.Counters[tskSFV].DownloadMs);
    CheckEquals(Int64(100), fSnapshot.Counters[tskSFV].DownloadMaxMs);
    CheckEquals(Int64(1), fSnapshot.Counters[tskSFV].RetryErrors);
    CheckEquals(Int64(0), fSnapshot.Counters[tskSFV].RetryWaits);
    CheckEquals(Int64(0), fSnapshot.Counters[tskNFO].Downloads);
    CheckEquals(Int64(1), fSnapshot.Counters[tskNFO].RetryWaits);
    fText := FormatTLSStats(tskSFV, fSnapshot.Counters[tskSFV]);
    CheckTrue(Pos('kind=sfv hs=0', fText) > 0);
    CheckTrue(Pos('dl=2 dl_ok=1 dl_err=1 dl_ms=150 dl_max_ms=100', fText) > 0);
  finally
    fStats.Free;
  end;
end;

procedure TTestTLSStats.ClassifiesMetadataWithoutKeepingNames;
begin
  CheckEquals(Ord(tskSFV), Ord(TLSDownloadKind('/release/file.SFV')));
  CheckEquals(Ord(tskNFO), Ord(TLSDownloadKind('release.nfo')));
  CheckEquals(Ord(tskOther), Ord(TLSDownloadKind('/release.nfo/file.txt')));
  CheckEquals(Ord(tskOther), Ord(TLSDownloadKind('')));
end;

initialization
  {$IFDEF FPC}
    RegisterTest('sltlsstats', TTestTLSStats.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestTLSStats);
  {$ENDIF}
end.
