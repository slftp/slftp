unit sltlsstats;

interface

uses SyncObjs;

type
  { Purpose of the measured operation; downloads are classified by extension. }
  TTLSStatsKind = (tskLogin, tskSFV, tskNFO, tskOther);
  { Handshake result, including cancellation separately from network failures. }
  TTLSHandshakeOutcome = (thoError, thoSuccess, thoTimeout, thoCancelled);
  { One completed client or server handshake call, measured with a monotonic clock. }
  TTLSHandshakeSample = record
    Outcome: TTLSHandshakeOutcome; //< Result of the call
    ElapsedMs: Int64; //< Wall time including failure cleanup
  end;
  { Cumulative counters for one purpose since the site was created. }
  TTLSStatsCounters = record
    Handshakes, Successes, Errors, Timeouts, Cancelled: Int64; //< Disjoint outcomes
    HandshakeMs, HandshakeMaxMs: Int64; //< Total and maximum handshake wall time
    Downloads, DownloadSuccesses, DownloadErrors: Int64; //< Admitted Leechfile calls
    DownloadMs, DownloadMaxMs: Int64; //< Total and maximum whole Leechfile wall time
    RetryErrors, RetryWaits: Int64; //< Enqueued retries after failure or not-ready/capacity
  end;
  { Consistent copy of every purpose, taken under a single short lock. }
  TTLSStatsSnapshot = record
    WindowMs: Int64; //< Time since the site statistics were created
    Counters: array[TTLSStatsKind] of TTLSStatsCounters; //< Cumulative counters
  end;
  { In-memory per-site diagnostic totals. No network I/O or formatting under the lock. }
  TSiteTLSStats = class
  private
    FLock: TCriticalSection;
    FStartedAt: Int64;
    FCounters: array[TTLSStatsKind] of TTLSStatsCounters;
  public
    constructor Create;
    destructor Destroy; override;
    { Records one completed handshake, including failures. }
    procedure AddHandshake(const aKind: TTLSStatsKind; const aSample: TTLSHandshakeSample);
    { Records an admitted download; duration includes its TLS handshake. }
    procedure AddDownload(const aKind: TTLSStatsKind; const aSuccess: Boolean; const aElapsedMs: Int64);
    { Counts a retry only after a new SFV/NFO task was enqueued. }
    procedure AddRetry(const aKind: TTLSStatsKind; const aAfterError: Boolean);
    { Returns cumulative counters without resetting them. }
    function Snapshot: TTLSStatsSnapshot;
  end;

{ Maps a metadata filename to a purpose without retaining the filename. }
function TLSDownloadKind(const aFilename: String): TTLSStatsKind;
{ Formats one snapshot row; no session resumption is attempted in this test variant. }
function FormatTLSStats(const aKind: TTLSStatsKind; const aCounters: TTLSStatsCounters): String;

implementation

uses SysUtils, mormot.core.os;

constructor TSiteTLSStats.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FStartedAt := GetTickCount64;
end;

destructor TSiteTLSStats.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TSiteTLSStats.AddHandshake(const aKind: TTLSStatsKind; const aSample: TTLSHandshakeSample);
begin
  FLock.Enter;
  try
    with FCounters[aKind] do
    begin
      Inc(Handshakes);
      case aSample.Outcome of
        thoSuccess: Inc(Successes);
        thoError: Inc(Errors);
        thoTimeout: Inc(Timeouts);
        thoCancelled: Inc(Cancelled);
      end;
      Inc(HandshakeMs, aSample.ElapsedMs);
      if aSample.ElapsedMs > HandshakeMaxMs then
        HandshakeMaxMs := aSample.ElapsedMs;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TSiteTLSStats.AddDownload(const aKind: TTLSStatsKind; const aSuccess: Boolean; const aElapsedMs: Int64);
begin
  FLock.Enter;
  try
    with FCounters[aKind] do
    begin
      Inc(Downloads);
      if aSuccess then
        Inc(DownloadSuccesses)
      else
        Inc(DownloadErrors);
      Inc(DownloadMs, aElapsedMs);
      if aElapsedMs > DownloadMaxMs then
        DownloadMaxMs := aElapsedMs;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TSiteTLSStats.AddRetry(const aKind: TTLSStatsKind; const aAfterError: Boolean);
begin
  FLock.Enter;
  try
    if aAfterError then
      Inc(FCounters[aKind].RetryErrors)
    else
      Inc(FCounters[aKind].RetryWaits);
  finally
    FLock.Leave;
  end;
end;

function TSiteTLSStats.Snapshot: TTLSStatsSnapshot;
var
  fKind: TTLSStatsKind;
begin
  FLock.Enter;
  try
    Result.WindowMs := Int64(GetTickCount64) - FStartedAt;
    for fKind := Low(TTLSStatsKind) to High(TTLSStatsKind) do
      Result.Counters[fKind] := FCounters[fKind];
  finally
    FLock.Leave;
  end;
end;

function TLSDownloadKind(const aFilename: String): TTLSStatsKind;
var
  fExtension: String;
begin
  fExtension := ExtractFileExt(aFilename);
  if SameText(fExtension, '.sfv') then
    Result := tskSFV
  else if SameText(fExtension, '.nfo') then
    Result := tskNFO
  else
    Result := tskOther;
end;

function FormatTLSStats(const aKind: TTLSStatsKind; const aCounters: TTLSStatsCounters): String;
const
  cNames: array[TTLSStatsKind] of String = ('login', 'sfv', 'nfo', 'other');
begin
  with aCounters do
    Result := Format('kind=%s hs=%d ok=%d err=%d timeout=%d cancelled=%d hs_ms=%d hs_max_ms=%d ' +
      'dl=%d dl_ok=%d dl_err=%d dl_ms=%d dl_max_ms=%d retry_err=%d retry_wait=%d',
      [cNames[aKind], Handshakes, Successes, Errors, Timeouts, Cancelled, HandshakeMs, HandshakeMaxMs,
       Downloads, DownloadSuccesses, DownloadErrors, DownloadMs, DownloadMaxMs, RetryErrors, RetryWaits]);
end;

end.
