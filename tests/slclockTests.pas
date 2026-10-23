unit slclockTests;

interface

{$IF Defined(FPC) and Defined(LINUX)}
uses
  fpcunit, testregistry;

type
  { Checks the Linux/FPC clock redirection and real event timeout duration. }
  TTestSLClock = class(TTestCase)
  private
    procedure _CheckClock(const aClockId: LongInt);
  published
    { Verifies RTL monotonic readings stay between precise libc readings. }
    procedure TestMonotonicClockPrecision;
    { Verifies RTL realtime readings stay between precise libc readings. }
    procedure TestRealtimeClockPrecision;
    { Checks unsignaled events wait for positive 1 ms and 5 ms timeouts. }
    procedure TestPositiveEventTimeout;
  end;
{$IFEND}

implementation

{$IF Defined(FPC) and Defined(LINUX)}
uses
  SysUtils, SyncObjs, UnixType, Linux, mormot.core.os;

{ Bypass the RTL redirection so these tests compare against the requested clock. }
function _PreciseClockGetTime(aClockId: LongInt; aTime: UnixType.PTimeSpec): LongInt;
  cdecl; external 'c' name 'clock_gettime';

function _ClockNanoseconds(const aTime: UnixType.TTimeSpec): Int64;
begin
  Result := Int64(aTime.tv_sec) * 1000000000 + Int64(aTime.tv_nsec);
end;

function _PreciseClockNanoseconds(const aClockId: LongInt): Int64;
var
  fTime: UnixType.TTimeSpec;
begin
  if _PreciseClockGetTime(aClockId, @fTime) <> 0 then
    raise Exception.CreateFmt('Precise clock_gettime failed for clock %d', [aClockId]);
  Result := _ClockNanoseconds(fTime);
end;

procedure TTestSLClock._CheckClock(const aClockId: LongInt);
var
  fBefore, fAfter, fRTL: Int64;
  fTime: UnixType.TTimeSpec;
  i: Integer;
begin
  for i := 1 to 20 do
  begin
    fBefore := _PreciseClockNanoseconds(aClockId);
    CheckEquals(0, Linux.clock_gettime(aClockId, @fTime));
    fAfter := _PreciseClockNanoseconds(aClockId);
    fRTL := _ClockNanoseconds(fTime);
    CheckTrue(fRTL >= fBefore,
      Format('RTL clock %d trails the precise clock by %d ns', [aClockId, fBefore - fRTL]));
    CheckTrue(fRTL <= fAfter, 'RTL clock must use the same clock as libc');
  end;
end;

procedure TTestSLClock.TestMonotonicClockPrecision;
begin
  _CheckClock(Linux.CLOCK_MONOTONIC);
end;

procedure TTestSLClock.TestRealtimeClockPrecision;
begin
  _CheckClock(Linux.CLOCK_REALTIME);
end;

procedure TTestSLClock.TestPositiveEventTimeout;
const
  fTimeouts: array[0..1] of Cardinal = (1, 5);
var
  fEvent: TEvent;
  fBefore, fElapsedNs, fRequestedNs: Int64;
  i, j: Integer;
begin
  fEvent := TEvent.Create(nil, False, False, '');
  try
    for i := 1 to 20 do
      for j := Low(fTimeouts) to High(fTimeouts) do
      begin
        fBefore := _PreciseClockNanoseconds(Linux.CLOCK_MONOTONIC);
        CheckEquals(Ord(wrTimeout), Ord(fEvent.WaitFor(fTimeouts[j])));
        fElapsedNs := _PreciseClockNanoseconds(Linux.CLOCK_MONOTONIC) - fBefore;
        fRequestedNs := Int64(fTimeouts[j]) * 1000000;
        CheckTrue(fElapsedNs >= fRequestedNs - 100000,
          Format('%d ms timeout returned after %d ns (100 us tolerance)',
            [fTimeouts[j], fElapsedNs]));
      end;
  finally
    fEvent.Free;
  end;
end;

initialization
  RegisterTest('slclock', TTestSLClock.Suite);
{$IFEND}
end.
