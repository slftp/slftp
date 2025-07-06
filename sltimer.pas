unit sltimer;

interface

uses
  {$ifdef MSWINDOWS}
    Windows,
  {$else}
    BaseUnix, UnixType, ctypes,
  {$endif}
  Classes;

type
  TSLtimer = class
  private
    {$ifdef MSWINDOWS}
    FStart, FEnd: Int64;
    FFrequency: Int64;
    {$else}
    type
      TTimespec = record
        tv_sec: Int64;
        tv_nsec: LongInt;
      end;
    const
      CLOCK_MONOTONIC = 1; // Common on Linux/BSD; adjust if needed
    var
      FStart, FEnd: TTimespec;
    {$endif}
  public
    constructor Create;
    procedure Start;
    procedure Stop;
    function ElapsedMilliseconds: Double;
    function ElapsedMicroseconds: Double;
  end;

implementation

{$ifndef MSWINDOWS}
function clock_gettime(clk_id: cint; var ts: TSLtimer.TTimespec): cint; cdecl;
  external 'c' name 'clock_gettime';
{$endif}

constructor TSLtimer.Create;
begin
  inherited;
  {$ifdef MSWINDOWS}
  QueryPerformanceFrequency(FFrequency);
  {$endif}
end;

procedure TSLtimer.Start;
begin
  {$ifdef MSWINDOWS}
  QueryPerformanceCounter(FStart);
  {$else}
  clock_gettime(CLOCK_MONOTONIC, FStart);
  {$endif}
end;

procedure TSLtimer.Stop;
begin
  {$ifdef MSWINDOWS}
  QueryPerformanceCounter(FEnd);
  {$else}
  clock_gettime(CLOCK_MONOTONIC, FEnd);
  {$endif}
end;

function TSLtimer.ElapsedMilliseconds: Double;
begin
  {$ifdef MSWINDOWS}
  Result := (FEnd - FStart) * 1000 / FFrequency;
  {$else}
  Result := (FEnd.tv_sec - FStart.tv_sec) * 1000 +
            (FEnd.tv_nsec - FStart.tv_nsec) / 1000000;
  {$endif}
end;

function TSLtimer.ElapsedMicroseconds: Double;
begin
  {$ifdef MSWINDOWS}
  Result := (FEnd - FStart) * 1000000 / FFrequency;
  {$else}
  Result := (FEnd.tv_sec - FStart.tv_sec) * 1000000 +
            (FEnd.tv_nsec - FStart.tv_nsec) / 1000;
  {$endif}
end;

end.
