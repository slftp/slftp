unit slsignals;

interface

procedure slSignal(signalNo: Longint; handler: Pointer);

implementation

uses
{$IFDEF FPC}
  BaseUnix
{$ELSE}
  Libc
{$ENDIF}
;

procedure slSignal(signalNo: Longint; handler: Pointer);
{$IFDEF FPC}
var v: PSigActionRec;
{$ENDIF}
begin
{$IFDEF FPC}
  new(v);
  v^.sa_Handler:=SigActionHandler(handler);
  fillchar(v^.Sa_Mask,sizeof(v^.sa_mask),#0);
  v^.Sa_Flags:=0;
  {$ifdef Linux}               // Linux specific
     v^.Sa_Restorer:=Nil;
  {$endif}

  fpSigaction(signalNo, v, nil);
{$ELSE}
  Signal(signalNo, handler);
{$ENDIF}
end;

end.
