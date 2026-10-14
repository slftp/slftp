{ Temporary diagnostic build. Define DISABLE_QUEUE_DIAGNOSTICS, or comment out
  the DEFINE below, to compile out every hook and the diagnostic unit. }
{$IFNDEF DISABLE_QUEUE_DIAGNOSTICS}
  {$DEFINE QUEUE_DIAGNOSTICS}
{$ENDIF}
