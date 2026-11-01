unit blowfishdiag;

interface

type
  { Blowfish call paths included in the diagnostic summary. }
  TBlowfishDiagnosticMode = (
    bdmIrcEcbEncrypt, //< IRC Blowfish ECB encryption
    bdmIrcEcbDecrypt, //< IRC Blowfish ECB decryption
    bdmIrcCbcEncrypt, //< IRC Blowfish CBC encryption
    bdmIrcCbcDecrypt, //< IRC Blowfish CBC decryption
    bdmUdpCfbEncrypt, //< UDP Blowfish CFB encryption
    bdmUdpCfbDecrypt, //< UDP Blowfish CFB decryption
    bdmFileCbcEncrypt, //< encrypted file CBC encryption
    bdmFileCbcDecrypt  //< encrypted file CBC decryption
  );

  TBlowfishDiagnosticSnapshot = record
    Calls: Integer; //< number of operations recorded
    EmptyPayloads: Integer; //< operations with no input bytes
    SmallPayloads: Integer; //< operations with 1 to 8 input bytes
    MediumPayloads: Integer; //< operations with 9 to 64 input bytes
    LargePayloads: Integer; //< operations with more than 64 input bytes
  end;

/// Record one Blowfish use without storing input, output, key, site or file contents.
// - @param(aMode the high-level Blowfish operation)
// - @param(aKeyLength the key length in bytes)
// - @param(aPayloadLength the input buffer length in bytes at the call site)
// - @note Payloads are grouped as 0, 1..8, 9..64 and >64 bytes. For decrypt
//   operations this is the ciphertext buffer length.
procedure RecordBlowfishDiagnosticUse(aMode: TBlowfishDiagnosticMode;
  aKeyLength, aPayloadLength: Integer);

/// Return an approximate snapshot of counters for one Blowfish use category.
// - @param(aMode the high-level Blowfish operation)
// - @returns(Counters read while other threads may still update them)
function GetBlowfishDiagnosticSnapshot(
  aMode: TBlowfishDiagnosticMode): TBlowfishDiagnosticSnapshot;

implementation

uses
  debugunit, mormot.core.base;

const
  ssection = 'blowfishdiag';
  DiagnosticReportInterval = 10000;
  ModeNames: array[TBlowfishDiagnosticMode] of String = (
    'irc-ecb-encrypt',
    'irc-ecb-decrypt',
    'irc-cbc-encrypt',
    'irc-cbc-decrypt',
    'udp-cfb-encrypt',
    'udp-cfb-decrypt',
    'file-cbc-encrypt',
    'file-cbc-decrypt'
  );

var
  glDiagnosticCounters: array[TBlowfishDiagnosticMode] of TBlowfishDiagnosticSnapshot;
  glObservedKeyLengths: array[TBlowfishDiagnosticMode, 0..57] of Integer;

procedure RecordBlowfishDiagnosticUse(aMode: TBlowfishDiagnosticMode;
  aKeyLength, aPayloadLength: Integer);
var
  fSnapshot: TBlowfishDiagnosticSnapshot;
  fCallCount: Integer;
  fKeyLengthIndex: Integer;
  fKeyLengthCount: Integer;
  fBucketCount: Integer;
  fNewObservation: Boolean;
begin
  fCallCount := InterlockedIncrement(glDiagnosticCounters[aMode].Calls);
  fNewObservation := False;
  if aKeyLength > 56 then
    fKeyLengthIndex := 57
  else if aKeyLength < 0 then
    fKeyLengthIndex := 0
  else
    fKeyLengthIndex := aKeyLength;
  fKeyLengthCount := InterlockedIncrement(glObservedKeyLengths[aMode, fKeyLengthIndex]);
  if fKeyLengthCount = 1 then
  begin
    fNewObservation := True;
    Debug(dpMessage, ssection,
      '[BLOWFISH-DIAG] mode=%s observed_key_bytes=%d', [ModeNames[aMode], aKeyLength]);
  end;
  case aPayloadLength of
    0:
      fBucketCount := InterlockedIncrement(glDiagnosticCounters[aMode].EmptyPayloads);
    1..8:
      fBucketCount := InterlockedIncrement(glDiagnosticCounters[aMode].SmallPayloads);
    9..64:
      fBucketCount := InterlockedIncrement(glDiagnosticCounters[aMode].MediumPayloads);
  else
    fBucketCount := InterlockedIncrement(glDiagnosticCounters[aMode].LargePayloads);
  end;

  if fBucketCount = 1 then
    fNewObservation := True;

  if fNewObservation or ((fCallCount mod DiagnosticReportInterval) = 0) then
  begin
    fSnapshot := GetBlowfishDiagnosticSnapshot(aMode);
    Debug(dpMessage, ssection,
      '[BLOWFISH-DIAG] mode=%s calls=%d key_bytes=%d input_buckets=0:%d,1-8:%d,9-64:%d,>64:%d',
      [ModeNames[aMode], fSnapshot.Calls, aKeyLength, fSnapshot.EmptyPayloads,
       fSnapshot.SmallPayloads, fSnapshot.MediumPayloads, fSnapshot.LargePayloads]);
  end;
end;

function GetBlowfishDiagnosticSnapshot(
  aMode: TBlowfishDiagnosticMode): TBlowfishDiagnosticSnapshot;
begin
  Result := glDiagnosticCounters[aMode];
end;

end.
