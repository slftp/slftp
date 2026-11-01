unit slblowfishTests;

interface

uses
  {$IFDEF FPC}
    fpcunit, testregistry;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestSlBlowfish = class(TTestCase)
  published
    procedure TestEncryptedStreamRoundTrip;
    procedure TestCompressedStreamRoundTrip;
  end;

implementation

uses
  Classes, SysUtils, slblowfish;

function TestStreamRoundTrip(const aCompress: Boolean): String;
const
  fText = 'slFtp encrypted configuration compatibility test';
var
  fInput, fEncrypted, fOutput: TMemoryStream;
  fKey: array[0..15] of Byte;
  i: Integer;
begin
  Result := '';
  for i := Low(fKey) to High(fKey) do
    fKey[i] := i;
  fInput := TMemoryStream.Create;
  fEncrypted := TMemoryStream.Create;
  fOutput := TMemoryStream.Create;
  try
    fInput.WriteBuffer(fText[1], Length(fText));
    EncryptStreamToStream(fInput, fEncrypted, fKey, aCompress);
    fEncrypted.Position := 0;
    DecryptStreamToStream(fEncrypted, fOutput, fKey, aCompress);
    SetLength(Result, fOutput.Size);
    if fOutput.Size > 0 then
    begin
      fOutput.Position := 0;
      fOutput.ReadBuffer(Result[1], fOutput.Size);
    end;
  finally
    fInput.Free;
    fEncrypted.Free;
    fOutput.Free;
  end;
end;

procedure TTestSlBlowfish.TestEncryptedStreamRoundTrip;
begin
  CheckEquals('slFtp encrypted configuration compatibility test', TestStreamRoundTrip(False));
end;

procedure TTestSlBlowfish.TestCompressedStreamRoundTrip;
begin
  CheckEquals('slFtp encrypted configuration compatibility test', TestStreamRoundTrip(True));
end;

initialization
  {$IFDEF FPC}
    RegisterTest('slblowfish', TTestSlBlowfish.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestSlBlowfish);
  {$ENDIF}
end.
