unit blowfishCipherTests;

interface

uses
  {$IFDEF FPC}
    fpcunit, testregistry;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestBlowfishCipher = class(TTestCase)
  published
    procedure TestEcbVector;
    procedure TestCbcVector;
    procedure TestCfbVector;
    procedure TestUdpRoundTrip;
  end;

implementation

uses
  SysUtils, slblowfishcipher, mycrypto, slmd5;

function HexToBytes(const aHex: String): TBytes;
var
  i: Integer;
begin
  SetLength(Result, Length(aHex) div 2);
  for i := 0 to High(Result) do
    Result[i] := StrToInt('$' + Copy(aHex, i * 2 + 1, 2));
end;

function BytesToHex(const aBytes: TBytes): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to High(aBytes) do
    Result := Result + IntToHex(aBytes[i], 2);
end;

procedure TTestBlowfishCipher.TestEcbVector;
var
  fKey, fPlainText, fCipherText, fOutput, fIV: TBytes;
  fCipher: TslBlowfishCipher;
begin
  fKey := HexToBytes('0123456789ABCDEF');
  fPlainText := HexToBytes('1111111111111111');
  fCipherText := HexToBytes('61F9C3802281B096');
  SetLength(fOutput, Length(fPlainText));

  fCipher := TslBlowfishCipher.Create(fKey, fIV, sbmECB, True);
  try
    CheckEquals(Length(fPlainText), fCipher.Update(@fPlainText[0], @fOutput[0],
      Length(fPlainText)));
    fCipher.Final;
  finally
    fCipher.Free;
  end;
  CheckEquals(BytesToHex(fCipherText), BytesToHex(fOutput));
end;

procedure TTestBlowfishCipher.TestCbcVector;
var
  fKey, fIV, fPlainText, fCipherText, fOutput: TBytes;
  fCipher: TslBlowfishCipher;
begin
  fKey := HexToBytes('000102030405060708090A0B0C0D0E0F');
  fIV := HexToBytes('0101010101010101');
  fPlainText := HexToBytes('0F0E0C0D0B0A09080706050403020100');
  fCipherText := HexToBytes('39C65006742B62A49F7A40FF69749C0A');
  SetLength(fOutput, Length(fPlainText));

  fCipher := TslBlowfishCipher.Create(fKey, fIV, sbmCBC, True);
  try
    CheckEquals(Length(fPlainText), fCipher.Update(@fPlainText[0], @fOutput[0],
      Length(fPlainText)));
    fCipher.Final;
  finally
    fCipher.Free;
  end;
  CheckEquals(BytesToHex(fCipherText), BytesToHex(fOutput));
end;

procedure TTestBlowfishCipher.TestCfbVector;
var
  fKey, fIV, fPlainText, fCipherText, fOutput: TBytes;
  fCipher: TslBlowfishCipher;
begin
  fKey := HexToBytes('0001020304050607');
  fIV := HexToBytes('0101010101010101');
  fPlainText := HexToBytes('0F0E0C0D0B0A09080706050403020100');
  fCipherText := HexToBytes('27BE8331CDC52DC61724029D302B9358');
  SetLength(fOutput, Length(fPlainText));

  fCipher := TslBlowfishCipher.Create(fKey, fIV, sbmCFB, True);
  try
    CheckEquals(Length(fPlainText), fCipher.Update(@fPlainText[0], @fOutput[0],
      Length(fPlainText)));
    fCipher.Final;
  finally
    fCipher.Free;
  end;
  CheckEquals(BytesToHex(fCipherText), BytesToHex(fOutput));
end;

procedure TTestBlowfishCipher.TestUdpRoundTrip;
const
  fPlainText = 'slftp UDP Blowfish payload';
var
  fCipherText: String;
begin
  MycryptoStart(slMD5String('blowfish-test-key'));
  try
    fCipherText := EncryptUDP(fPlainText);
    CheckEquals(fPlainText, DecryptUDP(fCipherText));
  finally
    MycryptoStop;
  end;
end;

initialization
  {$IFDEF FPC}
    RegisterTest('blowfish cipher', TTestBlowfishCipher.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestBlowfishCipher);
  {$ENDIF}
end.
