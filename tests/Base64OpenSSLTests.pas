unit Base64OpenSSLTests;

interface

uses
  TestFramework;

type
  // base class which should be used whenever the Indy OpenSSL is needed
  TTestIndyOpenSSL = class(TTestCase)
  protected
    procedure SetUpOnce; override;
    procedure TeardownOnce; override;
  end;

type
  TTestBase64OpenSSL = class(TTestIndyOpenSSL)
  published
    // test encoding
    procedure Base64OpenSSLEncode1;
    procedure Base64OpenSSLEncode2;
    procedure Base64OpenSSLEncode3;
    procedure Base64OpenSSLEncode4;
    procedure Base64OpenSSLEncode5;
    procedure Base64OpenSSLEncode6;
    // test decoding
    procedure Base64OpenSSLDecode1;
    procedure Base64OpenSSLDecode2;
    procedure Base64OpenSSLDecode3;
    procedure Base64OpenSSLDecode4;
    procedure Base64OpenSSLDecode5;
    procedure Base64OpenSSLDecode6;
  end;

implementation

uses
  SysUtils, IdSSLOpenSSL, IdSSLOpenSSLHeaders, Base64OpenSSL;

{ TTestIndyOpenSSL }

procedure TTestIndyOpenSSL.SetUpOnce;
begin
  {$IFDEF UNIX}
    // do not try to load sym links first
    IdOpenSSLSetLoadSymLinksFirst(False);
  {$ENDIF}

  try
    CheckTrue(IdSSLOpenSSLHeaders.Load, 'IdSSLOpenSSLHeaders loaded');
  except
    on e: EIdOSSLCouldNotLoadSSLLibrary do
    begin
      Fail(Format('Failed to load OpenSSL: %s %s', [sLineBreak, IdSSLOpenSSLHeaders.WhichFailedToLoad]));
    end;
    on e: Exception do
    begin
      Fail(Format('[EXCEPTION] Unexpected error while loading OpenSSL: %s%s %s%s', [sLineBreak, e.ClassName, sLineBreak, e.Message]));
    end;
  end;
end;

procedure TTestIndyOpenSSL.TeardownOnce;
begin
  try
    IdSSLOpenSSLHeaders.Unload;
  except
    on e: Exception do
    begin
      Fail(Format('Failed to unload OpenSSL: %s %s', [sLineBreak, e.Message]));
    end;
  end;
end;

{ TTestBase64OpenSSL }

procedure TTestBase64OpenSSL.Base64OpenSSLEncode1;
var
  fInputStr, fOutputStr, fExpectedResultStr: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF};
  fLength: integer;
begin
  fInputStr := 'Hello';
  fExpectedResultStr := 'SGVsbG8=';
  fLength := DoBase64Encode(fInputStr, fOutputStr);

  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Base64 encoded string differs');
  CheckEquals(8, fLength, 'Base64 encoded string length differs');
end;

procedure TTestBase64OpenSSL.Base64OpenSSLEncode2;
var
  fInputStr, fOutputStr, fExpectedResultStr: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF};
  fLength: integer;
begin
  fInputStr := 'This is just an example test!!';
  fExpectedResultStr := 'VGhpcyBpcyBqdXN0IGFuIGV4YW1wbGUgdGVzdCEh';
  fLength := DoBase64Encode(fInputStr, fOutputStr);

  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Base64 encoded string differs');
  CheckEquals(40, fLength, 'Base64 encoded string length differs');
end;

procedure TTestBase64OpenSSL.Base64OpenSSLEncode3;
var
  fInputStr, fOutputStr, fExpectedResultStr: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF};
  fLength: integer;
begin
  fInputStr := '';
  fExpectedResultStr := '';
  fLength := DoBase64Encode(fInputStr, fOutputStr);

  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Base64 encoded string differs');
  CheckEquals(0, fLength, 'Empty base64 encoded string length should be 0');
end;

procedure TTestBase64OpenSSL.Base64OpenSSLEncode4;
var
  fInputStr, fOutputStr, fExpectedResultStr: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF};
  fLength: integer;
begin
  fInputStr := 'To encode binaries (like images, documents, etc.) upload your data via the file encode form below.';
  fExpectedResultStr := 'VG8gZW5jb2RlIGJpbmFyaWVzIChsaWtlIGltYWdlcywgZG9jdW1lbnRzLCBldGMuKSB1cGxvYWQgeW91ciBkYXRhIHZpYSB0aGUgZmlsZSBlbmNvZGUgZm9ybSBiZWxvdy4=';
  fLength := DoBase64Encode(fInputStr, fOutputStr);

  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Base64 encoded string differs');
  CheckEquals(132, fLength, 'Base64 encoded string length differs');
end;

procedure TTestBase64OpenSSL.Base64OpenSSLEncode5;
var
  fInputStr, fOutputStr, fExpectedResultStr: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF};
  fLength: integer;
begin
  fInputStr := '192425';
  fExpectedResultStr := 'MTkyNDI1';
  fLength := DoBase64Encode(fInputStr, fOutputStr);

  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Base64 encoded string differs');
  CheckEquals(8, fLength, 'Base64 encoded string length differs');
end;

procedure TTestBase64OpenSSL.Base64OpenSSLEncode6;
var
  fInputStr, fOutputStr, fExpectedResultStr: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF};
  fLength: integer;
begin
  fInputStr := 'En France, il y a au total 11 fêtes pendant l’année.';
  fExpectedResultStr := 'RW4gRnJhbmNlLCBpbCB5IGEgYXUgdG90YWwgMTEgZsOqdGVzIHBlbmRhbnQgbOKAmWFubsOpZS4=';
  fLength := DoBase64Encode(fInputStr, fOutputStr);

  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Base64 encoded string differs');
  CheckEquals(76, fLength, 'Base64 encoded string length differs');
end;

procedure TTestBase64OpenSSL.Base64OpenSSLDecode1;
var
  fInputStr, fOutputStr, fExpectedResultStr: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF};
  fLength: integer;
begin
  fInputStr := 'SGVsbG8=';
  fExpectedResultStr := 'Hello';
  fLength := DoBase64Decode(fInputStr, fOutputStr);

  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Base64 decoded string differs');
  CheckEquals(5, fLength, 'Base64 decoded string length differs');
end;

procedure TTestBase64OpenSSL.Base64OpenSSLDecode2;
var
  fInputStr, fOutputStr, fExpectedResultStr: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF};
  fLength: integer;
begin
  fInputStr := 'V2UgdGVzdCBvdXIgZnVuY3Rpb25zIG5vdyEhITExMQ==';
  fExpectedResultStr := 'We test our functions now!!!111';
  fLength := DoBase64Decode(fInputStr, fOutputStr);

  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Base64 decoded string differs');
  CheckEquals(31, fLength, 'Base64 decoded string length differs');
end;

procedure TTestBase64OpenSSL.Base64OpenSSLDecode3;
var
  fInputStr, fOutputStr, fExpectedResultStr: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF};
  fLength: integer;
begin
  fInputStr := '';
  fExpectedResultStr := '';
  fLength := DoBase64Decode(fInputStr, fOutputStr);

  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Base64 decoded string differs');
  CheckEquals(0, fLength, 'Empty base64 decoded string length should be 0');
end;

procedure TTestBase64OpenSSL.Base64OpenSSLDecode4;
var
  fInputStr, fOutputStr, fExpectedResultStr: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF};
  fLength: integer;
begin
  fInputStr := 'VG8gZW5jb2RlIGJpbmFyaWVzIChsaWtlIGltYWdlcywgZG9jdW1lbnRzLCBldGMuKSB1cGxvYWQgeW91ciBkYXRhIHZpYSB0aGUgZmlsZSBlbmNvZGUgZm9ybSBiZWxvdy4=';
  fExpectedResultStr := 'To encode binaries (like images, documents, etc.) upload your data via the file encode form below.';
  fLength := DoBase64Decode(fInputStr, fOutputStr);

  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Base64 decoded string differs');
  CheckEquals(98, fLength, 'Base64 decoded string length differs');
end;

procedure TTestBase64OpenSSL.Base64OpenSSLDecode5;
var
  fInputStr, fOutputStr, fExpectedResultStr: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF};
  fLength: integer;
begin
  fInputStr := 'MTkyNDI1';
  fExpectedResultStr := '192425';
  fLength := DoBase64Decode(fInputStr, fOutputStr);

  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Base64 decoded string differs');
  CheckEquals(6, fLength, 'Base64 decoded string length differs');
end;

procedure TTestBase64OpenSSL.Base64OpenSSLDecode6;
var
  fInputStr, fOutputStr, fExpectedResultStr: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF};
  fLength: integer;
begin
  fInputStr := 'RW4gRnJhbmNlLCBpbCB5IGEgYXUgdG90YWwgMTEgZsOqdGVzIHBlbmRhbnQgbOKAmWFubsOpZS4=';
  fExpectedResultStr := 'En France, il y a au total 11 fêtes pendant l’année.';
  fLength := DoBase64Decode(fInputStr, fOutputStr);

  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Base64 decoded string differs');
  // length of fOutputStr and fExpectedResultStr is 56 when using Length()
  CheckEquals(56, fLength, 'Base64 decoded string length differs');
end;

initialization
  RegisterTest('Base64 OpenSSL', TTestBase64OpenSSL.Suite);
end.