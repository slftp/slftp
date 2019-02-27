unit Base64OpenSSLTests;

interface

uses
  TestFramework;

type
  TTestBase64OpenSSL = class(TTestCase)
  protected
    procedure SetUpOnce; override;
    procedure TeardownOnce; override;
  published
    // test encoding
    procedure Base64OpenSSLEncode1;
    procedure Base64OpenSSLEncode2;
    procedure Base64OpenSSLEncode3;
    // test decoding
    procedure Base64OpenSSLDecode1;
    procedure Base64OpenSSLDecode2;
    procedure Base64OpenSSLDecode3;
  end;

implementation

uses
  SysUtils, IdSSLOpenSSL, IdSSLOpenSSLHeaders, Base64OpenSSL;

{ TTestBase64OpenSSL }

procedure TTestBase64OpenSSL.SetUpOnce;
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

procedure TTestBase64OpenSSL.TeardownOnce;
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
  CheckNotEquals(-101, fLength, 'Base64 encoded should return error code -101');
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
  CheckEquals(5, fLength, 'Base64 decoded string length differs');
end;

procedure TTestBase64OpenSSL.Base64OpenSSLDecode3;
var
  fInputStr, fOutputStr, fExpectedResultStr: {$IFDEF UNICODE}RawByteString{$ELSE}String{$ENDIF};
  fLength: integer;
begin
  fInputStr := '';
  fExpectedResultStr := '61';
  fLength := DoBase64Decode(fInputStr, fOutputStr);

  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Base64 decoded string differs');
  CheckNotEquals(-1301, fLength, 'Base64 decoded should return error code -101');
end;

initialization
  RegisterTest(TTestBase64OpenSSL.Suite);
end.