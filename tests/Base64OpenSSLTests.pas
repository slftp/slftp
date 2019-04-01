unit Base64OpenSSLTests;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility, DUnitX.Assert;
  {$ENDIF}

type
  // base class which should be used whenever the Indy OpenSSL is needed
  TTestIndyOpenSSL = class(TTestCase)
  protected
    {$IFDEF FPC}
      procedure SetUpOnce; override;
      procedure TeardownOnce; override;
    {$ELSE}
      procedure SetUp; override;
      procedure Teardown; override;
    {$ENDIF}
  published
    procedure OpenSSLVersion;
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

{$IFDEF FPC}
  procedure TTestIndyOpenSSL.SetUpOnce;
{$ELSE}
  procedure TTestIndyOpenSSL.SetUp;
{$ENDIF}
begin
  {$IFDEF UNIX}
    // do not try to load sym links first
    IdOpenSSLSetLoadSymLinksFirst(False);
  {$ENDIF}

  try
    CheckTrue(IdSSLOpenSSL.LoadOpenSSLLibrary;, 'IdSSLOpenSSL.LoadOpenSSLLibrary loaded');
  except
    on e: EIdOSSLCouldNotLoadSSLLibrary do
    begin
      {$IFNDEF FPC}DUnitX.Assert.Assert.{$ENDIF}Fail(Format('Failed to load OpenSSL: %s %s', [sLineBreak, IdSSLOpenSSLHeaders.WhichFailedToLoad]));
    end;
    on e: Exception do
    begin
      {$IFNDEF FPC}DUnitX.Assert.Assert.{$ENDIF}Fail(Format('[EXCEPTION] Unexpected error while loading OpenSSL: %s%s %s%s', [sLineBreak, e.ClassName, sLineBreak, e.Message]));
    end;
  end;
end;

{$IFDEF FPC}
  procedure TTestIndyOpenSSL.TeardownOnce;
{$ELSE}
  procedure TTestIndyOpenSSL.Teardown;
{$ENDIF}
begin
  try
    IdSSLOpenSSL.UnLoadOpenSSLLibrary;
  except
    on e: Exception do
    begin
      {$IFNDEF FPC}DUnitX.Assert.Assert.{$ENDIF}Fail(Format('Failed to unload OpenSSL: %s %s', [sLineBreak, e.Message]));
    end;
  end;
end;

procedure TTestIndyOpenSSL.OpenSSLVersion;
var
  fExpectedResultStr, fShortVersion: String;
  {$I slftp.inc}
begin
  fExpectedResultStr := IdSSLOpenSSL.OpenSSLVersion; // e.g. OpenSSL 1.0.2n  7 Dec 2017
  fShortVersion := Copy(fExpectedResultStr, 9, 5);
  CheckEqualsString(lib_OpenSSL, fShortVersion, 'OpenSSL version is wrong');
  SetLength(fExpectedResultStr, 13);
  CheckEqualsString('OpenSSL ' + lib_OpenSSL, fExpectedResultStr, 'OpenSSL version string is wrong');
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
  {$IFDEF UNICODE}
    fInputStr := Utf8Encode('En France, il y a au total 11 fêtes pendant l’année.');
  {$ELSE}
    fInputStr := 'En France, il y a au total 11 fêtes pendant l’année.';
  {$ENDIF}
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

  {$IFDEF UNICODE}
    CheckEqualsString(fExpectedResultStr, Utf8Decode(fOutputStr), 'Base64 decoded string differs');
  {$ELSE}
    CheckEqualsString(fExpectedResultStr, fOutputStr, 'Base64 decoded string differs');
  {$ENDIF}

  // length of fOutputStr and fExpectedResultStr is 56 when using Length()
  CheckEquals(56, fLength, 'Base64 decoded string length differs');
end;

initialization
  {$IFDEF FPC}
    RegisterTest('Base64 OpenSSL', TTestBase64OpenSSL.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestBase64OpenSSL);
  {$ENDIF}
end.