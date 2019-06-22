unit mystringsTests.Base64;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility, DUnitX.Assert;
  {$ENDIF}

type
  TTestBase64 = class(TTestCase)
  published
    // test encoding
    procedure Base64Encode1;
    procedure Base64Encode2;
    procedure Base64Encode3;
    procedure Base64Encode4;
    procedure Base64Encode5;
    procedure Base64Encode6;
    procedure Base64Encode7;
    procedure Base64Encode8;
    // test decoding
    procedure Base64Decode1;
    procedure Base64Decode2;
    procedure Base64Decode3;
    procedure Base64Decode4;
    procedure Base64Decode5;
    procedure Base64Decode6;
    procedure Base64Decode7;
    procedure Base64Decode8;
  end;

implementation

uses
  SysUtils,
  {$IFDEF UNICODE}
    NetEncoding,
  {$ENDIF}
  mystrings;

{ TTestBase64 }

procedure TTestBase64.Base64Encode1;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Hello';
  fExpectedResultStr := 'SGVsbG8=';
  fOutputStr := DoBase64Encode(fInputStr);

  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Base64 encoded string differs');
  CheckEquals(8, fOutputStr.Length, 'Base64 encoded string length differs');
end;

procedure TTestBase64.Base64Encode2;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'This is just an example test!!';
  fExpectedResultStr := 'VGhpcyBpcyBqdXN0IGFuIGV4YW1wbGUgdGVzdCEh';
  fOutputStr := DoBase64Encode(fInputStr);

  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Base64 encoded string differs');
  CheckEquals(40, fOutputStr.Length, 'Base64 encoded string length differs');
end;

procedure TTestBase64.Base64Encode3;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := '';
  fExpectedResultStr := '';
  fOutputStr := DoBase64Encode(fInputStr);

  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Base64 encoded string differs');
  CheckEquals(0, fOutputStr.Length, 'Empty base64 encoded string length should be 0');
end;

procedure TTestBase64.Base64Encode4;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'To encode binaries (like images, documents, etc.) upload your data via the file encode form below.';
  fExpectedResultStr := 'VG8gZW5jb2RlIGJpbmFyaWVzIChsaWtlIGltYWdlcywgZG9jdW1lbnRzLCBldGMuKSB1cGxvYWQgeW91ciBkYXRhIHZpYSB0aGUgZmlsZSBlbmNvZGUgZm9ybSBiZWxvdy4=';
  fOutputStr := DoBase64Encode(fInputStr);

  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Base64 encoded string differs');
  CheckEquals(132, fOutputStr.Length, 'Base64 encoded string length differs');
end;

procedure TTestBase64.Base64Encode5;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := '192425';
  fExpectedResultStr := 'MTkyNDI1';
  fOutputStr := DoBase64Encode(fInputStr);

  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Base64 encoded string differs');
  CheckEquals(8, fOutputStr.Length, 'Base64 encoded string length differs');
end;

procedure TTestBase64.Base64Encode6;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'En France, il y a au total 11 fêtes pendant l’année.';
  fExpectedResultStr := 'RW4gRnJhbmNlLCBpbCB5IGEgYXUgdG90YWwgMTEgZsOqdGVzIHBlbmRhbnQgbOKAmWFubsOpZS4=';
  fOutputStr := DoBase64Encode(fInputStr);

  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Base64 encoded string differs');
  CheckEquals(76, fOutputStr.Length, 'Base64 encoded string length differs');
end;

procedure TTestBase64.Base64Encode7;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa. ' + 'Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus.';
  fExpectedResultStr := 'TG9yZW0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVlciBhZGlwaXNjaW5nIGVsaXQuIEFlbmVhbiBjb21tb2RvIGxpZ3VsYSBlZ2V0IGRvbG9yLiBBZW5lYW4' + 'gbWFzc2EuIEN1bSBzb2NpaXMgbmF0b3F1ZSBwZW5hdGlidXMgZXQgbWFnbmlzIGRpcyBwYXJ0dXJpZW50IG1vbnRlcywgbmFzY2V0dXIgcmlkaWN1bHVzIG11cy4=';
  fOutputStr := DoBase64Encode(fInputStr);

  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Base64 encoded string differs');
  CheckEquals(256, fOutputStr.Length, 'Base64 encoded string length differs');
end;

procedure TTestBase64.Base64Encode8;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur rid' + 'iculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium quis, sem.';
  fExpectedResultStr := 'TG9yZW0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVlciBhZGlwaXNjaW5nIGVsaXQuIEFlbmVhbiBjb21tb2RvIGxpZ3VsYSBlZ2V0IGRvbG9yLiBBZW5lYW4gbWFzc2EuIEN1bSBzb2NpaXMgbmF0b3F1ZSBwZW5hdGlidX' + 'MgZXQgbWFnbmlzIGRpcyBwYXJ0dXJpZW50IG1vbnRlcywgbmFzY2V0dXIgcmlkaWN1bHVzIG11cy4gRG9uZWMgcXVhbSBmZWxpcywgdWx0cmljaWVzIG5lYywgcGVsbGVudGVzcXVlIGV1LCBwcmV0aXVtIHF1aXMsIHNlbS4=';
  fOutputStr := DoBase64Encode(fInputStr);

  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Base64 encoded string differs');
  CheckEquals(348, fOutputStr.Length, 'Base64 encoded string length differs');
end;

procedure TTestBase64.Base64Decode1;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'SGVsbG8=';
  fExpectedResultStr := 'Hello';
  fOutputStr := DoBase64DecodeToString(fInputStr);

  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Base64 decoded string differs');
  CheckEquals(5, fOutputStr.Length, 'Base64 decoded string length differs');
end;

procedure TTestBase64.Base64Decode2;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'V2UgdGVzdCBvdXIgZnVuY3Rpb25zIG5vdyEhITExMQ==';
  fExpectedResultStr := 'We test our functions now!!!111';
  fOutputStr := DoBase64DecodeToString(fInputStr);

  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Base64 decoded string differs');
  CheckEquals(31, fOutputStr.Length, 'Base64 decoded string length differs');
end;

procedure TTestBase64.Base64Decode3;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := '';
  fExpectedResultStr := '';
  fOutputStr := DoBase64DecodeToString(fInputStr);

  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Base64 decoded string differs');
  CheckEquals(0, fOutputStr.Length, 'Empty base64 decoded string length should be 0');
end;

procedure TTestBase64.Base64Decode4;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'VG8gZW5jb2RlIGJpbmFyaWVzIChsaWtlIGltYWdlcywgZG9jdW1lbnRzLCBldGMuKSB1cGxvYWQgeW91ciBkYXRhIHZpYSB0aGUgZmlsZSBlbmNvZGUgZm9ybSBiZWxvdy4=';
  fExpectedResultStr := 'To encode binaries (like images, documents, etc.) upload your data via the file encode form below.';
  fOutputStr := DoBase64DecodeToString(fInputStr);

  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Base64 decoded string differs');
  CheckEquals(98, fOutputStr.Length, 'Base64 decoded string length differs');
end;

procedure TTestBase64.Base64Decode5;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'MTkyNDI1';
  fExpectedResultStr := '192425';
  fOutputStr := DoBase64DecodeToString(fInputStr);

  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Base64 decoded string differs');
  CheckEquals(6, fOutputStr.Length, 'Base64 decoded string length differs');
end;

procedure TTestBase64.Base64Decode6;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'RW4gRnJhbmNlLCBpbCB5IGEgYXUgdG90YWwgMTEgZsOqdGVzIHBlbmRhbnQgbOKAmWFubsOpZS4=';
  fExpectedResultStr := 'En France, il y a au total 11 fêtes pendant l’année.';
  fOutputStr := DoBase64DecodeToString(fInputStr);

  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Base64 decoded string differs');
  {$IFDEF UNICODE}
    CheckEquals(52, fOutputStr.Length, 'Base64 decoded string length differs');
  {$ELSE}
    // AnsiString has a different, 4 chars longer length
    CheckEquals(56, fOutputStr.Length, 'Base64 decoded string length differs');
  {$ENDIF}
end;

procedure TTestBase64.Base64Decode7;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'TG9yZW0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVlciBhZGlwaXNjaW5nIGVsaXQuIEFlbmVhbiBjb21tb2RvIGxpZ3VsYSBlZ2V0IGRvbG9yLiBBZW5lYW4' + 'gbWFzc2EuIEN1bSBzb2NpaXMgbmF0b3F1ZSBwZW5hdGlidXMgZXQgbWFnbmlzIGRpcyBwYXJ0dXJpZW50IG1vbnRlcywgbmFzY2V0dXIgcmlkaWN1bHVzIG11cy4=';
  fExpectedResultStr := 'Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa. Cum sociis nat' + 'oque penatibus et magnis dis parturient montes, nascetur ridiculus mus.';
  fOutputStr := DoBase64DecodeToString(fInputStr);

  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Base64 decoded string differs');
  CheckEquals(191, fOutputStr.Length, 'Base64 decoded string length differs');
end;

procedure TTestBase64.Base64Decode8;
var
  fInputStr, fOutputStr, fExpectedResultStr: String;
begin
  fInputStr := 'TG9yZW0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVlciBhZGlwaXNjaW5nIGVsaXQuIEFlbmVhbiBjb21tb2RvIGxpZ3VsYSBlZ2V0IGRvbG9yLiBBZW5lYW4gbWFzc2EuIEN1bSBzb2NpaXMgbmF0b3F1ZSBwZW5hdGlidX' + 'MgZXQgbWFnbmlzIGRpcyBwYXJ0dXJpZW50IG1vbnRlcywgbmFzY2V0dXIgcmlkaWN1bHVzIG11cy4gRG9uZWMgcXVhbSBmZWxpcywgdWx0cmljaWVzIG5lYywgcGVsbGVudGVzcXVlIGV1LCBwcmV0aXVtIHF1aXMsIHNlbS4=';
  fExpectedResultStr := 'Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur rid' + 'iculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium quis, sem.';
  fOutputStr := DoBase64DecodeToString(fInputStr);

  CheckEqualsString(fExpectedResultStr, fOutputStr, 'Base64 decoded string differs');
  CheckEquals(260, fOutputStr.Length, 'Base64 decoded string length differs');
end;

initialization
  {$IFDEF FPC}
    RegisterTest('mystrings Base64', TTestBase64.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestBase64);
  {$ENDIF}
end.