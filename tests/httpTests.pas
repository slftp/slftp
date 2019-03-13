unit httpTests;

interface

uses
  {$IFDEF FPC}
    TestFramework,
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility,
  {$ENDIF}
  Base64OpenSSLTests;

type
  TTestHTTP = class(TTestIndyOpenSSL)
  published
    procedure TestIMDB;
    //procedure TestBOM;
    //procedure TestMozilla;
  end;

implementation

uses
  SysUtils, http;

{ TTestHTTP }

procedure TTestHTTP.TestIMDB;
var
  Result: Boolean;
  fURL, fHTML, fErrMsg: String;
  fTryCount: Integer;
begin
  fURL := 'https://www.imdb.com/title/tt6966692/';
  fTryCount := 1;
  Result := HttpGetUrl(fURL, fHTML, fErrMsg, fTryCount);

  CheckTrue(Result, 'The HTTP fetch should work!');
  CheckNotEquals(0, Length(fHTML), 'Length of HTML code should be longer than 0');
  CheckEqualsString('no error', fErrMsg, 'Error message for IMDB is unexpected');
end;

initialization
  {$IFDEF FPC}
    RegisterTest('HTTP Get Tests', TTestHTTP.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestHTTP);
  {$ENDIF}
end.