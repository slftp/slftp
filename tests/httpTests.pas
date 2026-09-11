unit httpTests;

interface

uses
  {$IFDEF FPC}
    TestFramework,
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility,
  {$ENDIF}
  slftpUnitTestsSetupIndyOpenSSL;

type
  TTestHTTP = class(TTestIndyOpenSSL)
  published
    procedure TestIMDBHTTPS;
    procedure TestBOMHTTPS;
    procedure TestTVMAZEHTTPS;
  end;

implementation

uses
  SysUtils, StrUtils, http;

{ TTestHTTP }

procedure TTestHTTP.TestIMDBHTTPS;
var
  Result: Boolean;
  fURL, fHTML, fErrMsg: String;
begin
  fURL := 'https://api.tiffara.com/titles/tt6966692';
  Result := HttpGetUrl(fURL, fHTML, fErrMsg);

  CheckEqualsString('', fErrMsg, 'Error message for IMDB is unexpected');
  CheckTrue(Result, 'The HTTP fetch should work!');
  CheckNotEquals(0, Length(fHTML), 'Length of JSON response should be longer than 0');
  CheckTrue(ContainsText(fHTML, 'tt6966692'), 'JSON response should include IMDB ID tt6966692');
  CheckTrue(ContainsText(fHTML, 'Green Book'), 'JSON response should include title Green Book');
end;

procedure TTestHTTP.TestBOMHTTPS;
var
  Result: Boolean;
  fURL, fHTML, fErrMsg: String;
begin
  fURL := 'https://www.boxofficemojo.com/movies/?id=marvel2019.htm';
  Result := HttpGetUrl(fURL, fHTML, fErrMsg);

  CheckEqualsString('', fErrMsg, 'Error message for BOM is unexpected');
  CheckTrue(Result, 'The HTTP fetch should work!');
  CheckNotEquals(0, Length(fHTML), 'Length of HTML code should be longer than 0');
  CheckTrue(ContainsText(fHTML, '<title dir="ltr">Avengers: Endgame - Box Office Mojo</title>'), 'HTML content should include title');
  CheckTrue(ContainsText(fHTML, '<span>3 hr 1 min</span>'), 'HTML content should include Runtime (3hrs 1min)');
end;

procedure TTestHTTP.TestTVMAZEHTTPS;
var
  Result: Boolean;
  fURL, fHTML, fErrMsg: String;
begin
  fURL := 'https://api.tvmaze.com/search/shows?q=Utopia';
  Result := HttpGetUrl(fURL, fHTML, fErrMsg);

  CheckEqualsString('', fErrMsg, 'Error message for TVMAZE is unexpected');
  CheckTrue(Result, 'The HTTP fetch should work!');
  CheckNotEquals(0, Length(fHTML), 'Length of HTML code should be longer than 0');
  CheckTrue(ContainsText(fHTML, '{"id":64,"url":"https://www.tvmaze.com/shows/64/utopia","name":"Utopia",'), 'HTML content should include ID 64 - Utopia');
  CheckTrue(ContainsText(fHTML, '"country":{"name":"Australia","code":"AU"'), 'HTML content should include country Australia AU');
end;

initialization
  {$IFDEF FPC}
    RegisterTest('HTTP Get Tests', TTestHTTP.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestHTTP);
  {$ENDIF}
end.
