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
    procedure TestIMDBHTTP;
    procedure TestIMDBHTTPS;
    procedure TestBOMHTTP;
    procedure TestBOMHTTPS;
    procedure TestTVMAZEHTTP;
    procedure TestTVMAZEHTTPS;
  end;

implementation

uses
  SysUtils, StrUtils, http;

{ TTestHTTP }

procedure TTestHTTP.TestIMDBHTTP;
var
  Result: Boolean;
  fURL, fHTML, fErrMsg: String;
begin
  fURL := 'http://www.imdb.com/title/tt6966692/';
  Result := HttpGetUrl(fURL, fHTML, fErrMsg);

  CheckTrue(Result, 'The HTTP fetch should work!');
  CheckNotEquals(0, Length(fHTML), 'Length of HTML code should be longer than 0');
  CheckEqualsString('', fErrMsg, 'Error message for IMDB is unexpected');
  CheckTrue(ContainsText(fHTML, '<title>Green Book'), 'HTML content should include title');
  CheckTrue(ContainsText(fHTML, '<meta property="og:title" content="Green Book'), 'HTML content should include meta name title');
end;

procedure TTestHTTP.TestIMDBHTTPS;
var
  Result: Boolean;
  fURL, fHTML, fErrMsg: String;
begin
  fURL := 'https://www.imdb.com/title/tt6966692/';
  Result := HttpGetUrl(fURL, fHTML, fErrMsg);

  CheckTrue(Result, 'The HTTP fetch should work!');
  CheckNotEquals(0, Length(fHTML), 'Length of HTML code should be longer than 0');
  CheckEqualsString('', fErrMsg, 'Error message for IMDB is unexpected');
  CheckTrue(ContainsText(fHTML, '<title>Green Book'), 'HTML content should include title');
  CheckTrue(ContainsText(fHTML, '<meta property="og:title" content="Green Book'), 'HTML content should include meta name title');
end;

procedure TTestHTTP.TestBOMHTTP;
var
  Result: Boolean;
  fURL, fHTML, fErrMsg: String;
begin
  fURL := 'http://www.boxofficemojo.com/movies/?id=marvel2019.htm';
  Result := HttpGetUrl(fURL, fHTML, fErrMsg);

  CheckTrue(Result, 'The HTTP fetch should work!');
  CheckNotEquals(0, Length(fHTML), 'Length of HTML code should be longer than 0');
  CheckEqualsString('', fErrMsg, 'Error message for BOM is unexpected');
  CheckTrue(ContainsText(fHTML, '<title dir="ltr">Avengers: Endgame - Box Office Mojo</title>'), 'HTML content should include title');
  CheckTrue(ContainsText(fHTML, '<span>3 hr 1 min</span>'), 'HTML content should include Runtime (3hrs 1min)');
end;

procedure TTestHTTP.TestBOMHTTPS;
var
  Result: Boolean;
  fURL, fHTML, fErrMsg: String;
begin
  fURL := 'https://www.boxofficemojo.com/movies/?id=marvel2019.htm';
  Result := HttpGetUrl(fURL, fHTML, fErrMsg);

  CheckTrue(Result, 'The HTTP fetch should work!');
  CheckNotEquals(0, Length(fHTML), 'Length of HTML code should be longer than 0');
  CheckEqualsString('', fErrMsg, 'Error message for BOM is unexpected');
  CheckTrue(ContainsText(fHTML, '<title dir="ltr">Avengers: Endgame - Box Office Mojo</title>'), 'HTML content should include title');
  CheckTrue(ContainsText(fHTML, '<span>3 hr 1 min</span>'), 'HTML content should include Runtime (3hrs 1min)');
end;

procedure TTestHTTP.TestTVMAZEHTTP;
var
  Result: Boolean;
  fURL, fHTML, fErrMsg: String;
begin
  fURL := 'http://api.tvmaze.com/search/shows?q=Utopia';
  Result := HttpGetUrl(fURL, fHTML, fErrMsg);

  CheckTrue(Result, 'The HTTP fetch should work!');
  CheckNotEquals(0, Length(fHTML), 'Length of HTML code should be longer than 0');
  CheckEqualsString('', fErrMsg, 'Error message for TVMAZE is unexpected');
  CheckTrue(ContainsText(fHTML, '{"id":64,"url":"https://www.tvmaze.com/shows/64/utopia","name":"Utopia",'), 'HTML content should include ID 64 - Utopia');
  CheckTrue(ContainsText(fHTML, '"country":{"name":"Australia","code":"AU"'), 'HTML content should include country Australia AU');
end;

procedure TTestHTTP.TestTVMAZEHTTPS;
var
  Result: Boolean;
  fURL, fHTML, fErrMsg: String;
begin
  fURL := 'https://api.tvmaze.com/search/shows?q=Utopia';
  Result := HttpGetUrl(fURL, fHTML, fErrMsg);

  CheckTrue(Result, 'The HTTP fetch should work!');
  CheckNotEquals(0, Length(fHTML), 'Length of HTML code should be longer than 0');
  CheckEqualsString('', fErrMsg, 'Error message for TVMAZE is unexpected');
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
