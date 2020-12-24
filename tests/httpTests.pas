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
  CheckTrue({$IFDEF UNICODE}ContainsText{$ELSE}AnsiContainsText{$ENDIF}(fHTML, '<title>Green Book (2018) - IMDb</title>'), 'HTML content should include title');
  CheckTrue({$IFDEF UNICODE}ContainsText{$ELSE}AnsiContainsText{$ENDIF}(fHTML, '<meta name="title" content="Green Book (2018) - IMDb" />'), 'HTML content should include meta name title');
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
  CheckTrue({$IFDEF UNICODE}ContainsText{$ELSE}AnsiContainsText{$ENDIF}(fHTML, '<title>Green Book (2018) - IMDb</title>'), 'HTML content should include title');
  CheckTrue({$IFDEF UNICODE}ContainsText{$ELSE}AnsiContainsText{$ENDIF}(fHTML, '<meta name="title" content="Green Book (2018) - IMDb" />'), 'HTML content should include meta name title');
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
  CheckTrue({$IFDEF UNICODE}ContainsText{$ELSE}AnsiContainsText{$ENDIF}(fHTML, '<title dir="ltr">Avengers: Endgame - Box Office Mojo</title>'), 'HTML content should include title');
  CheckTrue({$IFDEF UNICODE}ContainsText{$ELSE}AnsiContainsText{$ENDIF}(fHTML, '<span>3 hr 1 min</span>'), 'HTML content should include Runtime (3hrs 1min)');
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
  CheckTrue({$IFDEF UNICODE}ContainsText{$ELSE}AnsiContainsText{$ENDIF}(fHTML, '<title dir="ltr">Avengers: Endgame - Box Office Mojo</title>'), 'HTML content should include title');
  CheckTrue({$IFDEF UNICODE}ContainsText{$ELSE}AnsiContainsText{$ENDIF}(fHTML, '<span>3 hr 1 min</span>'), 'HTML content should include Runtime (3hrs 1min)');
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
  CheckTrue({$IFDEF UNICODE}ContainsText{$ELSE}AnsiContainsText{$ENDIF}(fHTML, '{"id":64,"url":"http://www.tvmaze.com/shows/64/utopia","name":"Utopia",'), 'HTML content should include ID 64 - Utopia');
  CheckTrue({$IFDEF UNICODE}ContainsText{$ELSE}AnsiContainsText{$ENDIF}(fHTML, '"country":{"name":"Australia","code":"AU"'), 'HTML content should include country Australia AU');
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
  CheckTrue({$IFDEF UNICODE}ContainsText{$ELSE}AnsiContainsText{$ENDIF}(fHTML, '{"id":64,"url":"http://www.tvmaze.com/shows/64/utopia","name":"Utopia",'), 'HTML content should include ID 64 - Utopia');
  CheckTrue({$IFDEF UNICODE}ContainsText{$ELSE}AnsiContainsText{$ENDIF}(fHTML, '"country":{"name":"Australia","code":"AU"'), 'HTML content should include country Australia AU');
end;

initialization
  {$IFDEF FPC}
    RegisterTest('HTTP Get Tests', TTestHTTP.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestHTTP);
  {$ENDIF}
end.
