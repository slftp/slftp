unit imdbDatabaseTests;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}


type
  TTestIMDB = class(TTestCase)
  published
    procedure Test1;
  end;

implementation

uses
  SysUtils, StrUtils, dbaddimdb;

{ TTestHTTP }

procedure TTestIMDB.Test1;
var
  fImdbData: TDbImdbData;
  fRlsName: string;
begin

  fRlsName := 'Movie.Name.BluRay.x264-GRP';

  fImdbData := TDbImdbData.Create('tt1337');
  fImdbData.imdb_id := 'tt1337';
  fImdbData.imdb_year := 2022;
  fImdbData.imdb_languages.CommaText := 'English,German';
  fImdbData.imdb_countries.CommaText := 'USA';
  fImdbData.imdb_genres.CommaText := 'Action,Comedy';
  fImdbData.imdb_screens := 24561;
  fImdbData.imdb_rating := 44;
  fImdbData.imdb_votes := 51354;
  fImdbData.imdb_cineyear := 2022;
  fImdbData.imdb_ldt := False;
  fImdbData.imdb_wide := True;
  fImdbData.imdb_festival := False;
  fImdbData.imdb_stvm := False;
  fImdbData.imdb_stvs := 'Whatever';
  fImdbData.imdb_origtitle := 'Movie Name';
  dbaddimdb_SaveImdbData(fRlsName, fImdbData);
end;


initialization
  {$IFDEF FPC}
    RegisterTest('IMDB Database Tests', TTestIMDB.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestIMDB);
  {$ENDIF}
end.
