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
    procedure InsertUpdateDeleteTest1;
    procedure SameMovieNameOtherYearTest;
    procedure AlreadyInDbTest;
  end;

implementation

uses
  SysUtils, StrUtils, dbaddimdb;

{ TTestHTTP }

procedure TTestIMDB.InsertUpdateDeleteTest1;
var
  fImdbData, fImdbDataResult: TDbImdbData;
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

  fImdbDataResult := GetImdbMovieData(fRlsName);
  CheckEqualsString('Movie Name', fImdbDataResult.imdb_origtitle);
  CheckEquals(44, fImdbDataResult.imdb_rating);

  //update the imdb rating
  fImdbData.imdb_rating := 55;
  dbaddimdb_SaveImdbData(fRlsName, fImdbData);

  //check if rating has been updated
  fImdbDataResult := GetImdbMovieData(fRlsName);
  CheckEquals(55, fImdbDataResult.imdb_rating);


  DeleteIMDbDataWithImdbId('tt1337');

  //check if really deleted
  CheckNull(GetImdbMovieData(fRlsName));
end;


procedure TTestIMDB.SameMovieNameOtherYearTest;
var
  fImdbData, fImdbDataResult: TDbImdbData;
  fRlsName1, fRlsName2: String;
begin
  fRlsName1 := 'Movie.Name.Yeartest.1999.BluRay.x264-GRP';
  fRlsName2 := 'Movie.Name.Yeartest.2020.BluRay.x264-GRP';

  fImdbData := TDbImdbData.Create('tt872418');
  fImdbData.imdb_id := 'tt872418';
  fImdbData.imdb_year := 1999;
  dbaddimdb_SaveImdbData(fRlsName1, fImdbData);

  fImdbDataResult := GetImdbMovieData(fRlsName2);

  //we should not find a result with the release from a different year
  CheckNull(GetImdbMovieData(fRlsName2));
end;

procedure TTestIMDB.AlreadyInDbTest;
var
  fImdbData: TDbImdbData;
  fRlsName: String;
begin
  fRlsName := 'Already.In.DB.Test.1999.BluRay.x264-GRP';

  //first make sure the item is not in the database
  DeleteIMDbDataWithImdbId('tt8718818');
  CheckEquals(False, foundMovieAlreadyInDbWithReleaseName(fRlsName), 'We should NOT find this in the DB');

  //insert the item into the DB
  fImdbData := TDbImdbData.Create('tt8718818');
  fImdbData.imdb_id := 'tt8718818';
  fImdbData.imdb_year := 1999;
  dbaddimdb_SaveImdbData(fRlsName, fImdbData);

  CheckEquals(True, foundMovieAlreadyInDbWithReleaseName(fRlsName), 'We should find this in the DB');
end;


initialization
  {$IFDEF FPC}
    RegisterTest('IMDB Database Tests', TTestIMDB.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestIMDB);
  {$ENDIF}
end.
