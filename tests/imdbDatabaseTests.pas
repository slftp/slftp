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
    procedure AlreadyInDbWithReleasenameTest;
    procedure AlreadyInDbWithImdbIDTest;
    procedure FindExistingMovieByOtherReleaseNameTest;
    procedure FindExistingMovieByOtherReleaseLanguageTest;
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
  dbaddimdb_SaveImdbData(fRlsName, fImdbData, nil, nil, nil);

  fImdbDataResult := GetImdbMovieData(fRlsName);
  CheckEqualsString('Movie Name', fImdbDataResult.imdb_origtitle);
  CheckEquals(44, fImdbDataResult.imdb_rating);

  //update the imdb rating
  fImdbData.imdb_rating := 55;
  dbaddimdb_SaveImdbData(fRlsName, fImdbData, nil, nil, nil);

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
  dbaddimdb_SaveImdbData(fRlsName1, fImdbData, nil, nil, nil);

  fImdbDataResult := GetImdbMovieData(fRlsName2);

  //we should not find a result with the release from a different year
  CheckNull(GetImdbMovieData(fRlsName2));
end;

procedure TTestIMDB.AlreadyInDbWithReleasenameTest;
var
  fImdbData: TDbImdbData;
  fRlsName: String;
begin
  fRlsName := 'Already.In.DB.With.Releasename.Test.1999.BluRay.x264-GRP';

  //first make sure the item is not in the database
  DeleteIMDbDataWithImdbId('tt8718818');
  CheckEquals(False, foundMovieAlreadyInDbWithReleaseName(fRlsName), 'We should NOT find this in the DB');

  //insert the item into the DB
  fImdbData := TDbImdbData.Create('tt8718818');
  fImdbData.imdb_id := 'tt8718818';
  fImdbData.imdb_year := 1999;
  dbaddimdb_SaveImdbData(fRlsName, fImdbData, nil, nil, nil);

  CheckEquals(True, foundMovieAlreadyInDbWithReleaseName(fRlsName), 'We should find this in the DB');
end;

procedure TTestIMDB.AlreadyInDbWithImdbIDTest;
var
  fImdbData: TDbImdbData;
  fRlsName, fImdbID: String;
begin
  fRlsName := 'Already.In.DB.With.IMDB.ID.1999.BluRay.x264-GRP';
  fImdbID := 'tt8228818';

  //first make sure the item is not in the database
  DeleteIMDbDataWithImdbId(fImdbID);
  CheckEquals(False, foundMovieAlreadyInDbWithImdbID(fImdbID), 'We should NOT find this in the DB');

  //insert the item into the DB
  fImdbData := TDbImdbData.Create(fImdbID);
  fImdbData.imdb_id := fImdbID;
  fImdbData.imdb_year := 1999;
  dbaddimdb_SaveImdbData(fRlsName, fImdbData, nil, nil, nil);

  CheckEquals(True, foundMovieAlreadyInDbWithImdbID(fImdbID), 'We should find this in the DB');
end;

procedure TTestIMDB.FindExistingMovieByOtherReleaseNameTest;
var
  fRlsName1, fRlsName2, fImdbID: String;
  fImdbData, fImdbDataResult: TDbImdbData;
begin
  fRlsName1 := 'Movie.Name.For.Releasename.Test.2022.720p.BluRay.x264-GRP';
  fRlsName2 := 'Movie.Name.For.Releasename.Test.2022.1080p.WEB.h264-OTHERGRP';
  fImdbID := 'tt3417445';

  //first make sure the item is not in the database
  DeleteIMDbDataWithImdbId(fImdbID);
  CheckEquals(False, foundMovieAlreadyInDbWithImdbID(fImdbID), 'We should NOT find this in the DB');

  //insert the item into the DB
  fImdbData := TDbImdbData.Create(fImdbID);
  fImdbData.imdb_id := fImdbID;
  fImdbData.imdb_year := 2022;
  fImdbData.imdb_countries.CommaText := 'Uzbekistan';
  dbaddimdb_SaveImdbData(fRlsName1, fImdbData, nil, nil, nil);

  //we should find the entry with both release names
  CheckEquals(True, foundMovieAlreadyInDbWithReleasename(fRlsName1), fRlsName1 + 'not found in the DB');
  CheckEquals(True, foundMovieAlreadyInDbWithReleasename(fRlsName2), fRlsName2 + 'not found in the DB');

  //check if we get the correct info by other release name
  fImdbDataResult := GetImdbMovieData(fRlsName2);
  CheckEquals('Uzbekistan', fImdbDataResult.imdb_countries.CommaText);
end;

procedure TTestIMDB.FindExistingMovieByOtherReleaseLanguageTest;
var
  fRlsName1, fRlsName2, fImdbID: String;
  fImdbData, fImdbDataResult: TDbImdbData;
begin
  fRlsName1 := 'Movie.Name.For.Other.Language.Test.2022.720p.BluRay.x264-GRP';
  fRlsName2 := 'This.Is.The.Other.Language.2022.FRENCH.1080p.WEB.h264-OTHERGRP';
  fImdbID := 'tt3977445';

  //insert the item into the DB
  fImdbData := TDbImdbData.Create(fImdbID);
  fImdbData.imdb_id := fImdbID;
  fImdbData.imdb_year := 2022;
  dbaddimdb_SaveImdbData(fRlsName1, fImdbData, nil, nil, nil);

  //now we should add an 'alsoknownas' entry with the other release name
  dbaddimdb_SaveImdbData(fRlsName2, fImdbData, nil, nil, nil);

  //we should be able to find the entry with both release names
  CheckEquals(True, foundMovieAlreadyInDbWithReleaseName(fRlsName1), 'Could not find the entry with release name ' + fRlsName1);
  CheckEquals(True, foundMovieAlreadyInDbWithReleaseName(fRlsName2), 'Could not find the entry with release name ' + fRlsName2);
end;

initialization
  {$IFDEF FPC}
    RegisterTest('IMDB Database Tests', TTestIMDB.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestIMDB);
  {$ENDIF}
end.
