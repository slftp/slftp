unit ruleconditions.imdb;

interface

uses
  Classes, pazo, rulesunit;

type
  TConditionIMDBYear = class(TIntCondition)
    function SupplyValue(r: TPazo): integer; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionIMDBLanguages = class(TListCondition)
    procedure SupplyValues(r: TPazo; re: TStringList); override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionIMDBCountries = class(TMultiStringCondition)
    procedure SupplyValues(r: TPazo; re: TStringList); override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionIMDBGenres = class(TMultiStringCondition)
    procedure SupplyValues(r: TPazo; re: TStringList); override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionIMDBScreens = class(TIntCondition)
    function SupplyValue(r: TPazo): integer; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionIMDBRating = class(TIntCondition)
    function SupplyValue(r: TPazo): integer; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionIMDBVotes = class(TIntCondition)
    function SupplyValue(r: TPazo): integer; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionIMDBldt = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionIMDBWide = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionIMDBfestival = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionIMDBStv = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionIMDBCineyear = class(TIntCondition)
    function SupplyValue(r: TPazo): integer; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

implementation

uses
  SysUtils, Contnrs, kb, debugunit;

const
  dsection = 'rules.imdb';

{$I ruleconditions.imdb.inc}

{ TConditionIMDBYear }

function TConditionIMDBYear.SupplyValue(r: TPazo): integer;
begin
  Result := 0;

  if r.rls is TIMDBRelease then
  begin
    if TIMDBRelease(r.rls).IsLookupDone then
      Result := TIMDBRelease(r.rls).imdb_year;
  end;
end;

class function TConditionIMDBYear.Name: String;
begin
  Result := 'imdbyear';
end;

class function TConditionIMDBYear.Description: String;
begin
  Result := IMDBYearDescription;
end;

{ TConditionIMDBLanguages }

procedure TConditionIMDBLanguages.SupplyValues(r: TPazo; re: TStringList);
begin
  try
    if r.rls is TIMDBRelease then
    begin
      if TIMDBRelease(r.rls).IsLookupDone then
        re.Assign(TIMDBRelease(r.rls).imdb_languages);
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, dsection, Format('[EXCEPTION] TConditionIMDBLanguages.GetSupplyValues: %s', [e.Message]));
      re.Clear;
    end;
  end;
end;

class function TConditionIMDBLanguages.Name: String;
begin
  Result := 'imdblanguages';
end;

class function TConditionIMDBLanguages.Description: String;
begin
  Result := IMDBLanguagesDescription;
end;

{ TConditionIMDBCountries }

procedure TConditionIMDBCountries.SupplyValues(r: TPazo; re: TStringList);
begin
  try
    if r.rls is TIMDBRelease then
    begin
      if TIMDBRelease(r.rls).IsLookupDone then
        re.Assign(TIMDBRelease(r.rls).imdb_countries);
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, dsection, Format('[EXCEPTION] TConditionIMDBCountries.GetSupplyValues: %s', [e.Message]));
      re.Clear;
    end;
  end;
end;

class function TConditionIMDBCountries.Name: String;
begin
  Result := 'imdbcountries';
end;

class function TConditionIMDBCountries.Description: String;
begin
  Result := IMDBCountriesDescription;
end;

{ TConditionIMDBGenres }

procedure TConditionIMDBGenres.SupplyValues(r: TPazo; re: TStringList);
begin
  try
    if r.rls is TIMDBRelease then
    begin
      if TIMDBRelease(r.rls).IsLookupDone then
        re.Assign(TIMDBRelease(r.rls).imdb_genres);
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, dsection, Format('[EXCEPTION] TConditionIMDBGenres.GetSupplyValues: %s', [e.Message]));
      re.Clear;
    end;
  end;
end;

class function TConditionIMDBGenres.Name: String;
begin
  Result := 'imdbgenre';
end;

class function TConditionIMDBGenres.Description: String;
begin
  Result := IMDBGenresDescription;
end;

{ TConditionIMDBScreens }

function TConditionIMDBScreens.SupplyValue(r: TPazo): integer;
begin
  Result := 0;

  if r.rls is TIMDBRelease then
  begin
    if TIMDBRelease(r.rls).IsLookupDone then
      Result := TIMDBRelease(r.rls).imdb_screens;
  end;
end;

class function TConditionIMDBScreens.Name: String;
begin
  Result := 'imdbscreens';
end;

class function TConditionIMDBScreens.Description: String;
begin
  Result := IMDBScreensDescription;
end;

{ TConditionIMDBRating }

function TConditionIMDBRating.SupplyValue(r: TPazo): integer;
begin
  Result := 0;

  if r.rls is TIMDBRelease then
  begin
    if TIMDBRelease(r.rls).IsLookupDone then
      Result := TIMDBRelease(r.rls).imdb_rating;
  end;
end;

class function TConditionIMDBRating.Name: String;
begin
  Result := 'imdbrating';
end;

class function TConditionIMDBRating.Description: String;
begin
  Result := IMDBRatingDescription;
end;

{ TConditionIMDBVotes }

function TConditionIMDBVotes.SupplyValue(r: TPazo): integer;
begin
  Result := 0;

  if r.rls is TIMDBRelease then
  begin
    if TIMDBRelease(r.rls).IsLookupDone then
      Result := TIMDBRelease(r.rls).imdb_votes;
  end;
end;

class function TConditionIMDBVotes.Name: String;
begin
  Result := 'imdbvotes';
end;

class function TConditionIMDBVotes.Description: String;
begin
  Result := IMDBVotesDescription;
end;

{ TConditionIMDBldt }

function TConditionIMDBldt.SupplyValue(r: TPazo): boolean;
begin
  Result := False;

  if r.rls is TIMDBRelease then
  begin
    if TIMDBRelease(r.rls).IsLookupDone then
      Result := TIMDBRelease(r.rls).imdb_ldt;
  end;
end;

class function TConditionIMDBldt.Name: String;
begin
  Result := 'imdblimited';
end;

class function TConditionIMDBldt.Description: String;
begin
  Result := IMDBLimitedDescription;
end;

{ TConditionIMDBWide }

function TConditionIMDBWide.SupplyValue(r: TPazo): boolean;
begin
  Result := False;

  if r.rls is TIMDBRelease then
  begin
    if TIMDBRelease(r.rls).IsLookupDone then
      Result := TIMDBRelease(r.rls).imdb_wide;
  end;
end;

class function TConditionIMDBWide.Name: String;
begin
  Result := 'imdbwide';
end;

class function TConditionIMDBWide.Description: String;
begin
  Result := IMDBWideDescription;
end;

{ TConditionIMDBFestival }

function TConditionIMDBFestival.SupplyValue(r: TPazo): boolean;
begin
  Result := False;

  if r.rls is TIMDBRelease then
  begin
    if TIMDBRelease(r.rls).IsLookupDone then
      Result := TIMDBRelease(r.rls).imdb_festival;
  end;
end;

class function TConditionIMDBFestival.Name: String;
begin
  Result := 'imdbfestival';
end;

class function TConditionIMDBFestival.Description: String;
begin
  Result := IMDBFestivalDescription;
end;

{ TConditionIMDBStv }

function TConditionIMDBStv.SupplyValue(r: TPazo): boolean;
begin
  Result := False;

  if r.rls is TIMDBRelease then
  begin
    if TIMDBRelease(r.rls).IsLookupDone then
      Result := TIMDBRelease(r.rls).imdb_stvm;
  end;
end;

class function TConditionIMDBStv.Name: String;
begin
  Result := 'imdbstv';
end;

class function TConditionIMDBStv.Description: String;
begin
  Result := IMDBSTVDescription;
end;

{ TConditionIMDBCineyear }

function TConditionIMDBCineyear.SupplyValue(r: TPazo): integer;
begin
  Result := 0;

  if r.rls is TIMDBRelease then
  begin
    if TIMDBRelease(r.rls).IsLookupDone then
      Result := TIMDBRelease(r.rls).CineYear;
  end;
end;

class function TConditionIMDBCineyear.Name: String;
begin
  Result := 'imdbcineyear';
end;

class function TConditionIMDBCineyear.Description: String;
begin
  Result := IMDBCineYearDescription;
end;

end.
