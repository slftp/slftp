unit imdbapi;

interface

uses
  SysUtils, Classes, Variants, mormot.core.variants, http, debugunit;

type
  TImdbApi = class
  private
    const BASE_URL = 'https://graphql.imdb.com/';
    const GRAPHQL_QUERY = 'query{title(id:"%s"){originalTitleText{text}titleText{text}' +
      'titleType{id}releaseYear{year}ratingsSummary{aggregateRating voteCount}' +
      'genres{genres{text}}countriesOfOrigin{countries{id}}spokenLanguages{spokenLanguages{id text}}' +
      'releaseDates(first:100){edges{node{country{id}day month year attributes{text}}}}}}';
  public
    { Fetches all title details (Plot source fields, Rating, Votes, Genres, Countries,
      Languages and Release Dates) from the IMDb GraphQL API in a single request.
      @param(aImdbId IMDb title id, tt<numbers>)
      @param(aJson receives the "data.title" JSON document)
      @returns(@true on success, @false on failure) }
    class function GetTitle(const aImdbId: String; out aJson: Variant): Boolean;
  end;

implementation

const
  section = 'imdbapi';

class function TImdbApi.GetTitle(const aImdbId: String; out aJson: Variant): Boolean;
var
  fUrl, fQuery, fBody, fResponse, fErrMsg: String;
  fRoot, fData: Variant;
begin
  Result := False;
  aJson := Null;
  fUrl := BASE_URL;
  fQuery := Format(GRAPHQL_QUERY, [aImdbId]);
  fBody := '{"query":"' + StringReplace(fQuery, '"', '\"', [rfReplaceAll, rfIgnoreCase]) + '"}';

  if HttpPostJsonUrl(fUrl, fBody, fResponse, fErrMsg, 2,
    'Origin: https://www.imdb.com'#13#10 +
    'Referer: https://www.imdb.com/') then
  begin
    fRoot := _JsonFast(fResponse);
    if not VarIsNull(fRoot) then
    begin
      fData := TDocVariantData(fRoot).GetValueOrNull('data');
      if not VarIsNull(fData) then
      begin
        aJson := TDocVariantData(fData).GetValueOrNull('title');
        if not VarIsNull(aJson) then
          Result := True
        else
          Debug(dpError, section, Format('GraphQL response for %s contains no title (errors: %s)', [aImdbId, fResponse]));
      end
      else
        Debug(dpError, section, Format('GraphQL response for %s contains no data (errors: %s)', [aImdbId, fResponse]));
    end
    else
      Debug(dpError, section, Format('Failed to parse GraphQL JSON for Title %s', [aImdbId]));
  end
  else
  begin
    Debug(dpError, section, Format('Failed to fetch Title %s: %s', [aImdbId, fErrMsg]));
  end;
end;

end.
