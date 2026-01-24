unit imdbapi;

interface

uses
  SysUtils, Classes, Variants, mormot.core.variants, http, debugunit;

type
  TImdbApi = class
  private
    const BASE_URL = 'https://api.imdbapi.dev';
  public
    { Fetches the main title details (Plot, Rating, Votes, Genres, etc.) }
    class function GetTitle(const aImdbId: String; out aJson: Variant): Boolean;
    
    { Fetches the release dates }
    class function GetReleaseDates(const aImdbId: String; out aJson: Variant): Boolean;
    
    { Fetches the AKAs (Also Known As) }
    class function GetAKAs(const aImdbId: String; out aJson: Variant): Boolean;
  end;

implementation

const
  section = 'imdbapi';

class function TImdbApi.GetTitle(const aImdbId: String; out aJson: Variant): Boolean;
var
  fUrl, fResponse, fErrMsg: String;
begin
  Result := False;
  fUrl := Format('%s/titles/%s', [BASE_URL, aImdbId]);
  
  if HttpGetUrl(fUrl, fResponse, fErrMsg) then
  begin
    aJson := _JsonFast(fResponse);
    if not VarIsNull(aJson) then
      Result := True
    else
      Debug(dpError, section, Format('Failed to parse JSON for Title %s', [aImdbId]));
  end
  else
  begin
    Debug(dpError, section, Format('Failed to fetch Title %s: %s', [aImdbId, fErrMsg]));
  end;
end;

class function TImdbApi.GetReleaseDates(const aImdbId: String; out aJson: Variant): Boolean;
var
  fUrl, fResponse, fErrMsg: String;
begin
  Result := False;
  // pageSize=50 (max allowed) to hopefully get all dates
  fUrl := Format('%s/titles/%s/releaseDates?pageSize=50', [BASE_URL, aImdbId]);
  
  if HttpGetUrl(fUrl, fResponse, fErrMsg) then
  begin
    aJson := _JsonFast(fResponse);
    if not VarIsNull(aJson) then
      Result := True
    else
      Debug(dpError, section, Format('Failed to parse JSON for ReleaseDates %s', [aImdbId]));
  end
  else
  begin
    Debug(dpError, section, Format('Failed to fetch ReleaseDates %s: %s', [aImdbId, fErrMsg]));
  end;
end;

class function TImdbApi.GetAKAs(const aImdbId: String; out aJson: Variant): Boolean;
var
  fUrl, fResponse, fErrMsg: String;
begin
  Result := False;
  fUrl := Format('%s/titles/%s/akas', [BASE_URL, aImdbId]);
  
  if HttpGetUrl(fUrl, fResponse, fErrMsg) then
  begin
    aJson := _JsonFast(fResponse);
    if not VarIsNull(aJson) then
      Result := True
    else
      Debug(dpError, section, Format('Failed to parse JSON for AKAs %s', [aImdbId]));
  end
  else
  begin
    Debug(dpError, section, Format('Failed to fetch AKAs %s: %s', [aImdbId, fErrMsg]));
  end;
end;

end.
