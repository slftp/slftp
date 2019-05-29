unit ruleconditions.tv;

interface

uses
  Classes, pazo, rulesunit;

type
  TConditionTVShowName = class(TStringCondition)
    function SupplyValue(r: TPazo): String; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionTVtag = class(TStringCondition)
    function SupplyValue(r: TPazo): String; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionTVPremierYear = class(TIntCondition)
    function SupplyValue(r: TPazo): integer; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionTVCountry = class(TStringCondition)
    function SupplyValue(r: TPazo): String; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionTVLanguage = class(TStringCondition)
    function SupplyValue(r: TPazo): String; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionTVClassification = class(TStringCondition)
    function SupplyValue(r: TPazo): String; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionTVScripted = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionTVGenres = class(TMultiStringCondition)
    procedure SupplyValues(r: TPazo; re: TStringList); override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionTVNetwork = class(TStringCondition)
    function SupplyValue(r: TPazo): String; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionTVRuntime = class(TIntCondition)
    function SupplyValue(r: TPazo): integer; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionTVEndedYear = class(TIntCondition)
    function SupplyValue(r: TPazo): integer; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionTVRunning = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionTVStatus = class(TStringCondition)
    function SupplyValue(r: TPazo): String; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionTVCurrentSeason = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionTVCurrentEpisode = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionTVCurrentOnAir = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionTVDailyShow = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

implementation

uses
  SysUtils, Contnrs, kb, debugunit;

const
  dsection = 'rules.tv';

{$I ruleconditions.tv.inc}

{ TConditionTVShowName }

function TConditionTVShowName.SupplyValue(r: TPazo): String;
begin
  Result := '';
  try
    if r.rls is TTVRelease then
      Result := TTVRelease(r.rls).showname;
  except
    Result := '';
  end;
end;

class function TConditionTVShowName.Name: String;
begin
  Result := 'tvshowname';
end;

class function TConditionTVShowName.Description: String;
begin
  Result := TVShowNameDescription;
end;

{ TConditionTVtag }

function TConditionTVtag.SupplyValue(r: TPazo): String;
begin
  Result := '';
  try
    if r.rls is TTvRelease then
      Result := TTvRelease(r.rls).tvtag;
  except
    Result := '';
  end;
end;

class function TConditionTVtag.Name: String;
begin
  Result := 'tvtag';
end;

class function TConditionTVtag.Description: String;
begin
  Result := TVtagDescription;
end;

{ TConditionTVPremierYear }

function TConditionTVPremierYear.SupplyValue(r: TPazo): integer;
begin
  Result := 0;
  try
    if r.rls is TTVRelease then
    begin
      if TTVRelease(r.rls).showid <> '' then
        Result := TTVRelease(r.rls).premier_year;
    end;
  except
    Result := 0;
  end;
end;

class function TConditionTVPremierYear.Name: String;
begin
  Result := 'tvpremieryear';
end;

class function TConditionTVPremierYear.Description: String;
begin
  Result := TVPremierYearDescription;
end;

{ TConditionTVCountry }

function TConditionTVCountry.SupplyValue(r: TPazo): String;
begin
  Result := '';
  try
    if r.rls is TTVRelease then
    begin
      if TTVRelease(r.rls).showid <> '' then
        Result := TTVRelease(r.rls).country;
    end;
  except
    Result := '';
  end;
end;

class function TConditionTVCountry.Name: String;
begin
  Result := 'tvcountry';
end;

class function TConditionTVCountry.Description: String;
begin
  Result := TVCountryDescription;
end;

{ TConditionTVLanguage }

function TConditionTVLanguage.SupplyValue(r: TPazo): String;
begin
  Result := '';
  try
    if r.rls is TTVRelease then
    begin
      if TTVRelease(r.rls).showid <> '' then
        Result := TTVRelease(r.rls).tvlanguage;
    end;
  except
    Result := '';
  end;
end;

class function TConditionTVLanguage.Name: String;
begin
  Result := 'tvlanguage';
end;

class function TConditionTVLanguage.Description: String;
begin
  Result := TVLanguageDescription;
end;

{ TConditionTVClassification }

function TConditionTVClassification.SupplyValue(r: TPazo): String;
begin
  Result := '';
  try
    if r.rls is TTVRelease then
    begin
      if TTVRelease(r.rls).showid <> '' then
        Result := lowercase(TTVRelease(r.rls).classification);
    end;
  except
    Result := '';
  end;
end;

class function TConditionTVClassification.Name: String;
begin
  Result := 'tvclassification';
end;

class function TConditionTVClassification.Description: String;
begin
  Result := TVClassificationDescription;
end;

{ TConditionTVScripted }

function TConditionTVScripted.SupplyValue(r: TPazo): boolean;
begin
  Result := False;
  try
    if r.rls is TTVRelease then
    begin
      if TTVRelease(r.rls).showid <> '' then
        Result := TTVRelease(r.rls).scripted;
    end;
  except
    Result := False;
  end;
end;

class function TConditionTVScripted.Name: String;
begin
  Result := 'tvscripted';
end;

class function TConditionTVScripted.Description: String;
begin
  Result := TVScriptedDescription;
end;

{ TConditionTVGenres }

procedure TConditionTVGenres.SupplyValues(r: TPazo; re: TStringList);
begin
  try
    if r.rls is TTVRelease then
    begin
      if TTVRelease(r.rls).showid <> '' then
        re.Assign(TTVRelease(r.rls).genres);
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, dsection, Format('[EXCEPTION] TConditionTVGenres.GetSupplyValues: %s', [e.Message]));
      re.Clear;
      exit;
    end;
  end;
end;

class function TConditionTVGenres.Name: String;
begin
  Result := 'tvgenres';
end;

class function TConditionTVGenres.Description: String;
begin
  Result := TVGenresDescription;
end;

{ TConditionTVNetwork }

function TConditionTVNetwork.SupplyValue(r: TPazo): String;
begin
  Result := '';
  try
    if r.rls is TTVRelease then
    begin
      if TTVRelease(r.rls).showid <> '' then
        Result := TTVRelease(r.rls).network;
    end;
  except
    Result := '';
  end;
end;

class function TConditionTVNetwork.Name: String;
begin
  Result := 'tvnetwork';
end;

class function TConditionTVNetwork.Description: String;
begin
  Result := TVNetworkDescription;
end;

{ TConditionTVRuntime }

function TConditionTVRuntime.SupplyValue(r: TPazo): integer;
begin
  Result := 0;
  try
    if r.rls is TTVRelease then
    begin
      if TTVRelease(r.rls).showid <> '' then
        Result := TTVRelease(r.rls).runtime;
    end;
  except
    Result := 0;
  end;
end;

class function TConditionTVRuntime.Name: String;
begin
  Result := 'tvruntime';
end;

class function TConditionTVRuntime.Description: String;
begin
  Result := TVRuntimeDescription;
end;

{ TConditionTVEndedYear }

function TConditionTVEndedYear.SupplyValue(r: TPazo): integer;
begin
  Result := 0;
  try
    if r.rls is TTVRelease then
    begin
      if TTVRelease(r.rls).showid <> '' then
        Result := TTVRelease(r.rls).ended_year;
    end;
  except
    Result := 0;
  end;
end;

class function TConditionTVEndedYear.Name: String;
begin
  Result := 'tvendedyear';
end;

class function TConditionTVEndedYear.Description: String;
begin
  Result := TVEndedYearDescription;
end;

{ TConditionTVRunning }

function TConditionTVRunning.SupplyValue(r: TPazo): boolean;
begin
  Result := False;
  try
    if r.rls is TTVRelease then
    begin
      if TTVRelease(r.rls).showid <> '' then
        Result := TTVRelease(r.rls).running;
    end;
  except
    Result := False;
  end;
end;

class function TConditionTVRunning.Name: String;
begin
  Result := 'tvrunning';
end;

class function TConditionTVRunning.Description: String;
begin
  Result := TVRunningDescription;
end;

{ TConditionTVStatus }

function TConditionTVStatus.SupplyValue(r: TPazo): String;
begin
  Result := '';
  try
    if r.rls is TTVRelease then
    begin
      if TTVRelease(r.rls).showid <> '' then
        Result := TTVRelease(r.rls).status;
    end;
  except
    Result := '';
  end;
end;

class function TConditionTVStatus.Name: String;
begin
  Result := 'tvstatus';
end;

class function TConditionTVStatus.Description: String;
begin
  Result := TVStatusDescription;
end;

{ TConditionTVCurrentSeason }

function TConditionTVCurrentSeason.SupplyValue(r: TPazo): boolean;
begin
  Result := False;
  try
    if r.rls is TTVRelease then
    begin
      if TTVRelease(r.rls).showid <> '' then
        Result := TTVRelease(r.rls).currentseason;
    end;
  except
    Result := False;
  end;
end;

class function TConditionTVCurrentSeason.Name: String;
begin
  Result := 'tvcurrentseason';
end;

class function TConditionTVCurrentSeason.Description: String;
begin
  Result := TVCurrentSeasonDescription;
end;

{ TConditionTVCurrentEpisode }

function TConditionTVCurrentEpisode.SupplyValue(r: TPazo): boolean;
begin
  Result := False;
  try
    if r.rls is TTVRelease then
    begin
      if TTVRelease(r.rls).showid <> '' then
        Result := TTVRelease(r.rls).currentepisode;
    end;
  except
    Result := False;
  end;
end;

class function TConditionTVCurrentEpisode.Name: String;
begin
  Result := 'tvcurrentep';
end;

class function TConditionTVCurrentEpisode.Description: String;
begin
  Result := TVCurrentEpisodeDescription;
end;

{ TConditionTVCurrent (OnAir) }

function TConditionTVCurrentOnAir.SupplyValue(r: TPazo): boolean;
begin
  Result := False;
  try
    if r.rls is TTVRelease then
    begin
      if TTVRelease(r.rls).showid <> '' then
        Result := TTVRelease(r.rls).currentair;
    end;
  except
    Result := False;
  end;
end;

class function TConditionTVCurrentOnAir.Name: String;
begin
  Result := 'tvcurrent';
end;

class function TConditionTVCurrentOnAir.Description: String;
begin
  Result := TVCurrentOnAirDescription;
end;

{ TConditionTVDailyShow }

function TConditionTVDailyShow.SupplyValue(r: TPazo): boolean;
begin
  Result := False;
  try
    if r.rls is TTVRelease then
    begin
      if TTVRelease(r.rls).showid <> '' then
        Result := TTVRelease(r.rls).daily;
    end;
  except
    Result := False;
  end;
end;

class function TConditionTVDailyShow.Name: String;
begin
  Result := 'tvdaily';
end;

class function TConditionTVDailyShow.Description: String;
begin
  Result := TVDailyShowDescription;
end;

end.
