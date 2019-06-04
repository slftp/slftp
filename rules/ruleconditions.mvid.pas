unit ruleconditions.mvid;

interface

uses
  Classes, pazo, rulesunit;

type
  TConditionMVIDGenre = class(TMultiStringCondition)
    procedure SupplyValues(r: TPazo; re: TStringList); override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionMVIDFiles = class(TIntCondition)
    function SupplyValue(r: TPazo): integer; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionMVIDYear = class(TIntCondition)
    function SupplyValue(r: TPazo): integer; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionMVIDVA = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionMVIDPAL = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionMVIDNTSC = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionMVIDLive = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionMVIDLanguage = class(TMultiStringCondition)
    procedure SupplyValues(r: TPazo; re: TStringList); override;
    class function Name: String; override;
    class function Description: String; override;
  end;

implementation

uses
  SysUtils, Contnrs, kb, debugunit;

const
  dsection = 'rules.mvid';

{$I ruleconditions.mvid.inc}

{ TConditionMVIDGenre }

procedure TConditionMVIDGenre.SupplyValues(r: TPazo; re: TStringList);
begin
  try
    if r.rls is TMVIDRelease then
      re.Assign(TMvidRelease(r.rls).mvid_Genre);
  except
    on E: Exception do
    begin
      Debug(dpError, dsection, Format('[EXCEPTION] TConditionMVIDGenre.GetSupplyValues: %s', [e.Message]));
      re.Clear;
      exit;
    end;
  end;
end;

class function TConditionMVIDGenre.Name: String;
begin
  Result := 'mvidgenre';
end;

class function TConditionMVIDGenre.Description: String;
begin
  Result := MVIDGenreDescription;
end;

{ TConditionMVIDFiles }

function TConditionMVIDFiles.SupplyValue(r: TPazo): integer;
begin
  Result := -1;
  if r.rls is TMVIDRelease then
    Result := TMVIDRelease(r.rls).FileCount;
end;

class function TConditionMVIDFiles.Name: String;
begin
  Result := 'mvidfiles';
end;

class function TConditionMVIDFiles.Description: String;
begin
  Result := MVIDFilesDescription;
end;

{ TConditionMVIDYear }

function TConditionMVIDYear.SupplyValue(r: TPazo): integer;
begin
  Result := 0;
  if r.rls is TMVIDRelease then
    Result := TMVIDRelease(r.rls).mvid_year;
end;

class function TConditionMVIDYear.Name: String;
begin
  Result := 'mvidyear';
end;

class function TConditionMVIDYear.Description: String;
begin
  Result := MVIDYearDescription;
end;

{ TConditionMVIDVA }

function TConditionMVIDVA.SupplyValue(r: TPazo): boolean;
begin
  Result := False;
  if r.rls is TMVIDRelease then
    Result := TMVIDRelease(r.rls).mvid_va;
end;

class function TConditionMVIDVA.Name: String;
begin
  Result := 'mvidva';
end;

class function TConditionMVIDVA.Description: String;
begin
  Result := MVIDVADescription;
end;

{ TConditionMVIDPAL }

function TConditionMVIDPAL.SupplyValue(r: TPazo): boolean;
begin
  Result := False;
  if r.rls is TMVIDRelease then
    Result := TMVIDRelease(r.rls).mvid_pal;
end;

class function TConditionMVIDPAL.Name: String;
begin
  Result := 'mvidpal';
end;

class function TConditionMVIDPAL.Description: String;
begin
  Result := MVIDPALDescription;
end;

{ TConditionMVIDNTSC }

function TConditionMVIDNTSC.SupplyValue(r: TPazo): boolean;
begin
  Result := False;
  if r.rls is TMVIDRelease then
    Result := TMVIDRelease(r.rls).mvid_ntsc;
end;

class function TConditionMVIDNTSC.Name: String;
begin
  Result := 'mvidntsc';
end;

class function TConditionMVIDNTSC.Description: String;
begin
  Result := MVIDNTSCDescription;
end;

{ TConditionMVIDLIVE }

function TConditionMVIDLIVE.SupplyValue(r: TPazo): boolean;
begin
  Result := False;
  if r.rls is TMVIDRelease then
    Result := TMVIDRelease(r.rls).mvid_live;
end;

class function TConditionMVIDLIVE.Name: String;
begin
  Result := 'mvidlive';
end;

class function TConditionMVIDLIVE.Description: String;
begin
  Result := MVIDLIVEDescription;
end;

{ TConditionMVIDLanguage }

procedure TConditionMVIDLanguage.SupplyValues(r: TPazo; re: TStringList);
begin
  try
    if r.rls is TMVIDRelease then
      re.Assign(TMvidRelease(r.rls).Languages);
  except
    on E: Exception do
    begin
      Debug(dpError, dsection, Format('[EXCEPTION] TConditionMVIDLanguage.GetSupplyValues: %s', [e.Message]));
      re.Clear;
      exit;
    end;
  end;
end;

class function TConditionMVIDLanguage.Name: String;
begin
  Result := 'mvidlanguage';
end;

class function TConditionMVIDLanguage.Description: String;
begin
  Result := MVIDLanguageDescription;
end;

end.