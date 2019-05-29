unit ruleconditions.nfo;

interface

uses
  pazo, rulesunit;

type
  // seems it does only allow/is intended for usage of masks for nfogenre rules
  TConditionNfoGenreMask = class(TStringCondition)
    function SupplyValue(r: TPazo): String; override;
    constructor Create(parent: TRuleNode); override;
    class function Name: String; override;
    class function Description: String; override;
  end;

implementation

uses
  SysUtils, Classes, Contnrs, kb;

const
  dsection = 'rules.nfo';

{$I ruleconditions.nfo.inc}

{ TConditionNfoGenreMask }

function TConditionNfoGenreMask.SupplyValue(r: TPazo): String;
begin
  Result := '';
  try
    if r.rls is TNFORelease then
      Result := TNFORelease(r.rls).nfogenre;
  except
    Result := '';
  end;
end;

constructor TConditionNfoGenreMask.Create(parent: TRuleNode);
begin
  inherited;
  acceptedOperators.Clear;
  acceptedOperators.Add(TMaskOperator);
end;

class function TConditionNfoGenreMask.Name: String;
begin
  Result := 'nfogenre';
end;

class function TConditionNfoGenreMask.Description: String;
begin
  Result := NfoGenreMaskDescription;
end;

end.
