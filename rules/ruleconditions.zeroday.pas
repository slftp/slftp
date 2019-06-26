unit ruleconditions.zeroday;

interface

uses
  pazo, rulesunit;

type
  TCondition0daySource = class(TStringCondition)
    function Verify(const s: String): boolean; override;
    function SupplyValue(r: TPazo): String; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

implementation

uses
  SysUtils, Classes, Contnrs, kb;

const
  dsection = 'rules.zeroday';

{$I ruleconditions.zeroday.inc}

{ TCondition0daySource }

function TCondition0daySource.Verify(const s: String): boolean;
begin
  try
    Result := nulldaysources.IndexOfName(s) <> -1;
  except
    Result := False;
  end;
end;

function TCondition0daySource.SupplyValue(r: TPazo): String;
begin
  Result := '';
  try
    if (r.rls is T0dayRelease) then
      Result := T0dayRelease(r.rls).nulldaysource;
  except
    Result := '';
  end;
end;

class function TCondition0daySource.Name: String;
begin
  Result := '0daysource';
end;

class function TCondition0daySource.Description: String;
begin
  Result := zerodaySourceDescription;
end;

end.
