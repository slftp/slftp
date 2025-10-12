unit routeconfig;

interface

type

  TSpeedFromRouteInfo = record
    Sitename: String;
    Speed: Integer;
    AffilOnly: Boolean;
    Locked: Boolean;

    { Returs a string representation of this route. }
    function ToString: String;

    { Creates the string to be stored in the sites.dat config file. }
    function ToConfigString: String;

    { Parses the string from the sites.dat config file. }
    constructor CreateFromConfigString(const aStrValue: string);
  end;

implementation

uses SysUtils;

const
  CONST_AFFIL_ROUTE_FLAG = 'A';
  CONST_SPEED_LOCK_FLAG = 'F';

constructor TSpeedFromRouteInfo.CreateFromConfigString(const aStrValue: string);
begin
  Speed := 0;
  AffilOnly := False;
  if aStrValue.Length > 0 then
  begin
    if not TryStrToInt(aStrValue[1], Speed) then
    begin
      raise Exception.Create(Format('Unable to parse ''%s'' as speed value', [aStrValue]));
    end;
    AffilOnly := aStrValue.Contains(CONST_AFFIL_ROUTE_FLAG);
    Locked := aStrValue.Contains(CONST_SPEED_LOCK_FLAG);
  end;
end;

function TSpeedFromRouteInfo.ToConfigString: String;
var fFlags: String;
begin
  Result := IntToStr(Speed);
  fFlags := '';
  if AffilOnly then
    fFlags := fFlags + CONST_AFFIL_ROUTE_FLAG;
  if Locked then
    fFlags := fFlags + CONST_SPEED_LOCK_FLAG;

  if fFlags <> '' then
    Result := Result + '-' + fFlags;
end;

function TSpeedFromRouteInfo.ToString: String;
var
  fRouteOptionInfo: String;
begin
  fRouteOptionInfo := '';
  if Locked then
    fRouteOptionInfo := fRouteOptionInfo + 'L';
  if AffilOnly then
    fRouteOptionInfo := fRouteOptionInfo + 'A';

  if fRouteOptionInfo <> '' then
    fRouteOptionInfo := '(' + fRouteOptionInfo + ')';

  Result := IntToStr(Speed) + fRouteOptionInfo;
end;

end.
