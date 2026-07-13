unit routeconfig;

interface

type

  { Holds information about a configured route between two sites. }
  TSpeedFromRouteInfo = record
    Sitename: String;
    Speed: Integer;
    AffilOnly: Boolean;
    NoAffil: Boolean;
    Locked: Boolean;

    { Returs a string representation of this route. }
    function ToString: String;

    { Creates the string to be stored in the sites.dat config file. }
    function ToConfigString: String;

    { Parses the string from the sites.dat config file. }
    constructor CreateFromConfigString(const aStrValue: string);
  end;

  { Immutable shared list of speed-from routes. }
  TSpeedFromRouteList = class
  public
    Routes: TArray<TSpeedFromRouteInfo>;
    constructor Create(const aRoutes: TArray<TSpeedFromRouteInfo>);
    function CreateCopy: TSpeedFromRouteList;
  end;

  { If you need to store a TSpeedFromRouteInfo record as an object (e.g. in a TStringList). }
  TSpeedFromRouteInfoObjectWrapper = class
    private
      FSpeedInfo: TSpeedFromRouteInfo;
    public
      property SpeedInfo: TSpeedFromRouteInfo read FSpeedInfo;
      constructor Create(const aSpeedInfo: TSpeedFromRouteInfo);
  end;

implementation

uses SysUtils;

const
  CONST_AFFIL_ROUTE_FLAG = 'A';
  CONST_NO_AFFIL_ROUTE_FLAG = 'N';
  CONST_SPEED_LOCK_FLAG = 'F';

{ TSpeedFromRouteInfo }

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
    NoAffil := aStrValue.Contains(CONST_NO_AFFIL_ROUTE_FLAG);
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
  if NoAffil then
    fFlags := fFlags + CONST_NO_AFFIL_ROUTE_FLAG;
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
  if NoAffil then
    fRouteOptionInfo := fRouteOptionInfo + 'N';

  if fRouteOptionInfo <> '' then
    fRouteOptionInfo := '(' + fRouteOptionInfo + ')';

  Result := IntToStr(Speed) + fRouteOptionInfo;
end;

{ TSpeedFromRouteList }

constructor TSpeedFromRouteList.Create(const aRoutes: TArray<TSpeedFromRouteInfo>);
begin
  Routes := aRoutes;
end;

function TSpeedFromRouteList.CreateCopy: TSpeedFromRouteList;
var
  fRoutes: TArray<TSpeedFromRouteInfo>;
  i: Integer;
begin
  SetLength(fRoutes, Length(Routes));
  for i := 0 to High(Routes) do
    fRoutes[i] := Routes[i];
  Result := TSpeedFromRouteList.Create(fRoutes);
end;

{ TSpeedFromRouteInfoObjectWrapper }

constructor TSpeedFromRouteInfoObjectWrapper.Create(const aSpeedInfo: TSpeedFromRouteInfo);
begin
  FSpeedInfo := aSpeedInfo;
end;

end.
