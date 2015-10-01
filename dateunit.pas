//dead unit?
unit dateunit;

interface

//function DateTimeToUnixUTC(const AValue: TDateTime): Int64; 
//function UnixUTCToDateTime(const AValue: Int64): TDateTime;

implementation

uses
  SysUtils, DateUtils, debugunit, configunit, RegExpr;
    (*
const
  section = 'date';

var
  offset: string = '';

function ReadOffset: string;
begin
  if offset <> '' then
  begin
    Result:= offset;
    exit;
  end;

  offset:= config.ReadString(section, 'offset', '0');
  Result:= offset;
end;

function __DateTimeToUnixUTC(const AValue: TDateTime): Int64;
var
  r: TRegexpr;
  vval: Int64;
begin
  Result:= DateTimeToUnix(AValue);
  if ReadOffset = '0' then exit;

  r := TRegexpr.Create;
  r.Expression := '^(\+|\-)([\d]+)$';
  try
    if r.Exec(ReadOffset) then
    begin
      vval := StrToInt(r.Match[2]);
      vval := vval * 3600;

      if r.Match[1] = '+' then
      begin
        // we are at + so to get UTC we need to -
        Result:= Result - vval;
      end;
      if r.Match[1] = '-' then
      begin
        // we are at - so to get UTC we need to +
        Result:= Result + vval;
      end;
    end;
  finally
    r.Free;
  end;
end;

function __UnixUTCToDateTime(const AValue: Int64): TDateTime;
var
  r: TRegexpr;
  vval: Int64;
begin
  if ReadOffset = '0' then
  begin
    Result:= UnixToDateTime(AValue);
    exit;
  end;

  r := TRegexpr.Create;
  r.Expression := '^(\+|\-)([\d]+)$';
  try
    if r.Exec(ReadOffset) then
    begin
      vval := StrToInt(r.Match[2]);
      vval := vval * 3600;

      if r.Match[1] = '+' then
      begin
        // we are at + so to get from UTC we need to +
        Result:= UnixToDateTime(AValue + vval);
      end;
      if r.Match[1] = '-' then
      begin
        // we are at - so to get from UTC we need to -
        Result:= UnixToDateTime(AValue - vval);
      end;
    end;
  finally
    r.Free;
  end;

end;
   *)
end.
