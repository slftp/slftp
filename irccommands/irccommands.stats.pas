unit irccommands.stats;

interface

{ slftp stats commands functions }
function IrcStatRaces(const netname, channel, params: String): boolean;

implementation

uses
  SysUtils, Classes, StrUtils, DateUtils, irc, sitesunit, statsunit, mystrings;

const
  section = 'irccommands.stats';

function IrcStatRaces(const netname, channel, params: String): boolean;
var
  fSitename, fPeriod: String;
  fDetailed: Boolean;
begin
  Result := False;

  fSitename := UpperCase(SubString(params, ' ', 1));
  fPeriod := UpperCase(SubString(params, ' ', 2));
  fDetailed := StrToBoolDef(SubString(params, ' ', 3), False);

  if (fSitename <> '*') then
  begin
    if FindSiteByName(Netname, fSitename) = nil then
    begin
      irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [fSitename]);
      exit;
    end;
  end;

  if ((fPeriod <> 'YEAR') and (fPeriod <> 'MONTH')) then
  begin
    fPeriod := 'DAY';
  end;

  StatRaces(Netname, Channel, fSitename, fPeriod, fDetailed);

  Result := True;
end;

end.