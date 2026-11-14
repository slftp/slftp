unit irccommands.stats;

interface

{ slftp stats commands functions }
function IrcStatRaces(const netname, channel, params: String): boolean;
{ Prints cumulative per-site TLS diagnostics without resetting counters. }
function IrcTLSStats(const netname, channel, params: String): boolean;

implementation

uses
  SysUtils, Classes, StrUtils, DateUtils, irc, sitesunit, statsunit, mystrings, sltlsstats;

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

function IrcTLSStats(const netname, channel, params: String): boolean;
var
  fSite: TSite;
  fSnapshot: TTLSStatsSnapshot;
  fKind: TTLSStatsKind;
begin
  Result := False;
  fSite := FindSiteByName(netname, UpperCase(Trim(params)));
  if fSite = nil then
  begin
    irc_addtext(netname, channel, 'Site <b>%s</b> not found.', [params]);
    Exit;
  end;

  fSnapshot := fSite.TLSStats.Snapshot;
  irc_addtext(netname, channel, '[TLS STATS] site=%s window_ms=%d session_reuse=disabled',
    [fSite.Name, fSnapshot.WindowMs]);
  for fKind := Low(TTLSStatsKind) to High(TTLSStatsKind) do
    irc_addtext(netname, channel, '[TLS STATS] site=%s %s',
      [fSite.Name, FormatTLSStats(fKind, fSnapshot.Counters[fKind])]);
  Result := True;
end;

end.
