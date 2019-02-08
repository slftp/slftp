unit irccommands.rank;

interface

{ site rank irc commands }
function IrcRanks(const netname, channel, params: String): boolean;
function IrcRank(const netname, channel, params: String): boolean;
function IrcRankLock(const netname, channel, params: String): boolean;
function IrcRankRecalc(const netname, channel, params: String): boolean;

implementation

uses
  SysUtils, StrUtils, Classes, irccommandsunit, sitesunit, irc, ranksunit, math, mystrings;

const
  section = 'irccommands.rank';

function _mySpeedComparer(List: TStringList; Index1, Index2: integer): integer;
begin
  Result := CompareValue(StrToIntDef(List.ValueFromIndex[Index2], 0), StrToIntDef(List.ValueFromIndex[Index1], 0));
end;

function IrcRanks(const netname, channel, params: String): boolean;
var
  section: String;
  i, j: integer;
  s: TSite;
  x: TStringList;
  ss: String;
begin
  section := UpperCase(params);
  x := TStringList.Create;
  try
    for i := 0 to sites.Count - 1 do
    begin
      s := TSite(sites[i]);
      if section <> '' then
      begin
        if s.sectiondir[section] <> '' then
        begin
          j := s.RCInteger('ranklock-' + section, 0);
          if j <> 0 then
          begin
            x.Add(s.Name + '(L)' + '=' + s.RCString('ranklock-' + section, '1'));
          end
          else
          begin
            x.Add(s.Name + '=' + s.RCString('rank-' + section, '1'));
          end;
        end;
      end
      else
      begin
        j := s.RCInteger('ranklock', 0);
        if j <> 0 then
          x.Add(s.Name + '=' + IntToStr(j));
      end;
    end;

    x.CustomSort(_mySpeedComparer);

    ss := '';
    for i := 0 to x.Count - 1 do
    begin
      if ss <> '' then
        ss := ss + ', ';
      ss := ss + '"' + x.Names[i] + ' ' + x.ValueFromIndex[i] + '"';
      if (i + 1 mod 10 = 0) then
      begin
        irc_addtext(Netname, Channel, ss);
        ss := '';
      end;
    end;
    if ss <> '' then
      IrcLineBreak(Netname, Channel, ss);
  finally
    x.Free;
  end;
  Result := True;
end;

function IrcRank(const netname, channel, params: String): boolean;
var
  sitename, section: String;
  rank: integer;
  s: TSite;
begin
  Result := False;

  sitename := UpperCase(SubString(params, ' ', 1));
  section := UpperCase(SubString(params, ' ', 2));
  rank := StrToIntDef(SubString(params, ' ', 3), -1);

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site %s not found.', [sitename]);
    exit;
  end;
  if s.sectiondir[section] = '' then
  begin
    irc_addtext(Netname, Channel, 'Site %s has no section %s.',
      [sitename, section]);
    exit;
  end;

  if (rank = -1) then
  begin
    irc_addtext(Netname, Channel, 'Section %s on site %s is ranked %d.',
      [section, sitename, s.GetRank(section)]);
    Result := True;
    exit;
  end;

  if ((rank >= 0) and (rank <= 9)) then
  begin
    s.SetRank(section, rank);
  end
  else
  begin
    irc_addtext(Netname, Channel, 'Rank must be >= 0 and <= 9.', []);
    exit;
  end;

  if rank > 0 then
    irc_addtext(Netname, Channel, 'Section %s on site %s is ranked %d.',
      [section, sitename, s.GetRank(section)])
  else
    irc_addtext(Netname, Channel, 'Section %s on site %s is not ranked',
      [section, sitename]);

  Result := True;
end;

function IrcRankLock(const netname, channel, params: String): boolean;
var
  sitename, section: String;
  rank: integer;
  s: TSite;
begin
  Result := False;

  sitename := UpperCase(SubString(params, ' ', 1));
  section := UpperCase(SubString(params, ' ', 2));
  rank := StrToIntDef(SubString(params, ' ', 3), -1);

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site %s not found.', [sitename]);
    exit;
  end;
  if ((section <> '*') and (s.sectiondir[section] = '')) then
  begin
    irc_addtext(Netname, Channel,
      'Site %s has no section %s. Use * to set a global ranklock',
      [sitename, section]);
    exit;
  end;

  if (rank = -1) then
  begin
    irc_addtext(Netname, Channel, 'Section %s on site %s is rank locked %d.',
      [section, sitename, s.GetRankLock(section)]);
    Result := True;
    exit;
  end;

  if ((rank >= 0) and (rank <= 9)) then
  begin
    s.SetRankLock(section, rank);
  end
  else
  begin
    irc_addtext(Netname, Channel, 'Rank must be >= 0 and <= 9.', []);
    exit;
  end;

  if rank > 0 then
    irc_addtext(Netname, Channel, 'Section %s on site %s is rank locked %d.',
      [section, sitename, s.GetRankLock(section)])
  else
    irc_addtext(Netname, Channel, 'Section %s on site %s is not rank locked',
      [section, sitename]);

  Result := True;
end;

function IrcRankRecalc(const netname, channel, params: String): boolean;
begin
  RanksRecalc(Netname, Channel);
  Result := True;
end;

end.

