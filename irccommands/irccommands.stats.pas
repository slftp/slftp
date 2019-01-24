unit irccommands.stats;

interface

{ slftp stats commands functions }
function IrcStatSites(const netname, channel, params: String): boolean; // TODO: merge and remove code duplication
function IrcStatSitesByGroup(const netname, channel, params: String): boolean; // TODO: merge and remove code duplication
function IrcStatSitesByUser(const netname, channel, params: String): boolean; // TODO: merge and remove code duplication
function IrcStatGroups(const netname, channel, params: String): boolean; // TODO: merge and remove code duplication
function IrcStatGroupsBySite(const netname, channel, params: String): boolean; // TODO: merge and remove code duplication
function IrcStatUsers(const netname, channel, params: String): boolean; // TODO: merge and remove code duplication
function IrcStatUsersBySite(const netname, channel, params: String): boolean; // TODO: merge and remove code duplication
function IrcStatUsersByGroup(const netname, channel, params: String): boolean; // TODO: merge and remove code duplication
function IrcStatUsersByGroupBySite(const netname, channel, params: String): boolean; // TODO: merge and remove code duplication
function IrcStatRaces(const netname, channel, params: String): boolean;

implementation

uses
  SysUtils, Classes, StrUtils, DateUtils, irc, sitesunit, statsunit, mystrings;

const
  section = 'irccommands.stats';

function _StupidAntiSQLInjection(const aSQLText: String): String;
begin
  // stupid and safe anti sql injection
  Result := ReplaceText(aSQLText, '"', '');
  Result := ReplaceText(Result, '''', '');
end;

function IrcStatSites(const netname, channel, params: String): boolean;
var
  fSaveParams, q, ss: String;
  sectionname, sectionfilter: String;
  d: integer;
begin
  fSaveParams := _StupidAntiSQLInjection(params);

  d := 0;
  sectionfilter := '';

  sectionname := UpperCase(SubString(fSaveParams, ' ', 1));
  if sectionname <> '' then
  begin
    d := StrToIntDef(sectionname, 0);
    if d <= 0 then
    begin
      d := StrToIntDef(SubString(fSaveParams, ' ', 2), 0);
      sectionfilter := ' AND section like ''' + sectionname + ''' ' + #13#10;
    end
    else
      sectionname := '';
  end;

  if d <= 0 then
    d := 7; // default.

  q := 'SELECT sitename, ROUND(CAST(SUM(filesize) AS REAL)/1024,1) AS s ' + #13#10;
  q := q + 'FROM hit ';
  q := q + 'WHERE ts > DATETIME(''now'',''-' + IntToStr(d) + ' day'') ' + #13#10;
  q := q + sectionfilter;
  q := q + 'GROUP BY sitename ' + #13#10;
  q := q + 'ORDER BY s DESC ' + #13#10;
  q := q + 'LIMIT 20';

  irc_addtext(Netname, Channel, 'Race stats of sites:');
  irc_addtext(Netname, Channel, 'Query interval from %s to %s', [MyDateToStr(IncDay(now, -1 * d)), MyDateToStr(now)]);
  if sectionname <> '' then
    irc_addtext(Netname, Channel, 'Section: %s', [sectionname]);

  q := statsQuery(q);

  while (True) do
  begin
    ss := elsosor(q);
    if ss = '' then
      break;
    irc_addtext(Netname, Channel, ss);
  end;

  Result := True;
end;

function IrcStatSitesByGroup(const netname, channel, params: String): boolean;
var
  fSaveParams, sss, q, ss: String;
  groupname, groupfilter, sectionname, sectionfilter: String;
  d: integer;
begin
  fSaveParams := _StupidAntiSQLInjection(params);

  d := 0;
  sectionfilter := '';

  groupname := UpperCase(SubString(fSaveParams, ' ', 1));
  groupfilter := ' AND groupname like "' + groupname + '" ' + #13#10;

  sectionname := UpperCase(SubString(fSaveParams, ' ', 2));
  if sectionname <> '' then
  begin
    d := StrToIntDef(sectionname, 0);
    if d <= 0 then
    begin
      d := StrToIntDef(SubString(fSaveParams, ' ', 3), 0);
      sectionfilter := ' AND section like "' + sectionname + '" ' + #13#10;
    end
    else
      sectionname := '';
  end;

  if d <= 0 then
    d := 7; // default.

  q := 'SELECT sitename, ROUND(CAST(SUM(filesize) AS REAL)/1024,1) AS s ' + #13#10;
  q := q + 'FROM hit ';
  q := q + 'WHERE ts > DATETIME("now",-' + IntToStr(d) + ' day") ' + #13#10;
  q := q + sectionfilter;
  q := q + groupfilter;
  q := q + 'GROUP BY sitename ' + #13#10;
  q := q + 'ORDER BY s DESC ' + #13#10;
  q := q + 'LIMIT 20';

  irc_addtext(Netname, Channel, 'Race stats of sites:');
  irc_addtext(Netname, Channel, 'Query interval from %s to %s', [MyDateToStr(IncDay(now, -1 * d)), MyDateToStr(now)]);
  irc_addtext(Netname, Channel, 'Group: %s', [groupname]);
  if sectionname <> '' then
    irc_addtext(Netname, Channel, 'Section: %s', [sectionname]);

  sss := statsQuery(q);

  while (True) do
  begin
    ss := elsosor(sss);
    if ss = '' then
      break;
    irc_addtext(Netname, Channel, ss);
  end;

  Result := True;
end;

function IrcStatSitesByUser(const netname, channel, params: String): boolean;
var
  fSaveParams, q, ss: String;
  username, userfilter, sectionname, sectionfilter: String;
  d: integer;
begin
  fSaveParams := _StupidAntiSQLInjection(params);

  d := 0;
  sectionfilter := '';

  username := UpperCase(SubString(fSaveParams, ' ', 1));
  userfilter := ' AND username like ''' + username + ''' ' + #13#10;

  sectionname := UpperCase(SubString(fSaveParams, ' ', 2));
  if sectionname <> '' then
  begin
    d := StrToIntDef(sectionname, 0);
    if d <= 0 then
    begin
      d := StrToIntDef(SubString(fSaveParams, ' ', 3), 0);
      sectionfilter := ' AND section like ''' + UpperCase(sectionname) +
        ''' ' + #13#10;
    end
    else
      sectionname := '';
  end;

  if d <= 0 then
    d := 7; // default.

  q := 'SELECT sitename, ROUND(CAST(SUM(filesize) AS REAL)/1024,1) AS s ' + #13#10;
  q := q + 'FROM hit ';
  q := q + 'WHERE ts > DATETIME(''now'',''-' + IntToStr(d) + ' day'') ' + #13#10;
  q := q + sectionfilter;
  q := q + userfilter;
  q := q + 'GROUP BY sitename ' + #13#10;
  q := q + 'ORDER BY s DESC ' + #13#10;
  q := q + 'LIMIT 20';

  irc_addtext(Netname, Channel, 'Race stats of sites:');
  irc_addtext(Netname, Channel, 'Query interval from %s to %s', [MyDateToStr(IncDay(now, -1 * d)), MyDateToStr(now)]);
  irc_addtext(Netname, Channel, 'User: %s', [username]);
  if sectionname <> '' then
    irc_addtext(Netname, Channel, 'Section: %s', [sectionname]);

  q := statsQuery(q);

  while (True) do
  begin
    ss := elsosor(q);
    if ss = '' then
      break;
    irc_addtext(Netname, Channel, ss);
  end;

  Result := True;
end;

function IrcStatGroups(const netname, channel, params: String): boolean;
var
  fSaveParams, q, ss: String;
  sectionname, sectionfilter: String;
  d: integer;
begin
  fSaveParams := _StupidAntiSQLInjection(params);

  d := 0;
  sectionfilter := '';

  sectionname := UpperCase(SubString(fSaveParams, ' ', 1));
  if sectionname <> '' then
  begin
    d := StrToIntDef(sectionname, 0);
    if d <= 0 then
    begin
      d := StrToIntDef(SubString(fSaveParams, ' ', 2), 0);
      sectionfilter := ' AND section like ''' + sectionname + ''' ' + #13#10;
    end
    else
      sectionname := '';
  end;

  if d <= 0 then
    d := 7; // default.

  q := 'SELECT groupname, ROUND(CAST(SUM(filesize) AS REAL)/1024,1) AS s ' + #13#10;
  q := q + 'FROM hit ';
  q := q + 'WHERE ts > DATETIME(''now'',''-' + IntToStr(d) + ' day'') ' + #13#10;
  q := q + sectionfilter;
  q := q + 'GROUP BY groupname ' + #13#10;
  q := q + 'ORDER BY s DESC ' + #13#10;
  q := q + 'LIMIT 20';

  irc_addtext(Netname, Channel, 'Race stats of groups:');
  irc_addtext(Netname, Channel, 'Query interval from %s to %s', [MyDateToStr(IncDay(now, -1 * d)), MyDateToStr(now)]);
  if sectionname <> '' then
    irc_addtext(Netname, Channel, 'Section: %s', [sectionname]);

  q := statsQuery(q);

  while (True) do
  begin
    ss := elsosor(q);
    if ss = '' then
      break;
    irc_addtext(Netname, Channel, ss);
  end;

  Result := True;
end;

function IrcStatGroupsBySite(const netname, channel, params: String): boolean;
var
  fSaveParams, q, ss: String;
  sitename, sitefilter, sectionname, sectionfilter: String;
  d: integer;
begin
  fSaveParams := _StupidAntiSQLInjection(params);

  sitename := UpperCase(SubString(fSaveParams, ' ', 1));
  sitefilter := ' AND sitename like ''' + sitename + ''' ' + #13#10;

  d := 0;
  sectionfilter := '';
  sectionname := SubString(fSaveParams, ' ', 2);
  if sectionname <> '' then
  begin
    d := StrToIntDef(sectionname, 0);
    if d <= 0 then
    begin
      d := StrToIntDef(SubString(fSaveParams, ' ', 3), 0);
      sectionfilter := ' AND section like ''' + UpperCase(sectionname) +
        ''' ' + #13#10;
    end
    else
      sectionname := '';
  end;

  if d <= 0 then
    d := 7; // default.

  q := 'SELECT groupname, ROUND(CAST(SUM(filesize) AS REAL)/1024,1) AS s ' + #13#10;
  q := q + 'FROM hit ';
  q := q + 'WHERE ts > DATETIME(''now'',''-' + IntToStr(d) + ' day'') ' + #13#10;
  q := q + sectionfilter;
  q := q + sitefilter;
  q := q + 'GROUP BY groupname ' + #13#10;
  q := q + 'ORDER BY s DESC ' + #13#10;
  q := q + 'LIMIT 20';

  irc_addtext(Netname, Channel, 'Race stats of groups:');
  irc_addtext(Netname, Channel, 'Query interval from %s to %s', [MyDateToStr(IncDay(now, -1 * d)), MyDateToStr(now)]);
  irc_addtext(Netname, Channel, 'Site: %s', [sitename]);
  if sectionname <> '' then
    irc_addtext(Netname, Channel, 'Section: %s', [sectionname]);

  q := statsQuery(q);

  while (True) do
  begin
    ss := elsosor(q);
    if ss = '' then
      break;
    irc_addtext(Netname, Channel, ss);
  end;

  Result := True;
end;

function IrcStatUsers(const netname, channel, params: String): boolean;
var
  fSaveParams, q, ss: String;
  sectionname, sectionfilter: String;
  d: integer;
begin
  fSaveParams := _StupidAntiSQLInjection(params);

  d := 0;
  sectionfilter := '';

  sectionname := UpperCase(SubString(fSaveParams, ' ', 1));
  if sectionname <> '' then
  begin
    d := StrToIntDef(sectionname, 0);
    if d <= 0 then
    begin
      d := StrToIntDef(SubString(fSaveParams, ' ', 2), 0);
      sectionfilter := ' AND section like ''' + sectionname + ''' ' + #13#10;
    end
    else
      sectionname := '';
  end;

  if d <= 0 then
    d := 7; // default.

  q := 'SELECT username, ROUND(CAST(SUM(filesize) AS REAL)/1024,1) AS s ' + #13#10;
  q := q + 'FROM hit ';
  q := q + 'WHERE ts > DATETIME(''now'',''-' + IntToStr(d) + ' day'') ' + #13#10;
  q := q + sectionfilter;
  q := q + 'GROUP BY username ' + #13#10;
  q := q + 'ORDER BY s DESC ' + #13#10;
  q := q + 'LIMIT 20';

  irc_addtext(Netname, Channel, 'Race stats of users:');
  irc_addtext(Netname, Channel, 'Query interval from %s to %s', [MyDateToStr(IncDay(now, -1 * d)), MyDateToStr(now)]);
  if sectionname <> '' then
    irc_addtext(Netname, Channel, 'Section: %s', [sectionname]);

  q := statsQuery(q);

  while (True) do
  begin
    ss := elsosor(q);
    if ss = '' then
      break;
    irc_addtext(Netname, Channel, ss);
  end;

  Result := True;
end;

function IrcStatUsersBySite(const netname, channel, params: String): boolean;
var
  fSaveParams, q, ss: String;
  sitename, sitefilter, sectionname, sectionfilter: String;
  d: integer;
begin
  fSaveParams := _StupidAntiSQLInjection(params);

  sitename := UpperCase(SubString(fSaveParams, ' ', 1));
  sitefilter := ' AND sitename like ''' + sitename + ''' ' + #13#10;

  d := 0;
  sectionfilter := '';
  sectionname := UpperCase(SubString(fSaveParams, ' ', 2));
  if sectionname <> '' then
  begin
    d := StrToIntDef(sectionname, 0);
    if d <= 0 then
    begin
      d := StrToIntDef(SubString(fSaveParams, ' ', 3), 0);
      sectionfilter := ' AND section like ''' + UpperCase(sectionname) + ''' ' + #13#10;
    end
    else
      sectionname := '';
  end;

  if d <= 0 then
    d := 7; // default.

  q := 'SELECT username, ROUND(CAST(SUM(filesize) AS REAL)/1024,1) AS s ' + #13#10;
  q := q + 'FROM hit ';
  q := q + 'WHERE ts > DATETIME(''now'',''-' + IntToStr(d) + ' day'') ' + #13#10;
  q := q + sectionfilter;
  q := q + sitefilter;
  q := q + 'GROUP BY username ' + #13#10;
  q := q + 'ORDER BY s DESC ' + #13#10;
  q := q + 'LIMIT 20';

  irc_addtext(Netname, Channel, 'Race stats of users:');
  irc_addtext(Netname, Channel, 'Query interval from %s to %s', [MyDateToStr(IncDay(now, -1 * d)), MyDateToStr(now)]);
  irc_addtext(Netname, Channel, 'Site: %s', [sitename]);
  if sectionname <> '' then
    irc_addtext(Netname, Channel, 'Section: %s', [sectionname]);

  q := statsQuery(q);

  while (True) do
  begin
    ss := elsosor(q);
    if ss = '' then
      break;
    irc_addtext(Netname, Channel, ss);
  end;

  Result := True;
end;

function IrcStatUsersByGroup(const netname, channel, params: String): boolean;
var
  fSaveParams, q, ss: String;
  groupname, groupfilter, sectionname, sectionfilter: String;
  d: integer;
begin
  fSaveParams := _StupidAntiSQLInjection(params);

  groupname := UpperCase(SubString(fSaveParams, ' ', 1));
  groupfilter := ' AND groupname like ''' + groupname + ''' ' + #13#10;

  d := 0;
  sectionfilter := '';
  sectionname := UpperCase(SubString(fSaveParams, ' ', 2));
  if sectionname <> '' then
  begin
    d := StrToIntDef(sectionname, 0);
    if d <= 0 then
    begin
      d := StrToIntDef(SubString(fSaveParams, ' ', 3), 0);
      sectionfilter := ' AND section like ''' + UpperCase(sectionname) + ''' ' + #13#10;
    end
    else
      sectionname := '';
  end;

  if d <= 0 then
    d := 7; // default.

  q := 'SELECT username, ROUND(CAST(SUM(filesize) AS REAL)/1024,1) AS s ' + #13#10;
  q := q + 'FROM hit ';
  q := q + 'WHERE ts > DATETIME(''now'',''-' + IntToStr(d) + ' day'') ' + #13#10;
  q := q + sectionfilter;
  q := q + groupfilter;
  q := q + 'GROUP BY username ' + #13#10;
  q := q + 'ORDER BY s DESC ' + #13#10;
  q := q + 'LIMIT 20';

  irc_addtext(Netname, Channel, 'Race stats of users:');
  irc_addtext(Netname, Channel, 'Query interval from %s to %s', [MyDateToStr(IncDay(now, -1 * d)), MyDateToStr(now)]);
  irc_addtext(Netname, Channel, 'Group: %s', [groupname]);
  if sectionname <> '' then
    irc_addtext(Netname, Channel, 'Section: %s', [sectionname]);

  q := statsQuery(q);

  while (True) do
  begin
    ss := elsosor(q);
    if ss = '' then
      break;
    irc_addtext(Netname, Channel, ss);
  end;

  Result := True;
end;

function IrcStatUsersByGroupBySite(const netname, channel, params: String): boolean;
var
  fSaveParams, q, ss: String;
  groupname, groupfilter, sitename, sitefilter, sectionname, sectionfilter: String;
  d: integer;
begin
  fSaveParams := _StupidAntiSQLInjection(params);

  groupname := UpperCase(SubString(fSaveParams, ' ', 1));
  groupfilter := ' AND groupname like ''' + groupname + ''' ' + #13#10;
  sitename := UpperCase(SubString(fSaveParams, ' ', 2));
  sitefilter := ' AND sitename like ''' + sitename + ''' ' + #13#10;

  d := 0;
  sectionfilter := '';
  sectionname := UpperCase(SubString(fSaveParams, ' ', 3));
  if sectionname <> '' then
  begin
    d := StrToIntDef(sectionname, 0);
    if d <= 0 then
    begin
      d := StrToIntDef(SubString(fSaveParams, ' ', 4), 0);
      sectionfilter := ' AND section like ''' + UpperCase(sectionname) + ''' ' + #13#10;
    end
    else
      sectionname := '';
  end;

  if d <= 0 then
    d := 7; // default.

  q := 'SELECT username, ROUND(CAST(SUM(filesize) AS REAL)/1024,1) AS s ' + #13#10;
  q := q + 'FROM hit ';
  q := q + 'WHERE ts > DATETIME(''now'',''-' + IntToStr(d) + ' day'') ' + #13#10;
  q := q + sectionfilter;
  q := q + groupfilter;
  q := q + sitefilter;
  q := q + 'GROUP BY username ' + #13#10;
  q := q + 'ORDER BY s DESC ' + #13#10;
  q := q + 'LIMIT 20';

  irc_addtext(Netname, Channel, 'Race stats of users:');
  irc_addtext(Netname, Channel, 'Query interval from %s to %s', [MyDateToStr(IncDay(now, -1 * d)), MyDateToStr(now)]);
  irc_addtext(Netname, Channel, 'Group: %s', [groupname]);
  irc_addtext(Netname, Channel, 'Site: %s', [sitename]);
  if sectionname <> '' then
    irc_addtext(Netname, Channel, 'Section: %s', [sectionname]);

  q := statsQuery(q);

  while (True) do
  begin
    ss := elsosor(q);
    if ss = '' then
      break;
    irc_addtext(Netname, Channel, ss);
  end;

  Result := True;
end;

function IrcStatRaces(const netname, channel, params: String): boolean;
var
  sitename, period: String;
  detailed: Boolean;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  period := UpperCase(SubString(params, ' ', 2));
  detailed := StrToBoolDef(SubString(params, ' ', 3), True);

  if (sitename <> '*') then
  begin
    if FindSiteByName(Netname, sitename) = nil then
    begin
      irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
      exit;
    end;
  end;

  if ((period <> 'YEAR') and (period <> 'MONTH')) then
  begin
    period := 'DAY';
  end;

  StatRaces(Netname, Channel, sitename, period, detailed);

  Result := True;
end;

end.