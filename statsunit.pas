unit statsunit;

interface

uses slsqlite, dirlist;

procedure statsStart;
procedure statsInit;
procedure statsUninit;
procedure statsBeginTransaction();
procedure statsEndTransaction();
function statsQuery(const q: String): String;

function StatsAlive: boolean;

procedure statsProcessRace(const sitesrc, sitedst, rls_section, rls, filename: String; filesize: Int64);

procedure statsProcessDirlist(d: TDirlist; sitename, rls_section, username: String);

procedure StatRaces(netname, channel, sitename, period: String; detailed: Boolean);

procedure RecalcSizeValueAndUnit(var size: double; out sizevalue: String; StartFromSizeUnit: Integer = 0);

procedure RemoveStats(const sitename: String); overload;
procedure RemoveStats(const sitename, section: String); overload;


implementation

uses DateUtils, debugunit, configunit, sitesunit, queueunit, SysUtils, pazo, kb, ranksunit, irc;

const
  section = 'stats';

var
  statsInsert: Psqlite3_stmt;
  statsRace: Psqlite3_stmt;
  stats: TslSqliteDB;


function StatsAlive: boolean;
begin
  if stats = nil then
    Result := False
  else
    Result := true;
end;

function statsQuery(const q: String): String;
var
  s: Psqlite3_stmt;
  i: Integer;
  size: Double;
  s_unit: String;
begin
  s := stats.Open(q);
  i := 1;
  Result := '';
  while stats.Step(s) do
  begin
    {$IFDEF FPC}
      size := StrToInt(StringReplace(stats.column_text(s, 1), '.', DefaultFormatSettings.DecimalSeparator, [rfReplaceAll, rfIgnoreCase]));
    {$ELSE}
      size := StrToInt(StringReplace(stats.column_text(s, 1), '.', {$IFDEF UNICODE}FormatSettings.DecimalSeparator{$ELSE}DecimalSeparator{$ENDIF}, [rfReplaceAll, rfIgnoreCase]));
    {$ENDIF}

    RecalcSizeValueAndUnit(size, s_unit, 2);

    Result := Result + Format('%d. %s (%.2f %s)', [i, stats.column_text(s, 0), size, s_unit]) + #13#10;
    inc(i);
  end;
end;

procedure RecalcSizeValueAndUnit(var size: double; out sizevalue: String; StartFromSizeUnit: Integer = 0);
{$I common.inc}
begin
  if ((StartFromSizeUnit > FileSizeUnitCount) or (StartFromSizeUnit < 0)) then
  begin
    Debug(dpError, section, Format('[EXCEPTION] RecalcSizeValueAndUnit : %d cannot be smaller or bigger than %d', [StartFromSizeUnit, FileSizeUnitCount]));
    exit;
  end;

  while ( (size >= 1024) and (StartFromSizeUnit < FileSizeUnitCount) ) do
  begin
    size := size / 1024;
    Inc(StartFromSizeUnit);
  end;

  if (StartFromSizeUnit > FileSizeUnitCount) then
  begin
    Debug(dpError, section, Format('[EXCEPTION] RecalcSizeValueAndUnit : %d cannot be bigger than %d', [StartFromSizeUnit, FileSizeUnitCount]));
    exit;
  end;

  sizevalue := FileSizeUnits[StartFromSizeUnit];
end;

procedure RemoveStats(const sitename: String); overload;
begin
  if stats = nil then
    Exit;
  stats.ExecSQL('DELETE FROM hit WHERE sitename = ' + chr(39) + sitename + chr(39) + ';');
  stats.ExecSQL('DELETE FROM race WHERE ( sitedst = ' + chr(39) + sitename + chr(39) + ' OR sitedst = ' + chr(39) + sitename + chr(39) + ' );');
end;

procedure RemoveStats(const sitename, section: String); overload;
begin
  if stats = nil then
    Exit;
  stats.ExecSQL('DELETE FROM hit WHERE sitename = ' + chr(39) + sitename + chr(39) + ' AND section = ' + chr(39) + section + chr(39) + ';');
  stats.ExecSQL('DELETE FROM race WHERE ( sitedst = ' + chr(39) + sitename + chr(39) + ' OR sitedst = ' + chr(39) + sitename + chr(39) + ' ) AND section = ' + chr(39) + section + chr(39) + ';');
end;


procedure StatRaces(netname, channel, sitename, period: String; detailed: Boolean);
var
  q, sql_period: String;
  s: Psqlite3_stmt;
  s_size, s_unit: String;
  size, size_all_out, size_all_in: Double;
  i, files_per_site_in, files_per_site_out, files_all_in, files_all_out: Integer;
begin
  files_per_site_out := 0;
  files_per_site_in := 0;
  files_all_in := 0;
  files_all_out := 0;
  size_all_out := 0;
  size_all_in := 0;

  if (period = 'MONTH') then
  begin
    sql_period := 'start of month';
  end
  else if (period = 'YEAR') then
  begin
    sql_period := 'start of year';
  end
  else
  begin
    sql_period := 'start of day';
  end;

  if sitename = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      if (TSite(sites.Items[i]).Name = getAdminSiteName) then
        Continue;

      irc_addtext(netname, channel, Format('%s race stats of site: <b><c07>%s</c></b>', [period, TSite(sites.Items[i]).Name]));

      q := 'SELECT count(*) AS files, ROUND(CAST(SUM(filesize) AS REAL)/1024,1) AS size FROM race WHERE sitesrc = '+chr(39)+TSite(sites.Items[i]).Name+chr(39)+' AND ts > date('+chr(39)+'now'+chr(39)+','+chr(39)+sql_period+chr(39)+');';
      s := stats.Open(q);
      while stats.Step(s) do
      begin
        {$IFDEF FPC}
          s_size := StringReplace(stats.column_text(s, 1), '.', DefaultFormatSettings.DecimalSeparator, [rfReplaceAll, rfIgnoreCase]);
        {$ELSE}
          s_size := StringReplace(stats.column_text(s, 1), '.', {$IFDEF UNICODE}FormatSettings.DecimalSeparator{$ELSE}DecimalSeparator{$ENDIF}, [rfReplaceAll, rfIgnoreCase]);
        {$ENDIF}
        size := StrToFloatDef(s_size, 0);
        size_all_out := size + size_all_out;
        files_per_site_out := StrToInt(stats.column_text(s, 0));
        files_all_out := files_all_out + files_per_site_out;

        RecalcSizeValueAndUnit(size, s_unit, 1);

        irc_addtext(netname, channel, Format('TOTAL <b>out</b>: <c04>%.2f</c> %s (%s files)', [size, s_unit, stats.column_text(s, 0)]));
      end;

      q := 'SELECT count(*) AS files, ROUND(CAST(SUM(filesize) AS REAL)/1024,1) AS size FROM race WHERE sitedst = '+chr(39)+TSite(sites.Items[i]).Name+chr(39)+' AND ts > date('+chr(39)+'now'+chr(39)+','+chr(39)+sql_period+chr(39)+');';
      s := stats.Open(q);
      while stats.Step(s) do
      begin
        {$IFDEF FPC}
          s_size := StringReplace(stats.column_text(s, 1), '.', DefaultFormatSettings.DecimalSeparator, [rfReplaceAll, rfIgnoreCase]);
        {$ELSE}
          s_size := StringReplace(stats.column_text(s, 1), '.', {$IFDEF UNICODE}FormatSettings.DecimalSeparator{$ELSE}DecimalSeparator{$ENDIF}, [rfReplaceAll, rfIgnoreCase]);
        {$ENDIF}
        size := StrToFloatDef(s_size, 0);
        size_all_in := size + size_all_in;
        files_per_site_in := StrToInt(stats.column_text(s, 0));
        files_all_in := files_all_in + files_per_site_in;

        RecalcSizeValueAndUnit(size, s_unit, 1);

        irc_addtext(netname, channel, Format('TOTAL <b>in</b>:  <c09>%.2f</c> %s (%s files)', [size, s_unit, stats.column_text(s, 0)]));
      end;

    end;

    RecalcSizeValueAndUnit(size_all_out, s_unit, 1);

    irc_addtext(netname, channel, Format('<b>Total In + Out:</b> <c07>%.2f</c> %s (%d files)', [size_all_out, s_unit, files_all_out]));
  end
  else
  begin
    irc_addtext(netname, channel, Format('%s race stats of site: <b><c07>%s</c></b>', [period, sitename]));

    q := 'SELECT count(*) AS files, ROUND(CAST(SUM(filesize) AS REAL)/1024,1) AS size FROM race WHERE sitesrc = '+chr(39)+sitename+chr(39)+' AND ts > date('+chr(39)+'now'+chr(39)+','+chr(39)+sql_period+chr(39)+');';
    s := stats.Open(q);
    while stats.Step(s) do
    begin
      {$IFDEF FPC}
        s_size := StringReplace(stats.column_text(s, 1), '.', DefaultFormatSettings.DecimalSeparator, [rfReplaceAll, rfIgnoreCase]);
      {$ELSE}
        s_size := StringReplace(stats.column_text(s, 1), '.', {$IFDEF UNICODE}FormatSettings.DecimalSeparator{$ELSE}DecimalSeparator{$ENDIF}, [rfReplaceAll, rfIgnoreCase]);
      {$ENDIF}
      size := StrToFloatDef(s_size, 0);

      RecalcSizeValueAndUnit(size, s_unit, 1);

      irc_addtext(netname, channel, Format('TOTAL <b>out</b>: <c04>%.2f</c> %s (%s files)', [size, s_unit, stats.column_text(s, 0)]));
    end;

    q := 'SELECT count(*) AS files, ROUND(CAST(SUM(filesize) AS REAL)/1024,1) AS size FROM race WHERE sitedst = '+chr(39)+sitename+chr(39)+' AND ts > date('+chr(39)+'now'+chr(39)+','+chr(39)+sql_period+chr(39)+');';
    s := stats.Open(q);
    while stats.Step(s) do
    begin
      {$IFDEF FPC}
        s_size := StringReplace(stats.column_text(s, 1), '.', DefaultFormatSettings.DecimalSeparator, [rfReplaceAll, rfIgnoreCase]);
      {$ELSE}
        s_size := StringReplace(stats.column_text(s, 1), '.', {$IFDEF UNICODE}FormatSettings.DecimalSeparator{$ELSE}DecimalSeparator{$ENDIF}, [rfReplaceAll, rfIgnoreCase]);
      {$ENDIF}
      size := StrToFloatDef(s_size, 0);

      RecalcSizeValueAndUnit(size, s_unit, 1);

      irc_addtext(netname, channel, Format('TOTAL <b>in</b>:  <c09>%.2f</c> %s (%s files)', [size, s_unit, stats.column_text(s, 0)]));
    end;

    if not detailed then Exit;

    q := 'SELECT DISTINCT sitedst, COUNT(filename) AS files, ROUND(CAST(SUM(filesize) AS REAL)/1024,1) AS size FROM race WHERE sitesrc = '+ chr(39) + sitename + chr(39) +' AND ts > date('+ chr(39) + 'now' + chr(39) + ',' + chr(39) + sql_period + chr(39) +') GROUP BY sitedst ORDER BY size';
    s := stats.Open(q);
    while stats.Step(s) do
    begin
      {$IFDEF FPC}
        s_size := StringReplace(stats.column_text(s, 2), '.', DefaultFormatSettings.DecimalSeparator, [rfReplaceAll, rfIgnoreCase]);
      {$ELSE}
        s_size := StringReplace(stats.column_text(s, 2), '.', {$IFDEF UNICODE}FormatSettings.DecimalSeparator{$ELSE}DecimalSeparator{$ENDIF}, [rfReplaceAll, rfIgnoreCase]);
      {$ENDIF}
      size := StrToFloatDef(s_size, 0);

      RecalcSizeValueAndUnit(size, s_unit, 1);

      irc_addtext(netname, channel, Format('  <b>to</b> %s: %.2f %s (%s files)', [stats.column_text(s, 0), size, s_unit, stats.column_text(s, 1)]));
    end;

    q := 'SELECT DISTINCT sitesrc, COUNT(filename) AS files, ROUND(CAST(SUM(filesize) AS REAL)/1024,1) AS size FROM race WHERE sitedst = '+ chr(39) + sitename + chr(39) +' AND ts > date('+ chr(39) + 'now' + chr(39) + ',' + chr(39) + sql_period + chr(39) +') GROUP BY sitesrc ORDER BY size';
    s := stats.Open(q);
    while stats.Step(s) do
    begin
      {$IFDEF FPC}
        s_size := StringReplace(stats.column_text(s, 2), '.', DefaultFormatSettings.DecimalSeparator, [rfReplaceAll, rfIgnoreCase]);
      {$ELSE}
        s_size := StringReplace(stats.column_text(s, 2), '.', {$IFDEF UNICODE}FormatSettings.DecimalSeparator{$ELSE}DecimalSeparator{$ENDIF}, [rfReplaceAll, rfIgnoreCase]);
      {$ENDIF}
      size := StrToFloatDef(s_size, 0);

      RecalcSizeValueAndUnit(size, s_unit, 1);

      irc_addtext(netname, channel, Format('  <b>from</b> %s: %.2f %s (%s files)', [stats.column_text(s, 0), size, s_unit, stats.column_text(s, 1)]));
    end;

  end;
end;

procedure statsStart;
var
  s: String;
begin
  if slsqlite_inited then
  begin
    s := Trim(config.ReadString(section, 'database', ''));
    if s = '' then exit;

    stats := TslSqliteDB.Create(s, config.ReadString(section, 'pragma', ''));

    stats.ExecSQL(
      'CREATE TABLE IF NOT EXISTS hit ('+
      ' sitename VARCHAR(50) NOT NULL, '+
      ' section VARCHAR(50) NOT NULL, '+
      ' username VARCHAR(55) NOT NULL, '+
      ' groupname VARCHAR(55) NOT NULL, '+
      ' filename VARCHAR(255) NOT NULL, '+
      ' filesize INT UNSIGNED NOT NULL, '+
      ' ts DATETIME NOT NULL '+
      ')'
    );

    stats.ExecSQL(
      'CREATE UNIQUE INDEX IF NOT EXISTS hit_index ON race (sitename, section, filename)'
    );
    stats.ExecSQL(
      'CREATE INDEX IF NOT EXISTS sitesection_index ON hit (sitename, section)'
    );
    stats.ExecSQL(
      'CREATE INDEX IF NOT EXISTS group_index ON hit (groupname, section)'
    );

    stats.ExecSQL(
      'CREATE TABLE IF NOT EXISTS race ('+
      ' sitesrc VARCHAR(50) NOT NULL, '+
      ' sitedst VARCHAR(50) NOT NULL, '+
      ' section VARCHAR(50) NOT NULL, '+
      ' rls VARCHAR(255) NOT NULL, '+
      ' filename VARCHAR(255) NOT NULL, '+
      ' filesize INT UNSIGNED NOT NULL, '+
      ' ts DATETIME NOT NULL '+
      ')'
    );

    stats.ExecSQL(
      'CREATE UNIQUE INDEX IF NOT EXISTS race_index ON race (sitesrc, sitedst, section, rls, filename)'
    );
    stats.ExecSQL(
      'CREATE INDEX IF NOT EXISTS race_sitesrc ON race (sitesrc)'
    );
    stats.ExecSQL(
      'CREATE INDEX IF NOT EXISTS race_sitedst ON race (sitedst)'
    );
    stats.ExecSQL(
      'CREATE INDEX IF NOT EXISTS race_section ON race (section)'
    );
    stats.ExecSQL(
      'CREATE INDEX IF NOT EXISTS race_rls ON race (rls)'
    );

    statsInsert:= stats.Open('INSERT OR IGNORE INTO hit (sitename, section, username, groupname, filename, filesize, ts) VALUES (?, ?, ?, ?, ?, ?, ?)');

    statsRace:= stats.Open('INSERT OR IGNORE INTO race (sitesrc, sitedst, section, rls, filename, filesize, ts) VALUES (?, ?, ?, ?, ?, ?, ?)');
  end;
end;

procedure statsProcessRace(const sitesrc, sitedst, rls_section, rls, filename: String; filesize: Int64);
var
  s_src, s_dst: TSite;
begin
  if stats = nil then
    exit;

  if statsRace = nil then
    exit;

  if (filesize < config.ReadInteger(section, 'min_filesize', 5000000)) then
  begin
    Debug(dpSpam, section, Format('[statsProcessRace] Filesize %d for %s too small for adding to stats', [filesize, filename]));
    exit;
  end;

  s_src := FindSiteByName('', sitesrc);
  s_dst := FindSiteByName('', sitedst);

  if ((s_src = nil) or (s_dst = nil)) then
  begin
    Debug(dpSpam, section, '[statsProcessRace] SRC or DST site is not found!');
    exit;
  end;

  try
    stats.ExecSQL( statsRace, [uppercase(s_src.name), uppercase(s_dst.name), uppercase(rls_section), rls, filename, filesize, FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)]);
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] statsProcessRace : %s', [e.Message]));
      exit;
    end;
  end;
end;

procedure statsProcessDirlist(d: TDirlist; sitename, rls_section, username: String);
var
  i: Integer;
  de: TDirlistEntry;
  u: String;
begin
  if d = nil then
    exit;
  if d.entries = nil then
    exit;
  if stats = nil then
    exit;
  if statsInsert = nil then
    exit;

  d.dirlist_lock.Enter;
  try
    for i := 0 to d.entries.Count - 1 do
    begin
      try if i > d.entries.Count then Break; except Break; end;
      try
        de:= TDirlistEntry(d.entries[i]);
        if not de.directory then
        begin
          if ((de.megvanmeg) and (not de.skiplisted) and (de.username <> '') and (de.filesize >= config.ReadInteger(section, 'min_filesize', 1000000))) then
          begin
            if de.username <> username then
              u := de.username
            else
              u := '!me!';
            stats.ExecSQL( statsInsert, [uppercase(sitename), uppercase(rls_section), u, UpperCase(de.groupname), de.filename, Round(de.filesize / 1024), FormatDateTime('yyyy-mm-dd hh:nn:ss', de.timestamp)]);
          end;
        end
        else
          statsProcessDirlist(de.subdirlist, sitename, rls_section, username);
      except
        on E: Exception do
        begin
          Debug(dpError, section, Format('[EXCEPTION] statsProcessDirlist : %s', [e.Message]));
          Break;
        end;
      end;
    end;
  finally
    d.dirlist_lock.Leave;
  end;
end;

procedure statsInit;
begin
//  statsFilename:= ExtractFilePath(ParamStr(0))+'stats.db';
end;

procedure statsUninit;
begin
  Debug(dpSpam, section, 'Uninit1');
  if stats <> nil then
  begin
    stats.Free;
    stats := nil;
  end;

  Debug(dpSpam, section, 'Uninit2');
end;

procedure statsBeginTransaction();
begin
  if stats = nil then
    exit;

  stats.ExecSQL('BEGIN TRANSACTION');
end;

procedure statsEndTransaction();
begin
  if stats = nil then
    exit;

  stats.ExecSQL('COMMIT TRANSACTION');
end;

end.
