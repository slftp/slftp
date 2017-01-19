unit statsunit;

interface

uses slsqlite, slmysql2, dirlist;

procedure statsStart;
procedure statsInit;
procedure statsUninit;
procedure statsBeginTransaction();
procedure statsEndTransaction();
function statsQuery(const q: AnsiString): AnsiString;

function StatsAlive:boolean;

procedure statsProcessRace(sitesrc, sitedst, rls_section, rls, filename, filesize: AnsiString);

procedure statsProcessDirlist(d: TDirlist; sitename, rls_section, username: AnsiString);

procedure StatRaces(netname, channel, sitename, period: AnsiString; detailed: Boolean);

procedure RecalcSizeValueAndUnit(var size: double; out sizevalue: AnsiString; StartFromSizeUnit: Integer = 0);


implementation

uses DateUtils, debugunit, configunit, sitesunit, queueunit, SysUtils, pazo, kb, ranksunit, irc;

const section = 'stats';

var
    statsInsert: Psqlite3_stmt;
    statsRace: Psqlite3_stmt;
    stats: TslSqliteDB;

function StatsAlive:boolean;
begin
  if stats = nil then result:= False else result:=true;
end;

function statsQuery(const q: AnsiString): AnsiString;
var
  s: Psqlite3_stmt;
  i: Integer;
  size: Double;
  s_unit: AnsiString;
begin
  s := stats.Open(q);
  i := 1;
  Result := '';
  while stats.Step(s) do
  begin
    {$IFDEF FPC}
    size := StrToInt(StringReplace(stats.column_text(s, 1), '.', DefaultFormatSettings.DecimalSeparator, [rfReplaceAll, rfIgnoreCase]));
    {$ELSE}
    size := StrToInt(StringReplace(stats.column_text(s, 1), '.', DecimalSeparator, [rfReplaceAll, rfIgnoreCase]));
    {$ENDIF}
    s_unit := 'MB';

    if size > 1024  then begin
      size := size / 1024;
      s_unit := 'GB';
    end;
    if size > 1024  then begin
      size := size / 1024;
      s_unit := 'TB';
    end;

    //RecalcSizeValueAndUnit(size, s_unit, 2);

    Result := Result + Format('%d. %s (%.2f %s)', [i, stats.column_text(s, 0), size,s_unit])+#13#10;
    inc(i);
  end;
end;

procedure RecalcSizeValueAndUnit(var size: double; out sizevalue: AnsiString; StartFromSizeUnit: Integer = 0);
{$I common.inc}
begin
  if (StartFromSizeUnit > FileSizeUnitCount or StartFromSizeUnit < 0) then
  begin
    Debug(dpError, section, Format('[EXCEPTION] RecalcSizeValueAndUnit : %d cannot be smaller or bigger than %d', [StartFromSizeUnit, FileSizeUnitCount]));
    exit;
  end;

  while (size >= 1024) do
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

procedure StatRaces(netname, channel, sitename, period: AnsiString; detailed: Boolean);
var
  q, sql_period: AnsiString;
  s: Psqlite3_stmt;
  s_size, s_unit: AnsiString;
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
      if (TSite(sites.Items[i]).Name = config.ReadString('sites', 'admin_sitename', 'SLFTP')) then
        Continue;

      irc_addtext(netname, channel, Format('%s race stats of site: <b><c07>%s</c></b>', [period, TSite(sites.Items[i]).Name]));

      q := 'SELECT count(*) AS files, ROUND(CAST(SUM(filesize) AS REAL)/1024,1) AS size FROM race WHERE sitesrc = '+chr(39)+TSite(sites.Items[i]).Name+chr(39)+' AND ts > date('+chr(39)+'now'+chr(39)+','+chr(39)+sql_period+chr(39)+');';
      s := stats.Open(q);
      while stats.Step(s) do
      begin
        {$IFDEF FPC}
          s_size := StringReplace(stats.column_text(s, 1), '.', DefaultFormatSettings.DecimalSeparator, [rfReplaceAll, rfIgnoreCase]);
        {$ELSE}
          s_size := StringReplace(stats.column_text(s, 1), '.', DecimalSeparator, [rfReplaceAll, rfIgnoreCase]);
        {$ENDIF}
        size := StrToFloatDef(s_size, 0);
        size_all_out := size + size_all_out;
        files_per_site_out := StrToInt(stats.column_text(s, 0));
        files_all_out := files_all_out + files_per_site_out;

{
        s_unit := 'KB';
        if size > 1024 then
        begin
          size := size / 1024;
          s_unit := 'MB';
        end;
        if size > 1024 then
        begin
          size := size / 1024;
          s_unit := 'GB';
        end;
        if size > 1024 then
        begin
          size := size / 1024;
          s_unit := 'TB';
        end;
}

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
          s_size := StringReplace(stats.column_text(s, 1), '.', DecimalSeparator, [rfReplaceAll, rfIgnoreCase]);
        {$ENDIF}
        size := StrToFloatDef(s_size, 0);
        size_all_in := size + size_all_in;
        files_per_site_in := StrToInt(stats.column_text(s, 0));
        files_all_in := files_all_in + files_per_site_in;

{
        s_unit := 'KB';
        if size > 1024 then
        begin
          size := size / 1024;
          s_unit := 'MB';
        end;
        if size > 1024 then
        begin
         size := size / 1024;
         s_unit := 'GB';
        end;
        if size > 1024 then
        begin
          size := size / 1024;
          s_unit := 'TB';
        end;
}

        RecalcSizeValueAndUnit(size, s_unit, 1);

        irc_addtext(netname, channel, Format('TOTAL <b>in</b>:  <c09>%.2f</c> %s (%s files)', [size, s_unit, stats.column_text(s, 0)]));
      end;

    end;

{
    //in and out files/size are the same
    // size_all_out == size_all_in
    s_unit := 'KB';
    if size_all_out > 1024 then
    begin
      size_all_out := size_all_out / 1024;
      s_unit := 'MB';
    end;
    if size_all_out > 1024 then
    begin
      size_all_out := size_all_out / 1024;
      s_unit := 'GB';
    end;
    if size_all_out > 1024 then
    begin
      size_all_out := size_all_out / 1024;
      s_unit := 'TB';
    end;
}

    RecalcSizeValue(size_all_out, s_unit, 1);

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
        s_size := StringReplace(stats.column_text(s, 1), '.', DecimalSeparator, [rfReplaceAll, rfIgnoreCase]);
      {$ENDIF}
      size := StrToFloatDef(s_size, 0);

{
      s_unit := 'KB';
      if size > 1024 then
      begin
        size := size / 1024;
        s_unit := 'MB';
      end;
      if size > 1024 then
      begin
        size := size / 1024;
        s_unit := 'GB';
      end;
      if size > 1024 then
      begin
        size := size / 1024;
        s_unit := 'TB';
      end;
}

      RecalcSizeValue(size, s_unit, 1);

      irc_addtext(netname, channel, Format('TOTAL <b>out</b>: <c04>%.2f</c> %s (%s files)', [size, s_unit, stats.column_text(s, 0)]));
    end;

    q := 'SELECT count(*) AS files, ROUND(CAST(SUM(filesize) AS REAL)/1024,1) AS size FROM race WHERE sitedst = '+chr(39)+sitename+chr(39)+' AND ts > date('+chr(39)+'now'+chr(39)+','+chr(39)+sql_period+chr(39)+');';
    s := stats.Open(q);
    while stats.Step(s) do
    begin
      {$IFDEF FPC}
        s_size := StringReplace(stats.column_text(s, 1), '.', DefaultFormatSettings.DecimalSeparator, [rfReplaceAll, rfIgnoreCase]);
      {$ELSE}
        s_size := StringReplace(stats.column_text(s, 1), '.', DecimalSeparator, [rfReplaceAll, rfIgnoreCase]);
      {$ENDIF}
      size := StrToFloatDef(s_size, 0);

{
      s_unit := 'KB';
      if size > 1024 then
      begin
        size := size / 1024;
        s_unit := 'MB';
      end;
      if size > 1024 then
      begin
        size := size / 1024;
        s_unit := 'GB';
      end;
      if size > 1024 then
      begin
        size := size / 1024;
        s_unit := 'TB';
      end;
}
      RecalcSizeValue(size, s_unit, 1);

      irc_addtext(netname, channel, Format('TOTAL <b>in</b>:  <c09>%.2f</c> %s (%s files)', [size, s_unit, stats.column_text(s, 0)]));
    end;

    if not detailed then Exit;

    q := 'SELECT DISTINCT sitedst, COUNT(filename) AS files, ROUND(CAST(SUM(filesize) AS REAL)/1024,1) AS size FROM race WHERE sitesrc = '+chr(39)+sitename+chr(39)+' AND ts > date('+chr(39)+'now'+chr(39)+','+chr(39)+sql_period+chr(39)+') GROUP BY sitedst ORDER BY sitedst';
    s := stats.Open(q);
    while stats.Step(s) do
    begin
      {$IFDEF FPC}
        s_size := StringReplace(stats.column_text(s, 2), '.', DefaultFormatSettings.DecimalSeparator, [rfReplaceAll, rfIgnoreCase]);
      {$ELSE}
        s_size := StringReplace(stats.column_text(s, 2), '.', DecimalSeparator, [rfReplaceAll, rfIgnoreCase]);
      {$ENDIF}
      size := StrToFloatDef(s_size, 0);

{
      s_unit := 'KB';
      if size > 1024 then
      begin
        size := size / 1024;
        s_unit := 'MB';
      end;
      if size > 1024 then
      begin
        size := size / 1024;
        s_unit := 'GB';
      end;
      if size > 1024 then
      begin
        size := size / 1024;
        s_unit := 'TB';
      end;
}

      RecalcSizeValue(size, s_unit, 1);

      irc_addtext(netname, channel, Format('  <b>to</b> %s: %.2f %s (%s files)', [stats.column_text(s, 0), size, s_unit, stats.column_text(s, 1)]));
    end;

    q := 'SELECT DISTINCT sitesrc, COUNT(filename) AS files, ROUND(CAST(SUM(filesize) AS REAL)/1024,1) AS size FROM race WHERE sitedst = '+chr(39)+sitename+chr(39)+' AND ts > date('+chr(39)+'now'+chr(39)+','+chr(39)+sql_period+chr(39)+') GROUP BY sitesrc ORDER BY sitesrc';
    s := stats.Open(q);
    while stats.Step(s) do
    begin
      {$IFDEF FPC}
        s_size := StringReplace(stats.column_text(s, 2), '.', DefaultFormatSettings.DecimalSeparator, [rfReplaceAll, rfIgnoreCase]);
      {$ELSE}
        s_size := StringReplace(stats.column_text(s, 2), '.', DecimalSeparator, [rfReplaceAll, rfIgnoreCase]);
      {$ENDIF}
      size := StrToFloatDef(s_size, 0);

{
      s_unit := 'KB';
      if size > 1024 then
      begin
        size := size / 1024;
        s_unit := 'MB';
      end;
      if size > 1024 then
      begin
        size := size / 1024;
        s_unit := 'GB';
      end;
      if size > 1024 then
      begin
        size := size / 1024;
        s_unit := 'TB';
      end;
}

      RecalcSizeValue(size, s_unit, 1);

      irc_addtext(netname, channel, Format('  <b>from</b> %s: %.2f %s (%s files)', [stats.column_text(s, 0), size, s_unit, stats.column_text(s, 1)]));
    end;

  end;
end;

procedure statsStart;
var s: AnsiString;
begin
  if slsqlite_inited then
  begin
    s:= Trim(config.ReadString(section, 'database', ''));
    if s = '' then exit;

    stats:= TslSqliteDB.Create(s, config.ReadString(section, 'pragma', ''));

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

procedure statsProcessRace(sitesrc, sitedst, rls_section, rls, filename, filesize: AnsiString);
var s_src, s_dst: TSite;
begin
  if stats = nil then exit;
  if statsRace = nil then exit;

  if (StrToIntDef(filesize, 0) < config.ReadInteger(section, 'min_filesize', 1000000)) then exit;


  s_src:= FindSiteByName('', sitesrc);
  s_dst:= FindSiteByName('', sitedst);
  if ((s_src = nil) or (s_dst = nil)) then exit;


  try
    stats.ExecSQL( statsRace, [uppercase(s_src.name), uppercase(s_dst.name), uppercase(rls_section), rls, filename, StrToIntDef(filesize, 0), FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)]);
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] statsProcessRace : %s', [e.Message]));
      exit;
    end;
  end;
end;

procedure statsProcessDirlist(d: TDirlist; sitename, rls_section, username: AnsiString);
var i: Integer;
    de: TDirlistEntry;
    u: AnsiString;
begin
  if d = nil then exit;
  if d.entries = nil then exit;
  if stats = nil then exit;
  if statsInsert = nil then exit;

  for i:= 0 to d.entries.Count -1 do
  begin
    try if i > d.entries.Count then Break; except Break; end;
    try
      de:= TDirlistEntry(d.entries[i]);
      if not de.directory then
      begin
        if ((de.megvanmeg) and (not de.skiplisted) and (de.username <> '') and (de.filesize >= config.ReadInteger(section, 'min_filesize', 1000000))) then
        begin
          if de.username <> username then
            u:= de.username
          else
            u:= '!me!';
          stats.ExecSQL( statsInsert, [uppercase(sitename), uppercase(rls_section), u, UpperCase(de.groupname), de.filename, Round(de.filesize / 1024), FormatDateTime('yyyy-mm-dd hh:nn:ss', de.timestamp)]);
        end;
      end else
        statsProcessDirlist(de.subdirlist, sitename, rls_section, username);
    except
      on E: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] statsProcessDirlist : %s', [e.Message]));
        Break;
      end;
    end;
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
    stats:= nil;
  end;

  Debug(dpSpam, section, 'Uninit2');
end;
procedure statsBeginTransaction();
begin
  if stats = nil then exit;

  stats.ExecSQL('BEGIN TRANSACTION');
end;
procedure statsEndTransaction();
begin
  if stats = nil then exit;

  stats.ExecSQL('COMMIT TRANSACTION');
end;

end.
