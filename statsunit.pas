unit statsunit;

interface

uses
  dirlist;

procedure statsStart;
procedure statsInit;
procedure statsUninit;
function statsQuery(const aQuery: String): String;

function StatsAlive: boolean;

procedure statsProcessRace(const sitesrc, sitedst, rls_section, rls, filename: String; filesize: Int64);

procedure statsProcessDirlist(d: TDirlist; const sitename, rls_section, username: String);

procedure StatRaces(const netname, channel, sitename, period: String; detailed: Boolean);

procedure RecalcSizeValueAndUnit(var size: double; out sizevalue: String; StartFromSizeUnit: Integer = 0);

procedure RemoveStats(const sitename: String); overload;
procedure RemoveStats(const sitename, section: String); overload;


implementation

uses
  Classes, SyncObjs, Contnrs, DateUtils, debugunit, configunit, sitesunit, queueunit,
  SysUtils, pazo, kb, ranksunit, irc, dbhandler, SynDBSQLite3, SynDB;

const
  section = 'stats';

var
  statsSQLite3DBCon: TSQLDBSQLite3ConnectionProperties = nil; //< SQLite3 database connection
  SQLite3Lock: TCriticalSection = nil; //< Critical Section used for read/write blocking as concurrently does not work flawless

function StatsAlive: boolean;
begin
  if statsSQLite3DBCon = nil then
    Result := False
  else
    Result := true;
end;

function statsQuery(const aQuery: String): String;
var
  fQuery: TQuery;
  i: Integer;
  fSize: Double;
  fSizeUnit: String;
begin
  Result := '';
  i := 1;

  SQLite3Lock.Enter;
  try
    fQuery := TQuery.Create(statsSQLite3DBCon.ThreadSafeConnection);
    try
      // aQuery is already filled with the wanted query text
      fQuery.SQL.Text := aQuery;
      try
        fQuery.Open;

        if not fQuery.IsEmpty then
        begin
          fQuery.First;

          while not fQuery.Eof do
          begin
            fSize := StrToInt(StringReplace(fQuery.Fields[1].AsString, '.', {$IFNDEF FPC}FormatSettings.DecimalSeparator{$ELSE}DefaultFormatSettings.DecimalSeparator{$ENDIF}, [rfReplaceAll, rfIgnoreCase]));
            RecalcSizeValueAndUnit(fSize, fSizeUnit, 2);
            Result := Result + Format('%d. %s (%.2f %s)', [i, fQuery.Fields[0].AsString, fSize, fSizeUnit]) + #13#10;
            Inc(i);
            fQuery.Next;
          end;
        end;
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[EXCEPTION] statsQuery: %s with %s', [e.Message, aQuery]));
          exit;
        end;
      end;
    finally
      fQuery.free;
    end;
  finally
    SQLite3Lock.Leave;
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
var
  fQuery: TQuery;
begin
  SQLite3Lock.Enter;
  try
    fQuery := TQuery.Create(statsSQLite3DBCon.ThreadSafeConnection);
    try
      try
        fQuery.SQL.Text := 'DELETE FROM hit WHERE sitename = :sitename';
        fQuery.ParamByName('sitename').AsString := sitename;
        fQuery.ExecSQL;

        // release the SQL statement, results and bound parameters before reopen
        fQuery.Close;

        fQuery.SQL.Text := 'DELETE FROM race WHERE ( sitesrc = :sitesrcname OR sitedst = :sitedstname';
        fQuery.ParamByName('sitesrcname').AsString := sitename;
        fQuery.ParamByName('sitedstname').AsString := sitename;
        fQuery.ExecSQL;
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[EXCEPTION] RemoveStats: %s', [e.Message]));
          exit;
        end;
      end;
    finally
      fQuery.free;
    end;
  finally
    SQLite3Lock.Leave;
  end;
end;

procedure RemoveStats(const sitename, section: String); overload;
var
  fQuery: TQuery;
begin
  SQLite3Lock.Enter;
  try
    fQuery := TQuery.Create(statsSQLite3DBCon.ThreadSafeConnection);
    try
      try
        fQuery.SQL.Text := 'DELETE FROM hit WHERE sitename = :sitename AND section = :section';
        fQuery.ParamByName('sitename').AsString := sitename;
        fQuery.ParamByName('section').AsString := section;
        fQuery.ExecSQL;

        // release the SQL statement, results and bound parameters before reopen
        fQuery.Close;

        fQuery.SQL.Text := 'DELETE FROM race WHERE ( sitesrc = :sitesrcname OR sitedst = :sitedstname ) AND section = :section';
        fQuery.ParamByName('sitesrcname').AsString := sitename;
        fQuery.ParamByName('sitedstname').AsString := sitename;
        fQuery.ParamByName('section').AsString := section;
        fQuery.ExecSQL;
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[EXCEPTION] RemoveStats with section %s: %s', [section, e.Message]));
          exit;
        end;
      end;
    finally
      fQuery.free;
    end;
  finally
    SQLite3Lock.Leave;
  end;
end;


procedure StatRaces(const netname, channel, sitename, period: String; detailed: Boolean);
var
  fQuery: TQuery;
  sql_period: String;
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

  SQLite3Lock.Enter;
  try
    fQuery := TQuery.Create(statsSQLite3DBCon.ThreadSafeConnection);
    try
      try
        if sitename = '*' then
        begin
          for i := 0 to sites.Count - 1 do
          begin
            if (TSite(sites.Items[i]).Name = getAdminSiteName) then
              Continue;

            irc_addtext(netname, channel, Format('%s race stats of site: <b><c07>%s</c></b>', [period, TSite(sites.Items[i]).Name]));

            // release the SQL statement, results and bound parameters before reopen
            fQuery.Close;

            fQuery.SQL.Text := 'SELECT count(*) AS files, ROUND(CAST(SUM(filesize) AS REAL)/1024,1) AS size FROM race WHERE sitesrc = :sitesrc AND ts > date(:timestring, :period)';
            fQuery.ParamByName('timestring').AsString := 'now';
            fQuery.ParamByName('sitesrc').AsString := TSite(sites.Items[i]).Name;
            fQuery.ParamByName('period').AsString := sql_period;
            fQuery.Open;

            while not fQuery.Eof do
            begin
              s_size := StringReplace(fQuery.FieldByName('size').AsString, '.', {$IFNDEF FPC}FormatSettings.DecimalSeparator{$ELSE}DefaultFormatSettings.DecimalSeparator{$ENDIF}, [rfReplaceAll, rfIgnoreCase]);
              size := StrToFloatDef(s_size, 0);
              size_all_out := size + size_all_out;
              files_per_site_out := StrToInt(fQuery.FieldByName('files').AsString);
              files_all_out := files_all_out + files_per_site_out;

              RecalcSizeValueAndUnit(size, s_unit, 1);
              irc_addtext(netname, channel, Format('TOTAL <b>out</b>: <c04>%.2f</c> %s (%s files)', [size, s_unit, fQuery.FieldByName('files').AsString]));

              fQuery.Next;
            end;

            // release the SQL statement, results and bound parameters before reopen
            fQuery.Close;

            fQuery.SQL.Text := 'SELECT count(*) AS files, ROUND(CAST(SUM(filesize) AS REAL)/1024,1) AS size FROM race WHERE sitedst = :sitedst AND ts > date(:timestring, :period)';
            fQuery.ParamByName('timestring').AsString := 'now';
            fQuery.ParamByName('sitedst').AsString := TSite(sites.Items[i]).Name;
            fQuery.ParamByName('period').AsString := sql_period;
            fQuery.Open;

            while not fQuery.Eof do
            begin
              s_size := StringReplace(fQuery.FieldByName('size').AsString, '.', {$IFNDEF FPC}FormatSettings.DecimalSeparator{$ELSE}DefaultFormatSettings.DecimalSeparator{$ENDIF}, [rfReplaceAll, rfIgnoreCase]);
              size := StrToFloatDef(s_size, 0);
              size_all_in := size + size_all_in;
              files_per_site_in := StrToInt(fQuery.FieldByName('files').AsString);
              files_all_in := files_all_in + files_per_site_in;

              RecalcSizeValueAndUnit(size, s_unit, 1);
              irc_addtext(netname, channel, Format('TOTAL <b>in</b>:  <c09>%.2f</c> %s (%s files)', [size, s_unit, fQuery.FieldByName('files').AsString]));

              fQuery.Next;
            end;
          end;

          RecalcSizeValueAndUnit(size_all_out, s_unit, 1);
          irc_addtext(netname, channel, Format('<b>Total In + Out:</b> <c07>%.2f</c> %s (%d files)', [size_all_out, s_unit, files_all_out]));
        end
        else
        begin
          irc_addtext(netname, channel, Format('%s race stats of site: <b><c07>%s</c></b>', [period, sitename]));

          fQuery.SQL.Text := 'SELECT count(*) AS files, ROUND(CAST(SUM(filesize) AS REAL)/1024,1) AS size FROM race WHERE sitesrc = :sitesrc AND ts > date(:timestring, :period)';
          fQuery.ParamByName('timestring').AsString := 'now';
          fQuery.ParamByName('sitesrc').AsString := sitename;
          fQuery.ParamByName('period').AsString := sql_period;
          fQuery.Open;

          while not fQuery.Eof do
          begin
            s_size := StringReplace(fQuery.FieldByName('size').AsString, '.', {$IFNDEF FPC}FormatSettings.DecimalSeparator{$ELSE}DefaultFormatSettings.DecimalSeparator{$ENDIF}, [rfReplaceAll, rfIgnoreCase]);
            size := StrToFloatDef(s_size, 0);

            RecalcSizeValueAndUnit(size, s_unit, 1);
            irc_addtext(netname, channel, Format('TOTAL <b>out</b>: <c04>%.2f</c> %s (%s files)', [size, s_unit, fQuery.FieldByName('files').AsString]));

            fQuery.Next;
          end;

          // release the SQL statement, results and bound parameters before reopen
          fQuery.Close;

          fQuery.SQL.Text := 'SELECT count(*) AS files, ROUND(CAST(SUM(filesize) AS REAL)/1024,1) AS size FROM race WHERE sitedst = :sitedst AND ts > date(:timestring, :period)';
          fQuery.ParamByName('timestring').AsString := 'now';
          fQuery.ParamByName('sitedst').AsString := sitename;
          fQuery.ParamByName('period').AsString := sql_period;
          fQuery.Open;

          while not fQuery.Eof do
          begin
            s_size := StringReplace(fQuery.FieldByName('size').AsString, '.', {$IFNDEF FPC}FormatSettings.DecimalSeparator{$ELSE}DefaultFormatSettings.DecimalSeparator{$ENDIF}, [rfReplaceAll, rfIgnoreCase]);
            size := StrToFloatDef(s_size, 0);

            RecalcSizeValueAndUnit(size, s_unit, 1);
            irc_addtext(netname, channel, Format('TOTAL <b>in</b>:  <c09>%.2f</c> %s (%s files)', [size, s_unit, fQuery.FieldByName('files').AsString]));

            fQuery.Next;
          end;


          if not detailed then
            exit;


          // release the SQL statement, results and bound parameters before reopen
          fQuery.Close;

          fQuery.SQL.Text := 'SELECT DISTINCT sitedst, COUNT(filename) AS files, ROUND(CAST(SUM(filesize) AS REAL)/1024,1) AS size FROM race WHERE sitesrc = :sitesrc AND ts > date(:timestring, :period) GROUP BY sitedst ORDER BY size';
          fQuery.ParamByName('timestring').AsString := 'now';
          fQuery.ParamByName('sitesrc').AsString := sitename;
          fQuery.ParamByName('period').AsString := sql_period;
          fQuery.Open;

          while not fQuery.Eof do
          begin
            s_size := StringReplace(fQuery.FieldByName('size').AsString, '.', {$IFNDEF FPC}FormatSettings.DecimalSeparator{$ELSE}DefaultFormatSettings.DecimalSeparator{$ENDIF}, [rfReplaceAll, rfIgnoreCase]);
            size := StrToFloatDef(s_size, 0);

            RecalcSizeValueAndUnit(size, s_unit, 1);
            irc_addtext(netname, channel, Format('  <b>to</b> %s: %.2f %s (%s files)', [fQuery.FieldByName('sitedst').AsString, size, s_unit, fQuery.FieldByName('files').AsString]));

            fQuery.Next;
          end;

          // release the SQL statement, results and bound parameters before reopen
          fQuery.Close;

          fQuery.SQL.Text := 'SELECT DISTINCT sitesrc, COUNT(filename) AS files, ROUND(CAST(SUM(filesize) AS REAL)/1024,1) AS size FROM race WHERE sitedst = :sitedst AND ts > date(:timestring, :period) GROUP BY sitesrc ORDER BY size';
          fQuery.ParamByName('timestring').AsString := 'now';
          fQuery.ParamByName('sitedst').AsString := sitename;
          fQuery.ParamByName('period').AsString := sql_period;
          fQuery.Open;

          while not fQuery.Eof do
          begin
            s_size := StringReplace(fQuery.FieldByName('size').AsString, '.', {$IFNDEF FPC}FormatSettings.DecimalSeparator{$ELSE}DefaultFormatSettings.DecimalSeparator{$ENDIF}, [rfReplaceAll, rfIgnoreCase]);
            size := StrToFloatDef(s_size, 0);

            RecalcSizeValueAndUnit(size, s_unit, 1);
            irc_addtext(netname, channel, Format('  <b>from</b> %s: %.2f %s (%s files)', [fQuery.FieldByName('sitesrc').AsString, size, s_unit, fQuery.FieldByName('files').AsString]));

            fQuery.Next;
          end;
        end;
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[EXCEPTION] StatRaces: %s', [e.Message]));
          exit;
        end;
      end;
    finally
      fQuery.free;
    end;
  finally
    SQLite3Lock.Leave;
  end;
end;

procedure statsStart;
var
  fDBName: String;
begin
  fDBName := Trim(config.ReadString(section, 'database', 'stats.db'));
  if fDBName = '' then
    exit;

  statsSQLite3DBCon := CreateSQLite3DbConn(fDBName, '');
  SQLite3Lock := TCriticalSection.Create;

  statsSQLite3DBCon.MainSQLite3DB.Execute(
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

  statsSQLite3DBCon.MainSQLite3DB.Execute(
    'CREATE UNIQUE INDEX IF NOT EXISTS hit_index ON hit (sitename, section, filename)'
  );

  statsSQLite3DBCon.MainSQLite3DB.Execute(
    'CREATE INDEX IF NOT EXISTS sitesection_index ON hit (sitename, section)'
  );

  statsSQLite3DBCon.MainSQLite3DB.Execute(
    'CREATE INDEX IF NOT EXISTS group_index ON hit (groupname, section)'
  );


  statsSQLite3DBCon.MainSQLite3DB.Execute(
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

  statsSQLite3DBCon.MainSQLite3DB.Execute(
    'CREATE UNIQUE INDEX IF NOT EXISTS race_index ON race (sitesrc, sitedst, section, rls, filename)'
  );

  statsSQLite3DBCon.MainSQLite3DB.Execute(
    'CREATE INDEX IF NOT EXISTS race_sitesrc ON race (sitesrc)'
  );

  statsSQLite3DBCon.MainSQLite3DB.Execute(
    'CREATE INDEX IF NOT EXISTS race_sitedst ON race (sitedst)'
  );

  statsSQLite3DBCon.MainSQLite3DB.Execute(
    'CREATE INDEX IF NOT EXISTS race_section ON race (section)'
  );

  statsSQLite3DBCon.MainSQLite3DB.Execute(
    'CREATE INDEX IF NOT EXISTS race_rls ON race (rls)'
  );
end;

procedure statsProcessRace(const sitesrc, sitedst, rls_section, rls, filename: String; filesize: Int64);
var
  s_src, s_dst: TSite;
  fQuery: TQuery;
begin
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

  SQLite3Lock.Enter;
  try
    fQuery := TQuery.Create(statsSQLite3DBCon.ThreadSafeConnection);
    try
      fQuery.SQL.Text := 'INSERT OR IGNORE INTO race (sitesrc, sitedst, section, rls, filename, filesize, ts) VALUES (:sitesrc, :sitedst, :section, :rls, :filename, :filesize, :time)';
      fQuery.ParamByName('sitesrc').AsString := uppercase(s_src.name);
      fQuery.ParamByName('sitedst').AsString := uppercase(s_dst.name);
      fQuery.ParamByName('section').AsString := uppercase(rls_section);
      fQuery.ParamByName('rls').AsString := rls;
      fQuery.ParamByName('filename').AsString := filename;
      fQuery.ParamByName('filesize').AsInt64 := filesize;
      fQuery.ParamByName('time').AsString := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
      try
        fQuery.ExecSQL;
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[EXCEPTION] statsProcessRace: %s', [e.Message]));
          exit;
        end;
      end;
    finally
      fQuery.free;
    end;
  finally
    SQLite3Lock.Leave;
  end;
end;

procedure statsProcessDirlist(d: TDirlist; const sitename, rls_section, username: String);
var
  i: Integer;
  de: TDirlistEntry;
  u: String;
  fQuery: TQuery;
begin
  if d = nil then
    exit;
  if d.entries = nil then
    exit;

  fQuery := TQuery.Create(statsSQLite3DBCon.ThreadSafeConnection);
  try
    d.dirlist_lock.Enter;
    try
      for i := 0 to d.entries.Count - 1 do
      begin
        try if i > d.entries.Count then Break; except Break; end;
        try
          de:= TDirlistEntry(d.entries[i]);
          if not de.directory then
          begin
            if ((de.megvanmeg) and (not de.skiplisted) and (de.username <> '') and (de.filesize >= config.ReadInteger(section, 'min_filesize', 100000))) then
            begin
              if de.username <> username then
                u := de.username
              else
                u := '!me!';

              SQLite3Lock.Enter;
              try
                fQuery.SQL.Text := 'INSERT OR IGNORE INTO hit (sitename, section, username, groupname, filename, filesize, ts) VALUES (:sitename, :section, :username, :groupname, :filename, :filesize, :time)';
                fQuery.ParamByName('sitename').AsString := uppercase(sitename);
                fQuery.ParamByName('section').AsString := uppercase(rls_section);
                fQuery.ParamByName('username').AsString := u;
                fQuery.ParamByName('groupname').AsString := de.groupname;
                fQuery.ParamByName('filename').AsString := de.filename;
                fQuery.ParamByName('filesize').AsInt64 := Round(de.filesize / 1024);
                fQuery.ParamByName('time').AsString := FormatDateTime('yyyy-mm-dd hh:nn:ss', de.timestamp);
                try
                  fQuery.ExecSQL;

                  // release the SQL statement, results and bound parameters before reopen
                  fQuery.Close;
                except
                  on e: Exception do
                  begin
                    Debug(dpError, section, Format('[EXCEPTION] statsProcessDirlist: %s', [e.Message]));
                    exit;
                  end;
                end;
              finally
                SQLite3Lock.Leave;
              end;
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
  finally
    fQuery.free;
  end;
end;

procedure statsInit;
begin

end;

procedure statsUninit;
begin
  Debug(dpSpam, section, 'Uninit1');
  if Assigned(SQLite3Lock) then
  begin
    FreeAndNil(SQLite3Lock);
  end;

  if Assigned(statsSQLite3DBCon) then
  begin
    FreeAndNil(statsSQLite3DBCon);
  end;
  Debug(dpSpam, section, 'Uninit2');
end;

end.
