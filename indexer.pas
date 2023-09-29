unit indexer;

interface

procedure indexerStart;
procedure indexerInit;
procedure indexerUninit;
function indexerQuery(var aRls: String): String;
function indexerQueryPartially(var aRls: String; const aLimit: Integer): String;
function indexerStat: String;
procedure indexerAddRelease(const rls, site, section, path: String);
procedure indexerRemoveSiteSection(const site, section: String);
function IndexerAlive: boolean;

implementation

uses
  configunit, debugunit, DateUtils, SysUtils, console, slvision, slblowfish,
  StrUtils, Classes, SyncObjs, dbhandler, mormot.db.sql.sqlite3;

const
  section = 'indexer';

var
  indexesSQLite3DBCon: TSQLDBSQLite3ConnectionProperties = nil; //< SQLite3 database connection
  SQLite3Lock: TCriticalSection = nil; //< Critical Section used for read/write blocking as concurrently does not work flawless

function IndexerAlive: boolean;
begin
  if indexesSQLite3DBCon = nil then
    result := False
  else
    result := True;
end;

procedure indexerAddRelease(const rls, site, section, path: String);
var
  fQuery: TSqlDBSQLite3Statement;
begin
  SQLite3Lock.Enter;
  try
    fQuery := TSqlDBSQLite3Statement.Create(indexesSQLite3DBCon.ThreadSafeConnection);
    try
      fQuery.Prepare('INSERT INTO rls (rls, sitename, section, path) VALUES (?, ?, ?, ?)');
      fQuery.BindTextS(1, rls);
      fQuery.BindTextS(2, site);
      fQuery.BindTextS(3, section);
      fQuery.BindTextS(4, path);
      try
        fQuery.ExecutePrepared;
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[EXCEPTION] indexerAddRelease: %s', [e.Message]));
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

procedure indexerRemoveSiteSection(const site, section: String);
var
  fQuery: TSqlDBSQLite3Statement;
begin
  SQLite3Lock.Enter;
  try
    fQuery := TSqlDBSQLite3Statement.Create(indexesSQLite3DBCon.ThreadSafeConnection);
    try
      if section = '' then
      begin
        fQuery.Prepare('DELETE FROM rls WHERE sitename = ?');
        fQuery.BindTextS(1, site);
      end
      else
      begin
        fQuery.Prepare('DELETE FROM rls WHERE sitename = ? AND section = ?');
        fQuery.BindTextS(1, site);
        fQuery.BindTextS(2, section);
      end;
      try
        fQuery.ExecutePrepared;
      except
        on e: Exception do
        begin
          if section = '' then
            Debug(dpError, section, Format('[EXCEPTION] indexerRemoveSiteSection with empty section: %s', [e.Message]))
          else
            Debug(dpError, section, Format('[EXCEPTION] indexerRemoveSiteSection with section: %s', [e.Message]));
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

procedure indexerStart;
var
  db_name: String;
begin
  db_name := Trim(config.ReadString(section, 'database', 'indexes.db'));

  indexesSQLite3DBCon := CreateSQLite3DbConn(db_name, '');

  indexesSQLite3DBCon.MainSQLite3DB.Execute(
    'CREATE TABLE IF NOT EXISTS rls (' +
    ' rls VARCHAR(200) NOT NULL, ' +
    ' sitename VARCHAR(20) NOT NULL, ' +
    ' section VARCHAR(40) NOT NULL, ' +
    ' path VARCHAR(200) NOT NULL ' +
    ')'
  );

  indexesSQLite3DBCon.MainSQLite3DB.Execute(
    'CREATE INDEX IF NOT EXISTS rls_index ON rls (rls)'
  );
  indexesSQLite3DBCon.MainSQLite3DB.Execute(
    'CREATE INDEX IF NOT EXISTS sitenamesection_index ON rls (sitename, section)'
  );
end;

procedure indexerInit;
begin
  SQLite3Lock := TCriticalSection.Create;
end;

procedure indexerUninit;
begin
  Debug(dpSpam, section, 'Uninit1');
  if Assigned(SQLite3Lock) then
  begin
    FreeAndNil(SQLite3Lock);
  end;

  if Assigned(indexesSQLite3DBCon) then
  begin
    FreeAndNil(indexesSQLite3DBCon);
  end;
  Debug(dpSpam, section, 'Uninit2');
end;

function indexerStat: String;
var
  fQuery: TSqlDBSQLite3Statement;
  fAll, fCount: Integer;
begin
  Result := '';
  fAll := 0;

  SQLite3Lock.Enter;
  try
    fQuery := TSqlDBSQLite3Statement.Create(indexesSQLite3DBCon.ThreadSafeConnection);
    try
      fQuery.Prepare('SELECT sitename, section, COUNT(*) FROM rls GROUP BY sitename, section ORDER BY sitename, section');
      try
        fQuery.ExecutePrepared;
        while fQuery.Step do
        begin
          fCount := fQuery.ColumnInt(2);
          Result := Result + Format('%s-%s=%d', [fQuery.ColumnUtf8('sitename'), fQuery.ColumnUtf8('section'), fCount]) + #13#10;
          Inc(fAll, fCount);
        end;
        Result := Result + 'Total: ' + IntToStr(fAll) + ' releases';
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[EXCEPTION] indexerStat: %s', [e.Message]));
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

function indexerQuery(var aRls: String): String;
var
  fQuery: TSqlDBSQLite3Statement;
begin
  Result := '';

  SQLite3Lock.Enter;
  try
    fQuery := TSqlDBSQLite3Statement.Create(indexesSQLite3DBCon.ThreadSafeConnection);
    try
      fQuery.Prepare('SELECT sitename, section, path, rls FROM rls WHERE rls = ?');
      fQuery.BindTextS(1, aRls);
      try
        fQuery.ExecutePrepared;
        while fQuery.Step do
        begin
          Result := Result + fQuery.ColumnUtf8('sitename') + '-' + fQuery.ColumnUtf8('section') + '=' + fQuery.ColumnUtf8('path') + #13#10;
          aRls := fQuery.ColumnUtf8('rls');
        end;
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[EXCEPTION] indexerQuery: %s', [e.Message]));
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

function indexerQueryPartially(var aRls: String; const aLimit: Integer): String;
var
  fQuery: TSqlDBSQLite3Statement;
begin
  Result := '';

  aRls := ReplaceText(aRls, '_', '%');
  aRls := ReplaceText(aRls, ' ', '%');
  aRls := '%' + aRls + '%';

  SQLite3Lock.Enter;
  try
    fQuery := TSqlDBSQLite3Statement.Create(indexesSQLite3DBCon.ThreadSafeConnection);
    try
      fQuery.Prepare('SELECT sitename, section, path, rls FROM rls WHERE rls LIKE ? ORDER BY rls LIMIT ?');
      fQuery.BindTextS(1, aRls);
      fQuery.Bind(2, aLimit);
      try
        fQuery.ExecutePrepared;
        while fQuery.Step do
        begin
          Result := Result + fQuery.ColumnUtf8('sitename') + '-' + fQuery.ColumnUtf8('section') + '=' + fQuery.ColumnUtf8('path') + '/' + fQuery.ColumnUtf8('rls') + #13#10;
        end;
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[EXCEPTION] indexerQueryPartially: %s', [e.Message]));
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

end.

