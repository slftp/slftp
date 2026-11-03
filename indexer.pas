unit indexer;

interface

procedure indexerStart;
procedure indexerInit;
procedure indexerUninit;
function indexerQuery(var aRls: String): String;
function indexerQueryPartially(var aRls: String; const aLimit: Integer): String;
function indexerStat: String;
{ Adds releases to the index in a single transaction, so the lock is taken and the database is written
  to disk only once per call instead of once per release. If one release cannot be added, none of them are added
  and the exception is raised again
  @param(aReleases names of the releases)
  @param(aSite name of the site)
  @param(aSection name of the section)
  @param(aPath path on the site in which the releases are located) }
procedure indexerAddReleases(const aReleases: TArray<String>; const aSite, aSection, aPath: String);
procedure indexerRemoveSiteSection(const site, section: String);
function IndexerAlive: boolean;

implementation

uses
  configunit, debugunit, DateUtils, SysUtils, console, slvision, slblowfish,
  StrUtils, Classes, SyncObjs, dbhandler, mormot.db.sql.sqlite3, mormot.db.raw.sqlite3, slcriticalsection2;

const
  section = 'indexer';

var
  indexesSQLite3DBCon: TSQLDBSQLite3ConnectionProperties = nil; //< SQLite3 database connection
  SQLite3Lock: TSlCriticalSection2 = nil; //< Critical Section used for read/write blocking as concurrently does not work flawless

function IndexerAlive: boolean;
begin
  if indexesSQLite3DBCon = nil then
    result := False
  else
    result := True;
end;

procedure indexerAddReleases(const aReleases: TArray<String>; const aSite, aSection, aPath: String);
var
  fConnection: TSqlDBSQLite3Connection;
  fQuery: TSqlDBSQLite3Statement;
  fRls: String;
begin
  if Length(aReleases) = 0 then
    exit;

  SQLite3Lock.Enter('indexerAddReleases');
  try
    fConnection := indexesSQLite3DBCon.ThreadSafeConnection as TSqlDBSQLite3Connection;
    fQuery := TSqlDBSQLite3Statement.Create(fConnection);
    try
      fQuery.Prepare('INSERT INTO rls (rls, sitename, section, path) VALUES (?, ?, ?, ?)');
      try
        // plain SQL instead of StartTransaction/Commit/Rollback of the connection, see SQLite3RollbackIfActive.
        // Inside the try: if a transaction was left open, BEGIN fails and the rollback below closes it
        fConnection.DB.Execute('BEGIN TRANSACTION;');
        for fRls in aReleases do
        begin
          fQuery.BindTextS(1, fRls);
          fQuery.BindTextS(2, aSite);
          fQuery.BindTextS(3, aSection);
          fQuery.BindTextS(4, aPath);
          fQuery.ExecutePrepared;
          fQuery.Reset;
        end;
        fConnection.DB.Execute('COMMIT TRANSACTION;');
      except
        // one failed release drops the releases of the whole call: acceptable, as each autoindex run clears the
        // section and indexes it again from scratch
        try
          SQLite3RollbackIfActive(fConnection.DB);
        except
          // only log it, the caller needs the original exception
          on e: Exception do
            Debug(dpError, section, Format('[EXCEPTION] indexerAddReleases rollback: %s', [e.Message]));
        end;
        raise;
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
  SQLite3Lock.Enter('indexerRemoveSiteSection');
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
  // in WAL mode a commit then no longer waits for the disk; a power loss can only drop the last commits
  indexesSQLite3DBCon.MainSQLite3DB.Synchronous := smNormal;

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
  SQLite3Lock := TSlCriticalSection2.Create('Indexer');
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
    // Checkpoint WAL to merge changes back into main database and truncate WAL file
    indexesSQLite3DBCon.MainSQLite3DB.Execute('PRAGMA wal_checkpoint(TRUNCATE)');
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

  SQLite3Lock.Enter('indexerStat');
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

  SQLite3Lock.Enter('indexerQuery');
  try
    fQuery := TSqlDBSQLite3Statement.Create(indexesSQLite3DBCon.ThreadSafeConnection);
    try
      fQuery.Prepare('SELECT sitename, section, path, rls FROM rls WHERE rls = ?');
      fQuery.BindTextS(1, aRls);
      try
        fQuery.ExecutePrepared;
        while fQuery.Step do
        begin
          Result := Result + fQuery.ColumnString('sitename') + '-' + fQuery.ColumnString('section') + '=' + fQuery.ColumnString('path') + #13#10;
          aRls := fQuery.ColumnString('rls');
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

  SQLite3Lock.Enter('indexerQueryPartially');
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
          Result := Result + fQuery.ColumnString('sitename') + '-' + fQuery.ColumnString('section') + '=' + fQuery.ColumnString('path') + '/' + fQuery.ColumnString('rls') + #13#10;
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

