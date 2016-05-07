unit indexer;

interface

procedure indexerStart;
procedure indexerInit;
procedure indexerUninit;
procedure indexerBeginTransaction();
procedure indexerEndTransaction();
function indexerQuery(var rls: string): string;
function indexerQueryPartially(var rls: string): string;
function indexerStat: string;
procedure indexerAddRelease(const rls, site, section, path: string);
procedure indexerRemoveSiteSection(const site, section: string);
function indexerCapable: Boolean;

function IndexerAlive: boolean;

implementation

uses configunit, debugunit, DateUtils, SysUtils, console, slvision, slblowfish,
  mystrings, slsqlite, Classes, SyncObjs;

const
  section = 'indexer';

var
  indexes: TslSqliteDB = nil;
  indexerCount: Psqlite3_stmt = nil;
  indexerDelete: Psqlite3_stmt = nil;
  indexerInsert: Psqlite3_stmt = nil;
  indexerSelect: Psqlite3_stmt = nil;
  indexerSelectPartially: Psqlite3_stmt = nil;
  indexer_lock: TCriticalSection;

function IndexerAlive: boolean;
begin
  if indexes = nil then
    result := False
  else
    result := True;
end;

procedure indexerBeginTransaction();
begin
  if indexes = nil then
    exit;

  indexes.ExecSQL('BEGIN TRANSACTION');
end;

procedure indexerEndTransaction();
begin
  if indexes = nil then
    exit;

  indexes.ExecSQL('COMMIT TRANSACTION');
end;

function indexerCapable: Boolean;
begin
  Result := indexes <> nil;
end;

procedure indexerAddRelease(const rls, site, section, path: string);
begin
  if indexes = nil then
    exit;
  if indexerInsert = nil then
    exit;

  indexer_lock.Enter;
  try
    indexes.ExecSQL(indexerInsert, [rls, site, section, path]);
  finally
    indexer_lock.Leave;
  end;
end;

procedure indexerRemoveSiteSection(const site, section: string);
begin
  if indexes = nil then
    exit;
  if indexerDelete = nil then
    exit;
  indexer_lock.Enter;
  try
    if section = '' then
      indexes.ExecSQL('DELETE FROM rls WHERE sitename=' + site)
    else
      indexes.ExecSQL(indexerDelete, [site, section]);
  finally
    indexer_lock.Leave;
  end;
end;

procedure indexerStart;
var
  s: string;
begin
  if slsqlite_inited then
  begin
    s := Trim(config.ReadString(section, 'database', ''));
    if s = '' then
      exit;

    indexes := TslSqliteDB.Create(s, config.ReadString(section, 'pragma', ''));
    indexes.ExecSQL(
      'CREATE TABLE IF NOT EXISTS rls (' +
      ' rls VARCHAR(200) NOT NULL, ' +
      ' sitename VARCHAR(20) NOT NULL, ' +
      ' section VARCHAR(40) NOT NULL, ' +
      ' path VARCHAR(200) NOT NULL ' +
      ')'
      );

    indexes.ExecSQL(
      'CREATE INDEX IF NOT EXISTS rls_index ON rls (rls)'
      );
    indexes.ExecSQL(
      'CREATE INDEX IF NOT EXISTS sitenamesection_index ON rls (sitename,section)'
      );

    indexerCount := indexes.Open('SELECT sitename, section, COUNT(*) FROM rls GROUP BY sitename, section ORDER BY sitename, section');
    indexerSelect := indexes.Open('SELECT sitename,section,path,rls FROM rls WHERE rls=?');
    indexerSelectPartially := indexes.Open('SELECT sitename,section,path, rls FROM rls WHERE rls LIKE ? ORDER BY rls LIMIT 20');
    indexerInsert := indexes.Open('INSERT INTO rls (rls, sitename, section, path) VALUES (?, ?, ?, ?)');
    indexerDelete := indexes.Open('DELETE FROM rls WHERE sitename=? AND section=?');

  end;
end;

function indexerStat: string;
var
  all, db: Integer;
begin
  Result := '';
  if indexes = nil then
    exit;
  if indexerCount = nil then
    exit;

  indexer_lock.Enter;
  try
    all := 0;
    indexes.Open(indexerCount);
    while indexes.Step(indexerCount) do
    begin
      db := indexes.column_int(indexerCount, 2);
      Result := Result + Format('%s-%s=%d', [indexes.column_text(indexerCount, 0), indexes.column_text(indexerCount, 1), db]) + #13#10;
      inc(all, db);
    end;
    Result := Result + 'Total: ' + IntToStr(all) + ' releases' + #13#10;
  finally
    indexer_lock.Leave;
  end;
end;

procedure indexerInit;
begin
  indexer_lock := TCriticalSection.Create;
end;

procedure indexerUninit;
begin
  Debug(dpSpam, section, 'Uninit1');
  if indexes <> nil then
  begin
    if indexerCount <> nil then
      indexes.Close(indexerCount);
    if indexerSelect <> nil then
      indexes.Close(indexerSelect);
    if indexerDelete <> nil then
      indexes.Close(indexerDelete);
    if indexerInsert <> nil then
      indexes.Close(indexerInsert);
    indexes.Free;
    indexes := nil;
  end;
  indexer_lock.Free;
  Debug(dpSpam, section, 'Uninit2');
end;

function indexerQuery(var rls: string): string;
begin
  Result := '';
  if indexes = nil then
    exit;
  if indexerSelect = nil then
    exit;

  indexer_lock.Enter;
  try
    indexes.open(indexerSelect, [rls]);
    while indexes.Step(indexerSelect) do
    begin
      Result := Result + indexes.column_text(indexerSelect, 0) + '-' + indexes.column_text(indexerSelect, 1) + '=' + indexes.column_text(indexerSelect, 2) + #13#10;
      rls := indexes.column_text(indexerSelect, 3);
    end;
  finally
    indexer_lock.Leave;
  end;
end;

function indexerQueryPartially(var rls: string): string;
begin
  Result := '';
  if indexes = nil then
    exit;
  if indexerSelect = nil then
    exit;

  rls := Csere(rls, '_', '%');
  rls := Csere(rls, ' ', '%');
  rls := '%' + rls + '%';

  indexer_lock.Enter;
  try
    indexes.open(indexerSelectPartially, [rls]);
    while indexes.Step(indexerSelectPartially) do
      Result := Result + indexes.column_text(indexerSelectPartially, 0) + '-' + indexes.column_text(indexerSelectPartially, 1) + '=' + indexes.column_text(indexerSelectPartially,
        2) + ' => ' + indexes.column_text(indexerSelectPartially, 3) + #13#10;
  finally
    indexer_lock.Leave;
  end;
end;

end.

