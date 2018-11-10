unit indexer;

interface

procedure indexerStart;
procedure indexerInit;
procedure indexerUninit;
function indexerQuery(var aRls: String): String;
function indexerQueryPartially(var aRls: String): String;
function indexerStat: String;
procedure indexerAddRelease(const rls, site, section, path: String);
procedure indexerRemoveSiteSection(const site, section: String);
function IndexerAlive: boolean;

implementation

uses
  configunit, debugunit, DateUtils, SysUtils, console, slvision, slblowfish,
  mystrings, Classes, SyncObjs, dbhandler, SynDBSQLite3, SynDB;

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
  fQuery: TQuery;
begin
  SQLite3Lock.Enter;
  try
    fQuery := TQuery.Create(indexesSQLite3DBCon.ThreadSafeConnection);
    try
      fQuery.SQL.Text := 'INSERT INTO rls (rls, sitename, section, path) VALUES (:release, :sitename, :section, :path)';
      fQuery.ParamByName('release').AsString := rls;
      fQuery.ParamByName('sitename').AsString := site;
      fQuery.ParamByName('section').AsString := section;
      fQuery.ParamByName('path').AsString := path;
      try
        fQuery.ExecSQL;
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
  fQuery: TQuery;
begin
  SQLite3Lock.Enter;
  try
    fQuery := TQuery.Create(indexesSQLite3DBCon.ThreadSafeConnection);
    try
      if section = '' then
      begin
        fQuery.SQL.Text := 'DELETE FROM rls WHERE sitename = :sitename';
        fQuery.ParamByName('sitename').AsString := site;
      end
      else
      begin
        fQuery.SQL.Text := 'DELETE FROM rls WHERE sitename = :sitename AND section = :section';
        fQuery.ParamByName('sitename').AsString := site;
        fQuery.ParamByName('section').AsString := section;
      end;
      try
        fQuery.ExecSQL;
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
  fQuery: TQuery;
  fAll, fCount: Integer;
begin
  Result := '';
  fAll := 0;

  SQLite3Lock.Enter;
  try
    fQuery := TQuery.Create(indexesSQLite3DBCon.ThreadSafeConnection);
    try
      fQuery.SQL.Text := 'SELECT sitename, section, COUNT(*) FROM rls GROUP BY sitename, section ORDER BY sitename, section';
      try
        fQuery.Open;

        if not fQuery.IsEmpty then
        begin
          fQuery.First;

          while not fQuery.Eof do
          begin
            fCount := fQuery.Fields[2].AsInteger;
            Result := Result + Format('%s-%s=%d', [fQuery.FieldByName('sitename').AsString, fQuery.FieldByName('section').AsString, fCount]) + #13#10;
            Inc(fAll, fCount);
            fQuery.Next;
          end;
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
  fQuery: TQuery;
begin
  Result := '';

  SQLite3Lock.Enter;
  try
    fQuery := TQuery.Create(indexesSQLite3DBCon.ThreadSafeConnection);
    try
      fQuery.SQL.Text := 'SELECT sitename, section, path, rls FROM rls WHERE rls = :release';
      fQuery.ParamByName('release').AsString := aRls;
      try
        fQuery.Open;

        if not fQuery.IsEmpty then
        begin
          fQuery.First;

          while not fQuery.Eof do
          begin
            Result := Result + fQuery.FieldByName('sitename').AsString + '-' + fQuery.FieldByName('section').AsString + '=' + fQuery.FieldByName('path').AsString + #13#10;
            aRls := fQuery.FieldByName('rls').AsString;
            fQuery.Next;
          end;
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

function indexerQueryPartially(var aRls: String): String;
var
  fQuery: TQuery;
begin
  Result := '';

  aRls := Csere(aRls, '_', '%');
  aRls := Csere(aRls, ' ', '%');
  aRls := '%' + aRls + '%';

  SQLite3Lock.Enter;
  try
    fQuery := TQuery.Create(indexesSQLite3DBCon.ThreadSafeConnection);
    try
      fQuery.SQL.Text := 'SELECT sitename, section, path, rls FROM rls WHERE rls LIKE :name ORDER BY rls LIMIT 20';
      fQuery.ParamByName('name').AsString := aRls;
      try
        fQuery.Open;

        if not fQuery.IsEmpty then
        begin
          fQuery.First;

          while not fQuery.Eof do
          begin
            Result := Result + fQuery.FieldByName('sitename').AsString + '-' + fQuery.FieldByName('section').AsString + '=' + fQuery.FieldByName('path').AsString + '/' + fQuery.FieldByName('rls').AsString + #13#10;
            fQuery.Next;
          end;
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

