// DEAD UNIT?

unit pretimeunit;

interface

uses
  slsqlite;

procedure DupeDBInit;
procedure DupeDBUninit;

function AllreadyInDataBase(const rls: String): boolean;
procedure ReadDupeDB(const rls: String; out pretime: integer; out size: integer);
procedure Addrlstodupedb(const rls, section, event: String; const pretime: longint; const size: integer);
function dupeDBQuery(const q: String): String;

implementation

uses
  DateUtils, debugunit, configunit, sitesunit, queueunit, dirlist, SysUtils,
  pazo, kb, ranksunit, regexpr, mystrings, irc;

const
  section = 'pretimeunit';

var
  dupedb: TslSqliteDB;
  dupeInsert: Psqlite3_stmt;

function AllreadyInDataBase(const rls: String): boolean;
var
  q: String;
begin
  q := 'SELECT COUNT(rlsname) as count FROM dupes WHERE rlsname = '+Chr(39)+rls+Chr(39)+';';
  q := dupeDBQuery(q);

  if q = '0' then
    result := True
  else
    Result := False;
end;

procedure ReadDupeDB(const rls: String; out pretime: integer; out size: integer);
var
 q: String;
 rx: Tregexpr;
begin
  q := 'SELECT ctime, size AS s '+#13#10;
  q := q + 'FROM dupes ';
  q := q + 'WHERE rlsname = '+Chr(39)+rls+Chr(39)+#13#10;

  q := dupeDBQuery(q);

  pretime := -1;
  size := -1;

  rx := Tregexpr.Create;
  try
    rx.Expression := '([\d]+)\;([\d]+)';
    if rx.Exec(q) then
    begin
      pretime := StrToInt(rx.Match[1]);
      size := StrToInt(rx.Match[2]);
    end;
  finally
    rx.free;
  end;
end;

procedure Addrlstodupedb(const rls, section, event: String; const pretime: longint; const size: integer);
begin
  if dupedb = nil then
  begin
    DupeDBInit;
    irc_addtext('', '', 'DUPEDB INITED!');
  end;

  if dupeInsert = nil then
    exit;

  dupedb.ExecSQL(dupeInsert, [rls, uppercase(section), pretime, size, uppercase(event)]);
end;

function dupeDBQuery(const q: String): String;
var
  s: Psqlite3_stmt;
begin
 s := dupedb.Open(q);
 Result := '';
 while dupedb.Step(s) do
  Result := Result + Format('%s;%s)', [dupedb.column_text(s, 0), dupedb.column_text(s, 1)]) + #13#10;
end;

procedure DupeDBInit;
var
  s: String;
begin
  s := Trim('dupe.db');
  dupedb := TslSqliteDB.Create(s, '');

  dupedb.ExecSQL(
    'CREATE TABLE IF NOT EXISTS dupes ('+
    ' rlsname VARCHAR(250) NOT NULL, '+
    ' section VARCHAR(150) NOT NULL, '+
    ' ctime INT UNSIGNED NOT NULL, '+
    ' size INT UNSIGNED , '+
    ' event VARCHAR(55) NOT NULL '+
    ')'
  );

(*
  dupedb.ExecSQL(
    'CREATE TABLE IF NOT EXISTS dupes ('+
    ' id  INTEGER PRIMARY KEY AUTOINCREMENT, '+
    ' rlsname VARCHAR(250) NOT NULL, '+
    ' section VARCHAR(150) NOT NULL, '+
    ' ctime INT UNSIGNED NOT NULL, '+
    ' size INT UNSIGNED , '+
    ' event VARCHAR(55) NOT NULL '+
    ')'
  );
*)

  dupeInsert := dupedb.Open('INSERT INTO dupes (rlsname, section, ctime, size, event) VALUES (?, ?, ?, ?, ?)');
end;

procedure DupeDBUninit;
begin
  Debug(dpSpam, section, 'Uninit1');
  if dupedb <> nil then
  begin
    FreeAndNil(dupedb);
  end;
  Debug(dpSpam, section, 'Uninit2');
end;

end.
