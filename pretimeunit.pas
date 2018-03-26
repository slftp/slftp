unit pretimeunit;

interface

uses slsqlite;

procedure DupeDBInit;
procedure DupeDBUninit;

procedure dupedbBeginTransaction();
procedure dupedbEndTransaction();

procedure Addrlstodupedb(rls,section,event:String;pretime:longint;size:integer);

procedure ReadDupeDB(const rls: String; out pretime: integer; out size: integer);

function dupeDBQuery(const q: String): String;

function AllreadyInDataBase(rls:String):boolean;

var dupedb: TslSqliteDB;
    dupeInsert: Psqlite3_stmt;

implementation

uses DateUtils, debugunit, configunit, sitesunit, queueunit, dirlist, SysUtils,
     pazo, kb, ranksunit, regexpr, mystrings, irc;


function AllreadyInDataBase(rls:String):boolean;
var q:String;
begin
q:='SELECT COUNT(rlsname) as count FROM dupes WHERE rlsname = '+Chr(39)+rls+Chr(39)+';';
q:= dupeDBQuery(q);
if q = '0' then result:=True else Result:=False;

end;

procedure ReadDupeDB(const rls: String; out pretime: integer; out size: integer);
var
 q: String;
 rx: Tregexpr;
begin
//  irc_addtext('','','TRY TO READ PRETIME');
  q := 'SELECT ctime, size AS s '+#13#10;
  q := q+ 'FROM dupes ';
  q := q+ 'WHERE rlsname = '+Chr(39)+rls+Chr(39)+#13#10;

  q := dupeDBQuery(q);
//  irc_addtext('','','RESULT: '+q);

  pretime := -1;
  size := -1;

  rx:=Tregexpr.Create;
  try
  rx.Expression:='([\d]+)\;([\d]+)';
  if rx.Exec(q) then
  begin
    pretime := strtoint(rx.Match[1]);
    size := strtoint(rx.Match[2]);
  end;
  finally
    rx.free;
  end;
end;

procedure Addrlstodupedb(rls,section,event:String;pretime:longint;size:integer);
begin
//  irc_addtext('','','ADD PRETIME');
  if dupedb = nil then begin
  DupeDBInit;
    irc_addtext('','','DUPEDB INITED!');
  end;
  if dupeInsert = nil then exit;

  dupedb.ExecSQL( dupeInsert,[rls,uppercase(section),pretime,size,uppercase(event)]);
end;

function dupeDBQuery(const q: String): String;
var
  s: Psqlite3_stmt;
begin
 s:= dupedb.Open(q);
 Result:= '';
 while dupedb.Step(s) do
 Result:= Result + Format('%s;%s)', [dupedb.column_text(s, 0), dupedb.column_text(s, 1)])+#13#10;
end;

procedure DupeDBInit;
var
  s: String;
begin
  s:= Trim('dupe.db');
  dupedb:= TslSqliteDB.Create(s,'');

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
' CREATE TABLE IF NOT EXISTS dupes ('+
' id  INTEGER PRIMARY KEY AUTOINCREMENT, '+
      ' rlsname VARCHAR(250) NOT NULL, '+
      ' section VARCHAR(150) NOT NULL, '+
      ' ctime INT UNSIGNED NOT NULL, '+
      ' size INT UNSIGNED , '+
      ' event VARCHAR(55) NOT NULL '+
' )'
);
*)
dupeInsert:= dupedb.Open('INSERT INTO dupes (rlsname, section, ctime, size, event) VALUES (?, ?, ?, ?, ?)');



end;

procedure DupeDBUninit;
begin
  Debug(dpSpam, 'pretimeunit', 'Uninit1');
  if dupedb <> nil then
  begin

    dupedb.Free;
    dupedb:= nil;
  end;

  Debug(dpSpam, 'pretimeunit', 'Uninit2');
end;


procedure dupedbBeginTransaction();
begin
  if dupedb = nil then exit;

  dupedb.ExecSQL('BEGIN TRANSACTION');
end;
procedure dupedbEndTransaction();
begin
  if dupedb = nil then exit;

  dupedb.ExecSQL('COMMIT TRANSACTION');
end;

end.
