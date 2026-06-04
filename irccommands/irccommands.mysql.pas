// TODO: Maybe add proper code, so mysql data is saved encrypted
unit irccommands.mysql;

interface

{ slftp mysql commands functions }
function IrcSetMYSQLData(const netname, channel: string;params: string): Boolean;
function IrcViewMYSQLValue(const netname, channel: string;params: string): Boolean;
function IrcTweakMYSQL(const netname, channel: string;params: string): Boolean;
function IrcMYSQLStatus(const netname, channel: string;params: string): Boolean;

implementation

uses
  SysUtils, Classes, sitesunit, mystrings;

const
  section = 'irccommands.mysql';

function IrcSetMYSQLData(const netname, channel, params: String): boolean;
var
  fhostport, fhost, fport, fuser, fpassw, fdbname, ftable: String;
begin
  Result := False;

  fhostport := SubString(params, ' ', 1);
  fuser := SubString(params, ' ', 2);
  fpassw := SubString(params, ' ', 3);
  fdbname := SubString(params, ' ', 4);
  ftable := SubString(params, ' ', 5);
  fhost := SubString(fhostport, ':', 1);
  fport := SubString(fhostport, ':', 2);

  try
    sitesdat.WriteString('MYSQL', 'Host', fhost);
    sitesdat.WriteString('MYSQL', 'Port', fport);
    sitesdat.WriteString('MYSQL', 'Username', fuser);
    sitesdat.WriteString('MYSQL', 'Password', fpassw);
    sitesdat.WriteString('MYSQL', 'dbname', fdbname);
    sitesdat.WriteString('MYSQL', 'tablename', ftable);
  finally
    sitesdat.UpdateFile;
  end;

  Result := True;
end;

function IrcViewMYSQLValue(const netname, channel, params: String): boolean;
begin
  Result := True;
end;

function IrcTweakMYSQL(const netname, channel, params: String): boolean;
begin
  Result := True;
end;

function IrcMYSQLStatus(const netname, channel, params: String): boolean;
begin
  Result := False;
end;

end.