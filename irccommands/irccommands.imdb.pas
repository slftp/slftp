unit irccommands.imdb;

interface

{ slftp imdb commands functions }
function IrcAnnounceIMDBInfo(const netname, channel, params: String): boolean;

implementation

uses
  SysUtils, Classes, dbaddimdb, irc;

const
  section = 'irccommands.imdb';

function IrcAnnounceIMDBInfo(const netname, channel, params: String): boolean;
var

  i: integer;
  imdbdata: TDbImdbData;
begin
  Result := False;

  dbaddimdb_cs.Enter;
  try
    i := last_imdbdata.IndexOf(params);
  finally
    dbaddimdb_cs.Leave;
  end;

  if i = -1 then
  begin
    irc_addtext(Netname, Channel, Format('<c4><b>ERROR</c></b>: %s not found in database!', [params]));
    Result := True;
    exit;
  end
  else
  begin
    dbaddimdb_cs.Enter;
    try
      imdbdata := TDbImdbData(last_imdbdata.Objects[i]);
    finally
      dbaddimdb_cs.Leave;
    end;

    imdbdata.PostResults(Netname, Channel, params);
  end;
  Result := True;
end;

end.