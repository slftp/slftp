unit irccommands.imdb;

interface

{ slftp imdb commands functions }
function IrcAnnounceIMDBInfo(const netname, channel, params: String): boolean;
function IrcDeleteIMDBInfo(const netname, channel, params: String): boolean;

implementation

uses
  SysUtils, Classes, Contnrs, SyncObjs, dbaddimdb, irc;

const
  section = 'irccommands.imdb';

function IrcAnnounceIMDBInfo(const netname, channel, params: String): boolean;
var

  i: integer;
  imdbdata: TDbImdbData;
begin
  Result := foundMovieAlreadyInDbWithReleaseName(params);
  if Result then
  begin
    imdbdata := GetImdbMovieData(params);
    imdbdata.PostResults(params);
  end
  else
  begin
    irc_addtext(Netname, Channel, Format('<c4><b>ERROR</c></b>: %s not found in database!', [params]));
    result := True;
    exit;
  end;
end;

function IrcDeleteIMDBInfo(const netname, channel, params: String): boolean;
begin
  Result := DeleteIMDbDataWithImdbId(params);
  if Result then
  begin
    irc_addtext(Netname, Channel, Format('<c4><b>INFO</c></b>: %s has been deleted from database!', [params]));
  end
  else
  begin
    irc_addtext(Netname, Channel, Format('<c4><b>ERROR</c></b>: %s not found in database!', [params]));
    result := True;
    exit;
  end;
end;

end.