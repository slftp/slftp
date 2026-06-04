unit irccommands.imdb;

interface

{ slftp imdb commands functions }
function IrcAnnounceIMDBInfo(const netname, channel, params: String): boolean;

implementation

uses
  SysUtils, Classes, Contnrs, SyncObjs, dbaddimdb, irc, debugunit;

const
  section = 'irccommands.imdb';

function IrcAnnounceIMDBInfo(const netname, channel, params: String): boolean;
var
  imdbdata: TDbImdbData;
begin
  Debug(dpError, section, Format('[IrcAnnounceIMDBInfo] === START === netname=%s, channel=%s, params=%s', [netname, channel, params]));
  
  Result := foundMovieAlreadyInDbWithReleaseName(params);
  Debug(dpError, section, Format('[IrcAnnounceIMDBInfo] foundMovieAlreadyInDbWithReleaseName result: %s', [BoolToStr(Result, True)]));
  
  if Result then
  begin
    Debug(dpError, section, '[IrcAnnounceIMDBInfo] Movie found, calling GetImdbMovieData');
    imdbdata := GetImdbMovieData(params);
    if imdbdata <> nil then
    begin
      Debug(dpError, section, Format('[IrcAnnounceIMDBInfo] GetImdbMovieData returned data for: %s', [imdbdata.imdb_id]));
      try
        Debug(dpError, section, '[IrcAnnounceIMDBInfo] About to call PostResults with IRC output');
        imdbdata.PostResults(params);
        Debug(dpError, section, '[IrcAnnounceIMDBInfo] PostResults completed successfully');
      finally
        imdbdata.Free;
        Debug(dpError, section, '[IrcAnnounceIMDBInfo] imdbdata freed');
      end;
    end
    else
    begin
      Debug(dpError, section, '[IrcAnnounceIMDBInfo] GetImdbMovieData returned nil!');
      irc_addtext(Netname, Channel, Format('<c4><b>ERROR</c></b>: %s not found in database!', [params]));
      Result := True;
    end;
  end
  else
  begin
    Debug(dpError, section, Format('[IrcAnnounceIMDBInfo] Movie not found: %s', [params]));
    irc_addtext(Netname, Channel, Format('<c4><b>ERROR</c></b>: %s not found in database!', [params]));
    Result := True;
  end;
  
  Debug(dpError, section, Format('[IrcAnnounceIMDBInfo] === END === Result: %s', [BoolToStr(Result, True)]));
end;

end.