unit irccommands.news;

interface

{ slftp news commands functions }
function IrcNews(const netname, channel, params: String): boolean;
function IrcNewsAdd(const netname, channel, params: String): boolean;
function IrcNewsDel(const netname, channel, params: String): boolean;
function IrcNewsCategories(const netname, channel, params: String): boolean;

implementation

uses
  SysUtils, Classes, news, irc, mystrings;

const
  section = 'irccommands.news';

function IrcNews(const netname, channel, params: String): boolean;
var
  i: integer;
  category: String;
begin
  i := StrToIntDef(SubString(params, ' ', 1), 10);
  category := UpperCase(SubString(params, ' ', 2));
  Result := SlftpNewsShow(Netname, Channel, i, category);
end;

function IrcNewsAdd(const netname, channel, params: String): boolean;
var
  category: String;
begin
  category := UpperCase(SubString(params, ' ', 1));

  Result := SlftpNewsAdd(Netname, Channel, category, mystrings.RightStr(params, Length(category) + 1));
end;

function IrcNewsDel(const netname, channel, params: String): boolean;
var
  DeleteNumber: integer;
  input, announce: String;
  AnnounceIt: boolean;
begin
  AnnounceIt := False;
  input := SubString(params, ' ', 1);
  announce := SubString(params, ' ', 2);

  // check if first char of first input is alphabetical
  if input[1] in ['A'..'Z', 'a'..'z'] then
  begin
    if (announce = '-s') or (announce = '-show') then
      AnnounceIt := True;

    Result := SlftpNewsDelete(Netname, Channel, input, AnnounceIt);
  end
  else
  begin
    if input = '*' then
      DeleteNumber := -1
    else
      DeleteNumber := StrToIntDef(input, 0);

    Result := SlftpNewsDelete(Netname, Channel, DeleteNumber);
  end;
end;

function IrcNewsCategories(const netname, channel, params: String): boolean;
begin
  Result := False;
  irc_addtext(Netname, Channel, 'Valid categories are: %s', [ValidCategoriesAsString]);
  Result := True;
end;

end.