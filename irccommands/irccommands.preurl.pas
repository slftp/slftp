// TODO: test and maybe refactor this code
unit irccommands.preurl;

interface

{ slftp preurl commands functions }
function IrcPreURLList(const netname, channel, params: String): boolean;
function IrcPreURLAdd(const netname, channel, params: String): boolean;
function IrcPreURLDel(const netname, channel, params: String): boolean;
function IrcPreURLMod(const netname, channel, params: String): boolean;
function IrcTestOffset(const netname, channel, params: String): boolean;

implementation

uses
  SysUtils, Classes, DateUtils, irc, mrdohutils, mystrings, http, RegExpr;

const
  section = 'irccommands.preurl';

function IrcPreURLList(const netname, channel, params: String): boolean;
var
  url, offset: String;
  i: integer;
begin
  Result := False;

  for i := 0 to preurls.Count - 1 do
  begin
    url := SubString(preurls.Strings[i], ' ', 1);
    offset := SubString(preurls.Strings[i], ' ', 2);
    irc_addtext(Netname, Channel, '#%d %s %s', [i, url, offset]);
  end;

  Result := True;
end;

function IrcPreURLAdd(const netname, channel, params: String): boolean;
var
  url, offset: String;
begin
  Result := False;

  url := SubString(params, ' ', 1);
  offset := SubString(params, ' ', 2);

  if preurls.IndexOf(url + ';' + offset) > -1 then
  begin
    irc_addtext(Netname, Channel, 'Error! url already added!');
    Result := True;
    exit;
  end;
  preurls.Add(url + ';' + offset);

  Result := True;
end;

function IrcPreURLDel(const netname, channel, params: String): boolean;
var
  s: String;
  index: integer;
begin
  Result := False;

  s := SubString(params, ' ', 1);
  index := StrToInt(s);

  preurls.BeginUpdate;
  try
    preurls.Delete(index);
  finally
    preurls.EndUpdate;
  end;

  Result := True;
end;

function IrcPreURLMod(const netname, channel, params: String): boolean;
var
  s, url, offset: String;
  index: integer;
begin
  Result := False;

  s := SubString(params, ' ', 1);
  url := SubString(params, ' ', 2);
  offset := SubString(params, ' ', 3);

  index := StrToInt(s);

  preurls.BeginUpdate;
  try
    preurls.Strings[index] := url + ';' + offset;
  finally
    preurls.EndUpdate;
  end;
  
  Result := True;
end;

function IrcTestOffset(const netname, channel, params: String): boolean;
var
  voctime, vctime, vnow, cnow: int64;
  response, url, ss, s: String;
  x: TRegExpr;
  fHttpGetErrMsg: String;
begin
  //  Result := False;

  voctime := -99;
  vctime := -99;
  vnow := -1;
  url := '';
  x := TRegExpr.Create;
  try
    x.Expression := '^(\S+) (\S+) (\S+) (\S+) (\S+)$';
    irc_addtext(Netname, Channel, 'Offset TEST');
    // Irc_AddText(netname,channel,'Trigger: %s : Value: %d ',[offset.Trigger,offset.OffSetValue]);

    if params <> '' then
    begin
      if not HttpGetUrl(url + params, response, fHttpGetErrMsg) then
      begin
        irc_addtext(Netname, Channel, Format('<c4>[FAILED]</c> Offset TEST --> %s', [fHttpGetErrMsg]));
        exit;
      end;

      if x.Exec(response) then
      begin
        voctime := strtoint64(x.Match[2]);
        // vctime:=offset.NewCtime(voctime);
      end;
      if ((voctime <> -99) and (vctime <> -99)) then
      begin
        s := format('[%d] %s', [voctime, DateTimeAsString(
            UnixToDateTime(voctime))]) + #13#10;
        ss := format('[%d] %s', [vctime, DateTimeAsString(
            UnixToDateTime(vctime))]) + #13#10;
        s := s + '' + DatetimetoStr(UnixToDateTime(voctime));
        ss := ss + '' + DatetimetoStr(UnixToDateTime(vctime));
      end
      else
      begin
        s := 'No Pretime Found!';
        ss := 'No Pretime Found!';
      end; // if ((voctime <> -99) and (vctime <> -99)) then begin
      irc_addtext(Netname, Channel, 'Database Time:');
      irc_addtext(Netname, Channel, s);
      irc_addtext(Netname, Channel, 'Fixed Time:');
      irc_addtext(Netname, Channel, ss);

    end
    else
    begin // if params <> '' then begin
      cnow := DateTimeToUnix(now);

      irc_addtext(Netname, Channel, 'Offset TEST');
      // (%s%sh)',[vtrigger,offset.OffSet]);
      // Irc_AddText(netname,channel,'Trigger: %s : Value: %d ',[vtrigger,offset.OffSetValue]);
      irc_addtext(Netname, Channel, 'Realtime: %d (%s)',
        [cnow, DatetimetoStr(UnixToDateTime(cnow))]);
      irc_addtext(Netname, Channel, 'Fixxedtime: %d (%s)',
        [vnow, DatetimetoStr(UnixToDateTime(vnow))]);
    end;

  finally
    x.Free;
  end;

  Result := True;
end;

end.