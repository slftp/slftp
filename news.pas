unit news;

interface

uses
  SysUtils;

{ Just a helper function to set @value SlftpNewsFilename on startup }
procedure NewsInit;
{ Add a new news entry
  @param(NewsMessage is a string with the message with should be stored (supports all mirc color stuff etc))
  @returns(@true on success, @false otherwise) }
function SlftpNewsAdd(const NewsMessage: AnsiString): boolean; overload;
{ Add a new news entry (calls @link(SlftpNewsAdd)) and returns a text with the msg which was added
  @param(Netname for output text)
  @param(Channel for output text)
  @param(NewsMessage is a string with the message with should be stored (supports all mirc color stuff etc))
  @returns(@true on success, @false otherwise) }
function SlftpNewsAdd(const Netname, Channel, NewsMessage: AnsiString): boolean; overload;
{ Shows newsfile entries.
  @param(ShowCount actual news count to show; -1 means all) }
function SlftpNewsShow(const Netname, Channel: AnsiString; const ShowCount: Integer): boolean;
{ Deletes given newsfile entries.
  @param(DeleteNumber is the entry ID which should be deleted) }
function SlftpNewsDelete(const Netname, Channel: AnsiString; const DeleteNumber: Integer): boolean;
{ Status text for @link(IrcShowAppStatus) command, shows read/unread messages }
function SlftpNewsStatus(): AnsiString;

implementation

uses
  encinifile, configunit, irc, mystrings;

const
  cREAD_IDENTIFIER = '!READ!';
  cUNREAD_IDENTIFIER = '!UNREAD!';

var
  SlftpNewsFilename: AnsiString;

procedure NewsInit;
begin
  SlftpNewsFilename := ExtractFilePath(ParamStr(0)) + 'slftp.news';
end;


function SlftpNewsAdd(const NewsMessage: AnsiString): boolean; overload;
var
  x: TEncStringList;
begin
  Result := False;

  x := TEncStringList.Create(passphrase);
  try
    x.BeginUpdate;
    x.LoadFromFile(SlftpNewsFilename);

    x.Insert(0, cUNREAD_IDENTIFIER + ' ' + FormatDateTime('dd-mmm-yyyy hh:nn', Now) + ' :: ' + NewsMessage);

    x.SaveToFile(SlftpNewsFilename);
    x.EndUpdate;
  finally
    x.Free;
  end;

  Result := True;
end;

function SlftpNewsAdd(const Netname, Channel, NewsMessage: AnsiString): boolean; overload;
begin
  Result := SlftpNewsAdd(NewsMessage);

  if Result then
    irc_addtext(Netname, Channel, Format('New entry ''%s'' added.', [NewsMessage]));
end;


function SlftpNewsShow(const Netname, Channel: AnsiString; const ShowCount: Integer): boolean;
var
  x: TEncStringList;
  i, j, padding: integer;
  ReadStatus: AnsiString;
begin
  Result := False;

  x := TEncStringList.Create(passphrase);
  try
    x.LoadFromFile(SlftpNewsFilename);
    x.BeginUpdate;

    j := x.Count - 1;

    if j < 0 then
      irc_addtext(Netname, Channel, '<b>No news are good news!</b>')
    else
    begin
      if ShowCount > j then
      begin
        j := x.Count - 1;
        irc_addtext(Netname, Channel, Format('Showing the last <b>%d</b> entries:', [x.Count]));
      end
      else
      begin
        j := ShowCount;
        irc_addtext(Netname, Channel, Format('Showing the last <b>%d</b> of %d entries:', [ShowCount, x.Count]));
      end;

      for i := 0 to j do
      begin
        ReadStatus := SubString(x[i], ' ', 1);
        if ReadStatus = cUNREAD_IDENTIFIER then
        begin
          x[i] := cREAD_IDENTIFIER + ' ' + RightStr(x[i], Length(cUNREAD_IDENTIFIER) + 1);
        end;

        // pad numbers on the left
        case j of
          0..9: padding := 1;
          10..99: padding := 2;
          100..999: padding := 3;
          else
            padding := 4;
        end;

        irc_addtext(Netname, Channel, Format('[%*d:] %s', [padding, i + 1, RightStr(x[i], Length(cREAD_IDENTIFIER) + 1)]));
      end;
    end;

    x.EndUpdate;
    x.SaveToFile(SlftpNewsFilename);
  finally
    x.Free;
  end;

  Result := True;
end;


function SlftpNewsDelete(const Netname, Channel: AnsiString; const DeleteNumber: Integer): boolean;
var
  x: TEncStringList;
  newsentry, unwanted: AnsiString;
  i: Integer;
begin
  Result := False;

  x := TEncStringList.Create(passphrase);
  try
    x.LoadFromFile(SlftpNewsFilename);
    x.BeginUpdate;

    if DeleteNumber = -1 then
    begin
      // delete all entries
      x.Clear;
      irc_addtext(Netname, Channel, '<b>All</b> entries deleted.');
    end
    else
    begin
      // only delete given entry by number
      if x.Count >= DeleteNumber then
      begin
        newsentry := x[DeleteNumber - 1];
        i := Pos('::', newsentry) + 3;
        x.Delete(DeleteNumber - 1);
        irc_addtext(Netname, Channel, Format('Entry ''%s'' deleted.', [Copy(newsentry, i, Length(newsentry))]));
      end
      else
        irc_addtext(Netname, Channel, Format('No entry for given number <b>%d</b> found.', [DeleteNumber]));
    end;

    x.EndUpdate;
    x.SaveToFile(SlftpNewsFilename);
  finally
    x.Free;
  end;

  Result := True;
end;


function SlftpNewsStatus(): AnsiString;
var
  x: TEncStringList;
  i, read, unread: Integer;
  ReadStatus: AnsiString;
begin
  unread := 0;
  read := 0;

  x := TEncStringList.Create(passphrase);
  try
    x.LoadFromFile(SlftpNewsFilename);

    for i := 0 to x.Count - 1 do
    begin
      ReadStatus := SubString(x[i], ' ', 1);

      if ReadStatus = cUNREAD_IDENTIFIER then
        Inc(unread)
      else
        Inc(read);
    end;

  finally
    x.Free;
  end;

  Result := Format('<b>News</b>: You have <b>%d</b> unread from overall <b>%d</b> messages.', [unread, unread + read]);
end;

end.
