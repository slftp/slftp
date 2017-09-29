unit news;

interface

uses
  SysUtils;

{ Just a helper function to set @value(SlftpNewsFilename) on startup }
procedure NewsInit;

{ Check if category is in known MessageCategories array. If not found it returns -1, else index of matching element. }
function CheckForValidCategory(const category: AnsiString): Integer;

{ Returns the categories from MessageCategories array as String. }
function ValidCategoriesAsString: AnsiString;

{ Adds a new message entry
  @param(category is to sort messages into categories. See @link(MessageCategories))
  @param(NewMessage is a string with the message which should be stored (supports all mirc codes stuff etc))
  @param(dupecheck if @true, it checks if there is an identical entry as the new one and deletes old and add it again if it's older than 7 days)
  @returns(@true on success, @false otherwise) }
function SlftpNewsAdd(const category, NewMessage: AnsiString; dupecheck: boolean = False): boolean; overload;

{ Add a new news entry (calls @link(SlftpNewsAdd)) and returns a text with the msg which was added
  @param(Netname for output text)
  @param(Channel for output text)
  @param(category is to sort messages into categories. See @link(MessageCategories))
  @param(NewMessage is a string with the message with should be stored (supports all mirc color stuff etc))
  @returns(@true on success, @false otherwise) }
function SlftpNewsAdd(const Netname, Channel, category, NewMessage: AnsiString): boolean; overload;

{ Shows newsfile entries.
  @param(ShowCount actual news count to show; -1 means all)
  @param(category if not definied, all messages are shown. When given, it only shows messages which matches category (see @link(MessageCategories))) }
function SlftpNewsShow(const Netname, Channel: AnsiString; const ShowCount: Integer; category: AnsiString = ''): boolean;


{ Deletes given newsfile entry.
  @param(DeleteNumber is the entry ID which should be deleted) }
function SlftpNewsDelete(const Netname, Channel: AnsiString; const DeleteNumber: Integer): boolean; overload;

{ Deletes given category entries.
  @param(category is the category which should be deleted) }
function SlftpNewsDelete(const Netname, Channel: AnsiString; const category: AnsiString): boolean; overload;

{ Status text for @link(IrcShowAppStatus) command, shows read/unread messages }
function SlftpNewsStatus(): AnsiString;

implementation

uses
  Classes, StrUtils, DateUtils, encinifile, configunit, irc, mystrings, debugunit,regexpr;

const
  section = 'news';
  { Identifier for a read message }
  cREAD_IDENTIFIER = '!READ!';
  { Identifier for an unread message }
  cUNREAD_IDENTIFIER = '!UNREAD!';
  { All possible categories to sort a message. Try to avoid using UNSORTED!
    Only for use via sourcecode. }
  MessageCategories: array[0..8] of AnsiString = (
    'AUTORULES', 'FTP', 'GROUPS', 'IRC', 'NUKES', 'RULES', 'SITES', 'TVMAZE', 'UNSORTED'
  );

var
  SlftpNewsFilename: AnsiString;


procedure NewsInit;
begin
  SlftpNewsFilename := ExtractFilePath(ParamStr(0)) + 'slftp.news';
end;

function CheckForValidCategory(const category: AnsiString): Integer;
begin
  Result := AnsiIndexText(category, MessageCategories);
end;

function ValidCategoriesAsString: AnsiString;
var
  i: Integer;
  validcategories: AnsiString;
begin
  validcategories := '';
  for i := Low(MessageCategories) to High(MessageCategories) do
    validcategories := validcategories + MessageCategories[i] + ' ';

  Result := validcategories;
end;

function SlftpNewsAdd(const category, NewMessage: AnsiString; dupecheck: boolean = False): boolean; overload;
var
  x: TEncStringList;
  msgformat: TStringList;
  i, j: Integer;
  myDate: TDateTime;
  newsDate : TDateTime;
  dontadd: boolean;
  rx: TRegexpr;
begin
  Result := False;
  dontadd := False;

  i := CheckForValidCategory(category);
  if i = -1 then
  begin
    debug(dpSpam, section, Format('Category %s not valid!', [category]));
    exit;
  end;

  rx := TRegexpr.Create;
  try
    rx.Expression := '(\d{1,2}).(\d{1,2}).(\d{2,4})\s+(\d{1,2})\:(\d{2})';

    x := TEncStringList.Create(passphrase);
    try
      x.BeginUpdate;
      x.LoadFromFile(SlftpNewsFilename);

      msgformat := TStringList.Create;
      try

        if dupecheck then
        begin
          for j := 0 to x.Count - 1 do
          begin
            msgformat.DelimitedText := x[j];

            if (msgformat[2] = category) and (msgformat[3] = NewMessage) then
            begin
              myDate := IncDay(Now, -7); // get date from 7 days ago

              if rx.Exec(msgformat[1]) and TryEncodeDateTime(StrToInt(rx.Match[2]), StrToInt(rx.Match[3]), StrToInt(rx.Match[4]), StrToInt(rx.Match[5]), StrToInt(rx.Match[6]), 0, 0, newsDate) then
              begin
                if newsDate < myDate then
                begin
                  x.Delete(j);
                end
                else
                begin
                  dontadd := True;
                end;
              end;

              break;
            end;
          end;

          msgformat.Clear;
        end;

        if not dontadd then
        begin
          // stored as: !UNREAD!,"08-9-17 18:30",IRC,"This is just a message!"
          msgformat.Add(cUNREAD_IDENTIFIER);
          msgformat.Add(FormatDateTime('dd-m-yy hh:nn', Now));
          msgformat.Add(MessageCategories[i]);
          msgformat.Add(NewMessage);

          x.Insert(0, msgformat.CommaText);
        end;
      finally
        msgformat.Free;
      end;

      x.SaveToFile(SlftpNewsFilename);
      x.EndUpdate;
    finally
      x.Free;
    end;

  finally
    rx.Free;
  end;

  Result := True;
end;

function SlftpNewsAdd(const Netname, Channel, category, NewMessage: AnsiString): boolean; overload;
var
  i: Integer;
begin
  Result := False;

  i := CheckForValidCategory(category);
  if i = -1 then
  begin
    irc_addtext(Netname, Channel, Format('You need to use a valid message category! Use one of these: %s', [ValidCategoriesAsString()]));
    exit;
  end;

  Result := SlftpNewsAdd(category, NewMessage, False);

  if Result then
    irc_addtext(Netname, Channel, Format('New entry ''[%s] %s'' added.', [category, NewMessage]));
end;

function SlftpNewsShow(const Netname, Channel: AnsiString; const ShowCount: Integer; category: AnsiString = ''): boolean;
var
  x: TEncStringList;
  i, j, padding: integer;
  IrcAnnounceText: AnsiString;
  actualmsg: TStringList;
  textshown: boolean;
begin
  Result := False;
  textshown := False;

  if (category <> '') and (CheckForValidCategory(category) = -1) then
  begin
    irc_addtext(Netname, Channel, Format('Given message category %s is unknown. Valid ones are: %s', [category, ValidCategoriesAsString()]));
    exit;
  end;

  x := TEncStringList.Create(passphrase);
  try
    x.LoadFromFile(SlftpNewsFilename);
    x.BeginUpdate;

    j := x.Count - 1;

    if j < 0 then
    begin
      irc_addtext(Netname, Channel, '<b>No news are good news!</b>');
    end
    else
    begin
      if ShowCount > j then
      begin
        // showing count is higher than all entries -> show all!
        j := x.Count - 1;
        IrcAnnounceText := Format('Showing the last <b>%d</b> entries:', [x.Count]);
      end
      else
      begin
        // showing count is less than all entries -> show ShowCount
        j := ShowCount - 1;
        IrcAnnounceText := Format('Showing the last <b>%d</b> of %d entries:', [ShowCount, x.Count]);
      end;

      // format output line -> pad numbers on the left
      // as we increment actual entry index by one each time, we need to decrease padding by one
      // e.g. j := 8 will be shown as 9, j := 9 will be shown as 10 (so padding need to be 2)
      case j of
        0..8: padding := 1;
        9..98: padding := 2;
        99..998: padding := 3;
        else
          padding := 4;
      end;

      actualmsg := TStringList.Create;
      try

        for i := 0 to j do
        begin
          actualmsg.DelimitedText := x[i];

          if ( (category = '') or ((category <> '') and (category = actualmsg[2])) ) then
          begin

            if not textshown then
            begin
              if (category <> '') then
              begin
                // needed to avoid that it writes to IRC when a invalid category is given
                irc_addtext(Netname, Channel, Format('Showing the last <b>%d</b> (or less) entries with category %s:', [ShowCount, actualmsg[2]]));
              end
              else
                irc_addtext(Netname, Channel, Format('%s', [IrcAnnounceText]));

              textshown := True;
            end;

            if actualmsg[0] = cUNREAD_IDENTIFIER then
            begin
              // change '!UNREAD!' to '!READ!' status of shown entry
              actualmsg[0] := cREAD_IDENTIFIER;

              x[i] := actualmsg.CommaText;
            end;

            irc_addtext(Netname, Channel, Format('[%*d:] %s :: [%s] :: %s', [padding, i + 1, actualmsg[1], actualmsg[2], actualmsg[3]]));
          end;

        end;

      finally
        actualmsg.Free;
      end;

    end;

    x.EndUpdate;
    x.SaveToFile(SlftpNewsFilename);
  finally
    x.Free;
  end;

  Result := True;
end;

function SlftpNewsDelete(const Netname, Channel: AnsiString; const DeleteNumber: Integer): boolean; overload;
var
  x: TEncStringList;
  msgtext: TStringList;
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
        msgtext := TStringList.Create;
        try
          // - 1 because we show all entries with an offset of one
          msgtext.CommaText := x[DeleteNumber - 1];

          x.Delete(DeleteNumber - 1);
          irc_addtext(Netname, Channel, Format('Entry ''[%s] %s'' deleted.', [msgtext[2], msgtext[3]]));
        finally
          msgtext.Free;
        end;
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

function SlftpNewsDelete(const Netname, Channel: AnsiString; const category: AnsiString): boolean; overload;
var
  x: TEncStringList;
  i, j: Integer;
  msgtext: TStringList;
begin
  Result := False;
  j := 0;

  if (CheckForValidCategory(category) = -1) then
  begin
    irc_addtext(Netname, Channel, Format('Given message category %s is unknown. Valid ones are: %s', [UpperCase(category), ValidCategoriesAsString()]));
    exit;
  end;

  x := TEncStringList.Create(passphrase);
  try
    x.LoadFromFile(SlftpNewsFilename);

    msgtext := TStringList.Create;
    try
      for i := 0 to x.Count - 1 do
      begin
        msgtext.CommaText := x[i];

        if msgtext[2] = UpperCase(category) then
        begin
          SlftpNewsDelete(Netname, Channel, i + 1 - j);
          Inc(j);
        end;
      end;
    finally
      msgtext.Free;
    end;

  finally
    x.Free;
  end;

  Result := True;
end;

function SlftpNewsStatus(): AnsiString;
var
  x: TEncStringList;
  i, ReadCount, UnreadCount: Integer;
  ReadStatus: AnsiString;
begin
  UnreadCount := 0;
  ReadCount := 0;

  x := TEncStringList.Create(passphrase);
  try
    x.LoadFromFile(SlftpNewsFilename);

    for i := 0 to x.Count - 1 do
    begin
      ReadStatus := SubString(x[i], ',', 1);

      if ReadStatus = cUNREAD_IDENTIFIER then
        Inc(UnreadCount)
      else
        Inc(ReadCount);
    end;
  finally
    x.Free;
  end;

  Result := Format('<b>News</b>: You have <b>%d</b> unread from overall <b>%d</b> messages.', [UnreadCount, UnreadCount + ReadCount]);
end;

end.
