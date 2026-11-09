unit news;

interface

uses
  SysUtils;

{ Just a helper function to set @link(SlftpNewsFilename) on startup, initialize @link(NewsMultiReadSingleWriteLock)
  and load all entries from the news file into memory. A news file which can't be loaded is renamed to
  slftp.news.corrupt_<timestamp> and slFtp starts without news entries. }
procedure NewsInit;

{ Saves changed entries with @link(NewsSave) and frees @link(NewsMultiReadSingleWriteLock) }
procedure NewsUnInit;

{ Writes the entries to the news file if they have been changed since the last save. All other functions only
  change the entries in memory, so this has to be called regularly. Must not be called from several threads at
  the same time. }
procedure NewsSave;

{ Check if category is in known MessageCategories array. If not found it returns -1, else index of matching element. }
function CheckForValidCategory(const category: String): Integer;

{ Returns the categories from MessageCategories array as String. }
function ValidCategoriesAsString: String;

{ Adds a new message entry
  @param(category is to sort messages into categories. See @link(MessageCategories))
  @param(NewMessage is a string with the message which should be stored (supports all mirc codes stuff etc))
  @param(dupecheck if @true, it checks if there is an identical entry as the new one and deletes old and add it again if it's older than 7 days)
  @returns(@true on success, @false otherwise) }
function SlftpNewsAdd(const category, NewMessage: String; dupecheck: boolean = False): boolean; overload;

{ Add a new news entry (calls @link(SlftpNewsAdd)) and returns a text with the msg which was added
  @param(Netname for output text)
  @param(Channel for output text)
  @param(category is to sort messages into categories. See @link(MessageCategories))
  @param(NewMessage is a string with the message with should be stored (supports all mirc color stuff etc))
  @returns(@true on success, @false otherwise) }
function SlftpNewsAdd(const Netname, Channel, category, NewMessage: String): boolean; overload;

{ Shows newsfile entries.
  @param(ShowCount actual news count to show; -1 means all)
  @param(category if not definied, all messages are shown. When given, it only shows messages which matches category (see @link(MessageCategories))) }
function SlftpNewsShow(const Netname, Channel: String; const ShowCount: Integer; category: String = ''): boolean;


{ Deletes given newsfile entry.
  @param(DeleteNumber is the entry ID which should be deleted)
  @param(AnnounceIt is to enable/disable IRC Announce of deleted entry) }
function SlftpNewsDelete(const Netname, Channel: String; const DeleteNumber: Integer; AnnounceIt: boolean = True): boolean; overload;

{ Deletes given category entries.
  @param(category is the category which should be deleted)
  @param(AnnounceIt is to enable/disable IRC Announce of deleted entry) }
function SlftpNewsDelete(const Netname, Channel: String; const category: String; AnnounceIt: boolean = False): boolean; overload;

{ Status text for @link(IrcShowAppStatus) command, shows read/unread messages }
function SlftpNewsStatus(): String;

var
  last_news_announce: TDateTime; //< time value when @link(SlftpNewsStatus) was triggered the last time
  news_last_save: TDateTime; //< time value when @link(NewsSave) was called the last time

implementation

uses
  // PasMP does not compile on BSD, so don't use it and don't use the news unit
  Classes, StrUtils, DateUtils, encinifile, configunit, irc, console, mystrings, debugunit, RegExpr{$IFNDEF BSD}, PasMP{$ENDIF};

const
  section = 'news';
  { Identifier for a read message }
  cREAD_IDENTIFIER = '!READ!';
  { Identifier for an unread message }
  cUNREAD_IDENTIFIER = '!UNREAD!';
  { All possible categories to sort a message. Try to avoid using UNSORTED!
    Only for use via sourcecode. }
  MessageCategories: array[0..8] of String = (
    'AUTORULES', 'FTP', 'GROUPS', 'IRC', 'NUKES', 'RULES', 'SITES', 'TVMAZE', 'UNSORTED'
  );

var
  SlftpNewsFilename: String;
{$IFNDEF BSD}
  NewsMultiReadSingleWriteLock: TPasMPMultipleReaderSingleWriterLock;
  { all entries of the news file, newest first; only access it while holding @link(NewsMultiReadSingleWriteLock) }
  glNewsEntries: TEncStringList;
  { @true if @link(glNewsEntries) has been changed since it was loaded or saved the last time }
  glNewsChanged: Boolean;
{$ENDIF}


{$IFNDEF BSD}
{ Loads the news file into @link(glNewsEntries). If it can't be loaded, e.g. because it is corrupt, it is renamed to
  slftp.news.corrupt_<timestamp> so that the next save doesn't overwrite it, and @link(glNewsEntries) stays empty. }
procedure _LoadNewsFile;
var
  fBackupFilename, fMessage: String;
begin
  try
    glNewsEntries.LoadFromFile(SlftpNewsFilename);
  except
    on e: Exception do
    begin
      glNewsEntries.Clear;
      fBackupFilename := SlftpNewsFilename + '.corrupt_' + FormatDateTime('yyyymmdd-hhnnss', Now);
      if RenameFile(SlftpNewsFilename, fBackupFilename) then
        fMessage := Format('Could not load %s (%s), renamed it to %s and started without news entries.',
          [SlftpNewsFilename, e.Message, fBackupFilename])
      else
        fMessage := Format('Could not load %s (%s) and could not rename it to %s, started without news entries. ' +
          'The file gets overwritten with the next save.', [SlftpNewsFilename, e.Message, fBackupFilename]);

      Debug(dpError, section, fMessage);
      console_addline('Admin', fMessage, True);
    end;
  end;
end;
{$ENDIF}

procedure NewsInit;
begin
{$IFNDEF BSD}
  SlftpNewsFilename := ExtractFilePath(ParamStr(0)) + 'slftp.news';
  NewsMultiReadSingleWriteLock := TPasMPMultipleReaderSingleWriterLock.Create;
  last_news_announce := Now;
  news_last_save := Now;

  glNewsEntries := TEncStringList.Create(passphrase);
  _LoadNewsFile;
  glNewsChanged := False;
{$ENDIF}
end;

procedure NewsUnInit;
begin
{$IFNDEF BSD}
  if Assigned(glNewsEntries) then
  begin
    try
      NewsSave;
    except
      on e: Exception do
        Debug(dpError, section, '[EXCEPTION] NewsUnInit(NewsSave): %s', [e.Message]);
    end;
    FreeAndNil(glNewsEntries);
  end;

  if Assigned(NewsMultiReadSingleWriteLock) then
    FreeAndNil(NewsMultiReadSingleWriteLock);
{$ENDIF}
end;

procedure NewsSave;
{$IFNDEF BSD}
var
  fSnapshot: TEncStringList;
{$ENDIF}
begin
{$IFNDEF BSD}
  news_last_save := Now;

  fSnapshot := TEncStringList.Create(passphrase);
  try
    // copy the entries and encrypt/write them without holding the lock: the encryption of a big news file
    // takes long, and IRC threads which add news entries would wait for it
    NewsMultiReadSingleWriteLock.AcquireWrite;
    try
      if not glNewsChanged then
        exit;
      fSnapshot.Assign(glNewsEntries);
      glNewsChanged := False;
    finally
      NewsMultiReadSingleWriteLock.ReleaseWrite;
    end;

    try
      fSnapshot.SaveToFile(SlftpNewsFilename);
    except
      // try again with the next call
      NewsMultiReadSingleWriteLock.AcquireWrite;
      try
        glNewsChanged := True;
      finally
        NewsMultiReadSingleWriteLock.ReleaseWrite;
      end;
      raise;
    end;
  finally
    fSnapshot.Free;
  end;
{$ENDIF}
end;

function CheckForValidCategory(const category: String): Integer;
begin
  Result := AnsiIndexText(category, MessageCategories);
end;

function ValidCategoriesAsString: String;
var
  i: Integer;
  validcategories: String;
begin
  validcategories := '';
  for i := Low(MessageCategories) to High(MessageCategories) do
    validcategories := validcategories + MessageCategories[i] + ' ';

  Result := validcategories;
end;

function SlftpNewsAdd(const category, NewMessage: String; dupecheck: boolean = False): boolean; overload;
var
  msgformat: TStringList;
  i, j: Integer;
  myDate: TDateTime;
  newsDate : TDateTime;
  dontadd: boolean;
  rx: TRegexpr;
begin
{$IFNDEF BSD}
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

    NewsMultiReadSingleWriteLock.AcquireWrite;
    try
      msgformat := TStringList.Create;
      try

        if dupecheck then
        begin
          for j := 0 to glNewsEntries.Count - 1 do
          begin
            msgformat.DelimitedText := glNewsEntries[j];

            if (msgformat[2] = category) and (msgformat[3] = NewMessage) then
            begin
              myDate := IncDay(Now, -7); // get date from 7 days ago

              if rx.Exec(msgformat[1]) and TryEncodeDateTime(StrToInt(rx.Match[1]), StrToInt(rx.Match[2]), StrToInt(rx.Match[3]), StrToInt(rx.Match[4]), StrToInt(rx.Match[5]), 0, 0, newsDate) then
              begin
                if newsDate < myDate then
                begin
                  glNewsEntries.Delete(j);
                  glNewsChanged := True;
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

          glNewsEntries.Insert(0, msgformat.CommaText);
          glNewsChanged := True;
        end;
      finally
        msgformat.Free;
      end;
    finally
      NewsMultiReadSingleWriteLock.ReleaseWrite;
    end;

  finally
    rx.Free;
  end;
{$ENDIF}
  Result := True;
end;

function SlftpNewsAdd(const Netname, Channel, category, NewMessage: String): boolean; overload;
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

function SlftpNewsShow(const Netname, Channel: String; const ShowCount: Integer; category: String = ''): boolean;
var
  i, j, padding: integer;
  IrcAnnounceText: String;
  actualmsg: TStringList;
  textshown: boolean;
begin
{$IFNDEF BSD}
  Result := False;
  textshown := False;

  if (category <> '') and (CheckForValidCategory(category) = -1) then
  begin
    irc_addtext(Netname, Channel, Format('Given message category %s is unknown. Valid ones are: %s', [category, ValidCategoriesAsString()]));
    exit;
  end;

  NewsMultiReadSingleWriteLock.AcquireWrite;
  try
    j := glNewsEntries.Count - 1;

    if j < 0 then
    begin
      irc_addtext(Netname, Channel, '<b>No news are good news!</b>');
    end
    else
    begin
      if ShowCount > j then
      begin
        // showing count is higher than all entries -> show all!
        j := glNewsEntries.Count - 1;
        IrcAnnounceText := Format('Showing the last <b>%d</b> entries:', [glNewsEntries.Count]);
      end
      else
      begin
        // showing count is less than all entries -> show ShowCount
        j := ShowCount - 1;
        IrcAnnounceText := Format('Showing the last <b>%d</b> of %d entries:', [ShowCount, glNewsEntries.Count]);
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
          actualmsg.DelimitedText := glNewsEntries[i];

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

              glNewsEntries[i] := actualmsg.CommaText;
              glNewsChanged := True;
            end;

            irc_addtext(Netname, Channel, Format('[%*d:] %s :: [%s] :: %s', [padding, i + 1, actualmsg[1], actualmsg[2], actualmsg[3]]));
          end;
        end;
      finally
        actualmsg.Free;
      end;
    end;
  finally
    NewsMultiReadSingleWriteLock.ReleaseWrite;
  end;
{$ENDIF}
  Result := True;
end;

{$IFNDEF BSD}
{ Deletes an entry from @link(glNewsEntries), the caller must hold the write lock of @link(NewsMultiReadSingleWriteLock)
  @param(aEntry entry which should be deleted, already split into its fields)
  @param(aIndex index of the entry in @link(glNewsEntries))
  @param(aAnnounceIt announce the deleted entry on IRC if @true) }
procedure _DeleteEntry(const aNetname, aChannel: String; const aEntry: TStringList; const aIndex: Integer; const aAnnounceIt: Boolean);
begin
  glNewsEntries.Delete(aIndex);
  glNewsChanged := True;
  if aAnnounceIt then
    irc_addtext(aNetname, aChannel, Format('Entry ''[%s] %s'' deleted.', [aEntry[2], aEntry[3]]));
end;
{$ENDIF}

function SlftpNewsDelete(const Netname, Channel: String; const DeleteNumber: Integer; AnnounceIt: boolean = True): boolean; overload;
var
  msgtext: TStringList;
begin
{$IFNDEF BSD}
  Result := False;

  NewsMultiReadSingleWriteLock.AcquireWrite;
  try
    if DeleteNumber = -1 then
    begin
      // delete all entries
      glNewsEntries.Clear;
      glNewsChanged := True;
      irc_addtext(Netname, Channel, '<b>All</b> entries deleted.');
    end
    else
    begin
      // only delete given entry by number
      if ( (glNewsEntries.Count >= DeleteNumber) and (DeleteNumber <> 0) ) then
      begin
        msgtext := TStringList.Create;
        try
          // - 1 because we show all entries with an offset of one
          msgtext.CommaText := glNewsEntries[DeleteNumber - 1];
          _DeleteEntry(Netname, Channel, msgtext, DeleteNumber - 1, AnnounceIt);
        finally
          msgtext.Free;
        end;
      end
      else
        irc_addtext(Netname, Channel, Format('No entry for given number <b>%d</b> found.', [DeleteNumber]));
    end;
  finally
    NewsMultiReadSingleWriteLock.ReleaseWrite;
  end;
{$ENDIF}
  Result := True;
end;

function SlftpNewsDelete(const Netname, Channel: String; const category: String; AnnounceIt: boolean = False): boolean; overload;
var
  i: Integer;
  msgtext: TStringList;
begin
{$IFNDEF BSD}
  Result := False;

  if (CheckForValidCategory(category) = -1) then
  begin
    irc_addtext(Netname, Channel, Format('Given message category %s is unknown. Valid ones are: %s', [UpperCase(category), ValidCategoriesAsString()]));
    exit;
  end;

  NewsMultiReadSingleWriteLock.AcquireWrite;
  try
    msgtext := TStringList.Create;
    try
      i := 0;
      while i < glNewsEntries.Count do
      begin
        msgtext.CommaText := glNewsEntries[i];

        if msgtext[2] = UpperCase(category) then
          _DeleteEntry(Netname, Channel, msgtext, i, AnnounceIt)
        else
          Inc(i);
      end;
    finally
      msgtext.Free;
    end;
  finally
    NewsMultiReadSingleWriteLock.ReleaseWrite;
  end;
{$ENDIF}
  Result := True;
end;

function SlftpNewsStatus(): String;
var
  i, ReadCount, UnreadCount: Integer;
  ReadStatus: String;
begin
  UnreadCount := 0;
  ReadCount := 0;
{$IFNDEF BSD}
  NewsMultiReadSingleWriteLock.AcquireRead;
  try
    for i := 0 to glNewsEntries.Count - 1 do
    begin
      ReadStatus := SubString(glNewsEntries[i], ',', 1);

      if ReadStatus = cUNREAD_IDENTIFIER then
        Inc(UnreadCount)
      else
        Inc(ReadCount);
    end;
  finally
    NewsMultiReadSingleWriteLock.ReleaseRead;
  end;
{$ENDIF}
  last_news_announce := Now;
  Result := Format('<b>News</b>: You have <b>%d</b> unread from overall <b>%d</b> messages. See %snews for further details.', [UnreadCount, UnreadCount + ReadCount, irc.irccmdprefix]);
end;

end.
