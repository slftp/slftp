unit newsTests;

interface

uses
  {$IFDEF FPC}
    fpcunit, testregistry;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestNews = class(TTestCase)
  private
    FFileName: String; //< news file used by the tests, removed after each test
    { Saves the entries, frees the news unit and loads the entries again from the news file }
    procedure SaveAndReload;
    { Counts the backups of corrupt news files next to @link(FFileName)
      @param(aDelete delete the backups if @true)
      @returns(number of backups found) }
    function CorruptBackups(const aDelete: Boolean): Integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddWritesFileOnlyOnSave;
    procedure TestNewsSaveWithoutChangesDoesNotWriteFile;
    procedure TestNewsSaveReplacesFileWithoutTempFile;
    procedure TestDeleteCategoryRemovesOnlyThatCategory;
    procedure TestDeleteNumberRemovesEntry;
    procedure TestAddWithDupecheckAddsMessageOnlyOnce;
    procedure TestDupecheckSkipsRecentDuplicate;
    procedure TestDupecheckReplacesOldDuplicate;
    procedure TestCorruptFileIsBackedUpAndNewsStartEmpty;
  end;

implementation

uses
  Classes, SysUtils, configunit, encinifile, news;

{ TTestNews }

procedure TTestNews.SaveAndReload;
begin
  NewsUnInit;
  NewsInit;
end;

function TTestNews.CorruptBackups(const aDelete: Boolean): Integer;
var
  fSearchRec: TSearchRec;
begin
  Result := 0;
  if FindFirst(FFileName + '.corrupt_*', faAnyFile, fSearchRec) = 0 then
  begin
    try
      repeat
        Inc(Result);
        if aDelete then
          DeleteFile(ExtractFilePath(FFileName) + fSearchRec.Name);
      until FindNext(fSearchRec) <> 0;
    finally
      FindClose(fSearchRec);
    end;
  end;
end;

procedure TTestNews.SetUp;
begin
  FFileName := ExtractFilePath(ParamStr(0)) + 'slftp.news';
  DeleteFile(FFileName);
  CorruptBackups(True);
  NewsInit;
end;

procedure TTestNews.TearDown;
begin
  NewsUnInit;
  DeleteFile(FFileName);
  CorruptBackups(True);
end;

procedure TTestNews.TestAddWritesFileOnlyOnSave;
begin
  CheckTrue(SlftpNewsAdd('IRC', 'first message'));
  CheckTrue(SlftpNewsAdd('FTP', 'second message'));
  CheckFalse(FileExists(FFileName), 'adding an entry must not write the news file');

  NewsSave;
  CheckTrue(FileExists(FFileName), 'NewsSave must write the changed entries');

  SaveAndReload;
  CheckTrue(Pos('<b>2</b> unread from overall <b>2</b>', SlftpNewsStatus) > 0, SlftpNewsStatus);
end;

procedure TTestNews.TestNewsSaveWithoutChangesDoesNotWriteFile;
begin
  NewsSave;
  CheckFalse(FileExists(FFileName));
end;

procedure TTestNews.TestNewsSaveReplacesFileWithoutTempFile;
begin
  SlftpNewsAdd('IRC', 'irc 1');
  NewsSave;
  SlftpNewsAdd('IRC', 'irc 2');
  NewsSave;
  CheckFalse(FileExists(FFileName + '.sltmp'), 'the temp file must be renamed to the news file');

  SaveAndReload;
  CheckTrue(Pos('overall <b>2</b>', SlftpNewsStatus) > 0, SlftpNewsStatus);
end;

procedure TTestNews.TestDeleteCategoryRemovesOnlyThatCategory;
begin
  SlftpNewsAdd('IRC', 'irc 1');
  SlftpNewsAdd('FTP', 'ftp 1');
  SlftpNewsAdd('IRC', 'irc 2');
  SlftpNewsAdd('IRC', 'irc 3');

  SlftpNewsDelete('', '', 'IRC', False);
  CheckTrue(Pos('overall <b>1</b>', SlftpNewsStatus) > 0, SlftpNewsStatus);

  SaveAndReload;
  CheckTrue(Pos('overall <b>1</b>', SlftpNewsStatus) > 0, SlftpNewsStatus);
end;

procedure TTestNews.TestDeleteNumberRemovesEntry;
begin
  SlftpNewsAdd('IRC', 'irc 1');
  SlftpNewsAdd('FTP', 'ftp 1');

  // entries are stored newest first, so number 1 is the FTP entry
  SlftpNewsDelete('', '', 1, False);

  SaveAndReload;
  CheckTrue(Pos('overall <b>1</b>', SlftpNewsStatus) > 0, SlftpNewsStatus);

  SlftpNewsDelete('', '', 'IRC', False);
  CheckTrue(Pos('overall <b>0</b>', SlftpNewsStatus) > 0, 'the IRC entry must be the one left: ' + SlftpNewsStatus);
end;

procedure TTestNews.TestAddWithDupecheckAddsMessageOnlyOnce;
begin
  SlftpNewsAdd('TVMAZE', 'same message', True);
  SlftpNewsAdd('TVMAZE', 'same message', True);
  SlftpNewsAdd('TVMAZE', 'other message', True);
  CheckTrue(Pos('overall <b>2</b>', SlftpNewsStatus) > 0, SlftpNewsStatus);

  SaveAndReload;
  CheckTrue(Pos('overall <b>2</b>', SlftpNewsStatus) > 0, SlftpNewsStatus);
end;

procedure TTestNews.TestDupecheckSkipsRecentDuplicate;
begin
  SlftpNewsAdd('TVMAZE', 'same message', True);
  SlftpNewsAdd('IRC', 'other message');
  SlftpNewsAdd('TVMAZE', 'same message', True);

  // entries are stored newest first: the skipped duplicate keeps the IRC entry as number 1
  SlftpNewsDelete('', '', 1, False);
  SlftpNewsDelete('', '', 'TVMAZE', False);
  CheckTrue(Pos('overall <b>0</b>', SlftpNewsStatus) > 0, 'the duplicate must not be added again: ' + SlftpNewsStatus);
end;

procedure TTestNews.TestDupecheckReplacesOldDuplicate;
var
  fFile: TEncStringList;
begin
  NewsUnInit;
  fFile := TEncStringList.Create(passphrase);
  try
    fFile.Add('!READ!,"01-1-20 10:00",TVMAZE,"same message"');
    fFile.SaveToFile(FFileName);
  finally
    fFile.Free;
  end;
  NewsInit;

  SlftpNewsAdd('TVMAZE', 'same message', True);
  CheckTrue(Pos('<b>1</b> unread from overall <b>1</b>', SlftpNewsStatus) > 0,
    'the old duplicate must be replaced by a new unread entry: ' + SlftpNewsStatus);
end;

procedure TTestNews.TestCorruptFileIsBackedUpAndNewsStartEmpty;
var
  fFile: TStringList;
begin
  NewsUnInit;
  fFile := TStringList.Create;
  try
    fFile.Add('this is not an encrypted news file');
    fFile.SaveToFile(FFileName);
  finally
    fFile.Free;
  end;

  NewsInit;
  CheckFalse(FileExists(FFileName), 'the corrupt news file must be renamed');
  CheckEquals(1, CorruptBackups(False), 'the corrupt news file must be kept as backup');
  CheckTrue(Pos('overall <b>0</b>', SlftpNewsStatus) > 0, SlftpNewsStatus);

  SlftpNewsAdd('IRC', 'irc 1');
  SaveAndReload;
  CheckTrue(Pos('overall <b>1</b>', SlftpNewsStatus) > 0, SlftpNewsStatus);
end;

initialization
  {$IFDEF FPC}
    RegisterTest('news', TTestNews.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestNews);
  {$ENDIF}
end.
