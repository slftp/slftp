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
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddWritesFileOnlyOnSave;
    procedure TestNewsSaveWithoutChangesDoesNotWriteFile;
    procedure TestDeleteCategoryRemovesOnlyThatCategory;
    procedure TestDeleteNumberRemovesEntry;
  end;

implementation

uses
  SysUtils, news;

{ TTestNews }

procedure TTestNews.SaveAndReload;
begin
  NewsUnInit;
  NewsInit;
end;

procedure TTestNews.SetUp;
begin
  FFileName := ExtractFilePath(ParamStr(0)) + 'slftp.news';
  DeleteFile(FFileName);
  NewsInit;
end;

procedure TTestNews.TearDown;
begin
  NewsUnInit;
  DeleteFile(FFileName);
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

  SlftpNewsDelete('', '', 1, False);

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
