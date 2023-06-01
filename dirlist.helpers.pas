unit dirlist.helpers;

interface

{ Check if given file is screwed up by FTPRush
  @param(aFilename Filename)
  @param(aFileExtension File extension of given filename)
  @returns(@true if screwed up file, @false otherwise.) }
function IsFtpRushScrewedUpFile(const aFilename, aFileExtension: String): Boolean;

{ Returns true, if the dir contains a special tag indicating the rls can be complete only containing the NFO (dirfix, nfofix, ...)
  @param(aFullPath the path/dir to check)
  @returns(@true release can contain only a NFO, @false otherwise.) }
function ReleaseOnlyConsistsOfNFO(const aFullPath: String): Boolean;

{ Parses a 'stat -l' line and extracts the information
  @param(aRespLine single line of ftpd response)
  @param(aDirMask extracted dirmask)
  @param(aUsername extracted username)
  @param(aGroupname extracted group of user)
  @param(aFilesize extracted filesize, -1 if parsed text is not a number)
  @param(aDatum extracted date and time with removed extra whitespaces)
  @param(aItem extracted dirname or filename) }
procedure ParseStatResponseLine(var aRespLine: String; out aDirMask, aUsername, aGroupname: String; out aFilesize: Int64; out aDatum, aItem: String);

{ Checks if given input is valid for a file (e.g. doesn't start with dot or is skipped globally)
  @param(aInput File or Dirname)
  @returns(@true if input is valid, @false otherwise.) }
function IsValidFilename(const aInput: String): Boolean;

{ Checks if given input is valid for a dir (e.g. doesn't start with dot or is skipped globally)
  @param(aInput File or Dirname)
  @returns(@true if input is valid, @false otherwise.) }
function IsValidDirname(const aInput: String): Boolean;

{ returns the value for NewdirMaxUnchanged initially stored in config to have a better performance and don't load the value everytime from file)
  @returns(@glNewdirMaxUnchanged) }
function GetNewdirMaxUnchangedValue(): integer;

{ returns the value for NewdirMaxEmpty initially stored in config to have a better performance and don't load the value everytime from file)
  @returns(@glNewdirMaxEmpty) }
function GetNewdirMaxEmptyValue(): integer;

{ returns the value for NewdirMaxCompleted initially stored in config to have a better performance and don't load the value everytime from file)
  @returns(@glNewdirMaxCompleted) }
function GetNewdirMaxCompletedValue(): integer;

{ returns the value for NewdirMaxCreated initially stored in config to have a better performance and don't load the value everytime from file)
  @returns(@glNewdirMaxCreated) }
function GetNewdirMaxCreatedValue(): integer;

{ returns the value for NewdirDirlistReadd initially stored in config to have a better performance and don't load the value everytime from file)
  @returns(@glNewdirDirlistReadd) }
function GetNewdirDirlistReaddValue(): integer;

{ Just a helper function to initialize @link(glSkiplistFilesRegex) and @link(glSkiplistDirsRegex) }
procedure DirlistHelperInit;

implementation

uses
  SysUtils, IdGlobal, RegExpr, globals, StrUtils, debugunit, configunit;

const
  section = 'dirlist.helpers';

var
  glSkiplistFilesRegex: String; //< global_skip_files regex from slftp.ini
  glSkiplistDirsRegex: String; //< global_skip_dirs regex from slftp.ini
  glNewdirMaxUnchanged: Integer;
  glNewdirMaxEmpty: Integer;
  glNewdirMaxCompleted: Integer;
  glNewdirMaxCreated: Integer;
  glNewdirDirlistReadd: Integer;


{$I common.inc}

function IsFtpRushScrewedUpFile(const aFilename, aFileExtension: String): Boolean;
var
  l: Integer;
begin
  Result := False;

  l := Length(aFilename);
  if l > Length(aFileExtension) + 6 then
  begin
    // for 3 chars in extension like .nfo, .rar, .mp3, .r02, etc
    if ( (aFilename[l-6] = '(') and (aFilename[l-4] = ')') and (aFilename[l-5] in ['0'..'9']) ) then
    begin
      Exit(True);
    end;

    // for 4 chars like .flac
    if ( (aFilename[l-7] = '(') and (aFilename[l-5] = ')') and (aFilename[l-6] in ['0'..'9']) ) then
    begin
      Exit(True);
    end;
  end;
end;

function ReleaseOnlyConsistsOfNFO(const aFullPath: String): Boolean;
var
  fTag: string;
begin
  Result := False;
  for fTag in SpecialDirsTags do
  begin
    if {$IFDEF UNICODE}ContainsText{$ELSE}AnsiContainsText{$ENDIF}(aFullPath, fTag) then
    begin
      debugunit.Debug(dpSpam, section, 'SpecialDir %s contains %s.', [aFullPath, fTag]);
      Result := true;
      Break;
    end;
  end;
end;

procedure ParseStatResponseLine(var aRespLine: String; out aDirMask, aUsername, aGroupname: String; out aFilesize: Int64; out aDatum, aItem: String);
begin
  // drwxrwxrwx   2 aq11     iND              3 Apr 19 23:14 Sample
  // -rw-r--r--   1 abc      Friends  100000000 Apr 19 23:14 baby.animals.s01e05.little.hunters.internal.2160p.uhdtv.h265-cbfm.r00
  aDirMask := Fetch(aRespLine, ' ', True, False);
  aRespLine := aRespLine.TrimLeft;
  Fetch(aRespLine, ' ', True, False); // No. of something
  aRespLine := aRespLine.TrimLeft;
  aUsername := Fetch(aRespLine, ' ', True, False);
  aRespLine := aRespLine.TrimLeft;
  aGroupname := Fetch(aRespLine, ' ', True, False);
  aRespLine := aRespLine.TrimLeft;
  aFilesize := StrToInt64Def(Fetch(aRespLine, ' ', True, False), -1);
  aDatum := Fetch(aRespLine, ' ', True, False);
  aRespLine := aRespLine.TrimLeft;
  aDatum := aDatum + ' ' + Fetch(aRespLine, ' ', True, False);
  aRespLine := aRespLine.TrimLeft;
  aDatum := aDatum + ' ' + Fetch(aRespLine, ' ', True, False); // date and time
  aItem := aRespLine.Trim; // file or dirname
end;

function IsValidFilename(const aInput: String): Boolean;
var
  fRegExpr: TRegExpr;
begin
  Result := False;

  // must be at least extension + something for filename like x.nfo or y.zip
  // releasenames also shouldn't be that short
  if (aInput.Length < 5) then
    Exit(False);

  if (aInput[1] = '.') then
    Exit(False);

  if glSkiplistFilesRegex <> '' then
  begin
    fRegExpr := TRegExpr.Create;
    try
      fRegExpr.ModifierI := True;
      fRegExpr.Expression := glSkiplistFilesRegex;

      if fRegExpr.Exec(aInput) then
        Exit(False);
    finally
      fRegExpr.Free;
    end;
  end;

  Result := True;
end;

function IsValidDirname(const aInput: String): Boolean;
var
  fRegExpr: TRegExpr;
begin
  Result := False;

  if (aInput[1] = '.') then
    Exit(False);

  if glSkiplistDirsRegex <> '' then
  begin
    fRegExpr := TRegExpr.Create;
    try
      fRegExpr.ModifierI := True;
      fRegExpr.Expression := glSkiplistDirsRegex;

      if fRegExpr.Exec(aInput) then
        Exit(False);
    finally
      fRegExpr.Free;
    end;
  end;

  Result := True;
end;

procedure DirlistHelperInit;
begin
  glSkiplistFilesRegex := config.ReadString('dirlist', 'global_skip', '^(tvmaze|imdb)\.nfo$|\-missing$|\-offline$|^\.|^file\_id\.diz$|\.htm$|\.html|\.bad$|\[IMDB\]\W+');
  glSkiplistDirsRegex := config.ReadString('dirlist', 'global_skip_dir', '\[IMDB\]\W+|\[TvMaze\]\W+');

  glNewdirMaxUnchanged := config.ReadInteger('taskrace', 'newdir_max_unchanged', 300);
  glNewdirMaxEmpty := config.ReadInteger('taskrace', 'newdir_max_empty', 300);
  glNewdirMaxCompleted := config.ReadInteger('taskrace', 'newdir_max_completed', 300);
  glNewdirMaxCreated := config.ReadInteger('taskrace', 'newdir_max_created', 600);
  glNewdirDirlistReadd := config.ReadInteger('taskrace', 'newdir_dirlist_readd', 100);
end;

function GetNewdirMaxUnchangedValue(): integer;
begin
  Result := glNewdirMaxUnchanged;
end;

function GetNewdirMaxEmptyValue(): integer;
begin
  Result := glNewdirMaxEmpty;
end;

function GetNewdirMaxCompletedValue(): integer;
begin
  Result := glNewdirMaxCompleted;
end;

function GetNewdirMaxCreatedValue(): integer;
begin
  Result := glNewdirMaxCreated;
end;

function GetNewdirDirlistReaddValue(): integer;
begin
  Result := glNewdirDirlistReadd;
end;

end.

