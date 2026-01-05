unit dirlist.helpers;

interface

uses Generics.Collections;

type
  { @abstract(Information for a specific file which is parsed from a TDirlist) }
  {fDirMask, fUsername, fGroupname, fFilesize, fDatum, fFilename}
  TParsedDirListEntry = class
    private
      fFilename: String; //< lowercased filename
      fUsername: String; //< name of user who sent this file
      fGroupname: String; //< name of group the @link(FUsername) is associated with
      fDirMask: String; //< Indicates what kind of Directory Mask the current dir is
      fFilesize: int64; //Current size of the file
      fDate: String; //Current timestamp of the file
    public
      property Filename: string read fFilename;
      property Username: string read fUsername;
      property Groupname: string read fGroupname;
      property DirMask: string read fDirMask;
      property Date: string read fDate;
      property Filesize: int64 read fFilesize;
  end;

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
procedure ParseStatResponseLine(const aRespLine: String; out aDirMask, aUsername, aGroupname: String; out aFilesize: Int64; out aDatum, aItem: String);

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

function ParseStatResponse(s: String): TObjectList<TParsedDirlistEntry>;

{ Just a helper function to initialize @link(glSkiplistFilesRegex) and @link(glSkiplistDirsRegex) }
procedure DirlistHelperInit;

{ Frees the thread vars of the current thread (call this when a thread terminates). }
procedure CleanupDirlistThreadVars;

implementation

uses
  SysUtils, IdGlobal, RegExpr, globals, StrUtils, debugunit, configunit, mystrings,
  mormot.core.base, mormot.core.text;

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

threadvar
  glSkiplistFilesRegexInstance: TRegExpr;
  glSkiplistDirsRegexInstance: TRegExpr;

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
    if ( (aFilename[l-6] = '(') and (aFilename[l-4] = ')') and CharInSet(aFilename[l-5], ['0'..'9']) ) then
    begin
      Exit(True);
    end;

    // for 4 chars like .flac
    if ( (aFilename[l-7] = '(') and (aFilename[l-5] = ')') and CharInSet(aFilename[l-6], ['0'..'9']) ) then
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

procedure ParseStatResponseLine(const aRespLine: String; out aDirMask, aUsername, aGroupname: String; out aFilesize: Int64; out aDatum, aItem: String);
var
  P: PUtf8Char;
  fUtf8Line: RawUtf8;
  fDate1, fDate2, fDate3: RawUtf8;

  procedure SkipSpaces;
  begin
    while (P <> nil) and (P^ = ' ') do
      Inc(P);
  end;

begin
  // drwxrwxrwx   2 aq11     iND              3 Apr 19 23:14 Sample
  // -rw-r--r--   1 abc      Friends  100000000 Apr 19 23:14 baby.animals.s01e05.little.hunters.internal.2160p.uhdtv.h265-cbfm.r00
  fUtf8Line := UTF8String(aRespLine);
  P := Pointer(fUtf8Line);

  aDirMask := string(GetNextItem(P, ' '));
  SkipSpaces;
  GetNextItem(P, ' '); // Skip "No. of links"
  SkipSpaces;
  aUsername := string(GetNextItem(P, ' '));
  SkipSpaces;
  aGroupname := string(GetNextItem(P, ' '));
  SkipSpaces;
  aFilesize := GetNextItemInt64(P, ' ');
  SkipSpaces;

  fDate1 := GetNextItem(P, ' ');
  SkipSpaces;
  fDate2 := GetNextItem(P, ' ');
  SkipSpaces;
  fDate3 := GetNextItem(P, ' ');
  aDatum := string(fDate1 + ' ' + fDate2 + ' ' + fDate3);

  // The rest of the line is the filename/item
  if P <> nil then
    aItem := string(Trim(P))
  else
    aItem := '';
end;

function GetSkiplistDirsRegexInstance: TRegExpr;
begin
  if glSkiplistDirsRegexInstance = nil then
  begin
    glSkiplistDirsRegexInstance := TRegExpr.Create;
    glSkiplistDirsRegexInstance.ModifierI := True;
    glSkiplistDirsRegexInstance.Expression := glSkiplistDirsRegex;
  end;

  Result := glSkiplistDirsRegexInstance;
end;

function GetSkiplistFilesRegexInstance: TRegExpr;
begin
  if glSkiplistFilesRegexInstance = nil then
  begin
    glSkiplistFilesRegexInstance := TRegExpr.Create;
    glSkiplistFilesRegexInstance.ModifierI := True;
    glSkiplistFilesRegexInstance.Expression := glSkiplistFilesRegex;
  end;

  Result := glSkiplistFilesRegexInstance;
end;

function IsValidFilename(const aInput: String): Boolean;
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
    if GetSkiplistFilesRegexInstance.Exec(aInput) then
      Exit(False);
  end;

  Result := True;
end;

function IsValidDirname(const aInput: String): Boolean;
begin
  Result := False;

  if (aInput[1] = '.') then
    Exit(False);

  if glSkiplistDirsRegex <> '' then
  begin
    if GetSkiplistDirsRegexInstance.Exec(aInput) then
      Exit(False);
  end;

  Result := True;
end;

function ParseStatResponse(s: String): TObjectList<TParsedDirlistEntry>;
var
  fLineToParse: RawUtf8;
  fParsedDirlistEntries: TObjectList<TParsedDirlistEntry>;
  fDirMask, fUsername, fGroupname, fDatum, fFilename: String;
  fFilesize: Int64;
  fParsedDirlistEntry: TParsedDirlistEntry;
  P: PUtf8Char;
  fUtf8Input: RawUtf8;
begin
  fParsedDirlistEntries := TObjectList<TParsedDirlistEntry>.Create(True);
  try
    fUtf8Input := UTF8String(s);
    P := Pointer(fUtf8Input);
    while P <> nil do
    begin
      fLineToParse := Trim(GetNextItem(P, #10));
      // tmp contains a single line:
      // drwxrwxrwx   2 nete     Death_Me     4096 Jan 29 05:05 Whisteria_Cottage-Heathen-RERIP-2009-pLAN9

      if fLineToParse = '' then continue;
      if (Length(fLineToParse) > 11) then
      begin
        // Only process lines that match UNIX permission format: 'd' or '-' at position 1, space at position 11
        // This filters out FTP status messages like "213- status of -l:", "total 12345", "213 End of Status"
        if ((fLineToParse[1] <> 'd') and (fLineToParse[1] <> '-')) or (fLineToParse[11] <> ' ') then
          continue;
        ParseStatResponseLine(string(fLineToParse), fDirMask, fUsername, fGroupname, fFilesize, fDatum, fFilename);
        fParsedDirlistEntry := TParsedDirlistEntry.Create;
        fParsedDirlistEntry.fDirMask := fDirMask;
        fParsedDirlistEntry.fUsername := fUsername;
        fParsedDirlistEntry.fGroupname := fGroupname;
        fParsedDirlistEntry.fFilesize := fFilesize;
        fParsedDirlistEntry.fDate := fDatum;
        fParsedDirlistEntry.FFilename := fFilename;
        fParsedDirlistEntries.Add(fParsedDirlistEntry);
      end;
    end;
  except
    fParsedDirlistEntries.Free;
    raise;
  end;

  Result := fParsedDirlistEntries;
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

procedure CleanupDirlistThreadVars;
begin
  if glSkiplistFilesRegexInstance <> nil then
    FreeAndNil(glSkiplistFilesRegexInstance);
  if glSkiplistDirsRegexInstance <> nil then
    FreeAndNil(glSkiplistDirsRegexInstance);
end;

end.
