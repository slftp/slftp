unit dirlist.helpers;

interface

uses Generics.Collections, sitesunit;

type
  { Site Priority Levels for Dynamic Dirlist Performance }
  TSiteDirlistPriority = (spVeryLow, spLow, spNormal, spHigh, spVeryHigh);

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
      fIsSymlink: Boolean; //< @true if entry is a symlink
      fSymlinkTarget: String; //< target of symlink (if applicable)
    public
      property Filename: string read fFilename;
      property Username: string read fUsername;
      property Groupname: string read fGroupname;
      property DirMask: string read fDirMask;
      property Date: string read fDate;
      property Filesize: int64 read fFilesize;
      property IsSymlink: Boolean read fIsSymlink;
      property SymlinkTarget: string read fSymlinkTarget;
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
function GetNewdirDirlistReaddValue(): integer; overload;

{ returns the site-specific value for NewdirDirlistReadd or falls back to global default
  @param(sitename Name of the site to get the value for)
  @returns(Site-specific value or global default) }
function GetNewdirDirlistReaddValue(const sitename: String): integer; overload;

{ Returns performance-adjusted dirlist readd value based on current system load and site priority
  @param(sitename Name of the site to get the value for)
  @param(usePerformanceAdjustment Whether to apply performance-based adjustments)
  @returns(Performance-adjusted dirlist readd value in milliseconds) }
function GetPerformanceAdjustedDirlistReaddValue(const sitename: String; usePerformanceAdjustment: Boolean = True): integer;

{ Converts TSiteDirlistPriority enum to human-readable string
  @param(priority The priority level)
  @returns(String representation of priority) }
function GetSitePriorityText(priority: TSiteDirlistPriority): String;

{ Converts integer priority value (0-4) to TSiteDirlistPriority enum
  @param(priorityValue Integer value 0-4)
  @returns(TSiteDirlistPriority enum value) }
function IntToDirlistPriority(priorityValue: Integer): TSiteDirlistPriority;

function ParseStatResponse(s: String): TObjectList<TParsedDirlistEntry>;

{ Just a helper function to initialize @link(glSkiplistFilesRegex) and @link(glSkiplistDirsRegex) }
procedure DirlistHelperInit;

{ Frees the thread vars of the current thread (call this when a thread terminates). }
procedure CleanupDirlistThreadVars;

implementation

uses
  SysUtils, IdGlobal, RegExpr, globals, StrUtils, debugunit, configunit, mystrings, loadmonitorunit, Math;

const
  section = 'dirlist.helpers';

  // Performance Level Constants
  MIN_DIRLIST_INTERVAL = 10;     // 10ms absolute minimum
  MAX_DIRLIST_INTERVAL = 1000;   // 1000ms absolute maximum
  DEFAULT_PERFORMANCE_LEVEL = 5; // Balanced default

  // Dynamic Dirlist Performance Matrix (10ms - 1000ms range)
  // Performance Level → Site Priority → Interval (milliseconds)
  DIRLIST_PERFORMANCE_MATRIX: array[1..9] of record
    VeryLow, Low, Normal, High, VeryHigh: Integer;
  end = (
    // Performance Level 1 (CPU overload - slowest)
    (VeryLow: 1000; Low: 1000; Normal: 1000; High: 600; VeryHigh: 300),
    // Performance Level 2 (very high load)
    (VeryLow: 1000; Low: 1000; Normal: 900; High: 500; VeryHigh: 220),
    // Performance Level 3 (high load)
    (VeryLow: 1000; Low: 1000; Normal: 800; High: 420; VeryHigh: 170),
    // Performance Level 4 (medium-high load)
    (VeryLow: 1000; Low: 950; Normal: 700; High: 350; VeryHigh: 130),
    // Performance Level 5 (balanced - standard)
    (VeryLow: 1000; Low: 900; Normal: 620; High: 290; VeryHigh: 95),
    // Performance Level 6 (low load)
    (VeryLow: 1000; Low: 850; Normal: 560; High: 240; VeryHigh: 70),
    // Performance Level 7 (very low load)
    (VeryLow: 1000; Low: 820; Normal: 530; High: 200; VeryHigh: 50),
    // Performance Level 8 (minimal load)
    (VeryLow: 1000; Low: 810; Normal: 515; High: 170; VeryHigh: 35),
    // Performance Level 9 (optimal performance - fastest)
    (VeryLow: 1000; Low: 800; Normal: 500; High: 150; VeryHigh: 25)
  );

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

function ParseStatResponse(s: String): TObjectList<TParsedDirListEntry>;
var
  fLineToParse: string;
  fParsedDirlistEntries: TObjectList<TParsedDirListEntry>;
  fDirMask, fUsername, fGroupname, fDatum, fFilename: String;
  fFilesize: Int64;
  fParsedDirlistEntry: TParsedDirlistEntry;
  fIsSymlink: Boolean;
  fSymlinkTarget: String;
  fArrowPos: Integer;
begin
  fParsedDirlistEntries := TObjectList<TParsedDirListEntry>.Create(True);
  try
    while (True) do
    begin
      fLineToParse := Trim(GetFirstLineFromTextViaNewlineIndicators(s));
      // tmp contains a single line:
      // drwxrwxrwx   2 nete     Death_Me     4096 Jan 29 05:05 Whisteria_Cottage-Heathen-RERIP-2009-pLAN9
      // lrwxrwxrwx   1 user     group        10 Jan 01 00:00 linkname -> target

      if fLineToParse = '' then break;
      if (Length(fLineToParse) > 11) then
      begin
        if ((fLineToParse[1] <> 'd') and (fLineToParse[1] <> '-') and (fLineToParse[1] <> 'l') and (fLineToParse[11] = ' ')) then
          continue;

        fIsSymlink := (fLineToParse[1] = 'l');
        fSymlinkTarget := '';

        if fIsSymlink then
        begin
          fArrowPos := Pos(' -> ', fLineToParse);
          if fArrowPos > 0 then
          begin
            fSymlinkTarget := Trim(Copy(fLineToParse, fArrowPos + 4, MaxInt));
            fLineToParse := Copy(fLineToParse, 1, fArrowPos - 1);
          end;
        end;

        ParseStatResponseLine(fLineToParse, fDirMask, fUsername, fGroupname, fFilesize, fDatum, fFilename);
        fParsedDirlistEntry := TParsedDirlistEntry.Create;
        fParsedDirlistEntry.fDirMask := fDirMask;
        fParsedDirlistEntry.fUsername := fUsername;
        fParsedDirlistEntry.fGroupname := fGroupname;
        fParsedDirlistEntry.fFilesize := fFilesize;
        fParsedDirlistEntry.fDate := fDatum;
        fParsedDirlistEntry.FFilename := fFilename;
        fParsedDirlistEntry.fIsSymlink := fIsSymlink;
        fParsedDirlistEntry.fSymlinkTarget := fSymlinkTarget;
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
  glSkiplistFilesRegex := config.ReadString('dirlist', 'global_skip_files', '^(tvmaze|imdb)\.nfo$|\-missing$|\-offline$|^\.|^file\_id\.diz$|\.htm$|\.html|\.bad$|\[IMDB\]\W+');
  glSkiplistDirsRegex := config.ReadString('dirlist', 'global_skip_dirs', '\[IMDB\]\W+|\[TvMaze\]\W+');

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

function GetNewdirDirlistReaddValue(const sitename: String): integer;
var
  s: TSite;
begin
  if sitename <> '' then
  begin
    s := FindSiteByName('', sitename);
    if s <> nil then
    begin
      Result := s.NewdirDirlistReadd;
      if Result > 0 then
        exit;
    end;
  end;
  
  Result := glNewdirDirlistReadd;
end;

function GetPerformanceAdjustedDirlistReaddValue(const sitename: String; usePerformanceAdjustment: Boolean = True): integer;
var
  baseValue: Integer;
  performanceLevel: Integer;
  sitePriority: TSiteDirlistPriority;
  site: TSite;
  perfEnabled: Boolean;
begin
  // Get base value (current static system)
  baseValue := GetNewdirDirlistReaddValue(sitename);

  // Check if performance adjustment is globally enabled
  perfEnabled := config.ReadBool('dirlist_performance', 'enabled', True);

  if not usePerformanceAdjustment or not perfEnabled then
  begin
    Result := baseValue;
    Exit;
  end;

  try
    // Check if LoadMonitor is available and running
    if not IsLoadMonitorAvailable then
    begin
      Result := baseValue;
      Exit;
    end;

    // Get current performance level from LoadMonitor
    performanceLevel := GlLoadMonitor.CurrentPerformanceLevel;

    // Validate performance level
    if (performanceLevel < 1) or (performanceLevel > 9) then
    begin
      Debug(dpError, section, 'Invalid performance level %d, using default', [performanceLevel]);
      performanceLevel := DEFAULT_PERFORMANCE_LEVEL;
    end;

    // Get site priority (default to normal if site not found or no priority set)
    sitePriority := spNormal; // Default
    site := FindSiteByName('', sitename);

    // Thread-safe access: verify site still valid before accessing properties
    if (site <> nil) then
    begin
      try
        // Check if performance adjustment is enabled for this site
        if not site.PerformanceAdjustedDirlist then
        begin
          Result := baseValue; // Use static value if performance adjustment disabled
          Exit;
        end;

        // Convert integer priority to enum (0=VeryLow, 1=Low, 2=Normal, 3=High, 4=VeryHigh)
        sitePriority := IntToDirlistPriority(site.DirlistPriority);
      except
        on E: Exception do
        begin
          // Site object may have been freed, use default
          Debug(dpError, section, 'Error accessing site %s: %s, using default priority',
            [sitename, E.Message]);
          sitePriority := spNormal;
        end;
      end;
    end;

    // Matrix lookup for performance-adjusted value
    case sitePriority of
      spVeryLow:  Result := DIRLIST_PERFORMANCE_MATRIX[performanceLevel].VeryLow;
      spLow:      Result := DIRLIST_PERFORMANCE_MATRIX[performanceLevel].Low;
      spNormal:   Result := DIRLIST_PERFORMANCE_MATRIX[performanceLevel].Normal;
      spHigh:     Result := DIRLIST_PERFORMANCE_MATRIX[performanceLevel].High;
      spVeryHigh: Result := DIRLIST_PERFORMANCE_MATRIX[performanceLevel].VeryHigh;
    else
      Result := baseValue; // Fallback on unknown priority
    end;

    // Apply safety bounds
    Result := Max(MIN_DIRLIST_INTERVAL, Min(MAX_DIRLIST_INTERVAL, Result));

    // Debug output (controlled via !logverbosity spam)
    Debug(dpSpam, section,
      'Performance adjustment: Site=%s, PerfLevel=%d, Priority=%s, BaseValue=%dms, AdjustedValue=%dms',
      [sitename, performanceLevel, GetSitePriorityText(sitePriority), baseValue, Result]);

  except
    on E: Exception do
    begin
      Debug(dpError, section, 'Error in performance adjustment for %s: %s, using base value %dms',
        [sitename, E.Message, baseValue]);
      Result := baseValue; // Fallback to static value on any error
    end;
  end;
end;

function GetSitePriorityText(priority: TSiteDirlistPriority): String;
begin
  case priority of
    spVeryLow:  Result := 'VeryLow';
    spLow:      Result := 'Low';
    spNormal:   Result := 'Normal';
    spHigh:     Result := 'High';
    spVeryHigh: Result := 'VeryHigh';
  else
    Result := 'Unknown';
  end;
end;

function IntToDirlistPriority(priorityValue: Integer): TSiteDirlistPriority;
begin
  case priorityValue of
    0: Result := spVeryLow;
    1: Result := spLow;
    2: Result := spNormal;
    3: Result := spHigh;
    4: Result := spVeryHigh;
  else
    Result := spNormal; // Default for invalid values
  end;
end;

procedure CleanupDirlistThreadVars;
begin
  if glSkiplistFilesRegexInstance <> nil then
    FreeAndNil(glSkiplistFilesRegexInstance);
  if glSkiplistDirsRegexInstance <> nil then
    FreeAndNil(glSkiplistDirsRegexInstance);
end;

end.
