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
function GetNewdirDirlistReaddValue(): integer;

{ Calculates the load-adjusted dirlist readd value from base value, load, and threshold.
  If aLoad1 < aThreshold: returns aBaseValue unchanged.
  If aLoad1 >= aThreshold: scales as aBaseValue * (aLoad1 - aThreshold + 2)
  Example: base=10, threshold=4.0, load=4.0 -> 20; load=5.0 -> 30; load=6.0 -> 40 }
function CalculateLoadAdjustedDirlistReadd(const aBaseValue: Integer; const aLoad1, aThreshold: Double): Integer;

{ Calculates the load-adjusted dirlist readd value using explicit step values.
  aSteps[0] is used for load at threshold, aSteps[1] for threshold+1, etc.
  If load exceeds the highest step, the last step value is used.
  If aSteps is empty, falls back to CalculateLoadAdjustedDirlistReadd formula. }
function CalculateLoadAdjustedDirlistReaddWithSteps(const aBaseValue: Integer; const aLoad1, aThreshold: Double; const aSteps: array of Integer): Integer;

{ Reads current system load and applies load adjustment to the given base value.
  Uses glNewdirDirlistReaddLoadEnabled and glNewdirDirlistReaddLoadThreshold globals.
  If newdir_dirlist_readd_load_steps is configured, uses explicit steps instead of formula. }
function GetNewdirDirlistReaddLoadAdjustedValue(const aBaseValue: Integer): Integer;

{ Returns whether load-based dirlist readd adjustment is enabled }
function IsNewdirDirlistReaddLoadEnabled(): Boolean;

function ParseStatResponse(s: String): TObjectList<TParsedDirlistEntry>;

{ Just a helper function to initialize @link(glSkiplistFilesRegex) and @link(glSkiplistDirsRegex) }
procedure DirlistHelperInit;

{ Frees the thread vars of the current thread (call this when a thread terminates). }
procedure CleanupDirlistThreadVars;

implementation

uses
  SysUtils, IdGlobal, RegExpr, globals, StrUtils, debugunit, configunit, mystrings,
  mormot.core.os, DateUtils, slcriticalsection2, Classes, Math;

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
  glNewdirDirlistReaddLoadEnabled: Boolean;
  glNewdirDirlistReaddLoadThreshold: Double;
  glNewdirDirlistReaddLoadSteps: array of Integer;
  glLastLoggedLoadAdjustedValue: Integer = -1;
  glLastLoggedLoadAdjustedTime: TDateTime = 0;
  glLoadAdjustedLogLock: TSlCriticalSection2;

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
  fArrowPos: Integer;
begin
  fParsedDirlistEntries := TObjectList<TParsedDirListEntry>.Create(True);
  try
    while (True) do
    begin
      fLineToParse := Trim(GetFirstLineFromTextViaNewlineIndicators(s));
      // tmp contains a single line:
      // drwxrwxrwx   2 nete     Death_Me     4096 Jan 29 05:05 Whisteria_Cottage-Heathen-RERIP-2009-pLAN9
      // lrwxrwxrwx   1 user     group           0 Jan 29 05:05 linkname -> targetname

      if fLineToParse = '' then break;
      if (Length(fLineToParse) > 11) then
      begin
        if ((fLineToParse[1] <> 'd') and (fLineToParse[1] <> '-') and (fLineToParse[1] <> 'l') and (fLineToParse[11] = ' ')) then
          continue;
        ParseStatResponseLine(fLineToParse, fDirMask, fUsername, fGroupname, fFilesize, fDatum, fFilename);
        fParsedDirlistEntry := TParsedDirlistEntry.Create;
        fParsedDirlistEntry.fDirMask := fDirMask;
        fParsedDirlistEntry.fUsername := fUsername;
        fParsedDirlistEntry.fGroupname := fGroupname;
        fParsedDirlistEntry.fFilesize := fFilesize;
        fParsedDirlistEntry.fDate := fDatum;
        fParsedDirlistEntry.fIsSymlink := (fDirMask <> '') and (fDirMask[1] = 'l');
        fParsedDirlistEntry.fSymlinkTarget := '';
        if fParsedDirlistEntry.fIsSymlink then
        begin
          // Extract symlink target from "name -> target" format
          fArrowPos := Pos(' -> ', fFilename);
          if fArrowPos > 0 then
          begin
            fParsedDirlistEntry.fSymlinkTarget := Trim(Copy(fFilename, fArrowPos + 4, MaxInt));
            fFilename := Trim(Copy(fFilename, 1, fArrowPos - 1));
          end;
        end;
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
var
  fStepsStr: String;
  fParts: TStringList;
  i: Integer;
begin
  glSkiplistFilesRegex := config.ReadString('dirlist', 'global_skip_files', '^(tvmaze|imdb)\.nfo$|\-missing$|\-offline$|^\.|^file\_id\.diz$|\.htm$|\.html|\.bad$|\[IMDB\]\W+');
  glSkiplistDirsRegex := config.ReadString('dirlist', 'global_skip_dirs', '\[IMDB\]\W+|\[TvMaze\]\W+');

  glNewdirMaxUnchanged := config.ReadInteger('taskrace', 'newdir_max_unchanged', 300);
  glNewdirMaxEmpty := config.ReadInteger('taskrace', 'newdir_max_empty', 300);
  glNewdirMaxCompleted := config.ReadInteger('taskrace', 'newdir_max_completed', 300);
  glNewdirMaxCreated := config.ReadInteger('taskrace', 'newdir_max_created', 600);
  glNewdirDirlistReadd := config.ReadInteger('taskrace', 'newdir_dirlist_readd', 100);
  glNewdirDirlistReaddLoadEnabled := config.ReadBool('taskrace', 'newdir_dirlist_readd_load_enabled', False);
  glNewdirDirlistReaddLoadThreshold := config.ReadFloat('taskrace', 'newdir_dirlist_readd_load_threshold', CpuThreads);
  glLoadAdjustedLogLock := TSlCriticalSection2.Create('LoadAdjustedLogLock');

  // Parse optional explicit load steps (comma-separated list)
  // Example: 20,30,50,100 means: at threshold -> 20ms, +1 -> 30ms, +2 -> 50ms, +3+ -> 100ms
  fStepsStr := config.ReadString('taskrace', 'newdir_dirlist_readd_load_steps', '');
  if fStepsStr <> '' then
  begin
    fParts := TStringList.Create;
    try
      ExtractStrings([','], [], PChar(fStepsStr), fParts);
      SetLength(glNewdirDirlistReaddLoadSteps, fParts.Count);
      for i := 0 to fParts.Count - 1 do
        glNewdirDirlistReaddLoadSteps[i] := StrToIntDef(Trim(fParts[i]), 0);
    finally
      fParts.Free;
    end;
  end;
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

function IsNewdirDirlistReaddLoadEnabled(): Boolean;
begin
  Result := glNewdirDirlistReaddLoadEnabled;
end;

function CalculateLoadAdjustedDirlistReadd(const aBaseValue: Integer; const aLoad1, aThreshold: Double): Integer;
var
  fMultiplier: Double;
begin
  if aLoad1 < aThreshold then
    Result := aBaseValue
  else
  begin
    fMultiplier := aLoad1 - aThreshold + 2;
    Result := Round(aBaseValue * fMultiplier);
  end;
end;

function CalculateLoadAdjustedDirlistReaddWithSteps(const aBaseValue: Integer; const aLoad1, aThreshold: Double; const aSteps: array of Integer): Integer;
var
  fStepIndex: Integer;
begin
  if Length(aSteps) = 0 then
  begin
    Result := CalculateLoadAdjustedDirlistReadd(aBaseValue, aLoad1, aThreshold);
    Exit;
  end;

  fStepIndex := Floor(aLoad1 - aThreshold);
  if fStepIndex < 0 then
    Result := aBaseValue
  else if fStepIndex >= Length(aSteps) then
    Result := aSteps[High(aSteps)]
  else
    Result := aSteps[fStepIndex];
end;

function GetNewdirDirlistReaddLoadAdjustedValue(const aBaseValue: Integer): Integer;
var
  f: TextFile;
  fLine: String;
  fLoad1: Double;
  fFormatSettings: TFormatSettings;
begin
  Result := aBaseValue;

  if not glNewdirDirlistReaddLoadEnabled then
    Exit;

  {$I-}
  AssignFile(f, '/proc/loadavg');
  Reset(f);
  if IOResult <> 0 then
  begin
    {$I+}
    Exit;
  end;

  try
    ReadLn(f, fLine);
    if IOResult <> 0 then
      Exit;

    fLine := Trim(fLine);
    if fLine = '' then
      Exit;

    // Extract first word (1min load average)
    fLine := Copy(fLine, 1, Pos(' ', fLine) - 1);
    if fLine = '' then
      Exit;

    {$IFDEF FPC}
      fFormatSettings := DefaultFormatSettings;
    {$ELSE}
      fFormatSettings := TFormatSettings.Create;
    {$ENDIF}
    fFormatSettings.DecimalSeparator := '.';

    if not TryStrToFloat(fLine, fLoad1, fFormatSettings) then
      Exit;

    // Round load to 0.5 steps to prevent flapping on minor fluctuations
    fLoad1 := Round(fLoad1 * 2) / 2;

    if Length(glNewdirDirlistReaddLoadSteps) > 0 then
      Result := CalculateLoadAdjustedDirlistReaddWithSteps(aBaseValue, fLoad1, glNewdirDirlistReaddLoadThreshold, glNewdirDirlistReaddLoadSteps)
    else
      Result := CalculateLoadAdjustedDirlistReadd(aBaseValue, fLoad1, glNewdirDirlistReaddLoadThreshold);

    glLoadAdjustedLogLock.Enter('GetNewdirDirlistReaddLoadAdjustedValue');
    try
      if (Result <> aBaseValue) and ((Result <> glLastLoggedLoadAdjustedValue) or (MilliSecondsBetween(Now, glLastLoggedLoadAdjustedTime) >= 5000)) then
      begin
        Debug(dpMessage, section, Format('Load adjustment triggered: load=%.2f threshold=%.2f base=%dms -> adjusted=%dms',
          [fLoad1, glNewdirDirlistReaddLoadThreshold, aBaseValue, Result]));
        glLastLoggedLoadAdjustedValue := Result;
        glLastLoggedLoadAdjustedTime := Now;
      end
      else if (Result = aBaseValue) and (glLastLoggedLoadAdjustedValue <> aBaseValue) and
              (glLastLoggedLoadAdjustedTime > 0) and
              (MilliSecondsBetween(Now, glLastLoggedLoadAdjustedTime) >= 5000) then
      begin
        Debug(dpMessage, section, Format('Load adjustment cleared: load=%.2f threshold=%.2f back to base=%dms',
          [fLoad1, glNewdirDirlistReaddLoadThreshold, aBaseValue]));
        glLastLoggedLoadAdjustedValue := Result;
        glLastLoggedLoadAdjustedTime := Now;
      end;
    finally
      glLoadAdjustedLogLock.Leave;
    end;
  finally
    CloseFile(f);
  end;
  {$I+}
end;

procedure CleanupDirlistThreadVars;
begin
  if glSkiplistFilesRegexInstance <> nil then
    FreeAndNil(glSkiplistFilesRegexInstance);
  if glSkiplistDirsRegexInstance <> nil then
    FreeAndNil(glSkiplistDirsRegexInstance);
end;

end.
