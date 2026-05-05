unit dirlist;

interface

uses Classes, Contnrs, SyncObjs, slcriticalsection2, skiplists, globals, Generics.Collections, IniFiles, sfv, tags;

type
  {
  @value(dlSFVUnknown unknown SFV status (default))
  @value(dlSFVNoNeed SFV file not needed)
  @value(dlSFVFound SFV file found and needed)
  @value(dlSFVNotFound SFV file not found but needed)
  }
  TdlSFV = (dlSFVUnknown, dlSFVNoNeed, dlSFVFound, dlSFVNotFound);
  {
  @value(NotComplete release is still incomplete)
  @value(FromIrc COMPLETE announce fetched from IRC)
  @value(FromFtpd COMPLETE announce fetched from FTP dirlist)
  @value(FromFtpdAndIrc COMPLETE announce fetched from IRC and FTP dirlist)
  }
  TCompleteInfo = (NotComplete = 0, FromIrc = 4, FromFtpd = 5, FromFtpdAndIrc = 9);

  TDirlist = class;

  { @abstract(Information for a specific file (also dir?) from a TDirlist) }
  TDirListEntry = class
  private
    FFilenameLowerCase: String; //< lowercased filename
    FExtension: String; //< lowercased file extension, includes the '.' prefix - such as '.nfo'
    FUsername: String; //< name of user who sent this file
    FRacedByMe: Boolean; //< @true if we send this file to the site, @false otherwise.
    FGroupname: String; //< name of group the @link(FUsername) is associated with
    FDirectory: Boolean; //< @true if current dir is a directory
    FDirType: TDirType; //< Indicates what kind of Directory the current dir is
    FIsOnSite: Boolean; //< @true if this entry is available on the site
    FWasOnSite: Boolean; //< Helper flag to remember if a file has been on the site before ParseDirlist reset the IsOnSite flag
    FIsBeingUploaded: Boolean;  //< @true if this entry is a file currently being uploaded TODO: flag is only valid on glftpd, for all other ftpds it'll always be false
    FSkipListAlreadyProcessed: Boolean;  //< @true if the skiplist process has already been applied to this dirlistentry, @false otherwise.
    { Contains the index of the file type in the skiplist. For files and directories. For example when you have this in
      your skiplist: "alloweddirs=_ROOT_:Sample,Sub,Subs,Proof" then the entry for directory "Sample" will have a value 0 and the entry for "Subs" will have a value 2
      in this field. This is only needed for sorting the dirlist. Not really important. }
    FSkipListAllowedFileIndex: integer;
    { Tries to identify the dirtype from subdirectory name by different regexes
      @returns(Recognized DirType see @link(globals.TDirType), @link(globals.TDirType.IsUnknown) otherwise) }
    FIsNFO: Boolean; //< True when this is a NFO file.
    FIsSFV: Boolean; //< True when this is a SFV file.
    FIsSymlink: Boolean; //< @true if entry is a symlink
    FSymlinkTarget: String; //< target of symlink (if applicable)
    function RecognizeDirTypeFromDirname(const aDirname: String): TDirType;
  public
    dirlist: TDirList;
    justadded: Boolean;
    error: Boolean; //< @true if file cannot be send, will be skipped then, @false otherwise.
    subdirlist: TDirList;
    filename: String; //< filename
    filesize: Int64; //< filesize
    skiplisted: Boolean; //< @true if the this entity is skiplisted. It will not be transferred.
    cdno: Integer;
    timestamp: TDateTime; //< parsed value of date and time from dirlisting string (via @link(TDirlist.Timestamp) function)
    FSizeChanged: boolean; //< Set to true once ParseDirlist finds a changed file size, set to false once the changed file size has been registered at the release wide store.

    procedure CalcCDNumber;
    constructor Create(const filename: String; dirlist: TDirList; const aIsDirectory: boolean); overload;
    destructor Destroy; override;
    function DirTypeAsString: String;
    procedure RegenerateSkiplist;



    { Checks if the @link(Extension) matches any of the @link(AsciiFiletypes) values
      as their filesize could differ e.g. due to different line endings.
      @returns(@true if file is an ASCII type, @false otherwise.) }
    function IsAsciiFiletype: Boolean;

    property FilenameLowerCased: String read FFilenameLowerCase;
    property Extension: String read FExtension;
    property Username: String read FUsername;
    property RacedByMe: Boolean read FRacedByMe write FRacedByMe;
    property Directory: Boolean read FDirectory;
    property DirType: TDirType read FDirType;
    property IsOnSite: Boolean read FIsOnSite write FIsOnSite;
    property IsBeingUploaded: Boolean read FIsBeingUploaded write FIsBeingUploaded;
    property IsNFO: Boolean read FIsNFO;
    property IsSFV: Boolean read FIsSFV;
    property IsSymlink: Boolean read FIsSymlink;
    property SymlinkTarget: string read FSymlinkTarget;
  end;

  { @abstract(Information for a single release dirlist) }
  TDirList = class
  private
    FLastChanged: TDateTime;
    FLastUpdated: TDateTime;
    allcdshere: Boolean;
    skiplist: TSkipList;
    FPazoSFV: TPazoSFV;
    s: String;
    FIsValidFileCache: TDictionary<string, boolean>; //< cache for results of IsValidFilename
    FIsValidDirCache: TDictionary<string, boolean>; //< cache for results of IsValidDirname
    FMatchFileCache: TDictionary<string, integer>; //< cache for results of MatchFile
    FMatchFileDirectoryCache: TDictionary<string, integer>; //< cache for results of MatchFile (directory)
    FContainsNFOOnlyDirTag: boolean; //true, if the dir contains a special tag indicating the rls can be complete only containing the NFO (dirfix, nfofix, ...)


    FCompleteInfo: TCompleteInfo; //< value of @link(TCompleteInfo) status and where we got it
    FCachedCompleteResult: Boolean; //< @true if @link(Complete) has been called and determined the TDirlist as complete, @false otherwise.

    // TODO: sometimes its without a '/' at the end, check if this is correct and what happens if we add it by default (could remove extra code in RegenerateSkiplist then)
    FCompleteDirTag: String; //< complete dir found on ftpd while dirlisting
    FCompleteDirTagLastChecked: String; //< contains the last complete tag that was checked  (cache)
    FCompleteDirTagLastResult: TTagCompleteType; // contains the result of the last complete tag that was checked (cache)

    FStartedTime: TDateTime; //< time when the first @link(TDirlistEntry) was created
    FCompletedTime: TDateTime; //< time when the TDirlit was recognized as complete
    FFullPath: String; //< path of section and releasename (e.g. /MP3-today/Armin_van_Buuren_-_A_State_of_Trance_921__Incl_Ruben_de_Ronde_Guestmix-SAT-07-04-2019-TALiON/)
    FIsSpeedTest: Boolean; //< @true if task is a speedtest, @false otherwise
    FIsAutoIndex: Boolean; //< @true if task was created by autoindexer, @false otherwise
    FIsFromIrc: Boolean; //< @true if task was created by an IRC command (e.g. !dirlist), @false otherwise
    FDirlistGaveUp: Boolean; //< @true if dirlisting has been given up for this dir
    FHasNFO: Boolean; //< True when dirlist has found a valid NFO file
    FHasSFV: Boolean; //< True when dirlist has found a valid SFV file
    FMultiCD: Boolean; //< True when this dirlist contains multiple disks (e.g. CD1, CD2, Disk1, Disk2, ...)

    { Checks if there is a @link(CompleteDirTag) and then calls @link(tags.TagComplete) to check if it results in COMPLETE
      @returns(@true if determined as COMPLETE, @false otherwise) }
    function CompleteByTag: Boolean;

    procedure SetLastChanged(const value: TDateTime);

    procedure SetFullPath(const aFullPath: string);
    { Calculates the FMultiCD field based on the contained dirlist entries. }
    procedure CalculateMultiCD;
    class function Timestamp(ts: String): TDateTime;
  public
    dirlist_lock: TslRWLock;
    dirlistadded: Boolean;
    site_name: String; //< sitename
    error: Boolean;
    need_mkdir: Boolean; //< @true if MKDIR'ing is still needed (default), @false otherwise.
    sfv_status: TdlSFV;
    biggestcd: Integer;
    parent: TDirListEntry;
    entries: TObjectDictionary<string, TDirListEntry>; //< contains the @link(TDirlistEntry) objects for the dirlist
    skipped: TStringList;
    dependency_mkdir: String;

    procedure Clear;
    constructor Create(const site_name: String; parentdir: TDirListEntry; skiplist: TSkipList; const aPazoSFV: TPazoSFV; SpeedTest: Boolean = False; FromIrc: Boolean = False); overload;
    constructor Create(const site_name: String; parentdir: TDirListEntry; skiplist: TSkipList; const s: String; SpeedTest: Boolean = False; FromIrc: Boolean = False; aIsAutoIndex: boolean = False; const aPazoSFV: TPazoSFV = nil); overload;
    destructor Destroy; override;
    function Depth: Integer;
    function MultiCD: Boolean;
    function Dirname: String;
    procedure RegenerateSkiplist;
    { Parses the given dirlist response string from the site and adds corresponding TDirListEntry items to the entries list.
      @param(s The dirlist response from the site.)
      @param(aParseTimestamp If set to true, the timestamp will be parsed as well and set to the dirlist entries. Disabled
        by default for performance reasons.) }
    procedure ParseDirlist(const s: String; const aParseTimestamp: Boolean = False);
    { Does an investigation to determine if TDirlist is complete }
    function Complete: Boolean;
    procedure Usefulfiles(out files: Integer; out size: Int64);
    function FindNfo: TDirListEntry;
    function Find(const filename: String): TDirListEntry;
    { Same as Find but assumes the caller already holds dirlist_lock (Enter or
      EnterReadOnly). Use this from blocks that already acquired the lock so
      the call doesn't try to nest a ReadOnlyLock inside an existing WriteLock,
      which would self-deadlock the TRWLock. }
    function FindLocked(const filename: String): TDirListEntry;
    function FindDirlist(const dirname: String; createit: Boolean = False): TDirList;



    { The function counts all files inside a Dirlist that are considered @link(TDirListEntry.IsOnSite) and are not @link(TDirListEntry.skiplisted) or @link(TDirListEntry.IsBeingUploaded).
      Files from subdirs are included in this final count. Directories themselves are not counted.
      This function is mainly used for race stats, to determine how many files there
      were in total and for reqfilling to check that source and target site contain
      an equal amount of files. }
    function Done: Integer;
    { Counts the files raced by me for all @link(entries) including subdir files.
      @param(aExcludeAsciiFiletypes if @true, it ignores Ascii files)
      @returns(Amount of files raced by me) }
    function FilesRacedByMe(aExcludeAsciiFiletypes: boolean = False): Integer;
    { Summates the size of files raced by me for all @link(entries) including subdir files.
      @param(aExcludeAsciiFiletypes if @true, it ignores Ascii files)
      @returns(Amount of bytes raced by me) }
    function SizeRacedByMe(aExcludeAsciiFiletypes: boolean = False): Int64;
    { Converts the @link(FCompleteInfo) value to a String }
    function GetCompleteInfo: String;
    { Sets the @link(FCompleteInfo) value to given @link()
      @param(aCompleteInfo value from @link(TCompleteInfo) to set the info where we seen it complete) }
    procedure SetCompleteInfo(const aCompleteInfo: TCompleteInfo);
    { Tries to get a cached value indicating whether the given string is a valid file name. If no cached value is available,
      the value is being calculated and then added to the cache
      @returns(@true if input is valid, @false otherwise.) }
    function IsValidFilenameCached(const aFileName: string): boolean;
    { Tries to get a cached value indicating whether the given string is a valid dir name. If no cached value is available,
      the value is being calculated and then added to the cache
      @returns(@true if input is valid, @false otherwise.) }
    function IsValidDirnameCached(const aDirName: string): boolean;

    property LastChanged: TDateTime read FLastChanged write SetLastChanged;
    property CachedCompleteResult: Boolean read FCachedCompleteResult write FCachedCompleteResult;
    property CompleteDirTag: String read FCompleteDirTag;

    { Returns the % completion status of this dir based on the zipscript tag. If the % cannot be determined, because either
      there is no zipscript tag or we can't parse the % from it, -1 is returned.
      @returns(The completion status of this dir in %.) }
    function PercentComplete: Integer;
    property StartedTime: TDateTime read FStartedTime;
    property CompletedTime: TDateTime read FCompletedTime;
    property FullPath: String read FFullPath write SetFullPath;
    property DirlistGaveUp: boolean read FDirlistGaveUp write FDirlistGaveUp;
    property LastUpdated: TDateTime read FLastUpdated write FLastUpdated;
    { True if this TDirlist already has a NFO file
      @returns(@true if NFO is there, @false otherwise) }
    property HasNFO: Boolean read FHasNFO;
    { True if this TDirlist already has a SFV file
      @returns(@true if SFV is there, @false otherwise) }
    property HasSFV: Boolean read FHasSFV;
  end;

{ Just a helper function to initialize image_files_priority and video_files_priority }
procedure DirlistInit;

{ Just a helper function to deinitialize uid lock}
procedure DirlistUnInit;
{ Sorts the @link(aEntries) by the default sorting method }
procedure SortDirlistEntries(const aEntries: TList<TDirListEntry>);
{ Sorts the @link(aEntries) by @link(TDirListEntry.timestamp) }
procedure SortDirlistEntriesByModify(const aEntries: TList<TDirListEntry>);

var

  AsciiFiletypes: array [1..5] of String = ('.nfo', '.sfv', '.m3u', '.cue', '.diz'); //< ASCII files which may need special handling because they might differ in size due to different line endings etc

implementation

uses
  SysUtils, DateUtils, StrUtils, debugunit, mystrings, Math, RegExpr, irc, configunit, mrdohutils, console, IdGlobal, dirlist.helpers, Generics.Defaults;

const
  section = 'dirlist';

var
  image_files_priority: Integer; //< value for priority in dirlist sorter for image files from slftp.ini
  video_files_priority: Integer; //< value for priority in dirlist sorter for video files from slftp.ini
  uid_lock: TCriticalSection;
  uidg: UInt64 = 1;
{$I common.inc}

{ TDirList }

function TDirList.PercentComplete: Integer;
begin
  Result := -1;
  if FCompleteDirTag = '' then
  begin
    if GetDebugVerbosity = dpSpam then
      Debug(dpSpam, section, 'PercentComplete: No CompleteDirTag for %s', [FFullPath]);
    exit;
  end;

  // if we already determined this dirlist to be complete, just return the 100%
  if FCachedCompleteResult then
  begin
    Result := 100;
    exit;
  end;

  if not TagExtractPercent(FCompleteDirTag, Result) then
  begin
    Debug(dpSpam, section, 'PercentComplete: Failed to extract percent from tag "%s" (%s)', [FCompleteDirTag, FFullPath]);
    Result := -1;
  end
  else if GetDebugVerbosity = dpSpam then
    Debug(dpSpam, section, 'PercentComplete: Extracted %d%% from tag "%s" (%s)', [Result, FCompleteDirTag, FFullPath]);
end;

function TDirList.Complete: Boolean;
var
  d: TDirlistEntry;
  files: Integer;
  size: Int64;
  ResultType: String;
begin
  Result := False;
  ResultType := 'Unknown';
  files := 0;
  size := 0;

  // dir has already been seen as complete
  if FCachedCompleteResult then
  begin
    SetCompleteInfo(FromFtpd);
    Result := True;
    exit;
  end;

  if error then
  begin
    if parent <> nil then
      Debug(dpSpam, section, 'TDirlist.Complete ERROR: Site: %s - Dir: %s - DirType: %s', [site_name, FFullPath, parent.DirTypeAsString])
    else
      Debug(dpSpam, section, 'TDirlist.Complete ERROR: Site: %s - Dir: %s', [site_name, FFullPath]);
  end;

  if parent <> nil then
  begin
    // we are in a subdirectory
    // Debug(dpSpam, section, 'TDirlist.Complete DEBUG (START): Site: %s - Dir: %s - DirType: %s', [site_name, FFullPath, parent.DirTypeAsString]);

    // check if subdir has a complete tag
    Result := CompleteByTag;

    // no complete tag found
    // check if subdir is of a known type and has Useful files
    if not Result then
    begin
      if (parent.DirType in [IsSample]) then
      begin
        ResultType := 'Sample';
        Usefulfiles(files, size);
        Result := ((files <> 0) and (size <> 0));
      end
      else if (parent.DirType in [IsProof]) then
      begin
        ResultType := 'Proof';
        Usefulfiles(files, size);
        Result := ((files <> 0) and (size <> 0));
      end
      else if (parent.DirType in [IsCovers]) then
      begin
        ResultType := 'Covers';
        Usefulfiles(files, size);
        Result := ((files <> 0) and (size <> 0));
      end;
    end;

    // check if subdir has Useful files
    if ((not Result) and (ResultType = 'Unknown') and (sfv_status <> dlSFVNoNeed)) then
    begin
      Usefulfiles(files, size);
      Result := ((files <> 0) and (size <> 0));
    end;

    // For debug purposes only
    if (Result) then
    begin
      Debug(dpSpam, section, 'TDirlist.Complete SUBDIR COMPLETE: Site: %s - Dir: %s DirType: %s - ResultType: %s - files: %d size: %d', [site_name, FFullPath, parent.DirtypeAsString, ResultType, files, size]);
    end
    else
    begin
      //Debug(dpSpam, section, 'TDirlist.Complete SUBDIR INCOMPLETE: Site: %s - Dir: %s DirType: %s - ResultType: %s - files: %d size: %d', [site_name, FFullPath, parent.DirTypeAsString, ResultType, files, size]);
    end

  end
  else
  begin
    // we are in the main dir

    //check if there is a complete tag in the dir
    Result := CompleteByTag;

    // no complete tag found - check if the release is multi cd
    if (not Result) and (MultiCD) then
    begin
      if allcdshere then
      begin
        Result := True;

        // check if all multi-cd subdirs are complete
        dirlist_lock.EnterReadOnly('TDirList.Complete');
        try
          for d in entries.Values do
          begin
            try
              if ((d.cdno > 0) and (not d.skiplisted) and ((d.subdirlist = nil) or (not d.subdirlist.Complete))) then
              begin
                Result := False;
                Break;
              end;
            except
              on e: Exception do
              begin
                debugunit.Debug(dpError, section, '[EXCEPTION] TDirList.Complete (MultiCD): %s', [e.Message]);
                Continue;
              end;
            end;
          end;
        finally
          dirlist_lock.LeaveReadOnly;
        end;
      end;
    end;

    // is the release a special kind of release (dirfix, nfofix, etc.)
    if not Result and FContainsNFOOnlyDirTag then
    begin
      Result := HasNFO;
    end;

  end;

  // set complete date if not already set
  if ((Result) and (self.FCompletedTime = 0)) then
  begin
    self.FCompletedTime := Now();
  end;

  // avoid further execution of TDirlist.Complete if Result is true
  FCachedCompleteResult := Result;
end;

constructor TDirList.Create(const site_name: String; parentdir: TDirListEntry; skiplist: TSkipList; const aPazoSFV: TPazoSFV; SpeedTest: boolean = False; FromIrc: boolean = False);
begin
  Create(site_name, parentdir, skiplist, '', SpeedTest, FromIrc, False, aPazoSFV);
end;

constructor TDirList.Create(const site_name: String; parentdir: TDirListEntry; skiplist: TSkipList; const s: String; SpeedTest: boolean = False; FromIrc: boolean = False; aIsAutoIndex: boolean = False; const aPazoSFV: TPazoSFV = nil);
var
  sf: TSkipListFilter;
  uid: uint64;
begin
  if GetUseTimeoutLocking then
  begin
    uid_lock.Enter;
    try
      uid := uidg;
      inc(uidg);
    finally
      uid_lock.Leave;
    end;
    dirlist_lock := TslRWLock.Create('dirlist_' + site_name + '_' + uid.ToString());
  end
  else
  begin
    dirlist_lock := TslRWLock.Create('dirlist_create');
  end;

  biggestcd:= 0;
  error := False;

  need_mkdir := True;
  FCachedCompleteResult := False;
  FHasNFO := False;
  FHasSFV := False;
  FMultiCD := False;

  self.FStartedTime := 0;
  self.FCompletedTime := 0;

  self.site_name := site_name;
  self.FFullPath := 'Not set';
  FCompleteInfo := NotComplete;

  FLastChanged := Now();
  FLastUpdated := Now();
  allcdshere := False;
  entries := TObjectDictionary<string, TDirListEntry>.Create([doOwnsValues], GetCaseInsensitveStringComparer);
  skipped := TStringList.Create;
  skipped.CaseSensitive := False;
  self.parent := parentdir;
  self.FIsValidFileCache := TDictionary<string, boolean>.Create;
  self.FIsValidDirCache := TDictionary<string, boolean>.Create;
  self.FMatchFileCache := TDictionary<string, integer>.Create;
  self.FMatchFileDirectoryCache := TDictionary<string, integer>.Create;

  self.s := s;
  self.skiplist := nil;
  self.FPazoSFV := aPazoSFV;

  self.FIsSpeedTest := SpeedTest;
  self.FIsFromIrc := FromIrc;
  self.FIsAutoIndex := aIsAutoIndex;

  sfv_status := dlSFVUnknown;
  if skiplist <> nil then
  begin
    self.skiplist := TSkipList.CreateClone(skiplist);
    sf:= skiplist.AllowedDir('', 'testsfv.sfv');
    if sf = nil then
    begin
      sfv_status := dlSFVNoNeed;
    end else
    begin
      sfv_status := dlSFVNotFound;
    end;
  end;

  if s <> '' then
    ParseDirlist(s, True);
end;

destructor TDirList.Destroy;
begin
  skipped.Free;
  dirlist_lock.Enter('TDirList.Destroy');
  try
    entries.Free;
    FIsValidFileCache.Free;
    FIsValidDirCache.Free;
    FMatchFileCache.Free;
    FMatchFileDirectoryCache.Free;
    skiplist.Free;
  finally
    dirlist_lock.Leave;
  end;
  FreeAndNil(dirlist_lock);
  inherited;
end;

function TDirList.Depth: Integer;
begin
  if parent <> nil then
    Result := parent.dirlist.Depth + 1
  else
    Result := 1;
end;

function TDirList.Dirname: String;
begin
  if parent = nil then
  begin
    if MultiCd then
      Result := '_MULTICDROOT_'
    else
      Result := '_ROOT_';
  end
  else
    Result := parent.filename;
end;

function TDirList.MultiCD: Boolean;
var
  i: Integer;
  s: String;
  de: TDirListEntry;
begin
  if parent = nil then
  begin
    Result := FMultiCD;
  end
  else
  begin
    Result := parent.dirlist.MultiCD;
  end;
end;

class function TDirlist.Timestamp(ts: String): TDateTime;
const
  Months: array[1..12] of String = ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
var
  s1,s2,s3: String;
  l, ev, ora,perc, honap, nap, i: Integer;
  evnelkul: Boolean;
begin
  Result := 0;

  s1 := Fetch(ts, ' ', True, False);
  s2 := Fetch(ts, ' ', True, False);
  s3 := Fetch(ts, ' ', True, False);

  if s3 = '' then
    exit;

  honap := 0;
   for i := 1 to 12 do
   begin
     if (Months[i] = s1) then
     begin
       honap := i;
       Break;
     end;
  end;
  if (honap = 0) then
    exit;

  nap := StrToIntDef(s2, 0);
  if ((nap < 1) or (nap > 31)) then
    exit;


  l:= length(s3);

  ora := 0;
  perc := 0;
  evnelkul := False;
  if(l = 4) then
  begin
    ev := StrToIntDef(s3, 0);
    if ev < 1000 then exit;
    if not TryEncodeDateTime(ev, honap, nap, 0,0,0,0,  Result) then
      exit;
  end
  else if (l = 5) then
  begin
    evnelkul:= True;
    ora:= StrToIntDef(Copy(s3,1,2),0);
    perc:= StrToIntDef(Copy(s3,4,2),0);
    if not TryEncodeDateTime(YearOf(Now()), honap, nap, ora, perc, 0, 0, Result) then
      exit;
  end
  else
    exit;

  if ((Result > Now) and (evnelkul)) then
    TryEncodeDateTime(YearOf(Now)-1, honap, nap, ora, perc,0,0, Result);

end;

procedure TDirList.ParseDirlist(const s: String; const aParseTimestamp: Boolean = False);
var
  akttimestamp: TDateTime;
  de: TDirListEntry;
  added: Boolean;
  fTagCompleteType: TTagCompleteType;
  fParsedDirlistEntries: TObjectList<TParsedDirListEntry>;
  fParsedDirlistEntry: TParsedDirListEntry;
begin
  added := False;

  // No need to parse the dir again if it's complete
  if FCachedCompleteResult then exit;

  debugunit.Debug(dpSpam, section, Format('--> ParseDirlist %s (%s, %d entries)', [FFullPath, site_name, entries.Count]));

  fParsedDirlistEntries := ParseStatResponse(s);
  dirlist_lock.Enter('TDirList.ParseDirlist');
  try
    for de in entries.Values do
    begin
      de.FWasOnSite := de.IsOnSite;
      de.IsOnSite := False;
    end;

{
  s contains the whole ftpd response:
  drwxrwxrwx   2 aq11     iND              3 Apr 19 23:14 Sample
  drwxrwxrwx   2 xxxx     USA              2 Apr 19 23:15 [xxx] - ( 2539M 28F - COMPLETE ) - [xxx]
  -rw-r--r--   1 dqdq     DSE          10295 Apr 19 23:14 baby.animals.s01e05.little.hunters.internal.2160p.uhdtv.h265-cbfm.nfo
  -rw-r--r--   1 abc      Friends  100000000 Apr 19 23:14 baby.animals.s01e05.little.hunters.internal.2160p.uhdtv.h265-cbfm.r00
  ...
  drwxrwxrwx   2 nete     Death_Me     4096 Jan 29 05:05 Whisteria_Cottage-Heathen-RERIP-2009-pLAN9
}
    for fParsedDirlistEntry in fParsedDirlistEntries do
    begin
      if fParsedDirlistEntry.Filesize < 0 then
        Continue;

      if not FIsFromIrc then
      begin
        // Dont add complete tags to dirlist entries

        //if it's a dir and has already been checked to be valid, it can't be a complete tag
        if (((fParsedDirlistEntry.DirMask[1] = 'd') and not FIsValidDirCache.ContainsKey(fParsedDirlistEntry.Filename))

        //if it's a file and has a size > 0, it can't be a complete tag
        or ((fParsedDirlistEntry.Filesize < 1) and (fParsedDirlistEntry.DirMask[1] <> 'd')

          // skip .missing files which are created by the zipscript for files that have not yet been sent
          and not fParsedDirlistEntry.Filename.EndsWith('-missing')

          //if it's a file and has already been checked to be valid, it can't be a complete tag
          and not FIsValidFileCache.ContainsKey(fParsedDirlistEntry.Filename)))
        then
        begin
          if FCompleteDirTag = fParsedDirlistEntry.Filename then //if this has already been identified as complete tag, no need for any further action
            continue;

          fTagCompleteType := TagComplete(fParsedDirlistEntry.Filename);
          if (fTagCompleteType <> tctUNMATCHED) then
          begin
            FCompleteDirTag := fParsedDirlistEntry.Filename;
            Continue;
          end;
        end;
      end;

      if (fParsedDirlistEntry.DirMask[1] = 'd') then
      begin
        // directory: fDirMask[1] = 'd'
        if not IsValidDirnameCached(fParsedDirlistEntry.Filename) then Continue;
      end
      else
      begin
        // no directory: fDirMask[1] <> 'd'
        if not IsValidFilenameCached(fParsedDirlistEntry.Filename) then Continue;
      end;

      // Do not filter if we call the dirlist from irc
      if not FIsFromIrc then
      begin

        // file is flagged as skipped
        if (skipped.IndexOf(fParsedDirlistEntry.Filename) <> -1) then
        begin
          Continue;
        end;

        // entry is a file and is not downlodable
        if ((fParsedDirlistEntry.DirMask[1] <> 'd') and ((fParsedDirlistEntry.DirMask[5] <> 'r') and (fParsedDirlistEntry.DirMask[8] <> 'r'))) then
        begin
          Continue;
        end;
      end;

      if aParseTimestamp then
        akttimestamp := Timestamp(fParsedDirlistEntry.Date)
      else
        akttimestamp := MinDateTime;

      de := Find(fParsedDirlistEntry.Filename);
      if de = nil then
      begin
        de := TDirListEntry.Create(fParsedDirlistEntry.Filename, self, (fParsedDirlistEntry.DirMask[1] = 'd'));

        if ((de.IsSFV) and (HasSFV)) then
        begin
          de.Free;
          Continue;
        end;
        if ((de.IsNFO) and (HasNFO)) then
        begin
          de.Free;
          Continue;
        end;

        de.FUsername := fParsedDirlistEntry.Username;
        de.FGroupname := fParsedDirlistEntry.Groupname;
        de.FIsSymlink := fParsedDirlistEntry.IsSymlink;
        de.FSymlinkTarget := fParsedDirlistEntry.SymlinkTarget;
        de.timestamp := akttimestamp;
        de.justadded := True;

        if not de.directory then
          de.filesize := fParsedDirlistEntry.Filesize;

        // Do not filter if we call the dirlist from irc
        if not FIsFromIrc then
        begin
          if ((not de.Directory) and (de.Extension = '') and (not FIsSpeedTest)) then
          begin
            de.Free;
            Continue;
          end;

          // Dont add skip files to dirlist
          if ((not de.Directory) and (skiplist <> nil)) then
          begin
            de.RegenerateSkiplist;
            if (de.skiplisted) then
            begin
              de.Free;
              Continue;
            end;
          end;
        end;

        if ((not de.Directory) and (de.IsSFV) and (de.filesize > 0)) then
        begin
          sfv_status := dlSFVFound;
        end;

        if (de.Directory) then
        begin
          de.subdirlist := TDirlist.Create(site_name, de, skiplist, FPazoSFV, FIsSpeedTest, FIsFromIrc);
          if de.subdirlist <> nil then
            de.subdirlist.FullPath := MyIncludeTrailingSlash(FFullPath) + de.filename;
        end;

        entries.Add(de.filename, de);

        // the entry is valid and added, if it's a NFO or SFV file, set the field, then no more NFO or SFV files will be added
        if de.IsNFO and (de.filesize > 0) then
          FHasNFO := True;
        if de.IsSFV and (de.filesize > 0) then
          FHasSFV := True;

        // if this newly created entry is a dir with a cdno, recalculate MultiCD info
        if de.cdno > 0 then
          CalculateMultiCD;

        LastChanged := Now();
        added := True;
      end
      else if (de.filesize <> fParsedDirlistEntry.Filesize) then
      begin
        if ((de.filesize <> fParsedDirlistEntry.Filesize) or (de.FUsername <> fParsedDirlistEntry.Username)) then
        begin
          LastChanged := Now();
        end;

        de.filesize := fParsedDirlistEntry.Filesize;
        de.timestamp := akttimestamp;
        de.FUsername := fParsedDirlistEntry.Username;
        de.FGroupname := fParsedDirlistEntry.Groupname;
        de.FSizeChanged := True;
        de.justadded := Not de.FWasOnSite;

        // the file had size of 0 when first seen, then IsNFO / IsSFV was not set. Set it now when the file size is greater than 0.
        if de.IsNFO and not FHasNFO and (de.filesize > 0) then
          FHasNFO := True;
        if de.IsSFV and not FHasSFV and (de.filesize > 0) then
          FHasSFV := True;
      end;

      // entry is a file and is being uploaded (glftpd only?)
      de.FIsBeingUploaded := (fParsedDirlistEntry.DirMask[1] <> 'd') and ((fParsedDirlistEntry.DirMask[7] = 'x') and (fParsedDirlistEntry.DirMask[10] = 'x'));
      de.FIsSymlink := fParsedDirlistEntry.IsSymlink;
      de.FSymlinkTarget := fParsedDirlistEntry.SymlinkTarget;

      de.IsOnSite := True;
    end;

    // entries found means the dir exists
    if ((need_mkdir)) then
    begin
      for de in entries.Values do
      begin
        if de.IsOnSite then
        begin
          need_mkdir := False;
          break;
        end;
      end;
    end;

  finally
    dirlist_lock.Leave;
    fParsedDirlistEntries.Free;
  end;

  FLastUpdated := Now();

  // set defaults values if direcotry was just added
  if added then
  begin
    FCachedCompleteResult := False;

    if skiplist <> nil then
    begin
      try
        RegenerateSkiplist; // think it'll call RegenerateSkiplist several times for already existing files (which are already checked above when added)
      except
        on E: Exception do
        begin
          debugunit.Debug(dpError, section, '[EXCEPTION] TDirList.ParseDirList(RegenerateSkiplist): %s', [e.Message]);
        end;
      end;
    end;
  end;

  debugunit.Debug(dpSpam, section, Format('<-- ParseDirlist %s (%s, %d entries)', [FFullPath, site_name, entries.Count]));
end;

procedure TDirList.RegenerateSkiplist;
var
    ld: TDirListEntry;
begin
  if skiplist = nil then exit;

  dirlist_lock.Enter('TDirList.RegenerateSkiplist');
  try
    for ld in entries.Values do
    begin
      try
        ld.RegenerateSkiplist;
      except
        on E: Exception do
        begin
          debugunit.Debug(dpError, section, '[EXCEPTION] TDirList.RegenerateSkiplist: %s', [e.Message]);
          Continue;
        end;
      end;
    end;
  finally
    dirlist_lock.Leave;
  end;

end;

function _DirListSorter({$IFDEF FPC}constref{$ELSE}const{$ENDIF} i1, i2: TDirListEntry): Integer;
var
  c1, c2: Integer;
  i1IsImage, i1IsVideo: Boolean;
  i2IsImage, i2IsVideo: Boolean;
begin
  (*
     Result := -1 (i1 wins)
            := 1  (i2 wins)
            := 0  (no change)
  *)
  Result := 0;

  try

    // We don't care about skiplisted entries
    if ((i1.skiplisted) and (i2.skiplisted)) then exit;

    // At least one file need to have an extension for extention sorting
    if (i1.Extension <> '') or (i2.Extension <> '') then
    begin
      // sfv priority
      if ((i1.IsSFV) and (not i2.IsSFV)) then
      begin
        Result := -1;
        exit;
      end;
      if ((not i1.IsSFV) and (i2.IsSFV)) then
      begin
        Result := 1;
        exit;
      end;

      // nfo priority
      if ((i1.IsNFO) and (not i2.IsNFO)) then
      begin
        Result := -1;
        exit;
      end;
      if ((not i1.IsNFO) and (i2.IsNFO)) then
      begin
        Result := 1;
        exit;
      end;

      // image files priority (i.e.: proofs, covers)
      i1IsImage := AnsiMatchText(i1.Extension, ImageFileExtensions);
      i2IsImage := AnsiMatchText(i2.Extension, ImageFileExtensions);
      if (i1IsImage) or (i2IsImage) then
      begin
        if ((i1IsImage) and (not i2IsImage)) then
          case image_files_priority of
            1 : Result := -1;
            2 : Result := 1;
          end
        else if ((not i1IsImage) and (i2IsImage)) then
          case image_files_priority of
            1 : Result := 1;
            2 : Result := -1;
          end;

        //Debug(dpSpam, section, '_DirListSorter (image): i1: %s i2: %s result: %d', [i1.Extension, i2.Extension, Result]);
        exit;
      end;

      // video files priority
      i1IsVideo := AnsiMatchText(i1.Extension, VideoFileExtensions);
      i2IsVideo := AnsiMatchText(i2.Extension, VideoFileExtensions);
      if (i1IsVideo) or (i2IsVideo) then
      begin
        if ((i1IsVideo) and (not i2IsVideo)) then
          case video_files_priority of
            1 : Result := -1;
            2 : Result := 1;
          end
        else if ((not i1IsVideo) and (i2IsVideo)) then
          case video_files_priority of
            1 : Result := 1;
            2 : Result := -1;
          end;

        //Debug(dpSpam, section, '_DirListSorter (video): i1: %s i2: %s result: %d', [i1.Extension, i2.Extension, Result]);
        exit;
      end;
    end;

    // If not sorting occured yet - try default sorting
    // Sorting two directories
    if Result = 0 then
    begin
      if ((i1.directory) and (i2.directory)) then
      begin
        if (i1.FSkipListAllowedFileIndex <> -1) then
        begin
          c1 := i1.FSkipListAllowedFileIndex;
          c2 := i2.FSkipListAllowedFileIndex;

          if (c1 > c2) then
            Result := 1
          else
          if (c1 < c2) then
            Result := -1
          else
            Result := 0;
        end else
          Result := CompareStr(i1.filename, i2.filename);
      end
      else
      if ((not i1.directory) and (not i2.directory)) then
      begin
        c1 := i1.FSkipListAllowedFileIndex;
        c2 := i2.FSkipListAllowedFileIndex;

        if (c1 > c2) then
          Result := 1
        else
        if (c1 < c2) then
          Result := -1
        else
        begin
          // files are of the same type - sort by size
          if i1.filesize > i2.filesize then
            Result := -1
          else
          if i1.filesize < i2.filesize then
            Result := 1
          else
            Result := 0;
        end;
      end
      else
      // Priority to directories
      if (i1.directory) then
        Result := -1
      else
        Result := 1;
    end;
  except
    on e: Exception do
      debugunit.Debug(dpError, section, '[EXCEPTION] _DirListSorter: %s', [e.Message]);
  end;
end;

procedure SortDirlistEntries(const aEntries: TList<TDirListEntry>);
begin
  aEntries.Sort(TComparer<TDirListEntry>.Construct(_DirListSorter));
end;

function TDirList.CompleteByTag: Boolean;
var
  i: TTagCompleteType;
begin
  Result := False;
  if (FCompleteDirTag = '') then
    exit;

  // check if the previous check was against the same tag, then we don't need to calculate the result again
  if FCompleteDirTagLastChecked = FCompleteDirTag then
    i := FCompleteDirTagLastResult
  else
  begin
    i := TagComplete(FCompleteDirTag);

    // cache the result
    FCompleteDirTagLastResult := i;
    FCompleteDirTagLastChecked := FCompleteDirTag;

    if (i = tctCOMPLETE) then
    begin
      Result := True;
      exit;
    end;
  end;
end;

function TDirListEntry.RecognizeDirTypeFromDirname(const aDirname: String): TDirType;
var
  r: TRegExpr;
begin
  Result := IsUnknown;

  r := TRegExpr.Create;
  r.ModifierI := True;
  try
    r.Expression := '^sample$';
    if r.Exec(aDirname) then
    begin
      Result := IsSample;
      exit;
    end;

    r.Expression := '^proof$';
    if r.Exec(aDirname) then
    begin
      Result := IsProof;
      exit;
    end;

    r.Expression := '^(sub|subs)$';
    if r.Exec(aDirname) then
    begin
      Result := IsSubs;
      exit;
    end;

    r.Expression := '^(cover|covers)$';
    if r.Exec(aDirname) then
    begin
      Result := IsCovers;
      exit;
    end;
  finally
    r.Free;
  end;
end;

procedure TDirList.Usefulfiles(out files: Integer; out size: Int64);
var
  de: TDirlistEntry;
  afiles: Integer;
  asize: Int64;
begin
  asize := 0;
  afiles := 0;
  files := 0;
  size := 0;

  dirlist_lock.EnterReadOnly('TDirList.Usefulfiles');
  try
    for de in entries.Values do
    begin
      try
        if de.skiplisted then Continue;

        if not de.IsAsciiFiletype then
        begin
          inc(files);
          inc(size, de.filesize);
        end;
        if ((de.directory) and (de.subdirlist <> nil)) then
        begin
          de.subdirlist.Usefulfiles(afiles, asize);
          inc(files, afiles);
          inc(size, asize);
        end;
      except
        on e: Exception do
        begin
          debugunit.Debug(dpError, section, '[EXCEPTION] TDirList.Usefulfiles: %s', [e.Message]);
          Continue;
        end;
      end;
    end;
  finally
    dirlist_lock.LeaveReadOnly;
  end;
end;

function TDirList.Find(const filename: String): TDirListEntry;
begin
  Result := nil;
  if entries.Count = 0 then
    exit;

  dirlist_lock.EnterReadOnly('TDirList.Find');
  try
    entries.TryGetValue(filename, Result);
  finally
    dirlist_lock.LeaveReadOnly;
  end;
end;

function TDirList.FindLocked(const filename: String): TDirListEntry;
begin
  Result := nil;
  if entries.Count = 0 then
    exit;
  entries.TryGetValue(filename, Result);
end;

procedure TDirList.SetLastChanged(const value: TDateTime);
begin
  FLastChanged := Max(value, FLastChanged);
  if parent <> nil then
    parent.dirlist.LastChanged := FLastChanged;
end;

function TDirList.FindDirlist(const dirname: String; createit: Boolean = False): TDirList;
var
  p: Integer;
  firstdir, lastdir: String;
  d: TDirlistEntry;
begin
  Result := nil;

  if dirname = '' then
  begin
    Result := self;
    exit;
  end;

  try
    p := Pos('/', dirname);
    if 0 < p then
    begin
      firstdir := Copy(dirname, 1, p-1);
      lastdir := Copy(dirname, p+1, 1000);
    end
    else
    begin
      firstdir := dirname;
      lastdir := '';
    end;

    // Inline the lookup (instead of calling Find which would try a nested
    // EnterReadOnly while we hold WriteLock). On createit the WriteLock is
    // necessary to safely add to entries; otherwise a ReadOnlyLock would do
    // but we keep WriteLock since the path is rarely hot and reentrancy is safer.
    // The subdirlist initialization MUST happen inside the same lock - two
    // concurrent FindDirlist calls would otherwise both see d.subdirlist=nil
    // and each create a new TDirList, with the second overwriting the first
    // and leaking it (and any in-flight readers of the first instance).
    dirlist_lock.Enter('TDirList.FindDirlist');
    try
      if entries.Count = 0 then
        d := nil
      else if not entries.TryGetValue(firstdir, d) then
        d := nil;
      if d = nil then
      begin
        if not createit then
          exit;
        d := TDirListEntry.Create(firstdir, self, True);
        entries.Add(d.filename, d);
      end;

      if not d.Directory then
        exit; // d is a file - no subdirlist to recurse into

      if d.subdirlist = nil then
      begin
        d.subdirlist := TDirlist.Create(site_name, d, skiplist, FPazoSFV);
        if d.subdirlist <> nil then
          d.subdirlist.FullPath := MyIncludeTrailingSlash(self.FFullPath) + d.filename;
      end;
    finally
      dirlist_lock.Leave;
    end;
  except
    on E: Exception do
    begin
      debugunit.Debug(dpError, section, 'TDirList.FindDirlist: %s', [e.Message]);
      exit;
    end;
  end;

  // Recursive descent uses the subdirlist's own lock - safe to do outside ours.
  if d.subdirlist <> nil then
    Result := d.subdirlist.FindDirlist(lastdir, createit);
end;

function TDirList.Done: Integer;
var
  de: TDirlistEntry;
begin
  Result := 0;

  dirlist_lock.EnterReadOnly('TDirList.Done');
  try
    for de in entries.Values do
    begin
      try
        if de.skiplisted then
          Continue;

        if ((de.IsOnSite And Not de.IsBeingUploaded) and (not de.directory)) then
          Inc(Result);

        if ((de.directory) and (de.subdirlist <> nil)) then
          Inc(Result, de.subdirlist.Done);
      except
        on E: Exception do
        begin
          debugunit.Debug(dpError, section, 'TDirList.Done: %s', [e.Message]);
          Continue;
        end;
      end;
    end;
  finally
    dirlist_lock.LeaveReadOnly;
  end;
end;

function TDirList.FilesRacedByMe(aExcludeAsciiFiletypes: boolean = False): Integer;
var
  de: TDirlistEntry;
begin
  Result := 0;

  dirlist_lock.EnterReadOnly('TDirList.FilesRacedByMe');
  try
    for de in entries.Values do
    begin
      try
        if aExcludeAsciiFiletypes then
        begin
          if (de.FRacedByMe and not de.IsAsciiFiletype) then
            Inc(Result);
        end
        else
        begin
          if de.FRacedByMe then
            Inc(Result);
        end;

        if ((de.directory) and (de.subdirlist <> nil)) then
        begin
          Inc(Result, de.subdirlist.FilesRacedByMe(aExcludeAsciiFiletypes));
        end;
      except
        on E: Exception do
        begin
          debugunit.Debug(dpError, section, 'TDirList.FilesRacedByMe: %s', [e.Message]);
          Continue;
        end;
      end;
    end;
  finally
    dirlist_lock.LeaveReadOnly;
  end;
end;

function TDirList.SizeRacedByMe(aExcludeAsciiFiletypes: boolean = False): Int64;
var
  de: TDirlistEntry;
begin
  Result := 0;

  dirlist_lock.EnterReadOnly('TDirList.SizeRacedByMe');
  try
    for de in entries.Values do
    begin
      try
        if aExcludeAsciiFiletypes then
        begin
          if (de.FRacedByMe and not de.IsAsciiFiletype) then
            Inc(result, de.filesize);
        end
        else
        begin
          if de.FRacedByMe then
            Inc(result, de.filesize);
        end;

        if ((de.directory) and (de.subdirlist <> nil)) then
        begin
          Inc(result, de.subdirlist.SizeRacedByMe(aExcludeAsciiFiletypes));
        end;
      except
        on E: Exception do
        begin
          debugunit.Debug(dpError, section, 'TDirList.SizeRacedByMe: %s', [e.Message]);
          Continue;
        end;
      end;
    end;
  finally
    dirlist_lock.LeaveReadOnly;
  end;
end;


procedure TDirList.Clear;
var
  de: TDirListEntry;
begin
  allcdshere := False;
  FLastChanged := 0;
  FLastUpdated := 0;
  biggestcd := 0;
  FHasNFO := False;
  FHasSFV := False;

  dirlist_lock.Enter('TDirList.Clear');
  try

    for de in entries.Values do
    begin
      try
        de.IsOnSite := False;
        de.error := False;
      except
        on E: Exception do
        begin
          debugunit.Debug(dpError, section, 'TDirList.Clear: %s', [e.Message]);
          Continue;
        end;
      end;
    end;
  finally
    dirlist_lock.Leave;
  end;

end;

function _DirListModSorter({$IFDEF FPC}constref{$ELSE}const{$ENDIF} i1, i2: TDirListEntry): Integer;
begin
  // compare: -1 -> good order
  // compare:  1 -> exchange
  Result := CompareValue(i2.timestamp, i1.timestamp);
end;

procedure SortDirlistEntriesByModify(const aEntries: TList<TDirListEntry>);
begin
  aEntries.Sort(TComparer<TDirListEntry>.Construct(_DirListModSorter));
end;

function TDirList.FindNfo: TDirListEntry;
var de: TDirlistEntry;
begin
  Result := nil;

  dirlist_lock.EnterReadOnly('TDirList.FindNfo');
  try
    for de in entries.Values do
    begin
      try
        if ((de.Extension = '.nfo') and (de.filesize > 0) and (de.filesize < 32768)) then //nfo always smaller than 32kb
        begin
          Result := de;
          exit;
        end;
      except
        on E: Exception do
        begin
          debugunit.Debug(dpError, section, '[EXCEPTION] TDirList.FindNfo: %s', [e.Message]);
          Continue;
        end;
      end;
    end;
  finally
    dirlist_lock.LeaveReadOnly;
  end;
end;

procedure TDirList.SetCompleteInfo(const aCompleteInfo: TCompleteInfo);
begin
  if (FCompleteInfo <> FromFtpdAndIrc) then
  begin
    if ((FCompleteInfo = NotComplete) or ((FCompleteInfo <> FromFtpdAndIrc) and (aCompleteInfo <> FCompleteInfo))) then
      FCompleteInfo := TCompleteInfo(Ord(FCompleteInfo) + Ord(aCompleteInfo));
    if (FCompletedTime = 0) and (FCompleteInfo in [FromIrc, FromFtpdAndIrc]) then
      FCompletedTime := Now();
  end;
end;

function TDirList.GetCompleteInfo: String;
begin
  Result := 'UNKNOWN!';

  case FCompleteInfo of
    NotComplete:
      begin
        Result := 'Not Complete';
      end;
    FromIrc:
      begin
        Result := 'IRC';
      end;
    FromFtpd:
      begin
        Result := 'FTP';
      end;
    FromFtpdAndIrc:
      begin
        Result := 'FTP + IRC';
      end;
  end;
end;

{ TDirListEntry }

constructor TDirListEntry.Create(const filename: String; dirlist: TDirList; const aIsDirectory: boolean);
begin
  if aIsDirectory then
  begin
    try
      // check if we have a special kind of subdirectory
      self.FDirType := RecognizeDirTypeFromDirname(filename);
    except
      on e: Exception do
        debugunit.Debug(dpError, section, '[EXCEPTION] TDirListEntry.Create (DirType): %s', [e.Message]);
    end;
  end
  else
  begin
    if (dirlist <> nil) and (dirlist.parent <> nil) then
      self.FDirType := dirlist.parent.DirType
    else
      self.FDirType := IsMain;
  end;

  self.FDirectory := aIsDirectory;
  self.FIsSymlink := False;
  self.FSymlinkTarget := '';
  self.dirlist := dirlist;
  self.filename := filename;
  self.FRacedByMe := False;
  self.skiplisted := False;
  self.FSkipListAlreadyProcessed := False;
  self.IsOnSite := False;
  self.FIsBeingUploaded := False;
  self.error := False;
  subdirlist := nil;
  self.FSizeChanged := True;

  FFilenameLowerCase := LowerCase(filename);
  FExtension := ExtractFileExt(FFilenameLowerCase);
  cdno := 0;
  FSkipListAllowedFileIndex := -1;
  FIsNFO := FExtension = '.nfo';
  FIsSFV := FExtension = '.sfv';

  if aIsDirectory then
    CalcCDNumber;
  if (dirlist.FStartedTime = 0) then
  begin
    dirlist.FStartedTime := Now();
  end;
end;

destructor TDirListEntry.Destroy;
begin
  FreeAndNil(subdirlist);
  inherited;
end;

procedure TDirListEntry.CalcCDNumber;
const
  multicddirprefix : array[1..4] of String = ('cd', 'dvd', 'disc','disk');
var
  s: String;
  i: Integer;
begin
  s := ReplaceText(FFilenameLowerCase, ' ', '');
  s := ReplaceText(s, '_', '');
  s := ReplaceText(s, '-', '');

  for i := 1 to 4 do
  begin
    if (1 = Pos(UpperCase(multicddirprefix[i]), UpperCase(s))) then
    begin
      cdno := StrToIntDef(Copy(s, Length(multicddirprefix[i]) + 1, 1000), 0);
      exit;
    end;
  end;
end;

function TDirListEntry.IsAsciiFiletype: Boolean;
begin
  Result := False;

  if filesize = 0 then exit;
  if directory then exit;

  if MatchText(Extension, AsciiFiletypes) then
  begin
    Result := True;
    exit;
  end;
end;

function TDirListEntry.DirTypeAsString: String;
begin
  Result := 'Undefined';
  case self.DirType of
    IsUnknown: Result := 'Unknown';
    IsMain: Result := 'Main';
    IsMultiCD: Result := 'MultiCD';
    IsSample: Result := 'Sample';
    IsProof: Result := 'Proof';
    IsCovers: Result := 'Covers';
    IsSubs: Result := 'Subs';
  else
    Result := 'Unknown';
  end;
end;

procedure TDirListEntry.RegenerateSkiplist;
var
  ldepth: Integer;
  s, fDirPathHelper: String;
  sf: TSkipListFilter;
begin
  if dirlist.skiplist = nil then exit;

  if ( not FSkipListAlreadyProcessed ) then
  begin
    FSkipListAlreadyProcessed := True;

    if dirlist.FullPath.EndsWith('/', True) then
      fDirPathHelper := dirlist.FullPath
    else
      fDirPathHelper := dirlist.FullPath + '/';

    if not directory then
    begin
      s := dirlist.Dirname;

      // first we look for ftprush screwed up files like (1).nfo
      if IsFtpRushScrewedUpFile(filename, Extension) then
      begin
        skiplisted := True;
        dirlist.skipped.Add(filename);
        irc_Addtext_by_key('SKIPLOG', Format('<c2>[SKIP]</c> FTPRush screwed up file %s %s %s : %s%s', [dirlist.site_name, dirlist.skiplist.sectionname, s, fDirPathHelper, filename]));
        exit;
      end;

      sf := dirlist.skiplist.AllowedFile(s, filename, FSkipListAllowedFileIndex);

      if sf = nil then
      begin
        skiplisted := True;
        dirlist.skipped.Add(filename);
        irc_Addtext_by_key('SKIPLOG', Format('<c2>[SKIP]</c> Not AllowedFile %s %s %s : %s%s', [dirlist.site_name, dirlist.skiplist.sectionname, s, fDirPathHelper, filename]));
        exit;
      end;

      if dirlist.parent = nil then
        s := ''
      else
        s := dirlist.parent.filename;

      if (dirlist.FPazoSFV <> nil) and not dirlist.FPazoSFV.CheckSFV(s, FFilenameLowerCase, Extension) then
      begin
        skiplisted := True;
        dirlist.skipped.Add(filename);
        irc_Addtext_by_key('SKIPLOG', Format('<c2>[SKIP]</c> Not in SFV %s %s %s : %s%s', [dirlist.site_name, dirlist.skiplist.sectionname, s, fDirPathHelper, filename]));
        exit;
      end
    end
    else
    begin
      ldepth := dirlist.Depth();

      if ldepth < dirlist.skiplist.dirdepth then
      begin

        // entry is a dir with unwanted characters
        // this is probably to avoid complete tags that might not have been handeled by the 'TagComplete' function
        // to be transfered, because those might contain sensitive info
        if filename.Contains('[') or filename.Contains(']') or filename.Contains(',') or filename.Contains('=') then
        begin
          skiplisted := True;
          dirlist.skipped.Add(filename);
          irc_Addtext_by_key('SKIPLOG', Format('<c2>[SKIP]</c> Dir contains not allowed char %s %s : %s%s', [dirlist.site_name, dirlist.skiplist.sectionname, fDirPathHelper, filename]));
          exit;
        end;

        // you have to go through the alloweddirs and check if it's allowed
        s := dirlist.Dirname;
        sf := dirlist.skiplist.AllowedDir(s, filename, FSkipListAllowedFileIndex);
        if sf = nil then
        begin
          skiplisted := True;
          dirlist.skipped.Add(filename);
          irc_Addtext_by_key('SKIPLOG', Format('<c2>[SKIP]</c> Not AllowedDir %s %s : %s%s', [dirlist.site_name, dirlist.skiplist.sectionname, fDirPathHelper, filename]));
        end
      end
      else
      begin
        irc_Addtext_by_key('SKIPLOG', Format('<c2>[SKIP]</c> dirdepth %s %s : %s%s', [dirlist.site_name, dirlist.skiplist.sectionname, fDirPathHelper, filename]));
        skiplisted := True;
      end;
    end;
  end;
end;

function TDirlist.IsValidFilenameCached(const aFileName: string): boolean;
begin
  if FIsValidFileCache.TryGetValue(aFileName, Result) then
    exit;

  Result := IsValidFilename(aFileName);
  FIsValidFileCache.AddOrSetValue(aFileName, Result);
end;

function TDirlist.IsValidDirnameCached(const aDirName: string): boolean;
begin
  if FIsValidDirCache.TryGetValue(aDirName, Result) then
    exit;

  Result := IsValidDirname(aDirName);
  FIsValidDirCache.AddOrSetValue(aDirName, Result);
end;

procedure TDirList.SetFullPath(const aFullPath: string);
begin
  if FFullPath <> aFullPath then
  begin
    FFullPath := aFullPath;
    FContainsNFOOnlyDirTag := ReleaseOnlyConsistsOfNFO(aFullPath);
  end;
end;

procedure TDirList.CalculateMultiCD;
var
  allCdNumbers: String;
  i: Integer;
  de: TDirListEntry;
begin
  allCdNumbers := '';
  biggestcd := 0;

  // find the biggest CD
  for de in entries.Values do
  begin
    try
      if de.cdno > 0 then
      begin
        FMultiCD := True;
        allCdNumbers := allCdNumbers + IntToStr(de.cdno);

        if de.cdno > biggestcd then
          biggestcd := de.cdno;
      end;
    except
      on e: Exception do
      begin
        debugunit.Debug(dpError, section, '[EXCEPTION] TDirList.CalculateMultiCD: %s', [e.Message]);
        Continue;
      end;
    end;
  end;

  // find out if any CD number below the biggest one is still missing and set allcdshere accordingly
  if biggestcd > 1 then
  begin
    allcdshere := True;
    for i := 1 to biggestcd do
    begin
      if (0 = Pos(IntToStr(i), allCdNumbers)) then
      begin
        allcdshere := False;
        Break;
      end;
    end;
  end;
end;

procedure DirlistInit;
begin
  DirlistHelperInit;
  uid_lock := TCriticalSection.Create;

  image_files_priority := config.ReadInteger('queue', 'image_files_priority', 2);
  if not (image_files_priority in [0..2]) then
    image_files_priority := 2;

  video_files_priority := config.ReadInteger('queue', 'video_files_priority', 2);
  if not (video_files_priority in [0..2]) then
    video_files_priority := 2;
end;

procedure DirlistUnInit;
begin
  uid_lock.Free
end;

end.
