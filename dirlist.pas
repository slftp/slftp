unit dirlist;

interface

uses Classes, Contnrs, SyncObjs, sitesunit, skiplists, globals;

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

  TDirListEntry = class
    dirlist: TDirList;

    megvanmeg: Boolean;
    justadded: Boolean;
    error: Boolean; //< { @true if file cannot be send, will be skipped then, @false otherwise. }

    username: String;
    groupname: String;

    fDirectory: Boolean; //< current dir is a directory
    fDirType: TDirType; //< Indicates what kind of Directory the current dir is

    subdirlist: TDirList;

    filename: String; //< filename
    filenamelc: String; //< lowercase filename
    filesize: Int64;

    skiplisted: Boolean;
    racedbyme: Boolean; //< @true if we send this file to the site
    done: Boolean;

    tradeCount: Integer;

    cdno: Integer;

    timestamp: TDateTime;

    sfvfirsteventvoltmar: Boolean;

    addedfrom: TStringList;

    procedure CalcCDNumber;
    function Extension: String;

    constructor Create(const filename: String; dirlist: TDirList; SpeedTest: Boolean = False); overload;
    constructor Create(de: TDirlistEntry; dirlist: TDirList; SpeedTest: Boolean = False); overload;
    destructor Destroy; override;

    procedure SetDirectory(const value: Boolean);
    property Directory: Boolean read fDirectory write SetDirectory;

    procedure SetDirType(value: TDirType);
    property DirType: TDirType read fDirType write SetDirType;
    function DirTypeAsString: String;

    function RegenerateSkiplist: Boolean;

    function Useful: Boolean;


  end;

  TDirList = class
  private
    fLastChanged: TDateTime;
    allcdshere: Boolean;
    skiplist: TSkipList;
    sf_d, sf_f: TSkiplistFilter;
    s: String;
    _completeInfo: TCompleteInfo;

    procedure SetSkiplists;
    procedure SetLastChanged(const value: TDateTime);
    class function Timestamp(ts: String): TDateTime;

    procedure SetCompleteInfo(info : TCompleteInfo);
    procedure SetCompleteInfoFromFtpd;
  public
    dirlist_lock: TCriticalSection;

    dirlistadded: Boolean;
    mindenmehetujra: Boolean;

    site_name: String;
    full_path: String;

    error: Boolean;

    need_mkdir: Boolean;
    sfv_status: TdlSFV;

    biggestcd: Integer;

    parent: TDirListEntry;
    entries: TObjectList;
    skipped: TStringList;

    complete_tag: String;

    cache_completed: Boolean;
    cache_hasnfo: Boolean;
    cache_hassfv: Boolean;
    cache_multicd: Boolean;

    date_started: TDateTime;
    date_completed: TDateTime;

    dependency_mkdir: String;

    isSpeedTest: Boolean;
    isFromIrc: Boolean;

    procedure Clear;
    function hasnfo: Boolean;
    function hassfv: Boolean;
    function No_Raceable: Integer;
    function No_Skiplisted: Integer;
    function No_NotSkiplisted: Integer;
    function firstfile: TDateTime;
    function lastfile: TDateTime;
    constructor Create(const site_name: String; parentdir: TDirListEntry; skiplist: TSkipList; SpeedTest: Boolean = False; FromIrc: Boolean = False); overload;
    constructor Create(const site_name: String; parentdir: TDirListEntry; skiplist: TSkipList; const s: String; SpeedTest: Boolean = False; FromIrc: Boolean = False); overload;
    destructor Destroy; override;
    function Depth: Integer;
    function MultiCD: Boolean;
    function Dirname: String;

    procedure Sort;
    procedure SortByModify;

    procedure SetFullPath(const value: String);

    function RegenerateSkiplist: Boolean;

    function Directories: Integer;

    procedure ParseDirlist(s: String);
    function Complete: Boolean;
    function CompleteByTag: Boolean;

    procedure Usefulfiles(out files: Integer; out size: Int64);

    function FindNfo: TDirListEntry;
    function Find(const filename: String): TDirListEntry;

    function FindDirlist(const dirname: String; createit: Boolean = False): TDirList;
    function Done: Integer;
    function RacedByMe(only_useful: boolean = False): Integer;
    function SizeRacedByMe(only_useful: boolean = False): Int64;

    procedure SetCompleteInfoFromIrc();
    function GetCompleteInfo: String;
  published
    property LastChanged: TDateTime read fLastChanged write SetLastChanged;
  end;

procedure DirlistInit;

// make it global to use it in other units with those variables
var
  global_skip: String;
  useful_skip: String;

implementation

uses
  SysUtils, DateUtils, StrUtils, debugunit, mystrings, Math, tags, regexpr, irc, configunit, mrdohutils, console, IdGlobal;

const
  section = 'dirlist';

{$I common.inc}

{ TDirList }
function TDirList.Complete: Boolean;
var
  i: Integer;
  d: TDirlistEntry;
  files: Integer;
  size: Int64;
  tag: String;
  ResultType: String;
begin
  Result := False;
  ResultType := 'Unknown';
  files := 0;
  size := 0;

  // dir has already been seen as complete
  if cache_completed then
  begin
    SetCompleteInfoFromFtpd;
    Result := True;
    exit;
  end;

  if error then
  begin
    // set dir as complete if an error is catched (not sure it's really useful)
    //Result := True;
    //cache_completed := Result;
    //exit;
    if parent <> nil then
      Debug(dpSpam, section, 'TDirlist.Complete ERROR: Site: %s - Dir: %s - DirType: %s', [site_name, full_path, parent.DirTypeAsString])
    else
      Debug(dpSpam, section, 'TDirlist.Complete ERROR: Site: %s - Dir: %s', [site_name, full_path]);
  end;

  if parent <> nil then
  begin
    // we are in a subdirectory
    // Debug(dpSpam, section, 'TDirlist.Complete DEBUG (START): Site: %s - Dir: %s - DirType: %s', [site_name, full_path, parent.DirTypeAsString]);

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
    if ((not Result) and (ResultType = 'Unknown') and (sf_f <> nil) and (sf_f.MatchFile('.sfv') = -1)) then
    begin
      Usefulfiles(files, size);
      Result := ((files <> 0) and (size <> 0));
    end;

    // For debug purposes only
    if (Result) then
    begin
      Debug(dpSpam, section, 'TDirlist.Complete SUBDIR COMPLETE: Site: %s - Dir: %s DirType: %s - ResultType: %s - files: %d size: %d', [site_name, full_path, parent.DirtypeAsString, ResultType, files, size]);
    end
    else
    begin
      //Debug(dpSpam, section, 'TDirlist.Complete SUBDIR INCOMPLETE: Site: %s - Dir: %s DirType: %s - ResultType: %s - files: %d size: %d', [site_name, full_path, parent.DirTypeAsString, ResultType, files, size]);
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
        dirlist_lock.Enter;
        try
          for i := entries.Count - 1 downto 0 do
          begin
            if i < 0 then
              Break;
            try
              d := TDirlistEntry(entries[i]);

              if d = nil then
                Continue;

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
          dirlist_lock.Leave;
        end;
      end;
    end;

    // is the release a special kind of release (dirfix, nfofix, etc.)
    if not Result then
    begin
      //debugunit.Debug(dpError, section, '[DEBUG] SpecialDir START - Site %s - Path: %s', [site_name, full_path]);
      for tag in SpecialDirsTags do
      begin
        if AnsiContainsText(full_path, tag) then
        begin
          // TODO: Maybe add case by case checks instead of considering complete
          debugunit.Debug(dpSpam, section, 'SpecialDir %s contains %s.', [full_path, tag]);
          Result := True;
          Break;
        end;
      end;
    end;

  end;

  // set complete date if not already set
  if ((Result) and (self.date_completed = 0)) then
  begin
    self.date_completed := Now();
  end;

  // avoid further execution of TDirlist.Complete if Result is true
  cache_completed := Result;
end;

constructor TDirList.Create(const site_name: String; parentdir: TDirListEntry; skiplist: TSkipList; SpeedTest: boolean = False; FromIrc: boolean = False);
begin
  Create(site_name, parentdir, skiplist, '', SpeedTest, FromIrc);
end;

constructor TDirList.Create(const site_name: String; parentdir: TDirListEntry; skiplist: TSkipList; const s: String; SpeedTest: boolean = False; FromIrc: boolean = False);
var
  sf: TSkipListFilter;
begin
  dirlist_lock := TCriticalSection.Create;

  biggestcd:= 0;
  self.error := False;

  self.need_mkdir := True;

  self.cache_completed := False;

  self.date_started := 0;
  self.date_completed := 0;

  self.site_name := site_name;
  self.full_path := 'Not set';
  _completeInfo := NotComplete;

  fLastChanged := Now();
  allcdshere := False;
  entries := TObjectList.Create;
  skipped := TStringList.Create;
  skipped.CaseSensitive := False;
  self.parent := parentdir;

  self.s := s;
  self.skiplist := skiplist;
  SetSkiplists;

  self.isSpeedTest := SpeedTest;
  self.isFromIrc := FromIrc;

  sfv_status := dlSFVUnknown;
  if skiplist <> nil then
  begin
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
    ParseDirlist(s);
end;

destructor TDirList.Destroy;
begin
  entries.Free;
  dirlist_lock.Free;
  skipped.Free;
  inherited;
end;

procedure TDirList.SetSkiplists;
var s: String;
begin
  s:= Dirname;
  if skiplist <> nil then
  begin
    sf_f := skiplist.FindFileFilter(s);
    sf_d := skiplist.FindDirFilter(s);
  end else
  begin
    sf_f := nil;
    sf_d := nil;
  end;
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
    biggestcd := 0;
    Result := False;
    s := '';
    // megnezzuk van e CD1 CD2 stb jellegu direktorink
    dirlist_lock.Enter;
    try
      for i := entries.Count - 1 downto 0 do
      begin
        if i < 0 then Break;
        try
          de := TDirListEntry(entries[i]);

          if de.cdno <> 0 then
          begin
            Result := True;
            s := s + IntToStr(de.cdno);

            if de.cdno > biggestcd then
              biggestcd := de.cdno;
          end;
        except
          on e: Exception do
          begin
            debugunit.Debug(dpError, section, '[EXCEPTION] TDirList.MultiCD: %s', [e.Message]);
            Continue;
          end;
        end;
      end;
    finally
      dirlist_lock.Leave;
    end;

    if biggestcd > 1 then
    begin
      allcdshere := True;
      for i := 1 to biggestcd do
        if (0 = Pos(IntToStr(i), s)) then
        begin
          allcdshere := False;
          Break;
        end;
    end;
  end
  else
  begin
    Result := parent.dirlist.MultiCD;
  end;
end;

function TDirList.No_NotSkiplisted: Integer;
begin
  Result := entries.Count - No_Skiplisted;
end;

function TDirList.No_Raceable: Integer;
var
  i: Integer;
begin
  Result := 0;

  dirlist_lock.Enter;
  try
    for i := entries.Count - 1 downto 0 do
    begin
      if i < 0 then Break;
      try
        if ((not TDirListEntry(entries[i]).skiplisted) and (not TDirListEntry(entries[i]).done)) then
        begin
          inc(Result);
        end;
      except
        on e: Exception do
        begin
          debugunit.Debug(dpError, section, '[EXCEPTION] TDirList.No_Raceable: %s', [e.Message]);
          Continue;
        end;
      end;
    end;
  finally
    dirlist_lock.Leave;
  end;
end;

function TDirList.No_Skiplisted: Integer;
var
  i: Integer;
begin
  Result := 0;

  dirlist_lock.Enter;
  try
    for i := entries.Count - 1 downto 0 do
    begin
      if i < 0 then Break;
      try
        if TDirListEntry(entries[i]).skiplisted then
        begin
          inc(Result);
        end;
      except
        on e: Exception do
        begin
          debugunit.Debug(dpError, section, '[EXCEPTION] TDirList.No_Skiplisted: %s', [e.Message]);
          Continue;
        end;
      end;
    end;
  finally
    dirlist_lock.Leave;
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


procedure TDirList.ParseDirlist(s: String);
var
  tmp: String;
  akttimestamp: TDateTime;
  site: TSite;
  skip_being_uploaded_files: boolean;
  de: TDirListEntry;
  added: Boolean;
  dirmaszk, username, groupname, datum, filename: String;
  filesize: Int64;
  i, j: Integer;
  rrgx, splx: TRegExpr;
begin
  added := False;

  // No need to parse the dir again if it's complete
  if cache_completed then exit;

  debugunit.Debug(dpSpam, section, Format('--> ParseDirlist (%d entries)', [entries.Count]));

  site := FindSiteByName('', site_name);
  if site = nil then
  begin
    // should never happen
    Debug(dpError, section, 'ERROR: Can''t lookup site  %s. Using defaults.', [site_name]);
    skip_being_uploaded_files := config.ReadBool(section, 'skip_being_uploaded_files', False);
  end
  else
  begin
    skip_being_uploaded_files := site.SkipBeingUploadedFiles;
  end;

  dirlist_lock.Enter;
  try
    for i := entries.Count - 1 downto 0 do
    begin
      if i < 0 then Break;
      try
        de := TDirlistEntry(entries[i]);
        de.megvanmeg := False;
      except
        on e: Exception do
        begin
          debugunit.Debug(dpError, section, '[EXCEPTION] TDirList.ParseDirlist: %s', [e.Message]);
          Continue;
        end;
      end;
    end;
  finally
    dirlist_lock.Leave;
  end;

  rrgx := TRegExpr.Create;
  rrgx.ModifierI := True;
  rrgx.Expression := global_skip;

  dirlist_lock.Enter;
  try
    while(true) do
    begin
      tmp := trim(Elsosor(s));

      if tmp = '' then break;

      //drwxrwxrwx   2 nete     Death_Me     4096 Jan 29 05:05 Whisteria_Cottage-Heathen-RERIP-2009-pLAN9
      if (length(tmp) > 11) then
      begin
        if ((tmp[1] <> 'd') and (tmp[1] <> '-') and (tmp[11] = ' ')) then
          continue;

        dirmaszk := Fetch(tmp, ' ', True, False); // dir mask
        tmp := TrimLeft(tmp);
        Fetch(tmp, ' ', True, False); // No. of something
        tmp := TrimLeft(tmp);
        username := Fetch(tmp, ' ', True, False); // username
        tmp := TrimLeft(tmp);
        groupname := Fetch(tmp, ' ', True, False); // groupname
        tmp := TrimLeft(tmp);
        filesize := StrToInt64Def(Fetch(tmp, ' ', True, False), -1); // filesize

        if filesize < 0 then
          Continue;

        datum := Fetch(tmp, ' ', True, False);
        tmp := TrimLeft(tmp);
        datum := datum + ' ' + Fetch(tmp, ' ', True, False);
        tmp := TrimLeft(tmp);
        datum := datum + ' ' + Fetch(tmp, ' ', True, False); // date and time
        filename := Trim(tmp); // file or dirname

        if filename = '' then
          Continue;

        if ((filename = '.') or (filename = '..') or (filename[1] = '.')) then
          continue;

        if rrgx.Exec(filename) then
        begin
          //debugunit.Debug(dpMessage, section, Format('[iNFO] --> ParseDirlist skip: %s', [filename]));
          Continue;
        end;

        // Do not filter if we call the dirlist from irc
        if not isFromIrc then
        begin
          // Dont add complete tags to dirlist entries
          if ((dirmaszk[1] = 'd') or (filesize < 1)) then
          begin
            j := TagComplete(filename);
            if (j <> 0) then
            begin
              complete_tag := filename;
              Continue;
            end;
          end;

          // entry is a dir with unwanted characters
          if ((dirmaszk[1] = 'd') and (AnsiMatchText(filename, ['[', ']', ',', '=']))) then
          begin
            Continue;
          end;

          // file is flagged as skipped
          if (skipped.IndexOf(filename) <> -1) then
          begin
            Continue;
          end;

          // entry is a file and is 0 byte
          if ((dirmaszk[1] <> 'd') and (filesize < 1)) then
          begin
            Continue;
          end;

          // entry is a file and is not downlodable
          if ((dirmaszk[1] <> 'd') and ((dirmaszk[5] <> 'r') and (dirmaszk[8] <> 'r'))) then
          begin
            Continue;
          end;

          // entry is a file and is being uploaded (glftpd only?)
          if skip_being_uploaded_files then
          begin
            if ((dirmaszk[1] <> 'd') and ((dirmaszk[7] = 'x') and (dirmaszk[10] = 'x'))) then
            begin
              Continue;
            end;
          end;
        end;

        akttimestamp := Timestamp(datum);

        de := Find(filename);
        if de = nil then
        begin
          de := TDirListEntry.Create(filename, self);

          if ((AnsiLowerCase(de.Extension) = '.sfv') and (hassfv)) then
          begin
            de.Free;
            Continue;
          end;
          if ((AnsiLowerCase(de.Extension) = '.nfo') and (hasnfo)) then
          begin
            de.Free;
            Continue;
          end;

          de.username := username;
          de.groupname := groupname;
          de.timestamp := akttimestamp;
          de.done := True;
          de.justadded := True;
          de.directory := (dirmaszk[1] = 'd');

          if not de.directory then
            de.filesize := filesize;

          // Do not filter if we call the dirlist from irc
          if not isFromIrc then
          begin
            if (de.directory) then
            begin
              // check if we have a special kind of subdirectory
              de.DirType := IsUnknown;
              splx := TRegExpr.Create;
              splx.ModifierI := True;
              try
                try
                  splx.Expression := '^sample$';
                  if (splx.Exec(filename)) then
                    de.DirType := IsSample;
                  splx.Expression := '^proof$';
                  if (splx.Exec(filename)) then
                    de.DirType := IsProof;
                  splx.Expression := '^(sub|subs)$';
                  if(splx.Exec(filename)) then
                    de.DirType := IsSubs;
                  splx.Expression := '^(cover|covers)$';
                  if (splx.Exec(filename)) then
                    de.DirType := IsCovers;
                except
                  on e: Exception do
                    debugunit.Debug(dpError, section, '[EXCEPTION] TDirList.ParseDirlist (DirType): %s', [e.Message]);
                end;
              finally
                splx.Free;
              end;
            end;

            if ((not de.Directory) and (de.Extension = '') and (not isSpeedTest)) then
            begin
              de.Free;
              Continue;
            end;

            if ((not de.Directory) and (not (de.filesize > 0))) then
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

          if ((not de.Directory) and (AnsiLowerCase(de.Extension) = '.sfv') and (de.filesize > 0)) then
          begin
            sfv_status := dlSFVFound;
          end;

          if (de.Directory) then
          begin
            de.subdirlist := TDirlist.Create(site_name, de, skiplist, isSpeedTest, isFromIrc);
            if de.subdirlist <> nil then
              de.subdirlist.SetFullPath(MyIncludeTrailingSlash(full_path) + de.filename);
          end;

          if (self.date_started = 0) then
          begin
            self.date_started := Now();
          end;

          entries.Add(de);

          LastChanged := Now();
          added := True;
        end
        else if (de.filesize <> filesize) then
        begin
          if ((de.filesize <> filesize) or (de.username <> username)) then
          begin
            LastChanged := Now();
          end;

          de.filesize := filesize;
          de.timestamp := akttimestamp;
          de.username := username;
          de.groupname := groupname;
        end;
        de.megvanmeg := True;
      end;
    end;

  finally
    dirlist_lock.Leave;
    rrgx.Free;
  end;

  // entries found means the dir exists
  if ((need_mkdir) and (entries.Count > 0)) then
    need_mkdir := False;

  if parent = nil then
  begin
    try
      SetSkiplists;
    except
      on E: Exception do
      begin
        debugunit.Debug(dpError, section, '[EXCEPTION] TDirList.ParseDirList (SetSkiplists): %s', [e.Message]);
      end;
    end;
  end;

  // set defaults values if direcotry was just added
  if added then
  begin
    cache_completed := False;
    cache_multicd := False;
    allcdshere := False;

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

      try
        Sort;
      except
        on E: Exception do
        begin
          debugunit.Debug(dpError, section, '[EXCEPTION] TDirList.ParseDirList(Sort): %s', [e.Message]);
        end;
      end;

    end;
  end;

  debugunit.Debug(dpSpam, section, Format('<-- ParseDirlist (%d entries)', [entries.Count]));
end;

function TDirList.RegenerateSkiplist: Boolean;
var i: Integer;
    ld: TDirListEntry;
begin
  Result := False;
  if skiplist = nil then exit;

  dirlist_lock.Enter;
  try
    for i:= entries.Count -1 downto 0 do
    begin
      if i < 0 then Break;
      try
        ld:= TDirListEntry(entries[i]);
        if ld.RegenerateSkiplist then Result := True;
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

function DirListSorter(Item1, Item2: Pointer; dirtype: TDirType): Integer;
var
    i1, i2: TDirlistEntry;
    c1, c2: Integer;
    i1IsImage, i1IsVideo: Boolean;
    i2IsImage, i2IsVideo: Boolean;
    image_files_priority, video_files_priority: Integer;

begin
  (*
     Result := -1 (i1 wins)
            := 1  (i2 wins)
            := 0  (no change)
  *)
  Result := 0;

  try
    i1 := TDirlistEntry(Item1);
    i2 := TDirlistEntry(Item2);

    // We don't care about skiplisted entries
    if ((i1.skiplisted) and (i2.skiplisted)) then exit;

    // At least one file need to have an extension for extention sorting
    if (i1.Extension <> '') or (i2.Extension <> '') then
    begin
      // sfv priority
      if ((AnsiLowerCase(i1.Extension) = '.sfv') and (AnsiLowerCase(i2.Extension) <> '.sfv')) then
      begin
        Result := -1;
        exit;
      end;
      if ((AnsiLowerCase(i1.Extension) <> '.sfv') and (AnsiLowerCase(i2.Extension) = '.sfv')) then
      begin
        Result := 1;
        exit;
      end;

      // nfo priority
      if ((AnsiLowerCase(i1.Extension) = '.nfo') and (AnsiLowerCase(i2.Extension) <> '.nfo')) then
      begin
        Result := -1;
        exit;
      end;
      if ((AnsiLowerCase(i1.Extension) <> '.nfo') and (AnsiLowerCase(i2.Extension) = '.nfo')) then
      begin
        Result := 1;
        exit;
      end;

      // image files priority (i.e.: proofs, covers)
      i1IsImage := AnsiMatchText(i1.Extension, ImageFileExtensions);
      i2IsImage := AnsiMatchText(i2.Extension, ImageFileExtensions);
      if (i1IsImage) or (i2IsImage) then
      begin
        image_files_priority := config.ReadInteger('queue', 'image_files_priority', 2);
        if (image_files_priority > 0) and (image_files_priority <= 2) then
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

        //Debug(dpSpam, section, 'DirListSorter (image): i1: %s i2: %s result: %d', [i1.Extension, i2.Extension, Result]);
        exit;
      end;

      // video files priority
      i1IsVideo := AnsiMatchText(i1.Extension, VideoFileExtensions);
      i2IsVideo := AnsiMatchText(i2.Extension, VideoFileExtensions);
      if (i1IsVideo) or (i2IsVideo) then
      begin
        video_files_priority := config.ReadInteger('queue', 'video_files_priority', 2);
        if (video_files_priority > 0) and (video_files_priority <= 2) then
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

        //Debug(dpSpam, section, 'DirListSorter (video): i1: %s i2: %s result: %d', [i1.Extension, i2.Extension, Result]);
        exit;
      end;
    end;

    // If not sorting occured yet - try default sorting
    // Sorting two directories
    if Result = 0 then
    begin
      if ((i1.directory) and (i2.directory)) then
      begin
        if (i1.dirlist.sf_d <> nil) then
        begin
          c1 := i1.dirlist.sf_d.MatchFile(i1.filename);
          c2 := i2.dirlist.sf_d.MatchFile(i2.filename);

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
        c1 := i1.dirlist.sf_f.MatchFile(i1.filename);
        c2 := i2.dirlist.sf_f.MatchFile(i2.filename);

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
      debugunit.Debug(dpError, section, '[EXCEPTION] DirListSorter: %s', [e.Message]);
  end;
end;

function DirListModSorter(Item1, Item2: Pointer): Integer;
var i1, i2: TDirlistEntry;
begin
  // compare: -1 bekenhagyas, jo a sorrend     ~ good order
  // compare:  1 exchange
  i1 := TDirlistEntry(Item1);
  i2 := TDirlistEntry(Item2);

  Result:= CompareValue(i2.timestamp, i1.timestamp);
  (*
    if i1.timestamp > i2.timestamp then
      Result:= -1
    else
    if i1.timestamp < i2.timestamp then
      Result:= 1;
  *)
end;

procedure TDirList.Sort();
begin
  dirlist_lock.Enter;
  try
    try
      entries.Sort(@DirListSorter);
    except
      on E: Exception do
      begin
        debugunit.Debug(dpError, section, '[EXCEPTION] TDirList.Sort (DirListSorter): %s', [e.Message]);
      end;
    end;
  finally
    dirlist_lock.Leave;
  end;
end;

function TDirList.CompleteByTag: Boolean;
var i: Integer;
begin
  Result:= False;
  if (complete_tag = '') then
    exit;

  i:= TagComplete(complete_tag);
  if (i = 1) then
  begin
    Result:= True;
    exit;
  end;
end;

procedure TDirList.Usefulfiles(out files: Integer; out size: Int64);
var i: Integer;
    de: TDirlistEntry;
    afiles: Integer;
    asize: Int64;
begin
  asize := 0;
  afiles := 0;
  files := 0;
  size := 0;

  dirlist_lock.Enter;
  try
    for i := entries.Count -1 downto 0 do
    begin
      if i < 0 then Break;
      try
        de := TDirlistEntry(entries[i]);
        if de.skiplisted then Continue;

        if de.Useful then
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
    dirlist_lock.Leave;
  end;
end;

function TDirList.Find(const filename: String): TDirListEntry;
var
  i: Integer;
  de: TDirListEntry;
begin
  Result := nil;
  if entries.Count = 0 then
    exit;

  dirlist_lock.Enter;
  try
    for i := entries.Count - 1 downto 0 do
    begin
      if i < 0 then Break;
      try
        de := TDirListEntry(entries[i]);
        if (AnsiUpperCase(de.filename) = AnsiUpperCase(filename)) then
        begin
          Result := de;
          Break;
        end;
      except
        on e: Exception do
        begin
          debugunit.Debug(dpError, section, '[EXCEPTION] TDirList.Find: %s', [e.Message]);
          Continue;
        end;
      end;
    end;
  finally
    dirlist_lock.Leave;
  end;
end;

procedure TDirList.SetLastChanged(const value: TDateTime);
begin
  fLastChanged := Max(value, fLastChanged);
  if parent <> nil then
    parent.dirlist.LastChanged := fLastChanged;
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

    d := Find(firstdir);
    if d = nil then
    begin
      if not createit then
      begin
        exit;
      end;
      d := TDirListEntry.Create(firstdir, self);
      d.Directory := True;
      entries.Add(d);
    end;

    if (not d.Directory) then
    begin
      exit;
    end;

    if d.subdirlist = nil then
    begin
      d.subdirlist := TDirlist.Create(site_name, d, skiplist);
      if d.subdirlist <> nil then
        d.subdirlist.SetFullPath(MyIncludeTrailingSlash(self.full_path) + d.filename);
    end;
  except
    on E: Exception do
    begin
      debugunit.Debug(dpError, section, 'TDirList.FindDirlist: %s', [e.Message]);
      exit;
    end;
  end;
  Result := d.subdirlist.FindDirlist(lastdir, createit);
end;

function TDirList.Done: Integer;
var
  de: TDirlistEntry;
  i: Integer;
begin
  Result := 0;

  dirlist_lock.Enter;
  try
    for i := entries.Count - 1 downto 0 do
    begin
      if i < 0 then Break;
      try
       de := TDirlistEntry(entries[i]);
        if de.skiplisted then Continue;
        if de.done then inc(Result);
        if ((de.directory) and (de.subdirlist <> nil)) then
          inc(Result, de.subdirlist.Done);
      except
        on E: Exception do
        begin
          debugunit.Debug(dpError, section, 'TDirList.Done: %s', [e.Message]);
          Continue;
        end;
      end;
    end;
  finally
    dirlist_lock.Leave;
  end;
end;

function TDirList.RacedByMe(only_useful: boolean = False): Integer;
var
  de: TDirlistEntry;
  i: Integer;
begin
  Result := 0;

  dirlist_lock.Enter;
  try
    for i := entries.Count - 1 downto 0 do
    begin
      if i < 0 then Break;
      try
        de := TDirlistEntry(entries[i]);
        if only_useful then
        begin
          if (de.racedbyme and de.Useful) then inc(Result);
        end
        else
        begin
          if de.racedbyme then inc(Result);
        end;
        if ((de.directory) and (de.subdirlist <> nil)) then
        begin
          inc(Result, de.subdirlist.RacedbyMe(only_useful));
        end;
      except
        on E: Exception do
        begin
          debugunit.Debug(dpError, section, 'TDirList.RacedByMe: %s', [e.Message]);
          Continue;
        end;
      end;
    end;
  finally
    dirlist_lock.Leave;
  end;
end;

function TDirList.SizeRacedByMe(only_useful: boolean = False): Int64;
var
  de: TDirlistEntry;
  i: Integer;
begin
  Result:= 0;

  dirlist_lock.Enter;
  try
    for i := entries.Count - 1 downto 0 do
    begin
      if i < 0 then Break;
      try
        de := TDirlistEntry(entries[i]);
        if only_useful then
        begin
          if (de.racedbyme and de.Useful) then inc(result,de.filesize);
        end
        else
        begin
          if de.racedbyme then inc(result,de.filesize);
        end;
        if ((de.directory) and (de.subdirlist <> nil)) then inc(result,de.subdirlist.SizeRacedByMe(only_useful));
      except
        on E: Exception do
        begin
          debugunit.Debug(dpError, section, 'TDirList.SizeRacedByMe: %s', [e.Message]);
          Continue;
        end;
      end;
    end;
  finally
    dirlist_lock.Leave;
  end;
end;


function TDirList.hassfv: boolean;
var
  i: Integer;
  de: TDirlistEntry;
begin
  Result := False;
  if (self.cache_hassfv) then
  begin
    Result := True;
    exit;
  end;

  dirlist_lock.Enter;
  try
    for i := entries.Count - 1 downto 0 do
    begin
      if i < 0 then Break;
      try de := TDirlistEntry(entries[i]);
        if ((AnsiLowerCase(de.Extension) = '.sfv') and (de.megvanmeg) and (de.filesize > 0)) then
        begin
          Result := True;
          Self.cache_hassfv := True;
          exit;
        end;
      except
        on E: Exception do
        begin
          debugunit.Debug(dpError, section, 'TDirList.hassfv: %s', [e.Message]);
          Continue;
        end;
      end;
    end;
  finally
    dirlist_lock.Leave;
  end;
end;

function TDirList.hasnfo: boolean;
var
  i: Integer;
  de: TDirlistEntry;
begin
  Result := False;
  if (self.cache_hasnfo) then
  begin
    Result := True;
    exit;
  end;

  dirlist_lock.Enter;
  try
    for i := entries.Count - 1 downto 0 do
    begin
      if i < 0 then Break;
      try
        de := TDirlistEntry(entries[i]);
        if ((AnsiLowerCase(de.Extension) = '.nfo') and (de.megvanmeg) and (de.filesize > 0)) then
        begin
          Result := True;
          Self.cache_hasnfo := True;
          exit;
        end;
      except
        on E: Exception do
        begin
          debugunit.Debug(dpError, section, 'TDirList.hasnfo: %s', [e.Message]);
          Continue;
        end;
      end;
    end;
  finally
    dirlist_lock.Leave;
  end;
end;


procedure TDirList.Clear;
var
  i: Integer;
begin
  allcdshere := False;
  fLastChanged := 0;
  biggestcd := 0;

  dirlist_lock.Enter;
  try

    for i := entries.Count - 1 downto 0 do
    begin
      try if i < 0 then Break; except Break; end;
      try
        TDirlistEntry(entries[i]).megvanmeg := False;
        TDirlistEntry(entries[i]).error := False;
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

procedure TDirList.SortByModify;
begin
  dirlist_lock.Enter;
  try
    try
      entries.Sort(@DirListModSorter);
    except
      on E: Exception do
      begin
        debugunit.Debug(dpError, section, '[EXCEPTION] TDirList.SortByModify (DirListModSorter): %s', [e.Message]);
      end;
    end;
  finally
    dirlist_lock.Leave;
  end;
end;

function TDirList.FindNfo: TDirListEntry;
var de: TDirlistEntry;
    i: Integer;
begin
  Result := nil;

  dirlist_lock.Enter;
  try
    for i := 0 to entries.Count - 1 do
    begin
      if i < 0 then Break;
      try de := TDirlistEntry(entries[i]);

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
    dirlist_lock.Leave;
  end;
end;

function TDirList.Directories: Integer;
var i: Integer;
    de: TDirlistEntry;
begin
  Result := 0;

  dirlist_lock.Enter;
  try
    for i := entries.Count -1 downto 0 do
    begin
      if i < 0 then Break;
      try
        de := TDirlistEntry(entries[i]);
        if ((de.directory) and (not de.skiplisted) and (de.timestamp <> 0)) then
          inc(result);
      except
        on E: Exception do
        begin
          debugunit.Debug(dpError, section, '[EXCEPTION] TDirList.Directories: %s', [e.Message]);
          Continue;
        end;
      end;
    end;
  finally
    dirlist_lock.Leave;
  end;
end;

function TDirList.firstfile: TDateTime;
var i: Integer;
    de: TDirlistEntry;
    t: TDateTime;
begin
  Result := 0;

  dirlist_lock.Enter;
  try
    for i:= entries.Count -1 downto 0 do
    begin
      if i < 0 then Break;
      try
        de := TDirlistEntry(entries[i]);
        if (de.timestamp <> 0) and (not de.skiplisted) then
        begin
          if ((Result = 0) or (Result > de.timestamp)) then
            Result := de.timestamp;

          if ((de.Directory) and (de.subdirlist <> nil)) then
          begin
            t := de.subdirlist.firstfile;
            if ((t <> 0) and (Result > t)) then
              Result := t;
          end;
        end;
      except
        on E: Exception do
        begin
          debugunit.Debug(dpError, section, '[EXCEPTION] TDirList.firstfile: %s', [e.Message]);
          Continue;
        end;
      end;
    end;
  finally
    dirlist_lock.Leave;
  end;
end;

function TDirList.lastfile: TDateTime;
var i: Integer;
    de: TDirlistEntry;
    t: TDateTime;
begin
  Result := 0;

  dirlist_lock.Enter;
  try
    for i := entries.Count -1 downto 0 do
    begin
      if i < 0 then Break;
      try
        de := TDirlistEntry(entries[i]);
        if (de.timestamp <> 0) and (not de.skiplisted) then
        begin
          if ((Result = 0) or (Result < de.timestamp)) then
            Result := de.timestamp;

          if ((de.Directory) and (de.subdirlist <> nil)) then
          begin
            t := de.subdirlist.lastfile;
            if ((t <> 0) and (Result < t)) then
              Result := t;
          end;
        end;
      except
        on E: Exception do
        begin
          debugunit.Debug(dpError, section, '[EXCEPTION] TDirList.firstfile: %s', [e.Message]);
          Continue;
        end;
      end;
    end;
  finally
    dirlist_lock.Leave;
  end;
end;

procedure TDirList.SetFullPath(const value: String);
begin
  self.full_path := value;
end;

procedure TDirList.SetCompleteInfoFromIrc;
begin
  SetCompleteInfo(FromIrc);
end;

procedure TDirList.SetCompleteInfoFromFtpd;
begin
  SetCompleteInfo(FromFtpd);
end;

procedure TDirList.SetCompleteInfo(info : TCompleteInfo);
begin
  if (_completeInfo <> FromFtpdAndIrc) then
  begin
    if ((_completeInfo = NotComplete) or ((_completeInfo <> FromFtpdAndIrc) and (info <> _completeInfo))) then
      _completeInfo := TCompleteInfo(ord(_completeInfo) + ord(info));
    if (date_completed = 0) and (_completeInfo in [FromIrc, FromFtpdAndIrc]) then
      date_completed := Now();
  end;
end;

function TDirList.GetCompleteInfo: String;
begin
  Result := 'UNKNOWN!';

  case TCompleteInfo(_completeInfo) of
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

constructor TDirListEntry.Create(const filename: String; dirlist: TDirList; SpeedTest: Boolean = False);
begin
  addedfrom := TStringList.Create;

  self.tradeCount := 0;
  self.DirType := IsMain;

  self.sfvfirsteventvoltmar := False;
  self.dirlist := dirlist;
  self.filename := filename;
  self.done := False;
  self.skiplisted := False;
  self.megvanmeg := False;
  self.error := False;
  subdirlist := nil;

  filenamelc := LowerCase(filename);
  cdno := 0;
end;

constructor TDirListEntry.Create(de: TDirlistEntry; dirlist: TDirList; SpeedTest: Boolean = False);
begin
  addedfrom := TStringList.Create;

  self.tradeCount := 0;
  self.DirType := de.DirType;

  self.sfvfirsteventvoltmar := False;
  self.filename := de.filename;
  self.filesize := de.filesize;

  self.directory := de.directory;
  self.DirType := de.DirType;

  self.done := False;
  self.skiplisted := de.skiplisted;
  self.dirlist := dirlist;
  self.subdirlist := nil;
  self.timestamp := de.timestamp;
  self.megvanmeg := False;
  self.error := False;
  self.justadded := True;
  filenamelc := LowerCase(filename);

  if self.directory then CalcCDNumber;
end;

destructor TDirListEntry.Destroy;
begin
  FreeAndNil(subdirlist);
  addedFrom.Free;
  inherited;
end;

procedure TDirListEntry.CalcCDNumber;
const multicddirprefix : array[1..4] of String = ('cd', 'dvd', 'disc','disk');
var
  s: String;
  i: Integer;
begin

  s := ReplaceText(filenamelc, ' ', '');
  s := ReplaceText(s, '_', '');
  s := ReplaceText(s, '-', '');

  for i := 1 to 4 do
  begin
    if (1 = AnsiPos(AnsiUpperCase(multicddirprefix[i]), AnsiUpperCase(s))) then
    begin
      cdno := StrToIntDef(Copy(s, Length(multicddirprefix[i]) + 1, 1000), 0);
      exit;
    end;
  end;
end;

function TDirListEntry.Extension: String;
begin
  Result := ExtractFileExt(filenamelc);
end;

function TDirListEntry.Useful: Boolean;
var
  rrgx: TRegExpr;
begin
  Result := False;

  if filesize = 0 then exit;
  if directory then exit;

  rrgx := TRegExpr.Create;
  rrgx.ModifierI := True;
  rrgx.Expression := useful_skip;
  try
    try
      if not rrgx.Exec(filename) then
      begin
        Result := True;
        exit;
      end;
    except
      on E: Exception do
      begin
        debugunit.Debug(dpError, section, '[EXCEPTION] TDirListEntry.Useful: %s', [e.Message]);
      end;
    end;
  finally
    rrgx.Free;
  end;
end;


procedure TDirListEntry.SetDirectory(const value: Boolean);
begin
  fDirectory := value;
  if directory then
    CalcCDNumber;
end;

procedure TDirListEntry.SetDirType(value: TDirType);
begin
  fDirType := value;
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

function TDirListEntry.RegenerateSkiplist: Boolean;
var
  l, ldepth: Integer;
  s: String;
  sf: TSkipListFilter;
begin
  Result := False;

  if dirlist.skiplist = nil then exit;

  if ( not skiplisted ) then
  begin
    if not directory then
    begin
      s := dirlist.Dirname;

      // first we look for ftprush screwed up files like (1).nfo
      l := length(filename);
      if l > length(Extension) + 6 then
      begin
        // first one is for 3 chars in extension like .nfo, .rar, .mp3, .r02 and second one is for 4 chars like .flac
        if ( (filename[l-6] = '(') and (filename[l-4] = ')') and (filename[l-5] in ['0'..'9']) ) or ( (filename[l-7] = '(') and (filename[l-5] = ')') and (filename[l-6] in ['0'..'9']) ) then
        begin
          skiplisted := True;
          dirlist.skipped.Add(filename);
          irc_Addtext_by_key('SKIPLOG', Format('<c2>[SKIP]</c> FTPRush screwed up file %s %s %s : %s', [dirlist.site_name, dirlist.skiplist.sectionname, s, filename]));
          exit;
        end;
      end;

      sf := dirlist.skiplist.AllowedFile(s, filename);

      if sf = nil then
      begin
        skiplisted := True;
        dirlist.skipped.Add(filename);
        irc_Addtext_by_key('SKIPLOG', Format('<c2>[SKIP]</c> Not AllowedFile %s %s %s : %s', [dirlist.site_name, dirlist.skiplist.sectionname, s, filename]));
      end
      else
      begin
        Result := True;
      end;
    end
    else
    begin
      ldepth := dirlist.Depth();

      if ldepth < dirlist.skiplist.dirdepth then
      begin
        // you have to go through the alloweddirs and check if it's allowed
        s := dirlist.Dirname;
        sf := dirlist.skiplist.AllowedDir(s, filename);
        if sf = nil then
        begin
          skiplisted := True;
          dirlist.skipped.Add(filename);
          irc_Addtext_by_key('SKIPLOG', Format('<c2>[SKIP]</c> Not AllowedDir %s %s : %s', [dirlist.site_name, dirlist.skiplist.sectionname, filename]));
        end
        else
        begin
          Result := True;
        end;
      end
      else
      begin
        irc_Addtext_by_key('SKIPLOG', Format('<c2>[SKIP]</c> dirdepth %s %s : %s', [dirlist.site_name, dirlist.skiplist.sectionname, filename]));
        skiplisted := True;
      end;
    end;
  end;
end;

procedure DirlistInit;
begin
  global_skip := config.ReadString(section, 'global_skip', '\-missing$|\-offline$|^\.|^file\_id\.diz$|\.htm$|\.html|\.bad$|([^\w].*DONE\s\-\>\s\d+x\d+[^\w]*)');
  useful_skip := config.ReadString(section, 'useful_skip', '\.nfo$|\.sfv$|\.m3u$|\.cue$');
end;

end.
