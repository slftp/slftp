unit dirlist;

interface

uses Classes, Contnrs, SyncObjs, sitesunit, skiplists, globals;

type
  TdlSFV = (dlSFVUnknown, dlSFVNoNeed, dlSFVFound, dlSFVNotFound);

  TDirlist = class;

  TDirListEntry = class
    dirlist: TDirList;

    megvanmeg: Boolean;
    justadded: Boolean;
    error: Boolean; //< { @true if file cannot be send, will be skiped then, @false otherwise. }

    username: AnsiString;
    groupname: AnsiString;

    fDirectory: Boolean; //< current dir is a directory
    fDirType: TDirType; //< Indicates what kind of Directory the current dir is

    subdirlist: TDirList;

    filename: AnsiString; //< filename
    filenamelc: AnsiString; //< lowercase filename
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
    function Extension: AnsiString;

    constructor Create(filename: AnsiString; dirlist: TDirList; SpeedTest: Boolean = False); overload;
    constructor Create(de: TDirlistEntry; dirlist: TDirList; SpeedTest: Boolean = False); overload;
    destructor Destroy; override;

    procedure SetDirectory(value: Boolean);
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
    s: AnsiString;

    procedure SetSkiplists;
    procedure SetLastChanged(value: TDateTime);
    class function Timestamp(ts: AnsiString): TDateTime;
  public
    dirlistadded: Boolean;
    mindenmehetujra: Boolean;

    site_name: AnsiString;
    full_path: AnsiString;

    error: Boolean;

    need_mkdir: Boolean;
    sfv_status: TdlSFV;

    biggestcd: Integer;

    parent: TDirListEntry;
    entries: TObjectList;
    skiped: TStringList;

    complete_tag: AnsiString;

    cache_completed: Boolean;
    cache_hasnfo: Boolean;
    cache_hassfv: Boolean;
    cache_multicd: Boolean;

    date_started: TDateTime;
    date_completed: TDateTime;

    dependency_mkdir: AnsiString;

    isSpeedTest: Boolean;

    procedure Clear;
    function hasnfo: Boolean;
    function hassfv: Boolean;
    function No_Raceable: Integer;
    function No_Skiplisted: Integer;
    function No_NotSkiplisted: Integer;
    function firstfile: TDateTime;
    function lastfile: TDateTime;
    constructor Create(site_name: AnsiString; parentdir: TDirListEntry; skiplist: TSkipList; SpeedTest: Boolean = False); overload;
    constructor Create(site_name: AnsiString; parentdir: TDirListEntry; skiplist: TSkipList; s: AnsiString; SpeedTest: Boolean = False); overload;
    destructor Destroy; override;
    function Depth: Integer;
    function MultiCD: Boolean;
    function Dirname: AnsiString;

    procedure Sort;
    procedure SortByModify;

    procedure SetFullPath(value: AnsiString);

    function RegenerateSkiplist: Boolean;

    function Directories: Integer;

    procedure ParseDirlist(s: AnsiString);
    function Complete: Boolean;
    function CompleteByTag: Boolean;

    procedure Usefulfiles(out files: Integer; out size: Int64);

    function FindNfo: TDirListEntry;
    function Find(filename: AnsiString): TDirListEntry;

    function FindDirlist(dirname: AnsiString; createit: Boolean = False): TDirList;
    function Done: Integer;
    function RacedByMe(only_useful: boolean = False): Integer;
    function SizeRacedByMe(only_useful: boolean = False): Int64;
  published
    property LastChanged: TDateTime read fLastChanged write SetLastChanged;
  end;

procedure  DirlistInit;
procedure  DirlistUninit;

// make it global to use it in other units with those variables
var
  global_skip: AnsiString;
  useful_skip: AnsiString;

implementation

uses SysUtils, DateUtils, StrUtils, debugunit, mystrings, Math, tags, regexpr, irc, configunit, mrdohutils, console;

const section = 'dirlist';

{$I common.inc}

{ TDirList }
function TDirList.Complete: Boolean;
var
  i: Integer;
  d: TDirlistEntry;
  files: Integer;
  size: Int64;
  tag: AnsiString;
  ResultType: AnsiString;
begin
  Result := False;
  ResultType := 'Unknown';
  files := 0;
  size := 0;

  // dir has already been seen as complete
  if cache_completed then
  begin
    Result := True;
    exit;
  end;

  if error then
  begin
    // set dir as complete if an error is catched (not sure it's really useful)
    //Result:= True;
    //cache_completed:= Result;
    //exit;
    Debug(dpSpam, section, 'TDirlist.Complete ERROR: Site: %s - Dir: %s - DirType: %s', [site_name, full_path, parent.DirTypeAsString]);
  end;

  if parent <> nil then
  begin
    // we are in a subdirectory
    // Debug(dpMessage, section, 'TDirlist.Complete DEBUG (START): Site: %s - Dir: %s - DirType: %s', [site_name, full_path, parent.DirTypeAsString]);

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

    if (Result) then
      Debug(dpError, section, 'TDirlist.Complete SUBDIR COMPLETE: Site: %s - Dir: %s DirType: %s - ResultType: %s - files: %d size: %d', [site_name, full_path, parent.DirtypeAsString, ResultType, files, size])
    else
      Debug(dpSpam, section, 'TDirlist.Complete SUBDIR INCOMPLETE: Site: %s - Dir: %s DirType: %s - ResultType: %s - files: %d size: %d', [site_name, full_path, parent.DirTypeAsString, ResultType, files, size]);

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
      end;
    end;

    // is the release a special kind of release (dirfix, nfofix, etc.)
    if not Result then
    begin
      for tag in SpecialDirsTags do
      begin
        if AnsiMatchText(tag, full_path) then
        begin
          // TODO: Maybe add case by case checks instead of considering complete
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

constructor TDirList.Create(site_name: AnsiString; parentdir: TDirListEntry; skiplist: TSkipList; SpeedTest: boolean = False);
begin
  Create(site_name, parentdir, skiplist, '', speedtest);
end;

constructor TDirList.Create(site_name: AnsiString; parentdir: TDirListEntry; skiplist: TSkipList; s: AnsiString; SpeedTest: boolean = False);
var sf: TSkipListFilter;
begin
  biggestcd:= 0;
  self.error := False;

  self.need_mkdir := True;

  self.cache_completed := False;

  self.date_started := 0;
  self.date_completed := 0;

  self.site_name := site_name;
  self.full_path := 'Not set';

  fLastChanged := Now();
  allcdshere := False;
  entries := TObjectList.Create;
  skiped := TStringList.Create;
  skiped.CaseSensitive := False;
  self.parent := parentdir;

  self.s := s;
  self.skiplist := skiplist;
  SetSkiplists;

  self.isSpeedTest := SpeedTest;

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

procedure TDirList.SetSkiplists;
var s: AnsiString;
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

destructor TDirList.Destroy;
begin
  entries.Free;
  inherited;
end;

function TDirList.Dirname: AnsiString;
begin
  if parent = nil then
  begin
    if MultiCd then
      Result := '_MULTICDROOT_'
    else
      Result := '_ROOT_';
  end else
    Result := parent.filename;
end;

function TDirList.MultiCD: Boolean;
var i: Integer;
    s: AnsiString;
    de: TDirListEntry;
begin
  if parent = nil then
  begin
    biggestcd:= 0;
    Result := False;
    s := '';
    // megnezzuk van e CD1 CD2 stb jellegu direktorink

    for i:= entries.Count -1 downto 0 do
    begin
      try if i < 0 then Break; except Break; end;
      try
        de := TDirListEntry(entries[i]);

        if de.cdno <> 0 then
        begin
          Result := True;
          s:= s + IntToStr(de.cdno);

          if de.cdno > biggestcd then
            biggestcd := de.cdno;
        end;
      except
        Continue;
      end;
    end;

    if biggestcd > 1 then
    begin
      allcdshere := True;
      for i:= 1 to biggestcd do
        if (0 = Pos(IntToStr(i), s)) then
        begin
          allcdshere := False;
          Break;
        end;
    end;
  end else
  begin
    Result := parent.dirlist.MultiCD;
  end;
end;

function TDirList.No_NotSkiplisted: Integer;
begin
  Result := entries.Count - No_Skiplisted;
end;

function TDirList.No_Raceable: Integer;
var i: Integer;
begin
  Result:= 0;

  for i := entries.Count -1 downto 0 do
  begin
    try if i < 0 then Break; except Break; end;
    try
      if ((not TDirListEntry(entries[i]).skiplisted) and (not TDirListEntry(entries[i]).done)) then
      begin
        inc(Result);
      end;
    except
      Continue;
    end;
  end;

end;

function TDirList.No_Skiplisted: Integer;
var i: Integer;
begin
  Result := 0;

  for i := entries.Count -1 downto 0 do
  begin
    try if i < 0 then Break; except Break; end;
    try
      if TDirListEntry(entries[i]).skiplisted then
      begin
        inc(Result);
      end;
    except
      Continue;
    end;
  end;
end;

class function TDirlist.Timestamp(ts: AnsiString): TDateTime;
const
  Months: array[1..12] of AnsiString =
    ('Jan', 'Feb', 'Mar', 'Apr', 'May','Jun','Jul','Aug','Sep','Oct','Nov','Dec');
var s1,s2,s3: AnsiString;
   l, ev, ora,perc, honap, nap, i: Integer;
   evnelkul: Boolean;
begin
  Result := 0;

  s1 := Fetch(ts, ' ');
  s2 := Fetch(ts, ' ');
  s3 := Fetch(ts, ' ');

  if s3 = '' then exit;

  honap:= 0;
   for i:=1 to 12 do
   begin
     if (Months[i] = s1) then
     begin
       honap:= i;
       Break;
     end;
  end;
  if (honap = 0) then exit;

  nap:= StrToIntDef(s2, 0);
  if ((nap < 1) or (nap > 31)) then exit;


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
  end else
    exit;

  if((Result > Now)and(evnelkul)) then
    TryEncodeDateTime(YearOf(Now)-1, honap, nap, ora, perc,0,0, Result);

end;


procedure TDirList.ParseDirlist(s: AnsiString);
var
  tmp: AnsiString;
  akttimestamp: TDateTime;
  site: TSite;
  skip_being_uploaded_files: boolean;
  de: TDirListEntry;
  added: Boolean;
  dirmaszk, username, groupname, datum, filename: AnsiString;
  filesize: Int64;
  i, j: Integer;
  rrgx, splx: TRegExpr;
begin
  added := False;

  // No need to parse the dir again if it's complete
  if cache_completed then
  begin
    //Debug(dpError, section, 'SKIP: Directory %s on %s is considered complete. ParseDirList skipped (cache_complete)', [Dirname, site_name]);
    exit;
  end;

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

  for i := entries.Count - 1 downto 0 do
  begin
    try
      if i < 0 then
        Break;
    except
      Break;
    end;

    try
      de := TDirlistEntry(entries[i]);
      de.megvanmeg := False;
    except
      Continue;
    end;
  end;

  rrgx := TRegExpr.Create;
  try
    rrgx.ModifierI := True;
    rrgx.Expression := global_skip;

      while(true) do
      begin
        tmp:= trim(Elsosor(s));

        if tmp = '' then
          break;
        //Inc(lines_read);
        //if (lines_read > 2000) then break;

        //drwxrwxrwx   2 nete     Death_Me     4096 Jan 29 05:05 Whisteria_Cottage-Heathen-RERIP-2009-pLAN9
        if (length(tmp) > 11) then
        begin
          if((tmp[1] <> 'd') and (tmp[1] <> '-') and (tmp[11] = ' ')) then
            continue;

          dirmaszk:= Fetch(tmp, ' '); // dir mask
          Fetch(tmp, ' '); // No. of something
          username:= Fetch(tmp, ' '); // dir mask
          groupname:= Fetch(tmp, ' '); // dir mask
          filesize:= StrToInt64Def(Fetch(tmp, ' '),-1); // dir mask

          if filesize < 0 then
            Continue;

          datum:= Fetch(tmp, ' ')+' '+Fetch(tmp, ' ')+' '+Fetch(tmp, ' ');
          filename:= Trim(tmp);

          if filename = '' then
            Continue;

          if ((filename = '.') or (filename = '..') or (filename[1] = '.')) then
            continue;

          if rrgx.Exec(filename) then
          begin
            //debugunit.Debug(dpMessage, section, Format('[iNFO] --> ParseDirlist skip: %s', [filename]));
            Continue;
          end;

          // Dont add complete tags to dirlist entries
          if ((dirmaszk[1] = 'd') or (filesize = 0)) then
          begin
            j:= TagComplete(filename);
            if (j <> 0) then
            begin
              complete_tag:= filename;
              Continue;
            end;
          end;

          // file is flagged as skipped
          if (skiped.IndexOf(filename) <> -1) then
            Continue;

          // entry is a file and is 0 byte
          if ((dirmaszk[1] <> 'd') and (filesize = 0)) then
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

          akttimestamp:= Timestamp(datum);

          de:= Find(filename);
          if de = nil then
          begin
            de:= TDirListEntry.Create(filename, self);

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

            de.username:= username;
            de.groupname:= groupname;
            de.timestamp:= akttimestamp;
            de.done:= True;
            de.justadded:= True;
            de.directory := (dirmaszk[1] = 'd');

            if not de.directory then
              de.filesize := filesize;


            if (de.directory) then
            begin
              // check if we have a special kind of subdirectory
              de.DirType := Unknown;
              splx := TRegExpr.Create;
              try
                splx.ModifierI := True;
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

              finally
                // TODO: remove debug
                Debug(dpError, section, 'DEBUG SET TYPE: Site: %s - Dir: %s - DirType: %s', [site_name, full_path, de.DirTypeAsString]);
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

            if ((not de.Directory) and (AnsiLowerCase(de.Extension) = '.sfv') and (de.filesize > 0)) then
            begin
              sfv_status:= dlSFVFound;
            end;

            if (de.Directory) then
            begin
              de.subdirlist := TDirlist.Create(site_name, de, skiplist);
              if de.subdirlist <> nil then
                de.subdirlist.SetFullPath(MyIncludeTrailingSlash(full_path) + de.filename);
            end;

            if (self.date_started = 0) then
            begin
              self.date_started:= Now();
            end;

            entries.Add(de);

            LastChanged:= Now();
            added:= True;
          end
          else if (de.filesize <> filesize) then
          begin
            if ((de.filesize <> filesize) or (de.username <> username)) then
            begin
              LastChanged:= Now();
            end;

            de.filesize:= filesize;
            de.timestamp:= akttimestamp;
            de.username:= username;
            de.groupname:= groupname;
          end;
          de.megvanmeg:= True;
        end;
      end;

  finally
    rrgx.Free;
  end;

  // entries found means the dir exists
  if ((need_mkdir) and (entries.Count > 0)) then
    need_mkdir:= False;

  if parent = nil then
  begin
    try
      SetSkiplists;
    except
      on E: Exception do
      begin
        debugunit.Debug(dpError, section, 'SetSkiplists exception : %s', [e.Message]);
      end;
    end;
  end;

  // exit if the dir is complete
  if (Complete) then
    exit;

  // set defaults values if direcotry was just added
  if added then
  begin
    cache_completed := False;
    cache_multicd := False;
    allcdshere:= False;

    if skiplist <> nil then
    begin
      try
        RegenerateSkiplist;
      except
        on E: Exception do
        begin
          debugunit.Debug(dpError, section, '[EXCEPTION] RegenerateSkiplist: %s', [e.Message]);
        end;
      end;

      try
        Sort;
      except
        on E: Exception do
        begin
          debugunit.Debug(dpError, section, '[EXCEPTION]Sort: %s', [e.Message]);
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
  Result:= False;
  if skiplist = nil then exit;


  for i:= entries.Count -1 downto 0 do
  begin
    try if i < 0 then Break; except Break; end;
    try
      ld:= TDirListEntry(entries[i]);
      if ld.RegenerateSkiplist then
        Result:= True;
    except
      Continue;
    end;
  end;

end;

function DirListSorter(Item1, Item2: Pointer; dirtype: TDirType): Integer;
var
    i1, i2: TDirlistEntry;
    c1, c2: Integer;
    i1IsImage, i1IsVideo: Boolean;
    i2IsImage, i2IsVideo: Boolean;
    image_files_priority, videos_files_priority: Integer;

begin
  (*
     Result := -1 (i1 wins)
            := 1  (i2 wins)
            := 0  (no change)
  *)
  Result := 0;
  try
    i1:= TDirlistEntry(Item1);
    i2:= TDirlistEntry(Item2);

    // We don't care about skiplisted entries
    if ((i1.skiplisted) and (i2.skiplisted)) then exit;

    // At least one file need to have an extension for extention sorting
    if (i1.Extension <> '') or (i2.Extension <> '') then
    begin
      // sfv priority
      if ((AnsiLowerCase(i1.Extension) = '.sfv') and (AnsiLowerCase(i2.Extension) <> '.sfv')) then
      begin
        Result:= -1;
        exit;
      end;
      if ((AnsiLowerCase(i1.Extension) <> '.sfv') and (AnsiLowerCase(i2.Extension) = '.sfv')) then
      begin
        Result:= 1;
        exit;
      end;

      // nfo priority
      if ((AnsiLowerCase(i1.Extension) = '.nfo') and (AnsiLowerCase(i2.Extension) <> '.nfo')) then
      begin
        Result:= -1;
        exit;
      end;
      if ((AnsiLowerCase(i1.Extension) <> '.nfo') and (AnsiLowerCase(i2.Extension) = '.nfo')) then
      begin
        Result:= 1;
        exit;
      end;

      // image files priority (i.e.: proofs, covers)
      i1IsImage := AnsiMatchText(i1.Extension, ImageFileExtensions);
      i2IsImage := AnsiMatchText(i2.Extension, ImageFileExtensions);
      if (i1IsImage) or (i2IsImage)  then
      begin
        image_files_priority := config.ReadInteger('queue', 'image_files_priority', 0);
        if (image_files_priority > 0) and (image_files_priority <= 2) then
        begin
          if ((i1IsImage) and (not i2IsImage)) then
          begin
            if (image_files_priority = 1) then Result := -1;
            if (image_files_priority = 2) then Result := 1;
          end;
          if ((not i1IsImage) and (i2IsImage)) then
          begin
            if (image_files_priority = 1) then Result := 1;
            if (image_files_priority = 2) then Result := -1;
          end;
          Debug(dpError, section, 'DirListSorter (image): i1: %s i2: %s result: %d', [i1.Extension, i2.Extension, Result]);
          exit;
        end;
      end;

      // video files priority
      i1IsVideo := AnsiMatchText(i1.Extension, VideoFileExtensions);
      i2IsVideo := AnsiMatchText(i2.Extension, VideoFileExtensions);
      videos_files_priority := config.ReadInteger('queue', 'videos_files_priority', 0);
      if (i1IsVideo) or (i2IsVideo)  then
      begin
      if (videos_files_priority > 0) and (videos_files_priority <= 2) then
        begin
          if ((i1IsVideo) and (not i2IsVideo)) then
          begin
            if (videos_files_priority = 1) then Result := -1;
            if (videos_files_priority = 2) then Result := 1;
          end;
          if ((not i1IsVideo) and (i2IsVideo)) then
          begin
            if (videos_files_priority = 1) then Result := 1;
            if (videos_files_priority = 2) then Result := -1;
          end;
          Debug(dpError, section, 'DirListSorter (video): i1: %s i2: %s result: %d', [i1.Extension, i2.Extension, Result]);
          exit;
        end;
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
          c1:= i1.dirlist.sf_d.MatchFile(i1.filename);
          c2:= i2.dirlist.sf_d.MatchFile(i2.filename);

          if (c1 > c2) then
            Result:= 1
          else
          if (c1 < c2) then
            Result:= -1
          else
            Result:= 0;
        end else
          Result:= CompareStr(i1.filename, i2.filename);
      end
      else
      if ((not i1.directory) and (not i2.directory)) then
      begin
        c1:= i1.dirlist.sf_f.MatchFile(i1.filename);
        c2:= i2.dirlist.sf_f.MatchFile(i2.filename);

        if (c1 > c2) then
          Result:= 1
        else
        if (c1 < c2) then
          Result:= -1
        else
        begin
          // files are of the same type - sort by size
          if i1.filesize > i2.filesize then
            Result:= -1
          else
          if i1.filesize < i2.filesize then
            Result:= 1
          else
            Result:= 0;
        end;
      end
      else
      // Priority to directories
      if (i1.directory) then
        Result:= -1
      else
        Result:= 1;
    end;
  except
    on e: Exception do
    begin
      debugunit.Debug(dpError, section, '[EXCEPTION] DirListSorter: %s', [e.Message]);
      Result:= 0;
    end;
  end;
end;

function DirListModSorter(Item1, Item2: Pointer): Integer;
var i1, i2: TDirlistEntry;
begin
  // compare: -1 bekenhagyas, jo a sorrend     ~ good order
  // compare:  1 csere
  i1:= TDirlistEntry(Item1);
  i2:= TDirlistEntry(Item2);

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
  try
    entries.Sort(@DirListSorter);
  except
    on E: Exception do
    begin
      debugunit.Debug(dpError, section, '[EXCEPTION] TDirList.Sort : %s', [e.Message]);
    end;
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

  for i:= entries.Count -1 downto 0 do
  begin
    try if i < 0 then Break; except Break; end;
    try
      de:= TDirlistEntry(entries[i]);
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
      Continue;
    end;
  end;

end;

function TDirList.Find(filename: AnsiString): TDirListEntry;
var
  i: Integer;
  de: TDirListEntry;
begin
  Result := nil;
  if entries.Count = 0 then
    exit;

  for i := entries.Count - 1 downto 0 do
  begin
    try
      if i < 0 then
        Break;
    except
      Break;
    end;
    try
      de := TDirListEntry(entries[i]);
      if (AnsiUpperCase(de.filename) = AnsiUpperCase(filename)) then
      begin
        Result := de;
        Break;
      end;
    except
      Continue;
    end;
  end;

end;

procedure TDirList.SetLastChanged(value: TDateTime);
begin
  fLastChanged:= Max(value, fLastChanged);
  if parent <> nil then
    parent.dirlist.LastChanged:= fLastChanged;
end;

function TDirList.FindDirlist(dirname: AnsiString; createit: Boolean = False): TDirList;
var
  p: Integer;
  firstdir, lastdir: AnsiString;
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
      Result := nil;
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

  for i := entries.Count - 1 downto 0 do
  begin
    try if i < 0 then Break; except Break; end;
    try
     de := TDirlistEntry(entries[i]);
      if de.skiplisted then Continue;
      if de.done then inc(Result);
      if ((de.directory) and (de.subdirlist <> nil)) then
        inc(Result, de.subdirlist.Done);
    except
      Continue;
    end;
  end;

end;

function TDirList.RacedByMe(only_useful: boolean = False): Integer;
var
  de: TDirlistEntry;
  i: Integer;
begin
  Result := 0;

  for i := entries.Count - 1 downto 0 do
  begin
    try if i < 0 then Break; except Break; end;
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
      Break;
    end;
  end ;

end;

function TDirList.SizeRacedByMe(only_useful: boolean = False): Int64;
var
  de: TDirlistEntry;
  i: Integer;
begin
  Result:= 0;

  for i := entries.Count - 1 downto 0 do
  begin
    try if i < 0 then Break; except Break; end;
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
      Continue;
    end;
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

  for i := entries.Count - 1 downto 0 do
  begin
    try if i < 0 then Break; except Break; end;
    try de := TDirlistEntry(entries[i]);
      if ((AnsiLowerCase(de.Extension) = '.sfv') and (de.megvanmeg) and (de.filesize > 0)) then
      begin
        Result := True;
        Self.cache_hassfv := True;
        exit;
      end;
    except
      Break;
    end;
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

  for i := entries.Count - 1 downto 0 do
  begin
    try if i < 0 then Break; except Break; end;
    try
      de := TDirlistEntry(entries[i]);
      if ((AnsiLowerCase(de.Extension) = '.nfo') and (de.megvanmeg) and (de.filesize > 0)) then
      begin
        Result := True;
        Self.cache_hasnfo := True;
        exit;
      end;
    except
      Continue;
    end;
  end;

end;


procedure TDirList.Clear;
var
  i: Integer;
begin
  allcdshere := False;
  fLastChanged := 0;
  biggestcd := 0;

  for i := entries.Count - 1 downto 0 do
  begin
    try if i < 0 then Break; except Break; end;
    try
      TDirlistEntry(entries[i]).megvanmeg := False;
      TDirlistEntry(entries[i]).error := False;
    except
      Continue;
    end;
  end;

end;

procedure TDirList.SortByModify;
begin
  entries.Sort(@DirListModSorter);
end;

function TDirList.FindNfo: TDirListEntry;
var de: TDirlistEntry;
    i: Integer;
begin
  Result := nil;

  for i := 0 to entries.Count - 1 do
  begin
//    try if i < 0 then Break; except Break; end;
    try de := TDirlistEntry(entries[i]);

      if ((de.Extension = '.nfo') and (de.filesize > 0) and (de.filesize < 32768)) then //nfo always smaller than 32kb
      begin
        Result := de;
        exit;
      end;
    except
      Continue;
    end;
  end;

end;

function TDirList.Directories: Integer;
var i: Integer;
    de: TDirlistEntry;
begin
  Result:= 0;


  for i:= entries.Count -1 downto 0 do
  begin
    try if i < 0 then Break; except Break; end;
    try
      de:= TDirlistEntry(entries[i]);
      if ((de.directory) and (not de.skiplisted) and (de.timestamp <> 0)) then
        inc(result);
    except
      Continue;
    end;
  end;


end;

function TDirList.firstfile: TDateTime;
var i: Integer;
    de: TDirlistEntry;
    t: TDateTime;
begin
  REsult:= 0;
  for i:= entries.Count -1 downto 0 do
  begin
    try if i < 0 then Break; except Break; end;
    try
      de:= TDirlistEntry(entries[i]);
      if (de.timestamp <> 0) and (not de.skiplisted) then
      begin
        if ((Result= 0) or (Result > de.timestamp)) then
          Result:= de.timestamp;

        if ((de.Directory) and (de.subdirlist <> nil)) then
        begin
          t:= de.subdirlist.firstfile;
          if ((t <> 0) and (Result > t)) then
            Result:= t;
        end;
      end;
    except
      Continue;
    end;
  end;
end;

function TDirList.lastfile: TDateTime;
var i: Integer;
    de: TDirlistEntry;
    t: TDateTime;
begin
  Result:= 0;


  for i:= entries.Count -1 downto 0 do
  begin
    try if i < 0 then Break; except Break; end;
    try
      de:= TDirlistEntry(entries[i]);
      if (de.timestamp <> 0) and (not de.skiplisted) then
      begin
        if ((Result= 0) or (Result < de.timestamp)) then
          Result:= de.timestamp;

        if ((de.Directory) and (de.subdirlist <> nil)) then
        begin
          t:= de.subdirlist.lastfile;
          if ((t <> 0) and (Result < t)) then
            Result:= t;
        end;
      end;
    except
      Continue;
    end;
  end;
end;

procedure TDirList.SetFullPath(value: AnsiString);
begin
  self.full_path := value;
end;

{ TDirListEntry }

constructor TDirListEntry.Create(filename: AnsiString; dirlist: TDirList; SpeedTest: Boolean = False);
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
const multicddirprefix : array[1..4] of AnsiString = ('cd', 'dvd', 'disc','disk');
var
  s: AnsiString;
  i: Integer;
begin

  s := Csere(filenamelc, ' ', '');
  s := Csere(s, '_', '');
  s := Csere(s, '-', '');

  for i := 1 to 4 do
  begin
    if (1 = AnsiPos(AnsiUpperCase(multicddirprefix[i]), AnsiUpperCase(s))) then
    begin
      cdno := StrToIntDef(Copy(s, Length(multicddirprefix[i]) + 1, 1000), 0);
      exit;
    end;
  end;
end;

function TDirListEntry.Extension: AnsiString;
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
  try
    rrgx.ModifierI := True;
    rrgx.Expression := useful_skip;

    if rrgx.Exec(filename) then
    begin
      Result := False;
      exit;
    end;

  finally
    rrgx.Free;
  end;

  Result := True;
end;


procedure TDirListEntry.SetDirectory(value: Boolean);
begin
  fDirectory := value;
  if directory then CalcCDNumber;
end;

procedure TDirListEntry.SetDirType(value: TDirType);
begin
  fDirType := value;
end;

function TDirListEntry.DirTypeAsString: String;
begin
  Result := 'Undefined';
  case self.DirType of
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
  s: AnsiString;
  sf: TSkipListFilter;
begin
  Result := False;

  if dirlist.skiplist = nil then exit;

  ldepth := dirlist.Depth();

    if ( not skiplisted ) then
    begin
      if not directory then
      begin
        s := dirlist.Dirname;
        sf := dirlist.skiplist.AllowedFile(s, filename);

        // first we look for ftprush screwed up files like (1).nfo
        l := length(filename);
        if l > length(Extension) + 6 then
        begin
          // first one is for 3 chars in extension like .nfo, .rar, .mp3, .r02 and second one is for 4 chars like .flac
          if ( (filename[l-6] = '(') and (filename[l-4] = ')') and (filename[l-5] in ['0'..'9']) ) or ( (filename[l-7] = '(') and (filename[l-5] = ')') and (filename[l-6] in ['0'..'9']) )then
          begin
            skiplisted := True;
            dirlist.skiped.Add(filename);
            irc_Addtext_by_key('SKIPLOG', Format('<c2>[SKIP]</c> FTPRush screwed up file %s %s %s : %s', [dirlist.site_name, dirlist.skiplist.sectionname, s, filename]));
            exit;
          end;
        end;

        if sf = nil then
        begin
          skiplisted := True;
          dirlist.skiped.Add(filename);
          irc_Addtext_by_key('SKIPLOG', Format('<c2>[SKIP]</c> Not AllowedFile %s %s %s : %s', [dirlist.site_name, dirlist.skiplist.sectionname, s, filename]));
        end
        else
        begin
          Result := True;
        end;
      end
      else
      begin
        if ldepth < dirlist.skiplist.dirdepth then
        begin
          // vegig kell menni az alloweddirs-en es megnezni hogy
          //I need to go in and see that the en-alloweddirs
          s := dirlist.Dirname;
          sf := dirlist.skiplist.AllowedDir(s, filename);
          if sf = nil then
          begin
            skiplisted := True;
            dirlist.skiped.Add(filename);
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
  global_skip:= config.ReadString(section, 'global_skip', '\-missing$|\-offline$|^\.|^file\_id\.diz$|\.htm$|\.html$|\.bad$');
  useful_skip:= config.ReadString(section, 'useful_skip', '\.nfo$|\.sfv$|\.m3u$|\.cue$');
end;
procedure DirlistUninit;
begin

end;

end.
