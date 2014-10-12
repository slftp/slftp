unit dirlist;

interface

uses Classes, Contnrs, SyncObjs, skiplists;

type
  TdlSFV = (dlSFVUnknown, dlSFVNoNeed, dlSFVFound, dlSFVNotFound);

  TDirlist = class;

  TDirListEntry = class
    dirlist: TDirList;

    megvanmeg: Boolean;
    justadded: Boolean;
    error: Boolean;

    username: string;
    groupname: string;

    fDirectory: Boolean;
    fSample: Boolean;
    subdirlist: TDirList;

    filename: string;
    filenamelc: string;
    filesize: Integer;

    skiplisted: Boolean; // ez egyertelmu = it is a clear
    racedbyme: Boolean;  // ha a kliens toltotte fel vegig = if the client is served up along
    done: Boolean;       // siteon fent van mar = site is already

    tradeCount: Integer;

    cdno: Integer;

    timestamp: TDateTime;

    sfvfirsteventvoltmar: Boolean;

    addedfrom: TStringList;



    procedure CalcCDNumber;
    function Extension: string;

    constructor Create(filename: string; dirlist: TDirList;SpeedTest:boolean=False); overload;
    constructor Create(de: TDirlistEntry; dirlist: TDirList;SpeedTest:boolean=False); overload;
    destructor Destroy; override;

    procedure SetDirectory(value: Boolean);
    procedure SetSample(value: Boolean);

    function RegenerateSkiplist: Boolean;

    function Useful: Boolean;
    property Directory: Boolean read fDirectory write SetDirectory;
    property Sample: Boolean read fSample write SetSample;
  end;
  TDirList = class
  private
    fLastChanged: TDateTime;
    allcdshere: Boolean;
    skiplist: TSkipList;
    sf_d, sf_f: TSkiplistFilter;

    procedure SetSkiplists;
    procedure SetLastChanged(value: TDateTime);
    class function Timestamp(ts: string): TDateTime;
  public
    dirlistadded: Boolean;
    mindenmehetujra: Boolean;

    site_name: String;

    error: Boolean;

    need_mkdir: Boolean;
    sfv_status: TdlSFV;

    biggestcd: Integer;

    parent: TDirListEntry;
    entries: TObjectList;
    skiped: TStringList;

    complet_tag: String;

    cache_completed: Boolean;
    cache_hasnfo: Boolean;
    cache_hassfv: Boolean;
    cache_multicd: Boolean;

    date_started: TDateTime;
    date_completed: TDateTime;

    dependency_mkdir: String;

    isSpeedTest:boolean;

    procedure Clear;
    function hasnfo: boolean;
    function hassfv: boolean;
    function No_Raceable: Integer;
    function No_Skiplisted: Integer;
    function No_NotSkiplisted: Integer;
    function firstfile: TDateTime;
    function lastfile: TDateTime;
    constructor Create( site_name: String; parentdir: TDirListEntry; skiplist: TSkipList;SpeedTest:boolean = False); overload;
    constructor Create( site_name: String; parentdir: TDirListEntry; skiplist: TSkipList; s: string;SpeedTest:boolean = False); overload;
    destructor Destroy; override;
    function Depth: Integer;
    function MultiCD: Boolean;
    function Dirname: string;

    procedure Sort;
    procedure SortByModify;
    function RegenerateSkiplist: Boolean;

    function Directories: Integer;

    procedure ParseDirlist(s: string);
    function Complete: Boolean;
    function CompleteByTag: Boolean;

    procedure Usefulfiles(var files, size: Integer);

    function FindNfo: TDirListEntry;
    function Find(filename: string): TDirListEntry;

    function FindDirlist(dirname: string; createit: Boolean = False): TDirList;
    function Done: Integer;
    function RacedByMe(only_useful: boolean = False): Integer;
    function SizeRacedByMe(only_useful: boolean = False): Int64;
  published
    property LastChanged: TDateTime read fLastChanged write SetLastChanged;
  end;

procedure  DirlistInit;
procedure  DirlistUninit;

implementation

uses SysUtils, DateUtils, debugunit, mystrings, Math, tags, regexpr, irc, configunit, mrdohutils, console;

const section = 'dirlist';

var
  global_skip: String;
  useful_skip: String;
  
{ TDirList }
function TDirList.Complete: Boolean;
var i: Integer;
    d: TDirlistEntry;
    files, size: Integer;
begin
  if cache_completed then
  begin
    Result:= True;
    exit;
  end;

(*
  if error then
  begin
    Result:= True;
    cache_completed:= Result;
    exit;
  end;
  *)

  if parent <> nil then
  begin
    // we are in a subdirectory,
    // there are two options:
    // dir cant contain an sfv
    Result:= CompleteByTag;
    if ((not Result) and (sf_f <> nil) and (sf_f.MatchFile('.sfv') = -1)) then
    begin
      Usefulfiles(files, size);

      Result:= ((files <> 0) and (size <> 0));
    end;
    if ((parent.Sample) and (entries.Count > 0)) then
      Result := true;
  end else
  begin
    // main dir vagyunk = We are main dir
    Result:= CompleteByTag;
    if (not Result) and (MultiCD) then
    begin
      if allcdshere then
      begin
        Result:= True;

        for i:= entries.Count -1 downto 0 do
        begin
          try if i < 0 then Break; except Break; end;
          try
            d:= TDirlistEntry(entries[i]);
            if ((d.cdno > 0) and (not d.skiplisted) and ((d.subdirlist = nil) or (not d.subdirlist.Complete))) then            
//            if ((d.cdno > 0) and (not d.skiplisted) and (not d.Sample) and ((d.subdirlist = nil) or (not d.subdirlist.Complete))) then
//            if ((d.cdno > 0) and (d.subdirlist <> nil) and (not d.subdirlist.Complete)) then
            begin
              Result:= False;
              break;
            end;
          except
            Continue;
          end;
        end;

      end;
    end;
  end;

  if ((Result) and (self.date_completed = 0)) then
  begin
    self.date_completed:= Now();
  end;

  cache_completed:= Result;
end;



constructor TDirList.Create( site_name: String; parentdir: TDirListEntry; skiplist: TSkipList; SpeedTest:boolean = False);
begin
  Create(site_name, parentdir, skiplist, '',speedtest);
end;

constructor TDirList.Create( site_name: String; parentdir: TDirListEntry; skiplist: TSkipList; s: string;SpeedTest:boolean = False);
var sf: TSkipListFilter;
begin
  biggestcd:= 0;
  self.error:= False;

  self.need_mkdir:= True;

  self.cache_completed:= False;

  self.date_started:= 0;
  self.date_completed:= 0;

  self.site_name:= site_name;

  fLastChanged:= Now();
  allcdshere:= False;
  entries:= TObjectList.Create;
  skiped:= TStringList.Create;
  skiped.CaseSensitive:= False;
  self.parent:= parentdir;

  self.skiplist:= skiplist;
  SetSkiplists;

  self.isSpeedTest:=SpeedTest;


  sfv_status:= dlSFVUnknown;
  if skiplist <> nil then
  begin
    sf:= skiplist.AllowedDir('', 'testsfv.sfv');
    if sf = nil then
    begin
      sfv_status:= dlSFVNoNeed;
    end else
    begin
      sfv_status:= dlSFVNotFound;
    end;
  end;

  if s <> '' then
    ParseDirlist(s);
end;

procedure TDirList.SetSkiplists;
var s: string;
begin
  s:= Dirname;
  if skiplist <> nil then
  begin
    sf_f:= skiplist.FindFileFilter(s);
    sf_d:= skiplist.FindDirFilter(s);
  end else
  begin
    sf_f:= nil;
    sf_d:= nil;
  end;
end;

function TDirList.Depth: Integer;
begin
  if parent <> nil then
    Result:= parent.dirlist.Depth + 1
  else
    Result:= 1;
end;

destructor TDirList.Destroy;
begin
  entries.Free;
  inherited;
end;

function TDirList.Dirname: string;
begin
  if parent = nil then
  begin
    if MultiCd then
      Result:= '_MULTICDROOT_'
    else
      Result:= '_ROOT_';
  end else
    Result:= parent.filename;
end;

function TDirList.MultiCD: Boolean;
var i: Integer;
    s: string;
    de: TDirListEntry;
begin
  if parent = nil then
  begin
    biggestcd:= 0;
    Result:= False;
    s:= '';
    // megnezzuk van e CD1 CD2 stb jellegu direktorink

    for i:= entries.Count -1 downto 0 do
    begin
      try if i < 0 then Break; except Break; end;
      try
        de:= TDirListEntry(entries[i]);

        if de.cdno <> 0 then
        begin
          Result:= True;
          s:= s + IntToStr(de.cdno);

          if de.cdno > biggestcd then
            biggestcd:= de.cdno;
        end;
      except
        Continue;
      end;
    end;

    if biggestcd > 1 then
    begin
      allcdshere:= True;
      for i:= 1 to biggestcd do
        if (0 = Pos(IntToStr(i), s)) then
        begin
          allcdshere:= False;
          Break;
        end;
    end;
  end else
  begin
    Result:= parent.dirlist.MultiCD;
  end;
end;

function TDirList.No_NotSkiplisted: Integer;
begin
  Result:= entries.Count - No_Skiplisted;
end;

function TDirList.No_Raceable: Integer;
var i: Integer;
begin
  Result:= 0;

  for i:= entries.Count -1 downto 0 do
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
  Result:= 0;

  for i:= entries.Count -1 downto 0 do
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

class function TDirlist.Timestamp(ts: string): TDateTime;
const
  Months: array[1..12] of string =
    ('Jan', 'Feb', 'Mar', 'Apr', 'May','Jun','Jul','Aug','Sep','Oct','Nov','Dec');
var s1,s2,s3: string;
   l, ev, ora,perc, honap, nap, i: Integer;
   evnelkul: Boolean;
begin
  Result:= 0;

  s1:= Fetch(ts, ' ');
  s2:= Fetch(ts, ' ');
  s3:= Fetch(ts, ' ');

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
  if (honap=0) then exit;

  nap:= StrToIntDef(s2, 0);
  if ((nap < 1) or (nap > 31)) then exit;


  l:= length(s3);

  ora:= 0;
  perc:= 0;
  evnelkul:= False;
  if(l = 4) then
  begin
    ev:= StrToIntDef(s3, 0);
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


procedure TDirList.ParseDirlist(s: string);
var tmp: string;
    akttimestamp: TDateTime;
    de: TDirListEntry;
    added: Boolean;

    dirmaszk, username, groupname, datum, filename: string;
    filesize: Integer;
    i, j: Integer;
    lines_read: Integer;
    rrgx,splx:TRegExpr;
begin
  added:= False;

  if cache_completed then exit;
  

  debugunit.Debug(dpSpam, section, Format('--> ParseDirlist (%d entries)', [entries.Count]));


  for i:= entries.Count -1 downto 0 do
  begin
    try if i < 0 then Break; except Break; end;
    try
      de:= TDirlistEntry(entries[i]);
      de.megvanmeg:= False;
    except
      Continue;
    end;
  end;

  rrgx:=TRegExpr.Create;
  rrgx.ModifierI:=True;
  rrgx.Expression:=global_skip;
  splx:=TRegExpr.Create;
  splx.ModifierI:=True;
//  splx.Expression:='^sample|cover?|sub?|proof$';
  splx.Expression:='^sample$';

  lines_read:= 0;
  while(true) do
  begin
    tmp:= trim(Elsosor(s));

    if tmp = '' then break;
//    Inc(lines_read);
//    if (lines_read > 2000) then break;

//drwxrwxrwx   2 nete     Death_Me     4096 Jan 29 05:05 Whisteria_Cottage-Heathen-RERIP-2009-pLAN9




    if (length(tmp) > 11) then
    begin
      if((tmp[1] <> 'd') and (tmp[1] <> '-') and (tmp[11] = ' ')) then
        continue;

      dirmaszk:= Fetch(tmp, ' '); // dirmaszk = dir mask
      Fetch(tmp, ' '); // valami szam = No. of something
      username:= Fetch(tmp, ' '); // dirmaszk = dir mask
      groupname:= Fetch(tmp, ' '); // dirmaszk = dir mask
      fileSize:= StrToIntDef(Fetch(tmp, ' '),-1); // dirmaszk = dir mask
      if fileSize < 0 then Continue;
      datum:= Fetch(tmp, ' ')+' '+Fetch(tmp, ' ')+' '+Fetch(tmp, ' ');
      filename:= Trim(tmp);
     if filename = '' then Continue;

      if ((filename = '.') or (filename = '..') or (filename[1] = '.')) then continue;


      if rrgx.Exec(filename) then
      begin
        //debugunit.Debug(dpMessage, section, Format('[iNFO] --> ParseDirlist skip: %s', [filename]));
       Continue;
      end;


      // Dont add complet tag to dirlist entries
      if ((dirmaszk[1] = 'd') or (filesize = 0)) then
      begin
        j:= TagComplete(filename);
        if (j <> 0) then
        begin
          complet_tag:= filename;
          Continue;
        end;
      end;

      if (skiped.IndexOf(filename) <> -1) then
        Continue;

      if ((dirmaszk[1] <> 'd') and (filesize = 0)) then
      begin
        Continue;
      end;

      akttimestamp:= Timestamp(datum);

      de:= Find(filename);
      if nil = de then
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
          de.filesize:= filesize;

        if ((de.directory) and (splx.Exec(filename))) then
        begin
          de.Sample := True;
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
            de.free;
            Continue;
          end;
        end;

        if ((not de.Directory) and (AnsiLowerCase(de.Extension) = '.sfv') and (de.filesize > 0)) then
        begin
          sfv_status:= dlSFVFound;
        end;

        if (de.Directory) then
        begin
          de.subdirlist:= TDirlist.Create(site_name, de, skiplist);
        end;

        if (self.date_started = 0) then
        begin
          self.date_started:= Now();
        end;

        entries.Add(de);

        LastChanged:= Now();
        added:= True;
      end else
      if (de.filesize <> filesize) then
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
  rrgx.Free;
  splx.Free;

(*
  if ((need_mkdir) and (entries.Count > 0)) then
  begin
    need_mkdir:= False;
  end;
*)


  if parent = nil then // megvaltozhatott a MULTI CD statusz = changed the status MULTI CD
  begin
    try
      SetSkiplists;
    except
      on E: Exception do
      begin
        debugunit.Debug(dpError, section, 'SetSkiplists exception : %s', [e.Message]);
      end;
    end;
  end else
  begin
    if ((entries.Count > 0) and (parent.Sample)) then
    begin
      cache_completed:= true;
    end;
  end;

  if added then
  begin
    allcdshere:= False;
    if skiplist <> nil then
    begin
      try
        RegenerateSkiplist;
      except
        on E: Exception do
        begin
          debugunit.Debug(dpError, section, 'RegenerateSkiplist exception : %s', [e.Message]);
        end;
      end;
    end;

    // now sort
    //Sort;
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

function DirListSorter(Item1, Item2: Pointer): Integer;
var i1, i2: TDirlistEntry;
    c1, c2: Integer;
begin
// compare: -1 bekenhagyas, jo a sorrend ~ bekenhagyas, good order
// compare:  1 csere  = replacement
  Result:= 0;
  try
    i1:= TDirlistEntry(Item1);
    i2:= TDirlistEntry(Item2);

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

    if ((AnsiLowerCase(i1.Extension) = '.mkv') and (AnsiLowerCase(i2.Extension) <> '.mkv')) then
    begin
      Result:= -1;
      exit;
    end;
    if ((AnsiLowerCase(i1.Extension) <> '.mkv') and (AnsiLowerCase(i2.Extension) = '.mkv')) then
    begin
      Result:= 1;
      exit;
    end;

    if ((AnsiLowerCase(i1.Extension) = '.mp4') and (AnsiLowerCase(i2.Extension) <> '.mp4')) then
    begin
      Result:= -1;
      exit;
    end;
    if ((AnsiLowerCase(i1.Extension) <> '.mp4') and (AnsiLowerCase(i2.Extension) = '.mp4')) then
    begin
      Result:= 1;
      exit;
    end;

    if ((AnsiLowerCase(i1.Extension) = '.avi') and (AnsiLowerCase(i2.Extension) <> '.avi')) then
    begin
      Result:= -1;
      exit;
    end;
    if ((AnsiLowerCase(i1.Extension) <> '.avi') and (AnsiLowerCase(i2.Extension) = '.avi')) then
    begin
      Result:= 1;
      exit;
    end;

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

    if ((i1.skiplisted) and (i2.skiplisted)) then exit;
    

    if ((i1.directory) and (i2.directory)) then
    begin
      if (i1.dirlist.sf_d <> nil) then
      begin
        c1:= i1.dirlist.sf_d.MatchFile(i1.filename);
        c2:= i2.dirlist.sf_d.MatchFile(i2.filename);

  //    if ((c1 = -1) or (c2 = -1)) then exit; // ez elvileg nem fordulhat elo, mert akkor skiplisted kene legyen  = This should not happen, because you will be skiplisted Kene

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

  //    if ((c1 = -1) or (c2 = -1)) then exit; // ez elvileg nem fordulhat elo, mert akkor skiplisted kene legyen

      if (c1 > c2) then
        Result:= 1
      else
      if (c1 < c2) then
        Result:= -1
      else
      begin
        // mindketto ugyanolyan kategoriaju fajl, itt fajlmeret alapjan rendezunk.
        //both show the same class file, be settled in size.
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
    if (i1.directory) then //i2 = file, elorebb kell lennie = forward should be
      Result:= -1
    else
      Result:= 1; //i1 = file, jo a sorrend = good order
  except
    Result:= 0;
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
  if (complet_tag = '') then
    exit;

  i:= TagComplete(complet_tag);
  if (i = 1) then
  begin
    Result:= True;
    exit;
  end;
end;

procedure TDirList.Usefulfiles(var files, size: Integer);
var i: Integer;
    de: TDirlistEntry;
    afile, asize: Integer;
begin
  files:= 0;
  size:= 0;


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
        de.subdirlist.Usefulfiles(afile, asize);
        inc(files, afile);
        inc(size, asize);
      end;
    except
      Continue;
    end;
  end;

end;

function TDirList.Find(filename: string): TDirListEntry;
var i: Integer;
    de: TDirListEntry;
begin
  Result:= nil;
  if entries.Count = 0 then
    exit;

  for i:= entries.Count -1 downto 0 do
  begin
    try if i < 0 then Break; except Break; end;
    try
      de:= TDirListEntry(entries[i]);
      if (AnsiUpperCase(de.filename) = AnsiUpperCase(filename)) then
      begin
        Result:= de;
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

function TDirList.FindDirlist(dirname: string; createit: Boolean = False): TDirList;
var p: Integer;
    firstdir, lastdir: string;
    d: TDirlistEntry;
begin
  Result:= nil;

  if dirname = '' then
  begin
    Result:= self;
    exit;
  end;

  try
    p:= Pos('/', dirname);
    if 0 < p then
    begin
      firstdir:= Copy(dirname, 1, p-1);
      lastdir:= Copy(dirname, p+1, 1000);
    end else
    begin
      firstdir:= dirname;
      lastdir:= '';
    end;

    d:= Find(firstdir);
    if d = nil then
    begin
      if not createit then
      begin
        exit;
      end;
      d:= TDirListEntry.Create(firstdir, self);
      d.Directory:= True;
      entries.Add(d);
    end;

    if (not d.Directory) then
    begin
      exit;
    end;

    if d.subdirlist = nil then
      d.subdirlist:= TDirlist.Create(site_name, d, skiplist);
  except
    on E: Exception do
    begin
      debugunit.Debug(dpError, section, 'TDirList.FindDirlist: %s', [e.Message]);
      Result:= nil;
      exit;
    end;
  end;
  Result:= d.subdirlist.FindDirlist(lastdir, createit);
end;

function TDirList.Done: Integer;
var de: TDirlistEntry;
    i: Integer;
begin
  Result:= 0;


  for i:= entries.Count -1 downto 0 do
  begin
    try if i < 0 then Break; except Break; end;
    try
     de:= TDirlistEntry(entries[i]);
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
var de: TDirlistEntry;
    i: Integer;
begin
  Result:= 0;


  for i:= entries.Count -1 downto 0 do
  begin
    try if i < 0 then Break; except Break; end;
    try
      de:= TDirlistEntry(entries[i]);
      if only_useful then
      begin
        if (de.racedbyme and de.Useful) then inc(Result);
      end else begin
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

function TDirList.SizeRacedByMe(only_useful: boolean = False):Int64;
var de: TDirlistEntry;
   i: Integer;
begin
  Result:= 0;


  for i:= entries.Count -1 downto 0 do
  begin
    try if i < 0 then Break; except Break; end;
    try
      de:= TDirlistEntry(entries[i]);
      if only_useful then
      begin
        if (de.racedbyme and de.Useful) then inc(result,de.filesize);
      end else begin
        if de.racedbyme then inc(result,de.filesize);
      end;

      if ((de.directory) and (de.subdirlist <> nil)) then inc(result,de.subdirlist.SizeRacedByMe(only_useful));
    except
      Continue;
    end;
  end;

end;


function TDirList.hassfv: boolean;
var i: Integer;
    de: TDirlistEntry;
begin
  Result:= False;
  if (self.cache_hassfv) then
  begin
    Result:= True;
    exit;
  end;


  for i:= entries.Count -1 downto 0 do
  begin
    try if i < 0 then Break; except Break; end;
    try de:= TDirlistEntry(entries[i]);
      if ((AnsiLowerCase(de.Extension) = '.sfv') and (de.megvanmeg) and (de.filesize > 0)) then
      begin
        Result:= True;
        Self.cache_hassfv:= True;
        exit;
      end;
    except
      Break;
    end;
  end;

end;

function TDirList.hasnfo: boolean;
var i: Integer;
    de: TDirlistEntry;
begin
  Result:= False;
  if (self.cache_hasnfo) then
  begin
    Result:= True;
    exit;
  end;


  for i:= entries.Count -1 downto 0 do
  begin
    try if i < 0 then Break; except Break; end;
    try
      de:= TDirlistEntry(entries[i]);
      if ((AnsiLowerCase(de.Extension) = '.nfo') and (de.megvanmeg)) then
      begin
        Result:= True;
        Self.cache_hasnfo:= True;
        exit;
      end;
    except
      Continue;
    end;
  end;

end;


procedure TDirList.Clear;
var i: Integer;
begin
  allcdshere:= False;
  fLastChanged:= 0;
  biggestcd:= 0;

  for i:= entries.Count -1 downto 0 do
  begin
    try if i < 0 then Break; except Break; end;
    try
      TDirlistEntry(entries[i]).megvanmeg:= False;
      TDirlistEntry(entries[i]).error:= False;
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
  Result:= nil;

  for i:= entries.Count -1 downto 0 do
  begin
    try if i < 0 then Break; except Break; end;
    try de:= TDirlistEntry(entries[i]);
      if ((de.Extension = '.nfo') and (de.filesize > 0)) then
      begin
        Result:= de;
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

{ TDirListEntry }

constructor TDirListEntry.Create(filename: string; dirlist: TDirList;SpeedTest:boolean=False);
begin
  addedfrom:= TStringList.Create;

  self.tradeCount:= 0;

  self.sfvfirsteventvoltmar:= False;
  self.dirlist:= dirlist;
  self.filename:= filename;
  self.done:= False;
  self.skiplisted:= False;
  self.megvanmeg:= False;
  self.error:= False;
  subdirlist:= nil;

  filenamelc:= LowerCase(filename);
  cdno:= 0;
end;

constructor TDirListEntry.Create(de: TDirlistEntry; dirlist: TDirList;SpeedTest:boolean=False);
begin
  addedfrom:= TStringList.Create;

  self.tradeCount:= 0;

  self.sfvfirsteventvoltmar:= False;
  self.filename:= de.filename;
  self.filesize:= de.filesize;
  self.directory:= de.directory;
  self.sample:= de.sample;
  self.done:= False;
  self.skiplisted:= de.skiplisted;
  self.dirlist:= dirlist;
  self.subdirlist:= nil;
  self.timestamp:= de.timestamp;
  self.megvanmeg:= False;
  self.error:= False;
  self.justadded:= True;
  filenamelc:= LowerCase(filename);




  if self.directory then CalcCDNumber;
end;

destructor TDirListEntry.Destroy;
begin
  FreeAndNil(subdirlist);
  addedFrom.Free;
  inherited;
end;

procedure TDirListEntry.CalcCDNumber;
const multicddirprefix : array[1..4] of string = ('cd', 'dvd', 'disc','disk');
var s: string;
    i: Integer;
begin

  s:= Csere(filenamelc, ' ', '');
  s:= Csere(s, '_', '');
  s:= Csere(s, '-', '');

  for i:= 1 to 4 do
  begin
    if (1 = AnsiPos(AnsiUpperCase(multicddirprefix[i]), AnsiUpperCase(s))) then
    begin
      cdno:= StrToIntDef(Copy(s, Length(multicddirprefix[i])+1, 1000), 0);
      exit;
    end;
  end;
end;

function TDirListEntry.Extension: string;
begin
  Result:= ExtractFileExt(filenamelc);
end;

function TDirListEntry.Useful: Boolean;
var
  rrgx: TRegExpr;
begin
  Result:= False;
  if filesize = 0 then exit;
  if directory then exit;

  rrgx:=TRegExpr.Create;
  rrgx.ModifierI:=True;
  rrgx.Expression:=useful_skip;

  if rrgx.Exec(filename) then
  begin
    rrgx.Free;
    Result:= False;
    exit;
  end;

  rrgx.Free;
  Result:= True;
end;

procedure TDirListEntry.SetDirectory(value: Boolean);
begin
  fDirectory:= value;
  if directory then CalcCDNumber;
end;

procedure TDirListEntry.SetSample(value: Boolean);
begin
  fSample:= value;
end;

function TDirListEntry.RegenerateSkiplist: Boolean;
var l,ldepth: Integer;
    s: string;
    sf: TSkipListFilter;
begin
  Result:= False;

  if dirlist.skiplist = nil then exit;

  ldepth:= dirlist.Depth();
//  if ldepth > dirdepth then// ez nem fordulhat elo elmeletileg, de inkabb kezeljuk = Theoretically, this does not occur, but with a better
  
    if ( not skiplisted ) then
    begin
      if not directory then
      begin

          //we first look for ftprush screwed up files like (1).nfo
          l:= length(filename);
          if l > length(Extension)+6 then
          begin
            if (
                (filename[l-6] = '(')
                and
                (filename[l-4] = ')')
                and
                (filename[l-5] in ['0'..'9'])
                ) then
            begin
              skiplisted:= True;
          irc_Addtext_by_key('SKIPLOG', Format('<c2>[SKIP]</c> (?) file %s %s %s : %s', [dirlist.site_name, dirlist.skiplist.sectionname, s, filename]));              
              exit;
            end;
          end;

        s:= dirlist.Dirname;
        sf:= dirlist.skiplist.AllowedFile(s, filename);
        if sf = nil then
        begin
          skiplisted:= True;
          dirlist.skiped.Add(filename);
          irc_Addtext_by_key('SKIPLOG', Format('<c2>[SKIP]</c> Not AllowedFile %s %s %s : %s', [dirlist.site_name, dirlist.skiplist.sectionname, s, filename]));
        end else begin
          Result:= True;
        end;
      end else
      begin
        if ldepth < dirlist.skiplist.dirdepth then
        begin
          // vegig kell menni az alloweddirs-en es megnezni hogy
          //I need to go in and see that the en-alloweddirs
          s:= dirlist.Dirname;
          sf:= dirlist.skiplist.AllowedDir(s, filename);
          if sf = nil then
          begin
            skiplisted:= True;
            dirlist.skiped.Add(filename);
            irc_Addtext_by_key('SKIPLOG', Format('<c2>[SKIP]</c> Not AllowedDir %s %s : %s', [dirlist.site_name, dirlist.skiplist.sectionname, filename]));
          end else begin
            Result:= True;
          end;
        end else
        begin
          irc_Addtext_by_key('SKIPLOG', Format('<c2>[SKIP]</c> dirdepth %s %s : %s', [dirlist.site_name, dirlist.skiplist.sectionname, filename]));
          skiplisted:= True;
        end;
      end;
    end;
end;


procedure DirlistInit;
begin
  global_skip:= config.ReadString(section, 'global_skip', '\-missing$|\-offline$|^\.');
  useful_skip:= config.ReadString(section, 'useful_skip', '\.nfo|\.sfv|\.m3u|\.cue|\.jpg|\.jpeg|\.gif|\.png|\.avi|\.mkv|\.vob|\.mp4|\.wmv');
end;
procedure DirlistUninit;
begin

end;

end.
