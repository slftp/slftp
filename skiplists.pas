unit skiplists;

interface

uses Contnrs;

type
  TSkipListFilter = class
  private
    dirmask: TObjectList;
    filemask: TObjectList;
    function Dirmatches(dirname: string): Boolean;
  public
    function MatchFile(filename: string): Integer;
    function Match(dirname, filename: string): Boolean;
    constructor Create(dms, fms: string);
    destructor Destroy; override;
  end;

  TSkipList = class
  private
    allowedfiles: TObjectList;
    alloweddirs: TObjectList;
    function FindDirFilterB(list: TObjectList; dirname: string): TSkiplistFilter;
  public
    sectionname: string;
    dirdepth: Integer;
    constructor Create(sectionname: string);
    destructor Destroy; override;

    function FindFileFilter(dirname: string): TSkiplistFilter;
    function FindDirFilter(dirname: string): TSkiplistFilter;
    function AllowedFile(dirname, filename: string): TSkipListFilter;
    function AllowedDir(dirname, filename: string): TSkipListFilter;    
  end;

function FindSkipList(section: string): TSkipList;
procedure SkiplistStart;
procedure SkiplistsInit;
procedure SkiplistsUninit;

function SkiplistRehash:boolean;

function SkiplistCount:integer;

implementation

uses slmasks, mystrings, SysUtils, DebugUnit, irc, console;

const section: string = 'skiplists';

var skiplist: TObjectList;
    skiplist_to_clean: TObjectList;

procedure SkiplistStart;
var f: TextFile;
    s, s1, s2: string;
    akt: TSkipList;
    addhere: TObjectList;
    i: Integer;
  isdupe:boolean;
begin

skiplist_to_clean.Clear;
//more memory frinedly
skiplist_to_clean.Assign(skiplist);

//  for i:= 0 to skiplist.Count -1 do skiplist_to_clean.Add(skiplist[i]);
  skiplist.Clear;
  addhere:= nil;
  akt:= nil;
  AssignFile(f, ExtractFilePath(ParamStr(0))+'slftp.skip');
  Reset(f);
  while not eof(f) do
  begin
    readln(f,s);
    s:= Trim(s);
    if ((s = '') or (s[1] = '#')) then Continue;
    if ((s[1] = '/') and (s[2] = '/')) then Continue;

    if Copy(s, 2, 8) = 'skiplist' then
    begin
      akt:= TSkiplist.Create(Copy(s, 11, Length(s)-11));
      //dupe check?
      skiplist.Add(akt);
    end
    else
    if akt <> nil then
    begin
      s1:= SubString(s, '=', 1);
      s2:= SubString(s, '=', 2);
      if s1 = 'dirdepth' then
        akt.dirdepth:= StrToIntDef(s2, 1)
      else
      if ((s1 = 'allowedfiles') or (s1 = 'alloweddirs')) then
      begin
        if (s1 ='allowedfiles') then
          addhere:= akt.allowedfiles
        else
        if (s1 ='alloweddirs') then
          addhere:= akt.alloweddirs;
        s1:= SubString(s2, ':', 1);
        s2:= SubString(s2, ':', 2);

        addhere.Add(TSkipListFilter.Create(s1,s2));
      end;
    end;
  end;

  CloseFile(f);

  if skiplist.Count = 0 then raise Exception.Create('slFtp cant run without skiplist initialized');
end;

procedure SkiplistsInit;
begin
  skiplist:= TObjectList.Create(False);
  skiplist_to_clean:= TObjectList.Create();
end;

procedure SkiplistsUnInit;
var i: Integer;
begin
  Debug(dpSpam, section, 'Uninit1');
  for i:= 0 to skiplist.Count -1 do
    skiplist_to_clean.Add(skiplist[i]);
  skiplist.Free;
  skiplist_to_clean.Free;
  Debug(dpSpam, section, 'Uninit2');
end;

function SkiplistCount:integer;
begin
  result:=skiplist.Count;
end;

function SkiplistRehash:boolean;
begin
  result:=False;
  try
  skiplist.clear;
  skiplist_to_clean.clear;
  SkiplistStart;
  finally
    result:=true;
  end;
end;

{ TSkipList }

function TSkipList.AllowedDir(dirname, filename: string): TSkipListFilter;
var j: Integer;
    sf: TSkipListFilter;
begin
  Result:= nil;
  try
    for j:= 0 to alloweddirs.Count -1 do
    begin
      sf:= TSkipListFilter(alloweddirs[j]);
      if sf.Match(dirname, filename) then
      begin
        Result:= sf;
        exit;
      end;
    end;
  except
    Result:= nil;
  end;
end;

function TSkipList.AllowedFile(dirname, filename: string): TSkipListFilter;
var j: Integer;
    sf: TSkipListFilter;
begin
  Result:= nil;
  try
    for j:= 0 to allowedfiles.Count -1 do
    begin
      sf:= TSkipListFilter(allowedfiles[j]);
      if sf.Match(dirname, filename) then
      begin
        Result:= sf;
        exit;
      end;
    end;
  except
    Result:= nil;
  end;
end;

constructor TSkipList.Create(sectionname: string);
begin
  allowedfiles:= TObjectList.Create;
  alloweddirs:= TObjectList.Create;
  self.sectionname:= UpperCase(sectionname);
  dirdepth:= 1;
end;

destructor TSkipList.Destroy;
begin
  allowedfiles.Free;
  alloweddirs.Free;
  
  inherited;
end;


function TSkipList.FindDirFilterB(list: TObjectList; dirname: string): TSkiplistFilter;
var i: Integer;
    sf: TSkiplistFilter;
begin
  Result:= nil;
  try
    for i:= 0 to list.Count-1 do
    begin
      sf:= TSkiplistFilter(list[i]);
      if sf.DirMatches(dirname) then
      begin
        Result:= sf;
        exit;
      end;
    end;
  except
    Result:= nil;
  end;
end;
function TSkipList.FindDirFilter(dirname: string): TSkiplistFilter;
begin
  Result:= FindDirFilterB(alloweddirs, dirname);
end;

function TSkipList.FindFileFilter(dirname: string): TSkiplistFilter;
begin
  Result:= FindDirFilterB(allowedfiles, dirname);
end;

{ TSkipListFilter }

constructor TSkipListFilter.Create(dms, fms: string);
var fm: string;
    dc, fc: Integer;
    i, j: Integer;
begin
  dirmask:= TObjectList.Create;
  filemask:= TObjectList.Create;

  dc:= Count(',', dms);
  fc:= Count(',', fms);

  for i:= 1 to dc + 1 do
    dirmask.Add(TslMask.Create( SubString(dms, ',', i) ));

  for j:= 1 to fc + 1 do
  begin
    fm:= SubString(fms, ',', j);
    if fm = '_RAR_' then
    begin
      filemask.Add(TslMask.Create('*.rar'));
      filemask.Add(TslMask.Create('*.r[0-9][0-9]'));
      filemask.Add(TslMask.Create('*.s[0-9][0-9]'));
      filemask.Add(TslMask.Create('*.[0-9][0-9][0-9]'));
    end
    else
      filemask.Add(TslMask.Create(fm));
  end;
end;

destructor TSkipListFilter.Destroy;
begin
  filemask.Free;
  dirmask.Free;
  inherited;
end;

function TSkiplistFilter.Dirmatches(dirname: string): Boolean;
var i: Integer;
begin
  Result:= False;
  try
    for i:= 0 to dirmask.Count -1 do
      if TslMask(dirmask[i]).Matches(dirname) then
      begin
        Result:= True;
        exit;
      end;
  except
    Result:= False;
  end;
end;

function TSkipListFilter.Match(dirname, filename: string): Boolean;
var i: Integer;
begin
  Result:= False;
  try
    if Dirmatches(dirname) then
      for i:= 0 to filemask.Count -1 do
        if TslMask(filemask[i]).Matches(filename) then
        begin
          Result:= True;
          exit;
        end;
  except
    Result:= False;
  end;
end;

function FindSkipList(section: string): TSkipList;
var i: Integer;
    s: TSkipList;
begin
  Result:= nil;
  try
    for i:= 1 to skiplist.Count -1 do
    begin
      s:= TSkipList(skiplist[i]);
      if (AnsiCompareText(s.sectionname, section) = 0) then
      begin
        Result:= s;
        exit;
      end;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, 'skiplists', '[EXCEPTION] FindSkipList : %s', [e.Message]);
      Result:= nil;
    end;
  end;
  irc_Addtext_by_key('SKIPLOG', Format('<c2>[SKIP]</c> section not found: %s', [section]));
  Result:= skiplist[0] as TSkipList;
end;

function TSkipListFilter.MatchFile(filename: string): Integer;
var i: Integer;
begin
  Result:= -1;
  try
    for i:= 0 to filemask.Count -1 do
      if TslMask(filemask[i]).Matches(filename) then
      begin
        Result:= i;
        exit;
      end;
  except
    Result:= -1;
  end;
end;

end.
