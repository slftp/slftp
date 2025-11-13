unit skiplists;

interface

uses Contnrs, slmasks;

type
  TSkipListFilter = class
  private
    dirmask: TObjectList;
    filemask: TObjectList;
    function Dirmatches(const dirname: String): boolean;
  public
    function MatchFile(const filename: String): integer;
    function Match(const dirname, filename: String; out fileGroupIndex: integer): boolean;
    constructor Create(const dms, fms: String);
    destructor Destroy; override;
  end;

  TSkipList = class
  private
    mask: TslMask;
    allowedfiles: TObjectList;
    alloweddirs: TObjectList;
    skipfiles: TObjectList;
    skipdirs: TObjectList;
    skipfiles_up: TObjectList;
    skipdirs_up: TObjectList;
    skipfiles_dn: TObjectList;
    skipdirs_dn: TObjectList;
    fIsCloned: boolean;
    function FindDirFilterB(list: TObjectList; const dirname: String): TSkiplistFilter;
  public
    sectionname: String;
    dirdepth: integer;
    constructor Create(const sectionname: String);
    { Creates a clone of the given TSkipList. The allowed files and allowed dirs lists are passed over to the clone by reference, just the mask is
      actually copied because it has locked regexes in it. }
    constructor CreateClone(const aOriginal: TSkipList);
    destructor Destroy; override;

    function FindFileFilter(const dirname: String): TSkiplistFilter;
    function FindDirFilter(const dirname: String): TSkiplistFilter;

    { Returns the TSkipListFilter object if the given file is allowed. Also returns the index of the file in the list of the allowed file types.
      For example when you have this in your skiplist: "allowedfiles=Sample:*.mkv,*.mp4" then the file "sample.mkv" will return value 0 and
      the file "sample.mp4" will return value 1. -1 if it's not allowed. }
    function AllowedFile(const dirname, filename: String; out fileGroupIndex: integer): TSkipListFilter;
    { Returns the TSkipListFilter object if the given file is allowed. }
    function AllowedDir(const dirname, filename: String): TSkipListFilter; overload;
    { Returns the TSkipListFilter object if the given file is allowed. Also returns the index of the file in the list of the allowed file types.
      For example when you have this in your skiplist: "alloweddirs=_ROOT_:Sample,Sub,Subs,Proof" then the file "Sample" will return value 0 and
      the file "Subs" will return value 2. -1 if it's not allowed. }
    function AllowedDir(const dirname, filename: String; out fileGroupIndex: integer): TSkipListFilter; overload;

    function ShouldSkipFile(const dirname, filename: String): boolean;
    function ShouldSkipDir(const dirname, filename: String): boolean;
    function ShouldSkipFileUp(const dirname, filename: String): boolean;
    function ShouldSkipDirUp(const dirname, filename: String): boolean;
    function ShouldSkipFileDn(const dirname, filename: String): boolean;
    function ShouldSkipDirDn(const dirname, filename: String): boolean;
  end;

{ Find skiplist for specified section.
  Fall back to default @link(TSkipList) if no matching section can be found.
  @param(section Name of the section of which to return the skiplist)
  @returns(@link(TSkipList) object of matching skiplist or default) }
function FindSkipList(const section: String): TSkipList; overload;

{ Find skiplist for specified section. If required fallback to default skiplist.
  @param(section Name of the section of which to return the skiplist)
  @param(fallback Set to @true if default skiplist should be returned
    if @link(section) skiplist can't be found.)
  @returns(@link(TSkipList) object if matching skiplist is found or nil otherwise) }
function FindSkipList(const section: String; const fallback: boolean): TSkipList; overload;

function FindSiteSkipList(const sitename, section: String): TSkipList;
procedure SkiplistStart;
procedure SkiplistsInit;
procedure SkiplistsUninit;

function SkiplistRehash: boolean;

function SkiplistCount: integer;

implementation

uses
  Classes, mystrings, SysUtils, DebugUnit, globals,
  irc {$IFDEF MSWINDOWS}, Windows{$ENDIF};

const
  section: String = 'skiplists';

var
  skiplist: TObjectList;
  skiplist_to_clean: TObjectList;

procedure SkiplistStart;
var
  f: TextFile;
  s, s1, s2: String;
  akt: TSkipList;
  addhere: TObjectList;
  // isdupe: boolean;
begin
  skiplist_to_clean.Clear;
  //more memory frinedly
  skiplist_to_clean.Assign(skiplist);
  skiplist.Clear;
  addhere := nil;
  akt := nil;
  AssignFile(f, ExtractFilePath(ParamStr(0)) + 'slftp.skip');
  Reset(f);
  while not EOF(f) do
  begin
    readln(f, s);
    s := Trim(s);
    if ((s = '') or (s[1] = '#')) then
      Continue;
    if ((s[1] = '/') and (s[2] = '/')) then
      Continue;

    if Copy(s, 2, 8) = 'skiplist' then
    begin
      akt := TSkiplist.Create(Copy(s, 11, Length(s) - 11));
      //dupe check?
      skiplist.Add(akt);
    end
    else if akt <> nil then
    begin
      s1 := SubString(s, '=', 1);
      s2 := SubString(s, '=', 2);
      if s1 = 'dirdepth' then
        akt.dirdepth := StrToIntDef(s2, 1)
      else if ((s1 = 'allowedfiles') or (s1 = 'alloweddirs') or
               (s1 = 'skipfiles') or (s1 = 'skipdirs') or
               (s1 = 'skipfiles_up') or (s1 = 'skipdirs_up') or
               (s1 = 'skipfiles_dn') or (s1 = 'skipdirs_dn')) then
      begin
        if (s1 = 'allowedfiles') then
          addhere := akt.allowedfiles
        else if (s1 = 'alloweddirs') then
          addhere := akt.alloweddirs
        else if (s1 = 'skipfiles') then
          addhere := akt.skipfiles
        else if (s1 = 'skipdirs') then
          addhere := akt.skipdirs
        else if (s1 = 'skipfiles_up') then
          addhere := akt.skipfiles_up
        else if (s1 = 'skipdirs_up') then
          addhere := akt.skipdirs_up
        else if (s1 = 'skipfiles_dn') then
          addhere := akt.skipfiles_dn
        else if (s1 = 'skipdirs_dn') then
          addhere := akt.skipdirs_dn;
        s1 := SubString(s2, ':', 1);
        s2 := SubString(s2, ':', 2);

        addhere.Add(TSkipListFilter.Create(s1, s2));
      end;
    end;
  end;

  CloseFile(f);

  if skiplist.Count = 0 then
    raise Exception.Create('slFtp cant run without skiplist initialized');
end;

procedure SkiplistsInit;
begin
  skiplist := TObjectList.Create(False);
  skiplist_to_clean := TObjectList.Create();
end;

procedure SkiplistsUnInit;
var
  i: integer;
begin
  Debug(dpSpam, section, 'Uninit1');
  for i := 0 to skiplist.Count - 1 do
    skiplist_to_clean.Add(skiplist[i]);
  skiplist.Free;
  skiplist_to_clean.Free;
  Debug(dpSpam, section, 'Uninit2');
end;

function SkiplistCount: integer;
begin
  Result := skiplist.Count;
end;

function SkiplistRehash: boolean;
begin
  skiplist.Clear;
  skiplist_to_clean.Clear;
  result := True;
  try
    SkiplistStart;
  except
    on e: Exception do
    begin
      Debug(dpError, 'skiplists', '[EXCEPTION] SkiplistRehash : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

{ TSkipList }

function TSkipList.AllowedDir(const dirname, filename: String): TSkipListFilter;
var
  fileGroupIndex: integer;
begin
  Result := self.AllowedDir(dirname, filename, fileGroupIndex);
end;

function TSkipList.AllowedDir(const dirname, filename: String; out fileGroupIndex: integer): TSkipListFilter;
var
  j: integer;
  sf: TSkipListFilter;
begin
  Result := nil;
  try
    for j := 0 to alloweddirs.Count - 1 do
    begin
      sf := TSkipListFilter(alloweddirs[j]);
      if sf.Match(dirname, filename, fileGroupIndex) then
      begin
        Result := sf;
        exit;
      end;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, 'skiplists', '[EXCEPTION] TSkipList.AllowedDir : %s', [e.Message]);
      Result := nil;
    end;
  end;
end;

function TSkipList.AllowedFile(const dirname, filename: String; out fileGroupIndex: integer): TSkipListFilter;
var
  j: integer;
  sf: TSkipListFilter;
begin
  Result := nil;
  fileGroupIndex := -1;
  try
    for j := 0 to allowedfiles.Count - 1 do
    begin
      sf := TSkipListFilter(allowedfiles[j]);
      if sf.Match(dirname, filename, fileGroupIndex) then
      begin
        Result := sf;
        exit;
      end;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, 'skiplists', '[EXCEPTION] TSkipList.AllowedFile : %s', [e.Message]);
      Result := nil;
    end;
  end;
end;

constructor TSkipList.Create(const sectionname: String);
begin
  allowedfiles := TObjectList.Create;
  alloweddirs := TObjectList.Create;
  skipfiles := TObjectList.Create;
  skipdirs := TObjectList.Create;
  skipfiles_up := TObjectList.Create;
  skipdirs_up := TObjectList.Create;
  skipfiles_dn := TObjectList.Create;
  skipdirs_dn := TObjectList.Create;
  self.sectionname := UpperCase(sectionname);
  dirdepth := 1;
  mask := TslMask.Create(sectionname);
  fIsCloned := False;
end;

constructor TSkipList.CreateClone(const aOriginal: TSkipList);
begin
  allowedfiles := aOriginal.allowedfiles;
  alloweddirs := aOriginal.alloweddirs;
  skipfiles := aOriginal.skipfiles;
  skipdirs := aOriginal.skipdirs;
  skipfiles_up := aOriginal.skipfiles_up;
  skipdirs_up := aOriginal.skipdirs_up;
  skipfiles_dn := aOriginal.skipfiles_dn;
  skipdirs_dn := aOriginal.skipdirs_dn;
  sectionname := aOriginal.sectionname;
  dirdepth := aOriginal.dirdepth;
  mask := TslMask.Create(sectionname);
  fIsCloned := True;
end;

destructor TSkipList.Destroy;
begin
  if not fIsCloned then
  begin
    allowedfiles.Free;
    alloweddirs.Free;
    skipfiles.Free;
    skipdirs.Free;
    skipfiles_up.Free;
    skipdirs_up.Free;
    skipfiles_dn.Free;
    skipdirs_dn.Free;
  end;
  mask.Free;
  inherited;
end;

function TSkipList.FindDirFilterB(list: TObjectList; const dirname: String): TSkiplistFilter;
var
  i: integer;
  sf: TSkiplistFilter;
begin
  Result := nil;
  try
    for i := 0 to list.Count - 1 do
    begin
      sf := TSkiplistFilter(list[i]);
      if sf.DirMatches(dirname) then
      begin
        Result := sf;
        exit;
      end;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, 'skiplists', '[EXCEPTION] TSkipList.FindDirFilterB : %s', [e.Message]);
      Result := nil;
    end;
  end;
end;

function TSkipList.FindDirFilter(const dirname: String): TSkiplistFilter;
begin
  Result := FindDirFilterB(alloweddirs, dirname);
end;

function TSkipList.FindFileFilter(const dirname: String): TSkiplistFilter;
begin
  Result := FindDirFilterB(allowedfiles, dirname);
end;

function TSkipList.ShouldSkipFile(const dirname, filename: String): boolean;
var
  j: integer;
  sf: TSkipListFilter;
  fileGroupIndex: integer;
begin
  Result := False;
  try
    for j := 0 to skipfiles.Count - 1 do
    begin
      sf := TSkipListFilter(skipfiles[j]);
      if sf.Match(dirname, filename, fileGroupIndex) then
      begin
        Result := True;
        exit;
      end;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, 'skiplists', '[EXCEPTION] TSkipList.ShouldSkipFile : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

function TSkipList.ShouldSkipDir(const dirname, filename: String): boolean;
var
  j: integer;
  sf: TSkipListFilter;
  fileGroupIndex: integer;
begin
  Result := False;
  try
    for j := 0 to skipdirs.Count - 1 do
    begin
      sf := TSkipListFilter(skipdirs[j]);
      if sf.Match(dirname, filename, fileGroupIndex) then
      begin
        Result := True;
        exit;
      end;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, 'skiplists', '[EXCEPTION] TSkipList.ShouldSkipDir : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

function TSkipList.ShouldSkipFileUp(const dirname, filename: String): boolean;
var
  j: integer;
  sf: TSkipListFilter;
  fileGroupIndex: integer;
begin
  Result := False;
  try
    for j := 0 to skipfiles_up.Count - 1 do
    begin
      sf := TSkipListFilter(skipfiles_up[j]);
      if sf.Match(dirname, filename, fileGroupIndex) then
      begin
        Result := True;
        exit;
      end;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, 'skiplists', '[EXCEPTION] TSkipList.ShouldSkipFileUp : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

function TSkipList.ShouldSkipDirUp(const dirname, filename: String): boolean;
var
  j: integer;
  sf: TSkipListFilter;
  fileGroupIndex: integer;
begin
  Result := False;
  try
    for j := 0 to skipdirs_up.Count - 1 do
    begin
      sf := TSkipListFilter(skipdirs_up[j]);
      if sf.Match(dirname, filename, fileGroupIndex) then
      begin
        Result := True;
        exit;
      end;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, 'skiplists', '[EXCEPTION] TSkipList.ShouldSkipDirUp : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

function TSkipList.ShouldSkipFileDn(const dirname, filename: String): boolean;
var
  j: integer;
  sf: TSkipListFilter;
  fileGroupIndex: integer;
begin
  Result := False;
  try
    for j := 0 to skipfiles_dn.Count - 1 do
    begin
      sf := TSkipListFilter(skipfiles_dn[j]);
      if sf.Match(dirname, filename, fileGroupIndex) then
      begin
        Result := True;
        exit;
      end;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, 'skiplists', '[EXCEPTION] TSkipList.ShouldSkipFileDn : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

function TSkipList.ShouldSkipDirDn(const dirname, filename: String): boolean;
var
  j: integer;
  sf: TSkipListFilter;
  fileGroupIndex: integer;
begin
  Result := False;
  try
    for j := 0 to skipdirs_dn.Count - 1 do
    begin
      sf := TSkipListFilter(skipdirs_dn[j]);
      if sf.Match(dirname, filename, fileGroupIndex) then
      begin
        Result := True;
        exit;
      end;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, 'skiplists', '[EXCEPTION] TSkipList.ShouldSkipDirDn : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

{ TSkipListFilter }

constructor TSkipListFilter.Create(const dms, fms: String);
var
  fm: String;
  dc, fc: integer;
  i, j: integer;
begin
  dirmask := TObjectList.Create;
  filemask := TObjectList.Create;

  dc := Count(',', dms);
  fc := Count(',', fms);

  for i := 1 to dc + 1 do
    dirmask.Add(TslMask.Create(SubString(dms, ',', i)));

  for j := 1 to fc + 1 do
  begin
    fm := SubString(fms, ',', j);
    if SameText(fm, CONST_RAR_FILES) then
    begin
      filemask.Add(TslMask.Create('*.rar'));
      filemask.Add(TslMask.Create('*.r[0-9][0-9]'));
      filemask.Add(TslMask.Create('*.s[0-9][0-9]'));
      filemask.Add(TslMask.Create('*.t[0-9][0-9]'));
      filemask.Add(TslMask.Create('*.u[0-9][0-9]'));
      filemask.Add(TslMask.Create('*.v[0-9][0-9]'));
      filemask.Add(TslMask.Create('*.w[0-9][0-9]'));
      filemask.Add(TslMask.Create('*.x[0-9][0-9]'));
      filemask.Add(TslMask.Create('*.y[0-9][0-9]'));
      filemask.Add(TslMask.Create('*.z[0-9][0-9]'));
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

function TSkiplistFilter.Dirmatches(const dirname: String): boolean;
var
  i: integer;
begin
  Result := False;
  try
    for i := 0 to dirmask.Count - 1 do
      if TslMask(dirmask[i]).Matches(dirname) then
      begin
        Result := True;
        exit;
      end;
  except
    on e: Exception do
    begin
      Debug(dpError, 'skiplists', '[EXCEPTION] TSkiplistFilter.Dirmatches : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

function TSkipListFilter.Match(const dirname, filename: String; out fileGroupIndex: integer): boolean;
var
  i: integer;
begin
  Result := False;
  fileGroupIndex := -1;
  try
    if Dirmatches(dirname) then
      for i := 0 to filemask.Count - 1 do
        if TslMask(filemask[i]).Matches(filename) then
        begin
          Result := True;
          fileGroupIndex := i;
          exit;
        end;
  except
    on e: Exception do
    begin
      Debug(dpError, 'skiplists', '[EXCEPTION] TSkipListFilter.Match : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

function FindSkipList(const section: String): TSkipList;
begin
  Result := FindSkipList(section, True);
end;

function FindSkipList(const section: String; const fallback: boolean): TSkipList;
var
  i: integer;
  s: TSkipList;
begin
  Result := nil;

  // Check if section starts with a slash (for compatiblity with !transfer using absolute paths)
  if ((1 = Pos('/', section)) or (length(section) = LastDelimiter('/', section))) then
  begin
    Result := skiplist[0] as TSkipList;
  end;

  // Lookup for section skiplist
  try
    for i := 1 to skiplist.Count - 1 do
    begin
      s := TSkipList(skiplist[i]);

      // Section found in skiplist
      if s.mask.Matches(section) then
      begin
        Result := s;
        break;
      end;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, 'skiplists', '[EXCEPTION] FindSkipList : %s', [e.Message]);
      result := nil;
    end;
  end;

  // Fallback to default skiplist if nothing is found
  if (Result = nil) and (fallback) then
  begin
    irc_Addtext_by_key('SKIPLOG', Format('<c2>[SKIPLIST]</c> section <b>%s</b> not found in slftp.skip', [section]));
    Result := skiplist[0] as TSkipList;
  end;
end;

function FindSiteSkipList(const sitename, section: String): TSkipList;
var
  i: integer;
  s: TSkipList;
  searchPattern: String;
begin
  Result := nil;

  try
    if section <> '' then
    begin
      searchPattern := sitename + '-' + section;
      for i := 1 to skiplist.Count - 1 do
      begin
        s := TSkipList(skiplist[i]);
        if s.mask.Matches(searchPattern) then
        begin
          Result := s;
          exit;
        end;
      end;
    end;

    searchPattern := sitename;
    for i := 1 to skiplist.Count - 1 do
    begin
      s := TSkipList(skiplist[i]);
      if s.mask.Matches(searchPattern) then
      begin
        Result := s;
        exit;
      end;
    end;

  except
    on e: Exception do
    begin
      Debug(dpError, 'skiplists', '[EXCEPTION] FindSiteSkipList : %s', [e.Message]);
      Result := nil;
    end;
  end;
end;

function TSkipListFilter.MatchFile(const filename: String): integer;
var
  i: integer;
begin
  Result := -1;
  try
    for i := 0 to filemask.Count - 1 do
      if TslMask(filemask[i]).Matches(filename) then
      begin
        Result := i;
        exit;
      end;
  except
    on e: Exception do
    begin
      Debug(dpError, 'skiplists', '[EXCEPTION] TSkipListFilter.MatchFile : %s', [e.Message]);
      Result := -1;
    end;
  end;
end;

end.
