unit taskmvidunit;

interface

uses
  Classes, pazo, taskrace, sltcp;

type
  TPazoMVIDTask = class(TPazoPlainTask)
  private
    ss: TStringStream;
    attempt: Integer;
  public
    constructor Create(const netname, channel, site: String; pazo: TPazo; const attempt: Integer);
    destructor Destroy; override;
    function Execute(slot: Pointer): Boolean; override;
    function Name: String; override;

    { Returns the amount of files in the SFV
      @param(aSFV Text from SFV file)
      @returns(Amount of files from SFV) }
    class function GetFileCountFromSFV(const aSFV: String): Integer;

    { Searches for @italic(Genre) and @italic(Subgenre) as a single line in aNFO and returns the genre(s) which are separated by each whitespace
      @param(aNFO Text from NFO file)
      @returns(List with all found Genres or empty if none found) }
    class function TryToParseGenre(const aNFO: String): TArray<String>;

    { Parses the NFO for the video region by searching for PAL/NTSC or the appropriate FPS
      @param(aNFO Text from NFO file)
      @returns(PAL, NTSC or empty string if nothing found) }
    class function ParseVideoRegion(const aNFO: String): String;
  end;

implementation

uses
  SysUtils, SyncObjs, Contnrs, StrUtils, kb, kb.releaseinfo, debugunit, dateutils, queueunit, tags,
  configunit, tasksunit, dirlist, mystrings, sitesunit, Regexpr;

const
  section = 'taskmvid';

{ TPazoMVIDTask }

constructor TPazoMVIDTask.Create(const netname, channel, site: String; pazo: TPazo; const attempt: Integer);
begin
  ss := TStringStream.Create('');
  self.attempt := attempt;
  self.wanted_dn := True;
  inherited Create(netname, channel, site, '', pazo);
end;

destructor TPazoMVIDTask.Destroy;
begin
  ss.Free;
  inherited;
end;

class function TPazoMVIDTask.GetFileCountFromSFV(const aSFV: String): Integer;
var
  i: Integer;
  fLines: TArray<String>;
  fStr: String;
begin
  Result := 0;

  // lines always end with #10
  fLines := aSFV.Split([#10]);

  for fStr in fLines do
  begin
    // ignore comments
    if not fStr.StartsWith(';') then
      Inc(Result);
  end;
end;

class function TPazoMVIDTask.TryToParseGenre(const aNFO: String): TArray<String>;
const
  OffsetForStringGenre = 5;
  OffsetForStringSubGenre = 8;
var
  fPos: Integer;
  fGenreLine: String;
  fGenres, fSubgenres: String;

  { extracts and cleans all genres found in a single line }
  function ExtractGenres(const aNFOLine: String): String;
  var
    i: Integer;
  begin
    Result := aNFOLine;

    for i := 1 to Length(Result) do
    begin
      if Result[i] in [#13, #10] then
      begin
        Result := Copy(Result, 1, i - 1);
        Break;
      end;

      if not (IsALetter(Result[i])) then
        Result[i] := ' ';
    end;

    Result := Result.Trim;
    Result := Result.Replace('  ', ' ');
  end;

begin
  { search for Genre }
  fPos := Pos('genre', LowerCase(aNFO));
  if fPos = 0 then
    Exit;
  fGenreLine := Copy(aNFO, fPos + OffsetForStringGenre, 100);
  fGenres := ExtractGenres(fGenreLine);

  Result := fGenres.Split([' ']);

  { search for Subgenre }
  fPos := Pos('subgenre', LowerCase(aNFO));
  if fPos = 0 then
    Exit;
  fGenreLine := Copy(aNFO, fPos + OffsetForStringSubGenre, 100);
  fSubgenres := ExtractGenres(fGenreLine);

  Result := Result + fSubgenres.Split([' ']);
end;

class function TPazoMVIDTask.ParseVideoRegion(const aNFO: String): String;
var
  rrx: TRegExpr;
begin
  Result := '';

  rrx := TRegExpr.Create;
  try
    rrx.ModifierI := True;

    rrx.Expression := '(NTSC|PAL)';
    if rrx.Exec(aNFO) then
    begin
      Result := rrx.Match[0];
      exit;
    end;

    rrx.Expression := '(23\.976|29\.970?|59\.940?)\s?FPS';
    if rrx.Exec(aNFO) then
    begin
      Result := 'NTSC';
      exit;
    end;

    rrx.Expression := '(25\.000)\s?FPS';
    if rrx.Exec(aNFO) then
    begin
      Result := 'PAL';
      exit;
    end;
  finally
    rrx.Free;
  end;
end;

function TPazoMVIDTask.Execute(slot: Pointer): boolean;
label
  ujra;
var
  s: TSiteSlot;
  i: Integer;
  de: TDirListEntry;
  r: TPazoMVIDTask;
  d: TDirList;
  fSFVFile, fNFOFile: String;
  fFilecount: Integer;
  fGenreList: TArray<String>;
  fVideoRegion: String;
  mvr: TMVIDRelease;
begin
  Result := False;
  s := slot;

  if mainpazo.stopped then
  begin
    readyerror := True;
    exit;
  end;

  Debug(dpMessage, section, Name);

ujra:
  if s.status <> ssOnline then
  begin
    if not s.ReLogin then
    begin
      readyerror := True;
      exit;
    end;
  end;

  if not s.Dirlist(MyIncludeTrailingSlash(ps1.maindir) + MyIncludeTrailingSlash(mainpazo.rls.rlsname)) then
  begin
    if s.status = ssDown then
      goto ujra;
    readyerror := True; // <- there is no dir ...
    exit;
  end;

  d := TDirlist.Create(s.Name, nil, nil, s.lastResponse);
  try
    d.dirlist_lock.Enter;
    try
      for i := 0 to d.entries.Count - 1 do
      begin
        de := TDirlistEntry(d.entries.Objects[i]);

        if ((not de.Directory) and (de.Extension = '.sfv')) then
          fSFVFile := de.filename;

        if ((not de.Directory) and (de.Extension = '.nfo')) then
          fNFOFile := de.filename;
      end;
    finally
      d.dirlist_lock.Leave;
    end;
  finally
    d.Free;
  end;

  if fSFVFile = '' then
  begin
    if attempt < config.readInteger(section, 'readd_attempts', 5) then
    begin
      Debug(dpSpam, section, 'READD: No SFV file available...');
      r := TPazoMVIDTask.Create(netname, channel, ps1.name, mainpazo, attempt + 1);
      r.startat := IncSecond(Now, config.ReadInteger(section, 'readd_interval', 60));
      AddTask(r);
    end
    else
    begin
      mainpazo.rls.aktualizalasfailed := True;
      Debug(dpSpam, section, 'READD: Attempt limit for finding file reached...');
    end;

    ready := True;
    Result := True;
    exit;
  end;

  if fNFOFile = '' then
  begin
    if attempt < config.readInteger(section, 'readd_attempts', 5) then
    begin
      Debug(dpSpam, section, 'READD: No NFO file available...');
      r := TPazoMVIDTask.Create(netname, channel, ps1.name, mainpazo, attempt + 1);
      r.startat := IncSecond(Now, config.ReadInteger(section, 'readd_interval', 60));
      AddTask(r);
    end
    else
    begin
      mainpazo.rls.aktualizalasfailed := True;
      Debug(dpSpam, section, 'READD: Attempt limit for finding file reached...');
    end;

    ready := True;
    Result := True;
    exit;
  end;

  // trying to get the SFV
  s.downloadingfrom := True;
  i := s.LeechFile(ss, fSFVFile);
  s.downloadingfrom := False;
  if (i < 0)  then
  begin
    readyerror := True;
    exit;
  end;
  if i = 0 then goto ujra;

  fFilecount := GetFileCountFromSFV(ss.DataString);
  ss.Clear;

  // trying to get the NFO
  s.downloadingfrom := True;
  i := s.LeechFile(ss, fNFOFile);
  s.downloadingfrom := False;
  if (i < 0)  then
  begin
    readyerror := true;
    exit;
  end;
  if i = 0 then goto ujra;

  fVideoRegion := ParseVideoRegion(ss.DataString);
  fGenreList := TryToParseGenre(ss.DataString);

  mvr := TMVIDRelease(mainpazo.rls);
  mvr.SetValuesFromTask(fFilecount, (fVideoRegion = 'PAL'), (fVideoRegion = 'NTSC'), fGenreList);

  kb_add(netname, channel, ps1.name, mainpazo.rls.section, '', kbeNEWDIR, mainpazo.rls.rlsname, '');

  Result := True;
  ready := True;
end;

function TPazoMVIDTask.Name: String;
begin
  try
    Result := Format('MVID <b>%s : %s</b>: %d',[site1, slot1name, pazo_id]);
  except
    Result := 'MVID';
  end;
end;

end.
