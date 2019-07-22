unit taskmvidunit;

interface

uses
  Classes, pazo, taskrace, sltcp;

type
  TPazoMVIDTask = class(TPazoPlainTask)
    private
      ss: TStringStream;
      ss1: TStringStream;
      attempt: Integer;
      function FetchGenre(const text: String): String;
      //function GetVideoSource(text:string): String;
      //function GetFileCount(text:string): Integer;
      function GetVideoRegion(const text:String): String;
    public
      constructor Create(const netname, channel, site: String; pazo: TPazo; const attempt: Integer);
      destructor Destroy; override;
      function Execute(slot: Pointer): Boolean; override;
      function Name: String; override;
  end;

implementation

uses
  SysUtils, SyncObjs, Contnrs, StrUtils, kb, debugunit, dateutils, queueunit, tags,
  configunit, tasksunit, dirlist, mystrings, sitesunit, Regexpr;

const
  section = 'taskmvid';

{ TPazoMVIDTask }

constructor TPazoMVIDTask.Create(const netname, channel, site: String; pazo: TPazo; const attempt: Integer);
begin
  ss := TStringStream.Create('');
  ss1 := TStringStream.Create('');
  self.attempt := attempt;
  self.wanted_dn := True;
  inherited Create(netname, channel, site, '', pazo);
end;

destructor TPazoMVIDTask.Destroy;
begin
  ss.Free;
  ss1.Free;
  inherited;
end;

function TPazoMVIDTask.GetVideoRegion(const text: String): String;
var
  rrx: TRegexpr;
begin
  result := '-1';
  rrx := TRegexpr.Create;
  try
    rrx.ModifierI := True;

    rrx.Expression := '(NTSC|PAL)';
    if rrx.Exec(text) then
    begin
      result := rrx.Match[0];
      exit;
    end;

    rrx.Expression := '(23\.976|29\.970?|59\.940?)\s?FPS';
    if rrx.Exec(text) then
    begin
      result := 'NTSC';
      exit;
    end;

    rrx.Expression := '(25\.000)\s?FPS';
    if rrx.Exec(text) then
    begin
      result := 'PAL';
      exit;
    end;
  finally
    rrx.Free;
  end;
end;

(*
function TPazoMVIDTask.GetVideoSource(text: string):string;
var
rrx:TRegexpr;
begin
result:='-1';
rrx:=TRegexpr.Create;
rrx.Expression:=config.ReadString('kb','mvidsource','');
if rrx.Exec(text) then result:=rrx.Match[0];
rrx.free;
end;
*)

function TPazoMVIDTask.FetchGenre(const text: String): String;
var
  i: Integer;
begin
  Result := '';
  i := Pos('genre', LowerCase(text));
  if i = 0 then exit;

  Result := Copy(text, i + 5, 100);
  for i := 1 to length(Result) do
  begin
    if Result[i] in [#13,#10] then
    begin
      Result := Copy(Result, 1, i - 1);
      Break;
    end;
    if not (IsALetter(Result[i])) then
      Result[i] := ' ';
  end;

  Result := ReplaceText(Result, '  ', ' ');
  Result := Trim(Result);
end;

(*
function TPazoMVIDTask.GetFileCount(text: string):integer;
var i: Integer;
    y: TStringlist;
    rrx:TRegexpr;
    countz:integer;
begin
result:=-1;
countz:=0;
y:= TStringlist.Create;
y.Text:=text;
rrx:=TRegexpr.Create;
rrx.Expression:='^\;';

//irc_addtext('','',y.Text+'  '+inttostr(y.Count));

for I := 0 to y.Count - 1 do
if not rrx.Exec(y.Strings[i]) then inc(countz);

if countz >= 1 then result:=countz;
rrx.Free;
y.free;
end;
*)

function TPazoMVIDTask.Execute(slot: Pointer): boolean;
label
  ujra;
var
  s: TSiteSlot;
  filecount, i: Integer;
  j, fTagCompleteType: TTagCompleteType;
  de: TDirListEntry;
  r: TPazoMVIDTask;
  d: TDirList;
  regiono, nfofile, genre: String;
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
    if not s.ReLogin then
    begin
      readyerror := True;
      exit;
    end;

    if not s.Dirlist(MyIncludeTrailingSlash(ps1.maindir) + MyIncludeTrailingSlash(mainpazo.rls.rlsname)) then
    begin
      if s.status = ssDown then
        goto ujra;
      readyerror := True; // <- there is no dir ...
      exit;
    end;

    j := tctUNMATCHED;
    filecount :=0;
    nfofile := '';
    d := TDirlist.Create(s.Name, nil, nil, s.lastResponse);
    d.dirlist_lock.Enter;
    try
      for i := 0 to d.entries.Count - 1 do
      begin
        de := TDirlistEntry(d.entries[i]);
        if ((not de.Directory) and (de.Extension = '.nfo') and (de.filesize < 32768)) then // With a 32kb higher nfo, there is no one
          nfofile := de.filename;

        if ((de.Directory) or (de.filesize = 0)) then
        begin
          fTagCompleteType := TagComplete(de.filenamelc);
          if j = tctUNMATCHED then j := fTagCompleteType;
          if fTagCompleteType = tctCOMPLETE then j := fTagCompleteType;
        end;

        if ((not de.Directory) and (de.filesize > 0) and (de.Extension <> '.jpg') and (de.Extension <> '.gif') and (de.Extension <> '.nfo') and (de.Extension <> '.sfv')) then
          inc(filecount);
      end;

    finally
      d.dirlist_lock.Leave;
      d.Free;
    end;

  if nfofile = '' then
  begin
    if attempt < config.readInteger(section, 'readd_attempts', 5) then
    begin
      Debug(dpSpam, section, 'READD: it is not a nfo/sfv file...');
      r := TPazoMVIDTask.Create(netname, channel, ps1.name, mainpazo, attempt + 1);
      r.startat := IncSecond(Now, config.ReadInteger(section, 'readd_interval', 60));
      AddTask(r);
    end
    else
    begin
      mainpazo.rls.aktualizalasfailed := True;
      Debug(dpSpam, section, 'READD: no more readd...');
    end;

    ready := True;
    Result := True;
    exit;
  end;

  // trying to get the nfo
  s.downloadingfrom := True;
  i := s.LeechFile(ss, nfofile);
  if (i < 0)  then
  begin
    readyerror := true;
    exit;
  end;
  if i = 0 then goto ujra;

  genre := FetchGenre(ss.DataString);
  regiono :=GetVideoRegion(ss.DataString);

  mvr := TMVIDRelease(mainpazo.rls);

  mvr.FileCount := filecount;
  mvr.mvid_Genre.add(Genre);
  if regiono = 'PAL' then mvr.mvid_pal := True;
  if regiono = 'NTSC' then mvr.mvid_ntsc := True;

  kb_add(netname, channel, ps1.name, mainpazo.rls.section, genre, kbeNEWDIR, mainpazo.rls.rlsname, '');

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
