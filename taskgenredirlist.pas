unit taskgenredirlist;

interface

uses pazo, taskrace;

type
  TPazoGenreDirlistTask = class(TPazoPlainTask)
    private
      attempt: Integer;
      function FetchGenre(filename: String): String;
    public
      constructor Create(const netname, channel: String;site: String; pazo: TPazo; attempt: Integer);
      function Execute(slot: Pointer): Boolean; override;
      function Name: String; override;
  end;

// TODO: Implement it similiar for other ftpd response as drftpd and glftpd output genre too
//function ParsePZSOutputForGenre(const s: String): String;

implementation

uses SysUtils, StrUtils, kb, kb.releaseinfo, debugunit, dateutils, queueunit, tags, configunit, tasksunit, dirlist, mystrings, sitesunit, irc;

const
  section = 'taskgenredirlist';

{
function ParsePZSOutputForGenre(const s: String): String;
var
  i, j: Integer;
begin
  Result := '';
  i := Pos('226- | Genre  : ', s);
  if i = 0 then
    exit;
  j := PosEx('|', s, i + 10);
  Result := Trim(Copy(s, i+16, j-i-17));
end;
}

{ TPazoGenreDirlistTask }

constructor TPazoGenreDirlistTask.Create(const netname, channel: String;site: String; pazo: TPazo; attempt: Integer);
begin
  self.attempt:= attempt;
  inherited Create(netname, channel, site, '', pazo);
end;

function TPazoGenreDirlistTask.FetchGenre(filename: String): String;
var
  i: Integer;
begin
  Result:= '';
  try
    for i:= 0 to GlMP3Genres.Count-1 do
    begin
      if AnsiContainsText(filename, GlMP3Genres[i]) then
      begin
        Result:= GlMP3Genres[i];
        Break;
      end;
    end;
    except
    on e: Exception do
    result := '';
  end;
end;

function TPazoGenreDirlistTask.Execute(slot: Pointer): Boolean;
label
  ujra;
var
  s: TSiteSlot;
  fTagCompleteType: TTagCompleteType;
  r: TPazoGenreDirlistTask;
  d: TDirList;
  tagfile, genre: String;
begin
  Result := False;
  s := slot;

  if mainpazo.stopped then
  begin
    readyerror := True;
    exit;
  end;

  Debug(dpMessage, section, Name);

  // mp3genre already known?
  if (mainpazo.rls is TMP3Release) then
  begin
    if (TMP3Release(mainpazo.rls).mp3genre <> '') then
    begin
      Result := True;
      ready := True;
      exit;
    end;
  end;


ujra:
  if s.status <> ssOnline then
  begin
    if not s.ReLogin then
    begin
      readyerror := True;
      exit;
    end;
  end;

  try
    if not s.Dirlist(MyIncludeTrailingSlash(ps1.maindir)+ MyIncludeTrailingSlash(mainpazo.rls.rlsname)) then
    begin
      readyerror := True;
      exit;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[Exception] in TPazoGenreDirlistTask  %s', [e.Message]));
      readyerror := True;
      exit;
    end;
  end;


  tagfile := '';
  try
    d := TDirlist.Create(s.site.Name, nil, nil, s.lastResponse);
    try
      fTagCompleteType := TagComplete(d.CompleteDirTag);
      if fTagCompleteType <> tctUNMATCHED then
        tagfile := d.CompleteDirTag;
    finally
      d.Free;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[Exception] in TPazoGenreDirlistTask  %s', [e.Message]));
      readyerror := True;
      exit;
    end;
  end;

  try
    genre := FetchGenre(tagfile);
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[Exception] in TPazoGenreDirlistTask  %s', [e.Message]));
      readyerror := True;
      exit;
    end;
  end;

  if ((fTagCompleteType = tctUNMATCHED) or (genre = '')) then
  begin
    if attempt < config.readInteger(section, 'readd_attempts', 5) then
    begin
      Debug(dpSpam, section, 'READD: No member or the complete lack of genre...');
      try
        r := TPazoGenreDirlistTask.Create(netname, channel, ps1.name, mainpazo, attempt + 1);
        r.startat := IncSecond(Now, config.ReadInteger(section, 'readd_interval', 60));
        AddTask(r);
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[Exception] in TPazoGenreDirlistTask AddTask %s', [e.Message]));
          irc_Adderror(Format('<c4>[Exception]</c> in TPazoGenreDirlistTask AddTask %s', [e.Message]));
          readyerror := True;
          exit;
        end;
      end;
    end
    else
      Debug(dpSpam, section, 'READD: Maximum readd attempts reached...');
  end
  else
  begin
    try
      kb_add(netname, channel, ps1.name, mainpazo.rls.section, genre, kbeUPDATE, mainpazo.rls.rlsname, '');
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[Exception] in TPazoGenreDirlistTask kb_add %s', [e.Message]));
        irc_Adderror(Format('<c4>[Exception]</c> in TPazoGenreDirlistTask kb_add %s', [e.Message]));
        readyerror := True;
        exit;
      end;
    end;
  end;

  Result := True;
  ready := True;
end;

function TPazoGenreDirlistTask.Name: String;
begin
  try
    Result := Format('GENREDIRLIST: %s (Count: %d)',[mainpazo.rls.rlsname, attempt]);
  except
    Result := 'GENREDIRLIST';
  end;
end;

end.
