unit taskgenrenfo;

interface

uses Classes, pazo, taskrace, sltcp;

type
  TPazoGenreNfoTask = class(TPazoPlainTask)
  private
    ss: TStringStream;
    attempt: Integer;
    function FetchGenre(text: string): string;
  public
    constructor Create(const netname, channel: string;site: string; pazo: TPazo; attempt: Integer);
    destructor Destroy; override;
    function Execute(slot: Pointer): Boolean; override;
    function Name: string; override;
  end;

implementation

uses SysUtils, irc, StrUtils, kb, debugunit, dateutils, queueunit, tags,
     configunit, tasksunit, dirlist, mystrings, sitesunit;

const
  section = 'taskgenrenfo';

{ TPazoGenreNfoTask }

constructor TPazoGenreNfoTask.Create(const netname, channel: string;site: string; pazo: TPazo; attempt: Integer);
begin
  ss:= TStringStream.Create('');
  self.attempt:= attempt;
  self.wanted_dn:= True;  
  inherited Create(netname, channel, site, '', pazo);
end;

function TPazoGenreNfoTask.FetchGenre(text: string): string;
var i: Integer;
    s: string;
begin
  Result:= '';
  i:= Pos('genre', LowerCase(text));
  if i = 0 then exit;

  Result:= Copy(text, i + 5, 100);
  for i:= 1 to length(Result) do
  begin
    if Result[i] in [#13,#10] then
    begin
      Result:= Copy(Result, 1, i-1);
      Break;
    end;
    if (not (Result[i] in ['a'..'z','A'..'Z'])) then
      Result[i]:= ' ';
  end;

  while(true) do
  begin
    s:= Csere(Result, '  ', ' ');
    if s = Result then Break;
    Result:= s;
  end;

  Result:= Trim(Result);
end;




function TPazoGenreNfoTask.Execute(slot: Pointer): Boolean;
label ujra;
var s: TSiteSlot;
    i, j, k: Integer;
    de: TDirListEntry;
    r: TPazoGenreNfoTask;
    d: TDirList;
    event, nfofile, genre: string;
begin
  Result:= False;
  s:= slot;

  if mainpazo.stopped then
  begin
    readyerror:= True;
    exit;
  end;

  Debug(dpMessage, section, Name);

  if (mainpazo.rls is TNFORelease) then
  begin
    if (TNFORelease(mainpazo.rls).nfogenre <> '') then
    begin
      Result:= True;
      ready:= True;
      exit;
    end;
  end; // else mas nem nagyon lehet...


ujra:
  if s.status <> ssOnline then
    if not s.ReLogin then
    begin
      readyerror:= True;
      exit;
    end;

    if not s.Dirlist(MyIncludeTrailingSlash(ps1.maindir)+ MyIncludeTrailingSlash(mainpazo.rls.rlsname)) then
    begin
      if s.status = ssDown then
        goto ujra;
      readyerror:= True; // <- nincs meg a dir...
      exit;
    end;


    j:= 0;
    nfofile:= '';
    d:= TDirlist.Create(s.site.name, nil, nil, s.lastResponse);
    for i:= 0 to d.entries.Count-1 do
    begin
      try if i > d.entries.Count then Break; except Break; end;
      try
        de:= TDirlistEntry(d.entries[i]);
        if ((not de.Directory) and (de.Extension = '.nfo') and (de.filesize < 32768)) then // 32kb-nal nagyobb nfoja csak nincs senkinek
          nfofile:= de.filename;

        if ((de.Directory) or (de.filesize = 0)) then
        begin
          k:= TagComplete(de.filenamelc);
          if j = 0 then j:= k;
          if k = 1 then j:= k;
        end;
      except
        Break;
      end;
    end;
    d.Free;

  if (nfofile = '') then
  begin
    if attempt < config.readInteger(section, 'readd_attempts', 5) then
    begin
      Debug(dpSpam, section, 'READD: nincs meg az nfo file...');

      r:= TPazoGenreNfoTask.Create(netname, channel, ps1.name, mainpazo, attempt+1);
      r.startat:= IncSecond(Now, config.ReadInteger(section, 'readd_interval', 60));
      try
        AddTask(r);
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[Exception] in TPazoGenreNfoTask AddTask %s', [e.Message]));
          irc_Adderror(Format('<c4>[Exception]</c> in TPazoGenreNfoTask AddTask %s', [e.Message]));
          readyerror:= True;
          exit;
        end;
      end;
    end else
    begin
      Debug(dpSpam, section, 'READD: nincs tobb readd...');
    end;

    ready:= True;
    Result:= True;
    exit;
  end;

  // try to get the nfo file
  s.downloadingfrom:= True;
  i := s.LeechFile(ss, nfofile); 

  if i < 0 then
  begin
    readyerror:= true;
    exit;
  end;
  if i = 0 then goto ujra;
  // else siker


  genre:= FetchGenre(ss.DataString);

  if j = 1 then event:= 'COMPLETE' else event:= 'NEWDIR';
  try
    kb_add(netname, channel, ps1.name, mainpazo.rls.section, genre, 'UPDATE', mainpazo.rls.rlsname, '');
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[Exception] in TPazoGenreNfoTask kb_add %s', [e.Message]));
      readyerror:= True;
      exit;
    end;
  end;

  Result:= True;
  ready:= True;
end;

function TPazoGenreNfoTask.Name: string;
begin
  try
    Result:=Format('GENRENFO: %s [Count:%d]',[mainpazo.rls.rlsname,attempt]);
  except
    Result:= 'GENRENFO';
  end;
end;

destructor TPazoGenreNfoTask.Destroy;
begin
  ss.Free;
  inherited;
end;

end.
