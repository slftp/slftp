unit taskgame;

interface

uses Classes, pazo, taskrace, sltcp;

type
  TPazoGameTask = class(TPazoPlainTask)
  private
    ss: TStringStream;
    attempt: Integer;
    function Parse(text: String): Boolean;
  public
    constructor Create(const netname, channel: String;site: String; pazo: TPazo; attempt: Integer);
    destructor Destroy; override;
    function Execute(slot: Pointer): Boolean; override;
    function Name: String; override;
  end;

implementation

uses SysUtils, irc, StrUtils, kb, debugunit, dateutils, queueunit, tags,
     configunit, tasksunit, dirlist, mystrings, sitesunit, regexpr,
     sllanguagebase;

const section = 'taskgame';


constructor TPazoGameTask.Create(const netname, channel: String;site: String; pazo: TPazo; attempt: Integer);
begin
  ss:= TStringStream.Create('');
  self.attempt:= attempt;
  self.wanted_dn:= True;
  inherited Create(netname, channel, site, '', pazo);
end;

destructor TPazoGameTask.Destroy;
begin
  ss.Free;
  inherited;
end;

function TPazoGameTask.Parse(text: String):boolean;
//var pgrx2,pgrx1,pgrx,pgrg:TRegExpr;
//ss,s:string;
//rg:TGameRelease;
begin
result:=False;
(*
pgrx:=TRegExpr.Create;
pgrx.ModifierI:=True;
pgrx1:=TRegExpr.Create;
pgrx1.ModifierI:=True;
pgrx2:=TRegExpr.Create;
pgrx2.ModifierI:=True;

rg:=TGameRelease(mainpazo.rls);

pgrx.Expression:='Lang((uage)?s)?[^\n]+';
if pgrx.Exec(text) then begin
// SLLanguagesFindLanguage
end;

pgrx.Expression:='(REGiON|Origin|Platform|Playble\s\@|Regions|FORMAT)[^\n]+';
pgrx1.Expression:='(PAL|NSTC[\-\w]?|Not[\s]?Region(s)?[\s]?Free|Region(s)?[\s]?Free|RF)';
if pgrx.Exec(text) then begin
s:=pgrx.Match[0];
if pgrx1.Exec(s) then begin REPEAT
ss:=uppercase(pgrx1.Match[0]);
pgrx2.Expression:='PAL';
if pgrx2.Exec(ss) then begin
rg.region_pal:=True;
rg.region_ntsc:=False;
rg.region_rf:=False;
end;

pgrx2.Expression:='NTSC(\-\w)?';
if pgrx2.Exec(ss) then begin
rg.region_pal:=False;
rg.region_ntsc:=True;
rg.region_rf:=False;
end;

pgrx2.Expression:='Region(s)?[\s]?Free|RF';
if pgrx2.Exec(ss) then rg.region_rf:=True;

pgrx2.Expression:='Not[\s]?Region(s)?[\s]?Free';
if pgrx2.Exec(ss) then rg.region_rf:=False;
UNTIL not pgrx1.ExecNext;
end;
end;

pgrx.Expression:='Genre[^/w]+?([\w]+)';
if pgrx.Exec(text) then rg.game_genre.Add(pgrx.Match[1]);


pgrx.Free;
pgrx1.Free;
pgrx2.Free;
*)
end;

function TPazoGameTask.Execute(slot: Pointer):boolean;
label ujra;
var s: TSiteSlot;
    i, j, k: Integer;
    de: TDirListEntry;
    r: TPazoGameTask;
    d: TDirList;
    event, nfofile: String;
begin
  Result:= False;
  s:= slot;

  if mainpazo.stopped then
  begin
    readyerror:= True;
    exit;
  end;

  Debug(dpMessage, section, Name);

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


    j := 0;
    nfofile := '';
    d := TDirlist.Create(s.site.name, nil, nil, s.lastResponse);
    d.dirlist_lock.Enter;
    try
      for i:= 0 to d.entries.Count-1 do
      begin
        de:= TDirlistEntry(d.entries[i]);
        if ((not de.Directory) and (de.Extension = '.nfo') and (de.filesize < 32768)) then // 32kb-nal nagyobb nfoja csak nincs senkinek
          nfofile:= de.filename;

        if ((de.Directory) or (de.filesize = 0)) then
        begin
          k:= TagComplete(de.filenamelc);
          if j = 0 then j:= k;
          if k = 1 then j:= k;
        end;
      end;

    finally
      d.dirlist_lock.Leave;
      d.Free;
    end;

  if (nfofile = '') then
  begin
    if attempt < config.readInteger(section, 'readd_attempts', 5) then
    begin
      Debug(dpSpam, section, 'READD: nincs meg az nfo file...');

      r:= TPazoGameTask.Create(netname, channel, ps1.name, mainpazo, attempt+1);
      r.startat:= IncSecond(Now, config.ReadInteger(section, 'readd_interval', 60));
      AddTask(r);
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
    readyerror:= True;
    exit;
  end;
  if i = 0 then
    goto ujra;
  // else siker

  if Parse(ss.DataString) then
  begin
    if j = 1 then event:= 'COMPLETE' else event:= 'NEWDIR';
    kb_add(netname, channel, ps1.name, mainpazo.rls.section, '', event, mainpazo.rls.rlsname, '');
  end;// else
//    debug(dpMessage, section, 'Couldnt find imdb url in nfo '+nfofile);

  Result:= True;
  ready:= True;
end;

function TPazoGameTask.Name: String;
begin
//Result:=Format('PGAME %d ROUND:%d',[pazo_id,attempt]);
Result:=Format('PGAME (PazoID:%d) %s [Count:%d]',[pazo_id,mainpazo.rls.rlsname,attempt]);
end;

end.
