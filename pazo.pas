unit pazo;

// EZTA Z UNITOT CSAK A QUEUE_LOCK ZARASA UTAN SZABAD HIVNI!
interface

uses Classes, kb, SyncObjs, Contnrs, dirlist, skiplists, UIntList;

type
  TQueueNotifyEvent = procedure(Sender: TObject; value: Integer) of object;
  TSafeInteger = class
  private
    fC: TCriticalSection;
    fValue: Integer;
  public
    onChange: TQueueNotifyEvent;
    function ActValue: Integer;
    procedure Increase;
    procedure Decrease;
    constructor Create;
    destructor Destroy; override;
  end;
  TCacheFile = class
    dir: String;
    filename: String;
    filesize: Integer;
    constructor Create(dir, filename: string; filesize: Integer);
    destructor Destroy; override;
  end;
  TPazo = class;
  TRlsSiteStatus = (rssNotAllowed, rssNotAllowedButItsThere, rssAllowed, rssShouldPre, rssRealPre, rssComplete, rssNuked);
  TPazoSite = class
  public
    midnightdone: Boolean;
    name: string;
    maindir: string;
    pazo: TPazo;
    //sources: TObjectList;
    destinations: TObjectList;
    destinationRanks: TIntList;
    dirlist: TDirList;

    delay_leech: Integer;
    delay_upload: Integer;

    s_dirlisttasks: TSafeInteger;
    s_racetasks: TSafeInteger;
    s_mkdirtasks: TSafeInteger;

    ircevent: Boolean; // ez jelzi hogy ez a site ircen kapott mar valamit
    error: Boolean; // ha a site lement downba vagy mkd nem sikerult..

    ts: TDateTime;
    lookupforcedhere: Boolean;
    status: TRlsSiteStatus;

    reason: string;
    dirlistgaveup: Boolean;

    badcrcevents: Integer;

    firesourcesinstead: Boolean;

    last_dirlist: TDateTime;
    last_race: TDateTime;

    speed_from: TStringList;
    speed_to: TStringList;

    activeTransfers: TStringList;

    function AllPre: Boolean; // returns true if its a pre or at least it should be
    function Source: Boolean;
    function Complete: Boolean;

    function Age: Integer;
    function AsText: string;
    function RoutesText: string;

    procedure DelaySetup;

    procedure RemoveMkdir;
    procedure MarkSiteAsFailed(echomsg: Boolean = False);

    function ParseDirlist(const netname, channel: string;dir, liststring: string; pre : Boolean = false): Boolean;
    function MkdirReady(dir: string): Boolean;
    function MkdirError(dir: string): Boolean;
    function AddDestination(sitename: string; rank: Integer): Boolean; overload;
    function AddDestination(ps: TPazoSite; rank: Integer): Boolean; overload;
    constructor Create(pazo: TPazo; name, maindir: string);
    destructor Destroy; override;
    procedure ParseXdupe(const netname, channel: string;dir, resp: string; added: Boolean = False);
    function ParseDupe(const netname, channel: string;dir, filename: string; byme: Boolean): Boolean; overload;
    function ParseDupe(const netname, channel: string;dl: TDirlist; dir, filename: string; byme: Boolean): Boolean; overload;
    function SetFileError(const netname, channel: string;dir, filename: string): Boolean;
    function Stats: string;
    function Allfiles: string;
    procedure SetComplete(cdno: string);
    function StatusText: string;
    procedure Clear;
  private
    cds: string;
    function Tuzelj(const netname, channel: string; dir: string; de: TDirListEntry): Boolean;
  end;
  TPazo = class
  private
    lastannounceconsole: string;
    lastannounceirc: string;
    lastannounceroutes: string;
    procedure QueueEvent(Sender: TObject; value: Integer);

    function StatsAllFiles: Integer;

  public
    pazo_id: Integer;

    stated: Boolean;
    cleared: Boolean;

    cs: TCriticalSection;

    completezve: Boolean;

    autodirlist: Boolean;
    srcsite: string; // <- ez a szabalyok tuzelgetesehez kell    -- This rule should tuzelgetesehez
    dstsite: string; // <- ez a szabalyok tuzelgetesehez kell   -- This rule should tuzelgetesehez
    rls: TRelease;

    stopped: Boolean;
    ready: Boolean; // successfully complete all all all
    readyerror: Boolean;
    errorreason: string;
    readyat: TDateTime;
    lastTouch: TDateTime;

    sites: TObjectList;
    sl: TSkipList;

    added: TDateTime;

    main_dirlist: TDirlist; // ez a globalis dirlist   //it is also the global dirlist

     // vagyis ha tobb presite van es egyik vegez akkor ez a valtozo initelve
     // lesz es akkor a tobbi szal nem baszkural dirlistelessel
    queuenumber: TSafeInteger;
    dirlisttasks: TSafeInteger;
    racetasks: TSafeInteger;
    mkdirtasks: TSafeInteger;

    cache_files: TStringList;

    function allfiles: Integer;
    procedure SiteDown(sitename: string);
    procedure Clear;
    function StatusText: string;
    function Age: Integer;
    function AsText: string;
    function RoutesText: string;
    function Stats(console: Boolean; withdirlist: Boolean = True): string;
    function FullStats: string;
    constructor Create(rls: TRelease;pazo_id: Integer);
    destructor Destroy; override;
    function FindSite(sitename: string): TPazoSite;
    function AddSite(sitename, maindir: string; delay: Boolean = True): TPazoSite;
    function AddSites(): Boolean;
    function AddSitesForSpread(): Boolean;
    function PFileSize(dir, filename: string): Integer;
    function PRegisterFile(dir, filename: string; filesize: Integer): Integer;
  end;

function FindPazoById(id: Integer): TPazo;
function FindPazoByName(section, rlsname: string): TPazo;
function FindPazoByRls(rlsname: string): TPazo;
function PazoAdd(rls: TRelease): TPazo;//; addlocal: Boolean = False
procedure PazoInit;
procedure PazoUninit;

function FindMostCompleteSite(pazo: TPazo): TPazoSite;

implementation

uses SysUtils, mainthread, sitesunit, DateUtils, debugunit, queueunit, taskrace, mystrings, irc, sltcp, slhelper,
  Math, helper, taskpretime, configunit, mrdohutils, console, RegExpr;

const section = 'pazo';

var
//    pazos: TObjectList;
    local_pazo_id: Integer;

{ TCacheFile }
constructor TCacheFile.Create(dir, filename: string; filesize: Integer);
begin
  self.dir := dir;
  self.filename := filename;
  self.filesize := filesize;
end;

destructor TCacheFile.Destroy;
begin
  inherited;
end;
    
function FindMostCompleteSite(pazo: TPazo): TPazoSite;
var ps: TPazoSite;
    i: Integer;
begin
  Result:= nil;
  try
    for i:= pazo.sites.Count -1 downto 0 do
    begin
      try if i < 0 then Break; except Break; end;
      ps:= TPazoSite(pazo.sites[i]);
      if ps.lookupforcedhere then //
      begin                                    
        ps.lookupforcedhere:= False;
        Result:= ps;
        exit;
      end;
    end;

    for i:= pazo.sites.Count -1 downto 0 do
    begin
      try if i < 0 then Break; except Break; end;
      ps:= TPazoSite(pazo.sites[i]);
      if ps.ts <> 0 then //
      begin
        Result:= ps;
        exit;
      end;
    end;

    for i:= pazo.sites.Count -1 downto 0 do
    begin
      try if i < 0 then Break; except Break; end;
      ps:= TPazoSite(pazo.sites[i]);
      if (ps.Complete) then
      begin
        Result:= ps;
        exit;
      end;
    end;

    for i:= pazo.sites.Count -1 downto 0 do
    begin
      try if i < 0 then Break; except Break; end;
      ps:= TPazoSite(pazo.sites[i]);
      if (ps.ircevent) then
      begin
        Result:= ps;
        exit;
      end;
    end;


    for i:= pazo.sites.Count -1 downto 0 do
    begin
      try if i < 0 then Break; except Break; end;
      ps:= TPazoSite(pazo.sites[i]);
      if (ps.status in [rssAllowed, rssRealPre, rssComplete]) then
      begin
        Result:= ps;
        exit;
      end;
    end;
  except
    Result:= nil;
  end;
end;

function PazoAdd(rls: TRelease): TPazo;//; addlocal: Boolean = False
begin
  Result:= TPazo.Create(rls, local_pazo_id);
//  if  addlocal then
//    pazos.Add(Result);
  inc(local_pazo_id);
end;


(*
function FindPazoById(id: Integer): TPazo;
var i: integer;
    p: TPazo;
begin
  Result:= nil;
  for i:= pazos.Count -1 downto 0 do
  begin
    p:= TPazo(pazos[i]);
    if p.pazo_id = id then
    begin
      p.lastTouch:= Now();
      Result:= p;
      exit;
    end;
  end;
end;
*)


function FindPazoById(id: Integer): TPazo;
var i: integer;
    p: TPazo;
begin
  Result:= nil;
  try
    for i:= kb_list.Count -1 downto 0 do
    begin
      try if i < 0 then Break; except Break; end;
      p:= TPazo(kb_list.Objects[i]);
      if p.pazo_id = id then
      begin
        Result:= p;
        p.lastTouch:= Now();
        exit;
      end;
    end;
  except
    Result:= nil;
  end;
end;


function FindPazoByName(section, rlsname: string): TPazo;
var i: integer;
begin
  Result:= nil;
  try
    i:= kb_list.IndexOf(section+'-'+rlsname);
    if i <> -1 then
    begin
      Result:= TPazo(kb_list.Objects[i]);
      Result.lastTouch:= Now();
      exit;
    end;
  except
    Result:= nil;
  end;
end;

function FindPazoByRls(rlsname: string): TPazo;
var i: integer;
    p: TPazo;
begin
  Result:= nil;
  try
    for i:= kb_list.Count -1 downto 0 do
    begin
      try if i < 0 then Break; except Break; end;
      p:= TPazo(kb_list.Objects[i]);
      if (p.rls.rlsname = rlsname) then
      begin
        Result := p;
        exit;
      end;
    end;
  except
    Result:= nil;
  end;
end;

procedure PazoInit;
begin
  local_pazo_id:= 0;
//  pazos:= TObjectList.Create;
end;
procedure PazoUnInit;
begin
  Debug(dpSpam, section, 'Uninit1');
//  pazos.Free;
//  pazos:= nil;
  Debug(dpSpam, section, 'Uninit2');
end;


function TPazoSite.Tuzelj(const netname, channel: string; dir: string; de: TDirListEntry): Boolean;
var i: Integer;
    dst: TPazoSite;
    dstdl: TDirList;
    pm: TPazoMkdirTask;
    pr: TPazoRaceTask;
    pd: TPazoDirlistTask;
    dde: TDirListEntry;
begin
  Result:= False;
  if error = True then exit;

  pazo.lastTouch:= Now();

  for i:= destinations.Count -1 downto 0 do
  begin
    try if i < 0 then Break; except Break; end;
    try
      try dst:= TPazoSite(destinations[i]); except Continue; end;
      if error then exit;
      if dst.error then Continue;

      if (dst.badcrcevents > config.ReadInteger('taskrace', 'badcrcevents', 15)) then Continue;

      if dirlist = nil then Continue;
      if dirlist.error then Continue;

      if dst.dirlist = nil then Continue;
      try dstdl:= dst.dirlist.FindDirlist(dir, True); except Continue; end;
      if dstdl = nil then Continue;
      if dstdl.error then Continue;

      // we are in a subdir, and the maindir dont have been mkdir
      if (dir <> '') then
      begin
        if ((dstdl.parent <> nil) and (dstdl.parent.dirlist.need_mkdir)) then Continue;
      end;

      try dde:= dstdl.Find(de.filename); except Continue; end;
      (* if ((dde <> nil) and (dde.done)) then Continue; *)
      if ((dde <> nil) and (dde.megvanmeg)) then Continue;
      if ((dde <> nil) and (dde.error)) then Continue;

      Debug(dpSpam, section, '%s :: Tuzelj, checking routes from %s to %s :: Checking if mkdir is needed on %s', [pazo.rls.rlsname, name, dst.name, dst.name]);
      if ((dstdl.entries <> nil) and (dstdl.entries.Count = 0)) then
      begin
        if ((dstdl.need_mkdir) and (dstdl.dependency_mkdir = '')) then
        begin
          // addolnunk kell TPazoMkdir taszkot dst-re dir-rel  -- Add link must Tazo Mkdir tasks dst-redir-rel
          Debug(dpSpam, section, '%s :: Tuzelj, checking routes from %s to %s :: Adding MKDIR task on %s', [pazo.rls.rlsname, name, dst.name, dst.name]);

          pm:= TPazoMkdirTask.Create(netname, channel, dst.name, pazo, dir);
          if dst.delay_upload > 0 then
            pm.startat:= IncSecond(Now, dst.delay_upload);

          if ((dstdl.parent <> nil) and (dstdl.parent.dirlist.dependency_mkdir <> '')) then
            pm.dependencies.Add(dstdl.parent.dirlist.dependency_mkdir);

          dstdl.dependency_mkdir:= pm.UidText;

          try
            AddTask(pm);
          except
            on e: Exception do
            begin
              Debug(dpError, section, Format('[EXCEPTION] TPazoSite.Tuzelj AddTask(pm): %s', [e.Message]));
              Break;
            end;
          end;
        end;
      end;

      Debug(dpSpam, section, '%s :: Tuzelj, checking routes from %s to %s :: Checking if dirlist is needed on %s', [pazo.rls.rlsname, name, dst.name, dst.name]);
      if ((dst.status <> rssNotAllowed) and (not dstdl.dirlistadded) and (not dst.dirlistgaveup)) then
      begin
        try
          pd:= TPazoDirlistTask.Create(netname, channel, dst.name, pazo, dir, false);

          Debug(dpSpam, section, '%s :: Tuzelj, checking routes from %s to %s :: Dirlist added to %s', [pazo.rls.rlsname, name, dst.name, dst.name]);
          irc_Addtext_by_key('PRECATCHSTATS', Format('<c7>[PAZO RLZ]</c> %s %s Dirlist added to : %s', [pazo.rls.rlsname, pazo.rls.section, dst.name]));
          dstdl.dirlistadded:= true;
          AddTask(pd);
        except
          on e: Exception do
          begin
            Debug(dpError, section, Format('[EXCEPTION] TPazoSite.Tuzelj AddTask(pd): %s', [e.Message]));
            Break;
          end;
        end;
      end;

      if not de.directory then
      begin
        //if dstdl.need_mkdir then Continue;
        
        // addolni kell tpazoracetask-ot
        if not dstdl.complete then
        begin
          if ((dstdl.hassfv) and (AnsiLowerCase(de.Extension) = '.sfv')) then Continue;
          if ((dstdl.hasnfo) and (AnsiLowerCase(de.Extension) = '.nfo')) then Continue;
          if ((dstdl.sfv_status = dlSFVNotFound) and (AnsiLowerCase(de.Extension) <> '.sfv')) then begin
              Debug(dpSpam, section, '%s :: Tuzelj, checking routes from %s to %s :: Not creating racetask, missing sfv on on %s', [pazo.rls.rlsname, name, dst.name, dst.name]);
              Continue;
          end;

//   bis rev 335       if ((dstdl.parent <> nil) and (dstdl.parent.Sample) and (dstdl.entries.Count > 0)) then Continue;

          if ((dstdl.parent <> nil) and (dstdl.entries.Count > 0)) then Continue;

//          if ((dde <> nil) and (dde.tradeCount > config.ReadInteger('taskrace', 'maxsame_trade', 100))) then Continue;

          Debug(dpSpam, section, '%s :: Tuzelj, checking routes from %s to %s :: Adding RACE task on %s %s', [dir, name, dst.name, dst.name, de.filename]);
          pr:= TPazoRaceTask.Create(netname, channel, name, dst.name, pazo, dir, de.filename, de.filesize, destinationRanks[i]);

          if (AnsiLowerCase(de.Extension) = '.sfv') then
            pr.IsSfv:= True;

          if (AnsiLowerCase(de.Extension) = '.nfo') then
            pr.IsNfo:= True;

          if ((AnsiLowerCase(de.Extension) = '.avi') or (AnsiLowerCase(de.Extension) = '.mkv') or (AnsiLowerCase(de.Extension) = '.mp4') or
              (AnsiLowerCase(de.Extension) = '.vob')) then
            pr.IsSample:= True;



          if ((delay_leech > 0) or (dst.delay_upload > 0)) then
          begin
            if delay_leech > dst.delay_upload then
              pr.startat:= IncSecond(Now, delay_leech)
            else
              pr.startat:= IncSecond(Now, dst.delay_upload);
          end;

          if dstdl.dependency_mkdir <> '' then
            pr.dependencies.Add(dstdl.dependency_mkdir);

          try
            AddTask(pr);
          except
            on e: Exception do
            begin
              Debug(dpError, section, Format('[EXCEPTION] TPazoSite.Tuzelj AddTask(pr): %s', [e.Message]));
              Break;
            end;
          end;
        end;
        Result:= True;
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] TPazoSite.Tuzelj Loop: %s', [e.Message]));
        Break;
      end;
    end;
  end;
end;

{ TPazo }

function TPazo.AddSite(sitename, maindir: string; delay: Boolean = True): TPazoSite;
begin
  Result:= TPazoSite.Create(self, sitename, maindir);
  if delay then
    Result.DelaySetup;
  sites.Add(Result);
end;

function TPazo.Age: Integer;
var i: Integer;
    ps: TPazoSite;
    a: Integer;
begin
(*
    ts: TDateTime;

  if ts <> 0 then
  begin
    Result:= SecondsBetween(Now, ts);
    exit;
  end;
*)

  Result:= -1;
  for i:= sites.Count -1 downto 0 do
  begin
    try if i < 0 then Break; except Break; end;
    ps:= TPazoSite(sites[i]);
    a:= ps.Age;
    if ((a <> -1) and ((Result = -1) or (Result < a))) then
      Result:= a; 
  end;

  if Result = -1 then
    Result:= SecondsBetween(Now, added);
end;

function TPazo.AsText: string;
var i: Integer;
   ps: TPazoSite;
begin
  Result:= rls.AsText(pazo_id);
  Result:= Result + 'Age: '+IntToStr(age) + 's'+ #13#10;
  Result:= Result + 'Sites: '+IntToStr(sites.Count) + ''+ #13#10;
  for i:= 0 to sites.Count -1 do
  begin
    try if i > sites.Count then Break; except Break; end;
    ps:= TPazoSite(sites[i]);
    Result:= Result + ps.AsText;
  end;
end;

function TPazo.RoutesText: string;
var i: Integer;
    ps: TPazoSite;
begin
  Result:= '<c3>[ROUTES]</c> : <b>'+rls.rlsname+'</b> ('+IntToStr(sites.Count)+' sites)'+ #13#10;
  for i:= 0 to sites.Count -1 do
  begin
    try
      ps:= TPazoSite(sites[i]);
      Result:= Result + ps.RoutesText;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] TPazo.RoutesText : %s', [e.Message]));
        break;
      end;
    end;
  end;

  if (Result <> lastannounceroutes) then
  begin
    lastannounceroutes:= Result;
  end else begin
    Result:= '';
  end;
  
end;

function TPazo.PRegisterFile(dir, filename: string; filesize: Integer): Integer;
var i: Integer;
    cache_file: TCacheFile;
begin
  Result:= filesize;
  try
    cs.Enter;
    try
      i:= cache_files.IndexOf(dir+'/'+filename);
      if i = -1 then
      begin
        cache_file:= TCacheFile.Create(dir, filename, filesize);
        cache_files.AddObject(dir+'/'+filename, cache_file);
        Result:= filesize;
      end else
      begin
        cache_file:= TCacheFile(cache_files.Objects[i]);
        if cache_file.filesize < filesize then
        begin
          cache_file.filesize := filesize;
        end;
        Result:= cache_file.filesize;
      end;
    finally
      cs.Leave;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TPazo.PRegisterFile: %s', [e.Message]));
      Result:= filesize;
    end;
  end;
end;

function TPazo.PFileSize(dir, filename: string): Integer;
var i: Integer;
begin
  Result:= -1;
  try
    i:= cache_files.IndexOf(dir+'/'+filename);
    if i <> -1 then
      Result:= TCacheFile(cache_files.Objects[i]).filesize;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TPazo.PFileSize: %s', [e.Message]));
      Result:= -1;
    end;
  end;
end;

constructor TPazo.Create(rls: TRelease; pazo_id: Integer);
begin
//  Debug(dpSpam, section, 'TPazo.Create: %s', [rls.rlsname]);
  if rls <> nil then begin
   Debug(dpSpam, section, 'TPazo.Create: %s', [rls.rlsname]);
    sl:= FindSkipList(rls.section);
    self.rls:= rls;
  end else begin
   Debug(dpSpam, section, 'TPazo.Create: SPEEDTEST');
   self.rls:= nil;
  end;

  added:= Now;
  cs:= TCriticalSection.Create;
  autodirlist:= False;
  queuenumber:= TSafeInteger.Create;
  queuenumber.onChange:= QueueEvent;
  dirlisttasks:= TSafeInteger.Create;
  racetasks:= TSafeInteger.Create;
  mkdirtasks:= TSafeInteger.Create;
  main_dirlist:= nil;

  readyerror:= False;
  sites:= TObjectList.Create(False);
  self.pazo_id:= pazo_id;
  stopped:= False;
  ready:= False;
  readyat:= 0;
  lastTouch:= Now();
  cache_files:= TStringList.Create;
  cache_files.CaseSensitive:= False;
  cache_files.Duplicates:= dupIgnore;

  self.stated:= False;
  self.cleared:= False;

  inherited Create;
end;

destructor TPazo.Destroy;
begin
  Debug(dpSpam, section, 'TPazo.Destroy: %s', [rls.rlsname]);
  sites.Free;
  queuenumber.Free;
  dirlisttasks.Free;
  racetasks.Free;
  mkdirtasks.Free;
  cache_files.Free;
  FreeAndNil(rls);
  cs.Free;
  inherited;
end;

function TPazo.FindSite(sitename: string): TPazoSite;
var i: integer;
begin
  Result:= nil;
  try
    for i:= sites.Count -1 downto 0 do
    begin
      try if i < 0 then Break; except Break; end;
      if TPazoSite(sites[i]).name = sitename then
      begin
        Result:= TPazoSite(sites[i]);
        exit;
      end;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TPazo.FindSite: %s', [e.Message]));
      Result:= nil;
    end;
  end;
end;

procedure TPazo.QueueEvent(Sender: TObject; value: Integer);
var s: string;
begin
  if value < 0 then value:=0;


  if (value <> 0) then
  begin
    ready:= False;
    readyerror:= False;
  end
  else
  if value = 0 then
  begin
    readyat:= Now();
    ready:= True;
    if ((not kilepes) and (rls <> nil)) then
    begin
      Debug(dpSpam, section, 'Number of pazo tasks is zero now! '+IntToStr(pazo_id));
      if not stopped then
      begin
        s:= Stats(True, False);
        if ((lastannounceconsole <> s) and (s <> '')) then
        begin
          irc_addtext('CONSOLE', 'Stats', rls.section+' ' +rls.rlsname+' ('+IntToStr(StatsAllFiles)+'): '+s);
          lastannounceconsole:= s;
        end;

        if noannouncesections.IndexOf( rls.section ) <> -1 then exit;

        s:= Stats(False, False);
        if ((lastannounceirc <> s) and (s <> '')) then
        begin
          irc_addstats('<c10>[STATS]</c> '+rls.section+' ' +rls.rlsname+' ('+IntToStr(StatsAllFiles)+'):');
          irc_AddstatsB(Stats(False, True));
          lastannounceirc:= s;
        end;
      end else begin
        irc_addstats('<c10>[STATS]</c> Pazo Stopped.');
      end;
    end;
  end;
end;


function TPazo.StatsAllFiles: Integer;
var i, j: Integer;
begin
  Result:= 0;
  if main_dirlist = nil then
  begin
    for i:= 0 to sites.Count -1 do
    begin
      j:= TPazoSite(sites[i]).dirlist.Done;
      if Result < j then
        Result:= j;
    end;
  end else
    Result:= main_dirlist.Done;

end;

function TPazo.Stats(console: Boolean; withdirlist: Boolean = True):  string;
var i,ii: Integer;
    ps: TPazoSite;
    s: TSite;
begin
  Result:= '';

  ii:=0;
  for i:= 0 to sites.Count -1 do
  begin
    try ps:= TPazoSite(sites[i]); except continue; end;

    try
      if ps.status = rssNotAllowed then Continue;
      s:= FindSiteByName('', ps.name);
      if s = nil then Continue;
      if s.noannounce and not console then Continue;

      if Result <> '' then
        Result := Result + ', ';

      if ((ps.dirlist <> nil) and (ps.dirlist.dirlistadded) and (withdirlist)) then
        Result:=  Result + '"'+ps.Stats+'"'
      else
        Result:= Result + '"'+ps.Stats+'"';
      inc(ii);
    except
      Continue;
    end;
  end;
end;

function TPazo.FullStats: string;
var i: Integer;
    ps: TPazoSite;
//    mysources: TObjectList;
begin
  Result:= '';
 
  for i:= 0 to sites.Count -1 do
  begin
    ps:= TPazoSite(sites[i]);
    if ps.status = rssNotAllowed then Continue;

    if Result <> '' then Result := Result + ', ';

    if ((ps.dirlist <> nil) and (ps.dirlist.dirlistadded)) then
      Result:= Result + ps.Allfiles
    else
      Result:= Result + ps.AllFiles;
  end;
end;

function TPazo.StatusText: string;
var i: Integer;
begin
  Result:= '';
  for i:= 0 to sites.Count -1 do
  begin
    try if i > sites.Count then Break; except Break end;
    Result:= Result + TPazoSite(sites[i]).StatusText;
    if i <> sites.Count -1 then
      Result:= Result + ' ';
  end;
end;

procedure TPazo.Clear;
begin
  try
    RemovePazo(pazo_id);

    completezve:= False;
    stopped:= False; // ha stoppoltak korabban akkor ez most szivas
    ready:= False;
    readyerror:= False;
    errorreason:= '';
    cache_files.Clear;
    sites.Clear;
    main_dirlist:= nil;

    self.cleared:= True;
  except
    on e: Exception do
    begin
      Debug(dpError, section, '[EXCEPTION] TPazo.Clear : %s', [e.Message]);
      exit;
    end;
  end;
end;

function TPazo.AddSites: Boolean;
var s: TSite;
    i: Integer;
    sectiondir: string;
    ps: TPazoSite;
begin
  Result:= False;
  for i:= sitesunit.sites.Count -1 downto 0 do
  begin
    try if i < 0 then Break; except Break; end;
    try
      s:= TSite(sitesunit.sites[i]);
      if s.working = sstDown then Continue;

      sectiondir:= s.sectiondir[rls.section];
      if ((sectiondir <> '') and (s.working <> sstDown) and (nil = FindSite(s.name))) then
      begin
        if TPretimeLookupMOde(config.ReadInteger('taskpretime','mode',0)) <> plmNone then
        begin
          if (DateTimeToUnix(rls.pretime) <> 0) then
          begin
            if (s.IsPretimeOk(rls.section, rls.pretime)) then
            begin
              sectiondir:= TodayCsere(sectiondir);

              Result:= True;
              //ps:= AddSite(s.name, sectiondir);

              ps:= TPazoSite.Create(self, s.name, sectiondir);
              ps.status:= rssNotAllowed;
              if s.IsAffil(rls.section, rls.groupname) then
                ps.status := rssShouldPre;
              sites.Add(ps);
            end;
          end;
        end else
        begin
          sectiondir:= TodayCsere(sectiondir);

          Result:= True;
          //ps:= AddSite(s.name, sectiondir);
          ps:= TPazoSite.Create(self, s.name, sectiondir);
          ps.status:= rssNotAllowed;
          if s.IsAffil(rls.section, rls.groupname) then
            ps.status := rssShouldPre;
          sites.Add(ps);
        end;
      end;
    except
      Continue;
    end;
  end;
end;



// just a copy of AddSites, but only for spread, to use skippre value...
//maybe we cann add it to AddSite, but i dont wanna messup any dev. of _xp :)

function TPazo.AddSitesForSpread:boolean;
var s: TSite;
    i: Integer;
    sectiondir: string;
    ps: TPazoSite;
begin
  Result:= False;
  for i:= 0 to sitesunit.sites.Count -1 do
  begin
    s:= TSite(sitesunit.sites[i]);
    if s.SkipPre then Continue;
    
    sectiondir:= s.sectiondir[rls.section];
    if ((sectiondir <> '') and (s.working <> sstDown) and (nil = FindSite(s.name))) then begin
    sectiondir:= TodayCsere(sectiondir);
    Result:= True;
    //ps:= AddSite(s.name, sectiondir);
    ps:= TPazoSite.Create(self, s.name, sectiondir);
    ps.status:= rssAllowed;//rssNotAllowed;
    if s.IsAffil(rls.section, rls.groupname) then ps.status := rssShouldPre;
    sites.Add(ps);
  end;
 end;
end;


procedure TPazo.SiteDown(sitename: string);
var ps: TPazoSite;
begin
  ps:= FindSite(sitename);
  if ps <> nil then
    ps.MarkSiteAsFailed;
end;


function TPazo.allfiles: Integer;
begin
  Result:= cache_files.Count;
end;

{ TPazoSite }

function TPazoSite.AddDestination(sitename: string; rank: Integer): Boolean;
var ps: TPazoSite;
begin
  Result:= False;
  ps:= pazo.FindSite(sitename);
  if (ps <> nil) then
  begin
    try
      Result:= AddDestination(ps, rank);
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] TPazoSite.AddDestination: %s', [e.Message]));
        Result:= False;
      end;
    end;
  end else pazo.errorreason:='AddDest - PazoSite is NIL';
end;

function TPazoSite.AddDestination(ps: TPazoSite; rank: Integer): Boolean;
var i: Integer;
begin
  Result:= False;
  if error = True then exit;

  try
    pazo.cs.Enter;
    try
      if ps <> nil then
      begin
        if ps.error then exit;

        i:= destinations.Indexof(ps);
        if i = -1 then
        begin
          Result:= True;
          destinations.Add(ps);
          destinationRanks.Add(rank);
          //i:= ps.sources.IndexOf(self);
          //if i = -1 then
          //  ps.sources.Add(self);
        end;
      end else pazo.errorreason:='AddDest - PazoSite is NIL';
    finally
      pazo.cs.Leave;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TPazoSite.AddDestination: %s', [e.Message]));
      Result:= False;
    end;
  end;
end;

function mySpeedComparer(List: TStringList; Index1, Index2: Integer): Integer;
begin
  try
    Result:=
      CompareValue(
        StrToIntDef(list.ValueFromIndex[index2],0),
        StrToIntDef(list.ValueFromIndex[index1],0)
      );
  except
    Result:= 0;
  end;
end;


constructor TPazoSite.Create(pazo: TPazo; name, maindir: string);
begin
  Debug(dpSpam, section, 'TPazoSite.Create: %s', [name]);
  inherited Create;

  activeTransfers:= TStringList.Create;

  self.ts:= 0;
  self.maindir:= maindir;
  self.pazo:= pazo;
  self.Name:= name;
  //sources:= TObjectList.Create(False);
  destinations:= TObjectList.Create(False);
  destinationRanks:= TIntList.Create;

  dirlist:= TDirlist.Create(name, nil, pazo.sl);

  s_dirlisttasks:= TSafeInteger.Create;
  s_racetasks:= TSafeInteger.Create;
  s_mkdirtasks:= TSafeInteger.Create;

  speed_from:= TStringList.Create;
  speed_to:= TStringList.Create;

  try
    sitesdat.ReadSectionValues('speed-from-'+Name, speed_from);
    sitesdat.ReadSectionValues('speed-from-'+Name, speed_to);

    speed_from.CustomSort(myspeedcomparer);
    speed_to.CustomSort(myspeedcomparer);
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TPazoSite.Create speed(s): %s', [e.Message]));
      speed_from.Clear;
      speed_to.Clear;
    end;
  end;
end;

destructor TPazoSite.Destroy;
begin
  Debug(dpSpam, section, 'TPazoSite.Destroy: %s', [name]);
  //sources.Free;
  destinations.Free;
  destinationRanks.Free;
  dirlist.Free;
  activeTransfers.Free;
  s_dirlisttasks.Free;
  s_racetasks.Free;
  s_mkdirtasks.Free;

  speed_from.Free;
  speed_to.Free;

  inherited;
end;

function TPazoSite.MkdirReady(dir: string): Boolean;
var d: TDirList;
begin
  Result:= False;

  d:= dirlist.FindDirlist(dir);
  if d <> nil then
  begin
    debug(dpSpam, section, 'MkdirReady '+name+' '+dir);
    if d.need_mkdir then
    begin
      Result:= True;
    end;
    d.need_mkdir:= False;
    d.dependency_mkdir:= '';
  end;

  try
    RemovePazoMKDIR(pazo.pazo_id, name, dir);
  except
    on e: Exception do
    begin
      Debug(dpError, section, 'TPazoSite.MkdirReady exception in RemovePazoMKDIR : %s', [e.Message]);
      exit;
    end;
  end;
  Result:= True;
end;

function TPazoSite.MkdirError(dir: string): Boolean;
var d: TDirList;
begin
  Result:= False;

  d:= dirlist.FindDirlist(dir);
  if d <> nil then
  begin
    debug(dpSpam, section, 'MkdirError '+name+' '+dir);
    irc_Addstats(Format('<c7>[MKDIR ERROR]</c> : %s %s/%s @ <b>%s</b>', [pazo.rls.section, pazo.rls.rlsname, dir, name]));
    d.need_mkdir:= True;
    d.error:= True;
  end;

  Result:= True;
end;

function TPazoSite.ParseDirlist(const netname, channel: string;dir, liststring: string; pre : Boolean = false): Boolean;
var d: TDirList;
    i: Integer;
    de: TDirListEntry;
begin
  Result:= False;
  if dirlist = nil then exit;
  try d:= dirlist.FindDirlist(dir, True); except exit; end;
  if d = nil then exit;

  try
    pazo.cs.Enter;
    try
      d.ParseDirlist(liststring);
    finally
      pazo.cs.Leave;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, 'TPazoSite.ParseDirlist exception in d.ParseDirlist : %s', [e.Message]);
      exit;
    end;
  end;

  if d.entries = nil then exit;
  if d.entries.Count = 0 then exit;

  try
    pazo.cs.Enter;
    try
      d.Sort;
    finally
      pazo.cs.Leave;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, 'TPazoSite.ParseDirlist exception in d.Sort : %s', [e.Message]);
      exit;
    end;
  end;

  for i:= 0 to d.entries.Count -1 do
  begin
    try if i > d.entries.Count then Break; except Break; end;
    try
      de:= TDirListEntry(d.entries.items[i]);
      if ((not de.skiplisted) and (de.megvanmeg)) then
      begin
        if not de.Directory then
        begin
          if (de.justadded) then
          begin
            de.justadded:= False;
            RemovePazoRace(pazo.pazo_id, name, dir, de.filename);
          end;
        end;
        if not de.Directory then de.filesize:= pazo.PRegisterFile(dir, de.filename, de.filesize);

        pazo.cs.Enter;
        try
          if Tuzelj(netname, channel, dir, de) then
          begin
            QueueFire;
            Result:= True;
          end;
        finally
          pazo.cs.Leave;
        end;
      end;
    except
      Continue;
    end;
  end;

  Result:= True;

  if Result then
    QueueSort;
end;

function TPazoSite.SetFileError(const netname, channel: string;dir, filename: string): Boolean;
var dl: TDirList;
    de: TDirlistEntry;
begin
  Result:= False;
  try
    dl:= dirlist.FindDirlist(dir);
    if dl = nil then pazo.errorreason:='Dirlist is NIL';
    if dl = nil then exit;

    de:= dl.Find(filename);
    if de <> nil then
    begin
      de.error:= true;
    end else
    begin
      de:= TDirListEntry.Create(filename, dl);
      de.error:= true;
      dl.entries.Add(de);
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TPazoSite.ParseDupe: %s', [e.Message]));
      Result:= False;
    end;
  end;
end;

function TPazoSite.ParseDupe(const netname, channel: string;dl: TDirlist; dir, filename: string; byme: Boolean): Boolean;
var de: TDirlistEntry;
    rrgx: TRegExpr;
begin
  Result:= False;

  //test skip
  if ((filename = '.') or (filename = '..') or (filename[1] = '.')) then exit;

  rrgx:=TRegExpr.Create;
  rrgx.ModifierI:=True;
  rrgx.Expression:= config.ReadString('dirlist', 'global_skip', '\-missing$|\-offline$|^\.');
  if rrgx.Exec(filename) then
  begin
    rrgx.Free;
    exit;
  end;
  rrgx.Free;

  //Debug(dpSpam, section, '--> '+Format('%d ParseDupe %s %s %s %s', [pazo.pazo_id, name, pazo.rls.rlsname, dir, filename]));
  try
    de:= dl.Find(filename);
    if de = nil then
    begin
      // ez azt jelenti hogy meg nem tuzeltuk vegig
      de:= TDirListEntry.Create(filename, dl);
      de.directory:= False;
      de.done:= True;
      de.filesize:= -1;
      if byme then
        de.racedbyme:= byme;

      dl.entries.Add(de);
      dl.LastChanged:= Now();
      Result:= True;
    end;
(*
    if ((dl.parent <> nil) and (dl.parent.Sample)) then
      dl.cache_completed:= True;
  *)
    //inc(de.tradeCount);

    if (AnsiLowerCase(de.Extension) = '.sfv') then
    begin
      dl.sfv_status:= dlSFVFound;
    end;

    if not de.done then Result:= True;


    if byme then
      de.racedbyme:= byme;

    de.done:= True;
    if (not de.megvanmeg) then
    begin
      de.megvanmeg:= True;
      RemovePazoRace(pazo.pazo_id, name, dir, filename);
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TPazoSite.ParseDupe: %s', [e.Message]));
      Result:= False;
    end;
  end;
  //Debug(dpSpam, section, '<-- '+Format('%d ParseDupe %s %s %s %s', [pazo.pazo_id, name, pazo.rls.rlsname, dir, filename]));
end;


function TPazoSite.ParseDupe(const netname, channel: string;dir, filename: string; byme: Boolean): Boolean;
var dl: TDirList;
begin
  Result:= False;
  try
    dl:= dirlist.FindDirlist(dir);
    if dl = nil then pazo.errorreason:='Dirlist is NIL';
    if dl = nil then exit;

    pazo.cs.Enter;
    try
      Result:= ParseDupe(netname, channel, dl, dir, filename, byme);
    finally
      pazo.cs.Leave;
    end;

    RemovePazoRace(pazo.pazo_id, name, dir, filename);
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TPazoSite.ParseDupe: %s', [e.Message]));
      Result:= False;
    end;
  end;
end;

procedure TPazoSite.ParseXdupe(const netname, channel: string;dir, resp: string; added: Boolean = False);
var s: string;
    dl: TDirList;
    lines_read: Integer;
begin
  try
    dl:= dirlist.FindDirlist(dir);
    if dl = nil then pazo.errorreason:='Dirlist is NIL';
    if dl = nil then exit;

    lines_read:=0;
    while (true) do
    begin
      s:= Elsosor(resp);
      if s = '' then Break;

      Inc(lines_read);
      if (lines_read > 500) then break;

      //553- X-DUPE: 09-soulless-deadly_sins.mp3
      if (Pos('553- X-DUPE: ', s) = 1) then
      begin
       pazo.cs.Enter;
        try
          ParseDupe(netname, channel, dl, dir, Copy(s, 14, 1000), False);
        finally
          pazo.cs.Leave;
        end;

        RemovePazoRace(pazo.pazo_id, name, dir, Copy(s, 14, 1000));
      end;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TPazoSite.ParseXdupe: %s', [e.Message]));
    end;
  end;
end;

function TPazoSite.Stats: string;
var fsize:real; fsname, fsizetrigger:string;
begin
  fsizetrigger:='KB';
  fsname:=name;
  if status = rssRealPre then fsname:=format('<c10>%s</c>',[Name]);//bCyan(name);
  if status = rssShouldPre then fsname:=format('<c7>%s</c>',[Name]);//Orange(name);
  if status = rssNotAllowed then fsname:=format('<c4>%s</c>',[Name]);//Red(name);

  Result:=fsname;

  if ((not AllPre) and (dirlist <> nil)) then begin
    if dirlist.RacedByMe(true) >= 1 then
    begin
      fsize:= dirlist.SizeRacedByMe(true);
      fsize:= fsize / 1024;

      if fsize > 1024 then begin
        fsize:= fsize / 1024;
        fsizetrigger:='MB';
      end;

      if fsize > 1024 then begin
        fsize:= fsize / 1024;
        fsizetrigger:='GB';
      end;

      Result:= Format('%s-(<b>%d</b>F @ <b>%.2f</b>%s)',[fsname,dirlist.RacedByMe(true),fsize,fsizetrigger])
    end else begin
      if ((status = rssRealPre) or (status = rssShouldPre)) then exit;
      //if ((sources.Count = 0) and (destinations.Count = 0)) then
      if (destinations.Count = 0) then
      begin
        Result:= Format('<c5>%s</c>',[fsname]);
      end else begin
        Result:= Format('<c14>%s</c>',[fsname]);
      end;
    end;
  end;
end;

function TPazoSite.Complete: Boolean;
begin
  // xperia test if dirlist is complete
  Result:= (status in [rssRealPre, rssComplete]) or (dirlist.Complete);
end;

function TPazoSite.Source: Boolean;
begin
  Result:= (status = rssAllowed) or Complete;
end;

procedure TPazoSite.SetComplete(cdno: string);
var i: Integer;
    d: TDirlist;
begin
  if (cdno = '') then
  begin
    d:= dirlist.FindDirlist(cdno);
    if d <> nil then
      d.cache_completed:= true;

    status:= rssComplete;
    exit;
  end;

  cds:= cds + cdno;
  i:= StrToIntDef(cdno, 0);
  if i > dirlist.biggestcd then
    dirlist.biggestcd:= i;

  if dirlist.biggestcd < 2 then
  begin
    exit;
  end;

  for i:= 1 to dirlist.biggestcd do
    if 0 = Pos(IntToStr(i), cds) then
      exit;

  status:= rssComplete;
end;


function TPazoSite.AsText: string;
var i: Integer;
begin
  Result:= '<u><b>SITE: '+name+'</b></u>';
  Result:= Result + ': '+maindir+' ('+IntToStr(dirlist.entries.Count)+' items)';

  if (dirlist.Complete) then
  begin
    Result:= Result + ' <b>COMPLETE</b>';
  end;
  Result:= Result +#13#10;

  
  //Result:= Result + 'Sources: ';
  //for i:= 0 to sources.Count -1 do
  //  Result:= Result + TPazoSite(sources[i]).name+' ';
  //Result:= Result + #13#10;
  Result:= Result + 'Destinations: ';
  for i:= 0 to destinations.Count -1 do
    Result:= Result + TPazoSite(destinations[i]).name+'('+IntToStr(destinationRanks[i])+')'+' ';
  Result:= Result + #13#10;
  Result:= Result + 'Status: ';
  case status of
    rssNotAllowed: Result:= Result + '<c4>not allowed</c> ('+reason+')';
    rssNotAllowedButItsThere: Result:= Result + 'not allowed but its there ('+reason+')';
    rssAllowed: Result:= Result + 'allowed ('+reason+')';
    rssShouldPre: Result:= Result + '(?)pre';
    rssRealPre: Result:= Result + '<b>pre</b>';
    rssComplete: Result:= Result + 'complete ('+reason+')';
    rssNuked: Result:= Result + 'nuked';
  end;
  Result:= Result + #13#10;
end;

function TPazoSite.RoutesText: string;
var i: Integer;
begin
  Result:= '<u>'+name+'</u> ->';
  for i:= 0 to destinations.Count -1 do
  begin
    Result:= Result + TPazoSite(destinations[i]).name+'('+IntToStr(destinationRanks[i])+')'+' ';
  end;
  Result:= Result + #13#10;
end;

function TPazoSite.Age: Integer;
begin
  if ts <> 0 then
  begin
    Result:= SecondsBetween(Now, ts);
    exit;
  end;

  Result:= -1;
  if dirlist <> nil then
    Result:= SecondsBetween(Now, dirlist.LastChanged);
end;

function TPazoSite.Allfiles: string;
begin
  Result:= Name;

  if ((status = rssRealPre) and (pazo.main_dirlist <> self.dirlist)) then
    Result:= Name + '-' + IntToStr(pazo.main_dirlist.Done)
  else
  if dirlist <> nil then
    Result:= Name + '-' + IntToStr(dirlist.Done);
end;

function TPazoSite.StatusText: string;
begin
  Result:= name + '-';
  case status of
    rssNotAllowed: Result:= Result + 'N';
    rssNotAllowedButItsThere: Result:= 'NABIT';
    rssAllowed: Result:= Result + 'A';
    rssShouldPre: Result:= Result + 'S';
    rssRealPre: Result:= Result + 'P';
    rssComplete: Result:= Result + 'C';
  end;
end;

function TPazoSite.AllPre: Boolean;
begin
  Result:= status in [rssShouldPre, rssRealPre];
end;

procedure TPazoSite.Clear;
begin
  error:= False;
  lookupforcedhere:= False;
  firesourcesinstead:= False;
  badcrcevents:= 0;

  dirlistgaveup:= False;
  try
    if dirlist <> nil then
      dirlist.Clear;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TPazoSite.Clear: %s', [e.Message]));
    end;
  end;
end;

procedure TPazoSite.MarkSiteAsFailed(echomsg: Boolean = False);
begin
  error:= True;
  Debug(dpSpam, section, Format('--> TPazoSite.MarkSiteAsFailed', []));
  try
    dirlistgaveup:= True;
    
    RemoveRaceTasks(pazo.pazo_id, name);
    RemoveDirlistTasks(pazo.pazo_id, name);

    if echomsg then
      irc_Addstats(Format('<c7>[SITE FAILED]</c> : %s %s @ <b>%s</b>', [pazo.rls.section, pazo.rls.rlsname, name]));
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TPazoSite.MarkSiteAsFailed: %s', [e.Message]));
    end;
  end;
  Debug(dpSpam, section, Format('<-- TPazoSite.MarkSiteAsFailed', []));
end;

procedure TPazoSite.RemoveMkdir;
begin
  if dirlist <> nil then
  begin
    RemovePazoMKDIR(pazo.pazo_id, name, '');
  end;
end;

procedure TPazoSite.DelaySetup;
var minv, maxv: Integer;
begin
  if not Assigned(pazo.rls) then exit;

  minv:= sitesdat.ReadInteger('site-'+name, 'delayleech-'+pazo.rls.section+'-min', 0);
  maxv:= sitesdat.ReadInteger('site-'+name, 'delayleech-'+pazo.rls.section+'-max', 0);
  if minv <= 0 then
  begin
    minv:= sitesdat.ReadInteger('site-'+name, 'delayleech-global-min', 0);
    maxv:= sitesdat.ReadInteger('site-'+name, 'delayleech-global-max', 0);
  end;

  if minv > 0 then
    delay_leech:= RandomRange(minv, maxv);


  minv:= sitesdat.ReadInteger('site-'+name, 'delayupload-'+pazo.rls.section+'-min', 0);
  maxv:= sitesdat.ReadInteger('site-'+name, 'delayupload-'+pazo.rls.section+'-max', 0);
  if minv <= 0 then
  begin
    minv:= sitesdat.ReadInteger('site-'+name, 'delayupload-global-min', 0);
    maxv:= sitesdat.ReadInteger('site-'+name, 'delayupload-global-max', 0);
  end;

  if minv > 0 then
    delay_upload:= RandomRange(minv, maxv);

end;

{ TSafeInteger }

function TSafeInteger.ActValue: Integer;
begin
  Result:= fValue;
end;

constructor TSafeInteger.Create;
begin
  fC:= TCriticalSection.Create;
  onChange:= nil;
end;

procedure TSafeInteger.Decrease;
begin
  fc.Enter;
  try
    dec(fvalue);
    if fValue < 0 then fValue:= 0;
    if (Assigned(onChange)) then
      onChange(self, fValue);
  finally
    fc.Leave;
  end;
end;

destructor TSafeInteger.Destroy;
begin
  fC.Free;
  inherited;
end;

procedure TSafeInteger.Increase;
begin
  fc.Enter;
  try
    inc(fValue);
    if (Assigned(onChange)) then
      onChange(self, fValue);
  finally
    fc.Leave;
  end;
end;

end.
