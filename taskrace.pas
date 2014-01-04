unit taskrace;

interface

uses SyncObjs, tasksunit, pazo;

type
     TPazoPlainTask = class(TTask) // no announce
       pazo_id: Integer;
       mainpazo: TPazo;
       ps1, ps2: TPazoSite;
       constructor Create(const netname, channel: string; site1: string;site2: string; pazo: TPazo);
       destructor Destroy; override;
     end;
     TPazoTask = class(TPazoPlainTask) // announce
       constructor Create(const netname, channel: string; site1: string;site2: string; pazo: TPazo);
       destructor Destroy; override;
     end;
     TPazoDirlistTask = class(TPazoTask)
       dir: string;
       is_pre: Boolean;
       constructor Create(const netname, channel: string; site: string; pazo: TPazo; dir: string; is_pre: Boolean);
       function Execute(slot: Pointer): Boolean; override;
       function Name: string; override;
     end;
     TPazoMkdirTask = class(TPazoTask)
       dir: string;
       constructor Create(const netname, channel: string; site: string; pazo: TPazo; dir: string);
       function Execute(slot: Pointer): Boolean; override;
       function Name: string; override;
     end;
     TWaitTask = class(TTask)
     public
       event: TEvent;
       wait_for: String;
       destructor Destroy; override;
       constructor Create(const netname, channel: string; site1: string);
       function Execute(slot: Pointer): Boolean; override;
       function Name: string; override;
     end;
     TPazoRaceTask = class(TPazoTask)
       dir: string;
       filename: string;
       storfilename: string;
       rank: Integer;
       filesize: Integer;
       isSfv: Boolean;
       isSample:boolean;
       isNFO:boolean;
       dontRemoveOtherSources: Boolean;
       dst: TWaitTask;
       constructor Create(const netname, channel: string; site1: string;site2: string; pazo: TPazo; dir, filename: string; filesize, rank: Integer);
       function Execute(slot: Pointer): Boolean; override;
       function Name: string; override;
     end;

implementation

uses StrUtils, kb, helper, sitesunit, configunit, taskdel, DateUtils, SysUtils, mystrings, statsunit
, slstack, DebugUnit, queueunit, irc, dirlist, midnight, speedstatsunit,// console,
rulesunit, mainthread, Regexpr, mrdohutils;

const c_section = 'taskrace';

{ TLoginTask }

constructor TPazoPlainTask.Create(const netname, channel: string; site1: string;site2: string; pazo: TPazo);
begin
  // egy taszk letrehozasakor es felszabaditasakor a queue lock mindig aktiv
  //tasks can create a queue and release the lock still active
  mainpazo:= pazo; //FindPazoById(pazo_id);
  if mainpazo = nil then raise Exception.Create('Pazo not found');
  self.pazo_id:= mainpazo.pazo_id;
  mainpazo.lastTouch:= Now();

  ps1:= mainpazo.FindSite(site1);
//  if ps1 = nil then raise Exception.Create('PazoSite1 not found');
  ps2:= nil;
  if site2 <> '' then
  begin
    ps2:= mainpazo.FindSite(site2);
//    if ps2 = nil then raise Exception.Create('PazoSite2 not found');
  end;

  inherited Create(netname, channel, site1, site2);
end;

destructor TPazoPlainTask.Destroy;
begin
  if readyerror then mainpazo.readyerror:= True;

  inherited;
end;


constructor TPazoTask.Create(const netname, channel: string; site1: string;site2: string; pazo: TPazo);
begin
  inherited Create(netname, channel, site1, site2, pazo);
  mainpazo.queuenumber.increase;

  if ClassType = TPazoRaceTask then
  begin
    mainpazo.racetasks.Increase;
    ps1.s_racetasks.Increase;
  end;
  if ClassType = TPazoMkdirTask then
  begin
    mainpazo.mkdirtasks.Increase;
    ps1.s_mkdirtasks.Increase;
  end;
  if ClassType = TPazoDirlistTask then
  begin
    mainpazo.dirlisttasks.Increase;
    ps1.s_dirlisttasks.Increase;
  end;
end;

destructor TPazoTask.Destroy;
begin
  mainpazo.queuenumber.Decrease;

  if ClassType = TPazoRaceTask then
  begin
    mainpazo.racetasks.Decrease;
    ps1.s_racetasks.Decrease;
  end;
  if ClassType = TPazoMkdirTask then
  begin
    mainpazo.mkdirtasks.Decrease;
    ps1.s_mkdirtasks.Decrease;
  end;
  if ClassType = TPazoDirlistTask then
  begin
    mainpazo.dirlisttasks.Decrease;
    ps1.s_dirlisttasks.Decrease;
  end;

  inherited;
end;



{ TPazoDirlistTask }

constructor TPazoDirlistTask.Create(const netname, channel: string; site: string; pazo: TPazo; dir: string; is_pre: Boolean);
begin
  self.dir:= dir;
  self.is_pre:= is_pre;
  inherited Create(netname, channel, site, '', pazo);
end;

function TPazoDirlistTask.Execute(slot: Pointer): Boolean;
label ujra;
var s: TSiteSlot;
    i: Integer;
    de: TDirListEntry;
    r, r_dst: TPazoDirlistTask;
    d: TDirList;
    aktdir: string;
    voltadd: Boolean;
    numerrors: Integer;
    tname: string;
    ps: TPazoSite;
begin
  numerrors := 0;
  Result:= False;
  s:= slot;
  tname:= Name;
  voltadd:= False;

  if mainpazo.stopped then
  begin
    readyerror:= True;
    mainpazo.errorreason:='Mainpazo stopped.';
    exit;
  end;

  Debug(dpSpam, c_section, '--> '+tname);

  mainpazo.lastTouch:= Now();

ujra:
  if ((ps1.error) or (ps1.dirlistgaveup) or (ps1.status = rssNuked) or (kilepes)) then
  begin
    readyerror:= True;

    if ps1.error then
    mainpazo.errorreason:='ERROR PS1';

    if ps1.dirlistgaveup then
    mainpazo.errorreason:='ERROR PS1: dirlistgaveup';

    if ps1.status = rssNuked then
    mainpazo.errorreason:='ERROR PS1: status = Nuked';


    Debug(dpSpam, c_section, '<-- '+tname);
    exit;
  end;

  try
    inc(numerrors);
    if numerrors > 3 then
    begin
      readyerror:= True;
      mainpazo.errorreason:=' TPazoDirlistTask -> numerror > 3';
      irc_Adderror(Format('<c4>[ERROR]</c> %s %s', [tname, s.lastResponse]));
      Debug(dpMessage, c_section, '<-- ERROR '+tname+' '+s.lastResponse);
      exit;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, c_section, Format('[EXCEPTION] TPazoDirlistTask error: %s', [e.Message]));
      readyerror:= True;
      exit;
    end;
  end;

  try
    if s.status <> ssOnline then
    begin
      if not s.ReLogin(0, False, 'TPazoDirlistTask') then
      begin
        mainpazo.errorreason:='Cant login. ';
        readyerror:= True;
        Debug(dpMessage, c_section, '<-- No LOGIN '+tname);
        exit;
      end;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, c_section, Format('[EXCEPTION] TPazoDirlistTask relogin: %s', [e.Message]));
      readyerror:= True;
      exit;
    end;
  end;


(* Old code!
  mainpazo.cs.Enter;
  // ha nem minket osztott ki a sors a globalis dirlist keszitesere akkor kilepunk
  //if we do not split the fate of the global dirlist preparation also exits
  if ((pre) and (mainpazo.dirlist <> nil) and (mainpazo.dirlist <> ps1.dirlist)) then
  begin

    if ps1.CopyMainDirlist(netname, channel, dir) then
      goto folytatas;

  end;


  if ((pre) and (mainpazo.dirlist = nil)) then
    mainpazo.dirlist:= ps1.dirlist;
  mainpazo.cs.Leave;

  *)




  if ((not ps1.midnightdone) and (IsMidnight(mainpazo.rls.section))) then
  begin
    if not s.Cwd(ps1.maindir, True) then
    begin
      if s.Status <> ssOnline then
        goto ujra;

      ps1.MarkSiteAsFailed;
      //mainpazo.errorreason:=ps1.name+' is marked as fail';
      mainpazo.errorreason:= 'Section dir on '+site1+' does not exist, marked as fail';
      readyerror:= true;
      Debug(dpMessage, c_section, '<-- '+tname);
      exit;
    end;

    if not s.Pwd(ps1.maindir) then
    begin
      if s.Status <> ssOnline then
        goto ujra;

      ps1.MarkSiteAsFailed;
//      mainpazo.errorreason:=ps1.name+' is marked as fail';
      mainpazo.errorreason:= 'Section dir on '+site1+ ' does not exist, marked as fail';
      readyerror:= true;
      Debug(dpMessage, c_section, '<-- '+tname);
      exit;
    end;

    ps1.midnightdone:= True;
  end;

  if not s.Dirlist(MyIncludeTrailingSlash(ps1.maindir)+MyIncludeTrailingSlash(mainpazo.rls.rlsname)+dir) then
  begin
   mainpazo.errorreason:= 'Src dir on '+site1+' does not exist';
    if ((s.lastResponseCode = 550) and (0 <> Pos('FileNotFound', s.lastResponse))) then
    begin
      // do nothing
    end else if ((s.lastResponseCode = 550) and (0 <> Pos('File not found', s.lastResponse))) then
    begin
     // do nothing
    end else if ((s.lastResponseCode = 550) and (0 <> Pos('No such file or directory', s.lastResponse))) then
    begin
     // do nothing
    end else
    begin
      goto ujra;
    end;
  end else
  begin
    ps1.last_dirlist:= Now();

    debug(dpSpam, c_section, 'ParseDirlist profiling 1');
    try
    voltadd:= ps1.ParseDirlist(netname, channel, dir, s.lastResponse, is_pre);
    except
      on e: Exception do
      begin
        Debug(dpError, c_section, '[EXCEPTION] ParseDirlist: %s', [e.Message]);
        mainpazo.errorreason:='do not have the dir';
        readyerror:= True;
        exit;
      end;
    end;
    debug(dpspam, c_section, 'ParseDirlist profiling 2');
  end;

  try
    d:= ps1.dirlist.FindDirlist(dir);

    // Search for sub dir
    if ((d <> nil) and (d.entries <> nil) and (d.entries.Count > 0)) then
    begin
      for i:= 0 to d.entries.Count -1 do
      begin
        try if i > d.entries.Count then Break; except Break; end;
        try
          de:= TDirlistEntry(d.entries[i]);

          if ((de.directory) and (not de.skiplisted)) then
          begin
//            if ((de.subdirlist <> nil) and (de.subdirlist.Complete)) then Continue; // kihagyjuk...
            if ((de.subdirlist <> nil) and (de.subdirlist.dirlistadded)) then Continue;

            //tpazodirlisttask addolasa de.filename -mel bovitve dir-t
            aktdir:= dir;
            if aktdir <> '' then aktdir:= aktdir + '/';
            aktdir:= aktdir + de.filename;
            Debug(dpSpam, c_section, 'READD: adding dirlist task to subdir '+aktdir);
            irc_Addtext_by_key('PRECATCHSTATS', Format('<c7>[DIRLIST RLZ]</c> %s %s %s Dirlist added to : %s', [mainpazo.rls.section, mainpazo.rls.rlsname, aktdir, site1]));
            try
              r:= TPazoDirlistTask.Create(netname, channel, site1, mainpazo, aktdir, is_pre);
              if (de.subdirlist <> nil) then
                de.subdirlist.dirlistadded:= True;
              AddTask(r);
            except
              on e: Exception do
              begin
                Debug(dpError, c_section, Format('[EXCEPTION] TPazoDirlistTask AddTask: %s', [e.Message]));
              end;
            end;
          end;
        except
          Continue;
        end;
      end;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, c_section, Format('[EXCEPTION] TPazoDirlistTask: %s', [e.Message]));
    end;
  end;

  if ((not is_pre) and (d <> nil) and (d.Complete) and (ps1.status <> rssComplete)) then
  begin
    if (dir <> '') then
    begin
      ps1.SetComplete(dir);
    end else
    begin
      ps1.status:= rssComplete;
    end;
  end;
  
  // check if need to give up
  if ((d <> nil) and (not d.Complete)) then
  begin
    if ((d.entries <> nil) and (d.entries.Count = 0) and (SecondsBetween(Now, d.LastChanged) > config.ReadInteger(c_section, 'newdir_max_empty', 60))) then
    begin
      if spamcfg.readbool(c_section,'incomplete',True) then
        irc_Addstats(Format('<c11>[EMPTY RLS]</c> %s: %s %s is still empty, giving up...',[site1, mainpazo.rls.section, mainpazo.rls.rlsname + ' ' + dir]));
      ps1.dirlistgaveup:= True;
    end;

    if ((d.entries <> nil) and (d.entries.Count > 0) and (SecondsBetween(Now, d.LastChanged) > config.ReadInteger(c_section, 'newdir_max_unchanged', 60))) then
    begin
      if spamcfg.readbool(c_section,'incomplete',True) then
        irc_Addstats(Format('<c11>[iNC RLS]</c> %s: %s %s is still incomplete, giving up...',[site1, mainpazo.rls.section, mainpazo.rls.rlsname + ' ' + dir]));
      ps1.dirlistgaveup:= True;
    end;
  end;

  if ((not is_pre) and (d.entries <> nil) and (d.date_started <> 0) and (SecondsBetween(Now, d.date_started) > config.ReadInteger(c_section, 'newdir_max_created', 600))) then
  begin
    if spamcfg.readbool(c_section,'incomplete',True) then
      irc_Addstats(Format('<c11>[LONG RLS]</c> %s: %s %s, giving up...',[site1, mainpazo.rls.section, mainpazo.rls.rlsname + ' ' + dir]));
    ps1.dirlistgaveup:= True;
  end;
  if ((not is_pre) and (d.entries <> nil) and (d.date_completed <> 0) and (SecondsBetween(Now, d.date_completed) > config.ReadInteger(c_section, 'newdir_max_completed', 300))) then
  begin
    if spamcfg.readbool(c_section,'incomplete',True) then
      irc_Addstats(Format('<c11>[FULL RLS]</c> %s: %s %s is complete, giving up...',[site1, mainpazo.rls.section, mainpazo.rls.rlsname + ' ' + dir]));
    ps1.dirlistgaveup:= True;
  end;
  if ((is_pre) and (d.entries <> nil) and (d.date_completed <> 0) and (SecondsBetween(Now, d.date_completed) > config.ReadInteger(c_section, 'newdir_max_completed', 300))) then
  begin
    if spamcfg.readbool(c_section,'incomplete',True) then
      irc_Addstats(Format('<c11>[PRE RLS]</c> %s: %s %s, giving up...',[site1, mainpazo.rls.section, mainpazo.rls.rlsname + ' ' + dir]));
    ps1.dirlistgaveup:= True;
  end;

  // check if need more dirlist
  voltadd:= False;
  if (not ps1.dirlistgaveup) then
  begin
    // check if still incomplet
    if ((d <> nil) and (not is_pre) and (not d.Complete)) then
    begin
      // dirlisst more
      r:= TPazoDirlistTask.Create(netname, channel, ps1.name, mainpazo, dir, is_pre);
      r.startat:= IncMilliSecond(Now(), config.ReadInteger(c_section, 'newdir_dirlist_readd', 1000));

      try
        AddTask(r);
        voltadd:= True;
      except
        on e: Exception do
        begin
          Debug(dpError, c_section, Format('[EXCEPTION] TPazoDirlistTask AddTask: %s', [e.Message]));
        end;
      end;
    end;

    // check if one dst need more dirlist
    if (not voltadd) then
    begin
      for i := ps1.destinations.Count - 1 downto 0 do
      begin
        try if i > ps1.destinations.Count then Break; except Break; end;
        if voltadd then Break;

        try
          ps:= TPazoSite(ps1.destinations[i]);
          
          if (ps.error) then Continue;
          if (ps.dirlistgaveup) then Continue;

          if ((is_pre) and (ps.status in [rssAllowed]) and (ps.dirlist <> nil) and (not ps.dirlist.Complete) and (not ps.dirlist.error)) then
          begin
            // dirlisst more
            r:= TPazoDirlistTask.Create(netname, channel, ps1.name, mainpazo, dir, is_pre);
            r.startat:= IncMilliSecond(Now(), config.ReadInteger(c_section, 'newdir_dirlist_readd', 1000));
            r_dst:= TPazoDirlistTask.Create(netname, channel, ps.name, mainpazo, dir, false);
            r_dst.startat:= IncMilliSecond(Now(), config.ReadInteger(c_section, 'newdir_dirlist_readd', 1000));

            try
              AddTask(r);
              AddTask(r_dst);
              voltadd:= True;
              Break;
            except
              on e: Exception do
              begin
                Debug(dpError, c_section, Format('[EXCEPTION] TPazoDirlistTask AddTask: %s', [e.Message]));
              end;
            end;
          end;

          if ((ps.status in [rssAllowed]) and (ps.dirlist <> nil) and (not ps.dirlist.Complete) and (ps.dirlist.entries.Count > 0) and (not ps.dirlist.error)) then
          begin
            // dirlisst more
            r:= TPazoDirlistTask.Create(netname, channel, ps1.name, mainpazo, dir, is_pre);
            r.startat:= IncMilliSecond(Now(), config.ReadInteger(c_section, 'newdir_dirlist_readd', 1000));
            r_dst:= TPazoDirlistTask.Create(netname, channel, ps.name, mainpazo, dir, false);
            r_dst.startat:= IncMilliSecond(Now(), config.ReadInteger(c_section, 'newdir_dirlist_readd', 1000));

            try
              AddTask(r);
              AddTask(r_dst);
              voltadd:= True;
              Break;
            except
              on e: Exception do
              begin
                Debug(dpError, c_section, Format('[EXCEPTION] TPazoDirlistTask AddTask: %s', [e.Message]));
              end;
            end;
          end;
        except
          Continue;
        end;
      end;
    end;
  end;

  Debug(dpSpam, c_section, '<-- '+tname);

  Result:= True;
  ready:= True;
end;

function TPazoDirlistTask.Name: string;
begin
  try
    if is_pre then
      Result:= 'PDIRLIST '+site1+' '+IntToStr(pazo_id)+' PRE '+mainpazo.rls.section+' '+mainpazo.rls.rlsname+' '+dir+' '+ScheduleText
    else
      Result:= 'PDIRLIST '+site1+' '+IntToStr(pazo_id)+' '+mainpazo.rls.section+' '+mainpazo.rls.rlsname+' '+dir+' '+ScheduleText;
  except
    Result:= 'PDIRLIST';
  end;
end;


{ TPazoMkdirTask }

constructor TPazoMkdirTask.Create(const netname, channel: string; site: string; pazo: TPazo; dir: string);
begin
  self.dir:= dir;
  inherited Create(netname, channel, site, '',  pazo);
end;

function TPazoMkdirTask.Execute(slot: Pointer): Boolean;
label ujra;
var s: TSiteSlot;
    aktdir, fulldir: string;
    hiba: Boolean;
    m: Boolean;
    r: TRule;
    e: string;
    grp: string;
    numerrors: Integer;
    tname: string;
begin
  numerrors:=0;
  Result:= False;
  s:= slot;
  tname:= Name;

  if mainpazo.stopped then
  begin
    mainpazo.errorreason:='MainPazo Stopped';
    readyerror:= True;
    exit;
  end;

  Debug(dpMessage, c_section, '--> '+tname);

  mainpazo.lastTouch:= Now();

ujra:
  if ((ps1.error) or (kilepes)) then
  begin
    readyerror:= True;
    mainpazo.errorreason:='ERROR PS1 or PS2';
    Debug(dpSpam, c_section, '<-- '+tname);
    exit;
  end;

  try
    inc(numerrors);
    if numerrors > 3 then
    begin
      irc_Adderror(Format('<c4>[ERROR] loop</c> %s', [tname]));
      mainpazo.errorreason:='MKDir Pazo errornum > 3';
      readyerror:= True;
      exit;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, c_section, Format('[EXCEPTION] TPazoMkdirTask error: %s', [e.Message]));
      readyerror:= True;
      exit;
    end;
  end;
  
  if s.status <> ssOnline then
    if not s.ReLogin(0, False, 'TPazoMkdirTask') then
    begin
      irc_Adderror(Format('<c4>[ERROR] site down</c> %s', [tname]));
      mainpazo.errorreason:='Site Down';
      readyerror:= True;
      exit;
    end;

  m:= IsMidnight(mainpazo.rls.section);

  try
    if not s.Cwd(ps1.maindir, m) then
    begin
      ps1.MarkSiteAsFailed(True);
      irc_Adderror(Format('<c4>[ERROR] cant CWD</c> %s', [tname]));
      mainpazo.errorreason:=ps1.name+' marked as failed';
      readyerror:= true;
      exit;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, c_section, Format('[EXCEPTION] TPazoMkdirTask Section dir does not exist: %s', [e.Message]));
      readyerror:= True;
      exit;
    end;
  end;

  try
    if m then
    begin
      if not s.Pwd(ps1.maindir) then
      begin
        ps1.MarkSiteAsFailed;
        mainpazo.errorreason:=ps1.name+' marked as failed';
        readyerror:= true;
        exit;
      end;

      ps1.midnightdone:= True;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, c_section, Format('[EXCEPTION] TPazoMkdirTask Section dir does not exist:%s', [e.Message]));
      readyerror:= True;
      exit;
    end;
  end;

  aktdir:= MyIncludeTrailingSlash(mainpazo.rls.rlsname) + dir;
  if not s.Mkdir(aktdir) then goto ujra;


  hiba:= False;
  if s.lastResponseCode <> 257 then
  begin
    if ((s.lastResponseCode = 550) and (0 <> Pos('File exists', s.lastResponse))) then
    begin
      hiba:= False;
    end else if ((s.lastResponseCode = 550) and (0 <> Pos('already exists', s.lastResponse))) then
    begin
      hiba:= False;
    end else if ((s.lastResponseCode = 550) and (0 <> Pos('the parent of that directory does not exist', s.lastResponse)) and (dir <> '')) then
    begin
      hiba:= False;
    end else if ((s.lastResponseCode = 550) and (0 <> Pos('Dupe detected', s.lastResponse))) then
    begin
      hiba:= False;
    end else if ((s.lastResponseCode = 550) and (0 <> Pos('is already on site', s.lastResponse))) then
    begin
      hiba:= False;
    end else if ((s.lastResponseCode = 213) and (0 <> Pos('status of', s.lastResponse))) then
    begin
      hiba:= False;
    end else if ((s.lastResponseCode = 550) and (0 <> Pos('Denying creation of', s.lastResponse))) then
    begin
      if spamcfg.ReadBool('taskrace','denying_creation_of',True) then
        irc_Adderror(s.todotask, '<c4>[DENIED]</c> %s', [tname]);
      hiba:= True;
    end else if ((s.lastResponseCode = 550) and (0 <> Pos('BLOCKED:', s.lastResponse))) then
    begin
      if spamcfg.ReadBool('taskrace','denying_creation_of',True) then
        irc_Adderror(s.todotask, '<c4>[DENIED]</c> %s', [tname]);
      hiba:= True;
    end else if ((s.lastResponseCode = 550) and (0 <> Pos('Denied by dirscript', s.lastResponse))) then
    begin
      if spamcfg.ReadBool('taskrace','denying_creation_of',True) then
        irc_Adderror(s.todotask, '<c4>[DENIED]</c> %s', [tname]);
      hiba:= True;
    end else if ((s.lastResponseCode = 533) and (0 <> Pos('File name not allowed', s.lastResponse))) then
    begin
      if spamcfg.ReadBool('taskrace','filename_not_allowed',True) then
        irc_Adderror(s.todotask, '<c4>[NOT ALLOWED]</c> %s', [tname]);
      hiba:= True;
    end else if ((0 <> Pos('Denied', s.lastResponse)) or (0 <> Pos('Denying', s.lastResponse)))  then
    begin
      if config.ReadBool(c_section, 'autoruleadd', true) then
      begin
        if ((s.lastResponseCode = 550) and (0 <> Pos('releases are not accepted here', s.lastResponse))) then
        begin
          // auto adding blacklist rule
          e:= s.lastResponse;
          grp := Fetch(e, ' ');
          grp := Fetch(e, ' '); // masodik szo
          e:= '';
          Debug(dpSpam, c_section, 'Adding grp %s to blacklist on %s', [grp, site1]);
          irc_Addadmin(Format('Adding grp %s to blacklist on %s', [grp, site1]));

          r:= AddRule(Format('%s %s if group = %s then DROP', [site1, mainpazo.rls.section, grp]), e);
          if ((r <> nil) and (e = '')) then
          begin
            rules.Insert(0, r);
            RulesSave;
          end;
        end;
      end;
      if spamcfg.ReadBool('taskrace','cant_create_dir',True) then
        irc_Adderror(s.todotask, '<c4>[MKDIR Denied]</c> TPazoMkdirTask %s: %s', [s.Name, s.lastResponse]);
      hiba:= True;
    end else begin
      if spamcfg.ReadBool('taskrace','denying_creation_of',True) then
        irc_Adderror(s.todotask, '<c4>[ERROR MKDIR]</c> TPazoMkdirTask %s: %s', [tname, s.lastResponse]);
      hiba:= True;
    end;
  end;

  try
    if (hiba) then
    begin
      fulldir := MyIncludeTrailingSlash(ps1.maindir) + MyIncludeTrailingSlash(mainpazo.rls.rlsname) + dir;
      if not s.Cwd(fulldir) then
      begin
        irc_Adderror(Format('<c4>[ERROR]</c> %s %s', [tname, s.lastResponse]));
        ps1.MkdirError(dir);
        if (dir = '') then
        begin
          ps1.MarkSiteAsFailed(True);
        end;
        Result:= True;
        readyerror:= true;
        exit;
      end;
    end;
    ps1.MkdirReady(dir);
  except
    on e: Exception do
    begin
      Debug(dpError, c_section, '[EXCEPTION] TPazoSite.MkdirReady : %s', [e.Message]);
      mainpazo.errorreason:=ps1.name+' marked as failed';
      readyerror:= true;
      exit;
    end;
  end;

  try
    // echo race info
    irc_SendRACESTATS(tname);
  except
    on e: Exception do
    begin
      Debug(dpError, c_section, '[EXCEPTION] TPazoMkdirTask Stats : %s', [e.Message]);
    end;
  end;

  Debug(dpMessage, c_section, '<-- '+tname);

  Result:= True;
  readyerror:= hiba;
  ready:= True;
end;


function TPazoMkdirTask.Name: string;
begin
  try
    Result:= 'MKDIR <b>'+site1+'</b> '+mainpazo.rls.rlsname+' '+dir;
  except
     Result:= 'MKDIR';
  end;
end;



{ TPazoRaceTask }

constructor TPazoRaceTask.Create(const netname, channel: string; site1, site2: string; pazo: TPazo;
  dir, filename: string; filesize, rank: Integer);
begin
  inherited Create(netname, channel, site1, site2, pazo);
  self.dir:= dir;
  self.rank:= rank;
  self.filename:= filename;
  self.storfilename:= filename;
  self.filesize:= filesize;
end;


function TPazoRaceTask.Execute(slot: Pointer): Boolean;
label ujra, brokentransfer, retrujra;
var ssrc, sdst: TSiteSlot;
    kellssl: Boolean;
    host: string;
    port: Integer;
    byme: Boolean;
    numerrors: Integer;
    elotte, utana: TDateTime;
    fs: Double;
    time_race: Integer;
    todir1, todir2: string;
    rss, rsd: Boolean;
    tname: string;
    speed_stat: string;
    rrgx: TRegExpr;
    lastResponseCode: Integer;
    lastResponse: String;
begin
  Result:= False;
  ssrc:= slot1;
  sdst:= slot2;
  numerrors:= 0;
  tname:= name;

  if mainpazo.stopped then
  begin
    mainpazo.errorreason:='Mainpazo stopped!';
    readyerror:= True;
    exit;
  end;

  if (ps2.badcrcevents > config.ReadInteger('taskrace', 'badcrcevents', 15)) then
  begin
    mainpazo.errorreason:='Too many CRC errors!';
    readyerror:= True;
    exit;
  end;

  mainpazo.lastTouch:= Now();
  Debug(dpMessage, c_section, '--> '+tname);

ujra:
  if ((ps1.error) or (ps2.error) or (ps1.status = rssNuked) or (ps2.status = rssNuked) or (kilepes)) then
  begin
    readyerror:= True;
    mainpazo.errorreason:='ERROR PS1 or PS2';
    Debug(dpMessage, c_section, '<- '+mainpazo.errorreason+' '+tname);
    exit;
  end;

  try
    inc(numerrors);
    if numerrors > 3 then
    begin
      if ssrc.status <> ssOnline then
        ssrc.DestroySocket(True);
      if sdst.status <> ssOnline then
        sdst.DestroySocket(True);

      irc_Adderror(Format('<c4>[ERROR] Protocol</c> %s', [tname]));
      mainpazo.errorreason:='PazoRaceTask numerror > 3';
      readyerror:= True;
      Debug(dpMessage, c_section, '<- '+tname);
      exit;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, c_section, '[EXCEPTION] Taskrace Protocol errors: %s', [e.Message]);
      mainpazo.errorreason:='PazoRaceTask numerror > 3';
      readyerror:= True;
      exit;
    end;
  end;

  if ssrc.status <> ssOnline then
    if not ssrc.ReLogin(0, False, 'TPazoRaceTask') then
    begin
      mainpazo.errorreason:='Site '+ssrc.site.Name+' is offline';
      readyerror:= True;
      Debug(dpMessage, c_section, '<- '+mainpazo.errorreason+' '+tname);
      exit;
    end;
  if sdst.status <> ssOnline then
    if not sdst.ReLogin(0, False, 'TPazoRaceTask') then
    begin
      mainpazo.errorreason:='Site '+sdst.site.Name+' is offline';
      readyerror:= True;
      Debug(dpMessage, c_section, '<- '+mainpazo.errorreason+' '+tname);
      exit;
    end;

  if mainpazo.rls <> nil then
    todir1:= MyIncludeTrailingSlash(ps1.maindir) + MyIncludeTrailingSlash(mainpazo.rls.rlsname) + dir
  else
    todir1:= MyIncludeTrailingSlash(ps1.maindir) + dir;

  try
    if not ssrc.Cwd(todir1) then
    begin
      if not ssrc.Cwd(todir1) then
      begin
        irc_Adderror(Format('<c4>[ERROR]</c> %s : %s', [tname, 'Src '+todir1+' on '+site1+' does not exist']));
        if (dir = '') then
        begin
          ps1.MarkSiteAsFailed(True);
        end;
        readyerror:= True;
        mainpazo.errorreason:='cant cwd on src';
        Debug(dpMessage, c_section, '<- '+mainpazo.errorreason+' '+tname);
        exit;
      end;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, c_section, '[EXCEPTION] Taskrace Src dir  does not exist: %s', [e.Message]);
      mainpazo.errorreason:='cant cwd on src';
      readyerror:= True;
      exit;
    end;
  end;

  if mainpazo.rls <> nil then
    todir2:= MyIncludeTrailingSlash(ps2.maindir) + MyIncludeTrailingSlash(mainpazo.rls.rlsname) + dir
  else
    todir2:= MyIncludeTrailingSlash(ps2.maindir) + dir;

  try
    if not sdst.Cwd(todir2) then
    begin
      if not sdst.Cwd(todir2) then
      begin
        irc_Adderror(Format('<c4>[ERROR]</c> %s : %s', [tname, 'Dst '+todir2+' on '+site2+' does not exist']));
        if (dir = '') then
        begin
          ps2.MarkSiteAsFailed(True);
        end;
        readyerror:= True;
        mainpazo.errorreason:='cant cwd on dst';
        Debug(dpMessage, c_section, '<- '+mainpazo.errorreason+' '+tname);
        exit;
      end;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, c_section, '[EXCEPTION] Taskrace Dst dir  does not exist: %s', [e.Message]);
      mainpazo.errorreason:='cant cwd on dest';
      readyerror:= True;
      exit;
    end;
  end;

  if ((ssrc.site.sslfxp = srNeeded) and (sdst.site.sslfxp = srUnsupported)) then
  begin
    Debug(dpSpam, c_section, 'SSLFXP on site %s is not supported', [sdst.site.name]);
    irc_Adderror(Format('<c4>[ERROR]</c> SSLFXP on site %s is not supported', [sdst.site.name]));
    mainpazo.errorreason:='SSLFXP on site '+sdst.site.name+' is not supported';
    readyerror:= True;
    Debug(dpMessage, c_section, '<- '+mainpazo.errorreason+' '+tname);
    exit;
  end;
  if ((ssrc.site.sslfxp = srUnsupported) and (sdst.site.sslfxp = srNeeded)) then
  begin
    Debug(dpSpam, c_section, 'SSLFXP on site %s is not supported', [ssrc.site.name]);
    mainpazo.errorreason:='SSLFXP on site '+ssrc.site.name+' is not supported';
    irc_Adderror(Format('<c4>[ERROR]</c> SSLFXP on site %s is not supported', [ssrc.site.name]));
    readyerror:= True;
    Debug(dpMessage, c_section, '<- '+mainpazo.errorreason+' '+tname);
    exit;
  end;

  if ((ssrc.site.sslfxp = srNeeded) or (sdst.site.sslfxp = srNeeded)) then
  begin
    kellssl:= True;
    if not ssrc.SendProtP() then goto ujra;
    if not sdst.SendProtP() then goto ujra;
  end else
  begin
    kellssl:= False;
    if not ssrc.SendProtC() then goto ujra;
    if not sdst.SendProtC() then goto ujra;
  end;

  if (ssrc.site.sw = sswDrftpd) then
  begin
    if not ssrc.Send('PRET RETR %s', [ssrc.TranslateFilename(filename)]) then goto ujra;
    if not ssrc.Read('PRET RETR') then goto ujra;
  end;

  if (kellssl) then
  begin
    if not ssrc.Send('CPSV') then goto ujra;
  end else begin
    if not ssrc.Send('PASV') then goto ujra;
  end;
  if not ssrc.Read('PASV') then goto ujra;

  lastResponseCode:= ssrc.lastResponseCode;
  lastResponse:= ssrc.lastResponse;

  if lastResponseCode <> 227 then
  begin
    if ((lastResponseCode = 500) and (0 <> Pos('You need to use a client supporting PRET', lastResponse))) then
    begin
      ssrc.site.sw:= sswDrftpd;
      ssrc.site.legacydirlist:=True;
    end;

    if ((kellssl) and (lastResponseCode = 500) and (0 < Pos('understood', lastResponse))) then
      ssrc.site.sslfxp:= srUnsupported;

    readyerror:= True;
    mainpazo.errorreason:='No clue anything about drftpd?';
    Debug(dpSpam, c_section, '<- '+mainpazo.errorreason+' '+tname);
    exit;
  end;

  try
    ParsePasvString(lastResponse, host, port);
  except
    on e: Exception do
    begin
      Debug(dpError, c_section, '[EXCEPTION] Taskrace ParsePasvString: %s', [e.Message]);
      readyerror:= True;
      exit;
    end;
  end;

  if (sdst.site.sw = sswDrftpd) then
  begin
    if not sdst.Send('PRET STOR %s', [sdst.TranslateFilename(storfilename)]) then goto ujra;
    if not sdst.Read('PRET STOR') then goto ujra;
  end;

  if not sdst.Send('PORT %s,%d,%d',[Csere(host,'.',','), port div 256, port mod 256]) then goto ujra;
  if not sdst.Read('PORT') then goto ujra;

  lastResponseCode:= sdst.lastResponseCode;
  lastResponse:= sdst.lastResponse;

  if ((lastResponseCode = 500) and (0 <> Pos('You need to use a client supporting PRET', lastResponse))) then
  begin
    sdst.site.sw:= sswDrftpd;
  end;


  if not sdst.Send('STOR %s', [sdst.TranslateFilename(storfilename)]) then goto ujra;
  if not sdst.Read('STOR') then
  begin
    sdst.Quit;
    goto ujra;
  end;

  lastResponseCode:= sdst.lastResponseCode;
  lastResponse:= sdst.lastResponse;

  if lastResponseCode <> 150 then
  begin
    if (((lastResponseCode = 427) and (0 < Pos('Use SSL FXP', lastResponse))) or
        ((lastResponseCode = 530) and (0 < Pos('USE SECURE DATA CONNECTION', lastResponse))))
    then
    begin
      sdst.site.sslfxp:= srNeeded;
      irc_AddINFO('[iNFO] SSLFXP Need for: '+sdst.Name);
      goto ujra;
    end;

    if (((lastResponseCode = 553) and (0 < Pos('out of disk space', lastResponse))) or
        ((lastResponseCode = 452) and (0 < Pos('No space left on device', lastResponse))) or
        ((lastResponseCode = 450) and (0 < Pos('No transfer-slave(s) available', lastResponse)))) then
    begin
      sdst.site.Setoutofspace;
      if config.ReadBool(c_section, 'mark_site_down_if_out_of_space', True) then
      begin
        sdst.site.markeddown:= True;
        sdst.DestroySocket(True);
      end;

      readyerror:= True;
      mainpazo.errorreason:='No freespace or slave';
      Debug(dpSpam, c_section, '<- '+mainpazo.errorreason+' '+tname);
      exit;
    end;

    if (((lastResponseCode = 533) and (0 < Pos('Multiple SFV files not allowed.', lastResponse)))) then
    begin
      readyerror:= True;
      Debug(dpMessage, c_section, '<- '+lastResponse+' '+tname);
      exit;
    end;

    if (((lastResponseCode = 400) and (0 < Pos('SFVFile still transferring', lastResponse)))) then
    begin
      ps2.ParseDupe(netname, channel, dir, filename, False);
      readyerror:= True;
      Debug(dpMessage, c_section, '<- '+lastResponse+' '+tname);
      exit;
    end;

    if (((lastResponseCode = 533) and (0 < Pos('You must upload sfv first', lastResponse)))) then
    begin
      ps2.ParseDupe(netname, channel, dir, filename, False);
      readyerror:= True;
      Debug(dpMessage, c_section, '<- '+lastResponse+' '+tname);
      exit;
    end;

    if (((lastResponseCode = 553) and (0 < Pos('Max sim UP per dir/sfv reached', lastResponse)))) then
    begin
      readyerror:= True;
      Debug(dpMessage, c_section, '<- '+lastResponse+' '+tname);
      exit;
    end;

    if (((lastResponseCode = 553) and (0 < Pos('Upload denied by pre_check script', lastResponse)))) then
    begin
      readyerror:= True;
      ps2.SetFileError(netname, channel, dir, filename);
      Debug(dpMessage, c_section, '<- '+lastResponse+' '+lastResponse+' '+tname);
      exit;
    end;

    if (((lastResponseCode = 533) and (0 < Pos('does not exist in the sfv', lastResponse)))) then
    begin
      ps2.ParseDupe(netname, channel, dir, filename, False);
      readyerror:= True;
      Debug(dpMessage, c_section, '<- '+lastResponse+' '+tname);
      exit;
    end;

    if (((lastResponseCode = 533) and (0 < Pos('File not found in sfv', lastResponse)))) then
    begin
      ps2.ParseDupe(netname, channel, dir, filename, False);
      readyerror:= True;
      Debug(dpMessage, c_section, '<- '+lastResponse+' '+tname);
      exit;
    end;

    if (((lastResponseCode = 425) and (0 < Pos('Connection refused', lastResponse)))) then
    begin
      irc_Adderror(Format('<c4>[REFUSED]</c> %s : %d %s', [tname, lastResponseCode, AnsiLeftStr(lastResponse, 60)]));
      ssrc.Quit;
      sdst.Quit;
      goto ujra;
    end;

    if (((lastResponseCode = 550) and (0 < Pos('No such directory', lastResponse)))) then
    begin
      irc_Adderror(Format('<c4>[ERROR]</c> %s %s', [tname, lastResponse]));
      if (dir = '') then
      begin
        ps2.MarkSiteAsFailed(True);
      end;
      readyerror:= True;
      Debug(dpMessage, c_section, '<- '+lastResponse+' '+tname);
      exit;
    end;

    if ((lastResponseCode = 550) or (lastResponseCode = 500)) then
    begin
      ps2.ParseDupe(netname, channel, dir, filename, False);
      ready:= True;
      Result:= True;
      Debug(dpMessage, c_section, '<-- DUPE '+tname);
      exit;
    end
    else
    if (lastResponseCode = 553) then
    begin
      ps2.ParseXdupe(netname, channel, dir, lastResponse, ps2.ParseDupe(netname, channel, dir, filename, False));
      ready:= True;
      Result:= True;
      Debug(dpMessage, c_section, '<-- DUPE '+tname);
      exit;
    end
    else
    begin
      Debug(dpMessage, c_section, '-- '+tname+Format(' : %d %s', [lastResponseCode, AnsiLeftStr(lastResponse, 200)]));
      irc_Adderror(Format('<c4>[ERROR]</c> unknown error %s after STOR (%s) : %d %s', [sdst.site.name, tname, lastResponseCode, AnsiLeftStr(lastResponse, 60)]));
      sdst.DestroySocket(False);
      mainpazo.errorreason:=Format('unknown error %s after STOR (%s) : %d %s', [sdst.site.name, tname, lastResponseCode, AnsiLeftStr(lastResponse, 60)]);
      readyerror:= True;
    end;
    Debug(dpMessage, c_section, '<- '+tname);
    exit;
  end;


retrujra:

  if not ssrc.Send('RETR %s', [ssrc.TranslateFilename(filename)]) then goto ujra;
  if not ssrc.Read('RETR') then
  begin
    // breastfed, the dst to run because it works at all. closes the login will fuck up again.
    sdst.Quit;
    goto ujra;
  end;

  lastResponseCode:= ssrc.lastResponseCode;
  lastResponse:= ssrc.lastResponse;

  elotte:= Now;

  if lastResponseCode <> 150 then
  begin
    if ((lastResponseCode = 550) and (0 < Pos('credit', LowerCase(lastResponse)))) then
      ssrc.site.SetKredits
    else
    if (((lastResponseCode = 427) and (0 < Pos('Use SSL FXP', lastResponse))) or
        ((lastResponseCode = 530) and (0 < Pos('USE SECURE DATA CONNECTION', lastResponse))))
    then
    begin
      ssrc.site.sslfxp:= srNeeded;
      irc_AddINFO('[iNFO] SSLFXP Need for: '+ssrc.Name);
      goto ujra;
    end
    else
    if ((lastResponseCode = 550) and (0 < Pos('Taglines Enforced', lastResponse))) then
    begin
      if not ssrc.Send('SITE TAGLINE %s', ['slftp.4tw']) then goto ujra;
      if not ssrc.Read('SITE TAGLINE') then goto ujra;
      goto retrujra;
    end
    else
    if (
           ((lastResponseCode = 550) and (0 < Pos('No such file or directory', lastResponse)))
        or ((lastResponseCode = 426) and (0 < Pos('File has been deleted on the master', lastResponse)))
        or ((lastResponseCode = 426) and (0 < Pos('is being deleted', lastResponse)))
        or ((lastResponseCode = 426) and (0 < Pos('found in any root', lastResponse)))
        or ((lastResponseCode = 426) and (0 < Pos('Transfer was aborted', lastResponse)))
        or ((lastResponseCode = 426) and (0 < Pos('Slave is offline', lastResponse)))
        or ((lastResponseCode = 550) and (0 < Pos('Unable to load your own user file', lastResponse)))
        or ((lastResponseCode = 550) and (0 < Pos('File not found', lastResponse)))
        or ((lastResponseCode = 550) and (0 < Pos('File unavailable', lastResponse)))
        ) then
    begin
      if spamcfg.readbool(c_section,'No_such_file_or_directory',True) then
        irc_Adderror(ssrc.todotask, '<c4>[ERROR No Such File]</c> TPazoRaceTask %s', [tname]);
    end
    else
    if (
       ((lastResponseCode = 425) and (0 < Pos('t open data connection', lastResponse))) or
       ((lastResponseCode = 426) and (0 < Pos('Read timed out', lastResponse)))
       ) then
    begin
      if spamcfg.readbool(c_section,'cant_open_data_connection',True) then
        irc_Adderror(ssrc.todotask, '<c4>[ERROR Cant open]</c> TPazoRaceTask %s', [tname]);
    end
    else
    if ((lastResponseCode = 553) and (0 < Pos('You have reached your maximum simultaneous downloads allowed', lastResponse))) then
    begin
      if spamcfg.readbool(c_section,'reached_max_sim_down',True) then
        irc_Adderror(ssrc.todotask, '<c4>[ERROR] Maxsim down</c> %s', [tname]);
    end
    else
    if ((lastResponseCode = 550) and (0 < Pos('Permission denied', lastResponse))) then
    begin
      if spamcfg.readbool(c_section,'permission_denied',True) then
        irc_Adderror(ssrc.todotask, '<c4>[ERROR] Permission denied</c> %s', [tname]);
    end
    else
    begin
      if spamcfg.readbool(c_section,'reached_max_sim_down',True) then
        irc_Adderror(ssrc.todotask, '<c4>[ERROR] unknown after RETR</c> %s : %d %s', [tname, lastResponseCode, AnsiLeftStr(lastResponse, 60)]);
    end;
    // ilyenkor a dst szalon a legjobb ha lezarjuk a geci a socketet mert az ABOR meg a sok szar amugy sem hasznalhato.
    // es majd ugyis automatan ujrabejelentkezik a cumo
    sdst.DestroySocket(False);

    mainpazo.errorreason:='No free slots?';
    readyerror:= True;
    Debug(dpSpam, c_section, '<- '+mainpazo.errorreason+' '+tname);
    exit;
  end;

  rss:= False;
  rsd:= False;
  while (true) do
  begin
    if not rsd then
      rsd:= sdst.Read('WAIT', False, True, 100);
    if ((sdst.error <> '') and (sdst.error <> 'timeout')) then
    begin
      ssrc.DestroySocket(False);
      mainpazo.errorreason:='sdst WAIT';
      readyerror:= True;
      Debug(dpSpam, c_section, '<- '+mainpazo.errorreason+' '+tname);
      exit;
    end;

    if not rss then
      rss:= ssrc.Read('WAIT', False, True, 100);
    if ((ssrc.error <> '') and (ssrc.error <> 'timeout')) then
    begin
      sdst.DestroySocket(False);
      mainpazo.errorreason:='ssrc WAIT';
      readyerror:= True;
      Debug(dpSpam, c_section, '<- '+mainpazo.errorreason+' '+tname);
      exit;
    end;

    if ((rsd) and (rss)) then Break;
    
    if (SecondsBetween(Now, elotte) > 600) then
    Begin
      Debug(dpError, c_section, Format('[iNFO] Long race break: %s %s %s', [name, ssrc.lastResponse, sdst.lastResponse]));
      ssrc.DestroySocket(False);
      sdst.DestroySocket(False);
      mainpazo.errorreason:='Long race break';
      readyerror:= True;
      exit;
    End;
  end;
  debug(dpSpam, c_section, 'File transfer ready %s->%s %s',[site1, site2, filename]);

  utana:= Now;
  time_race:= MilliSecondsBetween(utana, elotte);
  response:= IntToStr(time_race);

  if (ssrc.lastResponseCode <> 226) then
    if spamcfg.readbool(c_section,'turn_on_sslfxp',True) then
      irc_Adderror(ssrc.todotask, '<c4>[ERROR SSLFXP]</c> TPazoRaceTask %s: %s %d %s', [ssrc.name, tname, ssrc.lastResponseCode, AnsiLeftStr(ssrc.lastResponse, 60)]);

  if (sdst.lastResponseCode <> 226) then
  begin
    if spamcfg.readbool(c_section,'turn_on_sslfxp',True) then
      irc_Adderror(ssrc.todotask, '<c4>[ERROR FXP]</c> TPazoRaceTask %s, %s %d %s', [sdst.name, tname, sdst.lastResponseCode, AnsiLeftStr(sdst.lastResponse, 60)]);
  end;

  byme:= False;
  if (ssrc.lastResponseCode = 226) and (sdst.lastResponseCode = 226) then
    byme:= True;

    // ez egy nagyon elbaszott eset. ilyenkor megprobaljuk majd ujra.
  if (
       (mainpazo.rls <> nil)
       and
       (byme)
       and
       (
       (0 < Pos('CRC-Check: SFV first', sdst.lastResponse))
       or
       (0 < Pos('CRC-Check: BAD!', sdst.lastResponse))
       or
       (0 < Pos('CRC-Check: Not in sfv!', sdst.lastResponse))
       or
       (0 < Pos('0byte-file: Not allowed', sdst.lastResponse))
       )
     ) then
  begin
brokentransfer:
    Debug(dpSpam, c_section, 'Broken transfer event!');
    DontRemoveOtherSources:= True;
    if (0 < Pos('CRC-Check: SFV first', sdst.lastResponse)) then
      DontRemoveOtherSources:= False;

    if 0 < Pos('CRC-Check: BAD!', sdst.lastResponse) then
    begin
      if spamcfg.readbool(c_section,'crc_error',True) then
        irc_Adderror(ssrc.todotask, '<c4>[ERROR CRC]</c> %s: %d/%d', [Name, ps1.badcrcevents, config.ReadInteger(c_section, 'badcrcevents', 15)]);
      inc(ps1.badcrcevents);
    end;

    if 0 < Pos('CRC-Check: Not in sfv!', sdst.lastResponse) then
    begin
      if spamcfg.readbool(c_section,'crc_error',True) then
        irc_Adderror(ssrc.todotask, '<c4>[ERROR CRC]</c> %s: %d/%d', [Name, ps1.badcrcevents, config.ReadInteger(c_section, 'badcrcevents', 15)]);
      inc(ps1.badcrcevents);
    end;
    ready:= True;
    Result:= True;
    Debug(dpSpam, c_section, '<-- Broken? '+sdst.lastResponse+''+tname);
    Exit;
  end;

  if mainpazo.rls = nil then
  begin
    Debug(dpMessage, c_section, '<- '+tname);
    Result:= True;
    ready:= True;
    exit;
  end;

  ps2.ParseDupe(netname, channel, dir, filename, byme); // ezt regen readyracenek hivtuk, de ossze lett vonva parsedupe-pal

  if (byme and (time_race > 0)) then
  begin
    try
      fs:= mainpazo.PFileSize(dir, filename);
      if (fs > config.ReadInteger('speedstats', 'min_filesize', 5000000)) then
      begin
        SpeedStatAdd(site1, site2, fs * 1000 / time_race, mainpazo.rls.section, mainpazo.rls.rlsname);
      end;
    except
      on E: Exception do
      begin
        Debug(dpError, c_section, '[EXCEPTION] mainpazo.PFileSize/SpeedStatAdd: %s', [e.Message]);
      end;
    end;
  end;

  // echo race info
  try
    rrgx:=TRegExpr.Create;
    rrgx.ModifierI:=True;
    rrgx.Expression:=config.ReadString('dirlist', 'useful_skip', '\.nfo|\.sfv|\.m3u|\.cue|\.jpg|\.jpeg|\.gif|\.png|\.avi|\.mkv|\.vob|\.mp4|\.wmv');
    if not rrgx.Exec(filename) then
    begin
      speed_stat:= '';
      fs:= mainpazo.PFileSize(dir, filename);
      if (fs > 0) and (time_race > 0) then
      begin
        if filesize > 1024 then
          speed_stat:= Format('<b>%f</b>mB @ <b>%f</b>mB/s', [fs / 1024 / 1024, fs * 1000 / time_race / 1024 / 1024])
        else
          speed_stat:= Format('<b>%f</b>kB @ <b>%f</b>kB/s', [fs / 1024, fs * 1000 / time_race / 1024]);
      end;
      irc_SendRACESTATS(tname+' '+speed_stat);
      //irc_addtext('CONSOLE', 'Trades', tname);

      // add stats
      statsProcessRace(site1, site2, mainpazo.rls.section, mainpazo.rls.rlsname, filename, IntToStr(filesize));
    end;
    rrgx.Free;
  except
    on e: Exception do
    begin
      Debug(dpError, c_section, Format('[EXCEPTION] Exception in echo: %s', [e.Message]));
    end;
  end;

  Debug(dpMessage, c_section, '<-- '+tname);

  Result:= True;
  ready:= True;
end;

function TPazoRaceTask.Name: string;
begin
  try
    if mainpazo.rls = nil then
      Result:= Format('RACE %d <b>%s</b>-><b>%s</b>: %s (%d)',[pazo_id, site1, site2, filename, rank])
    else
      Result:= Format('RACE %d <b>%s</b>-><b>%s</b>: %s %s (%d)',[pazo_id, site1, site2, mainpazo.rls.rlsname, filename, rank]);
  except
    Result:= 'RACE';
  end;
end;

{ TWaitTask }

constructor TWaitTask.Create(const netname, channel: string;
  site1: string);
begin
  inherited Create(netname, channel, site1);
  event:= TEvent.Create(nil, False, False, '');
end;

destructor TWaitTask.Destroy;
begin
  event.Free;
  inherited;
end;

function TWaitTask.Execute(slot: Pointer): Boolean;
begin
  Result:= True;
  event.WaitFor($FFFFFFFF);
  (*
  case event.WaitFor(15 * 60 * 1000) of
    wrSignaled : { Event fired. Normal exit. }
    begin

    end;
    else { Timeout reach }
    begin
      irc_Adderror('TWaitTask.Execute: <c2>Force Leave</c>:'+Name+' TWaitTask 15min');
      Debug(dpSpam, c_section,'TWaitTask.Execute: <c2>Force Leave</c>:'+Name+' TWaitTask 15min');
    end;
  end;
  *)
  ready:= True;
end;

function TWaitTask.Name: string;
begin
  try
    Result:= 'WAITTASK :'+wait_for;
  except
    Result:= 'WAITTASK';
  end;
end;

end.
