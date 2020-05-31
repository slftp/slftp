{
  @abstract(Knowledge base functions)
}
unit kb;

interface

uses
  Classes, SyncObjs, IniFiles, kb.release;

type
  TKBThread = class(TThread)
  private
    kbevent: TEvent;
    function AddCompleteTransfers(pazo: Pointer): boolean;
  public
    constructor Create;
    procedure Execute; override;
    destructor Destroy; override;
  end;

function renameCheck(const pattern, i, len: integer; const rls: String): boolean;
function kb_Add(const netname, channel, sitename, section, genre: String; event: TKBEventType; const rls, cdno: String;
  dontFire: boolean = False; forceFire: boolean = False; ts: TDateTime = 0): integer;
function FindReleaseInKbList(const rls: String): String;

function FindSectionHandler(const section: String): TCRelease;

procedure kb_FreeList;
procedure kb_Save;
procedure KB_start;
procedure kb_Init;
procedure kb_Uninit;
procedure kb_Stop;

function kb_reloadsections: boolean;

var
  kb_sections: TStringList;
  mp3genres: TStringList;
  kb_list: TStringList;
  kb_thread: TKBThread;
  kb_last_saved: TDateTime;
  kb_lock: TCriticalSection;
  imdbcountries: TIniFile;
  kbevent: TEvent;

implementation

uses
  debugunit, mainthread, taskgenrenfo, taskgenredirlist, configunit, console,
  taskrace, sitesunit, queueunit, pazo, irc, SysUtils, fake, mystrings,
  rulesunit, Math, DateUtils, StrUtils, precatcher, tasktvinfolookup, encinifile,
  slvision, tasksitenfo, RegExpr, taskpretime, taskgame, mygrouphelpers,
  sllanguagebase, taskmvidunit, dbaddpre, dbaddimdb, dbtvinfo, irccolorunit,
  mrdohutils, ranksunit, tasklogin, dbaddnfo, contnrs, slmasks, dirlist,
  globalskipunit, irccommandsunit, Generics.Collections {$IFDEF MSWINDOWS}, Windows{$ENDIF};

const
  rsections = 'kb';

var
  addpreechocmd: String;

  // TODO: Using THashedStringList does fuckup cleaning because it does not have a constant index which is used to delete oldest (latest) entries
  // but it's much faster and as we use it very often it's worth it...but maybe there is a better solution
  kb_trimmed_rls: THashedStringList;
  kb_groupcheck_rls: THashedStringList;
  kb_latest: THashedStringList; //< holds release and section as rls=section
  kb_skip: THashedStringList;

  // Config vars
  trimmed_shit_checker: boolean;
  renamed_group_checker: boolean;
  renamed_release_checker: boolean;

  enable_try_to_complete: boolean;
  try_to_complete_after: integer;
  kb_save_entries: integer;

  rename_patterns: integer;
  taskpretime_mode: integer;

function FindSectionHandler(const section: String): TCRelease;
var
  i: integer;
begin
  Result := sectionhandlers[0];

  for i := 1 to High(sectionhandlers) do
  begin
    if sectionhandlers[i].SectionAccepted(section) then
    begin
      Result := sectionhandlers[i];
      exit;
    end;
  end;
end;

function renameCheck(const pattern, i, len: integer; const rls: String): boolean;
var
  ss: String;
begin
  Result := False;

  // increase rename_patterns in kb_init by 1 everytime a new pattern emerges

  ss := kb_latest.Names[i];
  if pattern = 0 then
  begin
    // Original: Point_Blank-X_History-2012-C4
    // Rename:   Pnt_t_Blank-X_History-2012-C4
    Delete(ss, 2, 2);
    Insert(Copy(ss, 3, 2), ss, 5);
  end
  else if pattern = 1 then
  begin
    // Original: VA-Soundwave_2013-2CD-2012-MTD
    // Rename:   V-Soundwave_20013-2CD-2012-MTD
    Delete(ss, 2, 1);
    Insert(Copy(ss, 14, 1), ss, 14);
  end
  else if pattern = 2 then
  begin
    // Original: VA-Soundwave_2013-2CD-2012-MTD
    // Rename:   VA-Soudwave_20013-2CD-2012-MTD
    Delete(ss, 7, 1);
    Insert(Copy(ss, 14, 1), ss, 14);
  end
  else if pattern = 3 then
  begin
    // Original: Teleport.Pro.v1.68.Incl.Keygen-BRD
    // Rename:   Teleport.Pro.v1.68.Incl.Keynen-BRD
    Delete(ss, len - 6, 1);
    Insert(Copy(ss, len - 5, 1), ss, len - 6);
  end
  else
    ss := '';

  if AnsiCompareText(ss, rls) = 0 then
    Result := True;
end;

function trimmedShitChecker(section, rls: String): boolean;
begin
  Result := False;
end;

function kb_AddB(const netname, channel, sitename, section, genre: String; event: TKBEventType; rls, cdno: String; dontFire: boolean = False; forceFire: boolean = False; ts: TDateTime = 0): integer;
var
  i, j, len: integer;
  r: TRelease;
  rc: TCRelease;
  s: TSite;
  ss: String;
  added: boolean;
  p: TPazo;
  ps, psource: TPazoSite;
  rule_result: TRuleAction;
  rlz, grp: String;
  dlt: TPazoDirlistTask;
  l: TLoginTask;

  { Removes the oldest knowledge base entries }
  procedure KbListsCleanUp;
  begin
    try
      i := kb_trimmed_rls.Count - 1;
      if i > 200 then
      begin
        while i > 150 do
        begin
          kb_trimmed_rls.Delete(0);
          i := kb_trimmed_rls.Count - 1;
        end;
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, rsections, '[EXCEPTION] kb_AddB clean kb_trimmed_rls : %s', [e.Message]);
      end;
    end;

    try
      i := kb_groupcheck_rls.Count - 1;
      if i > 200 then
      begin
        while i > 150 do
        begin
          kb_groupcheck_rls.Delete(0);
          i := kb_groupcheck_rls.Count - 1;
        end;
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, rsections, '[EXCEPTION] kb_AddB clean kb_groupcheck_rls : %s', [e.Message]);
      end;
    end;

    try
      i := kb_latest.Count - 1;
      if i > 200 then
      begin
        while i > 150 do
        begin
          kb_latest.Delete(i);
          i := kb_latest.Count - 1;
        end;
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, rsections, '[EXCEPTION] kb_AddB clean kb_latest : %s', [e.Message]);
      end;
    end;

    try
      i := kb_skip.Count - 1;
      if i > 300 then
      begin
        while i > 250 do
        begin
          kb_skip.Delete(i);
          i := kb_skip.Count - 1;
        end;
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, rsections, '[EXCEPTION] kb_AddB clean kb_skip : %s', [e.Message]);
      end;
    end;
  end;

begin
  debug(dpSpam, rsections, '--> %s %s %s %s %s %d %d', [sitename, section, KBEventTypeToString(event), rls, cdno, integer(dontFire), integer(forceFire)]);

  Result := -1;

  kb_lock.Enter;
  psource := nil;
  try
    // deny adding of a release twice with different section
    if (section <> '') then
    begin
      i := kb_latest.IndexOfName(rls);
      if i <> -1 then
      begin
        ss := kb_latest.ValueFromIndex[i];
        if (not ss.StartsWith('PRE') and (ss <> section)) then
        begin
          if spamcfg.readbool(rsections, 'already_in_another_section', True) then
            irc_addadmin(Format('<b><c4>%s</c> @ %s </b>was caught as section %s but is already in KB with section %s', [rls, sitename, section, ss]));
          exit;
        end;
      end
    end;

    // check if rls already skiped
    if kb_skip.IndexOf(rls) <> -1 then
    begin
      if spamcfg.readbool(rsections, 'skipped_release', True) then
        irc_addadmin(format('<b><c4>%s</c> @ %s </b>is in skipped releases list!', [rls, sitename]));
      exit;
    end;

    if trimmed_shit_checker then
    begin
      try
        i := kb_trimmed_rls.IndexOf(section + '-' + rls);
        if i <> -1 then
        begin
          irc_addadmin(Format('<b><c4>%s</c> @ %s is trimmed shit!</b>', [rls, sitename]));
          kb_skip.Insert(0, rls);
          exit;
        end;

        kb_trimmed_rls.Add(section + '-' + Copy(rls, 1, Length(rls) - 1));
        kb_trimmed_rls.Add(section + '-' + Copy(rls, 2, Length(rls) - 1));
      except
        on e: Exception do
        begin
          Debug(dpError, rsections, '[EXCEPTION] kb_AddB trimmed_shit_checker : %s', [e.Message]);
        end;
      end;
    end;

    if renamed_group_checker then
    begin
      try
        grp := GetGroupname(rls);
        rlz := RemoveGroupname(rls);
        ss := kb_groupcheck_rls.Values[rlz];
        if ss = '' then
          kb_groupcheck_rls.Values[rlz] := grp
        else
        begin
          if uppercase(grp) <> uppercase(ss) then
          begin
            if spamcfg.readbool(rsections, 'renamed_group', True) then
              irc_addadmin(format('<b><c4>%s</c> @ %s </b>is renamed group shit! %s vs. %s', [rls, sitename, grp, ss]));
            kb_skip.Insert(0, rls);
            exit;
          end;
          if grp <> ss then
          begin
            if spamcfg.readbool(rsections, 'renamed_group', True) then
              irc_addadmin(format('<b><c4>%s</c> @ %s </b>is changed case group shit! %s vs. %s', [rls, sitename, grp, ss]));
            kb_skip.Insert(0, rls);
            exit;
          end;
        end;
      except
        on e: Exception do
        begin
          Debug(dpError, rsections, '[EXCEPTION] kb_AddB renamed_group_checker : %s', [e.Message]);
        end;
      end;
    end;

    // don't even enter the checking code if the release is already in kb_latest, because then we already handled it and it's clean
    // because kb_skip would've prevented kb_addb being called from kb_add
    if (kb_latest.IndexOfName(rls) = -1) then
    begin
      if (renamed_release_checker) then
      begin
        try
          len := Length(rls); // no need to check the release length in every loop
          for i := 0 to kb_latest.Count - 1 do
          begin
            // makes no sense to run this "expensive" operation if both strings aren't equal length
            // since the current pattern shows only strings of equal length being renames of one another
            if Length(kb_latest.Names[i]) <> len then
              Continue;
            if AnsiCompareText(kb_latest.Names[i], rls) <> 0 then
            begin
              // loop through the amount of different patterns, reduces code duplication
              for j := 0 to rename_patterns - 1 do
              begin
                if renameCheck(j, i, len, rls) then
                begin
                  if spamcfg.readbool(rsections, 'renamed_release', True) then
                    irc_addadmin(format('<b><c4>%s</c> @ %s </b>is a rename of %s!', [rls, sitename, kb_latest.Names[i]]));

                  // release is brand-new but a rename of an already existing release
                  kb_latest.Insert(0, rls + '=' + section);
                  // gonna insert this anyway, because there are sometimes renames of renames
                  kb_skip.Insert(0, rls);
                  exit;
                end;
              end;
            end;
          end;
        except
          on e: Exception do
          begin
            Debug(dpError, rsections, '[EXCEPTION] kb_AddB renamed_release_checker : %s', [e.Message]);
          end;
        end;
      end;

      // release is fine and brand-new, add it to kb_latest
      kb_latest.Insert(0, rls + '=' + section);
    end;

    // Start cleanup lists
    KbListsCleanUp; // TODO: maybe run it only every 60mins? not needed to run it every time...

  finally
    kb_lock.Leave;
  end;
  //  i := -1;
  //  added := False;

  kb_lock.Enter;
  try
    i := kb_list.IndexOf(section + '-' + rls);
    if i = -1 then
    begin
      if (event = kbeNUKE) then
      begin
        // nuking an old rls not in kb
        irc_Addstats(Format('<c4>[NUKE]</c> %s %s @ %s (not in kb)',
          [section, rls, '<b>' + sitename + '</b>']));
        exit;
      end;

      if (event = kbeCOMPLETE) then
      begin
        // complet an old rls not in kb
        irc_Addstats(Format('<c7>[COMPLETE]</c> %s %s @ %s (not in kb)',
          [section, rls, '<b>' + sitename + '</b>']));
        exit;
      end;

      debug(dpSpam, rsections,
        'This NEWDIR [event: %s] task for %s (%s) was the first one to hit kb - checking eljut etc',
        [KBEventTypeToString(event), rls, section]);

      // uj joveveny!
      rc := FindSectionHandler(section);
      if (event = kbePRE) then
      begin
        // no fakecheck needed, it's a pre from one of our sites
        r := rc.Create(rls, section, False, DateTimeToUnix(Now(), False));
        irc_SendAddPre(format('%s %s %s', [addpreechocmd, rls, section]));
        if TPretimeLookupMOde(taskpretime_mode) = plmSQLITE then
        begin
          try
            dbaddpre_InsertRlz(rls, section, 'SITE-' + sitename);
          except
            on e: Exception do
            begin
              Debug(dpError, rsections, 'dbaddpre_InsertRlz error : %s', [e.Message]);
            end;
          end;
        end;
      end
      else if (event = kbeSPREAD) then
      begin
        r := rc.Create(rls, section, False, DateTimeToUnix(Now(), False));
      end
      else
      begin
        r := rc.Create(rls, section);
      end;

      r.kb_event := event;

      if genre <> '' then
      begin
        try
          r.Aktualizald(genre);
        except
          on e: Exception do
          begin
            Debug(dpError, rsections, 'r.Aktualizald(genre) : %s', [e.Message]);
          end;
        end;
      end;

      p := PazoAdd(r);

      // need to search all sites where there is such a section ...
      added := p.AddSites;

      kb_list.BeginUpdate;
      try
        kb_list.AddObject(section + '-' + rls, p);
      finally
        kb_list.EndUpdate;
      end;

      if added then
      begin
        // sorting
        RulesOrder(p);
      end;

      // announce event on admin chan
      if (event = kbeADDPRE) then
      begin
        if spamcfg.ReadBool('kb', 'new_rls', True) then
          irc_Addstats(Format('<c3>[ADDPRE]</c> %s %s', [section, rls]));
      end
      else if (event = kbePRE) then
      begin
        if spamcfg.ReadBool('kb', 'pre_rls', True) then
          irc_Addstats(Format('<c9>[<b>PRE</b>]</c> <b>%s</b> <b>%s</b> @ <b>%s</b>', [section, rls, sitename]));
      end
      else if (event = kbeSPREAD) then
      begin
        if spamcfg.ReadBool('kb', 'spread_rls', True) then
          irc_Addstats(Format('<c9>[<b>SPREAD</b>]</c> <b>%s</b> <b>%s</b> @ <b>%s</b>', [section, rls, sitename]));
      end
      else
      begin
        if (r.pretime = 0) then
        begin
          if TPretimeLookupMOde(taskpretime_mode) = plmNone then
          begin
            if spamcfg.ReadBool('kb', 'new_rls', True) then
              irc_Addstats(Format('<c7>[<b>NEW</b>]</c> %s %s @ <b>%s</b>', [section, rls, sitename]));
          end
          else
          begin
            if spamcfg.ReadBool('kb', 'new_rls', True) then
              irc_Addstats(Format('<c7>[<b>NEW</b>]</c> %s %s @ <b>%s</b> (<c7><b>Not found in PreDB</b></c>)', [section, rls, sitename]));
          end;
        end
        else
        begin
          if spamcfg.ReadBool('kb', 'new_rls', True) then
            irc_Addstats(Format('<c3>[<b>NEW</b>]</c> %s %s @ <b>%s</b> (<b>%s</b>) (<c3><b>%s ago</b></c>) (%s)', [section, rls, sitename, p.sl.sectionname, dbaddpre_GetPreduration(r.pretime), r.pretimefrom]));
        end;
      end;
    end
    else
    begin
      if (event = kbePRE) then
      begin
        if spamcfg.ReadBool('kb', 'pre_rls', True) then
          irc_Addstats(Format('<c9>[<b>PRE</b>]</c> <b>%s</b> <b>%s</b> @ <b>%s</b>', [section, rls, sitename]));
      end;

      // meg kell tudni mi valtozott //you need to know what's changed
      p := TPazo(kb_list.Objects[i]);
      r := p.rls;

      debug(dpSpam, rsections,
        'This NEWDIR [event: %s] task was not the first one to hit kb as kb_list already contained an entry for %s in %s',
        [KBEventTypeToString(event), rls, section]);

      if r.rlsname <> rls then
      begin
        irc_addadmin(Format('<b><c4>%s</c> @ %s changed case!</b>!!', [rls,
          sitename]));
        exit;
      end;

      if genre <> '' then
      begin
        try
          p.rls.Aktualizald(genre);
        except
          on e: Exception do
          begin
            Debug(dpError, rsections, 'p.rls.Aktualizald(genre) : %s',
              [e.Message]);
          end;
        end;
      end;

      if (event <> kbeSPREAD) and (TPretimeLookupMOde(taskpretime_mode) <> plmNone) then
      begin
        if (r.pretime = 0) then
        begin
          r.SetPretime;
          if (r.pretime <> 0) then
          begin
            if spamcfg.ReadBool('kb', 'updated_rls', True) then
              irc_SendUPDATE(Format('<c3>[UPDATE]</c> %s %s @ <b>%s</b> now has pretime (<c3><b>%s ago</b></c>) (%s)', [section, rls, sitename, dbaddpre_GetPreduration(r.pretime), r.pretimefrom]));
            added := p.AddSites;
            if added then
            begin
              // sorrendezes
              RulesOrder(p);
            end;
          end;
        end;
      end;
    end;
  finally
    kb_lock.Leave;
  end;

  Result := p.pazo_id;
  if p.PazoSitesList.Count = 0 then
    exit;

  if ((event <> kbeSPREAD) and (CheckIfGlobalSkippedGroup(rls))) then
  begin
    irc_addadmin(format('<b><c4>%s</c> @ %s </b>is a global skipped group!', [grp, rls]));
    debug(dpSpam, rsections, 'Group %s pred %s in %s but it is a global skipped group', [grp, rls, section]);
    exit;
  end;

  if (event <> kbeADDPRE) then
  begin
    psource := p.FindSite(sitename);
    if psource = nil then
    begin
      s := FindSiteByName(netname, sitename);

      // site not found in pazo but we got an event ...
      if spamcfg.ReadBool('kb', 'dont_match_rls', True) then
      begin
        if (event = kbeNUKE) then
          exit;

        if (s = nil) then
        begin
          irc_Addstats(Format('<c4>[SITE NOT FOUND]</c> : %s %s', [netname, sitename]));
          exit;
        end;

        if (s.WorkingStatus in [sstMarkedAsDownByUser]) then
        begin
          irc_Addstats(Format('<c4>[SITE DOWN]</c> : %s %s @ <b>%s</b>', [section, rls, sitename]));
          exit;
        end;

        if (TPretimeLookupMode(taskpretime_mode) <> plmNone) then
        begin
          if (r.pretime = 0) then
          begin
            irc_Addstats(Format('<c7>[NO PRETIME]</c> :  %s %s @ <b>%s</b>', [section, rls, sitename]));
            exit;
          end;

          if (not s.IsPretimeOk(p.rls.section, p.rls.pretime)) then
          begin
            irc_Addstats(Format('<c5>[BACKFILL]</c> : %s %s @ <b>%s</b>', [section, rls, sitename]));
            exit;
          end;
        end;

        if ((sitename <> getAdminSiteName) and (not s.PermDown) and (s.WorkingStatus in [sstUnknown, sstUp])) then
        begin
          irc_Addstats(Format('<c5>[SECTION NOT SET]</c> : %s %s @ %s (%s)', [p.rls.section, p.rls.rlsname, sitename, KBEventTypeToString(event)]));
        end;
      end;

      // races/kb_adds are happening - site must be up again
      if ((s <> nil) and (not s.PermDown) and (s.WorkingStatus in [sstDown, sstTempDown]) and (event in [kbeCOMPLETE, kbePRE, kbeSPREAD])) then
      begin
        try
          l := TLoginTask.Create(netname, channel, sitename, False, False);
          l.noannounce := True;
          AddTask(l);
        except
          on E: Exception do
            Debug(dpError, rsections, '[EXCEPTION] COMPLETE|PRE|SPREAD LoginTask : %s', [e.Message]);
        end;
      end;

      exit;
    end;

    s := FindSiteByName(netname, psource.Name);
    if ((s <> nil) and (not (s.WorkingStatus in [sstUnknown, sstUp]))) then
      exit;

    psource.ircevent := True;

    if psource.ts < ts then
    begin
      psource.ts := ts;
    end;

    if (event = kbePRE) then
    begin
      if (s <> nil) then
      begin
        if ((not s.IsAffil(r.groupname)) and (config.ReadBool(rsections, 'auto_add_affils', True))) then
          s.AddAffil(r.groupname);
      end;
      r.PredOnAnySite := True;
      psource.Status := rssRealPre;
    end
    else if (event = kbeSPREAD) then
    begin
      r.PredOnAnySite := True;
      psource.Status := rssRealPre;
    end
    else if ((event = kbeCOMPLETE) and (not psource.StatusRealPreOrShouldPre)) then
    begin
      psource.dirlist.SetCompleteInfo(FromIrc);
      psource.SetComplete(cdno);
    end;

    if (event = kbeNUKE) then
    begin
      psource.Status := rssNuked;
      irc_Addstats(Format('<c4>[NUKE]</c> %s %s @ <b>%s</b>',
        [section, rls, sitename]));
      try
        RemovePazoMKDIR(p.pazo_id, psource.Name, rls);
        RemoveRaceTasks(p.pazo_id, psource.Name);
        RemoveDirlistTasks(p.pazo_id, psource.Name);
        psource.dirlistgaveup := True;
      except
        on e: Exception do
        begin
          Debug(dpError, rsections,
            Format('[EXCEPTION] KBAdd RemovePazo on NUKE : %s',
            [e.Message]));
        end;
      end;
    end;
  end;

  if not p.rls.aktualizalva then
  begin
    p.rls.Aktualizal(p);
  end;

  // implement firerules, routes, stb. set rs.srcsite:= rss.sitename;
  if (not (event in [kbeNUKE, kbeADDPRE])) then
  begin
    kb_lock.Enter;
    try
      rule_result := raDrop;
      rule_result := FireRuleSet(p, psource);
    finally
      kb_lock.Leave;
    end;

    // announce SKIP and DONT MATCH only if the site is not a PRE site
    if (psource.status <> rssRealPre) then
    begin
      if (rule_result = raDrop) and (spamcfg.ReadBool('kb', 'skip_rls', True)) then
      begin
        irc_Addstats(Format('<c5>[SKIP]</c> : %s %s @ %s "%s" (%s)',
          [p.rls.section, p.rls.rlsname, psource.Name, psource.reason, KBEventTypeToString(event)]));
      end
      else if (rule_result = raDontmatch) and (spamcfg.ReadBool('kb', 'dont_match_rls', True)) then
      begin
        irc_Addstats(Format('<c5>[DONT MATCH]</c> : %s %s @ %s "%s" (%s)',
          [p.rls.section, p.rls.rlsname, psource.Name, psource.reason, KBEventTypeToString(event)]));
      end;
    end;
  end;

  try
    // check rules for site only if needed
    for i := p.PazoSitesList.Count - 1 downto 0 do
    begin
      try
        if i < 0 then
          Break;
      except
        Break;
      end;
      ps := TPazoSite(p.PazoSitesList[i]);
      kb_lock.Enter;
      try
        if (ps.status in [rssNotAllowed, rssNotAllowedButItsThere]) then
        begin
          if FireRuleSet(p, ps) = raAllow then
          begin
            ps.status := rssAllowed;
          end;
        end;
      finally
        kb_lock.Leave;
      end;
    end;

    // now add all dst
    for i := p.PazoSitesList.Count - 1 downto 0 do
    begin
      try
        if i < 0 then
          Break;
      except
        Break;
      end;
      ps := TPazoSite(p.PazoSitesList[i]);
      kb_lock.Enter;
      try
        FireRules(p, ps);
      finally
        kb_lock.Leave;
      end;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, rsections, Format('[EXCEPTION] KBAdd FireRules : %s',
        [e.Message]));
    end;
  end;

  if dontFire then
    exit;

  // status changed
  ss := p.RoutesText;
  if ss <> '' then
  begin
    irc_SendROUTEINFOS(ss);
  end;

  if (psource <> nil) and (psource.Status = rssNotAllowed) then
  begin
    psource.Status := rssNotAllowedButItsThere;
  end;

  // now add dirlist
  try
    if (event in [kbeNEWDIR, kbePRE, kbeSPREAD, kbeADDPRE, kbeUPDATE]) then
    begin
      for i := p.PazoSitesList.Count - 1 downto 0 do
      begin
        try
          if i < 0 then
            Break;
        except
          Break;
        end;
        try
          ps := TPazoSite(p.PazoSitesList[i]);

          // dirlist not available
          if ps.dirlist = nil then
          begin
            Debug(dpError, section, 'ERROR: ps.dirlist = nil');
            Continue;
          end;

          // dirlist task already added
          if (ps.dirlist.dirlistadded) and (event <> kbeUPDATE) then
            Continue;

          // Source site is PRE site for this group
          if ps.status in [rssShouldPre, rssRealPre] then
          begin
            r.PredOnAnySite := True;
            dlt := TPazoDirlistTask.Create(netname, channel, ps.Name, p, '', True);
            irc_Addtext_by_key('PRECATCHSTATS', Format('<c7>[KB]</c> %s %s Dirlist added to : %s (PRESITE) from event %s', [section, rls, ps.Name, KBEventTypeToString(event)]));
            ps.dirlist.dirlistadded := True;
            AddTask(dlt);
          end;

          // Source site is _not_ a PRE site for this group
          if ps.status in [rssNotAllowedButItsThere, rssAllowed, rssComplete] then
          begin
            dlt := TPazoDirlistTask.Create(netname, channel, ps.Name, p, '', False);
            irc_Addtext_by_key('PRECATCHSTATS', Format('<c7>[KB]</c> %s %s Dirlist added to : %s (NOT PRESITE) from event %s', [section, rls, ps.Name, KBEventTypeToString(event)]));
            ps.dirlist.dirlistadded := True;
            AddTask(dlt);
          end;

        except
          Continue;
        end;
      end;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] kb_Add add dirlist: %s', [e.Message]));
      exit;
    end;
  end;

  debug(dpSpam, rsections, '<-- %s %s %s %s %s %s %d %d',
    [sitename, section, genre, KBEventTypeToString(event), rls, cdno, integer(dontFire),
    integer(forceFire)]);
end;

function kb_Add(const netname, channel, sitename, section, genre: String; event: TKBEventType; const rls, cdno: String; dontFire: boolean = False; forceFire: boolean = False; ts: TDateTime = 0): integer;
begin
  Result := 0;
  if (Trim(sitename) = '') then
    exit;
  if (Trim(section) = '') then
    exit;
  if (Trim(rls) = '') then
    exit;
  if section = 'TRASH' then
    exit;

  if kb_skip.IndexOf(rls) <> -1 then
  begin
    if spamcfg.readbool(rsections, 'skipped_release', True) then
      irc_addadmin(format('<b><c4>%s</c> @ %s </b>is in skipped releases list!',
        [rls, sitename]));
    exit;
  end;

  try
    Debug(dpMessage, 'kb', '--> ' + Format('%s: %s %s @ %s (%s%s)',
      [KBEventTypeToString(event), section, rls, sitename, genre, cdno]));
    Result := kb_AddB(netname, channel, sitename, section, genre,
      event, rls, cdno, dontFire, forceFire, ts);
    Debug(dpMessage, 'kb', '<-- ' + Format('%s: %s %s @ %s (%s%s)',
      [KBEventTypeToString(event), section, rls, sitename, genre, cdno]));
  except
    on E: Exception do
    begin
      Debug(dpError, 'kb', Format('[EXCEPTION] kb_Add: %s', [e.Message]));
      Result := 0;
      exit;
    end;
  end;

  QueueFire;
end;

function FindReleaseInKbList(const rls: String): String;
var
  i: integer;
begin
  Result := '';
  for i := 0 to kb_list.Count - 1 do
  begin
    if AnsiContainsText(kb_list[i], rls) then
    begin
      Result := kb_list[i];
      break;
    end;
  end;
end;

{!--- KB Utils ---?}

function GetKbPazo(p: TPazo): String;
begin
  Result := p.rls.section + #9 + p.rls.rlsname + #9 + p.rls.ShowExtraInfo +
    #9 + IntToStr(DateTimeToUnix(p.added)) + #9 +
    IntToStr(p.rls.pretime) + #9 + KBEventTypeToString(p.rls.kb_event);
end;

procedure AddKbPazo(const line: String);
var
  section, rlsname, extra: String;
  event: TKBEventType;
  added: TDateTime;
  p: TPazo;
  r: TRelease;
  rc: TCRelease;
  ctime: int64;
begin
  section := SubString(line, #9, 1);
  rlsname := SubString(line, #9, 2);
  extra := SubString(line, #9, 3);
  added := UnixToDateTime(StrToInt64(SubString(line, #9, 4)));
  ctime := Strtoint64(SubString(line, #9, 5));
  event := EventStringToTKBEventType(SubString(line, #9, 6));
  kb_trimmed_rls.Add(section + '-' + Copy(rlsname, 1, Length(rlsname) - 1));
  kb_trimmed_rls.Add(section + '-' + Copy(rlsname, 2, Length(rlsname) - 1));

  rc := FindSectionHandler(section);

  if ctime > 0 then
    r := rc.Create(rlsname, section, True, ctime)
  else
    r := rc.Create(rlsname, section);

  //r.pretime:=UnixToDateTime(ctime);
  r.kb_event := event;

  if extra <> '' then
  begin
    r.Aktualizald(extra);
    r.aktualizalva := True;
  end;

  p := PazoAdd(r);

  p.added := added;
  p.stated := True;
  p.cleared := True;
  p.ExcludeFromIncfiller := True;
  kb_list.AddObject(section + '-' + rlsname, p);
end;

procedure KB_start;
var
  x: TEncStringlist;
  i: integer;
  last: TDateTime;
begin
  kb_reloadsections;

  // itt kell betoltenunk az slftp.kb -t
  kb_lock.Enter;
  try
    x := TEncStringlist.Create(passphrase);
    try
      //    Console_Addline('', 'Loading KB entries...');
      x.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'slftp.kb');
      last := Now;
      for i := 0 to x.Count - 1 do
      begin
        //Console_QueueStat(x.Count - i - 1);
        AddKbPazo(x[i]);
        if MilliSecondsBetween(Now, last) > 500 then
        begin
          last := Now;
          slapp.ProcessMessages;
        end;
      end;
      Console_Addline('', 'Ok.');
    finally
      x.Free;
    end;
  finally
    kb_lock.Leave;
  end;

  x := TEncStringlist.Create(passphrase);
  try
    //    Console_Addline('', 'Loading KB renames...');
    x.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'slftp.renames');
    for i := 0 to x.Count - 1 do
    begin
      kb_skip.Insert(0, x[i]);
    end;
    //    Console_Addline('', Format('Ok loaded %d KB renames.', [kb_skip.Count]));
  finally
    x.Free;
  end;

  kb_thread := TKBThread.Create;
end;

procedure kb_Save;
var
  i: integer;
  seconds: integer;
  x: TEncStringList;
  p: TPazo;
begin
  kb_last_saved := Now();
  Debug(dpSpam, rsections, 'kb_Save');
  seconds := config.ReadInteger(rsections, 'kb_keep_entries', 86400 * 7);
  x := TEncStringList.Create(passphrase);
  try
    try
      for i := 0 to kb_list.Count - 1 do
      begin
        p := TPazo(kb_list.Objects[i]);
        if ((p <> nil) and (1 <> Pos('TRANSFER-', kb_list[i])) and
          (1 <> Pos('REQUEST-', kb_list[i])) and
          (SecondsBetween(Now, p.added) < seconds)) then
          x.Add(GetKbPazo(p));
      end;
    except
      exit;
    end;
    x.SaveToFile(ExtractFilePath(ParamStr(0)) + 'slftp.kb');
  finally
    x.Free;
  end;

  debug(dpSpam, rsections, 'kb_Save - saving %d renames', [kb_skip.Count]);
  x := TEncStringList.Create(passphrase);
  try
    try
      for i := 0 to kb_skip.Count - 1 do
      begin
        if i > 249 then
          break;
        x.Add(kb_skip[i]);
      end;
    except
      exit;
    end;
    x.SaveToFile(ExtractFilePath(ParamStr(0)) + 'slftp.renames');
  finally
    x.Free;
  end;
end;

procedure kb_FreeList;
var
  i: integer;
begin
  for i := 0 to kb_list.Count - 1 do
  begin
    try
      if kb_List.Objects[i] <> nil then
      begin
        kb_List.Objects[i].Free;
        kb_List.Objects[i] := nil;
      end;
    except
      continue;
    end;
  end;

  kb_list.Free;
  kb_trimmed_rls.Free;
end;

function kb_reloadsections: boolean;
var
  xin: Tinifile;
  secs: TStringlist;
  r: TRegexpr;
  I: Integer;
begin
  //  Result := False;
  kb_sections.Free;
  kb_sections := TStringList.Create;
  kb_sections.Sorted := True;
  kb_sections.Duplicates := dupIgnore;

  secs := TStringlist.Create;
  r := TRegexpr.Create;
  xin := Tinifile.Create(ExtractFilePath(ParamStr(0)) + 'slftp.precatcher');
  try
    r.ModifierI := True;
    r.ModifierM := True;
    r.Expression := '^(\#|\/\/)';
    xin.ReadSection('sections', secs);
    for I := 0 to secs.Count - 1 do
      if not r.Exec(secs.Strings[i]) then
        kb_sections.Add(secs.Strings[i]);

    for I := 0 to mappingslist.Count - 1 do
    begin
      if TMap(mappingslist.Items[i]).origsection <> '' then
        kb_sections.Add(TMap(mappingslist.Items[i]).origsection);
      kb_sections.Add(TMap(mappingslist.Items[i]).newsection);
    end;

  finally
    xin.Free;
    r.free;
    secs.free;
  end;
  Result := True;
end;

procedure kb_Init;
var
  i: integer;
  ss: String;
  //  xin: Tinifile;
  x: TStringList;
begin
  kb_last_saved := Now();
  //  kbevent:=TEvent.Create(nil,false,false,'PRETIME_WAIT_EVENT');

  KbClassesInit;

  addpreechocmd := config.ReadString('dbaddpre', 'addpreechocmd', '!sitepre');

  kb_lock := TCriticalSection.Create;

  kb_trimmed_rls := THashedStringList.Create;
  kb_trimmed_rls.CaseSensitive := False;

  kb_list := TStringList.Create;
  kb_list.CaseSensitive := False;
  kb_list.Duplicates := dupIgnore;

  kb_sections := TStringList.Create;
  kb_sections.Sorted := True;
  kb_sections.Duplicates := dupIgnore;

  rename_patterns := 4;

  //xin := Tinifile.Create(ExtractFilePath(ParamStr(0)) + 'slftp.precatcher');
  //  xin.ReadSection('sections', kb_sections);
  //  xin.Free;

  mp3genres := TStringList.Create;
  mp3genres.Delimiter := ' ';
  mp3genres.QuoteChar := '"';
  mp3genres.DelimitedText := config.ReadString(rsections, 'mp3genres', '');
  i := 0;
  while (i < mp3genres.Count) do
  begin
    ss := ReplaceText(mp3genres[i], ' ', '');
    if ss <> mp3genres[i] then
    begin
      mp3genres.Insert(i + 1, ss);
      Inc(i);
    end;
    Inc(i);
  end;

  if FileExists(ExtractFilePath(ParamStr(0)) + 'imdbcountrys.nwo') then
  begin
    x := TStringList.Create;
    try
      x.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'imdbcountrys.nwo');
      x.SaveToFile(ExtractFilePath(ParamStr(0)) + 'slftp.imdbcountries');
      {$IFDEF MSWINDOWS}
        {$IFDEF UNICODE}
          DeleteFile(PChar(ExtractFilePath(ParamStr(0)) + 'imdbcountrys.nwo'));
        {$ELSE}
          DeleteFile(PAnsiChar(ExtractFilePath(ParamStr(0)) + 'imdbcountrys.nwo'));
        {$ENDIF}
      {$ELSE}
        DeleteFile(ExtractFilePath(ParamStr(0)) + 'imdbcountrys.nwo');
      {$ENDIF}
    finally
      x.Free;
    end;
  end;

  imdbcountries := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'slftp.imdbcountries');

  kb_groupcheck_rls := THashedStringList.Create;
  kb_latest := THashedStringList.Create;
  kb_skip := THashedStringList.Create;

  trimmed_shit_checker := config.ReadBool(rsections, 'trimmed_shit_checker', True);
  renamed_group_checker := config.ReadBool(rsections, 'renamed_group_checker', True);
  renamed_release_checker := config.ReadBool(rsections, 'renamed_release_checker', True);

  enable_try_to_complete := config.ReadBool(rsections, 'enable_try_to_complete', False);
  try_to_complete_after := config.ReadInteger(rsections, 'try_to_complete_after', 450);

  kb_save_entries := config.ReadInteger(rsections, 'kb_save_entries', 3600);

  taskpretime_mode := config.ReadInteger('taskpretime', 'mode', 0);
end;

procedure kb_Stop;
begin
  while (kb_thread <> nil) do
    sleep(100);
end;

procedure kb_Uninit;
begin
  Debug(dpSpam, rsections, 'Uninit1');
  kbevent.Free;
  kb_sections.Free;
  mp3genres.Free;
  kb_latest.Free;
  kb_skip.Free;
  kb_groupcheck_rls.Free;
  imdbcountries.Free;

  KbClassesUninit;

  kb_lock.Free;

  Debug(dpSpam, rsections, 'Uninit2');
end;

{ TKBThread }

constructor TKBThread.Create;
begin
  inherited Create(False);
  {$IFDEF DEBUG}
    NameThreadForDebugging('KB', self.ThreadID);
  {$ENDIF}
  FreeOnTerminate := True;
  kbevent := TEvent.Create(nil, False, False, 'kb');
end;

destructor TKBThread.Destroy;
begin
  kbevent.Free;
  inherited;

  // not sure if this is needed/useful?!
  kb_thread := nil;
end;

function TKBThread.AddCompleteTransfers(pazo: Pointer): boolean;
var
  i, j, rank: integer;
  ps, sps: TPazoSite;
  ss: TSite;
  p: TPazo;
  rc: TCRelease;
  rls: TRelease;
  pdt: TPazoDirlistTask;
  sources, destinations: TList<TPazoSite>;
  site_allocation: TObjectDictionary<String, TStringList>;
  ssites_info, dsites_info: TStringList;
  d: TDirlist;

  { Verify that the directory is still there
    @returns(@true if directory is still there, @false otherwise) }
  function IsDirStillAccessible: boolean;
  begin
    Result := True;
    d := DirlistB('', '', ss.Name, MyIncludeTrailingSlash(ps.maindir) + MyIncludeTrailingSlash(ps.pazo.rls.rlsname));
    try
      if (d = nil) then
      begin
        Debug(dpSpam, rsections, 'AddCompleteTransfers %s unable to do dirlist or directory is no longer there', [ps.Name]);
        Result := False;
      end;
    finally
      d.Free;
    end;
  end;

begin
  Result := False;
  p := TPazo(pazo);
  Debug(dpMessage, rsections, '<!-- START AddCompleteTransfers %s', [p.rls.rlsname]);

  sources := TList<TPazoSite>.Create;
  destinations := TList<TPazoSite>.Create;
  site_allocation := TObjectDictionary<String, TStringList>.Create([doOwnsValues]);

  try
    // check if the release is incomplete on any site and gather valid sites for filling
    for i := 0 to p.PazoSitesList.Count - 1 do
    begin
      ps := TPazoSite(p.PazoSitesList[i]);
      Debug(dpSpam, rsections, 'AddCompleteTransfers checking out %s', [ps.Name]);

      if ps.Name = getAdminSiteName then
        Continue;

      if ps.error then
      begin
        Debug(dpMessage, rsections, Format('Error AddCompleteTransfers for %s: %s', [ps.Name, ps.reason]));
        Continue;
      end;

      ss := FindSiteByName('', ps.Name);
      if ss = nil then
        Continue;

      if ss.PermDown then
      begin
        Debug(dpSpam, rsections, 'AddCompleteTransfers %s ss is permdown', [ps.Name]);
        Continue;
      end;

      if (ss.WorkingStatus in [sstMarkedAsDownByUser]) then
      begin
        Debug(dpSpam, rsections, 'AddCompleteTransfers %s ss is marked down by user', [ps.Name]);
        Continue;
      end;

      if ps.Complete then
      begin
        if not IsDirStillAccessible then
          Continue;

        sources.Add(ps);
        Debug(dpSpam, rsections, 'AddCompleteTransfers taking %s as source', [ps.Name]);
      end
      else
      begin
        if ps.status <> rssAllowed then
        begin
          Debug(dpSpam, rsections, 'AddCompleteTransfers %s not rssAllowed', [ps.Name]);
          Continue;
        end;

        if not IsDirStillAccessible then
          Continue;

        destinations.Add(ps);
        Debug(dpSpam, rsections, 'AddCompleteTransfers taking %s as destination', [ps.Name]);
      end;
    end;

    if ((destinations.Count = 0) or (sources.Count = 0)) then
    begin
      Result := True;
      exit;
    end;

    // Found at least one site that has the release, issue dirlists for each one and create pazo to send it to destinations
    kb_lock.Enter;
    try
      rc := FindSectionHandler(p.rls.section);
      rls := rc.Create(p.rls.rlsname, p.rls.section);
      p := PazoAdd(rls);
      kb_list.AddObject('INC-' + p.rls.rlsname, p);
    finally
      kb_lock.Leave;
    end;

    for ps in destinations do
    begin
      p.AddSite(ps.Name, ps.maindir);
      site_allocation.Add(ps.Name, TStringList.Create);
    end;

    for sps in sources do
    begin
      site_allocation.Add(sps.Name, TStringList.Create);
      ssites_info := site_allocation.Items[sps.Name];
      for ps in destinations do
      begin
        // Check for every destination if its routable if we care about that
        rank := sitesdat.ReadInteger('speed-from-' + sps.Name, ps.Name, 0);
        if ((config.ReadBool(rsections, 'only_use_routable_sites_on_try_to_complete', True)) and (rank = 0)) then
          Continue;
        ssites_info.Add(ps.Name);
      end;
      // Skip this source if there are no routable destinations available
      if ssites_info.Count = 0 then
        Continue;

      ps := p.AddSite(sps.Name, sps.maindir);
      // Add every destination and the real ranks (if available) or a default of 0 for routing source -> destination
      for j := 0 to ssites_info.Count - 1 do
      begin
        rank := sitesdat.ReadInteger('speed-from-' + sps.Name, ssites_info[j], 0);
        ps.AddDestination(ssites_info[j], rank);
        dsites_info := site_allocation.Items[ssites_info[j]];
        dsites_info.Add(sps.Name);
      end;
    end;

    for ps in sources do
    begin
      try
        ssites_info := site_allocation.Items[ps.Name];
        // if this source has no destination we dont need to issue a dirlist as it would not yield any race actions
        if ssites_info.Count = 0 then
          Continue;
        pdt := TPazoDirlistTask.Create('', '', ps.Name, p, '', True);
        AddTask(pdt);
      except
        on e: Exception do
        begin
          Debug(dpError, rsections, Format('[EXCEPTION] TAutoDirlistTask.ProcessRequest AddTask: %s', [e.Message]));
        end;
      end;
    end;
    for ps in destinations do
    begin
      try
        dsites_info := site_allocation.Items[ps.Name];
        // if this destination has no matching sources we arent going to fill anything and as such dont need the dirlists
        if dsites_info.Count = 0 then
          Continue;
        pdt := TPazoDirlistTask.Create('', '', ps.Name, p, '', False);
        AddTask(pdt);
        irc_Addstats(Format(
          '<c11>[<b>iNC</b> <b>%s</b>]</c> Trying to complete <b>%s</b> on <b>%s</b> from <b>%s</b>',
          [p.rls.section, p.rls.rlsname, ps.Name, dsites_info.CommaText]));
      except
        on e: Exception do
        begin
          Debug(dpError, rsections, Format('[EXCEPTION] TAutoDirlistTask.ProcessRequest AddTask: %s', [e.Message]));
        end;
      end;
    end;
  finally
    sources.Free;
    destinations.Free;
    site_allocation.Free;
  end;

  Debug(dpMessage, rsections, '<-- END AddCompleteTransfers %s', [p.rls.rlsname]);
end;

procedure TKBThread.Execute;
var
  i: integer;
  p: TPazo;
  fIncFillPazos: TList<TPazo>;
begin
  fIncFillPazos := TList<TPazo>.Create;
  try
    while (not slshutdown) do
    begin
      try
        kb_lock.Enter;
        p := nil;
        try
          for i := 0 to kb_list.Count - 1 do
          begin
            if kb_list[i].StartsWith('TRANSFER-') then
              Continue;
            if kb_list[i].StartsWith('REQUEST-') then
              Continue;
            if kb_list[i].StartsWith('INC-') then
              Continue;

            try
              p := TPazo(kb_list.Objects[i]);
            except
              Continue;
            end;
            if p = nil then
              Continue;
            if p.rls = nil then
              Continue;

            if enable_try_to_complete then
            begin
              if ((not p.ExcludeFromIncfiller) and (not p.stopped) and (SecondsBetween(Now, p.lastTouch) >= try_to_complete_after)) then
              begin
                RemovePazo(p.pazo_id);
                while (not (p.queuenumber.Value <= 0)) do
                begin
                  p.queuenumber.Decrease;
                end;
                p.ExcludeFromIncfiller := True;
                fIncFillPazos.Add(p);
              end;
            end;

            if ((p.ready) and (SecondsBetween(Now, p.lastTouch) > 3600) and (not p.stated) and (not p.cleared)) then
            begin
              RemovePazo(p.pazo_id);

              try
                RanksProcess(p);
              except
                on E: Exception do
                begin
                  Debug(dpError, rsections, Format('[EXCEPTION] TKBThread.Execute RanksProcess(p) : %s', [e.Message]));
                end;
              end;

              p.Clear;
              p.stated := True;
            end;
          end;
        finally
          kb_lock.Leave;
        end;
      except
        on e: Exception do
        begin
          Debug(dpError, rsections, '[EXCEPTION] TKBThread.Execute: %s', [e.Message]);
        end;
      end;

      // do this outside of kb_lock because of possible long running operations (dirlist)
      for p in fIncFillPazos do
      begin
        Debug(dpSpam, rsections, 'Looking for incomplete sites of %s', [p.rls.rlsname]);
        AddCompleteTransfers(p);
      end;
      fIncFillPazos.Clear;

      if ((kb_save_entries <> 0) and (SecondsBetween(Now(), kb_last_saved) > kb_save_entries)) then
      begin
        try
          kb_lock.Enter;
          try
            kb_Save;
          finally
            kb_lock.Leave;
          end;
        except
          on e: Exception do
          begin
            Debug(dpError, rsections, '[EXCEPTION] kb_Save : %s', [e.Message]);
          end;
        end;
      end;

      //sleep(900);
      kbevent.WaitFor(5000);
    end;
  finally
    fIncFillPazos.Free;
  end;
end;

end.

