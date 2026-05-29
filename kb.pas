{
  @abstract(Knowledge base functions)
}
unit kb;

interface

uses
  Classes, SyncObjs, slcriticalsection2, kb.releaseinfo, pazo,
  speedstatsunit, statsunit;

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
  dontFire: boolean = False; forceFire: boolean = False; ts: TDateTime = 0; aDetectedTick: Int64 = 0): integer;
function FindReleaseInKbList(const rls: String): String;

{ Finds a release in latest KB list
      @param(aRls The release name to be searched for)
      @returns(The section name if the release has been found, an empty string otherwise) }
function FindReleaseInLatestKBList(const aRls: String): String;
function FindPazoByRls(const rlsname: String): TPazo;
function FindPazoById(const id: integer): TPazo;
function FindPazoByName(const section, rlsname: String): TPazo;
{ Finds a release/pazo in the KB list by the given key. The key must be in the format of the KB list keys which is 'section-releasename'
      @param(aKey The KB key to be searched for)
      @returns(The found TPazo object or nil if the key is not present in the KB list.) }
function FindPazoByKey(const aKey: String): TPazo;

{ Adds a release/pazo to the KB list with the given key. The key must be in the format of the KB list keys which is 'section-releasename'
      @param(aKey The KB key to be used)
      @param(aPazo The TPazo object to be added) }
procedure AddPazoToKB(const aKey: String; const aPazo: TPazo);

function FindSectionHandler(const section: String): TCRelease;

{ Returns the number of items in the KB
      @returns(The number of items in the KB) }
function GetKBCount: integer;

{ @abstract(Returns a reference to the KB list for direct read access - caller must hold KB lock) }
function GetKBList: TStringList;

{ @abstract(Returns a reference to the KB lock for thread-safe access) }
function GetKBLock: TSlCriticalSection2;

{ Lists all KB entries to IRC which match the given section
      @param(section The section to show the KB entries of.)
      @param(hits The limit of how many entries should be listed.) }
procedure ListKBToIRC(const netname, channel, section: string; const hits: integer);

procedure kb_FreeList;
procedure kb_Save;
procedure KB_start;
procedure kb_Init;
procedure kb_Uninit;
procedure kb_Stop;

function kb_reloadsections: boolean;

var
  kb_sections: TStringList;
  kb_thread: TKBThread;

implementation

uses
  debugunit, mainthread, taskgenrenfo, taskgenredirlist, configunit, console,
  taskrace, sitesunit, queueunit, irc, SysUtils, fake, mystrings, tasksunit,
  rulesunit, Math, DateUtils, StrUtils, precatcher, tasktvinfolookup, encinifile,
  slvision, tasksitenfo, RegExpr, taskpretime, taskgame, mygrouphelpers, routeconfig,
  sllanguagebase, taskmvidunit, dbaddpre, dbaddimdb, dbtvinfo, irccolorunit,
  mrdohutils, ranksunit, tasklogin, dbaddnfo, contnrs, slmasks, dirlist, IniFiles, mormot.core.unicode,
  globalskipunit, irccommandsunit, slapi.issueshook, cbftpclient, cbftpevents, uLkJSON, Generics.Collections, Generics.Defaults {$IFDEF MSWINDOWS}, Windows{$ENDIF};

const
  rsections = 'kb';

var
  addpreechocmd: String;
  kb_last_saved: TDateTime;
  kb_list: TStringList;
  kb_lock: TSLCriticalSection2;
  GlRaceCompletions: TObjectDictionary<string, TList<TCbftpEvent>>;

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
  kb_keep_entries: integer;

  rename_patterns: integer;
  taskpretime_mode: integer;
  glAutoAddAffils: boolean;
  glOnlyUseRouteableSitesOnTryToComplete: boolean;

function GetKBCount: integer;
begin
  // access to Count is thread safe, so no lock required
  Result := kb_list.Count;
end;

function GetKBList: TStringList;
begin
  Result := kb_list;
end;

function GetKBLock: TSlCriticalSection2;
begin
  Result := kb_lock;
end;

function FindSectionHandler(const section: String): TCRelease;
var
  i: integer;
begin
  Result := GlSectionHandlers[0];

  for i := 1 to High(GlSectionHandlers) do
  begin
    if GlSectionHandlers[i].SectionAccepted(section) then
    begin
      Result := GlSectionHandlers[i];
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

function kb_AddB(const netname, channel, sitename, section, genre: String; event: TKBEventType; rls, cdno: String; dontFire: boolean = False; forceFire: boolean = False; ts: TDateTime = 0; aDetectedTick: Int64 = 0): integer;
var
  i, j, len: integer;
  r: TRelease;
  rc: TCRelease;
  s: TSite;
  ss: String;
  p: TPazo;
  ps, psource: TPazoSite;
  rule_result: TRuleAction;
  rlz, grp: String;
  dlt: TPazoDirlistTask;
  l: TLoginTask;
  fPretimeLookupTask: TPazoPretimeLookupTask;

  function IsUDPEnabled: Boolean;
  var
    rawEnable: String;
    udpIp: String;
    udpPort: Integer;
  begin
    rawEnable := Trim(config.ReadString('UDPConfig', 'EnableUDP', 'False'));
    udpIp := Trim(config.ReadString('UDPConfig', 'IP', ''));
    udpPort := config.ReadInteger('UDPConfig', 'Port', 0);
    Result := (SameText(rawEnable, 'True') or SameText(rawEnable, '1')) and
      (udpIp <> '') and (udpPort >= 1) and (udpPort <= 65535);
  end;

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

  kb_lock.Enter('kb_AddB_1');
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

  kb_lock.Enter('kb_AddB_2');
  try
    i := kb_list.IndexOf(section + '-' + rls);
    if i = -1 then
    begin
      if (event = kbeNUKE) then
      begin
        // nuking an old rls not in kb
        IssueLog('NUKE', section, rls, sitename, 'not in kb', KBEventTypeToString(event),
          'NUKE|' + sitename + '|' + rls);
        irc_Addstats(Format('<c4>[NUKE]</c> %s %s @ %s (not in kb)',
          [section, rls, '<b>' + sitename + '</b>']));
        exit;
      end;

      if (event = kbeCOMPLETE) then
      begin
        // complet an old rls not in kb
        if not IsUDPEnabled then
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
            dbaddpre_InsertRlz(rls, section, 'SITE-' + sitename, True);
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

      if aDetectedTick > 0 then
        r.DetectedTick := aDetectedTick;

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
      p.AddSites;

      kb_list.BeginUpdate;
      try
        kb_list.AddObject(section + '-' + rls, p);
      finally
        kb_list.EndUpdate;
      end;

      // announce event on admin chan
      if (event = kbeADDPRE) then
      begin
        if spamcfg.ReadBool('kb', 'new_rls', True) then
          irc_Addstats(Format('<c3>[ADDPRE]</c> %s %s @ <b>%s</b>', [section, rls, channel]));
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

            if GlTaskPretimeReaddAttempts > 0 then
            begin
              fPreTimeLookupTask := TPazoPretimeLookupTask.Create(netname, channel, getadminsitename, p, 1);
              fPreTimeLookupTask.startat := IncSecond(Now, GlTaskPretimeReaddInterval);
              AddTask(fPreTimeLookupTask);
            end;
          end;
        end
        else
        begin
          if spamcfg.ReadBool('kb', 'new_rls', True) then
            irc_Addstats(Format('<c3>[<b>NEW</b>]</c> %s %s @ <b>%s</b> (<b>%s</b>) (<c3><b>%s ago</b></c>) (%s)', [section, rls, sitename, p.sl.sectionname, dbaddpre_GetPreduration(r.pretime), r.PretimeSource]));
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
              irc_SendUPDATE(Format('<c3>[UPDATE]</c> %s %s @ <b>%s</b> now has pretime (<c3><b>%s ago</b></c>) (%s)', [section, rls, sitename, dbaddpre_GetPreduration(r.pretime), r.PretimeSource]));
            p.AddSites;
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
          if (p.rls.section <> '') and (s.sectiondir[p.rls.section] = '') then
          begin
            irc_Addstats(Format('<c5>[SECTION NOT SET]</c> : %s %s @ %s (%s)', [p.rls.section, p.rls.rlsname, sitename, KBEventTypeToString(event)]));
            IssueLog('MISSING_SECTION', p.rls.section, p.rls.rlsname, sitename, '', KBEventTypeToString(event),
              'MISSING_SECTION|' + sitename + '|' + p.rls.section, 300);
          end;
        end;
      end;

      // races/kb_adds are happening - site must be up again
      if ((s <> nil) and (not s.PermDown) and (s.WorkingStatus in [sstDown, sstTempDown]) and (event in [kbeCOMPLETE, kbePRE, kbeSPREAD])) then
      begin
        try
          l := TLoginTask.Create(netname, channel, sitename, False, False);
          l.noannounce := True;
          AddTask(l, true);
        except
          on E: Exception do
            Debug(dpError, rsections, '[EXCEPTION] COMPLETE|PRE|SPREAD LoginTask : %s', [e.Message]);
        end;
      end;

      exit;
    end;

    s := FindSiteByName(netname, psource.Name);
    if ((s <> nil) and (not p.IsUDPEnabled) and (not (s.WorkingStatus in [sstUnknown, sstUp]))) then
      exit;

    if (s <> nil) and (sitename <> getAdminSiteName) and (not s.PermDown) and (s.WorkingStatus in [sstUnknown, sstUp]) then
    begin
      if (p.rls.section <> '') and (s.sectiondir[p.rls.section] = '') then
      begin
        irc_Addstats(Format('<c5>[SECTION NOT SET]</c> : %s %s @ %s (%s)', [p.rls.section, p.rls.rlsname, sitename, KBEventTypeToString(event)]));
        IssueLog('MISSING_SECTION', p.rls.section, p.rls.rlsname, sitename, '', KBEventTypeToString(event),
          'MISSING_SECTION|' + sitename + '|' + p.rls.section, 300);
      end;
    end;

    psource.ircevent := True;

    if psource.ts < ts then
    begin
      psource.ts := ts;
    end;

    if (event = kbePRE) then
    begin
      if (s <> nil) then
      begin
        if ((not s.IsAffil(r.groupname)) and (glAutoAddAffils)) then
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
      IssueLog('NUKE', p.rls.section, p.rls.rlsname, psource.Name, '', KBEventTypeToString(event),
        'NUKE|' + psource.Name + '|' + p.rls.rlsname);
      irc_Addstats(Format('<c4>[NUKE]</c> %s %s @ <b>%s</b>',
        [section, rls, sitename]));
      try
        RemovePazoMKDIR(p.pazo_id, psource.Name, rls);
        RemoveRaceTasks(p.pazo_id, psource.Name);
        RemovePazoDirTasks(p.pazo_id, psource.Name);
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
    kb_lock.Enter('kb_AddB_3');
    try
      rule_result := raDrop;
      rule_result := FireRuleSet(p, psource);
    finally
      kb_lock.Leave;
    end;

    // announce SKIP and DONT MATCH only if the site is not a PRE site
    if (psource <> nil) and (psource.status <> rssRealPre) then
    begin
      if (rule_result = raDrop) and (spamcfg.ReadBool('kb', 'skip_rls', True)) then
      begin
        IssueLog('SKIP', p.rls.section, p.rls.rlsname, psource.Name, psource.reason, KBEventTypeToString(event),
          'SKIP|' + psource.Name + '|' + p.rls.rlsname);
        irc_Addstats(Format('<c5>[SKIP]</c> : %s %s @ %s "%s" (%s)',
          [p.rls.section, p.rls.rlsname, psource.Name, psource.reason, KBEventTypeToString(event)]));
      end
      else if (rule_result = raDontmatch) and (spamcfg.ReadBool('kb', 'dont_match_rls', True)) then
      begin
        IssueLog('DONT_MATCH', p.rls.section, p.rls.rlsname, psource.Name, psource.reason, KBEventTypeToString(event),
          'DONT_MATCH|' + psource.Name + '|' + p.rls.rlsname);
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
      kb_lock.Enter('kb_AddB_4');
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
      kb_lock.Enter('kb_AddB_5');
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
  if (event <> kbeNUKE) then
  begin
    ss := p.RoutesText;
    if (ss <> '') and (not p.IsUDPEnabled) then
      irc_SendROUTEINFOS(ss);
  end;

  if (psource <> nil) and (psource.Status = rssNotAllowed) then
  begin
    psource.Status := rssNotAllowedButItsThere;
  end;

  // now add dirlist (skip when UDP is enabled)
  try
    if (event in [kbeNEWDIR, kbePRE, kbeSPREAD, kbeADDPRE, kbeUPDATE]) then
    begin
      if not p.IsUDPEnabled then
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

            // dirlist task already added or failed
            if (ps.dirlist.error) then
              Continue;
            if (ps.dirlist.dirlistadded) and (event <> kbeUPDATE) then
              Continue;

            // Source site is PRE site for this group
            if ps.status in [rssShouldPre, rssRealPre] then
            begin
              r.PredOnAnySite := True;
              dlt := TPazoDirlistTask.Create(netname, channel, ps.Name, p, '', True);
              irc_Addtext_by_key('PRECATCHSTATS', Format('<c7>[KB]</c> %s %s Dirlist added to : %s (PRESITE) from event %s', [section, rls, ps.Name, KBEventTypeToString(event)]));
              ps.dirlist.dirlistadded := True;
              AddTask(dlt, true);
            end;

            // Source site is _not_ a PRE site for this group
            if ps.status in [rssNotAllowedButItsThere, rssAllowed, rssComplete] then
            begin
              dlt := TPazoDirlistTask.Create(netname, channel, ps.Name, p, '', False);
              irc_Addtext_by_key('PRECATCHSTATS', Format('<c7>[KB]</c> %s %s Dirlist added to : %s (NOT PRESITE) from event %s', [section, rls, ps.Name, KBEventTypeToString(event)]));
              ps.dirlist.dirlistadded := True;
              AddTask(dlt, true);
            end;

          except
            on E: Exception do
            begin
              Debug(dpError, section, Format('[EXCEPTION] kb_Add add dirlist iterate: %s', [e.Message]));
              continue;
            end;
          end;
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

function kb_Add(const netname, channel, sitename, section, genre: String; event: TKBEventType; const rls, cdno: String; dontFire: boolean = False; forceFire: boolean = False; ts: TDateTime = 0; aDetectedTick: Int64 = 0): integer;
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

  try
    Debug(dpMessage, 'kb', '--> ' + Format('%s: %s %s @ %s (%s%s)',
      [KBEventTypeToString(event), section, rls, sitename, genre, cdno]));
    Result := kb_AddB(netname, channel, sitename, section, genre,
      event, rls, cdno, dontFire, forceFire, ts, aDetectedTick);
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
end;

function FindReleaseInKbList(const rls: String): String;
var
  i: integer;
begin
  Result := '';
  kb_lock.Enter('FindReleaseInKbList ' + rls);
  try
    for i := 0 to kb_list.Count - 1 do
    begin
      if AnsiContainsText(kb_list[i], rls) then
      begin
        Result := kb_list[i];
        break;
      end;
    end;
  finally
    kb_lock.Leave;
  end;
end;

function FindReleaseInLatestKBList(const aRls: String): String;
var
  i: integer;
begin
  Result := '';
  kb_lock.Enter('FindReleaseInLatestKBList ' + aRls);
  try
    i := kb_latest.IndexOfName(aRls);
    if i <> -1 then
    begin
      Result := kb_latest.ValueFromIndex[i];
    end;
  finally
    kb_lock.Leave;
  end;
end;

function FindPazoByRls(const rlsname: String): TPazo;
var
  i: integer;
  p: TPazo;
begin
  Result := nil;
  kb_lock.Enter('FindPazoByRls');
  try
    try
      for i := kb_list.Count - 1 downto 0 do
      begin
        if i < 0 then
          Break;

        p := TPazo(kb_list.Objects[i]);

        if p = nil then
          Continue;

        if p.rls = nil then
          Continue;

        if (p.rls.rlsname = rlsname) then
        begin
          Result := p;
        end;
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, 'kb', Format('[EXCEPTION] FindPazoByRls: %s', [e.Message]));
        Result := nil;
      end;
    end;
  finally
    kb_lock.Leave;
  end;
end;

function FindPazoById(const id: integer): TPazo;
var
  i: integer;
  p: TPazo;
begin
  Result := nil;
  kb_lock.Enter('FindPazoById');
  try
    try
      for i := kb_list.Count - 1 downto 0 do
      begin
        if i < 0 then
            Break;

        p := TPazo(kb_list.Objects[i]);
        if p = nil then
          exit;
        if p.pazo_id = id then
        begin
          Result := p;
          p.lastTouch := Now();
          exit;
        end;
      end;
    except
      on E: Exception do
      begin
        Debug(dpError, 'kb', Format('[EXCEPTION] FindPazoById: %s', [e.Message]));
        Result := nil;
      end;
    end;
  finally
    kb_lock.Leave;
  end;
end;

function FindPazoByKey(const aKey: String): TPazo;
var
  i: integer;
begin
  Result := nil;
  kb_lock.Enter('FindPazoByKey');
  try
    try
      i := kb_list.IndexOf(aKey);
      if i <> -1 then
      begin
        Result := TPazo(kb_list.Objects[i]);

        if Result <> nil then
          Result.lastTouch := Now;

        exit;
      end;
    except
     on E: Exception do
     begin
       Debug(dpError, 'kb', Format('[EXCEPTION] FindPazoByKey: %s', [e.Message]));
       Result := nil;
     end;
    end;
  finally
     kb_lock.Leave;
  end;
end;

function FindPazoByName(const section, rlsname: String): TPazo;
begin
  if section = '' then
    Result := FindPazoByRls(rlsname)
  else
    Result := FindPazoByKey(section + '-' + rlsname);
end;

procedure AddPazoToKB(const aKey: String; const aPazo: TPazo);
begin
  kb_lock.Enter('AddPazoToKB');
  try
    kb_list.AddObject(aKey, aPazo);
  finally
    kb_lock.Leave;
  end;
end;

procedure ListKBToIRC(const netname, channel, section: string; const hits: integer);
var
  db, i: integer;
  p: TPazo;
begin
  kb_lock.Enter('ListKBToIRC');
  try
    db := 0;
    for i := kb_list.Count - 1 downto 0 do
    begin
      if (db > hits) then
        break;

      p := TPazo(kb_list.Objects[i]);
      if p <> nil then
      begin
        if ((section = '') or (p.rls.section = section)) then
        begin
          irc_addtext(Netname, Channel, '#%d %s %s [QueueNumber: %d (Race:%d Dirlist:%d Mkdir:%d)]',
            [p.pazo_id, p.rls.section, p.rls.rlsname, p.queuenumber.Value, p.racetasks.Value,
            p.dirlisttasks.Value, p.mkdirtasks.Value]);

          Inc(db);
        end;
      end
      else
      begin
        irc_addtext(Netname, Channel, 'Whops, Pazo is nil! Anything screwed up!');
      end;
    end;
  finally
    kb_lock.Leave;
  end;
end;

{!--- KB Utils ---?}

procedure SyncSitesFromCbftp;
var
  jsonStr: AnsiString;
  js: TlkJSONbase;
  arr: TlkJSONlist;
  obj: TlkJSONObject;
  i: Integer;
  siteName: String;
  disabled: Boolean;
  fSite: TSite;
  f: TlkJSONbase;
begin
  if GlCbftpClient = nil then
    Exit;

  GlSitesSyncing := True;
  try
    try
      jsonStr := AnsiString(GlCbftpClient.GetSites('detailed=true'));
      if jsonStr = '' then
      begin
        Debug(dpError, 'kb', '[cbftp] SyncSites: GetSites returned empty response');
        Exit;
      end;

      js := TlkJSON.ParseText(jsonStr);
      if js = nil then
      begin
        Debug(dpError, 'kb', '[cbftp] SyncSites: failed to parse JSON');
        Exit;
      end;

      try
        if js is TlkJSONlist then
        begin
          arr := TlkJSONlist(js);
          for i := 0 to arr.Count - 1 do
          begin
            if arr.Child[i] is TlkJSONObject then
            begin
              obj := TlkJSONObject(arr.Child[i]);
              
              // Get name
              f := obj.Field['name'];
              if (f <> nil) and (f.SelfType <> jsNull) then
                siteName := f.Value
              else
                siteName := '';

              // Get disabled
              f := obj.Field['disabled'];
              disabled := False;
              if (f <> nil) and (f.SelfType <> jsNull) then
              begin
                disabled := (f.Value = True) or (f.Value = 'true') or (f.Value = '1');
              end;

              if siteName <> '' then
              begin
                fSite := FindSiteByName('', siteName);
                if fSite <> nil then
                begin
                  if disabled then
                  begin
                    if fSite.WorkingStatus <> sstMarkedAsDownByUser then
                      fSite.WorkingStatus := sstMarkedAsDownByUser;
                  end
                  else
                  begin
                    if fSite.WorkingStatus <> sstUp then
                      fSite.WorkingStatus := sstUp;
                  end;
                end;
              end;
            end;
          end;
          Debug(dpMessage, 'kb', Format('[cbftp] Successfully synchronized %d sites status from cbftp', [arr.Count]));
        end
        else
        begin
          Debug(dpError, 'kb', '[cbftp] SyncSites: expected JSON list');
        end;
      finally
        js.Free;
      end;
    except
      on E: Exception do
        DebugException(dpError, 'kb', 'SyncSitesFromCbftp failed', E);
    end;
  finally
    GlSitesSyncing := False;
  end;
end;

procedure KB_start;
var
  x: TEncStringlist;
  i: integer;
  last: TDateTime;
  cbftpIp: String;
  cbftpApiPort: Integer;
  cbftpUdpPort: Integer;
  cbftpUdpBindIp: String;
  cbftpPassword: String;

  procedure AddKbPazo(const line: String);
  var
    section, rlsname: String;
    event: TKBEventType;
    added: TDateTime;
    p: TPazo;
    r: TRelease;
    rc: TCRelease;
    ctime: int64;
  begin
    section := SubString(line, #9, 1);
    rlsname := SubString(line, #9, 2);
    added := UnixToDateTime(StrToInt64(SubString(line, #9, 3)));
    ctime := Strtoint64(SubString(line, #9, 4));
    event := EventStringToTKBEventType(SubString(line, #9, 5));
    kb_trimmed_rls.Add(section + '-' + Copy(rlsname, 1, Length(rlsname) - 1));
    kb_trimmed_rls.Add(section + '-' + Copy(rlsname, 2, Length(rlsname) - 1));

    rc := FindSectionHandler(section);

    if ctime > 0 then
      r := rc.Create(rlsname, section, True, ctime)
    else
      r := rc.Create(rlsname, section);

    //r.pretime:=UnixToDateTime(ctime);
    r.kb_event := event;

    p := PazoAdd(r);

    p.added := added;
    p.stated := True;
    p.cleared := True;
    p.ExcludeFromIncfiller := True;
    kb_list.AddObject(section + '-' + rlsname, p);
  end;

begin
  // Initialize global cbftp REST client and start events thread
  if Assigned(config) then
  begin
    if SameText(Trim(config.ReadString('UDPConfig', 'EnableUDP', 'False')), 'True') or
       SameText(Trim(config.ReadString('UDPConfig', 'EnableUDP', 'False')), '1') then
    begin
      try
        cbftpIp := Trim(config.ReadString('UDPConfig', 'IP', '127.0.0.1'));
        cbftpApiPort := config.ReadInteger('UDPConfig', 'ApiPort', 0);
        if cbftpApiPort <= 0 then
          cbftpApiPort := config.ReadInteger('UDPConfig', 'Port', 55477); // fallback
        cbftpPassword := config.ReadString('UDPConfig', 'Password', '');

        cbftpclient_Init(StringToUtf8(cbftpIp), cbftpApiPort, StringToUtf8(cbftpPassword));
        Debug(dpMessage, 'kb', Format('cbftp REST client initialized globally: %s:%d', [cbftpIp, cbftpApiPort]));
        SyncSitesFromCbftp;

        // Start UDP push listener instead of HTTP long-poll
        // Use EventPushPort if configured, otherwise default to 5697 to avoid
        // conflict with cbftp's own RemoteCommandHandler on 5696
        cbftpUdpBindIp := Trim(config.ReadString('UDPConfig', 'EventPushBindIP', '127.0.0.1'));
        if cbftpUdpBindIp = '' then
          cbftpUdpBindIp := '127.0.0.1';
        cbftpUdpPort := config.ReadInteger('UDPConfig', 'EventPushPort', 5697);
        CbftpUdpEventsStart(cbftpUdpBindIp, cbftpUdpPort);
        Debug(dpMessage, 'kb', Format('cbftp UDP event listener started on %s:%d', [cbftpUdpBindIp, cbftpUdpPort]));
      except
        on E: Exception do
          DebugException(dpError, 'kb', 'cbftp global REST client/events init failed', E);
      end;
    end;
  end;

  kb_reloadsections;

  // itt kell betoltenunk az slftp.kb -t
  kb_lock.Enter('kb_start');
  try
    x := TEncStringlist.Create(passphrase);
    try
      //    Console_Addline('', 'Loading KB entries...');
      x.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'slftp.kb');
      last := Now;
      for i := 0 to x.Count - 1 do
      begin
        //Console_QueueStat(x.Count - i - 1);
        try
          AddKbPazo(x[i]);
        except
          on e: Exception do
          begin
            Debug(dpError, 'kb', Format('[EXCEPTION] AddKbPazo: %s', [e.Message]));
            exit;
          end;
        end;
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
  x: TEncStringList;
  p: TPazo;

  function GetKbPazoInfoLine(p: TPazo): String;
  const
    fSeparator: Char = #9;
  begin
    Result := Format('%s%s%s%s%d%s%d%s%s', [p.rls.section, fSeparator, p.rls.rlsname, fSeparator,
      DateTimeToUnix(p.added), fSeparator, p.rls.pretime, fSeparator, KBEventTypeToString(p.rls.kb_event)]);
  end;

begin
  kb_last_saved := Now();
  Debug(dpSpam, rsections, 'kb_Save');
  x := TEncStringList.Create(passphrase);
  try
    try
      for i := 0 to kb_list.Count - 1 do
      begin
        p := TPazo(kb_list.Objects[i]);
        if ((p <> nil) and (1 <> Pos('TRANSFER-', kb_list[i])) and
          (1 <> Pos('REQUEST-', kb_list[i])) and
          (SecondsBetween(Now, p.added) < kb_keep_entries)) then
          x.Add(GetKbPazoInfoLine(p));
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

  secs := nil;
  r := nil;
  xin := nil;
  try
    secs := TStringlist.Create;
    r := TRegexpr.Create;
    xin := Tinifile.Create(ExtractFilePath(ParamStr(0)) + 'slftp.precatcher');
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
    FreeAndNil(xin);
    FreeAndNil(r);
    FreeAndNil(secs);
  end;
  Result := True;
end;

procedure _CbftpEventHandler(const aEvent: TCbftpEvent); forward;

procedure kb_Init;
begin
  kb_last_saved := Now();

  KbReleaseInit;

  addpreechocmd := config.ReadString('dbaddpre', 'addpreechocmd', '!sitepre');

  kb_lock := TSLCriticalSection2.Create('kb_lock');
  GlRaceCompletions := TObjectDictionary<string, TList<TCbftpEvent>>.Create([doOwnsValues]);

  kb_trimmed_rls := THashedStringList.Create;
  kb_trimmed_rls.CaseSensitive := False;

  kb_list := TStringList.Create;
  kb_list.CaseSensitive := False;
  kb_list.Duplicates := dupIgnore;
  kb_list.OwnsObjects := False;

  kb_sections := TStringList.Create;
  kb_sections.Sorted := True;
  kb_sections.Duplicates := dupIgnore;

  CbftpEventsSetHandler(_CbftpEventHandler);

  rename_patterns := 4;

  kb_groupcheck_rls := THashedStringList.Create;
  kb_latest := THashedStringList.Create;
  kb_skip := THashedStringList.Create;

  trimmed_shit_checker := config.ReadBool(rsections, 'trimmed_shit_checker', True);
  renamed_group_checker := config.ReadBool(rsections, 'renamed_group_checker', True);
  renamed_release_checker := config.ReadBool(rsections, 'renamed_release_checker', True);

  enable_try_to_complete := config.ReadBool(rsections, 'enable_try_to_complete', False);
  try_to_complete_after := config.ReadInteger(rsections, 'try_to_complete_after', 450);

  kb_save_entries := config.ReadInteger(rsections, 'kb_save_entries', 0);
  kb_keep_entries := config.ReadInteger(rsections, 'kb_keep_entries', 86400 * 7);

  taskpretime_mode := config.ReadInteger('taskpretime', 'mode', 0);
  glAutoAddAffils := config.ReadBool(rsections, 'auto_add_affils', True);
  glOnlyUseRouteableSitesOnTryToComplete := config.ReadBool(rsections, 'only_use_routable_sites_on_try_to_complete', True);
end;

procedure kb_Stop;
begin
  while (kb_thread <> nil) do
    sleep(100);
end;

procedure kb_Uninit;
begin
  Debug(dpSpam, rsections, 'Uninit1');
  kb_sections.Free;
  kb_latest.Free;
  kb_skip.Free;
  kb_groupcheck_rls.Free;

  KbReleaseUninit;

  kb_lock.Free;
  FreeAndNil(GlRaceCompletions);

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
    kb_lock.Enter('AddCompleteTransfers');
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
        rank := TSpeedFromRouteInfo.CreateFromConfigString(sitesdat.ReadString('site-' + sps.Name, 'speed-from-' + ps.Name, '0')).Speed;
        if ((glOnlyUseRouteableSitesOnTryToComplete) and (rank = 0)) then
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
        rank := TSpeedFromRouteInfo.CreateFromConfigString(sitesdat.ReadString('site-' + sps.Name, 'speed-from-' + ssites_info[j], '0')).Speed;
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
  i, j: integer;
  p: TPazo;
  fIncFillPazos, fFinishedPazos, fFinishedRankCalcPazos, fDeletedPazos: TList<TPazo>;
  fIsSpecialKB, fTryToCompleteTimeReached: boolean;
begin
  fIncFillPazos := TList<TPazo>.Create;
  fFinishedPazos := TList<TPazo>.Create;
  fFinishedRankCalcPazos := TList<TPazo>.Create;
  fDeletedPazos := TList<TPazo>.Create;
  try
    while (not slshutdown) do
    begin
      try
        kb_lock.Enter('Execute');
        p := nil;
        try
          for i := kb_list.Count - 1 downto 0 do
          begin
            if i < 0 then
              Break;

            p := TPazo(kb_list.Objects[i]);
            fIsSpecialKB := kb_list[i].StartsWith('TRANSFER-') Or kb_list[i].StartsWith('REQUEST-') Or kb_list[i].StartsWith('INC-');
            fTryToCompleteTimeReached := True;

            if enable_try_to_complete and not fIsSpecialKB then
            begin
              fTryToCompleteTimeReached := (SecondsBetween(Now, p.lastTouch) >= try_to_complete_after);
              if ((not p.ExcludeFromIncfiller) and (not p.stopped) and fTryToCompleteTimeReached) then
              begin
                fIncFillPazos.Add(p);
              end;
            end;

            if ((p.ready) and (SecondsBetween(Now, p.lastTouch) > 3600) and (not p.stated) and (not p.cleared)) then
            begin
              fFinishedPazos.Add(p);
              if not fIsSpecialKB then
              begin
                fFinishedRankCalcPazos.Add(p);
              end;
            end;

            // finally if the pazo has been cleared and the time to keep it has been reached, delete it from the kb_list
            if p.stated and (fTryToCompleteTimeReached and not fIncFillPazos.Contains(p)) and ((kb_save_entries <= 0) Or (SecondsBetween(Now, p.added) > kb_keep_entries)) then
            begin
              kb_list.Delete(i);
              j := kb_latest.IndexOf(p.rls.rlsname);
              if j <> -1 then
              begin
                kb_latest.Delete(j);
              end;
              fDeletedPazos.Add(p);
            end;

          end;
        finally
          kb_lock.Leave;
        end;

        // do this outside of kb_lock because of possible long running operations (dirlist)
        for p in fIncFillPazos do
        begin
          Debug(dpSpam, rsections, 'Looking for incomplete sites of %s', [p.rls.rlsname]);
          while (not(p.queuenumber.Value <= 0)) do
          begin
            p.queuenumber.Decrease;
          end;
          p.ExcludeFromIncfiller := True;
          AddCompleteTransfers(p);
        end;
        fIncFillPazos.Clear;

        for p in fFinishedPazos do
        begin
          RemovePazo(p.pazo_id);
          if (fFinishedRankCalcPazos.Contains(p)) then
          begin
            try
              RanksProcess(p);
            except
              on e: Exception do
              begin
                Debug(dpError, rsections, Format('[EXCEPTION] TKBThread.Execute RanksProcess(p) : %s', [e.Message]));
              end;
            end;
          end;

          p.Clear;
          p.stated := True;
        end;
        fFinishedPazos.Clear;
        fFinishedRankCalcPazos.Clear;

        for p in fDeletedPazos do
        begin
          try
            p.Free;
          except
            on e: Exception do
            begin
              Debug(dpError, rsections, '[EXCEPTION] TKBThread.Execute FreePazo: %s', [e.Message]);
            end;
          end;
        end;
        fDeletedPazos.Clear;

      except
        on e: Exception do
        begin
          Debug(dpError, rsections, '[EXCEPTION] TKBThread.Execute: %s', [e.Message]);
        end;
      end;

      if ((kb_save_entries <> 0) and (SecondsBetween(Now(), kb_last_saved) > kb_save_entries)) then
      begin
        try
          kb_lock.Enter('kb_save');
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

      kbevent.WaitFor(5000);
    end;
  finally
    fIncFillPazos.Free;
    fFinishedPazos.Free;
    fFinishedRankCalcPazos.Free;
    fDeletedPazos.Free;
  end;
end;

{ cbftp event handler }
function _CompareCbftpEvents({$IFDEF FPC}constref{$ELSE}const{$ENDIF} e1, e2: TCbftpEvent): Integer;
begin
  if e1.TimeSpentSeconds < e2.TimeSpentSeconds then
    Result := -1
  else if e1.TimeSpentSeconds > e2.TimeSpentSeconds then
    Result := 1
  else
    Result := 0;
end;

procedure _CbftpEventHandler(const aEvent: TCbftpEvent);
var
  fPazo: TPazo;
  fPazoSite: TPazoSite;
  nfoData: String;
  genre: String;
  s: String;
  i: Integer;
  List: TList<TCbftpEvent>;
  Ev: TCbftpEvent;
  fSite: TSite;
  pazoId: Integer;
  sectionStr: String;
  fsize: Double;
  racebw: Double;
  speed_stat: String;
  tname: String;
  siteInfo: String;
  rank: Integer;
  js: TlkJSONbase;
  jsObj: TlkJSONObject;
  jsSites: TlkJSONlist;
  jsIncSites: TlkJSONlist;
  siteName: String;
  fsizeDummy: Int64;
  disabled: Boolean;
begin
  case aEvent.EventType of
    cetRaceStarted:
    begin
      Debug(dpMessage, rsections, Format('[cbftp] race_started: %s/%s', [aEvent.Section, aEvent.Name]));
      if GlRaceCompletions <> nil then
        GlRaceCompletions.Remove(aEvent.Name);
      fPazo := FindPazoByName(aEvent.Section, aEvent.Name);
      if fPazo <> nil then
      begin
        // cbftp has taken over routing for this release
      end;
    end;

    cetRaceProgress:
    begin
      fPazo := FindPazoByName('', aEvent.Name);
      if fPazo <> nil then
      begin
        fPazoSite := fPazo.FindSite(aEvent.Site);
        if fPazoSite <> nil then
        begin
          if aEvent.FilesDone > fPazoSite.CbftpFilesDone then
            fPazoSite.CbftpFilesDone := aEvent.FilesDone;
          fPazoSite.CbftpFilesTotal := aEvent.FilesTotal;
          if aEvent.BytesDone > fPazoSite.CbftpBytesDone then
            fPazoSite.CbftpBytesDone := aEvent.BytesDone;
          Debug(dpSpam, rsections, Format('[cbftp] progress %s on %s: %d/%d files, %d/%d bytes',
            [aEvent.Name, aEvent.Site, aEvent.FilesDone, aEvent.FilesTotal,
             aEvent.BytesDone, aEvent.BytesTotal]));
        end;
      end;
    end;

    cetRaceCompleted:
    begin
      Debug(dpMessage, rsections, Format('[cbftp] race_completed: %s on %s (%.2fs)',
        [aEvent.Name, aEvent.Site, aEvent.TimeSpentSeconds]));

      fPazo := FindPazoByName('', aEvent.Name);
      if fPazo <> nil then
      begin
        fPazoSite := fPazo.FindSite(aEvent.Site);
        if fPazoSite <> nil then
        begin
          fPazoSite.status := rssComplete;
          fPazoSite.CbftpCompletedTime := fPazo.added + (aEvent.TimeSpentSeconds / 86400.0);
        end;
      end;
      if GlRaceCompletions <> nil then
      begin
        if not GlRaceCompletions.TryGetValue(aEvent.Name, List) then
        begin
          List := TList<TCbftpEvent>.Create;
          GlRaceCompletions.Add(aEvent.Name, List);
        end;
        List.Add(aEvent);
      end;
    end;

    cetRaceDone:
    begin
      Debug(dpMessage, rsections, Format('[cbftp] race_done: %s status=%s',
        [aEvent.Name, aEvent.Status]));

      fPazo := FindPazoByName('', aEvent.Name);
      if fPazo <> nil then
      begin
        if GlCbftpClient <> nil then
        begin
          try
            s := string(GlCbftpClient.GetSpreadJob(StringToUtf8(aEvent.Name)));
            if s <> '' then
            begin
              js := TlkJSON.ParseText(s);
              if (js <> nil) and (js is TlkJSONObject) then
              begin
                jsObj := TlkJSONObject(js);
                jsSites := TlkJSONlist(jsObj.Field['sites']);
                jsIncSites := TlkJSONlist(jsObj.Field['sites_incomplete']);
                if jsSites <> nil then
                begin
                  for i := 0 to jsSites.Count - 1 do
                  begin
                    siteName := jsSites.Child[i].Value;
                    fPazoSite := fPazo.FindSite(siteName);
                    if fPazoSite <> nil then
                    begin
                      disabled := False;
                      if jsIncSites <> nil then
                      begin
                        for pazoId := 0 to jsIncSites.Count - 1 do
                        begin
                          if jsIncSites.Child[pazoId].Value = siteName then
                          begin
                            disabled := True;
                            Break;
                          end;
                        end;
                      end;

                      if not disabled then
                      begin
                        fPazoSite.status := rssComplete;
                        if fPazoSite.CbftpCompletedTime = 0 then
                          fPazoSite.CbftpCompletedTime := Now;

                        if (fPazoSite.CbftpFilesDone > 0) and (fPazoSite.CbftpFilesTotal = 0) then
                          fPazoSite.CbftpFilesTotal := fPazo.GetCountOfCachedFiles;

                        if fPazoSite.CbftpFilesTotal < fPazoSite.CbftpFilesDone then
                          fPazoSite.CbftpFilesTotal := fPazoSite.CbftpFilesDone;
                      end;
                    end;
                  end;
                end;
              end;
              if js <> nil then
                js.Free;
            end;
          except
            on E: Exception do
              Debug(dpError, rsections, Format('[cbftp] race_done REST sync error: %s', [E.Message]));
          end;
        end;

        { Fallback: ensure all participating sites have a completion time.
          If the REST call above failed, returned empty, or didn't list all
          sites, any site still without CbftpCompletedTime gets Now.
          Sites with rssNotAllowed are skipped. }
        if GlCbftpClient <> nil then
        begin
          for pazoId := 0 to fPazo.PazoSitesList.Count - 1 do
          begin
            fPazoSite := TPazoSite(fPazo.PazoSitesList[pazoId]);
            if (fPazoSite <> nil) and (fPazoSite.status <> rssNotAllowed) and (fPazoSite.CbftpCompletedTime = 0) then
            begin
              fPazoSite.CbftpCompletedTime := Now;
              if fPazoSite.status = rssAllowed then
                fPazoSite.status := rssComplete;
            end;
          end;
        end;

        s := fPazo.Stats(False, False);
        if s <> '' then
        begin
          irc_addstats(Format('<c10>[<b>STATS</b>]</c> %s %s (%d):', [fPazo.rls.section, fPazo.rls.rlsname, fPazo.GetCountOfCachedFiles]));
          irc_AddstatsB(fPazo.Stats(False, True));
        end
        else
        begin
          sectionStr := fPazo.rls.section;
          irc_Addstats(Format('<c10>[<b>STATS</b>]</c> %s <b>%s</b> : Race Done! [Status: <b>%s</b>]', [sectionStr, aEvent.Name, aEvent.Status]));
        end;

        // Update ranks when cbftp race is fully complete
        try
          RanksProcess(fPazo);
        except
          on E: Exception do
            Debug(dpError, rsections, Format('[cbftp] ranks update error: %s', [E.Message]));
        end;
      end
      else
      begin
        sectionStr := FindReleaseInLatestKBList(aEvent.Name);
        if sectionStr = '' then
          sectionStr := 'UNKNOWN';
        irc_Addstats(Format('<c10>[<b>STATS</b>]</c> %s <b>%s</b> : Race Done! [Status: <b>%s</b>]', [sectionStr, aEvent.Name, aEvent.Status]));
      end;

      if GlRaceCompletions <> nil then
        GlRaceCompletions.Remove(aEvent.Name);
    end;

    cetSpeedSample:
    begin
      Debug(dpSpam, rsections, Format('[cbftp] speed %s -> %s: %.2f Mbps (file %d bytes)',
        [aEvent.SrcSite, aEvent.DstSite, aEvent.SpeedMbps, aEvent.FileSize]));
      
      pazoId := 0;
      rank := 1;
      fPazo := FindPazoByName('', aEvent.Name);
      if fPazo <> nil then
      begin
        pazoId := fPazo.pazo_id;
        if (aEvent.Filename <> '') and (aEvent.FileSize > 0) then
        begin
          fPazo.RegisterCbftpFile(aEvent.Filename, aEvent.FileSize);
        end;

        fPazoSite := fPazo.FindSite(aEvent.DstSite);
        if fPazoSite <> nil then
        begin
          Inc(fPazoSite.CbftpFilesDone);
          Inc(fPazoSite.CbftpBytesDone, aEvent.FileSize);
        end;

        fPazoSite := fPazo.FindSite(aEvent.SrcSite);
        if fPazoSite <> nil then
        begin
          { NOTE: Do NOT set Source site to rssComplete here.
            A source site may transfer many files over time.
            It should only be marked complete by cetRaceCompleted
            (destination finished) or cetRaceDone (overall race done).
            Setting it here causes the site to disappear from the
            [CBFTP] announcement list prematurely. }
        end;
      end;

      if (aEvent.FileSize > 0) and (aEvent.SpeedMbps > 0) then
      begin
        fsize := aEvent.FileSize / 1024.0; // kB
        racebw := (aEvent.SpeedMbps / 8.0) * 1024.0; // kB/s

        if (aEvent.FileSize > 1024) then
        begin
          if (racebw > 1024) then
            speed_stat := Format('<b>%.2f</b>mB in <b>%.2f</b>s @ <b>%.2f</b>mB/s', [fsize / 1024.0, aEvent.TimeSpentSeconds, racebw / 1024.0])
          else
            speed_stat := Format('<b>%.2f</b>mB in <b>%.2f</b>s @ <b>%.2f</b>kB/s', [fsize / 1024.0, aEvent.TimeSpentSeconds, racebw]);
        end
        else
        begin
          if (racebw > 1024) then
            speed_stat := Format('<b>%.2f</b>kB in <b>%.2f</b>s @ <b>%.2f</b>mB/s', [fsize, aEvent.TimeSpentSeconds, racebw / 1024.0])
          else
            speed_stat := Format('<b>%.2f</b>kB in <b>%.2f</b>s @ <b>%.2f</b>kB/s', [fsize, aEvent.TimeSpentSeconds, racebw]);
        end;
      end
      else
        speed_stat := 'ZERO FILESIZE!';

      siteInfo := Format(' <c9>[%s -> %s]</c>', [aEvent.SrcSite, aEvent.DstSite]);
      tname := Format('<c7>[RACE]</c> #%d%s : <c10><b>%s</b></c> <c7>%s</c> <c7>(%d)</c>',
        [pazoId, siteInfo, aEvent.Name, aEvent.Filename, rank]);

      irc_Addstats(tname + ' ' + speed_stat);
      // Feed cbftp speed samples into slftp stats system
      s := FindReleaseInLatestKBList(aEvent.Name);
      if s = '' then
        s := 'UNKNOWN';
      try
        SpeedStatAdd(aEvent.SrcSite, aEvent.DstSite, aEvent.SpeedMbps, s, aEvent.Name);
        statsProcessRace(aEvent.SrcSite, aEvent.DstSite, s, aEvent.Name, aEvent.Filename, aEvent.FileSize);
      except
        on E: Exception do
          Debug(dpError, rsections, Format('[cbftp] stats write error: %s', [E.Message]));
      end;
    end;

    cetNfoAvailable:
    begin
      Debug(dpMessage, rsections, Format('[cbftp] nfo_available: %s on %s (path=%s, size=%d)',
        [aEvent.Name, aEvent.Site, aEvent.Section, aEvent.FileSize]));
      if (last_addnfo <> nil) and (last_addnfo.IndexOf(aEvent.Name) <> -1) then
      begin
        Debug(dpMessage, rsections, Format('[cbftp] NFO for %s already downloaded, skipping.', [aEvent.Name]));
        exit;
      end;
      fPazo := FindPazoByName('', aEvent.Name);
      if fPazo <> nil then
      begin
        if GlCbftpClient <> nil then
        begin
          nfoData := string(GlCbftpClient.GetFile(StringToUtf8(aEvent.Site), StringToUtf8(aEvent.Section)));
          if nfoData <> '' then
          begin
            dbaddnfo_SaveNfo(aEvent.Name, aEvent.Section, nfoData);
            // Extract genre from NFO and update release
            genre := '';
            i := Pos('genre', LowerCase(nfoData));
            if i > 0 then
            begin
              genre := Copy(nfoData, i + 5, 100);
              for i := 1 to Length(genre) do
              begin
                if CharInSet(genre[i], [#13, #10]) then
                begin
                  genre := Copy(genre, 1, i - 1);
                  Break;
                end;
                if not CharInSet(genre[i], ['a'..'z', 'A'..'Z']) then
                  genre[i] := ' ';
              end;
              while True do
              begin
                s := ReplaceText(genre, '  ', ' ');
                if s = genre then Break;
                genre := s;
              end;
              genre := Trim(genre);
            end;
            if genre <> '' then
              kb_Add('', '', aEvent.Site, fPazo.rls.section, genre, kbeUPDATE, aEvent.Name, '');
          end
          else
          begin
            Debug(dpError, rsections, Format('[cbftp] Failed to download NFO for %s from %s', [aEvent.Name, aEvent.Site]));
          end;
        end;
      end;
    end;

    cetHeartbeat:
    begin
      Debug(dpSpam, rsections, '[cbftp] heartbeat');
    end;

    cetSiteStatus:
    begin
      Debug(dpMessage, rsections, Format('[cbftp] site_status event: %s disabled=%d',
        [aEvent.Site, Ord(aEvent.Disabled)]));
      fSite := FindSiteByName('', aEvent.Site);
      if fSite <> nil then
      begin
        if aEvent.Disabled then
        begin
          if fSite.WorkingStatus <> sstMarkedAsDownByUser then
            fSite.WorkingStatus := sstMarkedAsDownByUser;
        end
        else
        begin
          if fSite.WorkingStatus <> sstUp then
            fSite.WorkingStatus := sstUp;
        end;
      end;
    end;
  end;
end;

end.
