unit irccommands.work;

interface

{ slftp work commands functions }
function IrcDirlist(const netname, channel, params: String): boolean;
function IrcAutoDirlist(const netname, channel, params: String): boolean;
function IrcLatest(const netname, channel, params: String): boolean;
function IrcSpread(const netname, channel, params: String): boolean; overload;
function IrcSpread(const netname, channel, params: String; const verbose: boolean): boolean; overload;
function IrcTransfer(const netname, channel, params: String): boolean;
function IrcPazoStop(const netname, channel, params: String): boolean;
function IrcLookup(const netname, channel, params: String): boolean;
function IrcNuke(const netname, channel, params: String): boolean;
function IrcUnNuke(const netname, channel, params: String): boolean;
function IrcShowSiteNukes(const netname, channel, params: String): boolean;
function IrcAutoNuke(const netname, channel, params: String): boolean;
function IrcCheckForExistsRip(const netname, channel, params: String): boolean;

implementation

uses
  SysUtils, Classes, math, DateUtils, Contnrs, SyncObjs, irccommandsunit, sitesunit, dirlist, pazo,
  kb, kb.releaseinfo, rulesunit, mystrings, debugunit, queueunit, notify, irc, taskrace, statsunit, nuke,
  globalskipunit, configunit, mainthread, RegExpr, taskraw, sltcp, mygrouphelpers;

const
  section = 'irccommands.work';

function IrcDirlist(const netname, channel, params: String): boolean;
var
  s: TSite;
  i: integer;
  sitename, section, sectiondir, dir: String;
  d: TDirList;
  de: TDirListEntry;
begin
  Result := False;

  sitename := UpperCase(SubString(params, ' ', 1));
  section := UpperCase(SubString(params, ' ', 2));
  dir := SubString(params, ' ', 3);

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
    exit;
  end;

  sectiondir := s.sectiondir[section];

  if (sectiondir = '') then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> has no dir set for section <b>%s</b>.', [sitename, section]);
    exit;
  end;

  if ((0 < Pos('../', dir)) or (0 < Pos('/..', dir))) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  sectiondir := DatumIdentifierReplace(sectiondir);

  d := DirlistB(Netname, Channel, sitename, MyIncludeTrailingSlash(sectiondir) + dir);
  try
    if d <> nil then
    begin
      for i := 0 to d.entries.Count - 1 do
      begin
        de := TDirListEntry(d.entries.Objects[i]);

        if de.directory then
          irc_addtext(Netname, Channel, '<b>%s</b>', [de.filename])
        else
          irc_addtext(Netname, Channel, '%s (%d)', [de.filename, de.filesize]);
      end;
    end;
  finally
    d.Free;
  end;

  Result := True;
end;

function IrcAutoDirlist(const netname, channel, params: String): boolean;
var
  sitename: String;
  status: integer;
  s: TSite;
  fNeedTaskCreate: boolean;
  sections: String;
  ss: String;
  i: integer;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  status := StrToIntDef(SubString(params, ' ', 2), -1);
  sections := UpperCase(mystrings.RightStr(params, length(sitename) + 1 +
    length(IntToStr(status)) + 1));

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site %s not found', [sitename]);
    exit;
  end;

  if (s.PermDown) then
  begin
    irc_addtext(Netname, Channel, 'Site %s is set as PermDown', [sitename]);
    Exit;
  end;

  if ((status > -1) and (status <> 0)) then
  begin
    for i := 1 to 1000 do
    begin
      ss := SubString(sections, ' ', i);
      if ss = '' then
        break;

      if s.sectiondir[ss] = '' then
      begin
        irc_addtext(Netname, Channel, 'Site %s has no %s section',
          [sitename, ss]);
        exit;
      end;
    end;
  end;

  fNeedTaskCreate := False;
  if status > -1 then
  begin
    if status <> 0 then
    begin
      if s.AutoDirlistInterval <= 0 then
        fNeedTaskCreate := True;

      s.AutoDirlistInterval := status;
      s.AutoDirlistSections := sections;
    end
    else
    begin
      s.DeleteKey('autodirlist');
      s.DeleteKey('autodirlistsections');
      s.DeleteKey('nextautodirlist');
      s.RemoveAutoDirlist;
    end;
  end;
  irc_addtext(Netname, Channel, 'Autodirlist of %s is: %d (%s)', [sitename, s.AutoDirlistInterval, s.AutoDirlistSections]);

  if fNeedTaskCreate then
    s.AutoDirlist;

  Result := True;
end;

function IrcLatest(const netname, channel, params: String): boolean;
var
  s: TSite;
  i: integer;
  sitename, section, sectiondir: String;
  d: TDirList;
  de: TDirListEntry;
  amount: integer;
begin
  Result := False;

  sitename := UpperCase(SubString(params, ' ', 1));
  section := UpperCase(SubString(params, ' ', 2));
  amount := StrToIntDef(SubString(params, ' ', 3), 10);

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
    exit;
  end;

  sectiondir := s.sectiondir[section];
  if (sectiondir = '') then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> has no dir set for section %s.', [sitename, section]);
    exit;
  end;

  if (amount <= 0) then
  begin
    irc_addtext(Netname, Channel, 'Invalid amount');
    exit;
  end;

  sectiondir := DatumIdentifierReplace(sectiondir);

  d := DirlistB(Netname, Channel, sitename, sectiondir);
  try
    if d <> nil then
    begin
      d.SortByModify;
      for i := 0 to d.entries.Count - 1 do
      begin
        if i >= amount then
          break;
        de := TDirListEntry(d.entries.Objects[i]);

        if de.directory then
        begin
          irc_addtext(Netname, Channel, '<b>%s</b>', [de.filename]);
        end
        else
          irc_addtext(Netname, Channel, '%s (%d)', [de.filename, de.filesize]);
      end;
    end;
  finally
    d.Free;
  end;

  Result := True;
end;

function IrcSpread(const netname, channel, params: String): boolean; overload;
begin
  Result := IrcSpread(netname, channel, params, True);
end;

function IrcSpread(const netname, channel, params: String; const verbose: boolean): boolean; overload;
var
  sp, s: TSite;
  ps: TPazoSite;
  ssite, predir, sitename, section, dir: String;
  lastAnn: TDateTime;
  ann: integer;
  pazo_id: integer;
  p: TPazo;
  y: TStringList;
  sdone, ssss, ss, si, sj, sss: String;
  added: boolean;
  addednumber: integer;
  dd: double;

  // y-ba belepakolja az osszes olyan siteot amibe el lehet jutni honnanbol...   -- y into it packs all of the site into which you can reach honnanbol ...
  procedure Routeable(source: String; y: TStringList);
  var
    x: TStringList;
    i: integer;
    s: TSite;
  begin
    if -1 = y.IndexOf(source) then
    begin
      y.Add(source);
      x := TStringList.Create;
      try
        sitesdat.ReadSection('speed-from-' + source, x);
        for i := 0 to x.Count - 1 do
        begin
          s := FindSiteByName('', x[i]);
          if ((s <> nil) and (s.WorkingStatus = sstUp)) then
            Routeable(x[i], y);
        end;
      finally
        x.Free;
      end;
    end;
  end;

begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  ssite := sitename;
  section := UpperCase(SubString(params, ' ', 2));

  s := FindSiteByName(Netname, sitename);

  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
    exit;
  end;

  predir := s.sectiondir[section];

  dir := mystrings.RightStr(params, length(sitename) + length(section) + 2);
  if ((dir = '') and (predir = '')) then
  begin
    section := 'PRE';
    predir := s.sectiondir[section];
    dir := mystrings.RightStr(params, length(sitename) + 1);
  end;

  if (predir = '') then
  begin
    irc_addtext(Netname, Channel,
      'Site <b>%s</b> has no dir set for section %s.', [sitename, section]);
    exit;
  end;

  if ((0 < Pos('../', dir)) or (0 < Pos('/..', dir))) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  (* Now we check the routing *)
  added := True;
  addednumber := 0;
  if 1 = Pos('PRE', section) then
    pazo_id := kb_Add(Netname, Channel, sitename, section, '', kbeSPREAD, dir, '', True)
  else
    pazo_id := kb_Add(Netname, Channel, sitename, section, '', kbeNEWDIR, dir, '', True);
  if pazo_id = -1 then
  begin
    Irc_AddText(Netname, Channel, 'Pazoid = %d', [pazo_id]);
    exit;
  end;

  p := TPazo(kb_list.Objects[pazo_id]);
  p.Clear;

  try
    p.AddSites(True);
  except
    on E: Exception do
    begin
      Irc_AddText(Netname, Channel,
        '<c4><b>ERROR</c></b>: IrcSpread.AddSites for Spread: %s',
        [e.Message]);
      Debug(dpError, section,
        Format('[EXCEPTION] IrcSpread.AddSites for Spread: %s',
        [e.Message]));
    end;
  end;

  try
    FireRules(p, p.FindSite(sitename));
  except
    on E: Exception do
    begin
      Irc_AddText(Netname, Channel,
        '<c4><b>ERROR</c></b>: IrcSpread.FireRules: %s',
        [e.Message]);
      Debug(dpError, section, Format('[EXCEPTION] IrcSpread.FireRules: %s',
        [e.Message]));
    end;
  end;

  y := TStringList.Create;
  try

    // recurrere run, so we can use y.text to check! or?
    try
      Routeable(sitename, y);
    except
      on E: Exception do
      begin
        Irc_AddText(Netname, Channel, '<c4><b>ERROR</c></b>: IrcSpread.Routeable: %s', [e.Message]);
        Debug(dpError, section, Format('[EXCEPTION] IrcSpread.Routeable: %s', [e.Message]));
      end;
    end;

    if y.Text = '' then
    begin
      irc_addtext(Netname, Channel, 'No Routeable sites found!');
      exit;
    end;

    for ps in p.PazoSitesList do
    begin
      sp := FindSiteByName('', ps.Name);

      if sp.SkipPre then
      begin
        if verbose then
          irc_addtext(Netname, Channel, '<c8><b>INFO</c></b> we skip %s for spread, skip pre is set', [sp.Name]);
        Continue;
      end;

      try
        if FireRuleSet(p, ps) = raAllow then
          ps.status := rssAllowed;
      except
        on E: Exception do
        begin
          Irc_AddText(Netname, Channel, '<c4><b>ERROR</c></b>: IrcSpread.FireRuleSet: %s', [e.Message]);
          Debug(dpError, section, Format('[EXCEPTION] IrcSpread.FireRuleSet: %s', [e.Message]));
        end;
      end;

      try
        FireRules(p, ps);
      except
        on E: Exception do
        begin
          Irc_AddText(Netname, Channel, '<c4><b>ERROR</c></b>: IrcSpread.FireRules: %s', [e.Message]);
          Debug(dpError, section, Format('[EXCEPTION] IrcSpread.FireRules: %s', [e.Message]));
        end;
      end;

      if ps.status = rssNotAllowed then
      begin
        if verbose then
          irc_addtext(netname, channel, '<c8><b>INFO</c></b> %s is not allowed on %s, skipping spread', [dir, ps.Name]);
        Continue;
      end;

      try
        s := FindSiteByName(Netname, ps.Name);
      except
        on E: Exception do
        begin
          Irc_AddText(Netname, Channel, '<c4><b>ERROR</c></b>: IrcSpread.FindSiteByName: %s', [e.Message]);
          Debug(dpError, section, Format('[EXCEPTION] IrcSpread.FindSiteByName: %s', [e.Message]));
        end;
      end;

      if s.WorkingStatus <> sstUp then
      begin
        case s.WorkingStatus of
          sstUnknown: sss := 'unknown';
          sstDown: sss := 'down';
          sstTempDown: sss := 'temp down';
          sstMarkedAsDownByUser: sss := 'marked down by user';
          sstOutOfCredits: sss := 'out of credits';
          sstOutOfSpace: sss := 'out of space';
        end;

        irc_addtext(Netname, Channel, 'Status of site <b>%s</b> is %s.', [s.Name, sss]);
      end;

      if s.WorkingStatus = sstUnknown then
      begin
        irc_addtext(Netname, Channel, 'Status of site <b>%s</b> is unknown.', [s.Name]);
        added := False;
        break;
      end;

      if ((ps.Name <> sitename) and (s.WorkingStatus = sstUp)) then
      begin
        Inc(addednumber);
        if y.IndexOf(ps.Name) = -1 then
        begin
          irc_addtext(Netname, Channel, '<b>%s</b> -> <b>%s</b> is not routeable.', [sitename, ps.Name]);
          added := False;
          break;
        end;
      end;
    end;

    if (addednumber = 0) then
    begin
      irc_addtext(Netname, Channel, 'There are no sites to spread to...');
    end;

    if not added then
    begin
      exit;
    end;

    if section.StartsWith('PRE') then
      pazo_id := kb_Add(Netname, Channel, sitename, section, '', kbeSPREAD, dir, '', False, True)
    else
      pazo_id := kb_Add(Netname, Channel, sitename, section, '', kbeNEWDIR, dir, '', False, True);

    if pazo_id = -1 then
    begin
      irc_addtext(Netname, Channel, 'Is it allowed anywhere at all?');
      exit;
    end;

    irc_addtext(Netname, Channel, 'Spread for %s has started. Type %sstop <b>%d</b> if you want.', [dir, irccmdprefix, pazo_id]);

    si := '-1';
    sj := '-1';
    sdone := '-1';

    ann := config.ReadInteger('spread', 'announcetime', 40);
    lastAnn := now();
    while (True) do
    begin
      if (slshutdown) then
        exit;
      Sleep(500);

      p := FindPazoById(pazo_id);
      if p = nil then
      begin
        irc_addtext(Netname, Channel, 'No valid Pazo found for %s', [dir]);
        exit;
      end;

      if p.stopped then
      begin
        if RemovePazo(p.pazo_id) then
          irc_addtext(Netname, Channel, 'DEBUG - Pazo Removed!')
        else
          irc_addtext(Netname, Channel, 'DEBUG - Pazo NOT Removed!');
        irc_addtext(Netname, Channel,
          'Spreading of <b>%s</b> has been stopped.', [dir]);
        Result := True;
        exit;
      end;

      if ((p.ready) or (p.readyerror)) then
      begin
        ssss := 'successfully finished.';
        if p.readyerror then
        begin
          if p.errorreason = '' then
            irc_addtext(Netname, Channel,
              '<b>%s</b> ERROR: <c4>NO ERROR MSG FOUND, SORRY!</c>', [dir])
          else
            irc_addtext(Netname, Channel, '<b>%s</b> ERROR: <c4>%s</c>',
              [dir, p.errorreason]);
          ssss := 'stopped!';
          RemovePazo(p.pazo_id);
          Result := True;
        end
        else
          Result := True;
        irc_addtext(Netname, Channel, 'Spreading of %s has been %s', [dir, ssss]);
        break;

      end;

      if ((ann <> 0) and (SecondsBetween(now, lastAnn) > ann)) then
      begin

        ps := p.FindSite(sitename);

        if ps = nil then
          irc_addtext(Netname, Channel,
            '<c4>DEBUG<b></c></b>: %s is not a valid pazo site.', [sitename]);
        if ps.dirlist = nil then
          irc_addtext(Netname, Channel,
            '<c4>DEBUG<b></c></b>: %s have no dirlist.', [sitename]);
        if ((ps <> nil) and (ps.dirlist <> nil)) then
          si := IntToStr(ps.dirlist.Done)
        else
          si := '?';

        sss := '';

        for ps in p.PazoSitesList do
        begin
          sj := '?';

          if ps.Name = ssite then
            Continue;
          if ps.Name = getAdminSiteName then
            Continue;

          if ps.dirlist = nil then
            irc_addtext(Netname, Channel,
              '<c7>DEBUG<b></c></b>: %s have no dirlist.', [ps.Name]);

          if ((ps <> nil) and (ps.dirlist <> nil)) then
          begin
            sj := IntToStr(ps.dirlist.FilesRacedByMe);
            sdone := IntToStr(ps.dirlist.Done);

            dd := ps.dirlist.SizeRacedByMe;

            RecalcSizeValueAndUnit(dd, ssss, 0);

            if sdone = si then
            begin
              ss := format('<c3>%s</c>', [ps.Name]);
              if sss = '' then
                sss := ss
              else
                sss := sss + ', ' + ss;
              Continue;
            end;

            if si = sj then
            begin
              ss := format('<c3>%s</c>', [ps.Name]);
              if sss = '' then
                sss := ss
              else
                sss := sss + ', ' + ss;
              Continue;
            end;

            if dd = 0 then
              ss := format('"<b>%s</b> (%s/%sF)"', [ps.Name, sj, si])
            else
              ss := format('"<b>%s</b> (%s/%sF in %.2f%s)"',
                [ps.Name, sj, si, dd, ssss]);

            if sss = '' then
              sss := ss
            else
              sss := sss + ', ' + ss;
          end;

        end;
        if verbose then
          IrcLineBreak(Netname, Channel, sss, AnsiChar('"'), '<b>STATUS</b>: ', 5);
        lastAnn := now();
      end;
    end;

  finally
    y.Free;
  end;
end;

function IrcTransfer(const netname, channel, params: String): boolean;
var
  srcsitename, dstsitename, srcdir, dstdir, rlsname, ftpsrcdir, ftpdstdir: String;
  srcsite, dstsite: TSite;
  p: TPazo;
  ps_src, ps_dst: TPazoSite;
  rc: TCRelease;
  rls: TRelease;
  //  pazo_id: integer;
  pd: TPazoDirlistTask;
  lastAnn: TDateTime;

  ann: integer;
  i, j, k: String;
begin
  Result := False;

  srcsitename := UpperCase(SubString(params, ' ', 1));
  dstsitename := UpperCase(SubString(params, ' ', 2));
  srcdir := SubString(params, ' ', 3);
  dstdir := SubString(params, ' ', 4);
  rlsname := SubString(params, ' ', 5);

  if ((srcsitename = '') or (dstsitename = '') or (srcdir = '') or (dstdir = '') or (rlsname = '')) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  // Check if source site is valid
  srcsite := FindSiteByName(Netname, srcsitename);
  if srcsite = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [srcsitename]);
    exit;
  end;
  if (srcsite.PermDown) then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> is perm down.', [srcsitename]);
    exit;
  end;
  if not (srcsite.WorkingStatus in [sstUnknown, sstUp]) then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> is down.', [srcsitename]);
    exit;
  end;

  // Check if destination site is valid
  dstsite := FindSiteByName(Netname, dstsitename);
  if dstsite = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [dstsitename]);
    exit;
  end;
  if (dstsite.PermDown) then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> is perm down.', [dstsitename]);
    exit;
  end;
  if not (dstsite.WorkingStatus in [sstUnknown, sstUp]) then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> is down.', [dstsitename]);
    exit;
  end;

  // Decide whether the supplied source dir is a direct path or a section
  if ((1 = AnsiPos('/', srcdir)) or (length(srcdir) = LastDelimiter('/', srcdir))) then
  begin
    ftpsrcdir := srcdir;
    irc_addtext(Netname, Channel, '<c14><b>%s</b> is a path.</c>', [srcdir]);
  end
  else
  begin
    srcdir := UpperCase(srcdir);
    ftpsrcdir := srcsite.sectiondir[srcdir];
    irc_addtext(Netname, Channel, '<c14><b>%s</b> is a slftp section.</c>', [srcdir]);
  end;

  // Decide whether the supplied destination dir is a direct path or a section
  if ((1 = AnsiPos('/', dstdir)) or (length(dstdir) = LastDelimiter('/', dstdir))) then
  begin
    ftpdstdir := dstdir;
    irc_addtext(Netname, Channel, '<c14><b>%s</b> is a path.</c>', [dstdir]);
  end
  else
  begin
    dstdir := UpperCase(dstdir);
    ftpdstdir := dstsite.sectiondir[dstdir];
    irc_addtext(Netname, Channel, '<c14><b>%s</b> is a slftp section.</c>', [dstdir]);
  end;

  // Check if source or destination dir is a SECTION but dir is not set
  if (ftpsrcdir = '') then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> has no dir set for section <b>%s</b>.',
      [srcsitename, srcdir]);
    exit;
  end;
  if (ftpdstdir = '') then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> has no dir set for section <b>%s</b>.',
      [dstsitename, dstdir]);
    exit;
  end;

  // The fun begins
  rc := FindSectionHandler(srcdir); //srcdir is our "section"
  rls := rc.Create(rlsname, srcdir);
  p := PazoAdd(rls);
  //  pazo_id := p.pazo_id;
  kb_list.AddObject('TRANSFER-' + IntToStr(RandomRange(10000000, 99999999)), p);

  p.AddSite(srcsite.Name, ftpsrcdir, False);
  p.AddSite(dstsite.Name, ftpdstdir, False);

  ps_src := p.FindSite(srcsite.Name);
  ps_src.AddDestination(dstsite.Name, 200);

  ps_dst := p.FindSite(dstsite.Name);
  ps_dst.status := rssAllowed;

  ps_src := TPazoSite(p.PazoSitesList[0]);
  ps_src.dirlist.dirlistadded := True;

  pd := TPazoDirlistTask.Create(Netname, Channel, ps_src.Name, p, '', False, False);
  AddTask(pd);
  QueueFire;

  irc_addtext(Netname, Channel,
    'File Transfer has started. Type <c4>%sstop <b>%d</b></c> if you need.', [irccmdprefix,
    p.pazo_id]);

  ann := config.ReadInteger('spread', 'announcetime', 60);
  lastAnn := Now();

  while (True) do
  begin
    if (slshutdown) then
    begin
      Result := False;
      exit;
    end;

    Sleep(1000);

    //[STATS] output was initiated by dirlist
    if p.ready then
    begin

      if ps_dst.dirlist.FilesRacedByMe <> 0 then
      begin
        // do nothing
      end
      else
      begin
        irc_addtext(netname, channel, '%s was already <c10>COMPLETE</c> on %s', [rlsname,
          dstsite.Name]);
      end;

      break;
    end;

    if p.stopped then
    begin
      irc_addtext(Netname, Channel, 'File Transfer of <b>%s</b> has <c4>stopped</c>.',
        [rlsname]);
      Result := True;
      exit;
    end;

    if p.readyerror then
    begin
      if p.errorreason = '' then
        irc_addtext(Netname, Channel, '<b>%s</b> ERROR: <c4>NO ERROR MSG FOUND, SORRY!</c>',
          [rlsname])
      else
        irc_addtext(Netname, Channel, '<b>%s</b> ERROR: <c4>%s</c>', [rlsname, p.errorreason]);

      irc_addtext(netname, channel, '%s STATS until ERROR: %s - %s', [rlsname, p.Stats(TRUE),
        p.StatusText]);
      Result := False;
      exit;
    end;

    if ((ann <> 0) and (SecondsBetween(Now, lastAnn) > ann)) then
    begin
      i := '?';
      if ((ps_src <> nil) and (ps_src.dirlist <> nil)) then
        i := IntToStr(ps_src.dirlist.Done);

      j := '?';
      k := '?';
      if ((ps_dst <> nil) and (ps_dst.dirlist <> nil)) then
      begin
        j := IntToStr(ps_dst.dirlist.FilesRacedByMe);

        k := IntToStr(ps_dst.dirlist.Done);
      end;

      irc_addtext(Netname, Channel,
        '%s: %s (%s)/%s files done. Type <c4>%sstop <b>%d</b></c> if you want.', [rlsname, j,
        k,
          i, irccmdprefix, p.pazo_id]);
      lastAnn := Now();
    end;

  end;

  Result := True;
end;

function IrcPazoStop(const netname, channel, params: String): boolean;
var
  p: TPazo;
  r: TRegExpr;
  fIsID: Boolean;
  fPazoID: integer;
  fRlsname: String;
begin
  Result := False;
  fIsID := False;

  r := TRegExpr.Create;
  try
    r.Expression := '^\d+$';
    if r.Exec(params) then
      fIsID := True;
  finally
    r.Free;
  end;

  if fIsID then
  begin
    fPazoID := StrToIntDef(params, -1);
    if fPazoID = -1 then
    begin
      irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
      exit;
    end;

    p := FindPazoById(fPazoID);
    if p <> nil then
    begin
      p.stopped := True;
      Result := RemovePazo(p.pazo_id);
    end
    else
    begin
      irc_addtext(Netname, Channel, 'No Pazo found for ID: <b>%d</b>', [fPazoID]);
      Result := True;
      exit;
    end;
  end
  else
  begin
    fRlsname := params;

    p := FindPazoByRls(fRlsname);
    if p <> nil then
    begin
      p.stopped := True;
      Result := RemovePazo(p.pazo_id);
    end
    else
    begin
      irc_addtext(Netname, Channel, 'No Pazo found for Releasename: <b>%d</b>', [fRlsname]);
      Result := True;
      exit;
    end;
  end;
end;

function IrcLookup(const netname, channel, params: String): boolean;
var
  sitename, section, dir: String;
  p: TPazo;
  ps: TPazoSite;
  i: integer;
begin
  Result := False;

  i := -1;
  section := UpperCase(SubString(params, ' ', 1));

  if kb_sections.IndexOf(section) <> -1 then
  begin
    sitename := '';
    dir := SubString(params, ' ', 2);
  end
  else
  begin
    sitename := section;
    section := UpperCase(SubString(params, ' ', 2));

    if kb_sections.IndexOf(section) = -1 then
    begin
      irc_addtext(Netname, Channel, '<b><c4>Error</c></b>: Section <b>%s</b> not found. Hint: Section <b>%s</b> must be in your <b>slftp.precatcher</b> file at [sections] and/or [mappings].', [section, section]);
      exit;
    end;

    dir := SubString(params, ' ', 3);
  end;

  if ((dir = '') or (dir = section) or (sitename = dir)) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Error</c></b>: No valid Rip found!');
    exit;
  end;

  if CheckIfGlobalSkippedGroup(dir) then
    irc_addtext(Netname, Channel, '<c4><b>Error</c></b>: Skipped group found...');

  try
    i := kb_Add(Netname, Channel, sitename, section, '', kbeNEWDIR, dir, '', True);
  except
    on E: Exception do
    begin
      irc_addtext(Netname, Channel, format('[EXCEPTION] IrcLookup_kb_add : %s', [E.Message]));
      exit;
    end;
  end;

  if i <> -1 then
  begin
    p := TPazo(kb_list.Objects[i]);
    p.AddSites;
    p.rls.aktualizalva := False;
    if sitename <> '' then
    begin
      ps := p.FindSite(sitename);
      if ps <> nil then
        ps.lookupforcedhere := True;
    end;
  end
  else
  begin
    irc_addtext(Netname, Channel, 'Cant find');
    exit;
  end;

  Result := True;
end;

function IrcNuke(const netname, channel, params: String): boolean;
var
  i, t, h, multiplier: integer;
  datestamp: String;
  reason, sitename, rip, section, yyyy, yy, mm, dd: String;
  site: TSite;
  n: TNukeQueueItem;
begin
  Result := False;
  i := 0; //< position of date value in string
  h := 0; //< position of first reason character
  sitename := UpperCase(SubString(params, ' ', 1));

  if nil <> FindSiteByName(Netname, sitename) then
  begin
    i := 3;
    Inc(h, length(sitename) + 1);
    section := UpperCase(SubString(params, ' ', 2));
  end
  else
  begin
    i := 2;
    section := sitename;
    sitename := '';
  end;

  if kb_sections.IndexOf(section) = -1 then
  begin
    irc_addtext(Netname, Channel, 'Section %s not found', [section]);
    exit;
  end;

  Inc(h, length(section) + 1);

  dd := '';
  datestamp := SubString(params, ' ', i);
  if length(datestamp) = 10 then
  begin
    if ((datestamp[5] = '-') and (datestamp[8] = '-')) then
    begin
      yyyy := Copy(datestamp, 1, 4);
      if StrToIntDef(yyyy, 1990) > 1990 then
      begin
        yy := Copy(yyyy, 3, 2);
        mm := Copy(datestamp, 6, 2);
        t := StrToIntDef(mm, 0);
        if ((t >= 1) and (t <= 12)) then
        begin
          dd := Copy(datestamp, 9, 2);
          t := StrToIntDef(dd, 0);
          if (t >= 1) and (t <= 31) then
          begin
            Inc(h, 10 + 1);
            Inc(i);
          end
          else
            dd := '';
        end;
      end;
    end;
  end;

  if dd = '' then
  begin
    yyyy := IntToStr(YearOf(now));
    yy := Copy(yyyy, 3, 2);
    mm := format('%.2d', [MonthOf(now)]);
    dd := format('%.2d', [DayOf(now)]);
  end;

  rip := SubString(params, ' ', i);
  Inc(i);
  Inc(h, length(rip) + 1);
  multiplier := StrToIntDef(SubString(params, ' ', i), 0);
  Inc(h, length(SubString(params, ' ', i)) + 1);
  reason := Copy(params, h + 1, 1000);

  for i := 0 to sites.Count - 1 do
  begin
    site := nil;
    site := TSite(sites[i]);

    if site = nil then
      Continue;
    if site.Name = getAdminSiteName then
      Continue;

    if site.IsAffil(GetGroupname(rip)) then
    begin
      irc_addtext(Netname, Channel, '<b>%s</b> is affil on %s - we dont nuke affil!', [GetGroupname(rip), site.Name]);
      Continue;
    end;

    if ((sitename = '') or (site.Name = sitename)) then
    begin
      n := TNukeQueueItem.Create;
      n.site := site.Name;
      n.section := section;
      n.yyyy := yyyy;
      n.yy := yy;
      n.mm := mm;
      n.dd := dd;
      n.rip := rip;
      n.multiplier := multiplier;
      n.reason := reason;

      NukeQueue.Add(n);

      if sitename <> '' then
        break;
    end;
  end;

  NukeSave;

  Result := True;
end;

function IrcUnNuke(const netname, channel, params: String): boolean;
var
  i, t, h: integer;
  datestamp: String;
  reason, sitename, rip, section, yyyy, yy, mm, dd: String;
  n: TNukeQueueItem;
begin
  Result := False;
  h := 0;
  sitename := UpperCase(SubString(params, ' ', 1));
  if nil <> FindSiteByName(Netname, sitename) then
  begin
    i := 3;
    Inc(h, length(sitename) + 1);
    section := UpperCase(SubString(params, ' ', 2));
  end
  else
  begin
    i := 2;
    section := sitename;
    sitename := '';
  end;

  if kb_sections.IndexOf(section) = -1 then
  begin
    irc_addtext(Netname, Channel, 'Section %s not found', [section]);
    exit;
  end;

  Inc(h, length(section) + 1);

  dd := '';
  datestamp := SubString(params, ' ', i);
  if length(datestamp) = 10 then
  begin
    if ((datestamp[5] = '-') and (datestamp[8] = '-')) then
    begin
      yyyy := Copy(datestamp, 1, 4);
      if StrToIntDef(yyyy, 1990) > 1990 then
      begin
        yy := Copy(yyyy, 3, 2);
        mm := Copy(datestamp, 6, 2);
        t := StrToIntDef(mm, 0);
        if ((t >= 1) and (t <= 12)) then
        begin
          dd := Copy(datestamp, 9, 2);
          t := StrToIntDef(dd, 0);
          if (t >= 1) and (t <= 31) then
          begin
            Inc(h, 10 + 1);
            Inc(i);
          end
          else
            dd := '';
        end;
      end;
    end;
  end;

  if dd = '' then
  begin
    yyyy := IntToStr(YearOf(now));
    yy := Copy(yyyy, 3, 2);
    mm := format('%.2d', [MonthOf(now)]);
    dd := format('%.2d', [DayOf(now)]);
  end;

  rip := SubString(params, ' ', i);
  Inc(h, length(rip) + 1);
  reason := Copy(params, h + 1, 1000);

  for i := 0 to sites.Count - 1 do
  begin
    if ((sitename = '') or (TSite(sites[i]).Name = sitename)) then
    begin
      n := TNukeQueueItem.Create;
      n.site := TSite(sites[i]).Name;
      n.section := section;
      n.yyyy := yyyy;
      n.yy := yy;
      n.mm := mm;
      n.dd := dd;
      n.rip := rip;
      n.multiplier := -1;
      n.reason := reason;

      NukeQueue.Add(n);
    end;
  end;

  NukeSave;

  Result := True;
end;

function IrcShowSiteNukes(const netname, channel, params: String): boolean;
var
  sitename, ss: String;
  Count: integer;
  r: TRegexpr;
  site: TSite;

  function RawC(const Netname, Channel: String; sitename, dir, command: String; AnnounceSitename: boolean = False): String;
  var
    r: TRawTask;
    tn: TTaskNotify;
    i: integer;
    ss: String;

  begin
    r := TRawTask.Create(Netname, Channel, sitename, dir, command);
    tn := AddNotify;
    tn.tasks.Add(r);
    AddTask(r);
    QueueFire;

    tn.event.WaitFor($FFFFFFFF);

    Result := '';
    if tn.responses.Count = 1 then
    begin
      i := 1;
      while (True) do
      begin
        ss := SubString(TSiteResponse(tn.responses[0]).response, slEOL, i);
        if ss = '' then
          break;
        if AnnounceSitename then
          Result := Result + Format('<b>%s</b>: %s %s', [sitename, ss, #10#13])
        else
          Result := Result + ss + #10#13;
        Inc(i);
      end;
    end;
    RemoveTN(tn);
  end;

begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  Count := StrToIntDef(SubString(params, ' ', 2), 150);

  site := FindSiteByName(Netname, sitename);
  if site = nil then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
    exit;
  end;

  if site.PermDown then
  begin
    irc_addtext(Netname, Channel, 'Site <b>%s</b> is set perm down.', [site.Name]);
    exit;
  end;

  if (site.WorkingStatus in [sstUnknown, sstDown]) then
  begin
    TSiteSlot(site.slots.Items[site.slots.Count - 1]).ReLogin();
    irc_addtext(Netname, Channel, 'Site <b>%s</b> is offline do a bnctest.... hand a sec!', [site.Name]);
  end;

  if site.GetSw <> sswGlftpd then
  begin
    irc_addtext(Netname, Channel, 'This command is currently only for GrayLine FTPD%ss.', [Chr(39)]);
    exit;
  end;

  try
    ss := RawC(Netname, Channel, site.Name, '', 'site nukes ' + IntToStr(Count));
  except
    on E: Exception do
    begin
      irc_addtext(Netname, Channel, '<c4>[Exception]</c> in IrcShowSiteNukes; %s', [E.Message]);
      Exit;
    end;
  end;

  r := TRegexpr.Create;
  //  r.ModifierS := False;
  //  r.ModifierG := False;
  try
    r.Expression := 'foo nukes';
    r.ModifierI := True;

    if r.Exec(ss) then
    begin
      irc_addtext(Netname, Channel, 'Sorry not compatible with tur-nukes');
      Result := False;
    end
    else
    begin
      r.Expression := '200- ';
      ss := r.Replace(ss, '', False);

      r.Expression := Format(
        '\|\s*%s\s*\|\s*(\d+)[xX]\s*([\d,.]+[Mm]?)\s*\|(.*?)\|[\r\n\s]+.*?\|\s*Age\:(.*?)\|\s*Dir\:(.*?)\s*\|',
        [site.UserName]);

      if not r.Exec(ss) then
        irc_addtext(Netname, Channel, 'No Nukes found, good boy!')
      else
        repeat
          irc_addtext(Netname, Channel, '%s x%s for: %s (%sM) %s ago.',
            [Trim(r.Match[5]), Trim(r.Match[1]), Trim(r.Match[3]), Trim(r.Match[2]), Trim(r.Match[4])]);
        until not r.ExecNext;

      Result := True;
    end;

  finally
    r.Free;
  end;

end;

function IrcAutoNuke(const netname, channel, params: String): boolean;
var
  sitename: String;
  status: integer;
  s: TSite;
  kell: boolean;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  status := StrToIntDef(SubString(params, ' ', 2), -1);

  s := FindSiteByName(Netname, sitename);
  if s = nil then
  begin
    irc_addtext(Netname, Channel, 'Site %s not found', [sitename]);
    exit;
  end;
  if (s.PermDown) then
  begin
    irc_addtext(Netname, Channel, 'Site %s is set as PermDown', [sitename]);
    Exit;
  end;

  kell := False;
  if status > -1 then
  begin
    if status <> 0 then
    begin
      if s.AutoNukeInterval <= 0 then
        kell := True;
      s.AutoNukeInterval := status;
    end
    else
    begin
      s.DeleteKey('autonuke');
      s.DeleteKey('nextautonuke');
      s.RemoveAutoNuke;
    end;
  end;
  irc_addtext(Netname, Channel, 'Autonuke of %s is: %d', [sitename, s.AutoNukeInterval]);

  if kell then
    s.AutoNuke;

  Result := True;
end;

function IrcCheckForExistsRip(const netname, channel, params: String): boolean;
var
  SOK, SBAD, predir: String;
  section, rip: String;
  s:     TSite;
  ii, i: integer;
  d:     TDirlist;
  de:    TDirListEntry;
begin
  Result := False;
  SOK    := '';
  SBAD   := '';

  section := UpperCase(SubString(params, ' ', 1));
  rip := SubString(params, ' ', 2);

  if (rip = '') then
  begin
    rip := SubString(params, ' ', 1);
    section := 'PRE';
  end;

  for i := 0 to sites.Count - 1 do
  begin
    s := TSite(sites.Items[i]);

    if s.Name = getAdminSiteName then
      continue;

    if s.SkipPre then
    begin
      irc_addtext(netname, channel, '<c8><b>INFO</c></b>: Skip check for %s because skip pre is set.', [s.Name]);
      continue;
    end;

    predir := s.sectiondir[section];
    if predir = '' then
      Continue;

    d := DirlistB(netname, channel, s.Name, MyIncludeTrailingSlash(predir));
    d.dirlist_lock.Enter;
    try
      if d <> nil then
      begin
        for ii := 0 to d.entries.Count - 1 do
        begin
          de := TDirListEntry(d.entries.Objects[ii]);
          if ((de.directory) and (de.filename = rip)) then
          begin
            if SOK = '' then
              SOK := s.Name
            else
              SOK := SOK + ', ' + s.Name;
          end
          else
          begin
            if SBAD = '' then
              SOK  := s.Name
            else
              SBAD := SBAD + ', ' + s.Name;
          end;
        end;
      end;
    finally
      d.dirlist_lock.Leave;
      d.Free;
    end;
  end;

  if (SOK <> '') then
    irc_addtext(netname, channel, '<c3>Sites with rip %s</c> %s', [rip, Trim(SOK)]);
  if (SBAD <> '') then
    irc_addtext(netname, channel, '<c4>Sites without rip %s</c> %s', [rip, Trim(SBAD)]);

  Result := True;
end;

end.
