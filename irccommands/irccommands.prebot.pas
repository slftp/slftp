unit irccommands.prebot;

interface

{ slftp prebot commands functions }
function IrcPrecmd(const netname, channel, params: String): boolean;
function IrcPredir(const netname, channel, params: String): boolean;
function IrcCheck(const netname, channel, params: String): boolean; overload;
function IrcCheck(const netname, channel, params: String; const verbose: boolean): boolean; overload;
function IrcPre(const netname, channel, params: String): boolean; overload;
function IrcPre(const netname, channel, params: String; const verbose: boolean): boolean; overload;
function IrcPretest(const netname, channel, params: String): boolean;
function IrcBatchAdd(const netname, channel, params: String): boolean;
function IrcBatchDel(const netname, channel, params: String): boolean;
function IrcDelrelease(const netname, channel, params: String): boolean;
function IrcDelAllrelease(const netname, channel, params: String): boolean;
function IrcListPreContent(const netname, channel, params: String): boolean;
function IrcSetReexamineTime(const netname, channel, params: String): boolean;
function IrcSetSkipPre(const netname, channel, params: String): boolean;

{ init/uninit functions }
procedure PrebotInit;
procedure PrebotUnInit;

implementation

uses
  SysUtils, Classes, StrUtils, DateUtils, Contnrs, SyncObjs, irc, sitesunit, configunit, dirlist, pazo,
  debugunit, queueunit, mystrings, notify, taskdel, statsunit, kb, kb.releaseinfo, taskdirlist, taskraw, slmasks, skiplists,
  rulesunit, irccommands.work, irccommands.site, irccommandsunit, Generics.Collections;

const
  section = 'irccommands.prebot';
  { section in config file }
  rrsection = 'prebot';

type
  TBatchQueueItem = record
    sitename: String;
    section: String;
    dir: String;
  end;

var
  batchqueue: TList<TBatchQueueItem>;

function IrcPrecmd(const netname, channel, params: String): boolean;
var
  sitename: String;
  s: TSite;
  precmd: String;
  section, helper: String;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  section := UpperCase(SubString(params, ' ', 2));
  precmd := mystrings.RightStr(params, length(sitename) + 1 + length(section) + 1);

  if not section.StartsWith('PRE') then
  begin
    irc_addtext(Netname, Channel, '<b><c4>Error</c></b>: Pre section <b>%s</b> is invalid. All pre sections must start with PRE.', [section]);
    exit;
  end;

  if (precmd <> '') and (0 = Pos('<rlsname>', precmd)) then
  begin
    irc_addtext(netname, channel, 'Syntax error. <rlsname> not found in precmd!');
    exit;
  end;

  s := FindSiteByName(netname, sitename);
  if s = nil then
  begin
    irc_addtext(netname, channel, 'Site <b>%s</b> not found.', [sitename]);
    exit;
  end;

  if precmd = '' then
  begin
    helper := s.sectionprecmd[section];
    s.sectionprecmd[section] := '';
    irc_addtext(Netname, Channel, 'Pre command <b>%s</b> for section <b>%s</b> removed from site <b>%s</b>', [helper, section, s.Name]);
  end
  else
  begin
    s.sectionprecmd[section] := precmd;
    irc_addtext(Netname, Channel, 'Pre command for section <b>%s</b> on site <b>%s</b> set to <b>%s</b>', [section, s.Name, precmd]);
  end;

  Result := True;
end;

function IrcPredir(const netname, channel, params: String): boolean;
var
  sitename: String;
  section: String;
  predir: String;
begin
  Result := False;

  sitename := UpperCase(SubString(params, ' ', 1));
  section  := UpperCase(SubString(params, ' ', 2));
  predir := SubString(params, ' ', 3);

  if ((predir <> '') and not section.StartsWith('PRE')) then
  begin
    irc_addtext(Netname, Channel, '<b><c4>Error</c></b>: Pre section <b>%s</b> is invalid. All pre sections must start with PRE.', [section]);
    exit;
  end;

  Result := IrcSetDir(netname, channel, Format('%s %s %s', [sitename, section, predir]));
end;

function IrcCheck(const netname, channel, params: String): boolean; overload;
begin
  Result := IrcCheck(netname, channel, params, True);
end;

function IrcCheck(const netname, channel, params: String; const verbose: boolean): boolean; overload;
var
  s: TSite;
  sttr, aksizestring, sizestring, predir, section, sitename, dir: String;
  tn: TTaskNotify;
  nfofound, added: boolean;
  sr: TSiteResponse;
  d: TDirList;
  i, failed, perfect, aktfiles, addednumber, files: integer;
  aktsize, size: Int64;
  p: TPazo;
  ps: TPazoSite;
  r: TDirListTask;
  csl: TSkipList;
  aksized, sized: double;
  checkedlist: TStringList;
begin
  Result := False;

  sitename := UpperCase(SubString(params, ' ', 1));
  section  := UpperCase(SubString(params, ' ', 2));

  queue_lock.Enter;
  try
    s := FindSiteByName(netname, sitename);
    if s = nil then
    begin
      irc_addtext(netname, channel, 'Site <b>%s</b> not found.', [sitename]);
      exit;
    end;

    predir := s.sectiondir[section];

    dir := mystrings.RightStr(params, length(sitename) + length(section) + 2);
    if ((dir = '') and (predir = '')) then
    begin
      section := 'PRE';
      predir  := s.sectiondir[section];
      dir     := mystrings.RightStr(params, length(sitename) + 1);
    end;
    if (predir = '') then
    begin
      irc_addtext(netname, channel, 'Site <b>%s</b> has no dir set for section %s.', [sitename, section]);
      exit;
    end;

    kb_Add(netname, channel, sitename, section, '', kbeCOMPLETE, dir, '', True);
    i := kb_list.IndexOf(section + '-' + dir);
    if i = -1 then
    begin
      irc_addtext(netname, channel, 'No valid kbID found for %s-%s!', [section, dir]);
      exit; // this is not possible
    end;

    p := TPazo(kb_list.Objects[i]);

    addednumber := 0;
    tn := AddNotify;
    for ps in p.PazoSitesList do
    begin
      s  := FindSiteByName(netname, ps.Name);
      if s = nil then
      begin
        if verbose then
          irc_addtext(netname, channel, '<c10><b>DEBUG</c></b> %s is not a valid site!', [ps.Name]);
        Continue;
      end;

      if s.Name = getAdminSiteName then
        Continue;

      if (s.PermDown) then
      begin
        if verbose then
          irc_addtext(Netname, Channel, '<c10><b>DEBUG</c></b> %s is perm down!', [ps.Name]);
        Continue;
      end;

      if (s.WorkingStatus in [sstMarkedAsDownByUser]) then
      begin
        if verbose then
          irc_addtext(netname, channel, '<c10><b>DEBUG</c></b> %s is marked as down!', [ps.Name]);
        Continue;
      end;

      if s.SkipPre then
      begin
        if verbose then
          irc_addtext(netname, channel, '<c8><b>INFO</c></b> %s is marked as skip pre, skipping check', [ps.Name]);
        Continue;
      end;

      if ps.status = rssNotAllowed then
      begin
        if verbose then
          irc_addtext(netname, channel, '<c8><b>INFO</c></b> %s is not allowed on %s, skipping check', [dir, ps.Name]);
        Continue;
      end;

      r := TDirlistTask.Create(netname, channel, ps.Name, MyIncludeTrailingSlash(ps.maindir) + dir);
      tn.tasks.Add(r);
      AddTask(r);
      Inc(addednumber);
    end;

    QueueFire;
  finally
    queue_lock.Leave;
  end;

  if addednumber = 0 then
  begin
    queue_lock.Enter;
    try
      RemoveTN(tn);
    finally
      queue_lock.Leave;
    end;
    exit;
  end;

  tn.event.WaitFor($FFFFFFFF);

  added := True;

  queue_lock.Enter;
  try
    csl := FindSkipList(section, False);
    if (csl = nil) then
      csl := FindSkipList('PRE', True);

    if tn.responses.Count <> addednumber then
    begin
      irc_addtext(netname, channel, 'ERROR: <c4>We got different number of dirlist responses...</c>');
      added := False;
    end;

    if (added) then
    begin
      added    := False;
      nfofound := False;
      sized:=0;
      for i := 0 to tn.responses.Count - 1 do
      begin
        sr := TSiteResponse(tn.responses[i]);
        if sr.sitename = sitename then
        begin
          added := True;
          d := TDirList.Create(sr.sitename, nil, csl, sr.response);
          try
            nfofound := d.HasNFO;
            d.UsefulFiles(files, size);
            sized := size;
            RecalcSizeValueAndUnit(sized, sizestring, 0);
          finally
            d.Free;
          end;
          Break;
        end;
      end;

      if ((files = 0) or (size = 0)) then
      begin
        irc_addtext(netname, channel, 'ERROR: <c4><b>Something is wrong: i think there are no files on src %s ...</b></c>', [sitename]);
        added := False;
      end;

      if (not nfofound) then
      begin
        irc_addtext(netname, channel, 'ERROR: <c4><b>No NFO on src %s ...</b></c>', [sitename]);
        added := False;
      end;

      if added then
      begin
        if verbose then
          irc_addtext(netname, channel, '<b>%s</b> @ <b>%s</b> is %.2f %s in %d files.', [dir, sitename, sized, sizestring, files]);

        perfect     := 0;
        failed      := 0;
        checkedlist := TStringList.Create;
        try
          for i := 0 to tn.responses.Count - 1 do
          begin
            sr := TSiteResponse(tn.responses[i]);
            if sr.sitename <> sitename then
            begin
              d := TDirList.Create(sr.sitename, nil, nil, sr.response);
              try
                d.UsefulFiles(aktfiles, aktsize);
                aksized := aktsize;
                RecalcSizeValueAndUnit(aksized, aksizestring, 0);
              finally
                d.Free;
              end;

              if ((aktfiles <> files) and verbose) then
              begin
                sttr := '';
                if aktfiles > files then
                  sttr := 'more';
                if aktfiles < files then
                  sttr := 'less';
                irc_addtext(netname, channel, '<c4><b>DEBUG</c></b>: %s (%d) has %s files than %s (%d)', [sr.sitename, aktfiles, sttr, sitename, files]);
              end;

              if ((aktsize <> size) and verbose) then
              begin
                sttr := '';
                if aktsize > size then
                  sttr := 'bigger';
                if aktsize < size then
                  sttr := 'smaller';
                irc_addtext(netname, channel, '<c4><b>DEBUG</c></b>: %s (%d) is %s than %s (%d)', [sr.sitename, aktsize, sttr, sitename, size]);
              end;

              if ((aktfiles <> files) or (aktsize <> size)) then
              begin
                irc_addtext(netname, channel,
                  'ERROR: <c4>%s</c> @ <c4><b>%s</b></c> is %.2f %s in %d files.',
                  [dir, sr.sitename, aksized, aksizestring, aktfiles]);
                added := False;
                checkedlist.Values['FAILED'] := checkedlist.Values['FAILED'] + ' ' + sr.sitename;
                Inc(failed);
              end
              else
              begin
                Inc(perfect);
                checkedlist.Values['PERFECT'] := checkedlist.Values['PERFECT'] + ' ' + sr.sitename;
              end;
            end;
          end;

          if ((perfect > 0) and (failed > 0)) then
          begin
            irc_addtext(netname, channel,
              '%s perfect on <c3><b>%d</c></b> sites and failed on <c4><b>%d</c></b> sites compared to <b>%s</b>.',
              [dir, perfect, failed, sitename]);
            irc_addtext(netname, channel, 'failed on: ' + checkedlist.Values['FAILED']);
          end
          else
          if (failed > 0) then
            irc_addtext(netname, channel,
              '<c4>%s failed on %d sites compared to</c> <b>%s</b>.', [dir, failed, sitename])
          else
          if (perfect > 0) then
            irc_addtext(netname, channel, '<c3>%s perfect on %d sites compared to</c> <b>%s</b>.', [dir, perfect, sitename]);

        finally
          checkedlist.Free;
        end;

      end;
    end;

    RemoveTN(tn);
  finally
    queue_lock.Leave;
  end;

  if added then
    Result := True;
end;

function IrcPre(const netname, channel, params: String): boolean; overload;
begin
  Result := IrcPre(netname, channel, params, True);
end;

function IrcPre(const netname, channel, params: String; const verbose: boolean): boolean; overload;
var
  s: TSite;
  dir: String;
  rc: TDirlistTask;
  rl: TDirlistTask;
  rr: TRawTask;
  tn1, tn2, tn3: TTaskNotify;
  added: boolean;
  sectiontype, section: String;
  i: integer;
  sr: TSiteResponse;
  d: TDirList;
  addednumber: integer;
  elozo: TDateTime;
  precmd: String;
  mind, maxd: TDateTime;
  mins, maxs: String;
  p: TPazo;
  ps: TPazoSite;
  sleep_value, pazo_id: integer;
begin
  Result := False;

  try
    section := UpperCase(SubString(params, ' ', 1));
    dir := SubString(params, ' ', 2);
    if (dir = '') then
    begin
      section := 'PRE';
      dir := SubString(params, ' ', 1);
    end;
  except
    on E: Exception do
      irc_addtext(netname, channel, '<c4><b>ERROR</c></b>: %s', [e.Message]);
  end;

  sectiontype := Copy(section, 4, 1000);
  if sectiontype = '' then
    sectiontype := 'MP3';

  if ((0 < Pos('../', dir)) or (0 < Pos('/..', dir))) then
  begin
    irc_addText(netname, channel, 'Syntax error.');
    exit;
  end;

  queue_lock.Enter;
  try
    pazo_id := kb_list.IndexOf(section + '-' + dir);
    if pazo_id = -1 then // this shouldnt happen
    begin
      irc_addtext(Netname, Channel, '<c4><b>ERROR</c> No pazo_id found for:</b> %s-%s', [section, dir]);
      exit;
    end;

    p := TPazo(kb_list.Objects[pazo_id]);

    for ps in p.PazoSitesList do
    begin
      s := FindSiteByName(netname, ps.Name);

      if s = nil then
      begin
        irc_addtext(netname, channel, '<c4><b>ERROR</c></b> %s is no valid site!', [ps.Name]);
        exit;
      end;

      if s.Name = getAdminSiteName then
        Continue;

      if s.SkipPre then
      begin
        if verbose then
          irc_addtext(netname, channel, '<c8><b>INFO</c></b> %s is marked as skip pre, skipping pre', [ps.Name]);
        Continue;
      end;

      if (s.WorkingStatus in [sstMarkedAsDownByUser]) then
      begin
        irc_addtext(netname, channel, '<c4><b>ERROR</c></b> %s is marked as down!', [ps.Name]);
        exit;
      end;

      if ps.status = rssNotAllowed then
      begin
        irc_addtext(netname, channel, '<c8><b>INFO</c></b> %s is not allowed on %s, skipping pre', [dir, ps.Name]);
        Continue;
      end;

      if (s.sectiondir[section] = '') then
      begin
        irc_addtext(netname, channel, '<c4><b>ERROR</c></b> Site <b>%s</b> has no predir set.', [ps.Name]);
        exit;
      end;
      if (s.sectionprecmd[section] = '') then
      begin
        irc_addtext(netname, channel, '<c4><b>ERROR</c></b> Site <b>%s</b> has no precmd set.', [ps.Name]);
        exit;
      end;
      if (s.WorkingStatus = sstUnknown) then
      begin
        irc_addtext(netname, channel, '<c4><b>ERROR</c></b> Status of site <b>%s</b> is unknown.', [ps.Name]);
        exit;
      end;
    end;

    ps := nil;
    addednumber := 0;
    tn1 := AddNotify;

    for ps in p.PazoSitesList do
    begin
      s := FindSiteByName(netname, ps.Name);

      if s.Name = getAdminSiteName then
        Continue;

      if s.SkipPre then
        continue;

      if ps.status = rssNotAllowed then
        Continue;

      if (not (s.WorkingStatus in [sstMarkedAsDownByUser])) then
      begin
        try
          rc := TDirlistTask.Create(netname, channel, ps.Name, MyIncludeTrailingSlash(s.sectiondir[section]), True);
          tn1.tasks.Add(rc);
          AddTask(rc);
          Inc(addednumber);
        except
          on E: Exception do
            irc_addtext(netname, channel, '<c4><b>ERROR</c></b>: %s', [e.Message]);
        end;
      end;
    end;

    if verbose then
      irc_addText(netname, channel, 'Changing working directory to the predir.');
    elozo := Now;

  finally
    queue_lock.Leave;
  end;

  queue_lock.Enter;
  try
    try
      QueueFire;
      tn1.event.WaitFor($FFFFFFFF);
      if verbose then
        irc_addtext(netname, channel, 'Dir change to predirs done.');
    except
      on E: Exception do
        irc_addtext(netname, channel, '<c4><b>ERROR</c></b>: %s', [e.Message]);
    end;
  finally
    queue_lock.Leave;
  end;

  queue_lock.Enter;
  try
    if tn1.responses.Count <> addednumber then
    begin
      irc_addtext(netname, channel, '<c4><b>ERROR</c></b>: <c4>We got different number of cwd responses.</c>. Should be %d, is %d.', [addednumber, tn1.responses.Count]);
      RemoveTN(tn1);
      exit;
    end;

    for i := 0 to tn1.responses.Count - 1 do
    begin
      sr := TSiteResponse(tn1.responses[i]);
      d := TDirList.Create(sr.sitename, nil, nil, sr.response);
      try
        if d.Find(dir) = nil then
        begin
          irc_Addtext(netname, channel, '<c4><b>ERROR</c></b>: Cant find %s on <c4><b>%s</b></c>!', [dir, sr.sitename]);
          RemoveTN(tn1);
          exit;
        end;
      finally
        d.Free;
      end;
    end;

    if (SecondsBetween(Now, elozo) > 20) then
    begin
      irc_addtext(netname, channel, '<c4><b>ERROR</c></b>: <c4>Changing directories to predir took too long, sorry.</c>');
      RemoveTN(tn1);
      exit;
    end;

    tn2 := AddNotify;
    for i := 0 to tn1.responses.Count - 1 do
    begin
      sr := TSiteResponse(tn1.responses[i]);
      s  := FindSiteByName(netname, sr.sitename);
      if s.SkipPre then
        Continue;

      precmd := s.sectionprecmd[section];
      precmd := ReplaceText(precmd, '<rlsname>', dir);
      precmd := ReplaceText(precmd, '<section>', sectiontype);

      rr := TRawTask.Create(netname, channel, sr.sitename, MyIncludeTrailingSlash(s.sectiondir[section]), precmd);
      rr.wantedslot := sr.slotname;
      tn2.tasks.Add(rr);
      AddTask(rr);
    end;

    RemoveTN(tn1);
    if verbose then
      irc_addtext(netname, channel, 'Sending site pre for %s', [dir]);
    QueueFire;
  finally
    queue_lock.Leave;
  end;

  tn2.event.WaitFor($FFFFFFFF);

  queue_lock.Enter;
  try
    if tn2.responses.Count <> addednumber then
    begin
      irc_addtext(netname, channel, '<c4><b>ERROR</c></b>: <c4>We got different number of pre responses. Should be %d, is %d.</c>', [addednumber, tn2.responses.Count]);
      RemoveTN(tn2);
      exit;
    end;

    maxs := '';
    mins := '';
    mind := 0;
    maxd := 0;
    for i := 0 to tn2.responses.Count - 1 do
    begin
      sr := TSiteResponse(tn2.responses[i]);
      if mind = 0 then
      begin
        mind := sr.time;
        mins := sr.sitename;
      end
      else
      if mind > sr.time then
      begin
        mins := sr.sitename;
        mind := sr.time;
      end;

      if maxd = 0 then
      begin
        maxd := sr.time;
        maxs := sr.sitename;
      end
      else
      if maxd < sr.time then
      begin
        maxd := sr.time;
        maxs := sr.sitename;
      end;
    end;

    irc_addtext(netname, channel, '<c3><b>%s</b></c> <b>%s</b> - Fastest site was %s (%s), slowest %s (%s).', [section, dir, mins, FormatDateTime('hh:nn:ss.zzz', mind), maxs, FormatDateTime('hh:nn:ss.zzz', maxd)]);

    sleep_value := config.ReadInteger(rrsection, 'predir_re_examine_time', 5);

    if verbose then
      irc_addtext(netname, channel, 'We will wait %d seconds and check the predirs again.', [sleep_value]);
    sleep_value := sleep_value * 1000;
    Sleep(sleep_value);

    tn3 := AddNotify;
    for i := 0 to tn2.responses.Count - 1 do
    begin
      sr := TSiteResponse(tn2.responses[i]);
      s  := FindSiteByName(netname, sr.sitename);
      if s.SkipPre then
        Continue;

      rl := TDirlistTask.Create(netname, channel, sr.sitename, MyIncludeTrailingSlash(s.sectiondir[section]));
      rl.wantedslot := sr.slotname;
      tn3.tasks.Add(rl);
      AddTask(rl);
    end;
    RemoveTN(tn2);
    if verbose then
      irc_addtext(netname, channel, 'Checking if %s is still in any of the predirs.', [dir]);
    QueueFire;
  finally
    queue_lock.Leave;
  end;

  tn3.event.Waitfor($FFFFFFFF);

  queue_lock.Enter;
  try
    if tn3.responses.Count <> addednumber then
    begin
      if tn3.responses.Count = 0 then
        RemoveTN(tn3);
      irc_addtext(netname, channel, 'ERROR: <c4>We got different number of dirlist responses. Should be %d, is %d</c>', [addednumber, tn3.responses.Count]);
      if tn3.responses.Count = 0 then
        exit;
    end;

    added := True;
    for i := 0 to tn3.responses.Count - 1 do
    begin
      sr := TSiteResponse(tn3.responses[i]);
      d := TDirlist.Create(sr.sitename, nil, nil, sr.response);
      try
        if d.Find(dir) <> nil then
        begin
          added := False;
          irc_addtext(netname, channel, 'ERROR: <c4><b>%s</b></c> still has %s in predir!', [sr.sitename, dir]);
        end;
      finally
        d.Free;
      end;
    end;
    RemoveTN(tn3);
  finally
    queue_lock.Leave;
  end;
  Result := added;

  if Result then
    irc_addtext(netname, channel, 'Pre successful.');
end;

function IrcPretest(const netname, channel, params: String): boolean;
var
  command, section, sectiontype, predir, dir, sitename: String;
  s: TSite;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  section := UpperCase(SubString(params, ' ', 2));
  dir := mystrings.RightStr(params, Length(sitename) + Length(section) + 2);

  queue_lock.Enter;
  try
    s := FindSiteByName(netname, sitename);
    if s = nil then
    begin
      irc_addtext(netname, channel, 'Site <b>%s</b> not found.', [sitename]);
      exit;
    end;

    predir := s.sectiondir[section];
    if predir = '' then
    begin
      dir := mystrings.RightStr(params, Length(sitename) + 1);
      section := 'PRE';
      predir := s.sectiondir[section];
    end;

    if predir = '' then
    begin
      irc_addtext(netname, channel, 'Site <b>%s</b> has no predir set.', [sitename]);
      exit;
    end;

    if s.sectionprecmd[section] = '' then
    begin
      irc_addtext(netname, channel, 'Site <b>%s</b> has no precmd set.', [sitename]);
      exit;
    end;

    command := ReplaceText(s.sectionprecmd[section], '<rlsname>', dir);
    sectiontype := Copy(section, 4, 1000);
    if sectiontype = '' then
      sectiontype := 'MP3';

    command := ReplaceText(command, '<section>', sectiontype);
  finally
    queue_lock.Leave;
  end;

  RawB(netname, channel, sitename, predir, command);

  Result := True;
end;

function IrcBatchAdd(const netname, channel, params: String): boolean;
var
  sitename, section, dir, verbosity: String;
  s: TSite;
  fInputRlsMask: TslMask;
  fDirlist: TDirList;
  fDirlistEntry: TDirListEntry;
  i: Integer;
  verbose: boolean;

  function _IrcBatch(const netname, channel: String): boolean;
  var
    ss: String;
    sitename, section, dir: String;
    i:     integer;
    site:  TSite;
    p:     TPazo;
    ps:    TPazoSite;
    item:  TBatchQueueItem;
  begin
    while (batchqueue.Count > 0) do
    begin
      queue_lock.Enter;
      try
        item := batchqueue.Extract(batchqueue.First);
      finally
        queue_lock.Leave;
      end;

      sitename := item.sitename;
      section  := item.section;
      dir      := item.dir;

      queue_lock.Enter;
      try
        i := kb_add(netname, channel, sitename, section, '', kbeNEWDIR, dir, '', True, False);
      finally
        queue_lock.Leave;
      end;

      if i = -1 then
      begin
        irc_Addtext(netname, channel, 'No Pazo found for ' + dir);
        continue;
      end;

      queue_lock.Enter;
      try
        p := TPazo(kb_list.Objects[i]);

        ss := '';
        for i := 0 to sites.Count - 1 do
        begin
          site := TSite(sites[i]);
          if site.Name = getAdminSiteName then
            Continue;

          if site.sectiondir[section] <> '' then
          begin
            if site.PermDown then
            begin
              if verbose then
                irc_addtext(netname, channel, '<c4>Site ' + site.Name + ' perm down, skipping bnctest!</c>');
              Continue;
            end;
            if (site.WorkingStatus in [sstMarkedAsDownByUser]) then
            begin
              if verbose then
                irc_addtext(netname, channel, '<c4>Site ' + site.Name + ' is marked as down, skipping bnctest!</c>');
              Continue;
            end;

            ps := p.FindSite(site.Name);
            if ((ps <> nil) and (ps.status = rssNotAllowed)) then
              continue;

            ss := ss + site.Name + ' ';
          end;
        end;
      finally
        queue_lock.Leave;
      end;

      ss := Trim(ss);
      if verbose then
        irc_Addtext(netname, channel, '%sbnctest %s', [irccmdprefix, ss]);
      IrcBnctest(netname, channel, Trim(ss));

      if verbose then
        irc_Addtext(netname, channel, '%sspread %s %s %s', [irccmdprefix, sitename, section, dir]);
      if not IrcSpread(netname, channel, Format('%s %s %s', [sitename, section, dir]), verbose) then
      begin
        irc_Addtext(netname, channel, 'ERROR: <c4>Spreading returned error. Skipping.</c>');
        continue;
      end
      else if verbose then
        irc_Addtext(netname, channel, 'Checking now....');

      if verbose then
        irc_Addtext(netname, channel, '%scheck %s %s %s', [irccmdprefix, sitename, section, dir]);
      if not IrcCheck(netname, channel, Format('%s %s %s', [sitename, section, dir]), verbose) then
      begin
        irc_Addtext(netname, channel, 'ERROR: <c4>Checking returned error. Skipping.</c>');
        continue;
      end
      else if verbose then
        irc_Addtext(netname, channel, 'preeeeing now....');

      if verbose then
        irc_Addtext(netname, channel, '%spre %s %s', [irccmdprefix, section, dir]);
      IrcPre(netname, channel, Format('%s %s', [section, dir]), verbose);
    end;

    if verbose then
      irc_Addtext(netname, channel, 'Batch queue is empty.');
    Result := True;
  end;

  // helper function to reduce code duplication
  // adds a new entry to batchqueue
  procedure AddEntryToBatchQueue;
  var
    item: TBatchQueueItem;
  begin
    queue_lock.Enter;
    try
      item.sitename := sitename;
      item.section := section;
      item.dir := dir;
      batchqueue.Add(item);
      if batchqueue.Count = 1 then
        _IrcBatch(netname, channel);
    finally
      queue_lock.Leave;
    end;
  end;

begin
  Result := False;

  sitename := UpperCase(SubString(params, ' ', 1));
  section  := UpperCase(SubString(params, ' ', 2));
  dir      := SubString(params, ' ', 3);
  verbosity  := LowerCase(SubString(params, ' ', 4));

  if ((dir = '') or (dir = '--verbose')) then
  begin
    section := 'PRE';
    dir     := SubString(params, ' ', 2);
    verbosity := LowerCase(SubString(params, ' ', 3));
  end;

  if (verbosity = '--verbose') then
    verbose := True
  else
    verbose := False;

  queue_lock.Enter;
  try
    s := FindSiteByName(netname, sitename);
    if s = nil then
    begin
      irc_Addtext(netname, channel, 'Site %s not found', [sitename]);
      exit;
    end;

    if s.sectiondir[section] = '' then
    begin
      dir     := SubString(params, ' ', 2);
      section := 'PRE';
    end;
  finally
    queue_lock.Leave;
  end;

  if (dir.IndexOfAny(['*', '?']) <> -1) then
  begin
    irc_Addtext(netname, channel, 'Doing a wildcard match for <b>%s</b> on %s/%s', [dir, sitename, section]);
    fInputRlsMask := TslMask.Create(dir);
    try
      fDirlist := DirlistB(netname, channel, sitename, s.sectiondir[section]);
      try
        if fDirlist <> nil then
        begin
          for i := 0 to fDirlist.entries.Count - 1 do
          begin
            fDirlistEntry := TDirListEntry(fDirlist.entries.Objects[i]);
            if verbose then
              irc_Addtext(netname, channel, 'Found %s in section %s', [fDirlistEntry.filename, section]);

            if fDirlistEntry.directory then
            begin
              if fInputRlsMask.Matches(fDirlistEntry.filename) then
              begin
                dir := fDirlistEntry.filename;
                irc_Addtext(netname, channel, 'Adding %s to batchqueue', [dir]);
                AddEntryToBatchQueue;
              end;
            end;
          end;
        end
        else
          irc_Addtext(netname, channel, 'Can''t dirlist section %s on %s', [section, sitename]);
      finally
        fDirlist.Free;
      end;
    finally
      fInputRlsMask.Free;
    end;
  end
  else
  begin
    AddEntryToBatchQueue;
  end;

  Result := True;
end;

function IrcBatchDel(const netname, channel, params: String): boolean;
var
  sitename, section, dir: String;
  i: integer;
  item: TBatchQueueItem;
begin
  Result := False;

  sitename := UpperCase(SubString(params, ' ', 1));
  section  := UpperCase(SubString(params, ' ', 2));
  dir      := SubString(params, ' ', 3);

  if (dir = '') then
  begin
    section := 'PRE';
    dir := SubString(params, ' ', 2);
  end;

  queue_lock.Enter;
  try
    item.sitename := sitename;
    item.section := section;
    item.dir := dir;

    i := batchqueue.IndexOf(item);
    if (i <> -1) then
    begin
      batchqueue.Delete(i);
      Result := True;
    end;
  finally
    queue_lock.Leave;
  end;

  if not Result then
    irc_Addtext(netname, channel, 'Cant find this one in the queue.');
end;

function IrcDelrelease(const netname, channel, params: String): boolean;
var
  s: TSite;
  rlsname, sitename, section, predir, dir: String;
  r: TDelreleaseTask;
  tn: TTaskNotify;
  i: integer;
  p: TPazo;
begin
  Result := False;

  rlsname := SubString(params, ' ', 3);
  section := UpperCase(SubString(params, ' ', 2));
  sitename := UpperCase(SubString(params, ' ', 1));
  try
    p := FindPazoByName(section, rlsname);
    if p <> nil then
    begin
      p.stopped := True;
      RemovePazo(p.pazo_id);
    end;
  except
    on E: Exception do
      irc_addtext(Netname, Channel, '<c4><b>ERROR</c></b>: %s', [E.Message]);
  end;

  dir := rlsname;

  try
    if ((0 < Pos('../', dir)) or (0 < Pos('/..', dir))) then
    begin
      irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
      exit;
    end; // if ((0 < Pos('../', dir)) or (0 < Pos('/..', dir))) then begin
  except
    on E: Exception do
      irc_addtext(Netname, Channel, '<c4><b>ERROR</c></b>: %s', [E.Message]);
  end;

  try
    if dir = '' then
    begin
      irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
      exit;
    end; // if dir = '' then begin
  except
    on E: Exception do
      irc_addtext(Netname, Channel, '<c><b>ERROR</c></b>: %s', [E.Message]);
  end;

  if ((sitename = '*') or (sitename = '')) then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      s := TSite(sites.Items[i]);
      if (s.Name = getAdminSiteName) then
        Continue;

      if (s.PermDown) then
        Continue;

      predir := s.sectiondir[section];
      if (predir = '') then
      begin
        irc_addtext(Netname, Channel, 'Site <b>%s</b> has no predir set.', [s.Name]);
        Continue;
      end;

      if s.WorkingStatus <> sstUp then
      begin
        irc_addtext(Netname, Channel, 'Site <b>%s</b> is not marked as up.', [s.Name]);
        Continue;
      end;

      irc_addtext(Netname, Channel, 'Adding Task for: %s', [s.Name]);
      try

        r := TDelreleaseTask.Create(Netname, Channel, s.Name, MyIncludeTrailingSlash(predir) + dir);
        tn := AddNotify;
        tn.tasks.Add(r);
        AddTask(r);
        QueueFire;

        irc_addtext(Netname, Channel, 'Firing %s @ %s ... hang on a sec bro!', [dir, s.Name]);
        tn.event.WaitFor($FFFFFFFF);
        // r.devent.WaitFor($FFFFFFFF);

        RemoveTN(tn);
        irc_addtext(Netname, Channel, 'Site %s is done!', [s.Name]);

      except
        on E: Exception do
          irc_addtext(Netname, Channel, '<c4><b>ERROR</c></b>: %s', [E.Message]);
      end;
    end;
  end
  else
  begin

    s := FindSiteByName(Netname, sitename);
    if s = nil then
    begin
      irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [sitename]);
      exit;
    end;

    predir := s.sectiondir[section];
    if (predir = '') then
    begin
      irc_addtext(Netname, Channel, 'Site <b>%s</b> has no predir set.', [sitename]);
      exit;
    end;

    if s.WorkingStatus <> sstUp then
    begin
      irc_addtext(Netname, Channel, 'Site <b>%s</b> is not marked as up.', [sitename]);
      exit;
    end;

    irc_addtext(Netname, Channel, 'Adding Task for: %s', [s.Name]);

    try
      r := TDelreleaseTask.Create(Netname, Channel, sitename, MyIncludeTrailingSlash(predir) + dir);
      tn := AddNotify;
      tn.tasks.Add(r);
      AddTask(r);
      QueueFire;
      irc_addtext(Netname, Channel, 'Firing %s @ %s ... hang on a sec bro!', [dir, s.Name]);
      tn.event.WaitFor($FFFFFFFF);

      RemoveTN(tn);
      irc_addtext(Netname, Channel, 'Site %s is done!', [s.Name]);
    except
      on E: Exception do
        irc_addtext(Netname, Channel, '<c4><b>ERROR</c></b>: %s', [E.Message]);
    end;
  end;
  Result := True;
end;

function IrcDelallrelease(const netname, channel, params: String): boolean;
var
  s: TSite;
  predir, section, sitename, dir: String;
  r: TDelreleaseTask;
  tn: TTaskNotify;
  added: boolean;
  i: integer;
  pazo_id: integer;
  p: TPazo;
  ps: TPazoSite;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
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

  if ((0 < Pos('../', dir)) or (0 < Pos('/..', dir))) then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  if dir = '' then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  if (predir = '') then
  begin
    irc_addtext(Netname, Channel,
      'Site <b>%s</b> has no dir set for section %s.', [sitename, section]);
    exit;
  end;

  pazo_id := kb_Add(Netname, Channel, sitename, section, '', kbeNEWDIR, dir, '', True);
  if pazo_id = -1 then
  begin
    exit;
  end;
  p := TPazo(kb_list.Objects[pazo_id]);
  p.Clear;
  p.AddSites;

  FireRules(p, p.FindSite(sitename));

  for ps in p.PazoSitesList do
  begin
    if (ps.Name <> sitename) then
    begin
      s := FindSiteByName(Netname, ps.Name);
      if s <> nil then
      begin
        if (s.sectiondir[section] <> '') and (s.WorkingStatus = sstUnknown) then
        begin
          irc_addtext(Netname, Channel, 'Status of site <b>%s</b> is unknown.', [s.Name]);
          exit;
        end;
      end;
    end;
  end;

  added := False;
  tn := AddNotify;
  for ps in p.PazoSitesList do
  begin
    if (ps.Name <> sitename) then
    begin
      if (ps.status <> rssNotAllowed) then
      begin
        (* ps.Clear; *)

        r := TDelreleaseTask.Create(Netname, Channel, ps.Name,
          MyIncludeTrailingSlash(ps.maindir) + dir);
        tn.tasks.Add(r);
        AddTask(r);
        added := True;
      end;
    end;
  end;

  QueueFire;

  if added then
    tn.event.WaitFor($FFFFFFFF)
  else
    irc_addtext(Netname, Channel, 'No sites found...');

  RemoveTN(tn);

  Result := True;
end;

function IrcListPreContent(const netname, channel, params: String): boolean;
var
  s:        TSite;
  ii, i:    integer;
  sitename: String;
  section:  String;
  predir:   String;
  d:        TDirlist;
  de:       TDirListEntry;
  plist:    TStringList;
begin
  Result := False;
  plist  := TStringList.Create;
  try
    sitename := UpperCase(SubString(params, ' ', 1));
    section := UpperCase(SubString(params, ' ', 2));

    if (section = '') then
      section := 'PRE';

    if sitename = '*' then
    begin
      for i := 0 to sites.Count - 1 do
      begin
        s := TSite(sites.Items[i]);
        if s.Name = getAdminSiteName then
          Continue;

        if s.PermDown then
          Continue;

        if s.SkipPre then
          Continue;

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
              if de.directory then
              begin
                plist.Values[de.filename] := plist.Values[de.filename] + ' ' + s.Name;
              end;
            end;
          end;
        finally
          d.dirlist_lock.Leave;
          d.Free;
        end;
      end;

      for i := 0 to pList.Count - 1 do
        irc_addtext(netname, channel, '%s ( %s )', [plist.Names[i], Trim(plist.ValueFromIndex[i])]);

    end
    else
    begin
      s := FindSiteByName(netname, sitename);
      if s = nil then
      begin
        irc_addtext(netname, channel, 'Site %s not found.', [sitename]);
        exit;
      end;

      predir := s.sectiondir[section];
      if predir = '' then
      begin
        irc_addtext(netname, channel, 'No valid path for section %s found on %s ', [section, s.Name]);
        exit;
      end;

      irc_addtext(netname, channel, 'Read content for %s:', [s.Name]);

      d := DirlistB(netname, channel, s.Name, MyIncludeTrailingSlash(predir));
      d.dirlist_lock.Enter;
      try
        if d <> nil then
        begin
          for ii := 0 to d.entries.Count - 1 do
          begin
            de := TDirListEntry(d.entries.Objects[ii]);
            if de.directory then
            begin
              irc_addtext(netname, channel, '%s', [de.filename]);
            end;
          end;
        end;
      finally
        d.dirlist_lock.Leave;
        d.Free;
      end;
    end;

  finally
    plist.Free;
  end;

  Result := True;
end;

function IrcSetReexamineTime(const netname, channel, params: String): boolean;
var
  ttime: integer;
begin
  Result := False;
  if params = '' then
  begin
    ttime := config.ReadInteger(rrsection, 'predir_re_examine_time', -1);
    if ttime = -1 then
      irc_addtext(netname, channel, 'No value set. So we use 5 sec' + Chr(39) + 's.')
    else
      irc_addtext(netname, channel, 'we wait ' + IntToStr(ttime) + ' sec' + Chr(39) + 's.');
    Result := True;
  end
  else
  begin
    ttime := strtointdef(params, -1);
    if ttime = -1 then
    begin
      irc_addtext(netname, channel, 'No param. found. "' + params + '" is not failed!');
      exit;
    end;
    config.WriteInteger(rrsection, 'predir_re_examine_time', ttime);
    config.UpdateFile;
    Result := True;
  end;
end;

function IrcSetSkipPre(const netname, channel, params: String): boolean;
var
  s: TSite;
  ii, i: integer;
  spstatus, sitename: String;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  spstatus := SubString(params, ' ', 2);

  if sitename = '*' then
  begin
    if spstatus = '' then
    begin
      for ii := 0 to sites.Count - 1 do
      begin
        s := TSite(sites.Items[ii]);
        if s.Name = getAdminSiteName then
          Continue;

        irc_addtext(netname, Channel, 'SkipPre status for %s: %d', [s.Name, integer(s.SkipPre)]);
      end;
      Result := True;
      Exit;
    end;

    i := StrToIntDef(spstatus, -1);

    if ((i < 0) or (i > 1)) then
    begin
      irc_addText(netname, channel, 'Syntax error.');
      exit;
    end;

    for ii := 0 to sites.Count - 1 do
    begin
      s := TSite(sites.Items[ii]);
      if s.Name = getAdminSiteName then
        Continue;
      s.skippre := boolean(i);
      irc_addtext(netname, Channel, 'SkipPre status for %s: %d', [s.Name, integer(s.SkipPre)]);
    end;

    Result := True;
    exit;
  end
  else
  begin
    if spstatus = '' then
    begin
      s := FindSiteByName(Netname, sitename);
      if s = nil then
      begin
        irc_addText(netname, channel, 'Site is not valid!');
        exit;
      end;
      irc_addtext(netname, Channel, 'SkipPre status for %s: %d', [s.Name, integer(s.SkipPre)]);
      Result := True;
      exit;
    end;
    i := StrToIntDef(spstatus, -1);

    if ((i < 0) or (i > 1)) then
    begin
      irc_addText(netname, channel, 'Syntax error.');
      exit;
    end;

    s := FindSiteByName(Netname, sitename);
    if s = nil then
    begin
      irc_addText(netname, channel, 'Site is not valid!');
      exit;
    end;

    s.skippre := boolean(i);
    irc_addtext(netname, Channel, 'SkipPre status for %s: %d', [s.Name, integer(s.SkipPre)]);
    Result := True;
  end;
end;



procedure PrebotInit;
begin
  batchqueue := TList<TBatchQueueItem>.Create;
end;

procedure PrebotUninit;
begin
  Debug(dpSpam, section, 'Uninit1');
  batchqueue.Free;
  Debug(dpSpam, section, 'Uninit2');
end;

end.
