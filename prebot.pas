unit prebot;

interface

uses
  Classes, irc;

function IrcLame(const netname, channel: String; params: String): boolean;
function IrcPretest(const netname, channel: String; params: String): boolean;
function IrcPredir(const netname, channel: String; params: String): boolean;
function IrcPrecmd(const netname, channel: String; params: String): boolean;
function IrcPre(const netname, channel: String; params: String): boolean;
function IrcCheck(const netname, channel: String; params: String): boolean;
function IrcBatchAdd(const netname, channel: String; params: String): boolean;
function IrcBatchDel(const netname, channel: String; params: String): boolean;

function IrcListPreContent(const netname, channel: String; params: String): boolean;

function IrcSetReexamineTime(const netname, channel: String; params: String): boolean;

function IrcSetSkipPre(const netname, channel: String; params: String): boolean;


function IrcCheckForExistsRip(const netname, channel: String; params: String): boolean;


procedure PrebotInit;
procedure PrebotUnInit;

implementation

uses
  Contnrs, SyncObjs, DateUtils, debugunit, ircblowfish, SysUtils, StrUtils, kb, configunit, queueunit,
  mystrings, sitesunit, pazo, notify, tasklame, taskdirlist, skiplists,
  taskraw, dirlist, irccommandsunit, statsunit, slmasks;

var
  batchqueue: TStringList;
  webtags:    TStringList;

const
  section   = 'irccommands';
  rrsection = 'prebot';


function IrcSetSkipPre(const netname, channel: String; params: String): boolean;
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

    if i = -1 then
    begin
      irc_addText(netname, channel, 'Syntax error.');
      exit;
    end;

    if i > 1 then
    begin
      irc_addText(netname, channel, 'Syntax error.');
      exit;
    end;

    if i < 0 then
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
    Exit;

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

    if i = -1 then
    begin
      irc_addText(netname, channel, 'Syntax error.');
      exit;
    end;

    if i > 1 then
    begin
      irc_addText(netname, channel, 'Syntax error.');
      exit;
    end;

    if i < 0 then
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

function IrcSetReexamineTime(const netname, channel: String; params: String): boolean;
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


function IrcLame(const netname, channel: String; params: String): boolean;
var
  s: TSite;
  i: integer;
  sitename, section, predir, dir: String;
  d: TDirlist;
  de: TDirListEntry;
  added: boolean;
  l: TLameTask;
  tn: TTaskNotify;
  addednumber: integer;
  sr: TSiteResponse;
  genre: String;
  genremode: boolean;
  pazo_id: integer;
  p: TPazo;
begin
  Result := False;

  sitename := UpperCase(SubString(params, ' ', 1));
  section := UpperCase(SubString(params, ' ', 2));
  genre := '';

  //really needed?
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
      predir := s.sectiondir[section];
      dir := mystrings.RightStr(params, length(sitename) + 1);
    end;

    genremode := 'genre' = SubString(dir, ' ', 2);
    dir := SubString(dir, ' ', 1);

    if ((0 < Pos('../', dir)) or (0 < Pos('/..', dir))) then
    begin
      irc_addText(netname, channel, 'Syntax error.');
      exit;
    end;

    if (predir = '') then
    begin
      irc_addtext(netname, channel, 'Site <b>%s</b> has no predir set.', [sitename]);
      exit;
    end;

    p := nil;
    pazo_id := kb_add(netname, channel, sitename, section, genre, 'NEWDIR', dir, '', True);
    if pazo_id <> -1 then
      p := FindPazoById(pazo_id);

  //really needed?
  finally
    queue_lock.Leave;
  end;

  d := DirlistB(netname, channel, sitename, MyIncludeTrailingSlash(predir) + dir);
  try
    if d <> nil then
    begin
      added := False;
      addednumber := 0;
      queue_lock.Enter;
      d.dirlist_lock.Enter;
      try
        tn := AddNotify;
        for i := 0 to d.entries.Count - 1 do
        begin
          de := TDirListEntry(d.entries[i]);
          if ((not de.directory) and (de.Extension = '.mp3')) then
          begin
            if p <> nil then
              p.PRegisterFile('', de.filename, de.filesize);

            added := True;
            Inc(addednumber);
            if ((not genremode) or (addednumber = 1)) then
            begin
              l := TLameTask.Create(netname, channel, sitename, MyIncludeTrailingSlash(predir) + dir, de.filename, de.filesize, genremode);
              l.announce := de.filename + ' lame ready';
              tn.tasks.Add(l);
              AddTask(l);
            end;

          end;
        end;
        //d.Free;
        QueueFire;
      finally
        d.dirlist_lock.Leave;
        queue_lock.Leave;
      end;

      if added then
      begin
        tn.event.WaitFor($FFFFFFFF);

        queue_lock.Enter;
        try
          if ((tn.responses.Count = addednumber) or (genremode)) then
          begin
            for i := 0 to tn.responses.Count - 1 do
            begin
              sr := TSiteResponse(tn.responses[i]);
              if not genremode then
              begin
                if ((1 <> Pos('LAME3.97.0 vbr mtrh V2 Joint 32 18.6 4 1 0 0 / ID3v1.1', sr.response)) or (0 = Pos('/ ID3v2', sr.response))) then
                begin
                  irc_addtext(netname, channel, 'ERROR: <c4>%s</c>', [sr.response]);
                  added := False;
                end;
              end;
              genre := Copy(SubString(sr.response, ' / ', 2), 9, 1000);
            end;

            if added then
            begin
              {
              genres.Values[dir] := genre;
              fajldb.Values[dir] := IntToStr(osszesfajl);
              fajlmeret.Values[dir] := IntToStr(osszesmeret);
              }

              p.rls.Aktualizald(genre);

              if not genremode then
                irc_addtext(netname, channel, 'LAME and ID3v1.1 are perfect in %d files, genre is %s.', [addednumber, genre]);
              Result := True;
            end;
          end
          else
          begin
            irc_addtext(netname, channel, 'ERROR: %s', ['<c4>different number of responses...</c>']);
          end;

          RemoveTN(tn);
        finally
          queue_lock.Leave;
        end;
      end
      else
        irc_addtext(netname, channel, 'ERROR: %s', ['<c4>No mp3 files found.</c>']);
    end
    else
      irc_addtext(netname, channel, 'ERROR: %s', ['<c4>couldnt dirlist...</c>']);

  finally
    d.Free;
  end;

end;

function IrcPredir(const netname, channel: String; params: String): boolean;
var
  sitename: String;
  predir: String;
begin
  sitename := UpperCase(SubString(params, ' ', 1));
  predir := SubString(params, ' ', 2);

  Result := IrcSetDir(netname, channel, sitename + ' PRE ' + predir);
end;

function IrcPrecmd(const netname, channel: String; params: String): boolean;
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

  if kb_sections.IndexOf(section) = -1 then
  begin
    irc_addtext(Netname, Channel, '<b><c4>Error</c></b>: Section <b>%s</b> not found. Hint: Section <b>%s</b> must be in your <b>slftp.precatcher</b> file at [sections] and/or [mappings].', [section, section]);
    exit;
  end;

  if (0 = Pos('<rlsname>', precmd)) then
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

function IrcPre(const netname, channel: String; params: String): boolean;
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
  ripper: String;
  genre: String;
  p: TPazo;
  ps: TPazoSite;
  sleep_value, pazo_id: integer;

  procedure SetupGenre;
  begin
    genre := '';
    if p.rls is TMP3Release then
      genre := TMP3Release(p.rls).mp3genre;
  end;

begin
  Result := False;
  //  exit;// egyelore tesztelunk nem preelunk

  try
    section := UpperCase(SubString(params, ' ', 1));
    // instead of using Pos() equal comparison is needed to avoid detecting releasenames (e.g. PreMe.S01E01.HDTV.x264-TEST) as section
    if (section = 'PRE') then
    begin
      dir := SubString(params, ' ', 2);
      ripper := SubString(params, ' ', 3);
    end
    else
    begin
      section := 'PRE';
      dir := SubString(params, ' ', 1);
      ripper := SubString(params, ' ', 2);
    end;
  except
    on E: Exception do
      irc_addtext(netname, channel, '<c4><b>ERROR</c></b>: %s', [e.Message]);
  end;

  sectiontype := Copy(section, 4, 1000);
  if sectiontype = '' then
    sectiontype := 'MP3';


  if ripper = 'AP' then
  begin
    irc_addText(netname, channel, 'Syntax error.');
    exit;
  end;
  if ((config.ReadBool('prebot', 'use_regpredb', False)) and (ripper = '')) then
  begin
    irc_addText(netname, channel, 'Who is the fish?');
    exit;
  end;

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

    for i := 0 to p.sites.Count - 1 do
    begin
      ps := TPazoSite(p.sites[i]);
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
        irc_addtext(netname, channel, '<c7><b>INFO</c></b> %s is marked for skippre.', [ps.Name]);
        Continue;
      end;

      if s.markeddown then
      begin
        irc_addtext(netname, channel, '<c4><b>ERROR</c></b> %s is marked as down!', [ps.Name]);
        exit;
      end;

      if ps.status = rssNotAllowed then
      begin
        irc_addtext(netname, channel, '<c4><b>ERROR</c></b> rip status for %s is not allowed!', [ps.Name]);
        exit;
      end;

      if (s <> nil) and (not s.markeddown) and (ps.status <> rssNotAllowed) then
      begin
        if (s.sectiondir[section] = '') then
        begin
          irc_addtext(netname, channel, 'Site <b>%s</b> has no predir set.', [ps.Name]);
          exit;
        end;
        if (s.sectionprecmd[section] = '') then
        begin
          irc_addtext(netname, channel, 'Site <b>%s</b> has no precmd set.', [ps.Name]);
          exit;
        end;
        if (s.working = sstUnknown) then
        begin
          irc_addtext(netname, channel, 'Status of site <b>%s</b> is unknown.', [ps.Name]);
          exit;
        end;
      end;
    end;


    SetupGenre;
    (* for debugging only
    irc_addtext(channel, 'genre: %s ripper: %s section %s tracks: %d size: %d', [genre, ripper, section, osszesfajl, osszesmeret]);
    exit;
    *)

    ps := nil;

    if ((genre = '') and (0 = Pos('dirfix', LowerCase(dir))) and
      (0 = Pos('nfofix', LowerCase(dir)))) then
    begin
      if (p.rls is TMP3Release) then
      begin
        irc_addText(netname, channel, 'Fetching genre...');
        s := nil;
        for i := 0 to p.sites.Count - 1 do
        begin
          ps := TPazoSite(p.sites[i]);
          if (ps.status <> rssNotAllowed) then
          begin
            s := FindSiteByName(netname, ps.Name);
            if ((s <> nil) and (s.working = sstUp)) then
              Break;
            //          s:= nil;
          end;
        end;

        if s = nil then
        begin // we are fucked somehow -- can't be happen anymore.. we add a check some lines approve ... but we will take it :)
          irc_addtext(netname, channel, '<c4><b>ERROR</c></b>: %s is no valid site!',
            [ps.Name]);
          exit;
        end;

        if ps = nil then
        begin // we are fucked somehow -- can't be happen anymore.. we add a check some lines approve ... but we will take it :)
          irc_addtext(netname, channel, '<c4><b>ERROR</c></b>: %s is no valid pazosite!');

          exit;
        end;

        if not IrcLame(netname, channel, s.Name + ' ' + section + ' ' + dir + ' genre') then
        begin
          irc_addtext(netname, channel, '<c4><b>ERROR</c></b>: No genre found!');
          exit;
        end;
        SetupGenre;
      end;
    end;


    addednumber := 0;
    tn1 := AddNotify;

    for i := 0 to p.sites.Count - 1 do
    begin
      ps := TPazoSite(p.sites[i]);
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
        irc_addtext(netname, channel, '<c8><b>INFO</c></b> %s is no valid site!', [ps.Name]);
        continue;
      end;

      if s.markeddown then
      begin
        irc_addtext(netname, channel, '<c4><b>ERROR</c></b> %s is marked as down!', [ps.Name]);
        exit;
      end;

      if ps.status = rssNotAllowed then
      begin
        irc_addtext(netname, channel, '<c4><b>ERROR</c></b> rip status for %s is not allowed!', [ps.Name]);
        exit;
      end;

      if ((s <> nil) and (ps.status <> rssNotAllowed) and (not s.markeddown)) then
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

    irc_addText(netname, channel, 'Changing working directory to the predir...');
    elozo := Now;

  finally
    queue_lock.Leave;
  end;

  queue_lock.Enter;
  try
    try
      QueueFire;
      tn1.event.WaitFor($FFFFFFFF);
      irc_addtext(netname, channel, 'dir change done...');
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
      irc_addtext(netname, channel, '<c4><b>ERROR</c></b>: %s', ['<c4>We got different number of cwd responses...</c>']);
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
          irc_Addtext(netname, channel, '<c4><b>ERROR</c></b>: Cant find the rip on <c4><b>%s</b></c>!', [sr.sitename]);
          RemoveTN(tn1);
          exit;
        end;
      finally
        d.Free;
      end;
    end;

    if (SecondsBetween(Now, elozo) > 20) then
    begin
      irc_addtext(netname, channel, '<c4><b>ERROR</c></b>: <c4>%s</c>', ['It took too long, sorry...']);
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
      precmd := ReplaceText(precmd, '<ripper>', ripper);
      precmd := ReplaceText(precmd, '<section>', sectiontype);

      rr := TRawTask.Create(netname, channel, sr.sitename, MyIncludeTrailingSlash(s.predir), precmd);
      rr.wantedslot := sr.slotname;
      tn2.tasks.Add(rr);
      AddTask(rr);
    end;

    RemoveTN(tn1); // ez mar nem kell
    irc_addtext(netname, channel, 'Sending pre...');
    QueueFire;
  finally
    queue_lock.Leave;
  end;

  tn2.event.WaitFor($FFFFFFFF);

  queue_lock.Enter;
  try
    if tn2.responses.Count <> addednumber then
    begin
      irc_addtext(netname, channel, '<c4><b>ERROR</c></b>: %s', ['<c4>We got different number of pre responses...</c>']);
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
        mind := sr.ido;
        mins := sr.sitename;
      end
      else
      if mind > sr.ido then
      begin
        mins := sr.sitename;
        mind := sr.ido;
      end;

      if maxd = 0 then
      begin
        maxd := sr.ido;
        maxs := sr.sitename;
      end
      else
      if maxd < sr.ido then
      begin
        maxd := sr.ido;
        maxs := sr.sitename;
      end;
    end;

    irc_addtext(netname, channel, 'Ok. Fastest site was %s (%s), slowest %s (%s). Now sleeping some...', [mins, FormatDateTime('hh:nn:ss.zzz', mind), maxs, FormatDateTime('hh:nn:ss.zzz', maxd)]);

    sleep_value := config.ReadInteger(rrsection, 'predir_re_examine_time', 5);

    //  irc_addtext(netname, channel,'We will wait %dsec%ss and check the predirs over.. so hang %0:d sec%1:ss :)',[sleep_value,chr(39)]);
    irc_addtext(netname, channel, 'We will wait ' + IntToStr(sleep_value) +
      'sec' + Chr(39) + 's and check the predirs over.. so hang ' + IntToStr(
      sleep_value) + ' sec' + Chr(39) + 's :)');
    sleep_value := sleep_value * 1000;
    Sleep(sleep_value);

    // es most meg le kell csekkolni maradt e valami predirben
    tn3 := AddNotify;
    for i := 0 to tn2.responses.Count - 1 do
    begin
      sr := TSiteResponse(tn2.responses[i]);
      s  := FindSiteByName(netname, sr.sitename);
      if s.SkipPre then
        Continue;

      rl := TDirlistTask.Create(netname, channel, sr.sitename,
        MyIncludeTrailingSlash(s.predir));
      rl.wantedslot := sr.slotname;
      tn3.tasks.Add(rl);
      AddTask(rl);
    end;
    RemoveTN(tn2); // most mar ez sem kell
    irc_addtext(netname, channel, 'Checking if rip is still in any of the predirs...');
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
      irc_addtext(netname, channel, 'ERROR: %s', ['<c4>We got different number of dirlist responses...</c>']);
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
          irc_addtext(netname, channel, 'ERROR: <c4><b>%s</b></c> still has the rip in predir!', [sr.sitename]);
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
  (*
  // now notifying utraw
  if(config.ReadBool('prebot', 'use_regpredb', False)) then begin
  if genre = '' then genre:= 'Unknown';

      DecimalSeparator:= '.';
      addcmd:= config.ReadString('prebot', 'cmd', '');
{      if addcmd <> '' then begin
        addcmd:= ReplaceText(addcmd, '<date>', FormatDateTime('yyyy-mm-dd',Now));
        addcmd:= ReplaceText(addcmd, '<ripper>', ripper);
        addcmd:= ReplaceText(addcmd, '<rls>', dir);
        addcmd:= ReplaceText(addcmd, '<files>', IntToStr(osszesfajl));
        addcmd:= ReplaceText(addcmd, '<size>', IntToStr(osszesmeret));
        osszesmeretf:= osszesmeret;
        addcmd:= ReplaceText(addcmd, '<size_mb>', Format('%.1f',[osszesmeretf / 1024 / 1024]));
        addcmd:= ReplaceText(addcmd, '<genre>', genre);
        addcmd:= ReplaceText(addcmd, '<group>', group);

//        irc_Addtext_by_key('GROUP',config.ReadString('prebot', 'nick', 'xxx'))
//        irc_addText(config.ReadString('regpredb', 'net', 'xxx'), config.ReadString('regpredb', 'nick', 'xxx'), addcmd);

      end;
}
  end;
  *)
end;

function IrcPretest(const netname, channel: String; params: String): boolean;
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


function webstuff(dir: String): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to webtags.Count - 1 do
    if 0 < Pos(webtags[i], dir) then
    begin
      Result := True;
      exit;
    end;
end;

function IsMP3(section, rls: String): boolean;
begin
  Result := FindSectionHandler(section) = TMP3Release;
end;

function IrcBatch(const netname, channel: String; params: String): boolean;
label
  ujkor;
var
  s, ss: String;
  sitename, section, dir, ripper: String;
  i:     integer;
  site:  TSite;
  p:     TPazo;
  ps:    TPazoSite;
begin
  while (True) do
  begin
    queue_lock.Enter;
    try
      if batchqueue.Count = 0 then
      begin
        Break;
      end;
      s := batchqueue[0];
    finally
      queue_lock.Leave;
    end;

    sitename := SubString(s, #9, 1);
    section  := SubString(s, #9, 2);
    dir      := SubString(s, #9, 3);
    ripper   := SubString(s, #9, 4);

    if (IsMP3(section, dir)) then
    begin
      if not webstuff(dir) then
      begin
        irc_Addtext(netname, channel, '%slame %s %s %s', [irccmdprefix, sitename, section, dir]);
        if not IrcLame(netname, channel, sitename + ' ' + dir) then
        begin
          irc_Addtext(netname, channel, 'ERROR: <c4>Lame checking returned error. Skipping.</c>');
          goto ujkor;
        end;
      end
      else
        irc_Addtext(netname, channel, 'Webcrap detected, skipping lame checking.');
    end;

    queue_lock.Enter;
    try
      i := kb_add(netname, channel, sitename, section, '', 'NEWDIR', dir, '', True, False);
    finally
      queue_lock.Leave;
    end;

    if i = -1 then
    begin
      irc_Addtext(netname, channel, 'No Pazo found for ' + dir);
      goto ujkor;
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
            irc_addtext(netname, channel, '<c4>Site ' + site.Name + ' perm. down, skipping bnctest!</c>');
            Continue;
          end;
          {
          * might be done automatically for several reasons but site works again now
          if site.markeddown then
          begin
            irc_addtext(netname, channel, '<c4>Site ' + site.Name + ' is marked as down, skipping bnctest!</c>');
            Continue;
          end;
          }
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
    irc_Addtext(netname, channel, '%sbnctest %s', [irccmdprefix, ss]);
    IrcBnctest(netname, channel, Trim(ss));

    irc_Addtext(netname, channel, '%sspread %s %s %s', [irccmdprefix, sitename, section, dir]);
    if not IrcSpread(netname, channel, sitename + ' ' + dir) then
    begin
      irc_Addtext(netname, channel, 'ERROR: <c4>Spreading returned error. Skipping.</c>');
      goto ujkor;
    end
    else
      irc_Addtext(netname, channel, 'Checking now....');

    irc_Addtext(netname, channel, '%scheck %s %s %s', [irccmdprefix, sitename, section, dir]);
    if not IrcCheck(netname, channel, sitename + ' ' + dir) then
    begin
      irc_Addtext(netname, channel, 'ERROR: <c4>Checking returned error. Skipping.</c>');
      goto ujkor;
    end
    else
      irc_Addtext(netname, channel, 'preeeeing now....');

    irc_Addtext(netname, channel, '%spre %s %s %s', [irccmdprefix, section, dir, ripper]);
    IrcPre(netname, channel, section + ' ' + dir + ' ' + ripper);

    ujkor:
    queue_lock.Enter;
    try
      i := batchqueue.IndexOf(s);
      if i = 0 then
        batchqueue.Delete(0);
    finally
      queue_lock.Leave;
    end;
  end;

  irc_Addtext(netname, channel, 'Batch queue is empty.');
  Result := True;
end;

function IrcBatchAdd(const netname, channel: String; params: String): boolean;
var
  sitename, section, dir, ripper: String;
  s: TSite;
  fInputRlsMask: TslMask;
  fDirlist: TDirList;
  fDirlistEntry: TDirListEntry;
  i: Integer;

  // helper function to reduce code duplication
  // adds a new entry to batchqueue
  procedure AddEntryToBatchQueue;
  begin
    queue_lock.Enter;
    try
      batchqueue.Add(sitename + #9 + section + #9 + dir + #9 + ripper);
      if batchqueue.Count = 1 then
        IrcBatch(netname, channel, params);
    finally
      queue_lock.Leave;
    end;
  end;

begin
  Result := False;

  sitename := UpperCase(SubString(params, ' ', 1));
  section  := UpperCase(SubString(params, ' ', 2));
  dir      := SubString(params, ' ', 3);
  ripper   := SubString(params, ' ', 4);

  if dir = '' then
  begin
    ripper  := '';
    section := 'PRE';
    dir     := SubString(params, ' ', 2);
  end;

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
      ripper  := dir;
      dir     := SubString(params, ' ', 2);
      section := 'PRE';
    end;
  finally
    queue_lock.Leave;
  end;

  // todo: check why 'AP' is not allowed or what it does mean - maybe remove
  if ripper = 'AP' then
  begin
    irc_addText(netname, channel, 'Syntax error.');
    exit;
  end;

  if ((config.ReadBool('prebot', 'use_regpredb', False)) and (ripper = '')) then
  begin
    irc_addText(netname, channel, 'Who is the fish?');
    exit;
  end;

  {$IFDEF UNICODE}
    if ContainsText(dir, '*') then
  {$ELSE}
    if AnsiContainsText(dir, '*') then
  {$ENDIF}
  begin
    irc_Addtext(netname, channel, 'Doing a wildcard batch for %s', [dir]);
    fInputRlsMask := TslMask.Create(dir);
    try
      fDirlist := DirlistB(netname, channel, sitename, section);
      try
        if fDirlist <> nil then
        begin
          for i := 0 to fDirlist.entries.Count - 1 do
          begin
            fDirlistEntry := TDirListEntry(fDirlist.entries[i]);
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
          irc_Addtext(netname, channel, 'Can''t dirlist section %s', [section]);
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

function IrcBatchDel(const netname, channel: String; params: String): boolean;
var
  sitename, dir, ripper: String;
  i: integer;
begin
  Result := False;

  sitename := SubString(params, ' ', 1);
  dir      := SubString(params, ' ', 2);
  ripper   := SubString(params, ' ', 3);

  queue_lock.Enter;
  try
    i := batchqueue.IndexOf(sitename + #9 + dir + #9 + ripper);
    if i <> -1 then
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


function IrcCheck(const netname, channel: String; params: String): boolean;
var
  s: TSite;
  sttr, aksizestring, sizestring, predir, section, sitename, dir: String;
  tn: TTaskNotify;
  nfofound, added: boolean;
  sr: TSiteResponse;
  d: TDirList;
  failed, perfect, aktfiles, addednumber, i, files: integer;
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

    kb_Add(netname, channel, sitename, section, '', 'COMPLETE', dir, '', True);
    i := kb_list.IndexOf(section + '-' + dir);
    if i = -1 then
    begin
      irc_addtext(netname, channel, 'No valid kbID found for %s-%s!', [section, dir]);
      exit; // this is not possible
    end;

    p := TPazo(kb_list.Objects[i]);


    addednumber := 0;
    tn := AddNotify;
    for i := 0 to p.sites.Count - 1 do
    begin
      ps := TPazoSite(p.sites[i]);
      s  := FindSiteByName(netname, ps.Name);
      if s.Name = getAdminSiteName then
        continue;

      if s.SkipPre then
      begin
        irc_addtext(netname, channel, '<c8><b>INFO</c></b>: we skip check for %s ', [ps.Name]);
        continue;
      end;


      if s = nil then
        irc_addtext(netname, channel, '<c10><b>DEBUG</c></b>: %s is no valid site!', [ps.Name]);

      if s.markeddown then
        irc_addtext(netname, channel, '<c10><b>DEBUG</c></b>: %s is marked as down!', [ps.Name]);

      if ps.status = rssNotAllowed then
        irc_addtext(netname, channel, '<c10><b>DEBUG</c></b>: %s status is NotAllowed', [ps.Name]);



      if ((s <> nil) and (not s.markeddown) and (ps.status <> rssNotAllowed)) then
      begin
        r := TDirlistTask.Create(netname, channel, ps.Name,
          MyIncludeTrailingSlash(ps.maindir) + dir);
        // r.announce:= ps.name+' dirlist ready...'; // we dont want this anymore because of the lag
        tn.tasks.Add(r);
        AddTask(r);
        Inc(addednumber);
      end;
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
    csl := FindSkipList('PRE');

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
            nfofound := d.hasnfo;
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
        irc_addtext(netname, channel, '<b>%s</b> @ <b>%s</b> is %.2f %s in %d files.', [dir, sitename, sized, sizestring, files]);

        perfect     := 0;
        failed      := 0;
        //      addednumber:= 0;
        checkedlist := TStringList.Create;
        try
          for i := 0 to tn.responses.Count - 1 do
          begin
            sr := TSiteResponse(tn.responses[i]);
            if sr.sitename <> sitename then
            begin
              //          inc(addednumber);
              d := TDirList.Create(sr.sitename, nil, nil, sr.response);
              try
                d.UsefulFiles(aktfiles, aktsize);
                //nfofound := d.hasnfo;
                aksized := aktsize;
                RecalcSizeValueAndUnit(aksized, aksizestring, 0);
              finally
                d.Free;
              end;

              if aktfiles <> files then
              begin
                sttr := '';
                if aktfiles > files then
                  sttr := 'more';
                if aktfiles < files then
                  sttr := 'less';
                irc_addtext(netname, channel, '<c4><b>DEBUG</c></b>: %s (%d) have %s file then %s (%d)', [sr.sitename, aktfiles, sttr, sitename, files]);
              end;


              if aktsize <> size then
              begin
                sttr := '';
                if aktsize > size then
                  sttr := 'bigger';
                if aktsize < size then
                  sttr := 'smaller';

                irc_addtext(netname, channel, '<c4><b>DEBUG</c></b>: %s (%d) is %s then %s (%d)', [sr.sitename, aktsize, sttr, sitename, size]);
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

              (*
              if (not nfofound) then
              begin
                irc_addtext(netname, channel, 'ERROR: %s @ %s has no condom.', [Red(dir), Red(sr.sitename))]);
                added:= False;
              end;
              *)
            end;
          end;

          if ((perfect > 0) and (failed > 0)) then
          begin
            irc_addtext(netname, channel,
              'Perfect on <c3><b>%d</c></b> sites and failed on <c4><b>%d</c></b> sites compared to <b>%s</b>.',
              [perfect, failed, sitename]);
            irc_addtext(netname, channel, 'failed on: ' + checkedlist.Values['FAILED']);
          end
          else
          if (failed > 0) then
            irc_addtext(netname, channel,
              '<c4>Failed on %d sites compared to</c> <b>%s</b>.', [failed, sitename])
          else
          if (perfect > 0) then
            irc_addtext(netname, channel, '<c3>Perfect on %d sites compared to</c> <b>%s</b>.', [perfect, sitename]);

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

function IrcCheckForExistsRip(const netname, channel: String; params: String): boolean;
var
  SOK, SBAD, predir, rip: String;
  s:     TSite;
  ii, I: integer;
  d:     TDirlist;
  de:    TDirListEntry;
begin
  Result := False;
  rip    := params;
  SOK    := '';
  SBAD   := '';

  for I := 0 to sites.Count - 1 do
  begin
    s := TSite(sites.Items[i]);

    if s.Name = getAdminSiteName then
      continue;

    if s.SkipPre then
    begin
      irc_addtext(netname, channel, '<c8><b>INFO</c></b>: we skip check for %s ', [s.Name]);
      continue;
    end;

    predir := s.sectiondir['PRE'];
    if predir = '' then
      Continue;

    d := DirlistB(netname, channel, s.Name, MyIncludeTrailingSlash(predir));
    d.dirlist_lock.Enter;
    try
      if d <> nil then
      begin
        for ii := 0 to d.entries.Count - 1 do
        begin
          de := TDirListEntry(d.entries[ii]);
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

  irc_addtext(netname, channel, '<c3>%s</c>%s', [SOK]);
  irc_addtext(netname, channel, '<c4>%s</c>%s', [SBAD]);

  Result := True;
end;


function IrcListPreContent(const netname, channel: String; params: String): boolean;
var
  s:      TSite;
  ii, i:  integer;
  predir: String;
  d:      TDirlist;
  de:     TDirListEntry;
  plist:  TStringList;
begin
  Result := False;
  plist  := TStringList.Create;
  try
    if params = '*' then
    begin
      for i := 0 to sites.Count - 1 do
      begin
        s := TSite(sites.Items[i]);
        if s.Name = getAdminSiteName then
          continue;

        if s.SkipPre then
        begin
          irc_addtext(netname, channel, '<c8><b>INFO</c></b>: we skip check for %s ', [s.Name]);
          continue;
        end;

        try
          predir := s.sectiondir['PRE'];
        except
          on E: Exception do
            irc_addtext(netname, channel, '<c4><b>ERROR</c></b>: %s', [e.Message]);
        end;
        if predir = '' then
        begin
          irc_addtext(netname, channel, '', []);
          Continue;
        end;

        d := DirlistB(netname, channel, s.Name, MyIncludeTrailingSlash(predir));
        d.dirlist_lock.Enter;
        try
          if d <> nil then
          begin
            for ii := 0 to d.entries.Count - 1 do
            begin
              de := TDirListEntry(d.entries[ii]);
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
        irc_addtext(netname, channel, '%s ( %s )', [plist.Names[i], plist.ValueFromIndex[i]]);

    end
    else
    begin
      s := FindSiteByName(netname, params);
      if s = nil then
      begin
        irc_addtext(netname, channel, 'Site %s not found.', [s.Name]);
        exit;
      end;

      try
        predir := s.sectiondir['PRE'];
      except
        on E: Exception do
          irc_addtext(netname, channel, '<c4><b>ERROR</c></b>: %s', [e.Message]);
      end;

      if predir = '' then
      begin
        irc_addtext(netname, channel, 'No valid path for section %s found on %s ', ['PRE', s.Name]);
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
            de := TDirListEntry(d.entries[ii]);
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

procedure PrebotInit;
begin
  batchqueue := TStringList.Create;

  webtags := TStringList.Create;
  webtags.Delimiter := ',';
  webtags.DelimitedText := config.ReadString(section, 'webtags', '-WEB-');
end;

procedure PrebotUninit;
begin
  Debug(dpSpam, section, 'Uninit1');
  batchqueue.Free;
  webtags.Free;
  Debug(dpSpam, section, 'Uninit2');
end;

end.

