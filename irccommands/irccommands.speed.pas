unit irccommands.speed;

interface

{ slftp speed commands functions }
function IrcSpeedTestLocal(const netname, channel, params: String): boolean;
function IrcSpeedTestOut(const netname, channel, params: String): boolean;
function IrcSpeedTestIn(const netname, channel, params: String): boolean;
function IrcSpeedTestCleanup(const netname, channel, params: String): boolean;

implementation

uses
  SysUtils, Classes, Contnrs, SyncObjs, sitesunit, pazo, taskrace, taskspeedtest, irc, notify, taskfilesize,
  speedstatsunit, kb, mystrings, dirlist, taskdirlist, configunit, queueunit, IdGlobal, irccommandsunit,
  Generics.Collections;

const
  section = 'irccommands.speed';

procedure _PickupSpeedtestFile(d: TDirList; var fsfilename: String; var fsfilesize: Int64);
var
  de: TDirListEntry;
  i: integer;
begin
  fsfilename := '';
  fsfilesize := 0;
  d.dirlist_lock.Enter;
  try
    for i := 0 to d.entries.Count - 1 do
    begin
      de := TDirListEntry(d.entries[i]);
      if ((de.filesize > fsfilesize) and (de.filesize >=
        config.ReadInteger('speedtest', 'min_filesize', 15) * 1024 * 1024) and
        (de.filesize <= config.ReadInteger('speedtest', 'max_filesize', 120) *
        1024 * 1024)) then
      begin
        fsfilename := de.filename;
        fsfilesize := de.filesize;
      end;
    end;
  finally
    d.dirlist_lock.Leave;
  end;
end;

function IrcSpeedTestLocal(const netname, channel, params: String): boolean;
var
  sitename: String;
  s: TSite;
  dir: String;
  t: TUploadSpeedtestFileTask;
  tn: TTaskNotify;
begin
  Result := False;
  sitename := UpperCase(params);
  s := FindSiteByName(Netname, sitename);
  if nil = s then
  begin
    irc_addtext(Netname, Channel, 'Site %s not found.', [sitename]);
    exit;
  end;
  dir := s.sectiondir['SPEEDTEST'];
  if dir = '' then
  begin
    irc_addtext(Netname, Channel, 'Site %s has no SPEEDTEST section.',
      [sitename]);
    exit;
  end;
  tn := AddNotify;
  t := TUploadSpeedtestFileTask.Create(Netname, Channel, sitename);
  tn.tasks.Add(t);
  AddTask(t);

  tn.event.WaitFor($FFFFFFFF);

  RemoveTN(tn);

  Result := True;
end;

function IrcSpeedTestOut(const netname, channel, params: String): boolean;
var
  fParams, oparams, ss: String;
  s: TSite;
  tn: TTaskNotify;
  p: TPazo;
  firstsite, ps: TPazoSite;
  i: integer;
  t: TPazoRaceTask;
  sr: TSiteResponse;
  j: integer;
  fs: TFileSizeTask;
  fssitename, fsfilename: String;
  fsfilesize: Int64;
  fsfilesizemb: double;
  todel: String;
  d1, d2: double;
  d: TDirList;
begin
  Result := False;
  tn := nil;

  // First, validate all the parameters ...
  fParams := Trim(UpperCase(params));
  fssitename := '';
  fsfilename := '';
  oparams := fParams;
  while (True) do
  begin
    ss := Fetch(fParams, ' ', True, False);
    if ss = '' then
      break;

    if fssitename = '' then
      fssitename := ss;

    s := FindSiteByName(Netname, ss);
    if s = nil then
    begin
      irc_addtext(Netname, Channel, 'Site %s not found.', [ss]);
      exit;
    end;

    if (s.PermDown) then
    begin
      irc_addtext(Netname, Channel, 'Site %s is set as PermDown', [s.Name]);
      Exit;
    end;

    if not (s.WorkingStatus in [sstUnknown, sstUp]) then
    begin
      irc_addtext(Netname, Channel, 'Site %s is down.', [ss]);
      exit;
    end;

    if '' = s.sectiondir['SPEEDTEST'] then
    begin
      irc_addtext(Netname, Channel, 'Site %s has no SPEEDTEST section.', [ss]);
      exit;
    end;
    if fsfilename = '' then
      fsfilename := s.sectiondir['SPEEDTEST'];
  end;

  // Getting files from SPEEDTEST directory
  d := DirlistB(Netname, Channel, fssitename, fsfilename, True);
  try
    if d = nil then
    begin
      irc_addtext(Netname, Channel, 'Can''t dirlist %s in %s.', [fsfilename, fssitename]);
      exit;
    end;
    // now we pick a file
    _PickupSpeedtestFile(d, fsfilename, fsfilesize);
  finally
    d.Free;
  end;

  if ((fsfilesize = 0) or (fsfilename = '')) then
  begin
    irc_addtext(Netname, Channel,'No suitable file found on site %s for speedtesting, check slftp.ini. Speedtest aborted.', [ss]);
    exit;
  end;

  fsfilesizemb := fsfilesize / 1024 / 1024;
  irc_addtext(Netname, Channel, 'Testing outgoing speed with file %s (%d bytes)', [fsfilename, fsfilesize]);

  // Checking if the file already exists on destinations
  try
    tn := AddNotify;
  except
    on e: Exception do
    begin
      irc_addtext(Netname, Channel, '<c4>[EXCEPTION]</c> in SpeedTestOut-AddNotify: %s', [e.Message]);
      exit;
    end;
  end;

  fParams := oparams;

  while (True) do
  begin
    ss := Fetch(fParams, ' ', True, False);
    if ss = '' then
      break;

    if fssitename = ss then
      Continue;

    s := FindSiteByName(Netname, ss);
    fs := TFileSizeTask.Create(Netname, Channel, s.Name,
      MyIncludeTrailingSlash(s.sectiondir['SPEEDTEST']) + speedtestfilename);
    tn.tasks.Add(fs);

    AddTask(fs);
  end;

  if tn.tasks.Count = 0 then
  begin
    irc_addtext(Netname, Channel, 'Failed to check if speedtest file %s already exists on destination sites. Speedtest aborted.', [speedtestfilename]);
    exit;
  end;

  tn.event.WaitFor($FFFFFFFF);

  // deleting existing destination files
  todel := '';
  for i := 0 to tn.responses.Count - 1 do
  begin
    sr := TSiteResponse(tn.responses[i]);
    if StrToIntDef(sr.response, -1) > 0 then
      todel := todel + sr.sitename + ' ';
  end;
  RemoveTN(tn);
  todel := trim(todel);
  if todel <> '' then
  begin
    irc_addtext(Netname, Channel, 'Removing existing speedtest files from %s', [todel]);
    if not IrcSpeedTestCleanup(Netname, Channel, todel) then
    begin
      irc_addtext(Netname, Channel, 'Removing existing speedtest files failed. Speedtest aborted.');
      exit;
    end;
  end;

  // Creating the pazo entries for the speedtest
  firstsite := nil;
  fParams := oparams;

  p := PazoAdd(nil);

  kb_list.AddObject('TRANSFER-speedtest-' + IntToStr(p.pazo_id), p);

  while (True) do
  begin
    ss := Fetch(fParams, ' ', True, False);
    if ss = '' then
      break;

    s := FindSiteByName(Netname, ss);
    ps := p.AddSite(ss, s.sectiondir['SPEEDTEST']);
    if p.PazoSitesList.Count > 1 then
      TPazoSite(p.PazoSitesList[0]).AddDestination(ps, 1)
    else
      firstsite := ps;
  end;

  if firstsite = nil then
  begin
    irc_addtext(Netname, Channel, 'wtf?');
    exit;
  end;

  for i := 1 to p.PazoSitesList.Count - 1 do
  begin
    ps := TPazoSite(p.PazoSitesList[i]);
    irc_addtext(Netname, Channel, 'Speedtesting %s -> %s ->> %s', [firstsite.Name, ps.Name, ps.maindir]);
    tn := AddNotify;
    t := TPazoRaceTask.Create(Netname, Channel, firstsite.Name, ps.Name, p, '', fsfilename, fsfilesize, 1);
    t.FFilenameForSTORCommand := speedtestfilename;

    tn.tasks.Add(t);

    AddTask(t);

    tn.event.WaitFor($FFFFFFFF);

    if tn.responses.Count = 1 then
    begin
      sr := TSiteResponse(tn.responses[0]);
      j := StrToIntDef(sr.response, 0);

      if j <> 0 then
      begin
        d2 := j;
        d2 := d2 / 1000;
        d1 := j;
        d1 := fsfilesize / d1;
        j := SpeedStatsScale(d1);
        if ((j >= 1) and (j <= 9)) then
          irc_addtext(Netname, Channel,
            '%s -> %s => %.1f kB/s (%.1fmB sent in %.1fs) : %srouteset %s %s %d',
            [firstsite.Name, ps.Name, d1, fsfilesizemb, d2, irccmdprefix,
            firstsite.Name, ps.Name, j])
        else
          irc_addtext(Netname, Channel,
            '%s -> %s => %.1f kB/s (%.1fmB sent in %.1fs)',
            [firstsite.Name, ps.Name, d1, fsfilesizemb, d2]);
      end
      else
        irc_addtext(Netname, Channel, '%s -> %s failed.', [firstsite.Name, ps.Name]);
    end
    else
      irc_addtext(Netname, Channel, '%s -> %s failed.', [firstsite.Name, ps.Name]);

    RemoveTN(tn);
  end;

  Result := True;
end;

function IrcSpeedTestIn(const netname, channel, params: String): boolean;
var
  fParams, oparams, ss: String;
  s: TSite;
  tn: TTaskNotify;
  p: TPazo;
  firstsite, ps: TPazoSite;
  i: integer;
  t: TPazoRaceTask;
  sr: TSiteResponse;
  j: integer;
  ds: TDirlistTask;
  fssitename: String;
  d1, d2: double;
  added: integer;
  d: TDirList;
  fsfilename: String;
  fsfilesize: Int64;
  fsfilesizemb: double;
  speedtestsites: TStringList;
  speedtestfilenames: TStringList;
  speedtestfilesizes: TList<integer>;
begin
  Result := False;
  fssitename := '';
  // eloszor validaljuk az osszes parametert... we first validate all the parameters ...
  fParams := Trim(UpperCase(params));
  oparams := fParams;
  while (True) do
  begin
    ss := Fetch(fParams, ' ', True, False);
    if ss = '' then
      break;

    if fssitename = '' then
      fssitename := ss;

    s := FindSiteByName(Netname, ss);
    if s = nil then
    begin
      irc_addtext(Netname, Channel, 'Site %s not found.', [ss]);
      exit;
    end;

    if (s.PermDown) then
    begin
      irc_addtext(Netname, Channel, 'Site %s is set as PermDown', [s.Name]);
      Exit;
    end;

    if not (s.WorkingStatus in [sstUnknown, sstUp]) then
    begin
      irc_addtext(Netname, Channel, 'Site %s is down.', [ss]);
      exit;
    end;

    if '' = s.sectiondir['SPEEDTEST'] then
    begin
      irc_addtext(Netname, Channel, 'Site %s has no SPEEDTEST section.', [ss]);
      exit;
    end;
  end;

  // most megnezzuk, van e mar speedtest file a forras siteokon
  // Now look, this is already the source file Speedtest siteokon
  added := 0;
  tn := AddNotify;
  fParams := oparams;

  while (True) do
  begin
    ss := Fetch(fParams, ' ', True, False);
    if ss = '' then
      break;

    if fssitename = ss then
      Continue;

    s := FindSiteByName(Netname, ss);
    ds := TDirlistTask.Create(Netname, Channel, s.Name, s.sectiondir['SPEEDTEST'], True);
    tn.tasks.Add(ds);
    AddTask(ds);
    Inc(added);
  end;

  if added = 0 then
  begin
    irc_addtext(Netname, Channel, 'wtf?');
    exit;
  end;

  tn.event.WaitFor($FFFFFFFF);

  speedtestsites := TStringList.Create;
  speedtestfilenames := TStringList.Create;
  speedtestfilesizes := TList<integer>.Create;
  try
    if tn.responses.Count <> added then
    begin
      RemoveTN(tn);
      irc_addtext(Netname, Channel, 'ERROR: Incorrect number of responses?!');
    end;

    for i := 0 to tn.responses.Count - 1 do
    begin
      sr := TSiteResponse(tn.responses[i]);
      d := TDirList.Create(sr.sitename, nil, nil, sr.response, True);
      if d <> nil then
        d.FullPath := s.sectiondir['SPEEDTEST'];
      try
        _PickupSpeedtestFile(d, fsfilename, fsfilesize);
      finally
        d.Free;
      end;

      if ((fsfilename = '') or (fsfilesize = 0)) then
      begin
        RemoveTN(tn);

        irc_addtext(Netname, Channel,
          'Site %s has no suitable file for speedtesting, check slftp.ini', [ss]);
        exit;
      end;

      speedtestsites.Add(sr.sitename);
      speedtestfilenames.Add(fsfilename);
      speedtestfilesizes.Add(fsfilesize);
    end;

    RemoveTN(tn);

    // es most kezdodik a moka, megcsinaljuk a pazot meg a szarjait
    // And now the fun begins, you do make a shit pazot

    firstsite := nil;
    fParams := oparams;

    p := PazoAdd(nil);

    kb_list.AddObject('TRANSFER-speedtest-' + IntToStr(p.pazo_id), p);
    while (True) do
    begin
      ss := Fetch(fParams, ' ', True, False);
      if ss = '' then
        break;

      s := FindSiteByName(Netname, ss);
      ps := p.AddSite(ss, s.sectiondir['SPEEDTEST']);
      if p.PazoSitesList.Count > 1 then
        ps.AddDestination(firstsite, 1)
      else
        firstsite := ps;
    end;

    if firstsite = nil then
    begin
      irc_addtext(Netname, Channel, 'wtf?');
      exit;
    end;

    for i := 1 to p.PazoSitesList.Count - 1 do
    begin
      ps := TPazoSite(p.PazoSitesList[i]);

      if not IrcSpeedTestCleanup(Netname, Channel, firstsite.Name) then
      begin
        irc_addtext(Netname, Channel, 'ERROR: cant remove speedtest file on site %s', [firstsite.Name]);
        exit;
      end;

      j := speedtestsites.IndexOf(ps.Name);
      if j = -1 then
        Continue; // wtf?
      fsfilename := speedtestfilenames[j];
      fsfilesize := speedtestfilesizes[j];
      fsfilesizemb := fsfilesize / 1024 / 1024;

      irc_addtext(Netname, Channel, 'Speedtesting %s -> %s (using %s / %d bytes)', [ps.Name,
        firstsite.Name, fsfilename, fsfilesize]);

      tn := AddNotify;

      t := TPazoRaceTask.Create(Netname, Channel, ps.Name, firstsite.Name, p, '', fsfilename, fsfilesize, 1);
      t.FFilenameForSTORCommand := speedtestfilename;
      tn.tasks.Add(t);
      AddTask(t);

      tn.event.WaitFor($FFFFFFFF);

      if tn.responses.Count = 1 then
      begin
        sr := TSiteResponse(tn.responses[0]);
        j := StrToIntDef(sr.response, 0);
        if j <> 0 then
        begin
          d2 := j;
          d2 := d2 / 1000;
          d1 := j;
          d1 := fsfilesize / d1;
          j := SpeedStatsScale(d1);
          if ((j >= 1) and (j <= 9)) then
            irc_addtext(Netname, Channel,
              '%s -> %s => %.1f kB/s (%.1fmB sent in %.1fs) : %srouteset %s %s %d',
              [ps.Name, firstsite.Name, d1, fsfilesizemb, d2, irccmdprefix,
              ps.Name, firstsite.Name, j])
          else
            irc_addtext(Netname, Channel,
              '%s -> %s => %.1f kB/s (%.1fmB sent in %.1fs)',
              [ps.Name, firstsite.Name, d1, fsfilesizemb, d2]);
        end
        else
          irc_addtext(Netname, Channel, '%s -> %s failed.',
            [ps.Name, firstsite.Name]);
      end
      else
        irc_addtext(Netname, Channel, '%s -> %s failed, site responses is:%d',
          [ps.Name, firstsite.Name, tn.responses.Count]);
      RemoveTN(tn);
    end;

  finally
    speedtestsites.Free;
    speedtestfilenames.Free;
    speedtestfilesizes.Free;
  end;

  Result := True;
end;

function IrcSpeedTestCleanup(const netname, channel, params: String): boolean;
var
  fParams, ss: String;
  s: TSite;
  tn: TTaskNotify;
  t: TDelSpeedtestFileTask;
  i: integer;
begin
  Result := False;
  fParams := Trim(UpperCase(params));

  if fParams = '' then
  begin
    tn := AddNotify;
    for i := 0 to sites.Count - 1 do
    begin
      s := TSite(sites[i]);
      if '' <> s.sectiondir['SPEEDTEST'] then
      begin
        t := TDelSpeedtestFileTask.Create(Netname, Channel, s.Name);
        tn.tasks.Add(t);
        AddTask(t);
      end;
    end;
  end
  else
  begin
    // specified sites only
    tn := AddNotify;

    while (True) do
    begin
      ss := Fetch(fParams, ' ', True, False);
      if ss = '' then
        break;

      s := FindSiteByName(Netname, ss);
      if s = nil then
      begin
        RemoveTN(tn);
        irc_addtext(Netname, Channel, 'Site %s not found.', [ss]);
        exit;
      end;
      if '' = s.sectiondir['SPEEDTEST'] then
      begin
        RemoveTN(tn);
        irc_addtext(Netname, Channel,
          'Site %s has no SPEEDTEST section.', [ss]);
        exit;
      end;
      t := TDelSpeedtestFileTask.Create(Netname, Channel, ss);
      tn.tasks.Add(t);
      AddTask(t);
    end;
  end;

  if tn.tasks.Count = 0 then
  begin
    RemoveTN(tn);
    irc_addtext(Netname, Channel,
      'ERROR: No sites to add del speedtest file tasks to.');
    exit;
  end;

  irc_addtext(Netname, Channel, '%d del speedtest file tasks added.',
    [tn.tasks.Count]);
  tn.event.WaitFor($FFFFFFFF);

  RemoveTN(tn);
  Result := True;
end;

end.
