unit taskautoindex;

interface

uses tasksunit;

type
  TAutoIndexTask = class(TTask)
    function Execute(slot: Pointer): Boolean; override;
    function Name: String; override;
  private
    function DoIndexing(slot: Pointer; const sectionname, path: String;
      const aktszint: Integer): Integer;
  end;

implementation

uses SyncObjs, Classes, configunit, mainthread, sitesunit, precatcher, kb, queueunit, mystrings, dateutils,
  dirlist, SysUtils, irc, debugunit, indexer, Regexpr;

const
  rsections = 'indexer';

  { TAutoSectionTask }

function IndexFindNfo(dl: TDirList): TDirListEntry;
var
  de: TDirlistEntry;
  //i: Integer;
  rx: TRegexpr;
begin
  Result := nil;
  rx := TRegexpr.Create;
  try
    rx.Expression := config.ReadString(rsections, 'expect_nfo_files', '');
    try
      de := dl.FindNfo;
      if (de <> nil) then
      begin
        if not rx.exec(de.filename) then
        begin
          Result := de;
          exit;
        end;
      end;

    except
      on e: Exception do
      begin
        Debug(dpError, rsections, Format('[EXCEPTION] IndexFindNfo: %s', [e.Message]));
        Result := nil;
      end;
    end;

  finally
    rx.free;
  end;
end;

function TAutoIndexTask.DoIndexing(slot: Pointer; const sectionname, path: String; const aktszint: Integer): Integer;
var
  s: TSiteSlot;
  dl: TDirList;
  de: TDirListEntry;
  db: Integer;
  j: Integer;

  procedure DoActualIndexing;
  var
    i: Integer;
  begin
    dl.dirlist_lock.Enter;
    try
      for i := 0 to dl.entries.Count - 1 do
      begin
        try
          de := TDirlistEntry(dl.entries[i]);
          if de.Directory then
          begin
            indexerAddRelease(de.filename, site1, sectionname, path);
            inc(Result);
          end;
        except
          on e: Exception do
          begin
            Debug(dpError, rsections, Format('[EXCEPTION] DoActualIndexing: %s', [e.Message]));
            Break;
          end;
        end;
      end;
    finally
      dl.dirlist_lock.Leave;
    end;
  end;
begin
  s := slot;

  Result := 0;
  if not s.Dirlist(path, true, true) then // daydir might have change
  begin
    if (not s.ReLogin) then
    begin
      Debug(dpError, rsections, Format('ERROR: can not ReLogin %s', [s.Name]));
      Result := -1;
      exit;
    end;
  end;

  if ((slshutdown) or (s.shouldquit)) then
  begin
    Result := -1;
    exit;
  end;


  // sikeres dirlist, fel kell dolgozni az elemeit
  //dirlist successful, you need to work with the elements
  dl := TDirlist.Create(s.site.name, nil, nil, s.lastResponse);

  try
    if nil = IndexFindNfo(dl) then
    begin
      //Debug(dpError, rsections,'ERROR: No NFO Found');
      if (aktszint < config.ReadInteger(rsections, 'max_deep', 5)) then // wont go any deeper
      begin
        for j := 0 to dl.entries.Count - 1 do
        begin
          try
            de := TDirlistEntry(dl.entries[j]);
            if de.Directory then
            begin
              db := doIndexing(slot, sectionname, MyIncludeTrailingSlash(path) + de.filename, aktszint + 1);

              if db = -1 then
              begin
                Result := db;
                Break;
              end
              else
              begin
                if db = -2 then
                begin
                  DoActualIndexing;
                  Break;
                end
                else
                  inc(Result, db);
              end;
            end;
          except
            on e: Exception do
            begin
              Debug(dpError, rsections, Format('[EXCEPTION] DoActualIndexing: %s', [e.Message]));
              Break;
            end;
          end;
        end;
      end;
    end
    else
      //Debug(dpError, rsections,'ERROR: No NFO Found max_deep.');
      Result := -2;
  finally
    dl.Free;
  end;
end;

function TAutoIndexTask.Execute(slot: Pointer): Boolean;
var
  s: TSiteSlot;
  i: Integer;
  l: TAutoIndexTask;
  ss, section, sectiondir: String;
  db: Integer;

  procedure UjraAddolas;
  begin
    // megnezzuk, kell e meg a taszk
    i := s.RCInteger('autoindex', 0);
    if i > 0 then
    begin
      try
        l := TAutoIndexTask.Create(netname, channel, site1);
        l.startat := IncSecond(Now, i);
        l.dontremove := True;
        AddTask(l);
        s.site.WCDateTime('nextautoindex', l.startat);
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[EXCEPTION] TAutoIndexTask.Execute AddTask: %s', [e.Message]));
        end;
      end;
    end;
  end;

begin
  Result := False;
  s := slot;
  debugunit.Debug(dpMessage, rsections, Name);

  // megnezzuk, kell e meg a taszk
  if s.RCInteger('autoindex', 0) = 0 then
  begin
    ready := True;
    Result := True;
    exit;
  end;

  if s.site.working = sstDown then
  begin
    ujraaddolas();
    readyerror := True;
    exit;
  end;

  if (s.status <> ssOnline) then
  begin
    if (not s.ReLogin) then
    begin
      ujraaddolas();
      readyerror := True;
      exit;
    end;
  end;

  // implement the task itself
  ss := s.RCString('autoindexsections', '');
  if not indexerCapable then
    ss := '';

  for i := 1 to 1000 do
  begin
    section := SubString(ss, ' ', i);
    if section = '' then
      break;
    sectiondir := s.site.sectiondir[section];
    if sectiondir <> '' then
    begin

      irc_SendINDEXER(Format('Indexing of %s on site %s start.', [section, site1]));
      try
        if config.ReadBool(rsections, 'transaction', True) then
          indexerBeginTransaction();

        try
          indexerRemoveSiteSection(site1, section);
          db := doIndexing(slot, section, sectiondir, 1);

          if db < 0 then
          begin
            irc_addtext(netname, channel, 'Indexing of %s on site %s finished, no rips added.', [section, site1]);
            irc_SendINDEXER(Format('Indexing of %s on site %s finished, no rips added.', [section, site1]));
          end;

          if db >= 0 then
          begin
            irc_addtext(netname, channel, 'Indexing of %s on site %s finished, %d rips in index.', [section, site1, db + 1]);
            irc_SendINDEXER(Format('Indexing of %s on site %s finished, %d rips in index.', [section, site1, db + 1]));
          end;

        finally
          if config.ReadBool(rsections, 'transaction', True) then
            indexerEndTransaction();
        end;

      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[EXCEPTION] TAutoIndexTask.Execute: %s', [e.Message]));
        end;
      end;

    end;
  end;

  ujraaddolas();

  Result := True;
  ready := True;
end;

function TAutoIndexTask.Name: String;
var
  cstr: String;
begin
  if ScheduleText <> '' then
    cstr := format('(%s)', [ScheduleText])
  else
    cstr := '';
  Result := format('AUTOINDEX %s %s', [site1, cstr]);
end;

end.

