unit taskautoindex;

interface

uses tasksunit;

type
  TAutoIndexTask = class(TTask)
  private
    function DoIndexing(slot: Pointer; const sectionname, path: String; const aktszint: Integer): Integer;
  public
    function Execute(slot: Pointer): Boolean; override;
    function Name: String; override;
  end;

implementation

uses
  SyncObjs, Classes, DateUtils, Contnrs, configunit, mainthread, sitesunit, precatcher, kb, queueunit,
  mystrings, dirlist, SysUtils, irc, debugunit, indexer, Regexpr;

const
  rsections = 'indexer';

  { TAutoSectionTask }

function IndexFindNfo(dl: TDirList): TDirListEntry;
var
  de: TDirlistEntry;
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
  if not s.Dirlist(path, true, true, true) then // daydir might have change
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
  dl := TDirlist.Create(s.site.name, nil, nil, s.lastResponse, False, False, True);

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
  i, fInterval: Integer;
  l: TAutoIndexTask;
  ss, section, sectiondir: String;
  db: Integer;

  procedure ReAddTask;
  begin
    // fInterval > 0: feature is enabled and new task will be executed in fInterval seconds
    if fInterval > 0 then
    begin
      try
        l := TAutoIndexTask.Create(netname, channel, site1);
        l.startat := IncSecond(Now, fInterval);
        l.dontremove := True;
        AddTask(l);
        s.site.NextAutoIndexDateTime := l.startat;
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[EXCEPTION] TAutoIndexTask.Execute ReAddTask: %s', [e.Message]));
        end;
      end;
    end;
  end;

begin
  Result := False;
  s := slot;
  Debug(dpSpam, rsections, '-->' + Name);

  fInterval := s.site.AutoIndexInterval;
  // fInterval = 0: feature disabled
  if fInterval = 0 then
  begin
    ready := True;
    Result := True;
    exit;
  end;

  if not (s.site.WorkingStatus in [sstUnknown, sstUp, sstMarkedAsDownByUser]) then
  begin
    ReAddTask();
    readyerror := True;
    exit;
  end;

  if (s.status <> ssOnline) then
  begin
    if (not s.ReLogin) then
    begin
      ReAddTask();
      readyerror := True;
      exit;
    end;
  end;

  // implement the task itself
  ss := s.site.AutoIndexSections;

  for i := 1 to 1000 do
  begin
    section := SubString(ss, ' ', i);
    if section = '' then
      break;
    sectiondir := s.site.sectiondir[section];
    if sectiondir <> '' then
    begin
      sectiondir := DatumIdentifierReplace(sectiondir);

      irc_SendINDEXER(Format('Indexing of %s (path: %s) on site %s start.', [section, sectiondir, site1]));
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
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[EXCEPTION] TAutoIndexTask.Execute: %s', [e.Message]));
        end;
      end;

    end;
  end;

  ReAddTask();

  ready := True;
  Debug(dpSpam, rsections, '<--' + Name);
  Result := True;
end;

function TAutoIndexTask.Name: String;
var
  cstr: String;
begin
  if ScheduleText <> '' then
    cstr := Format('(%s)', [ScheduleText])
  else
    cstr := '';
  Result := Format('AUTOINDEX %s %s', [site1, cstr]);
end;

end.