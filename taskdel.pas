unit taskdel;

interface

uses
  SyncObjs, SysUtils, tasksunit;

type
  TDelReleaseTask = class(TTask)
    private
      dir: String;
      devent: TEvent;
      function RemoveDir(slot: Pointer; const dir: String): Boolean;
    public
      constructor Create(const netname, channel, site, dir: String);
      destructor Destroy;override;
      function Execute(slot: Pointer): Boolean; override;
      function Name: String; override;
  end;

implementation

uses
  sitesunit,  mystrings, dirlist, DebugUnit, irc;

const
  section = 'del';

{ TDelReleaseTask }

constructor TDelReleaseTask.Create(const netname, channel, site, dir: String);
begin
  inherited Create(netname, channel, site);
  self.dir := dir;
  devent := TEvent.Create(nil, true, false, 'DEL_' + site + '-' + dir);
end;

destructor TDelReleaseTask.Destroy;
begin
  devent.Free;
  inherited;
end;

function TDelReleaseTask.RemoveDir(slot: Pointer; const dir: String): Boolean;
var
  s: TSiteSlot;
  d: TDirList;
  i: Integer;
  de: TDirListEntry;
begin
  Result := True;
  s := slot;
  if not s.Dirlist(dir, False, True) then
  begin
    irc_addtext(Netname, Channel, 'can%st dirlist %s', [Chr(39), Dir]);
    exit;
  end;

  d := TDirList.Create(s.site.Name, nil, nil, s.lastResponse);
  d.dirlist_lock.Enter;
  try
    for i := 0 to d.entries.Count - 1 do
    begin
      de := TDirListEntry(d.entries[i]);
      if not de.directory then
      begin
        if not s.RemoveFile(dir, de.filename) then
        begin
          Result := False;
          Break;
        end;
      end;
    end;

    if Result then
    begin
      for i := 0 to d.entries.Count - 1 do
      begin
        de := TDirListEntry(d.entries[i]);
        if ((de.filename = '.') or (de.filename = '..')) then Continue;
        if de.directory then
        begin
          if not RemoveDir(slot, dir + de.filename) then
          begin
            Result := False;
            Break;
          end;
        end;
      end;
    end;

    if Result then
    begin
      // and let's get out the main directory
      Result := s.RemoveDir(dir);
    end;
  finally
    d.dirlist_lock.Leave;
    d.Free;
  end;
end;

function TDelReleaseTask.Execute(slot: Pointer): Boolean;
label
  ujra;
var
  s: TSiteSlot;
begin
  Result := False;
  s := slot;
  Debug(dpMessage, section, Name);

ujra:
  if s.status <> ssOnline then
    if not s.ReLogin then
    begin
      readyerror := True;
      exit;
    end;

  dir := MyIncludeTrailingSlash(dir);
  if (not RemoveDir(s, dir)) then goto ujra;

  Result := True;
  ready := True;
  devent.setevent;
end;

function TDelReleaseTask.Name: String;
begin
  try
    Result := Format('DELETE %s - %s',[site1, dir]);
  except
    Result := 'DELETE';
  end;
end;

end.