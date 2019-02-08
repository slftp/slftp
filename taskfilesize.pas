unit taskfilesize;

interface

uses tasksunit;

type
  TFileSizeTask = class(TTask)
    private
      filename: String;
    public
      constructor Create(const netname, channel, site, filename: String);
      function Execute(slot: Pointer): Boolean; override;
      function Name: String; override;
  end;

implementation

uses
  Classes, debugunit, sitesunit, dirlist, SysUtils;

const
  section = 'taskfilesize';

{ TFileSizeTask }

constructor TFileSizeTask.Create(const netname, channel, site, filename: String);
begin
  inherited Create(netname, channel, site);
  self.filename := filename;
end;

function TFileSizeTask.Execute(slot: Pointer): Boolean;
label
  ujra;
var
  s: TSiteSlot;
  d: TDirList;
begin
  Result := False;
  response := '-1'; // file not found
  s := slot;
  Debug(dpMessage, section, Name);

ujra:
  if (s.status <> ssOnline) then
    if not s.ReLogin then
    begin
      readyerror := True;
      exit;
    end;

  if not s.Send('STAT -l %s', [filename]) then goto ujra;
  if not s.Read('STAT -l file') then goto ujra;

  try
    d := TDirlist.Create(s.site.name, nil, nil, s.lastResponse);
    try
      if d.entries.Count = 1 then
        response := IntToStr(TDirListEntry(d.entries[0]).filesize);
    finally
      d.Free;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TFileSizeTask.Execute: %s', [e.Message]));
      readyerror := True;
      exit;
    end;
  end;
  
  Result := True;
  ready := True;
end;

function TFileSizeTask.Name: String;
begin
  try
    Result := Format('FILESIZE: %s -> %s',[site1, filename]);
  except
    Result := 'FILESIZE';
  end;
end;

end.