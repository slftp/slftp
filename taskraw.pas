unit taskraw;

interface

uses tasksunit;

type
  TRawTask = class(TTask)
   cmd: AnsiString;
   dir: AnsiString;
   constructor Create(const netname, channel: AnsiString;site: AnsiString; dir: AnsiString; cmd: AnsiString);
   function Execute(slot: Pointer): Boolean; override;
   function Name: AnsiString; override;
  end;

implementation

uses sitesunit, SysUtils, mystrings, DebugUnit;

const
  section = 'raw';

constructor TRawTask.Create(const netname, channel: AnsiString;site: AnsiString; dir: AnsiString; cmd: AnsiString);
begin
  self.cmd := cmd;
  self.dir := dir;
  inherited Create(netname, channel, site);
end;

function TRawTask.Execute(slot: Pointer): Boolean;
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
    if not s.ReLogin(1) then
    begin
      readyerror := True;
      exit;
    end;

  if dir <> '' then
    if (not s.Cwd(dir, true)) then goto ujra;

  if (not s.Send(cmd)) then goto ujra;
  if (not s.Read(cmd)) then goto ujra;
  ido := Now();

  response := s.lastResponse;


  Result := True;
  ready := True;
end;

function TRawTask.Name: AnsiString;
begin
  try
    Result := 'RAW ' + site1 + ' -> ' + cmd;
  except
    Result := 'RAW';
  end;
end;

end.

