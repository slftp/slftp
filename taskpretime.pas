unit taskpretime;

interface

uses
  kb, Classes, pazo, tasksunit, taskrace, sitesunit, dbaddpre;

type
  TPretimeLookupMOde = (plmNone, plmHTTP, plmMYSQL, plmSQLITE);

  TPazoPretimeLookupTask = class(TPazoPlainTask)
    private
      attempt: integer;
      vctime: int64;
      url: String;
    public
      constructor Create(const netname, channel, site: String; pazo: TPazo; const attempt: integer);
      function Execute(slot: Pointer): boolean; override;
      function Name: String; override;
  end;

implementation

uses
  DateUtils, SysUtils, queueunit, debugunit, configunit,
  sltcp, http, RegExpr, irc, mrdohutils, kb.releaseinfo;

const
  section = 'taskpretime';


{ TPazoPretimeLookup }

constructor TPazoPretimeLookupTask.Create(const netname, channel, site: String; pazo: TPazo; const attempt: integer);
begin
  self.attempt := attempt;
  inherited Create(netname, channel, site, '', pazo);
end;

function TPazoPretimeLookupTask.Execute(slot: Pointer): boolean;
var
  r: TPazoPretimeLookupTask;
  plm: TPretimeLookupMOde;
  fPreTime: Int64;
begin
  Result := True;

  plm := TPretimeLookupMode(config.ReadInteger(section, 'mode', 0));
  if (plm = plmNone) or (mainpazo.rls.pretime <> 0) then
  begin
    ready := True;
    Result := True;
    Exit;
  end;

  fPreTime := getPretime(mainpazo.rls.rlsname).pretime;
  if fPreTime = 0 then
  begin
    if attempt < config.readInteger(section, 'readd_attempts', 5) then
    begin
      debug(dpSpam, section, 'READD: retrying pretime lookup for %s later', [mainpazo.rls.rlsname]);
      r := TPazoPretimeLookupTask.Create(netname, channel, getadminsitename, mainpazo, attempt + 1);
      r.startat := IncSecond(Now, config.ReadInteger(section, 'readd_interval', 3));
      AddTask(r);
    end
    else
    begin
      // mainpazo.rls.aktualizalasfailed:= True;
      debug(dpSpam, section, 'READD: no more attempts...');
    end;

    ready := True;
    Result := True;
    exit;
  end;

  kb_add(netname, channel, site1, mainpazo.rls.section, '', kbeADDPRE, mainpazo.rls.rlsname, '');
  ready := True;
  Result := True;
end;

function TPazoPretimeLookupTask.Name: String;
begin
  try
    Result := Format('<b>.:PRETIME:.</b> %s [ID: %d] [Round: %d]', [mainpazo.rls.rlsname, mainpazo.pazo_id, attempt]);
  except
    Result := '.:PRETIME:.';
  end;
end;

end.
