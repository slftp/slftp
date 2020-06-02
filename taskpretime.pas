unit taskpretime;

interface

uses
  kb, Classes, pazo, tasksunit, taskrace;

type
  TPretimeLookupMOde = (plmNone, plmHTTP, plmMYSQL, plmSQLITE);

  TPazoPretimeLookupTask = class(TPazoPlainTask)
    private
      attempt: integer;
      vctime: int64;
      url: String;
      function FetchTimeFromPHP: boolean;
      //function FetchTimeFromMYSQL: boolean;
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

function GetPretimeURL: String; overload;
begin
  Result := config.readString(section, 'url', '');
end;

function GetPretimeURL(const index: integer): String; overload;
begin
  Result := '';
  if index <= preurls.Count then
    Result := preurls.Strings[index];
end;

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
begin
  Result := True;

  plm := TPretimeLookupMOde(config.ReadInteger(section, 'mode', 0));
  case plm of
    plmNone: Result := True;
    plmHTTP: Result := FetchTimeFromPHP;
    //plmMYSQL: Result := FetchTimeFromMYSQL;
  end;

  if not Result then
  begin
    if attempt < config.readInteger(section, 'readd_attempts', 5) then
    begin
      debug(dpSpam, section, 'READD: retrying pretime lookup for %s later', [mainpazo.rls.rlsname]);
      r := TPazoPretimeLookupTask.Create(netname, channel, ps1.Name, mainpazo, attempt + 1);
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

  kb_add(netname, channel, site1, mainpazo.rls.section, '', kbeUPDATE, mainpazo.rls.rlsname, '');
  ready := True;
  Result := True;
end;

// TODO: Improve code and use different regex stuff, so that it could be used with a public predb (e.g. predb.ws)
//        maybe it loops it if no result is given as ready value is only set to true if url is empty
function TPazoPretimeLookupTask.FetchTimeFromPHP: boolean;
var
  response: TStringList;
  prex: TRegexpr;
  i: integer;
  fHttpGetErrMsg: String;
  fStrHelper: String;
begin
  Result := False;

  url := GetPretimeURL;
  if url = '' then
  begin
    debug(dpSpam, section, 'URL value is empty');
    ready := True;
    Result := True;
    exit;
  end;

  response := TStringList.Create;
  try
    if not HttpGetUrl(Format(url, [mainpazo.rls.rlsname]), fStrHelper, fHttpGetErrMsg) then
    begin
      Debug(dpError, section, Format('[FAILED] Pretime fetch from HTTP for %s --> %s ', [mainpazo.rls.rlsname, fHttpGetErrMsg]));
      irc_Adderror(Format('<c4>[FAILED]</c> Pretime fetch from HTTP for %s --> %s', [mainpazo.rls.rlsname, fHttpGetErrMsg]));
      exit;
    end;

    response.Text := fStrHelper;

    debug(dpSpam, section, 'Pretime results for %s' + #13#10 + '%s', [mainpazo.rls.rlsname, response.Text]);
    prex := TRegexpr.Create;
    try
      prex.ModifierM := True;
      prex.Expression := '(\S+) (\S+) (\S+) (\S+) (\S+)$';
      for i := 0 to response.Count - 1 do
      begin
        if prex.Exec(response.strings[i]) then
        begin
          vctime := StrToInt(prex.Match[2]);
          mainpazo.rls.pretime := vctime;
          Result := True;
        end;
      end;
    finally
      prex.Free;
    end;
  finally
    response.Free;
  end;
end;

{
function FetchTimeFromMYSQL(rls: TRelease): boolean;
begin
  Result := True;
end;
}

function TPazoPretimeLookupTask.Name: String;
begin
  try
    Result := Format('<b>.:PRETIME:.</b> %s [ID: %d] [Round: %d]', [mainpazo.rls.rlsname, mainpazo.pazo_id, attempt]);
  except
    Result := '.:PRETIME:.';
  end;
end;

end.
