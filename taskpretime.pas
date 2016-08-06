unit taskpretime;

interface

uses kb, Classes, pazo, tasksunit, taskrace, mysqlutilunit;

type
  TPretimeLookupMOde = (plmNone, plmHTTP, plmMYSQL, plmSQLITE);

  TPazoPretimeLookupTask = class(TPazoPlainTask)
  private
    attempt: integer;
    //duperes:PDupeResults;
//    foctime: int64;
    vctime: int64;
    url: AnsiString;
    function FetchTimeFromPHP: boolean;
    //    function FetchTimeFromMYSQL:boolean;
  public
    constructor Create(const netname, channel: AnsiString; site: AnsiString;
      pazo: TPazo; attempt: integer);
    function Execute(slot: Pointer): boolean; override;
    function Name: AnsiString; override;
  end;

  TSLOffset = class
  private
    fNewtime, fOldtime: int64;
    fvalue: AnsiString;
  public
    constructor Create;
    function ReCalcTimeStamp(oldtime: int64): boolean;
    property NewTimeStamp: int64 read fNewtime;
    property OldTimeStamp: int64 read fOldtime;
  end;

function PrepareTimestamp(TimeStamp: int64): int64; overload;
function PrepareTimestamp(DateTime: TDateTime): TDateTime; overload;

implementation

uses DateUtils, SysUtils, queueunit, debugunit, configunit, mystrings,
  sltcp, slhttp, RegExpr, irc, mrdohutils;

const
  section = 'taskpretime';

function GetPretimeURL: AnsiString; overload;
begin
  Result := config.readString(section, 'url', '');
end;

function GetPretimeURL(index: integer): AnsiString; overload;
begin
  if index <= preurls.Count then
    Result := preurls.Strings[index];
end;

{ TPazoPretimeLookup }

constructor TPazoPretimeLookupTask.Create(const netname, channel: AnsiString;
  site: AnsiString; pazo: TPazo; attempt: integer);
begin
  self.attempt := attempt;
  inherited Create(netname, channel, site, '', pazo);
end;

function TPazoPretimeLookupTask.Execute(slot: Pointer): boolean;
var
  r: TPazoPretimeLookupTask;
  plm: TPretimeLookupMOde;
  //  plresult:boolean;
begin
  Result := True;
  plm := TPretimeLookupMOde(config.ReadInteger(section, 'mode', 0));
  case plm of
    plmNone: Result := True;
    plmHTTP: Result := FetchTimeFromPHP;
    //  plmMYSQL:result:=FetchTimeFromMYSQL ;
  end;
  if not Result then
  begin
    if attempt < config.readInteger(section, 'readd_attempts', 5) then
    begin
      debug(dpSpam, section, 'READD: retrying pretime lookup for %s later',
        [mainpazo.rls.rlsname]);
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
  kb_add(netname, channel, site1, mainpazo.rls.section, '', 'UPDATE',
    mainpazo.rls.rlsname, '');
  ready := True;
  Result := True;
  //TPretimeLookupMOde = (plmNone, plmHTTP, plmMYSQL);
end;

function TPazoPretimeLookupTask.FetchTimeFromPHP: boolean;
////label nexturl, alldone;

var
  //response: string;
  response: TStringList;
  prex: TRegexpr;
  //  sctime: string;
    //urlcount:integer;
  i: integer;
begin
  Result := False;
  //urlcount := 0;
  //nexturl:
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
    response.Text := slUrlGet(Format(url, [mainpazo.rls.rlsname]));
    debug(dpSpam, section, 'Pretime results for %s' + #13#10 + '%s', [mainpazo.rls.rlsname, response.Text]);
    prex := TRegexpr.Create;
    try
      prex.ModifierM := True;
      prex.Expression := '(\S+) (\S+) (\S+) (\S+) (\S+)$';
      for i := 0 to response.Count - 1 do
      begin
        if prex.Exec(response.strings[i]) then
        begin
          vctime := PrepareTimestamp(StrToInt(prex.Match[2]));
          //vctime:=PrepareOffSet(strtoint(prex.Match[2]));
          mainpazo.rls.cpretime := vctime;
          mainpazo.rls.pretime := UnixToDateTime(vctime);
          Result := True;
        end
        else
        begin
          //urlcount:=urlcount+1;
          //goto nexturl;
        end;
      end;

    finally
      prex.Free;
    end;

  finally
    response.Free;
  end;
end;

function FetchTimeFromMYSQL(rls: TRelease): boolean;
begin
  Result := True;
end;

function TPazoPretimeLookupTask.Name: AnsiString;
begin
  try
    Result := Format('<b>::PRETIME:</b> %s [ID:%d] [Round:%d]',
      [mainpazo.rls.rlsname, mainpazo.pazo_id, attempt]);
  except
    Result := 'PRETIME';
  end;
end;

function PrepareTimestamp(DateTime: TDateTime): TDateTime;
begin
  result := UnixToDateTime(PrepareTimestamp(DateTimeToUnix(DateTime)));
end;

function PrepareTimestamp(TimeStamp: int64): int64;
var
  vof: TSLOffset;
begin
  Result := TimeStamp;
  vof := TSLOffset.Create;
  try
    try
      if vof.ReCalcTimeStamp(TimeStamp) then
        Result := vof.NewTimeStamp
    except
      on E: Exception do
      begin
        Debug(dpError, section, format('[EXCEPTION] PrepareTimestamp: %s', [E.Message]));
      end;
    end;
  finally
    vof.Free;
  end;
end;

constructor TSLOffset.Create;
begin
  fvalue := config.ReadString(section, 'offset', '0');
end;

function TSLOffset.ReCalcTimeStamp(oldtime: int64): boolean;
var
  r: TRegexpr;
  vval: int64;
begin
  Result := False;
  fNewtime := fOldtime;
  if fvalue = '0' then
    exit;

  r := TRegexpr.Create;
  try

  r.Expression := '^(\+|\-)([\d]+)$';
  try
    if r.Exec(fvalue) then
    begin
      vval := StrToInt(r.Match[2]);
      vval := vval * 3600;
      fNewtime := 0;
      fOldtime := oldtime;
      if r.Match[1] = '+' then
        fNewtime := fOldtime + vval;
      if r.Match[1] = '-' then
        fNewtime := fOldtime - vval;
    end;
  finally
    Result := True;
  end;

  finally
    r.Free;
  end;

end;

end.

