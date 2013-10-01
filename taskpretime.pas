unit taskpretime;

interface

uses kb, Classes, pazo, tasksunit, taskrace, mysqlutilunit;

type
TPretimeLookupMOde = (plmNone, plmHTTP, plmMYSQL, plmSQLITE);
  TPazoPretimeLookupTask = class(TPazoPlainTask)
  private
    attempt: Integer;
    //duperes:PDupeResults;
    foctime:Int64;
    vctime:Int64;
    url:string;
    function FetchTimeFromPHP:boolean;
//    function FetchTimeFromMYSQL:boolean;
  public
    constructor Create(const netname, channel: string;site: string; pazo: TPazo; attempt: Integer);
    function Execute(slot: Pointer): Boolean; override;
    function Name: string; override;
  end;

  TSLOffset = class
  private
    fNewtime, fOldtime: Int64;
    fvalue: string;
  public
    constructor Create;
    function ReCalcTimeStamp(oldtime: Int64): Boolean;
    Property NewTimeStamp: Int64 read fNewtime;
    Property OldTimeStamp: Int64 read fOldtime;
  end;


function PrepareTimestamp(TimeStamp: Int64): Int64;


implementation

uses DateUtils, SysUtils, queueunit, debugunit, configunit, mystrings,
sltcp, slhttp, RegExpr,irc, mrdohutils;

const
  section = 'taskpretime';

function GetPretimeURL:string;overload;
begin
  result:=config.readString(section,'url','');
end;

function GetPretimeURL(index:integer):string;overload;
begin
if index <= preurls.Count then result:= preurls.Strings[index];
end;


{ TPazoPretimeLookup }

constructor TPazoPretimeLookupTask.Create(const netname, channel: string;
  site: string; pazo: TPazo; attempt: Integer);
begin
  self.attempt:= attempt;
  inherited Create(netname, channel, site, '', pazo);
end;


function TPazoPretimeLookupTask.Execute(slot: Pointer): Boolean;
var
	r: TPazoPretimeLookupTask;
  plm: TPretimeLookupMOde;
  plresult:boolean;
begin
Result:= True;
plm:= TPretimeLookupMOde(config.ReadInteger(section,'mode',0));
case plm of
  plmNone: Result:= True;
  plmHTTP: result:=FetchTimeFromPHP;
//  plmMYSQL:result:=FetchTimeFromMYSQL ;
end;
if not result then begin
		if attempt < config.readInteger(section, 'readd_attempts', 5) then 	begin
		  debug(dpSpam, section, 'READD: retrying pretime lookup for %s later',[mainpazo.rls.rlsname]);
		  r:= TPazoPretimeLookupTask.Create(netname, channel, ps1.name, mainpazo, attempt+1);
		  r.startat:= IncSecond(Now, config.ReadInteger(section, 'readd_interval', 3));
		  AddTask(r);
		end else begin
		 // mainpazo.rls.aktualizalasfailed:= True;
		  debug(dpSpam, section, 'READD: no more attempts...');
		end;
		ready:= True;
		Result:= True;
		exit;
	end;
	kb_add(netname, channel, site1, mainpazo.rls.section, '', 'UPDATE', mainpazo.rls.rlsname, '' );
	ready:= True;
	Result:= True;
//TPretimeLookupMOde = (plmNone, plmHTTP, plmMYSQL);
end;



function TPazoPretimeLookupTask.FetchTimeFromPHP:boolean;
////label nexturl, alldone;

var
//response: string;
response: TstringList;
prex:TRegexpr;
sctime:string;
urlcount:integer;
  i: Integer;
begin
result:=False;
urlcount:=0;
//nexturl:
url:= GetPretimeURL;
if url = '' then begin
debug(dpSpam, section, 'URL value is empty');
ready:= True;
Result:= True;
exit;
end;

response:= TstringList.Create;
response.text:= slUrlGet(Format(url, [mainpazo.rls.rlsname]));
debug(dpSpam, section, 'Pretime results for %s'+#13#10+'%s', [mainpazo.rls.rlsname, response.text]);
prex:=TRegexpr.Create;
prex.ModifierM:=True;
prex.Expression:='(\S+) (\S+) (\S+) (\S+) (\S+)$';
for i := 0 to response.Count - 1 do
if prex.Exec(response.strings[i]) then begin
vctime:=PrepareTimestamp(strtoint(prex.Match[2]));
//vctime:=PrepareOffSet(strtoint(prex.Match[2]));
mainpazo.rls.cpretime:=vctime;
mainpazo.rls.pretime:=UnixToDateTime(vctime);
result:=True;
end else begin
urlcount:=urlcount+1;
//goto nexturl;
end;
prex.free;
response.free;
end;


function FetchTimeFromMYSQL(rls:TRelease):boolean;
begin
  result:=True;
end;


function TPazoPretimeLookupTask.Name: string;
begin
  try
    Result:= Format('<b>::PRETIME:</b> %s [ID:%d] [Round:%d]',[mainpazo.rls.rlsname,mainpazo.pazo_id,attempt]);
  except
    Result:= 'PRETIME';
  end;
end;


function PrepareTimestamp(TimeStamp: Int64): Int64;
var
  vof: TSLOffset;
begin
  result := TimeStamp;
  vof := TSLOffset.Create;
  if vof.ReCalcTimeStamp(TimeStamp) then result := vof.NewTimeStamp;
  vof.Free;
end;

constructor TSLOffset.Create;
begin
  fvalue := config.ReadString(section,'offset','0');
end;

function TSLOffset.ReCalcTimeStamp(oldtime: Int64): Boolean;
var
  r: TRegexpr;
  vval: Int64;
begin
  result := False;
  fNewtime := fOldtime;
  if fvalue = '0' then exit;

  r := TRegexpr.Create;
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
    result := True;
  end;
  r.Free;
end;


end.
