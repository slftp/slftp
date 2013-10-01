unit ranksunit;

interface

uses pazo, SyncObjs, Contnrs;

type
  TRankStat = class
    sitename, section: string;
    score: Integer;
    function ToString: string;
    constructor Create(const sitename, section: string; const score: Integer); overload;
    constructor Create(line: string); overload;
  end;

procedure RankStatAdd(const sitename, section: string; const score: Integer); overload;
procedure RankStatAdd(s: TRankStat); overload;

function DeleteRanks(sitename:string):boolean;


procedure RanksInit;
procedure RanksUnInit;
procedure RanksSave;
procedure RanksStart;
procedure RanksRecalc(const netname, channel: string);
procedure RanksProcess(p: TPazo);

function RanksReload:boolean;
function RemoveRanks(sitename:string):boolean;

var ranks_last_save: TDateTime;
    ranks_last_process: TDateTime;
    ranks: TObjectList;

implementation

uses Classes, irc, sitesunit, Debugunit, SysUtils,
     mystrings, configunit, encinifile, UIntList, DateUtils, RegExpr;

const r_section = 'ranks';

var rankslock: TCriticalSection;


function DeleteRanks(sitename:string):boolean;
var
  I: Integer;
begin
  result:=False;
  try
    for I := ranks.Count - 1 downto 0 do
      if TRankStat(ranks.Items[i]).sitename = sitename then
        ranks.Delete(i);
  except
    exit;
  end;
  result:=True;
end;


function RemoveRanks(sitename:string):boolean;
var i: Integer;
begin
  result:=False;
  try
    for I := ranks.Count - 1 downto 0 do
      if TRankStat(ranks.Items[i]).sitename = sitename then
        ranks.Delete(i);
  except
    exit;
  end;
  result:=True;
end;

(*
function RemoveRanks(sitename:string):boolean;
var x:TEncStringlist; i:Integer;
begin
result:=False;
x:=TEncstringlist.create(passphrase);
rankslock.Enter;
x.LoadFromFile(ExtractFilePath(ParamStr(0))+'slftp.ranks');
x.BeginUpdate;
try
for I := 0 to x.Count - 1 do begin
if pos(sitename,x.Strings[i]) = 1 then x.Delete(i);
end;
x.SaveToFile(ExtractFilePath(ParamStr(0))+'slftp.ranks');
result:=True;
finally
x.EndUpdate;
x.free;
end;
rankslock.Leave;
end;
 *)

function RanksReload:boolean;
begin
  result:=False;
  try
    rankslock.Enter;
    try
      ranks.clear;
      RanksStart;
    finally
      rankslock.Leave;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, r_section, Format('[EXCEPTION] RanksReload:boolean: %s', [e.Message]));
    end;
  end;
  result:=True;
end;

procedure RanksInit;
begin
  rankslock:= TCriticalSection.Create;
  ranks_last_save:= Now;
  ranks_last_process:= Now;
  ranks:= TObjectList.Create;
end;
procedure RanksUnInit;
begin
  Debug(dpSpam, r_section, 'Uninit1');
  ranks.Free;
  rankslock.Free;
  Debug(dpSpam, r_section, 'Uninit2');
end;

procedure RanksSave;
var x: TEncStringList;
    i: Integer;
begin
  debug(dpMessage, r_section, '--> RanksSave');
  //irc_Addconsole('--> RanksSave : '+FormatDateTime('hh:nn:ss', now));

  try
    x:= TEncStringlist.Create(passphrase);
    try
      for i:= ranks.Count-1 downto 0 do
      begin
        try
          x.Add(TRankStat(ranks[i]).ToString);
        except
          Continue;
        end;
      end;
      x.SaveToFile(ExtractFilePath(ParamStr(0))+'slftp.ranks');
      ranks_last_save:= Now;
    finally
      x.Free;
    end;
  except
    on E: Exception do
    begin
      ranks_last_save:= Now;
      Debug(dpError, r_section, Format('[EXCEPTION] RanksSave : %s', [e.Message]));
    end;
  end;

  //irc_Addconsole('<-- RanksSave : '+FormatDateTime('hh:nn:ss', now));
  debug(dpMessage, r_section, '<-- RanksSave');
end;


function FindSite(const s: string;const section:string): Boolean; overload;
begin
  result:=False;
  if '' =   sitesdat.ReadString('site-'+s, 'username', '') then exit;
  if True = sitesdat.ReadBool('site-'+s, section+'-ranklock', False) then exit;
  Result:= True;
end;

function FindSite(const s: string): Boolean; overload;
begin
  Result:= False;
  if '' =   sitesdat.ReadString('site-'+s, 'username', '') then exit;
  if True = sitesdat.ReadBool('site-'+s, 'ranklock', False) then exit;
  Result:= True;
end;

function NewAverage(const sitename, section: string): Integer;
var sumvalue, db, i: Integer;
    r: TRankStat;
begin
  Result:= 0;
  try
    db:= 0;
    sumvalue:= 0;
    for i:= 0 to ranks.Count -1 do
    begin
      r:= TRankStat(ranks[i]);
      if (r.sitename = sitename) and (r.section = section) then
      begin
        inc(db);
        inc(sumvalue, r.score);
      end;
    end;
    if db > 0 then
      Result:= Round(sumvalue / db);
  except
    on E: Exception do
    begin
      Debug(dpError, r_section, Format('[EXCEPTION] NewAverage : %s', [e.Message]));
      Result:= 0;
    end;
  end;
end;

procedure RanksProcess(p: TPazo);
var i: Integer;
    ps: TPazoSite;
    fcomplete, lcomplete: TDateTime;
    db: Integer;
    d, diff, minduration, maxduration: Integer;
    ranknew: Integer;
begin
  db:= 0;
  fcomplete:= 0;
  lcomplete:= 0;

  for i:= 0 to p.sites.Count-1 do
  begin
    try if i > p.sites.Count then Break; except Break; end;
    try
      ps:= TPazoSite(p.sites[i]);
      if (ps.dirlist = nil) then Continue;

      if ((ps.dirlist <> nil) and (ps.dirlist.date_completed <> 0)) then
      begin
        if ((fcomplete = 0) or (fcomplete > ps.dirlist.date_completed)) then
          fcomplete:= ps.dirlist.date_completed;
        if ((lcomplete = 0) or (lcomplete < ps.dirlist.date_completed)) then
          lcomplete:= ps.dirlist.date_completed;

        inc(db);
      end;
    except
      Continue;
    end;
  end;

  if (lcomplete = 0) or (fcomplete = 0) then exit; // if we didnt catch any announces
  if db < Round(p.sites.Count * config.ReadInteger(r_section, 'percent_of_sites_to_score', 30) / 100) then exit; // if we have not enough sites

  minduration:= MillisecondsBetween(fcomplete, p.added);
  maxduration:= MillisecondsBetween(lcomplete, p.added);

  diff:= maxduration - minduration;
  if diff <= 0 then exit; // division by zero is fucked

  // minduration  .. maxduration
  // 9               1

  for i:= 0 to p.sites.Count-1 do
  begin
    try if i > p.sites.Count then Break; except Break; end;
    try
      ps:= TPazoSite(p.sites[i]);
      if ((ps.dirlist <> nil) and (ps.dirlist.date_completed <> 0)) then
      begin
        d:= Millisecondsbetween(ps.dirlist.date_completed, p.added);
        ranknew:= Round((d - minduration) / diff * 8)+1;
        rankstatAdd(ps.name, p.rls.section, ranknew);
      end;
    except
      Continue;
    end;
  end;
end;

procedure RanksRecalc(const netname, channel: string);
var i, oa, na, rl: Integer;
    s, sitename: string;
    r: TRankStat;
begin
  Debug(dpMessage, r_section, '--> Recalculating rank stats');
  //irc_Addconsole('--> RanksRecalc : '+FormatDateTime('hh:nn:ss', now));

  ranks_last_process:= Now;
  try

      for i:= 0 to ranks.Count -1 do
      begin
        try if i > ranks.Count then Break; except Break; end;
        try
          r:= TRankStat(ranks[i]);
          sitename:= r.sitename;
          s:= r.section;
        except Break; end;

        rl:= sitesdat.ReadInteger('site-'+sitename, 'ranklock-'+s, 0);
        if rl > 0 then continue;
        rl:= sitesdat.ReadInteger('site-'+sitename, 'ranklock', 0);
        if rl > 0 then continue;

        na:= NewAverage(sitename, s);
        if na = 0 then na := 1;

        oa:= sitesdat.ReadInteger('site-'+sitename, 'rank-'+s, 1);
        if na <> oa then
        begin
          sitesdat.WriteInteger('site-'+sitename, 'rank-'+s, na);
          irc_SendRANKSTATS(Format('Changing rank of %s %s from %d to %d', [sitename, s, oa, na]));
        end;
      end;

  except
    on E: Exception do
    begin
      Debug(dpError, r_section, '[EXCEPTION] RanksRecalc: %s', [e.Message]);
    end;
  end;

  //irc_Addconsole('<-- RanksRecalc : '+FormatDateTime('hh:nn:ss', now));
  Debug(dpMessage, r_section, '<-- Recalculating rank stats');
end;

procedure RanksStart;
var x: TEncStringList;
    i: Integer;
    s: TRankStat;
begin
  x:= TEncStringlist.Create(passphrase);
  rankslock.Enter;
  try
    x.LoadFromFile(ExtractFilePath(ParamStr(0))+'slftp.ranks');
    for i:= 0 to x.Count-1 do
    begin
      s:= TRankStat.Create(x[i]);
      RankStatAdd(s);
    end;
  finally
    x.Free;
    rankslock.Leave;
  end;
end;

{ TRankStat }

constructor TRankStat.Create(line: string);
begin
  sitename:= Fetch(line, ' ');
  section:= Fetch(line, ' ');
  score:= StrToIntDef( Fetch(line, ' '), 0 );
end;

constructor TRankStat.Create(const sitename, section: string; const score: Integer);
begin
  self.sitename:= sitename;
  self.section:= section;
  self.score:= score;
end;

function TRankStat.ToString: string;
begin
  Result:= Format('%s %s %d', [sitename, section, score]);
end;

procedure RankStatAdd(const sitename, section: string; const score: Integer); overload;
var s: TRankStat;
begin
  s:= TRankStat.Create;
  s.sitename:= sitename;
  s.section:= section;
  s.score:= score;
  RankStatAdd(s);
end;

procedure RankStatAdd(s: TRankStat); overload;
var max_entries: Integer;
begin
  debug(dpSpam, r_section, 'Rankstat %s -> %s %d', [s.sitename, s.section, s.score]);
  max_entries:= config.readInteger(r_section, 'max_entries', 1000);

  try
    rankslock.Enter;
    try
      ranks.Add(s);
      while (ranks.Count > max_entries) do
      begin
        ranks.Delete(0);
      end;
    finally
      rankslock.Leave;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, r_section, '[EXCEPTION] RankStatAdd: %s', [e.Message]);
    end;
  end;
end;

end.
