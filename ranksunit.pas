unit ranksunit;

interface

uses pazo, SyncObjs, Contnrs;

type
  TRankStat = class
    sitename, section: String;
    score: Integer;
    function ToString: String; override;
    constructor Create(const sitename, section: String; const score: Integer); overload;
    constructor Create(line: String); overload;
  end;

procedure RankStatAdd(const sitename, section: String; const score: Integer); overload;
procedure RankStatAdd(s: TRankStat); overload;

procedure RanksInit;
procedure RanksUnInit;
procedure RanksSave;
procedure RanksStart;

procedure RanksProcess(p: TPazo);

{ Calculates the rank stats based on all @link(ranks)
  @param(aNetname Name if the IRC network)
  @param(aChannel Channelname) }
procedure RanksRecalc(const aNetname, aChannel: String);

{ Removes all @link(TRankStat) for given @link(aSitename) from @link(ranks) list
  @param(aSitename name of site which TRankStat should be deleted)
  @returns(@true if successful, @false on exception) }
function RemoveRanks(const aSitename: String): boolean; overload;

{ Removes all @link(TRankStat) from @link(ranks) where given @link(aSitename) and @link(aSection) matches exactly the values from @link(TRankStat)
  @param(aSitename name of site which TRankStat should be deleted)
  @param(aSection name of section)
  @returns(@true if successful, @false on exception) }
function RemoveRanks(const aSitename, aSection: String): boolean; overload;

function RanksReload: boolean;

var
  ranks_last_save: TDateTime;
  ranks_last_process: TDateTime;
  ranks: TObjectList;

implementation

uses
  Classes, irc, sitesunit, Debugunit, SysUtils, configunit, encinifile, DateUtils, IdGlobal;

const
  r_section = 'ranks';

var
  rankslock: TCriticalSection;

function RemoveRanks(const aSitename: String): boolean;
var
  i: Integer;
begin
  Result := False;
  try
    rankslock.Enter;
    try
      for i := ranks.Count - 1 downto 0 do
        if TRankStat(ranks.Items[i]).sitename = aSitename then
          ranks.Delete(i);
    finally
      rankslock.Leave;
    end;
  except
    exit;
  end;
  Result := True;
end;

function RemoveRanks(const aSitename, aSection: String): boolean;
var
  i: Integer;
  rank: TRankStat;
begin
  Result := False;
  try
    rankslock.Enter;
    try
      for i := ranks.Count - 1 downto 0 do
      begin
        rank := TRankStat(ranks.Items[i]);
        if ((rank.sitename = aSitename) and (rank.section = aSection)) then
          ranks.Delete(i);
      end;
    finally
      rankslock.Leave;
    end;
  except
    exit;
  end;
  Result := True;
end;

function RanksReload: boolean;
begin
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

  Result := True;
end;

procedure RanksInit;
begin
  rankslock := TCriticalSection.Create;
  ranks_last_save := Now;
  ranks_last_process := Now;
  ranks := TObjectList.Create;
end;

procedure RanksUnInit;
begin
  Debug(dpSpam, r_section, 'Uninit1');
  ranks.Free;
  rankslock.Free;
  Debug(dpSpam, r_section, 'Uninit2');
end;

procedure RanksSave;
var
  x: TEncStringList;
  i: Integer;
begin
  debug(dpMessage, r_section, '--> RanksSave');

  try
    x := TEncStringlist.Create(passphrase);
    try
      for i := ranks.Count - 1 downto 0 do
      begin
        try
          x.Add(TRankStat(ranks[i]).ToString);
        except
          Continue;
        end;
      end;
      x.SaveToFile(ExtractFilePath(ParamStr(0)) + 'slftp.ranks');
      ranks_last_save := Now;
    finally
      x.Free;
    end;
  except
    on E: Exception do
    begin
      ranks_last_save := Now;
      Debug(dpError, r_section, Format('[EXCEPTION] RanksSave : %s', [e.Message]));
    end;
  end;

  debug(dpMessage, r_section, '<-- RanksSave');
end;

function NewAverage(const sitename, section: String): Integer;
var
  sumvalue, db, i: Integer;
  r: TRankStat;
begin
  Result := 0;
  try
    db := 0;
    sumvalue := 0;

    for i := 0 to ranks.Count - 1 do
    begin
      r := TRankStat(ranks[i]);
      if (r.sitename = sitename) and (r.section = section) then
      begin
        inc(db);
        inc(sumvalue, r.score);
      end;
    end;

    if db > 0 then
      Result := Round(sumvalue / db);
  except
    on E: Exception do
    begin
      Debug(dpError, r_section, Format('[EXCEPTION] NewAverage : %s', [e.Message]));
      Result := 0;
    end;
  end;
end;

procedure RanksProcess(p: TPazo);
var
  i: Integer;
  ps: TPazoSite;
  fcomplete, lcomplete: TDateTime;
  db: Integer;
  d, diff, minduration, maxduration: Integer;
  ranknew: Integer;
begin
  db := 0;
  fcomplete := 0;
  lcomplete := 0;

  for i := 0 to p.PazoSitesList.Count - 1 do
  begin
    try if i > p.PazoSitesList.Count then Break; except Break; end;
    try
      ps := TPazoSite(p.PazoSitesList[i]);
      if (ps.dirlist = nil) then
        Continue;

      if ((ps.dirlist <> nil) and (ps.dirlist.CompletedTime <> 0)) then
      begin
        if ((fcomplete = 0) or (fcomplete > ps.dirlist.CompletedTime)) then
          fcomplete := ps.dirlist.CompletedTime;
        if ((lcomplete = 0) or (lcomplete < ps.dirlist.CompletedTime)) then
          lcomplete := ps.dirlist.CompletedTime;

        inc(db);
      end;
    except
      Continue;
    end;
  end;

  if (lcomplete = 0) or (fcomplete = 0) then exit; // if we didnt catch any announces
  if db < Round(p.PazoSitesList.Count * config.ReadInteger(r_section, 'percent_of_sites_to_score', 30) / 100) then exit; // if we have not enough sites

  minduration := MillisecondsBetween(fcomplete, p.added);
  maxduration := MillisecondsBetween(lcomplete, p.added);

  diff := maxduration - minduration;
  if diff <= 0 then exit; // division by zero is fucked

  // minduration  .. maxduration
  // 9               1

  for i := 0 to p.PazoSitesList.Count - 1 do
  begin
    try if i > p.PazoSitesList.Count then Break; except Break; end;
    try
      ps := TPazoSite(p.PazoSitesList[i]);
      if ((ps.dirlist <> nil) and (ps.dirlist.CompletedTime <> 0)) then
      begin
        d := Millisecondsbetween(ps.dirlist.CompletedTime, p.added);
        ranknew := Round((d - minduration) / diff * 8) + 1;
        rankstatAdd(ps.name, p.rls.section, ranknew);
      end;
    except
      Continue;
    end;
  end;
end;

procedure RanksRecalc(const aNetname, aChannel: String);
var
  i, fOldAvg, fNewAvg, fRankLockValue: Integer;
  fSection, fSitename: String;
  r: TRankStat;
  s: TSite;
begin
  Debug(dpMessage, r_section, '--> Recalculating rank stats');

  ranks_last_process := Now;
  try
    for i := 0 to ranks.Count - 1 do
    begin
      try if i > ranks.Count then Break; except Break; end;
      try
        r := TRankStat(ranks[i]);
        fSitename := r.sitename;
        s := findSiteByName(aNetname, fSitename);
        fSection := r.section;
      except
        Break;
      end;

      fRankLockValue := s.getRankLock(fSection);
      if fRankLockValue > 0 then
        continue;

      fNewAvg := NewAverage(fSitename, fSection);
      if fNewAvg = 0 then
        fNewAvg := 1;

      fOldAvg := sitesdat.ReadInteger('site-' + fSitename, 'rank-' + fSection, 1);
      if fNewAvg <> fOldAvg then
      begin
        sitesdat.WriteInteger('site-' + fSitename, 'rank-' + fSection, fNewAvg);
        irc_SendRANKSTATS(Format('Changing rank of %s %s from %d to %d', [fSitename, fSection, fOldAvg, fNewAvg]));
      end;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, r_section, '[EXCEPTION] RanksRecalc: %s', [e.Message]);
    end;
  end;

  Debug(dpMessage, r_section, '<-- Recalculating rank stats');
end;

procedure RanksStart;
var
  x: TEncStringList;
  i: Integer;
  s: TRankStat;
begin
  x := TEncStringlist.Create(passphrase);
  try
    rankslock.Enter;
    try
      x.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'slftp.ranks');
      for i := 0 to x.Count - 1 do
      begin
        s := TRankStat.Create(x[i]);
        RankStatAdd(s);
      end;
    finally
      rankslock.Leave;
    end;
  finally
    x.Free;
  end;
end;

{ TRankStat }

constructor TRankStat.Create(line: String);
begin
  sitename := Fetch(line, ' ', True, False);
  section := Fetch(line, ' ', True, False);
  score := StrToIntDef(Fetch(line, ' ', True, False), 0);
end;

constructor TRankStat.Create(const sitename, section: String; const score: Integer);
begin
  self.sitename := sitename;
  self.section := section;
  self.score := score;
end;

function TRankStat.ToString: String;
begin
  Result := Format('%s %s %d', [sitename, section, score]);
end;

procedure RankStatAdd(const sitename, section: String; const score: Integer); overload;
var
  s: TRankStat;
begin
  s := TRankStat.Create;
  s.sitename := sitename;
  s.section := section;
  s.score := score;
  RankStatAdd(s);
end;

procedure RankStatAdd(s: TRankStat); overload;
var
  max_entries: Integer;
begin
  debug(dpSpam, r_section, 'Rankstat %s -> %s %d', [s.sitename, s.section, s.score]);
  max_entries := config.readInteger(r_section, 'max_entries', 1000);

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
