unit speedstatsunit;

interface

uses Contnrs;

type
  TSpeedStat = class
    section, rlsname, src, dst: AnsiString;
    speed: Double;
    function ToString: AnsiString;
    function ToStringEx: AnsiString;
    constructor Create(src, dst: AnsiString; speed: Double); overload;
    constructor Create(src, dst: AnsiString; speed: Double; rip, section: AnsiString); overload;
    constructor Create(line: AnsiString); overload;
  end;

procedure SpeedStatAdd(src, dst: AnsiString; speed: Double; section, rip: AnsiString); overload;
procedure SpeedStatAdd(s: TSpeedStat; nolog : Boolean = False); overload;

procedure SpeedStatsInit;
procedure SpeedStatsUnInit;
procedure SpeedStatsSave;
procedure SpeedStatsStart;
procedure SpeedStatsRecalc(const netname, channel: AnsiString);
procedure SpeedStatsShowStats(const netname, channel, sitename: AnsiString); overload;
procedure SpeedStatsShowStats(const netname, channel, sitename, section: AnsiString); overload;
procedure SpeedStatsShowStats(const netname, channel, sitename, section, rlsname: AnsiString); overload;

procedure RemoveSpeedStats(const sitename, section: AnsiString);

function SpeedStatsScale(const toscale: Double): Integer;

var
  speedstats_last_save: TDateTime;
  speedstats_last_recalc: TDateTime;
  speedstats: TObjectList;

implementation

uses Classes, SyncObjs, irc, sitesunit, Debugunit, SysUtils, mystrings, configunit, encinifile, UIntList;

const
  r_section = 'speedstats';

var
  speedstatlock: TCriticalSection;

procedure SpeedStatsInit;
begin
  speedstatlock := TCriticalSection.Create;
  speedstats_last_save := Now;
  speedstats_last_recalc := Now;
  speedStats := TObjectList.Create;
end;

procedure SpeedStatsUnInit;
begin
  Debug(dpSpam, r_section, 'Uninit1');
  speedStats.Free;
  speedstatlock.Free;
  Debug(dpSpam, r_section, 'Uninit2');
end;

procedure SpeedStatsSave;
var
  x: TEncStringList;
  i: Integer;
begin
  debug(dpMessage, r_section, '--> Saving speedstats');
  //irc_Addconsole('--> SpeedStatsSave : '+FormatDateTime('hh:nn:ss', now));

  speedstatlock.Enter;
  try
    try
      // Cleanup

      // Save
      x := TEncStringlist.Create(passphrase);
      try
        for i := speedstats.Count - 1 downto 0 do
        begin
          if ((TSpeedStat(speedstats.Items[i]).section <> '') or (TSpeedStat(speedstats.Items[i]).rlsname <> '')) then
            x.Add(TSpeedStat(speedstats[i]).ToStringEx)
          else
            x.Add(TSpeedStat(speedstats[i]).ToString);
        end;
        x.SaveToFile(ExtractFilePath(ParamStr(0)) + 'slftp.speedstats');
      finally
        x.Free;
      end;
    except
      on E: Exception do
      begin
        Debug(dpError, r_section, '[EXCEPTION] SpeedStatsSave: %s', [e.Message]);
      end;
    end;
  finally
    speedstatlock.Leave;
  end;

  speedstats_last_save:= Now;
  //irc_Addconsole('<-- SpeedStatsSave : '+FormatDateTime('hh:nn:ss', now));
  debug(dpMessage, r_section, '<-- Saving speedstats');
end;

function AvgSpeedByReleasename(const rip: AnsiString): Double;
var
  i, db: Integer;
  sum: Double;
  s: TSpeedStat;
begin
  sum := 0;
  db := 0;
  Result := 0;

  try
    for i := speedstats.Count - 1 downto 0 do
    begin
      s := TSpeedStat(speedstats[i]);
      if s.rlsname = rip then
      begin
        sum := sum + s.speed;
        inc(db);
      end;
    end;
    if db <> 0 then
    begin
      Result := sum / db / 1024; // kilobyte / sec
    end;
  except
    Result := 0;
  end;

end;

function AvgSpeedBySection(const section: AnsiString): Double;
var
  i, db: Integer;
  sum: Double;
  s: TSpeedStat;
begin
  sum := 0;
  db := 0;
  Result := 0;

  try
    for i := speedstats.Count - 1 downto 0 do
    begin
      s := TSpeedStat(speedstats[i]);
      if s.section = section then
      begin
        sum := sum + s.speed;
        inc(db);
      end;
    end;
    if db <> 0 then
    begin
      Result := sum / db / 1024; // kilobyte / sec
    end;
  except
    Result := 0;
  end;

end;

function AvgSpeed(const src, dst: AnsiString): Double; overload;
var
  i, db: Integer;
  sum: Double;
  s: TSpeedStat;
begin
  sum := 0;
  db := 0;
  Result := 0;

  try
    for i := speedstats.Count - 1 downto 0 do
    begin
      s := TSpeedStat(speedstats[i]);
      if ((s.src = src) and (s.dst = dst)) then
      begin
        if (s.speed > 0) then
          sum := sum + s.speed;
        inc(db);
      end;
    end;
    if db <> 0 then
    begin
      Result := sum / db / 1024; // kilobyte / sec
    end;
  except
    Result := 0;
  end;

end;

function AvgSpeed(const src, dst, section: AnsiString): Double; overload;
var
  i, db: Integer;
  sum: Double;
  s: TSpeedStat;
begin
  sum := 0;
  db := 0;
  Result := 0;

  try
    for i := speedstats.Count - 1 downto 0 do
    begin
      s := TSpeedStat(speedstats[i]);
      if ((s.src = src) and (s.dst = dst) and (s.section = section)) then
      begin
        if (s.speed > 0) then
          sum := sum + s.speed;
        inc(db);
      end;
    end;
    if db <> 0 then
    begin
      Result := sum / db / 1024; // kilobyte / sec
    end;
  except
    Result := 0;
  end;

end;

function AvgSpeed(const src, dst, section, rip: AnsiString): Double; overload;
var
  i, db: Integer;
  sum: Double;
  s: TSpeedStat;
begin
  sum := 0;
  db := 0;
  Result := 0;

  try
    for i := speedstats.Count - 1 downto 0 do
    begin
      s := TSpeedStat(speedstats[i]);
      if ((s.src = src) and (s.dst = dst) and (s.section = section) and (s.rlsname = rip)) then
      begin
        if (s.speed > 0) then
          sum := sum + s.speed;
        inc(db);
      end;
    end;
    if db <> 0 then
    begin
      Result := sum / db / 1024; // kilobyte / sec
    end;
  except
    Result := 0;
  end;
end;


procedure AddSites(const src, dst: AnsiString; x: TStringList);
var
  i: Integer;
  s: TSpeedStat;
begin
  x.Clear;

  try
    for i := speedstats.Count - 1 downto 0 do
    begin
      s := TSpeedStat(speedstats[i]);
      if ((dst = '') and (s.src = src)) then
      begin
        if x.IndexOf(s.dst) = -1 then
          x.Add( s.dst );
      end
      else if ((src = '') and (s.dst = dst)) then
      begin
        if x.IndexOf(s.src) = -1 then
          x.Add( s.src );
      end
      else if (src ='') and (dst = '') then
      begin
        if x.IndexOf(s.src) = -1 then
          x.Add( s.src );
        if x.IndexOf(s.dst) = -1 then
          x.Add( s.dst );
      end;
    end;
  except
    x.Clear;
  end;
end;


function FindSite(const s: AnsiString): Boolean;
begin
  Result := False;

  if sitesdat.ReadString('site-' + s, 'username', '') = '' then
    exit;

  Result := True;
end;

procedure SpeedStatsRecalc(const netname, channel: AnsiString);
var
  i, j, speed_old, speed_new: Integer;
  min_speed, max_speed: Double;
  x, y: TStringList;
  d, diff: Double;
  minr: AnsiString;
  maxr: AnsiString;
begin
  speedstats_last_recalc := Now();

  Debug(dpMessage, r_section, 'Recalculating speed stats begin');
  //irc_Addconsole('--> SpeedStatsRecalc : '+FormatDateTime('hh:nn:ss', now));

  x := TStringList.Create;
  y := TStringList.Create;
  try
    AddSites('', '', x);

    // eloszor a kimeno sebessegeket rekalibraljuk
    minr := '';
    maxr := '';
    min_speed := 1000000000;
    max_speed := 0;
    for i := 0 to x.Count - 1 do
    begin
      AddSites(x[i], '', y);
      for j := 0 to y.Count - 1 do
      begin
        d := AvgSpeed(x[i], y[j]);
        if d < min_speed then
        begin
          min_speed := d;
          minr := x[i] + '->' + y[j];
        end;
        if d > max_speed then
        begin
          max_speed := d;
          maxr := x[i] + '->' + y[j];
        end;
      end;
    end;

    if max_speed > min_speed then
    begin
      if ((netname = 'CONSOLE') and (channel = 'SPEEDSTATS')) then
      begin
        irc_SendSPEEDSTATS(Format('Min speed %.1f (%s), max is %.1f (%s)', [min_speed, minr, max_speed, maxr]));
      end
      else
      begin
        irc_addtext(netname, channel, 'Min speed %.1f (%s), max is %.1f (%s)', [min_speed, minr, max_speed, maxr]);
      end;

      diff := max_speed - min_speed;
      for i := 0 to x.Count - 1 do
      begin
        if not FindSite(x[i]) then Continue;

        AddSites(x[i], '', y);
        for j := 0 to y.Count - 1 do
        begin
          if not FindSite(y[j]) then Continue;

          if (sitesdat.ValueExists('speedlock-from-' + x[i], y[j])) then
          begin
            irc_SendSPEEDSTATS(Format('Route locked %s -> %s %d', [x[i], y[j], sitesdat.ReadInteger('speed-from-' + x[i], y[j], 0)]));
            Continue;
          end;

          d := AvgSpeed(x[i], y[j]);
          if d <> 0 then
          begin

            speed_old := sitesdat.ReadInteger('speed-from-' + x[i], y[j], 0);
            if speed_old <> 0 then
            begin
              speed_new := Round((d - min_speed) / diff * 8) + 1;
              if ((speed_new >= 1) and (speed_new <= 9) and (speed_new <> speed_old)) then
              begin
                if ((netname = 'CONSOLE') and (channel = 'SPEEDSTATS')) then
                begin
                  irc_SendSPEEDSTATS(Format('Changing route %s -> %s from %d to %d', [x[i], y[j], speed_old, speed_new]));
                end else
                begin
                  irc_addtext(netname, channel, 'Changing route %s -> %s from %d to %d', [x[i], y[j], speed_old, speed_new]);
                end;

                sitesdat.WriteInteger('speed-from-' + x[i], y[j], speed_new);
                sitesdat.WriteInteger('speed-to-' + y[j], x[i], speed_new);
              end;
            end;
          end;
        end;
      end;

    end;
  finally
    y.Free;
    x.Free;
  end;

  //irc_Addconsole('<-- SpeedStatsRecalc : '+FormatDateTime('hh:nn:ss', now));
  Debug(dpMessage, r_section, 'Recalculating speed stats end');
end;

procedure SpeedStatsShowStats(const netname, channel, sitename: AnsiString); overload;
var
  x: TStringList;
  i: Integer;
  db: Integer;
  d: Double;
  ss: AnsiString;
begin
  irc_addtext(netname, channel, 'SpeedStats for <b>%s</b> :',[sitename]);

  x := TStringList.Create;
  try
    AddSites(sitename, '', x);

    // majd megjelenitjuk
    db := 0;
    ss := '';
    for i := 0 to x.Count - 1 do
    begin
      d := AvgSpeed(sitename, x[i]);
      if d = 0 then Continue;

      if ss <> '' then ss := ss + ', ';

      if d > 1024 then
        ss := ss + Format('%s %.1f mB/s', [x[i], d/1024])
      else
        ss := ss + Format('%s %.1f kB/s', [x[i], d]);

      inc(db);
      if db >= 10 then
      begin
        irc_addtext(netname, channel, '%s -> %s', [sitename, ss]);
        db := 0;
        ss := '';
      end;
    end;

    if ss <> '' then
      irc_addtext(netname, channel, '%s -> %s', [sitename, ss]);


    // most osszes destination sitet
    AddSites('', sitename, x);

    // majd ezt is
    db := 0;
    ss := '';
    for i := 0 to x.Count - 1 do
    begin
      d:= AvgSpeed(x[i], sitename);
      if d = 0 then Continue;

      if ss <> '' then ss := ss + ', ';

      if d > 1024 then
        ss := ss + Format('%s %.1f mB/s', [x[i], d/1024])
      else
        ss := ss + Format('%s %.1f kB/s', [x[i], d]);

      inc(db);
      if db >= 10 then
      begin
        irc_addtext(netname, channel, '%s <- %s', [sitename, ss]);
        db := 0;
        ss := '';
      end;
    end;

    if ss <> '' then
      irc_addtext(netname, channel, '%s <- %s', [sitename, ss]);

  finally
    x.Free;
  end;
end;

procedure SpeedStatsShowStats(const netname, channel, sitename, section: AnsiString); overload;
var
  x: TStringList;
  i: Integer;
  db: Integer;
  d: Double;
  ss: AnsiString;
begin
  irc_addtext(netname, channel, 'SpeedStats for <b>%s %s</b> :',[sitename, section]);

  x := TStringList.Create;
  try
    AddSites(sitename, '', x);

    // majd megjelenitjuk
    db := 0;
    ss := '';
    for i:= 0 to x.Count -1 do
    begin
      d := AvgSpeed(sitename, x[i],section);
      if d = 0 then Continue;

      if ss <> '' then ss := ss + ', ';

      if d > 1024 then
        ss := ss + Format('%s %.1f mB/s', [x[i], d/1024])
      else
        ss := ss + Format('%s %.1f kB/s', [x[i], d]);

      inc(db);
      if db >= 10 then
      begin
        irc_addtext(netname, channel, '%s -> %s',[sitename, ss]);
        db := 0;
        ss := '';
      end;
    end;

    if ss <> '' then
      irc_addtext(netname, channel, '%s -> %s',[sitename, ss]);


    // most osszes destination sitet
    AddSites('', sitename, x);

    // majd ezt is
    db := 0;
    ss := '';
    for i := 0 to x.Count - 1 do
    begin
      d := AvgSpeed(x[i], sitename, section);
      if d = 0 then Continue;

      if ss <> '' then ss := ss + ', ';

      if d > 1024 then
        ss := ss + Format('%s %.1f mB/s', [x[i], d/1024])
      else
        ss := ss + Format('%s %.1f kB/s', [x[i], d]);

      inc(db);
      if db >= 10 then
      begin
        irc_addtext(netname, channel, '%s <- %s',[sitename, ss]);
        db := 0;
        ss := '';
      end;
    end;

    if ss <> '' then
      irc_addtext(netname, channel, '%s <- %s',[sitename, ss]);

  finally
    x.Free;
  end;
end;

procedure SpeedStatsShowStats(const netname, channel, sitename, section, rlsname: AnsiString); overload;
var
  x: TStringList;
  i: Integer;
  db: Integer;
  d: Double;
  ss: AnsiString;
begin
  irc_addtext(netname, channel, 'SpeedStats for <b>%s %s %s</b> :',[sitename, section, rlsname]);

  x := TStringList.Create;
  try
    AddSites(sitename, '', x);

    // majd megjelenitjuk
    db := 0;
    ss := '';
    for i := 0 to x.Count - 1 do
    begin
      d := AvgSpeed(sitename, x[i], section, rlsname);
      if d = 0 then Continue;

      if ss <> '' then ss := ss + ', ';

      if d > 1024 then
        ss := ss + Format('%s %.1f mB/s', [x[i], d/1024])
      else
        ss := ss + Format('%s %.1f kB/s', [x[i], d]);

      inc(db);
      if db >= 10 then
      begin
        irc_addtext(netname, channel, '%s -> %s',[sitename, ss]);
        db := 0;
        ss := '';
      end;
    end;

    if ss <> '' then
      irc_addtext(netname, channel, '%s -> %s',[sitename, ss]);


    // most osszes destination sitet
    AddSites('', sitename, x);

    // majd ezt is
    db := 0;
    ss := '';
    for i := 0 to x.Count - 1 do
    begin
      d := AvgSpeed(x[i], sitename, section, rlsname);
      if d = 0 then Continue;

      if ss <> '' then ss := ss+', ';
      if d > 1024 then
        ss := ss + Format('%s %.1f mB/s', [x[i], d/1024])
      else
        ss := ss + Format('%s %.1f kB/s', [x[i], d]);

      inc(db);
      if db >= 10 then
      begin
        irc_addtext(netname, channel, '%s <- %s',[sitename, ss]);
        db := 0;
        ss := '';
      end;
    end;
    if ss <> '' then
      irc_addtext(netname, channel, '%s <- %s',[sitename, ss]);

  finally
    x.Free;
  end;
end;

procedure SpeedStatsStart;
var
  x: TEncStringList;
  i: Integer;
  s: TSpeedStat;
begin
  x := TEncStringlist.Create(passphrase);
  try
    speedstatlock.Enter;
    try
      x.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'slftp.speedstats');
      for i := 0 to x.Count - 1 do
      begin
        s := TSpeedStat.Create(x[i]);
        SpeedStatAdd(s, True);
      end;
    finally
      speedstatlock.Leave;
    end;
  finally
    x.Free;
  end;
end;

{ TSpeedStat }
constructor TSpeedStat.Create(line: AnsiString);
begin
  src := Fetch(line, ' ');
  dst := Fetch(line, ' ');
  speed := myStrToFloat( Fetch(line, ' '), 0 );
  rlsname := Fetch(line, ' ');
  section := Fetch(line, ' ');
end;

constructor TSpeedStat.Create(src, dst: AnsiString; speed: Double);
begin
  self.src := src;
  self.dst := dst;
  self.speed := speed;
end;

constructor TSpeedStat.Create(src: AnsiString; dst: AnsiString; speed: Double; rip: AnsiString; section: AnsiString);
begin
  self.src := src;
  self.dst := dst;
  self.speed := speed;
  self.section :=section;
  self.rlsname :=rip;
end;

function TSpeedStat.ToString: AnsiString;
begin
  try
    Result := FormaT('%s %s %f', [src, dst, speed]);
  except
    Result := '';
  end;
end;

function TSpeedStat.ToStringEx:AnsiString;
begin
  try
    Result := FormaT('%s %s %f %s %s', [src, dst, speed, section, rlsname]);
  except
    Result := '';
  end;
end;

procedure SpeedStatAdd(const src, dst: AnsiString; const speed: Double); overload;
var
  s: TSpeedStat;
begin
  try
    if (speed > 0) then
    begin
      s := TSpeedStat.Create;
      s.src := src;
      s.dst := dst;
      s.speed := speed;
      SpeedStatAdd(s);
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, r_section, '[EXCEPTION] SpeedStatAdd: %s', [e.Message]);
    end;
  end;
end;

procedure SpeedStatAdd(src, dst: AnsiString; speed: Double; section, rip: AnsiString); overload;
var
  s: TSpeedStat;
begin
  try
    if (speed > 0) then
    begin
      s := TSpeedStat.Create;
      s.src := src;
      s.dst := dst;
      s.speed := speed;
      s.rlsname := rip;
      s.section := section;
      SpeedStatAdd(s);
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, r_section, '[EXCEPTION] SpeedStatAdd: %s', [e.Message]);
    end;
  end;
end;

procedure SpeedStatAdd(s: TSpeedStat; nolog : Boolean = False); overload;
var
  max_entries: Integer;
begin
  if not nolog then
    debug(dpSpam, r_section, 'Speedstat %s -> %s %.1f', [s.src, s.dst, s.speed]);

  max_entries := config.readInteger(r_section, 'max_entries', 5000);

  try
    speedstatlock.Enter;
    try
      speedstats.Add(s);
      while (speedstats.Count > max_entries) do
      begin
        speedstats.Delete(0);
      end;
    finally
      speedstatlock.Leave;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, r_section, '[EXCEPTION] SpeedStatAdd: %s', [e.Message]);
    end;
  end;
end;

procedure RemoveSpeedStats(const sitename, section: AnsiString);
var
  i: integer;
begin
  speedstatlock.Enter;
  try
    for i := 0 to speedstats.Count - 1 do
    begin
      if (((TSpeedStat(speedstats.Items[i]).src = sitename) or (TSpeedStat(speedstats.Items[i]).dst = sitename)) and (TSpeedStat(speedstats.Items[i]).section = section)) then
        speedstats.Delete(i);
    end;
  finally
    speedstatlock.Leave;
  end;
  SpeedStatsSave;
end;

function SpeedStatsScale(const toscale: Double): Integer;
var
  i, j: Integer;
  min_speed, max_speed: Double;
  x, y: TStringList;
  d, diff: Double;
begin
  Result := 0;

  x := TStringList.Create;
  y := TStringList.Create;
  try
    AddSites('', '', x);

    // eloszor a kimeno sebessegeket rekalibraljuk
    min_speed := 1000000000;
    max_speed := 0;
    for i := 0 to x.Count - 1 do
    begin
      AddSites(x[i], '', y);
      for j := 0 to y.Count - 1 do
      begin
        d := AvgSpeed(x[i], y[j]);
        if d < min_speed then
        begin
          min_speed := d;
        end;
        if d > max_speed then
        begin
          max_speed := d;
        end;
      end;
    end;

    if max_speed > min_speed then
    begin
      if ((max_speed > toscale) and (min_speed < toscale)) then
      begin
        diff := max_speed - min_speed;
        Result := Round((toscale - min_speed) / diff * 8) + 1;
      end
      else
      if max_speed < toscale then
        Result := 9
      else
      if min_speed > toscale then
        Result := 1;
    end;
  finally
    y.Free;
    x.Free;
  end;
end;

end.
