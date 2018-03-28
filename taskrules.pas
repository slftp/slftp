unit taskrules;

interface

uses
  Classes, tasksunit;

type
  TRulesTask = class(TTask)
    constructor Create(const netname, channel: String; site: String);
    function Execute(slot: Pointer): Boolean; override;
    function Name: String; override;
  end;

implementation

uses
  sitesunit, SysUtils, DateUtils, mystrings, DebugUnit, Diff, HashUnit, encinifile, configunit, queueunit, irc, mrdohutils, news;

const
  section = 'taskrules';

{ TRulesTask }

constructor TRulesTask.Create(const netname, channel: String; site: String);
begin
  inherited Create(netname, channel, site);
end;

function TRulesTask.Execute(slot: Pointer): Boolean;
label
  ujra;
var
  s: TSiteSlot;
  cmd : String;
  t: TRulesTask;
  i: Integer;
  rules, ss: String;
  f: TStringlist;
  new_rules, old_rules: TStringList;
  hash_new_rules, hash_old_rules : TList;
  rules_path, rules_file: String;
  Diff: TDiff;
  numerrors: Integer;
begin
  Result := False;
  numerrors := 0;
  s := slot;
  Debug(dpMessage, section, Name);

ujra:
  inc(numerrors);
  if numerrors > 3 then
  begin
    readyerror := True;
    exit;
  end;

  if s.status <> ssOnline then
  begin
    if not s.ReLogin(1) then
    begin
      readyerror:= True;
      exit;
    end;
  end;

  cmd := 'SITE RULES';

  if (s.site.sslfxp = srNeeded) then
  begin
    if not s.SendProtP() then goto ujra;
  end
  else
  begin
    if not s.SendProtC() then goto ujra;
  end;

  if (not s.Send(cmd)) then
  begin
    readyerror := True;
    exit;
  end;

  if (not s.Read(cmd, true, true, 60000)) then
  begin
    readyerror := True;
    exit;
  end;

  if (s.lastResponseCode <> 200) then
  begin
    readyerror := True;
    exit;
  end;

  new_rules := TStringList.Create;
  old_rules := TStringList.Create;
  try
    rules := s.lastResponse;
    i := 1;
    while(true) do
    begin
      ss := SubString(rules, #13#10, i);
      if ss = '' then break;
      new_rules.Add(ss);
      inc(i);
    end;

    if (config.ReadBool('sites', 'split_site_data', True)) then
    begin
      rules_path := ExtractFilePath(ParamStr(0))+'rtpl';
      rules_file := rules_path+PathDelim+s.site.name+'.siterules';
    end
    else
    begin
      rules_path := ExtractFilePath(ParamStr(0))+'rules';
      rules_file := rules_path+PathDelim+s.site.name+'.rules';
    end;


    try
      ForceDirectories(rules_path);
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] TaskRules ForceDirectories Exception : %s', [e.Message]));
        readyerror := True;
        exit;
      end;
    end;

    f := TStringlist.Create();
    try

      if FileExists(rules_file) then
      begin
        f.LoadFromFile(rules_file);

        hash_old_rules := TList.Create;
        hash_new_rules := TList.Create;
        try

          for i := 0 to f.Count - 1 do
          begin
            old_rules.Add(f[i]);
            hash_old_rules.Add(HashLine(f[i], True, True));
          end;

          for i := 0 to new_rules.Count - 1 do
          begin
            hash_new_rules.Add(HashLine(new_rules[i], True, True));
          end;

          Diff := TDiff.Create;
          try
            try
              Diff.Execute(PInteger(hash_old_rules.list), PInteger(hash_new_rules.list), hash_old_rules.Count, hash_new_rules.Count);
            except
              on e: Exception do
              begin
                Debug(dpError, section, Format('[EXCEPTION] TaskRules Diff.Execute Exception : %s', [e.Message]));
                readyerror := True;
                exit;
              end;
            end;

            with Diff.DiffStats do
            begin
              if ((modifies > 0) or (adds > 0) or (deletes > 0)) then
              begin
                try
                  for i := 0 to Diff.Count - 1 do
                  begin
                    with Diff.Compares[i] do
                    begin
                      if Kind = ckNone then
                        Continue;

                      if Kind <> ckAdd then
                      begin
                        news.SlftpNewsAdd('AUTORULES', Format('<b>[AUTORULES %s] <c4>-</c></b> %d %s',[s.site.name, oldIndex1 + 1, old_rules[oldIndex1]]));
                        irc_Addstats(Format('<b>[AUTORULES %s] <c4>-</c></b> %d %s',[s.site.name, oldIndex1 + 1, old_rules[oldIndex1]]));
                      end;

                      if Kind <> ckDelete then
                      begin
                        news.SlftpNewsAdd('AUTORULES', Format('<b>[AUTORULES %s] <c4>+</c></b> %d %s',[s.site.name, oldIndex2 + 1, new_rules[oldIndex2]]));
                        irc_Addstats(Format('<b>[AUTORULES %s] <c4>+</c></b> %d %s',[s.site.name, oldIndex2 + 1, new_rules[oldIndex2]]));
                      end;
                    end;
                  end;
                except
                  on e: Exception do
                  begin
                    Debug(dpError, section, Format('[EXCEPTION] TaskRules %s Diff.DiffStats Exception : %s', [s.name, e.Message]));
                  end;
                end;

                // for testing purpose -- don't know what it exactly shows...never used autorules feature
                news.SlftpNewsAdd('AUTORULES', Format('<b>[AUTORULES %s]</b> Matches: %d', [s.site.name, matches]));
                news.SlftpNewsAdd('AUTORULES', Format('<b>[AUTORULES %s]</b> Modifies: %d', [s.site.name, modifies]));
                news.SlftpNewsAdd('AUTORULES', Format('<b>[AUTORULES %s]</b> Adds: %d', [s.site.name, adds]));
                news.SlftpNewsAdd('AUTORULES', Format('<b>[AUTORULES %s]</b> Deletes: %d', [s.site.name, deletes]));

                irc_Addstats(Format('<b>[AUTORULES %s]</b> Matches: %d', [s.site.name, matches]));
                irc_Addstats(Format('<b>[AUTORULES %s]</b> Modifies: %d', [s.site.name, modifies]));
                irc_Addstats(Format('<b>[AUTORULES %s]</b> Adds: %d', [s.site.name, adds]));
                irc_Addstats(Format('<b>[AUTORULES %s]</b> Deletes: %d', [s.site.name, deletes]));
              end
              else
              begin
                if spamcfg.readbool('sites', 'rules_nochange', False) then
                  irc_Addstats(Format('<b>[AUTORULES %s]</b> No Changes.', [s.site.name]));
              end;
            end;

          finally
            Diff.Free
          end;

        finally
          hash_old_rules.Free;
          hash_new_rules.Free;
        end;

      end;

      f.Clear;
      for i := 0 to new_rules.Count - 1 do
      begin
        f.Add(new_rules[i]);
      end;
      try
        f.SaveToFile(rules_file);
      except
        on e: Exception do
        begin
          Debug(dpError, section, Format('[EXCEPTION] TaskRules SaveToFile Exception : %s', [e.Message]));
          readyerror := True;
          exit;
        end;
      end;

    finally
      f.Free
    end;

  finally
    old_rules.Free;
    new_rules.Free;
  end;

  // re add
  i := s.site.AutoRulesStatus;
  if i > 0 then
  begin
    try
      t := TRulesTask.Create(netname, channel, site1);
      t.startat := IncSecond(Now, i);
      t.dontremove := True;
      AddTask(t);
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('[EXCEPTION] TRulesTask.Execute AddTask: %s', [e.Message]));
      end;
    end;
  end;

  Result := True;
  ready := True;
end;

function TRulesTask.Name: String;
begin
  Result := 'AUTORULES ' + ScheduleText;
end;

end.

