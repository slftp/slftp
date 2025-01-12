unit irccommands.rules;

interface

{ slftp rules commands functions }
function IrcRuleAdd(const netname, channel, params: String): boolean;
function IrcRuleDel(const netname, channel, params: String): boolean;
function IrcRuleMod(const netname, channel, params: String): boolean;
function IrcRuleIns(const netname, channel, params: String): boolean;
function IrcShowAllRules(const netname, channel, params: String): boolean;
function IrcAllRuleDel(const netname, channel, params: String): boolean; // TODO: rewrite it, it's not working as in helpfile declared
function IrcRules(const netname, channel, params: String): boolean;
function IrcRuleList(const netname, channel, params: String): boolean;
function IrcRuleHelp(const netname, channel, params: String): boolean;
function IrcRuleCopy(const netname, channel, params: String): boolean;
function IrcAutoRules(const netname, channel, params: String): boolean;

implementation

uses
  SysUtils, Classes, Contnrs, irc, sitesunit, rulesunit, RegExpr, mystrings, Generics.Collections;

const
  section = 'irccommands.rules';

function IrcRuleAdd(const netname, channel, params: String): boolean;
var
  r: TRule;
  sitename, rule, section, error: String;
  s: TSite;
  fAddedRule: TPair<TRule, integer>;
begin
  Result := False;

  sitename := UpperCase(SubString(params, ' ', 1));
  section := UpperCase(SubString(params, ' ', 2));
  rule := params;

  if rule = '' then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  s := FindSiteByName(Netname, sitename);
  if ((s = nil) and (sitename <> '*')) then
  begin
    irc_addtext(Netname, Channel, '<c4>ERROR</c>: Site %s not found.', [sitename]);
    exit;
  end;

  if ((section <> '*') and (s <> nil) and (s.sectiondir[section] = '')) then
  begin
    irc_addtext(Netname, Channel, 'Site %s has no section %s.', [sitename, section]);
    exit;
  end;

  fAddedRule := AddRule(rule, error);
  if error <> '' then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c> %s', [error]);
    exit;
  end;

  irc_addtext(Netname, Channel, '<b>Added<b>: %d %s', [fAddedRule.Value, fAddedRule.Key.AsText(True)]);

  Result := True;
end;

function IrcRuleDel(const netname, channel, params: String): boolean;
var
  id: integer;
  fSite: TSite;
  fMessage, fSitename, fSection: string;
begin
  Result := False;
  id := StrToIntDef(SubString(params, ' ', 1), -1);

  if RuleDel(id, fMessage) then
  begin
    Irc_AddText(netname, channel, fMessage);
    Result := True;
  end
  else
  begin
    irc_addtext(Netname, Channel, '<c4><b>Error</b>.</c> %s', [fMessage]);
  end;
end;

function IrcRuleMod(const netname, channel, params: String): boolean;
var
  id: integer;
  r: TRule;
  sitename, rule, section, fMessage: String;
  s: TSite;
begin
  Result := False;
  id := StrToIntDef(SubString(params, ' ', 1), -1);
  sitename := UpperCase(SubString(params, ' ', 2));
  section := UpperCase(SubString(params, ' ', 3));
  rule := Copy(params, length(IntToStr(id)) + 2, 1000);

  if rule = '' then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  s := FindSiteByName(Netname, sitename);
  if ((s = nil) and (sitename <> '*')) then
  begin
    irc_addtext(Netname, Channel, '<c4>ERROR</c>: Site %s not found.', [sitename]);
    exit;
  end;

  if ((section <> '*') and (s <> nil) and (s.sectiondir[section] = '')) then
  begin
    irc_addtext(Netname, Channel, 'Site %s has no section %s.', [sitename, section]);
    exit;
  end;

  if RuleMod(id, rule, fMessage) then
  begin
    irc_addtext(Netname, Channel, fMessage);
    Result := True;
  end
  else
  begin
    irc_addtext(Netname, Channel, '<c4><b>Error</b>.</c> %s', [fMessage]);
  end;
end;

function IrcRuleIns(const netname, channel, params: String): boolean;
var
  id: integer;
  r: TRule;
  sitename, rule, section, fMessage: String;
  s: TSite;
begin
  Result := False;
  id := StrToIntDef(SubString(params, ' ', 1), -1);
  sitename := UpperCase(SubString(params, ' ', 2));
  section := UpperCase(SubString(params, ' ', 3));
  rule := Copy(params, length(IntToStr(id)) + 2, 1000);

  if rule = '' then
  begin
    irc_addtext(Netname, Channel, '<c4><b>Syntax error</b>.</c>');
    exit;
  end;

  s := FindSiteByName(Netname, sitename);
  if ((s = nil) and (sitename <> '*')) then
  begin
    irc_addtext(Netname, Channel, '<c4>ERROR</c>: Site %s not found.', [sitename]);
    exit;
  end;

  if ((section <> '*') and (s <> nil) and (s.sectiondir[section] = '')) then
  begin
    irc_addtext(Netname, Channel, 'Site %s has no section %s.', [sitename, section]);
    exit;
  end;

  if RuleIns(id, rule, fMessage) then
  begin
    irc_addtext(Netname, Channel, fMessage);
    Result := True;
  end
  else
  begin
    irc_addtext(Netname, Channel, '<c4><b>Error</b>.</c> %s', [fMessage]);
  end;
end;

function IrcShowAllRules(const netname, channel, params: String): boolean;
var
  sitename, sections: String;
  xs: TStringList;
  ii, i, count: Integer;
  r: TRule;
  fFoundRules: TObjectList<TRuleWithID>;
  fRuleWithID: TRuleWithID;
begin
  Result := False;

  sitename := UpperCase(SubString(params, ' ', 1));
  sections := UpperCase(mystrings.RightStr(params, length(sitename) + 1));
  count := 0;

  if (sitename = '*') and (sections = '') then
  begin
    irc_addtext(Netname, Channel, 'You can not use the special sitename * as a wildcard for all sites to show all rules.', [sitename, sections]);
    exit;
  end;

  xs := TStringList.Create;
  try
    xs.Delimiter := ' ';
    xs.DelimitedText := sections;
    fFoundRules := FindRules(sitename, xs);

    if fFoundRules.Count = 0 then
    begin
      irc_addtext(Netname, Channel, 'No matching rule for your input of %s %s found!', [sitename, sections]);
      exit;
    end;

    for fRuleWithID in fFoundRules do
    begin
      irc_addtext(Netname, Channel, '%d %s', [fRuleWithID.ID, fRuleWithID.FRule.AsText(True)]);
    end;
  finally
    xs.Free;
    FreeAndNil(fFoundRules);
  end;

  Result := True;
end;

function IrcAllRuleDel(const netname, channel, params: String): boolean;
var
  sitess, sectionss: TStringList;
  // s: TSite;
  sitename, section: String;
  ii, i: integer;
begin
  sitename := UpperCase(SubString(params, ' ', 1));
  section := UpperCase(SubString(params, ' ', 2));
  // uppercase(mystrings.RightStr(params, length(sitename)+1));

  sitess := TStringList.Create;
  sectionss := TStringList.Create;
  try
    if sitename = '*' then
    begin
      if section = '' then
      begin
        for i := 0 to sites.Count - 1 do
        begin
          if (TSite(sites.Items[i]).Name = getAdminSiteName) then
            Continue;
          if TSite(sites.Items[i]).PermDown then
            Continue;

          RulesRemove(TSite(sites.Items[i]).Name, '');
        end;
      end
      else
      begin
        sectionss.commatext := section;
        for i := 0 to sites.Count - 1 do
        begin
          for ii := 0 to sectionss.Count - 1 do
          begin
            if TSite(sites.Items[i]).IsSection(sectionss.Strings[ii]) then
              RulesRemove(TSite(sites.Items[i]).Name, sectionss.Strings[ii]);
            // else irc_addtext(netname,channel,'Sections "%s" not found on site: %s',[sectionss.Strings[ii],TSite(sites.Items[i]).name])
          end;
        end;
      end;

    end;

    sitess.commatext := sitename;

    if section = '' then
      for i := 0 to sitess.Count - 1 do
        RulesRemove(sitess.Strings[i], '')
    else
    begin
      sectionss.commatext := section;
      for i := 0 to sitess.Count - 1 do
      begin
        for ii := 0 to sectionss.Count - 1 do
          RulesRemove(sitess.Strings[i], sectionss.Strings[ii]);
      end;
    end;

  finally
    sitess.Free;
    sectionss.Free;
  end;

  Result := True;
end;

function IrcRules(const netname, channel, params: String): boolean;
var
  i: integer;
  r: TRule;
  s: TSite;
  sitename, section, fRuleInfo: String;
  fFoundRulesInfoStrings: TList<String>;
begin
  Result := False;
  sitename := UpperCase(SubString(params, ' ', 1));
  section := UpperCase(SubString(params, ' ', 2));

  if sitename <> '*' then
  begin
    s := FindSiteByName('', sitename);
    if s = nil then
    begin
      irc_addtext(Netname, Channel, '<c4>ERROR</c>: Site %s not found.', [sitename]);
      exit;
    end;

    if section <> '*' then
    begin
      if not s.IsSection(section) then
      begin
        irc_addtext(Netname, Channel, '<c4><b>ERROR</b></c>: %s is not valid section!',
          [section]);
        exit;
      end;
    end;

  end;

  fFoundRulesInfoStrings := FindIrcRules(sitename, section);
  try
    for fRuleInfo in fFoundRulesInfoStrings do
      irc_addtext(Netname, Channel, fRuleInfo);
  finally
    fFoundRulesInfoStrings.Free;
  end;

  Result := True;
end;

function IrcRuleList(const netname, channel, params: String): boolean;
var
  i: integer;
  r: TRegExpr;
begin
  Result := False;
  r := TRegExpr.Create;
  r.ModifierI := True;
  try
    for i := 0 to conditions.Count - 1 do
    begin
      if UpperCase(params) = 'COMMON' then
      begin
        r.Expression := '^(MP3|0DAY|IMDB|NFO|TV|MVID|GAME|APP)[\w\d]+$';
        if not r.Exec(TConditionClass(conditions[i]).Name) then
          irc_addtext(Netname, Channel, TConditionClass(conditions[i]).Name +
            ', ops: ' + TConditionClass(conditions[i]).AcceptedOperatorsAsText);
      end
      else if params <> '' then
      begin
        r.Expression := format('^%s[\w\d]+$', [params]);
        if r.Exec(TConditionClass(conditions[i]).Name) then
          irc_addtext(Netname, Channel, TConditionClass(conditions[i]).Name +
            ', ops: ' + TConditionClass(conditions[i]).AcceptedOperatorsAsText);
      end
      else
      begin
        if conditions[i] <> TBooleanCondition then
          irc_addtext(Netname, Channel, TConditionClass(conditions[i]).Name +
            ', ops: ' + TConditionClass(conditions[i]).AcceptedOperatorsAsText)
        else
          irc_addtext(Netname, Channel, TConditionClass(conditions[i]).Name);
      end;
    end;
  finally
    r.Free;
  end;

  Result := True;
end;

function IrcRuleHelp(const netname, channel, params: String): boolean;
var
  i: integer;
  s, ss: String;
begin
  Result := False;

  if FindConditionClassByName(params) = nil then
  begin
    irc_addtext(Netname, Channel, '<c4>Rule condition "<b>%s</b>" not found!</c>', [params]);
    exit;
  end;

  for i := 0 to conditions.Count - 1 do
  begin
    if TConditionClass(conditions[i]).Name = params then
    begin
      s := TConditionClass(conditions[i]).Description;
      while (True) do
      begin
        ss := GetFirstLineFromTextViaNewlineIndicators(s);
        if ss = '' then
          break;
        irc_addtext(Netname, Channel, ss);
      end;

      if conditions[i] <> TBooleanCondition then
        irc_addtext(Netname, Channel, '<b>Accepted ops:</b> ' + TConditionClass(conditions[i]).AcceptedOperatorsAsText);
      break;
    end;
  end;

  Result := True;
end;

function IrcRuleCopy(const netname, channel, params: String): boolean;
var
  rr, r: TRule;
  rule, error, src_s, dst_s, src_section, dst_section: String;
  ss: TSite;
  i: integer;
begin
  Result := False;
  src_s := UpperCase(SubString(params, ' ', 1));
  dst_s := UpperCase(SubString(params, ' ', 2));
  src_section := UpperCase(SubString(params, ' ', 3));
  dst_section := UpperCase(SubString(params, ' ', 4));

  if (dst_section = '') then
    dst_section := src_section;

  ss := FindSiteByName('', src_s);
  if ss = nil then
  begin
    irc_addtext(Netname, Channel, '<c4>ERROR</c>: Site %s not found.', [src_s]);
    exit;
  end;

  ss := FindSiteByName('', dst_s);
  if ss = nil then
  begin
    irc_addtext(Netname, Channel, '<c4>ERROR</c>: Site %s not found.', [dst_s]);
    exit;
  end;

  RuleCopy(src_s, dst_s, src_section, dst_section);

  Irc_AddText(netname, channel, '<b>Copied</b>: %s to %s for section %s', [src_s, dst_s, dst_section]);
  Result := True;
end;

function IrcAutoRules(const netname, channel, params: String): boolean;
var
  sitename: String;
  status: integer;
  s: TSite;
  StartTask: boolean;
  i: integer;
  x: TStringList;
begin
  Result := False;

  sitename := UpperCase(SubString(params, ' ', 1));
  status := StrToIntDef(SubString(params, ' ', 2), -1);

  if sitename = '*' then
  begin
    for i := 0 to sites.Count - 1 do
    begin
      if (TSite(sites.Items[i]).Name = getAdminSiteName) then
        Continue;
      if (TSite(sites.Items[i]).PermDown) then
        Continue;

      StartTask := False;
      if status > -1 then
      begin
        if status <> 0 then
        begin
          if TSite(sites.Items[i]).AutoRulesStatus <= 0 then
            StartTask := True;

          TSite(sites.Items[i]).AutoRulesStatus := status;
        end
        else
        begin
          TSite(sites.Items[i]).DeleteKey('autorules');
          TSite(sites.Items[i]).RemoveAutoRules;
        end;
      end;
      irc_addtext(Netname, Channel, 'Autorules of %s is: %d', [TSite(sites.Items[i]).Name, TSite(sites.Items[i]).AutoRulesStatus]);

      if StartTask then
        TSite(sites.Items[i]).AutoRules;
    end;
  end
  else
  begin

    x := TStringList.Create;
    try
      x.commatext := sitename;
      for i := 0 to x.Count - 1 do
      begin
        s := FindSiteByName(Netname, x.Strings[i]);
        if s = nil then
        begin
          irc_addtext(Netname, Channel, 'Site <b>%s</b> not found.', [x.Strings[i]]);
          Continue;
        end;
        if s.PermDown then
        begin
          irc_addtext(Netname, Channel, 'Site <b>%s</b> is set perm down.', [sitename]);
          continue;
        end;

        StartTask := False;
        if status > -1 then
        begin
          if status <> 0 then
          begin
            if s.AutoRulesStatus <= 0 then
              StartTask := True;

            s.AutoRulesStatus := status;
          end
          else
          begin
            s.DeleteKey('autorules');
            s.RemoveAutoRules;
          end;
        end;
        irc_addtext(Netname, Channel, 'Autorules of %s is: %d', [sitename, s.AutoRulesStatus]);

        if StartTask then
          s.AutoRules;
      end;
    finally
      x.Free;
    end;
  end;

  Result := True;
end;

end.
