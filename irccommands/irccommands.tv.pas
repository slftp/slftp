unit irccommands.tv;

interface

{ TVInfo aka TTVRelease aka TVMaze irc commands }
function IrcAnnounceTVInfo(const netname, channel, params: String): boolean;
function IrcAddTVMazeToDb(const netname, channel, params: String): boolean;
function IrcSetTheTVDbID(const netname, channel, params: String): boolean;
function IrcSetTVRageID(const netname, channel, params: String): boolean;
function IrcUpdateTVMazeInfo(const netname, channel, params: String): boolean;
function IrcDelTheTVDbInfo(const netname, channel, params: String): boolean;

implementation

uses
  SysUtils, Classes, DateUtils, dbtvinfo, tasktvinfolookup, irc, debugunit, configunit, http, regexpr, mystrings;

const
  section = 'irccommands.tv';

function IrcAnnounceTVInfo(const netname, channel, params: String): boolean;
var
  db_tvrage: TTVInfoDB;
begin

  db_tvrage := nil;

  try
    if StrToIntDef(params, -1) = -1 then
    begin
      try
        db_tvrage := getTVInfoByReleaseName(params);
      except
        on E: Exception do
        begin
          Debug(dpError, section,
            format('Exception in IrcAnnounceTheTVDbInfo.getTVInfoBbyShowName: %s', [E.Message]));
          irc_AddText(Netname, Channel,
            format('<c4>[Exception]</c> in IrcAnnounceTVInfo.getTheTVInfoByShowName: %s',
            [E.Message]));
          Result := True;
          exit;
        end;
      end;
    end
    else
    begin
      try
        db_tvrage := getTVInfoByShowID(params);
      except
        on E: Exception do
        begin
          //        db_tvrage := nil;
          Debug(dpError, section,
            format('Exception in IrcAnnounceTVInfo: %s',
            [E.Message]));
          irc_AddText(Netname, Channel,
            format('<c4>[Exception]</c> in IrcAnnounceTVInfo: %s',
            [E.Message]));
          Result := True;
          exit;
        end;
      end;
    end;

    if db_tvrage <> nil then
    begin
      db_tvrage.PostResults(db_tvrage.rls_showname, Netname, Channel);
      Result := True;
    end
    else
    begin
      irc_addtext(Netname, Channel,
        format('<c4>[<b>FAILED<b>]</c> Nothing found for <b>%s</b>', [params]));
      Result := True;
    end;
  finally
    db_tvrage.Free;
  end;
end;

function IrcAddTVMazeToDb(const netname, channel, params: String): boolean;
var
  resp, ssname, sid: String;
  tvr: TTVInfoDB;
  x: TRegExpr;
  i, sresMAXi: integer;
  res: TStringlist;
  showname: String;
  fHttpGetErrMsg: String;
begin
  result := False;
  sid := UpperCase(SubString(params, ' ', 1));
  ssname := mystrings.RightStr(params, length(sid) + 1);
  sresMAXi := strtointdef(config.ReadString('tasktvinfo', 'max_sid_lookup_results', '5'), 5);

  x := TRegExpr.Create;
  try
    x.ModifierI := True;
    x.ModifierM := True;
    x.Expression := '\s\-c\:(\d+)$';
    // \s is importent for the right announce later...
    if x.Exec(params) then
      sresMAXi := StrToIntDef(x.Match[1], sresMAXi);
    ssname := x.Replace(ssname, '', False);
  finally
    x.Free;
  end;

  if ((sid = '--SEARCH') or (sid = '--S') or (sid = '-SEARCH') or (sid = '-S')) then
  begin
    getShowValues(ssname, showname);
    resp := findTVMazeIDByName(showname, netname, channel);

    if resp <> 'FAILED' then
    begin
      res := TStringlist.Create;
      try
        res.CommaText := resp;
        for I := 0 to res.Count - 1 do
        begin
          if i >= sresMAXi then
            break;
          irc_addtext(netname, channel, res.Strings[i]);
        end;
      finally
        res.free;
      end;
    end
    else
      irc_addtext(Netname, Channel, '<b><c5>TVInfo</c></b>: No match for %s found.',
        [showname]);

    result := True;
    Exit;
  end;

  if StrToIntDef(sid, -1) > -1 then
  begin
    if not HttpGetUrl('https://api.tvmaze.com/shows/' + sid + '?embed[]=nextepisode&embed[]=previousepisode', resp, fHttpGetErrMsg) then
    begin
      // TODO: maybe not showing correct stuff when no info was found
      irc_addtext(Netname, Channel, Format('<c4>[FAILED]</c> TVMaze API search for %s --> %s', [ssname, fHttpGetErrMsg]));
      Result := True;
      exit;
    end;

    if ((resp = '') or (resp = '[]')) then
    begin
      irc_addtext(Netname, Channel, Format('<c4>IrcUpdateTVMazeInfo</c>: No info found for %s', [ssname]));
      Result := True;
      Exit;
    end;

    tvr := parseTVMazeInfos(resp, ssname);
    tvr.rls_showname := mystrings.RightStr(params, length(sid) + 1);
    try
      tvr.Save;
    except
      on E: Exception do
      begin
        irc_AddText(Netname, Channel, format(
          '<c4>[Exception]</c> in IrcAddTheTVDbToDb.save: %s',
          [E.Message]));
        tvr.free;
        result := True;
        Exit;
      end;
    end;
    tvr.PostResults(mystrings.RightStr(params, length(sid) + 1), Netname, Channel);
    tvr.Free;
  end
  else
    irc_Addtext(netname, channel,
      '<c4><b>Syntax Error!</b></c> no id found to add, you may want to search? use -s');

  Result := True;
end;

function IrcSetTheTVDbID(const netname, channel, params: String): boolean;
var
  mazeid, thetvdbid: integer;
  tvi: TTVInfoDB;
begin
  thetvdbid := StrtoIntDef(SubString(params, ' ', 1), -1);
  mazeid := StrtoIntDef(SubString(params, ' ', 2), -1);
  result := False;

  if mazeid > -1 then
    tvi := getTVInfoByShowID(inttostr(mazeid))
  else
    tvi := getTVInfoByReleaseName(mystrings.RightStr(params, length(inttostr(thetvdbid)) + 1));

  if tvi = nil then
  begin
    Irc_AddText(Netname, Channel, '<c15><b>Info</c></b>: No entry found..');
    Exit;
  end;
  tvi.setTheTVDbID(thetvdbid);
  tvi.free;
  result := True;
end;

function IrcSetTVRageID(const netname, channel, params: String): boolean;
var
  mazeid, tvrageid: integer;
  tvi: TTVInfoDB;
begin
  tvrageid := StrtoIntDef(SubString(params, ' ', 1), -1);
  mazeid := StrtoIntDef(SubString(params, ' ', 2), -1);
  result := False;

  if mazeid > -1 then
    tvi := getTVInfoByShowID(inttostr(mazeid))
  else
    tvi := getTVInfoByReleaseName(mystrings.RightStr(params, length(inttostr(tvrageid)) + 1));

  if tvi = nil then
  begin
    Irc_AddText(Netname, Channel, '<c15><b>Info</c></b>: No entry found..');
    Exit;
  end;
  tvi.setTVRageID(tvrageid);
  tvi.free;
  result := True;
end;

function IrcUpdateTVMazeInfo(const netname, channel, params: String): boolean;
var
  respo, tvmaze_id, tv_showname: String;
  otvr, newtvi: TTVInfoDB;
  fHttpGetErrMsg: String;
begin
  Result := false;

  tvmaze_id := '';
  tv_showname := '';

  if strtointdef(params, -1) > -1 then
    tvmaze_id := params
  else
  begin
    otvr := getTVInfoByReleaseName(params);
    if otvr <> nil then
    begin
      try
        tvmaze_id := otvr.tvmaze_id;
        tv_showname := otvr.tv_showname;
      finally
        otvr.free;
      end;
    end
    else
      tv_showname := params;
  end;

  if tvmaze_id = '' then
  begin
    Irc_AddText(Netname, Channel, '<b><c4>Error</c></b>: Show named <b>%s</b> not found in our local database.', [tv_showname]);
    exit;
  end;

  if not HttpGetUrl('https://api.tvmaze.com/shows/' + tvmaze_id + '?embed[]=nextepisode&embed[]=previousepisode', respo, fHttpGetErrMsg) then
  begin
    if tv_showname <> '' then
      Irc_AddText(Netname, Channel, Format('<c4>[FAILED]</c> TVMaze Update for %s (ID: %s) --> %s', [tv_showname, tvmaze_id, fHttpGetErrMsg]))
    else
      Irc_AddText(Netname, Channel, Format('<c4>[FAILED]</c> TVMaze Update for %s --> %s', [tvmaze_id, fHttpGetErrMsg]));
    exit;
  end;

  if ((respo = '') or (respo = '[]')) then
  begin
    Irc_AddText(Netname, Channel, '<c4>IrcUpdateTVMazeInfo</c>: No Result from TVMaze API when updating %s', [tvmaze_id]);
    Exit;
  end;

  try
    newtvi := parseTVMazeInfos(respo);
    if newtvi = nil then
      Exit;

    try
      newtvi.last_updated := DateTimeToUnix(now());
      if (newtvi.Update(True)) then
      begin
        Result := True;
        newtvi.PostResults(newtvi.tv_showname, Netname, Channel);
      end;
    finally
      newtvi.free;
    end;

  except on e: Exception do
    begin
      Irc_AddText(Netname, Channel, '<c4>[EXCEPTION]</c> TTVInfoDB.Update: %s', [e.Message]);
      Exit;
    end;
  end;
end;

function IrcDelTheTVDbInfo(const netname, channel, params: String): boolean;
var
  return: Integer;
begin
  Result := False;
  if strtointdef(params, -1) > -1 then
    return := deleteTVInfoByID(params)
  else
    return := deleteTVInfoByRipName(params);

  //better error message system needed :/
  case return of
    1:
      begin
        //        Irc_AddText(Netname, Channel, '');
        result := true;
        exit;
      end;
    10:
      begin
        Irc_AddText(Netname, Channel, '<c4><b>Error</c></b>: Failed to delete id:' + params);
        result := true;
        exit;
      end;
    11:
      begin
        Irc_AddText(Netname, Channel, '<c4><b>Error</c></b>: Failed to delete id:' + params);
        result := true;
        exit;
      end;
    12:
      begin
        Irc_AddText(Netname, Channel, '<c4><b>Error</c></b>: Failed to delete ' + params);
        result := true;
        exit;
      end;
    13:
      begin
        Irc_AddText(Netname, Channel, '<c4><b>Error</c></b>: Failed to delete ' + params);
        result := true;
        exit;
      end;
  end;
  //we dont set result to true, to check if something went wrong...
end;

end.