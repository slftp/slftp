unit tasktvragelookup;

interface

uses Classes, pazo, tasksunit, taskrace, xmlwrapper, dbaddtvrage;

type
  TPazoTvRageLookupTask = class(TPazoPlainTask)
  private
    attempt:      integer;
    initial_site: string;
  public
    constructor Create(const netname, channel: string; site: string;
      pazo: TPazo; attempt: integer = 0);
    function Execute(slot: Pointer): boolean; override;
    function Name: string; override;
    procedure PostResults(id, network, country, classi, status: string;
      genre: TStringList; premyear: string);
  end;

  (*for !addtvrage channels*)
  TPazoHTTPTVRageTask = class(TTask)
  private
    rls: string;
    tv_showid: string;
  public
    constructor Create(const tv_showid: string; rls: string = '');
    destructor Destroy; override;
    function Execute(slot: Pointer): boolean; override;
    function Name: string; override;
  end;


function ParseTVRageXML(xml: TSLXMLDocument; Showname: string = ''): TDbTVRage; overload;
function ParseTVRageXML(content: string; Showname: string = ''): TDbTVRage; overload;

implementation

uses DateUtils, SysUtils, queueunit, debugunit, configunit, mystrings, kb,
  sltcp, slhttp, RegExpr, irc;

const
  section = 'tasktvrage';



function ParseTVRageXML(xml: TSLXMLDocument; Showname: string = ''): TDbTVRage;
var
  nnn, nn, n: TSLXMLNode;
  tvr:   TDbTVRage;
  i, gc: integer;
  s:     string;

begin

  if Showname <> '' then
    s := Csere(Showname, '.', ' ')
  else
    s := '';
  tvr := TDbTVRage.Create(s);
  try

    try
      n := xml.GetDocumentElement;
    except
      on E: Exception do
        irc_Adderror(format(
          '<c4>[Exception]</c> in ParseTVRageXML.GetDocumentElement: %s',
          [E.Message]));
    end;


    nn := xml.FindChildNode(n, 'showid');
    tvr.tv_showid := xml.GetNodeValue(nn);

    nn := xml.FindChildNode(n, 'showname');
    tvr.tv_showname := xml.GetNodeValue(nn);

    nn := xml.FindChildNode(n, 'showlink');
    tvr.tv_showurl := xml.GetNodeValue(nn);

    nn := xml.FindChildNode(n, 'started');
    if nn <> nil then
      tvr.tv_premiered_year := StrToIntDef(xml.GetNodeValue(nn), -1)
    else
      tvr.tv_premiered_year := -1;


    nn := xml.FindChildNode(n, 'status');
    if nn <> nil then
    begin
      tvr.tv_status := xml.GetNodeValue(nn);
      if ((Uppercase(tvr.tv_status) = 'ENDED') or
        (Uppercase(tvr.tv_status) = 'CANCELED/ENDED')) then
        tvr.tv_running := False
      else
        tvr.tv_running := True;
    end
    else
    begin
      tvr.tv_status  := 'not found.';
      tvr.tv_running := False;
    end;

    nn := xml.FindChildNode(n, 'classification');
    if nn <> nil then
      tvr.tv_classification := xml.GetNodeValue(nn)
    else
      tvr.tv_classification := 'not found.';

    nn := xml.FindChildNode(n, 'runtime');
    if nn <> nil then
    begin
      tvr.tv_runtime := StrToIntDef(xml.GetNodeValue(nn), -1);

    end
    else
      tvr.tv_runtime := -1;


    //        nn := xml.FindChildNode(n, 'seasons');
    //        tvr.tv_seasons := StrToIntDef(xml.GetNodeValue(nn), -1);


    nn := xml.FindChildNode(n, 'ended');
    if nn <> nil then
    begin
      tvr.tv_running   := True;
      tvr.tv_endedyear := StrToIntDef(Copy(xml.GetNodeValue(nn),8,4), -1);
      if tvr.tv_endedyear <> -1 then
        tvr.tv_running := False;
    end
    else
    begin
      tvr.tv_running   := False;
      tvr.tv_endedyear := -1;
    end;
    tvr.tv_genres.Clear;
    nn := xml.FindChildNode(n, 'genres');
    if nn <> nil then
    begin
      gc := xml.GetChildNodeCount(nn);
      for i := 0 to gc - 1 do
      begin
        nnn := xml.GetChildNodeItem(nn, i);
        tvr.tv_genres.Add(xml.GetNodeValue(nnn));
      end;
    end;

    nn := xml.FindChildNode(n, 'network');
    if nn <> nil then
    begin
      nnn := xml.GetAttributeNodeByIndex(nn, 0);
      if xml.GetNodeValue(nnn) = 'US' then
        tvr.tv_country := 'USA'
      else
        tvr.tv_country := xml.GetNodeValue(nnn);
      tvr.tv_network := xml.GetNodeValue(nn);
    end
    else
    begin
      tvr.tv_country := 'not found.';
      tvr.tv_network := 'not found.';
    end;

    Result := tvr;

  except
    on E: Exception do
      irc_Adderror(format('<c4>[Exception]</c> in ADDTVRageInfo.ParseTVRageXML: %s',
        [E.Message]));
  end;
end;

function ParseTVRageXML(content: string; Showname: string = ''): TDbTVRage;
var
  xml: TSLXMLDocument;
  s:   string;
  st:  TStream;
begin
  Result := nil;
  xml    := TSLXMLDocument.Create;

  s  := Csere(Showname, ' ', '.');
  st := TStringStream.Create(content);
  st.Position := 0;
  try
    try
      xml.LoadFromStream(st);
    except
      on E: Exception do
        irc_Adderror(format(
          '<c4>[Exception]</c> in ADDTVRageInfo.ParseTVRageXML.LoadFromStream: %s',
          [E.Message]));
    end;
    Result := ParseTVRageXML(xml, s);
  finally
    xml.Free;
    st.Free;
  end;
end;

function csakangolabc(s: string): string;
var
  i: integer;
begin
  s      := LowerCase(s);
  Result := '';
  for i := 1 to length(s) do
    if ((s[i] >= 'a') and (s[i] <= 'z')) then
    begin
      Result := Result + s[i];
    end;
end;


{ TPazoTvRageLookup }

constructor TPazoTvRageLookupTask.Create(const netname, channel: string;
  site: string; pazo: TPazo; attempt: integer = 0);
begin
  self.attempt      := attempt;
  self.initial_site := site;
  inherited Create(netname, channel, site, '', pazo);
end;

procedure TPazoTvRageLookupTask.PostResults(id, network, country, classi, status: string;
  genre: TStringList; premyear: string);
begin
  irc_Addstats(Format(
    '(<c9>i</c>)....<c7><b>TVRAGE</b></c>....... <c0><b>info for</c></b> ...........: <b>%s</b> (%s) - http://tvrage.com/shows/id-%s/',
    [mainpazo.rls.rlsname, premyear, id]));
  irc_Addstats(Format(
    '(<c9>i</c>)....<c7><b>TVRAGE</b></c>.. <c9><b>Genre (Class) @ Status</c></b> ..: %s (%s) @ %s',
    [genre.CommaText, classi, status]));
  irc_Addstats(Format(
    '(<c9>i</c>)....<c7><b>TVRAGE</b></c>....... <c4><b>Country/Channel</c></b> ....: <b>%s</b> (%s) ',
    [country, network]));
end;


function findTVRAGEID(showname: string): string;
var
s,  sname, url, response: string;
  xml: TSLXMLDocument;
  n:   TSLXMLNode;
  st:  TStream;
  i:   integer;
begin
  Result   := 'FAILED';
  s        := Csere(showname, '.and.', '.&.');
  s        := Csere(s, '.at.', '.@.');
  s        := Csere(s, '_and_', '_&_');
  s        := Csere(s, '_at_', '_@_');
  sname    := Csere(s, ' ', '+');
  sname    := Csere(sname, '.', '+');
  sname    := Csere(sname, '.', '+');

  url      := 'show=' + sname;
  response := slUrlGet('http://services.tvrage.com/feeds/search.php', url);


  if response = '' then
    Exit;
  st  := TStringStream.Create(response);
  xml := TSLXMLDocument.Create;
  try
    st.Position := 0;
    xml.LoadFromStream(st);
    n := xml.GetDocumentElement;
    for i := 0 to xml.GetChildNodeCount(n) - 1 do
    begin

      if csakangolabc(xml.GetNodeValue(xml.FindChildNode(xml.GetChildNodeItem(n, i),
        'name'))) = csakangolabc(s) then
      begin
        Result := xml.GetNodeValue(xml.FindChildNode(xml.GetChildNodeItem(n, i),
          'showid'));
        break;
      end;
    end;
  finally
    st.Free;
    {$IFDEF FPC}
    xml.Free;
    {$ENDIF}
  end;
end;


function TPazoTvRageLookupTask.Execute(slot: Pointer): boolean;
var
  tr:  TTvRelease;
  r:   TPazoTvRageLookupTask;
  response, uurl: string;
  db_tvrage: TDbTVRage;
  ps:  TPazoSite;
  xml: TSLXMLDocument;
  st:  TStream;
  sid: string;
begin
  tr := TTvRelease(mainpazo.rls);

  try
    db_tvrage := dbaddtvrage_gettvrage_show(tr.showname);
    if (db_tvrage <> nil) then
    begin
      db_tvrage.SetTVRageRelease(tr);
      ready  := True;
      Result := True;
      exit;
    end;
  except
    on e: Exception do
    begin
      //   db_tvrage := nil;
      Debug(dpError, section, Format('Exception in dbaddtvrage_gettvrage_show: %s',
        [e.Message]));

      ready  := True;
      Result := True;
      exit;
    end;
  end;

  try
    sid := findTVRAGEID(tr.showname);
  except
    on e: Exception do
    begin
      //   db_tvrage := nil;
      Debug(dpError, section, Format('Exception in findTVRAGEID: %s',
        [e.Message]));
    end;
  end;

  if sid = 'FAILED' then
  begin

    if attempt < config.readInteger(section, 'readd_attempts', 5) then
    begin
      debug(dpSpam, section, 'READD: retrying tv rage lookup for %s later',
        [tr.showname]);
      r := TPazoTvRageLookupTask.Create(netname, channel, initial_site,
        mainpazo, attempt + 1);
      r.startat := IncSecond(Now, config.ReadInteger(section, 'readd_interval', 60));
      try
        AddTask(r);
      except
        on e: Exception do
        begin
          Debug(dpError, section,
            Format('[Exception] in TPazoTvRageLookupTask Search %s', [e.Message]));
          irc_Adderror(Format(
            '<c4>[Exception]</c> in TPazoTvRageLookupTask Search %s', [e.Message]));
          readyerror := True;
          Result     := True;
          //          x.Free;
          exit;
        end;
      end;

    end
    else
    begin
      debug(dpSpam, section, 'READD: no more attempts...');
    end;
    irc_addadmin('<c4><b>ERROR</c> No TVRage ID found for %s</b>',
      [tr.showname]);

    ready  := True;
    Result := True;
    exit;
  end;



  uurl := 'sid=' + sid;


  try
    xml      := TSLXMLDocument.Create;
    //xml.LoadFromWeb(Format('http://services.tvrage.com/tools/quickinfo.php?%s', [uurl]));
    response := slUrlGet('http://services.tvrage.com/feeds/showinfo.php', uurl);
    // irc_addtext('', '', response);
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format(
        '[EXCEPTION] TPazoTvRageLookupTask slUrlGet: Exception : %s', [e.Message]));
      irc_Adderror(Format(
        '<c4>[EXCEPTION]</c> TPazoTvRageLookupTask slUrlGet: Exception : %s', [e.Message]));
      Result := True;
      ready  := True;
    {$IFDEF FPC}

    xml.Free;
    {$ENDIF}

      //      x.Free;
      exit;
    end;
  end;

  if response = '' then
  begin
    if attempt < config.readInteger(section, 'readd_attempts', 5) then
    begin
      debug(dpSpam, section, 'READD: retrying tv rage lookup for %s later',
        [tr.showname]);
      r := TPazoTvRageLookupTask.Create(netname, channel, initial_site,
        mainpazo, attempt + 1);
      r.startat := IncSecond(Now, config.ReadInteger(section, 'readd_interval', 60));
      try
        AddTask(r);
      except
        on e: Exception do
        begin
          Debug(dpError, section,
            Format('[Exception] in TPazoTvRageLookupTask AddTask %s', [e.Message]));
          irc_Adderror(Format('<c4>[Exception]</c> in TPazoTvRageLookupTask AddTask %s',
            [e.Message]));
          readyerror := True;
          Result     := True;
          //          x.Free;
    {$IFDEF FPC}

    xml.Free;
    {$ENDIF}

          exit;
        end;
      end;
    end
    else
    begin
      debug(dpSpam, section, 'READD: no more attempts...');
    end;
    ready  := True;
    Result := True;
    xml.Free;
    exit;
    //    x.Free;
  end;
  debug(dpSpam, section, 'TVRage results for %s' + #13#10 + '%s',
    [tr.showname, response]);

  st := TStringStream.Create(response);
  try
    st.Position := 0;
    xml.LoadFromStream(st);

    db_tvrage := ParseTVRageXML(xml, tr.showname);

  finally
    xml.Free;
    st.Free;
  end;

  if csakangolabc(db_tvrage.tv_showname) = csakangolabc(tr.showname) then
  begin
    try
      //      dbaddtvrage_SaveTVRage(db_tvrage.tv_showid, db_tvrage, mainpazo.rls.rlsname);
      irc_Addtext_by_key('ADDTVRAGE', '!addtvrage ' + mainpazo.rls.rlsname +
        ' ' + db_tvrage.tv_showid);
      db_tvrage.Save;
    except
      on e: Exception do
      begin
        Debug(dpError, section, Format('Exception in dbaddtvrage_SaveTVRage: %s',
          [e.Message]));
        Result     := True;
        readyerror := True;
        exit;
      end;
    end;
  end
  else
  begin
    irc_addadmin('<c4><b>ERROR</c> english alphabet check failed! %s <> %s   </b>',
      [db_tvrage.tv_showname, tr.showname]);

    if config.ReadBool(section, 'stop_on_englishcheck', True) then
    begin
      Result := True;
      ready  := True;
      exit;
    end;
  end;

  if config.ReadBool(section, 'post_lookup_infos', False) then
  begin
    PostResults(db_tvrage.tv_showid, db_tvrage.tv_network, db_tvrage.tv_country,
      db_tvrage.tv_classification, db_tvrage.tv_status, db_tvrage.tv_genres,
      IntToStr(db_tvrage.tv_premiered_year));
    //  PostResults(cur_id, cur_netw, cur_country, cur_cassi, cur_status,tr.genres, IntToStr(cur_premyear));
  end;

  try
    ps := FindMostCompleteSite(mainpazo);
    if ((ps = nil) and (mainpazo.sites.Count > 0)) then
      ps := TPazoSite(mainpazo.sites[0]);
    kb_add(netname, channel, ps.Name, mainpazo.rls.section, '', 'UPDATE',
      mainpazo.rls.rlsname, '');
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('Exception in TPazoTvRageLookupTask kb_add: %s',
        [e.Message]));
    end;
  end;

  ready  := True;
  Result := True;

end;

function TPazoTvRageLookupTask.Name: string;
begin
  try
    Result := format('PTVRAGE PazoID(%d) %s @ %s Count(%d)',
      [mainpazo.pazo_id, mainpazo.rls.rlsname, site1, attempt]);
  except
    Result := 'PTVRAGE';
  end;
end;


{ TPazoHTTPTVRageTask }

constructor TPazoHTTPTVRageTask.Create(const tv_showid: string; rls: string = '');
begin
  self.tv_showid := tv_showid;
  self.rls := rls;
  inherited Create('', '', config.ReadString('sites', 'admin_sitename', 'SLFTP'));
end;



function TPazoHTTPTVRageTask.Name: string;
begin
  try
    Result := Format('HTTPTVRage : %s', [tv_showid]);
  except
    Result := 'HTTPTVRage';
  end;
end;

destructor TPazoHTTPTVRageTask.Destroy;
begin
  inherited;
end;

function TPazoHTTPTVRageTask.Execute(slot: Pointer): boolean;
var
  tvrage: TDbTVRage;
  uurl:   string;
  rx, x:  TRegExpr;
  sname:  string;
  st:     TStream;
  xml:    TSLXMLDocument;
begin

  rx := TRegexpr.Create;
  try
    rx.ModifierI := True;

    rx.Expression := '(.*)[\._-](\d{4}\.\d{2}\.\d{2}|\d{2}\.\d{2}\.\d{4})[\._-](.*)';
    if rx.Exec(rls) then
    begin
      sname := rx.Match[1];
    end;

    rx.Expression := '(.*)[\._-](\d+)x(\d+)[\._-](.*)';
    if rx.Exec(rls) then
    begin
      sname := rx.Match[1];
      //    season   := StrToIntDef(rx.Match[2], 0);
      //    episode  := StrToIntDef(rx.Match[3], 0);
    end;

    rx.Expression :=
      '(.*)[\._-]S(\d{1,3})(\.?([DE]|EP|Episode|Part)(\d{1,4})\w?)?[\._-](.*)';
    if rx.Exec(rls) then
    begin
      sname := rx.Match[1];
      //    season   := StrToIntDef(rx.Match[2], 0);
      //    episode  := StrToIntDef(rx.Match[5], 0);
    end;

    rx.Expression := '[\.\_]';
    sname := rx.Replace(sname, ' ');
  finally
    rx.Free;
  end;

  uurl     := 'sid=' + tv_showid;
  response := slUrlGet('http://services.tvrage.com/feeds/showinfo.php', uurl);

  if response <> '' then
  begin
    st  := TStringStream.Create(response);
    xml := TSLXMLDocument.Create;
    try
      st.Position := 0;
      xml.LoadFromStream(st);

      tvrage := ParseTVRageXML(xml, sname);
      if tvrage <> nil then
        dbaddtvrage_SaveTVRage(tv_showid, tvrage, rls);
    finally
      st.Free;
      xml.Free;
    end;
  end;
end;


end.

