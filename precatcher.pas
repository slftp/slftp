unit precatcher;

interface

uses Classes, Contnrs, slmasks, encinifile;

type
  TSection = class
    eventtype: string;
    section: string;
    words: TStringList;

    constructor Create;
    destructor Destroy; override;
  end;

  TSiteChan = class
    sitename: string;

    sections: TObjectList;

    constructor Create;
    destructor Destroy; override;
  end;

  huntartunk_tipus = (sehun, racetool, ignorelist, replace, hunsections,
    mappings, channels, pretime);

  TMap = class
    origsection: string;
    newsection: string;
    mask: TslMask;
    constructor Create(origsection, newsection, mask: string);
    destructor Destroy; override;
  end;

function precatcherauto: boolean;

function Precatcher_Sitehasachan(sitename: string): boolean;
procedure Precatcher_DelSiteChans(sitename: string);
procedure PrecatcherReload(); overload;
procedure PrecatcherReload(out status: string); overload;
procedure PrecatcherRebuild();
procedure PrecatcherStart;
procedure PrecatcherProcessB(net, chan, nick, Data: string);
procedure PrecatcherProcess(net, chan, nick, Data: string);
function precatcher_logfilename: string;
procedure Precatcher_Init;
procedure Precatcher_Uninit;
function PrecatcherSectionMapping(rls, section: string; x_count: integer = 0):
  string;

function FindSection(section: string): boolean;

function KibontasSection(s, section: string): string;
function ProcessDoReplace(s: string): string;

var
  precatcher_debug: boolean = False;
  precatcher_ircdebug: boolean = False;
  precatcher_spamevents: TStringList;
  precatcher_debug_netname, precatcher_debug_channel: string;
  //  precatcher_auto: Boolean;
  catcherFile: TEncStringlist;
  mappingslist: TObjectList;
  sectionlist: TStringList;
  minimum_rlsname: integer = 10;

implementation

uses SysUtils, sitesunit, Dateutils, kb, irc, ircblowfish, queueunit, mystrings,
  inifiles, DebugUnit, StrUtils, configunit, Regexpr, globalskipunit,
  console, mrdohutils, SyncObjs
{$IFDEF MSWINDOWS}, Windows{$ENDIF}
  ;

const
  rsections = 'precatcher';

var
  catcherFilename, replacefromline: string;
  cd, skiprlses: THashedStringList;
  tagline, ignorelista, replacefrom, replaceto: TStringList;
  huntartunk: huntartunk_tipus;

  debug_f: TextFile;
  precatcher_debug_lock: TCriticalSection;

  ValidChars: set of char = ['0'..'9', 'A'..'Z', 'a'..'z', '?',
    '.', '>', '<', '+', '-', '~', '!', '@', '#', '$', '%', '&', '*',
    '(', ')', '_', '=', '{', '}', '[', ']', '|', '\', '/', ':', ';', ' '];
  StrippingChars: set of char = ['(', ')', '_', '-', '.', '&', '*', '<', '>'];

procedure mydebug(s: string); overload;
var
  nowstr: string;
begin
  Debug(dpSpam, rsections, s);
  if precatcher_ircdebug then
  begin
    try
      precatcher_debug_lock.Enter;
      try
        DateTimeToString(nowstr, 'mm-dd hh:nn:ss.zzz', Now());
        WriteLn(debug_f, Format('%s %s', [nowstr, s]));
        Flush(debug_f);
      finally
        precatcher_debug_lock.Leave;
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, rsections, Format('[EXCEPTION] mydebug: Exception : %s',
          [e.Message]));
        irc_Adderror(Format('<c4>[EXCEPTION]</c> mydebug: Exception : %s',
          [e.Message]));
      end;
    end;
  end;
  if (precatcher_debug) then
  begin
    irc_Addtext(precatcher_debug_netname, precatcher_debug_channel, s);
  end;
end;

procedure mydebug(s: string; args: array of const); overload;
begin
  myDebug(Format(s, args));
end;

function KibontasRiliz(sitename: string; var cdno: string; ts_data:
  TStringList): string;
var
  k, i: integer;
  maxi: integer;
  maxs: string;
begin
  cdno := '';

  //leghosszabb szo amiben van - a riliz
  maxi := 0;
  for i := 0 to ts_data.Count - 1 do
  begin
    if ((length(ts_data[i]) > maxi) and (0 <> Pos('-', ts_data[i]))) then
    begin
      maxi := length(ts_data[i]);
      maxs := ts_data[i];
    end;
  end;

  Result := maxs;

  k := length(Result);
  if (k > 0) and (Result[k] = '.') then
  begin
    Delete(Result, k, 1);
    Dec(k);
  end;
  if (k < minimum_rlsname) then
    Result := '';

  if ts_data.Count = 0 then
    exit;

  Result := trim(Result);
end;

function csupaszit(s: string): string;
var
  i: integer;
  skip: integer;
begin
  Result := '';
  skip := 0;
  for i := 1 to length(s) do
    if (skip = 0) then
    begin
      if (Ord(s[i]) >= 32) then
      begin
        if (Ord(s[i]) <> 255) then
        begin
          if (((s[i] >= 'a') and (s[i] <= 'z')) or
            ((s[i] >= 'A') and (s[i] <= 'Z')) or Szam(s[i]) or
            (s[i] in StrippingChars)) then
            Result := Result + s[i]
          else
            Result := Result + ' ';

        end
        else
          Result := Result + ' ';
      end
      else
      begin
        if ((s[i] = #3) and (i < length(s) - 2)) then
        begin
          if Szam(s[i + 1]) then
          begin
            if Szam(s[i + 2]) then
              skip := 2
            else
              skip := 1;
          end;
        end;
      end;
    end
    else
      Dec(skip);
end;

function StripNonAlpha(aInput: string): string;
var
  I: integer;
begin
  Result := '';
  for I := 1 to length(aInput) do
  begin
    if not (aInput[I] in ValidChars) then
      aInput[I] := ' ';
  end;
  Result := aInput;
end;

function Focsupaszitas(idata: string): string;
begin
  Result := idata;
  try
    Result := csupaszit(Result);
  except
    on e: Exception do
    begin
      Debug(dpError, rsections, Format('[EXCEPTION] csupaszit : %s',
        [e.Message]));
      irc_Adderror(Format('<c4>[EXCEPTION]</c> csupaszit : %s', [e.Message]));
      Result := '';
      exit;
    end;
  end;
  try
    Result := StripNonAlpha(Result);
  except
    on e: Exception do
    begin
      Debug(dpError, rsections, Format('[EXCEPTION] StripNonAlpha : %s',
        [e.Message]));
      irc_Adderror(Format('<c4>[EXCEPTION]</c> StripNonAlpha : %s',
        [e.Message]));
      Result := '';
      exit;
    end;
  end;
end;

function PrecatcherSectionMapping(rls, section: string; x_count: integer = 0):
  string;
var
  i: integer;
  x: TMap;
begin
  MyDebug(Format('PrecatcherSectionMapping start testing %s in %s', [rls,
    section]));

  Inc(x_count);
  if (x_count > 500) then
  begin
    Debug(dpError, rsections, Format(
      '[ERROR] in PrecatcherSectionMapping: big loop %s', [rls]));
    Result := '';
    exit;
  end;
  Result := section;

  for i := 0 to mappingslist.Count - 1 do
  begin
    try
      if i > mappingslist.Count then
        Break;
      x := mappingslist[i] as TMap;
      if (((x.origsection = '') and (x_count = 1)) or (x.origsection = Result))
        then
      begin
        MyDebug(Format('PrecatcherSectionMapping testing %s for %s',
          [rls, x.newsection]));
        if (x.mask.Matches(rls)) then
        begin
          if ((config.ReadBool(rsections, 'recursiv_mapping', False)) and
            (x.newsection <> 'TRASH')) then
          begin
            Result := PrecatcherSectionMapping(rls, x.newsection, x_count);
            exit;
          end
          else
          begin
            Result := x.newsection;
            MyDebug(Format('PrecatcherSectionMapping %s mapped to %s',
              [rls, x.newsection]));
            exit;
          end;
        end;
      end;
    except
      on E: Exception do
      begin
        Debug(dpError, rsections,
          Format('[EXCEPTION] in PrecatcherSectionMapping: %s',
          [e.Message]));
        break;
      end;
    end;
  end;

end;

{
function PrecatcherSectionMapping(rls, section: string; x_count : Integer = 0): string;
var i: Integer;
   x: TMap;
begin
 MyDebug(Format('PrecatcherSectionMapping start testing %s in %s', [rls, section]));

 inc(x_count);
 if (x_count > 500) then
 begin
   Debug(dpError, rsections, Format('[ERROR] in PrecatcherSectionMapping: big loop %s', [rls]));
   Result:= '';
   exit;
 end;

 Result:= section;
 for i:= 0 to mappingslist.Count -1 do
 begin

 if i > mappingslist.Count then Break;
 x:= mappingslist[i] as TMap;
if (((x.origsection = '') and (x_count = 1)) or (x.origsection = Result)) then
     begin
       MyDebug(Format('PrecatcherSectionMapping testing %s for %s', [rls, x.newsection]));
       if (x.mask.Matches(rls)) then
       begin
         if ((config.ReadBool(rsections,'recursiv_mapping',False)) and (x.newsection <> 'TRASH')) then
         begin
           Result := PrecatcherSectionMapping(rls, x.newsection, x_count);
           exit;
         end else begin
           Result:= x.newsection;
           MyDebug(Format('PrecatcherSectionMapping %s mapped to %s', [rls, x.newsection]));
           exit;
         end;
       end;
     end;

//    try if i > mappingslist.Count then Break; except Break; end;
//    try x:= mappingslist[i] as TMap; except Break; end;

   try
     if (((x.origsection = '') and (x_count = 1)) or (x.origsection = Result)) then
     begin
       MyDebug(Format('PrecatcherSectionMapping testing %s for %s', [rls, x.newsection]));
       if (x.mask.Matches(rls)) then
       begin
         if ((config.ReadBool(rsections,'recursiv_mapping',False)) and (x.newsection <> 'TRASH')) then
         begin
           Result := PrecatcherSectionMapping(rls, x.newsection, x_count);
           exit;
         end else begin
           Result:= x.newsection;
           MyDebug(Format('PrecatcherSectionMapping %s mapped to %s', [rls, x.newsection]));
           exit;
         end;
       end;
     end
   except
     Break;
   end;
 end;
end;
}

function KibontasSection(s, section: string): string;
var
  i: integer;
begin
  Result := section;
  if (Result = '') then
  begin
    for i := 0 to sectionlist.Count - 1 do
    begin
      if AnsiContainsText(s, sectionlist.ValueFromIndex[i]) then
      begin
        Result := sectionlist.Names[i];
        break;
      end;
    end;
  end;

end;

function ProcessDoReplace(s: string): string;
var
  i: integer;
  rep_s: string;
begin
  rep_s := s;

  if replacefrom.Count = replaceto.Count then
  begin
    for i := 0 to replacefrom.Count - 1 do
    begin
      MyDebug('ProcessDoReplace %s to %s', [replacefrom[i], replaceto[i]]);
      rep_s := Csere(rep_s, replacefrom[i], replaceto[i]);
    end;
  end
  else
    Debug(dpError, rsections, 'replacefrom count is <> replaceto count!');
  Result := rep_s;
  //  Irc_AddText('','','s= %s ;; rep_s= %s',[s,rep_s]);
end;

procedure ProcessReleaseVege(net, chan, nick, sitename, event, section: string;
  ts_data: TStringList);
var
  oldsection: string;
  rls: string;
  //    i: Integer;
  cdno: string;
  s: string;
begin
  // megvan, mar csak ki kell bontani a riliznevet
  try
    rls := KibontasRiliz(sitename, cdno, ts_data);
  except
    on E: Exception do
    begin
      Debug(dpError, rsections,
        Format('[EXCEPTION] in PrecatcherSectionMapping: %s',
        [e.Message]));
      exit;
    end;
  end;

  if (Trim(rls) = '') then
  begin
    Debug(dpError, rsections,
      '[EXCEPTION] in PrecatcherSectionMapping: relase is Empty');
    exit;
  end;

  MyDebug('ProcessReleaseVege %s %s %s %s', [rls, sitename, event, section]);
  Debug(dpSpam, rsections, Format('--> ProcessReleaseVege %s %s %s %s',
    [rls, sitename, event, section]));
  if rls <> '' then
  begin
    if (skiprlses.IndexOf(rls) <> -1) then
    begin
      MyDebug('Rls found in SkipRlses ...');
      Debug(dpSpam, rsections, 'Rls found in SkipRlses ...');
      exit;
    end;

    (* wadoh: i moved the replace stuff to  FoCsupaszitas, now we reraplce befor any catchline checkes.

      // elso korben megnezzuk van e ignoreword ra  -- this is the first place you look at to ignoreword
        s:= Csere(ts_data.DelimitedText, rls, '');
        // megcsinaljuk a [replace] szekcios csereket  -- We do the [replace] sectional exchanges
        s:= ProcessDoReplace(s);

        ts_data.DelimitedText:= s;

        *)
    (* Xperia : i dont think it can help
        // most levagjuk a sor veget ahol mar a juzer taglineja kezdodik vagyis barmi fassag lehet
        try
          j:= -1;
          for i:= 0 to tagline.Count -1 do
          begin
            j:= ts_data.IndexOf(tagline[i]);
            if j <> -1 then
              Break;
          end;
          if j <> -1 then
          begin
            k:= ts_data.Count - j;
            for i:= 1 to k do
            begin
              ts_data.Delete(j);
            end;
          end;
        except
          exit;
        end;
    *)

    if event <> 'REQUEST' then
    begin

      if CheckForBadAssGroup(rls) then
      begin
        MyDebug('<c4>[Bad Group]</c> detected!: ' + rls);
        Debug(dpSpam, rsections, 'Bad Group detected!: ' + rls);
        if not precatcher_debug then
          irc_addadmin('Bad Group detected!: ' + rls);
        skiprlses.Add(rls);
        //console_addline(net+' '+chan, Format('[%s] --> PRECATCHER Bad Group detected', [FormatDateTime('hh:nn:ss', Now)]));
        exit;
      end;

    end;

    // removing double spaces
    s := ts_data.DelimitedText;

    MyDebug('Cleanedup line wo rlsname: %s', [s]);
    Debug(dpSpam, rsections, 'Cleanedup line wo rlsname: %s', [s]);
    s := ' ' + s + ' ';

    if section = '' then
    begin
      section := KibontasSection(s, section);
    end;

    (*
    if section = '' then
    begin
      section:= sectionhelper.Values[rls];

      if section <> '' then
      begin
        MyDebug('Section by helper: %s', [section]);
        Debug(dpSpam, rsections, 'Section by helper: %s', [section]);
      end;
    end else
    begin
      MyDebug('Section: %s', [section]);
      Debug(dpSpam, rsections, 'Section: %s', [section]);
    end;
    *)

    MyDebug('Section: %s', [section]);

    if section <> 'REQUEST' then
    begin

      oldsection := section;
      try
        section := PrecatcherSectionMapping(rls, section);
      except
        on e: Exception do
        begin
          section := '';
          Debug(dpError, rsections,
            Format('[EXCEPTION] PrecatcherSectionMapping: %s',
            [e.Message]));
        end;
      end;

    end;
    if oldsection <> section then
    begin
      MyDebug('Mapped section: %s', [section]);
      Debug(dpSpam, rsections, 'Mapped section: %s', [section]);
    end;

    if section = '' then
    begin
      irc_Addadmin('No section?! ' + sitename + '@' + rls);
      MyDebug('No section?! ' + sitename + '@' + rls);
      Debug(dpSpam, rsections, 'No section?! ' + sitename + '@' + rls);
      exit;
    end;

    if (event = '') then
    begin
      event := 'NEWDIR';
    end;
    MyDebug('Event: %s', [event]);
    Debug(dpSpam, rsections, 'Event: %s', [event]);

    Debug(dpSpam, rsections, Format('-- ProcessReleaseVege %s %s %s %s',
      [rls, sitename, event, section]));
    if not precatcher_debug then
    begin
      try
        if (precatcher_spamevents.IndexOf(event) <> -1) then
        begin
          irc_Addtext_by_key('PRECATCHSTATS',
            Format('<c7>[%s]</c> %s %s @ <b>%s</b>', [event, section, rls,
            sitename]));
        end;
        kb_Add('', '', sitename, section, '', event, rls, '');
      except
        on e: Exception do
        begin
          Debug(dpError, rsections,
            Format('[EXCEPTION] ProcessReleaseVege kb_Add: %s',
            [e.Message]));
        end;
      end;
    end;
    Debug(dpSpam, rsections, Format('<-- ProcessReleaseVege %s %s %s %s',
      [rls, sitename, event, section]));
  end;
end;

procedure PrecatcherProcessB(net, chan, nick, Data: string);
var
  igindex, i, j: integer;
  sc: TSiteChan;
  ss: TSection;
  mind: boolean;
  ts_data: TStringList;
  rls, chno, s: string;
begin
  MyDebug('Process %s %s %s %s', [net, chan, nick, Data]);

  net := UpperCase(net);
  chan := LowerCase(chan);
  nick := LowerCase(nick);
  i := cd.IndexOf(net + chan + nick);
  if i <> -1 then
  begin
    MyDebug('Ok %s %s %s is valid for check', [net, chan, nick]);
    try
      sc := TSiteChan(cd.Objects[i]);
    except
      exit;
    end;

    ts_data := TStringList.Create;
    ts_data.CaseSensitive := False;
    ts_data.Delimiter := ' ';
    ts_data.QuoteChar := '"';

    try
      Data := FoCsupaszitas(Data); //main Stripping
    except
      on e: Exception do
      begin
        Debug(dpError, rsections, Format('[EXCEPTION] FoCsupaszitas : %s',
          [e.Message]));
        ts_data.Free;
        exit;
      end;
    end;

    ts_data.DelimitedText := Data;
    // megcsinaljuk a [replace] szekcios csereket  -- We do the [replace] sectional exchanges
    chno := '0';

    try
      rls := KibontasRiliz('SLFTP', chno, ts_data);
    except
      exit;
    end;

    (*     #chan bot user@NUKERS created Ginger... .  end in NUKEWORD found.

        for i := 0 to ignorelista.Count - 1 do
        begin
          if AnsiContainsText(ts_data.DelimitedText, ignorelista[i]) then
          begin
            MyDebug('Nukeword ' + ignorelista[i] + ' found in ' + rls);
            Debug(dpSpam, rsections, 'Nukeword ' + ignorelista[i] + ' found in ' + rls);
            skiprlses.Add(rls);
            //console_addline(net+' '+chan, Format('[%s] --> PRECATCHER Nukeword '+ignorelista[i]+' found in '+rls, [FormatDateTime('hh:nn:ss', Now)]));
            exit;
          end;
        end;
     *)

    for i := 0 to ts_data.Count - 1 do
    begin
      igindex := ignorelista.IndexOf(ts_data.Strings[i]);
      if igindex > -1 then
      begin
        MyDebug('Nukeword ' + ignorelista[i] + ' found in ' + rls);
        Debug(dpSpam, rsections, 'Nukeword ' + ignorelista.strings[igindex] +
          ' found in ' + rls);
        skiprlses.Add(rls);
        //console_addline(net+' '+chan, Format('[%s] --> PRECATCHER Nukeword '+ignorelista[i]+' found in '+rls, [FormatDateTime('hh:nn:ss', Now)]));
        exit;
      end;
    end;

    s := Csere(ts_data.DelimitedText, rls, '${RELEASENAMEPLACEHOLDER}$');

    s := ProcessDoReplace(s);

    s := Csere(s, '${RELEASENAMEPLACEHOLDER}$', rls);
    ts_data.DelimitedText := s;

    MyDebug('After FoCsupaszitas line is: %s', [ts_data.DelimitedText]);

    for i := 0 to sc.sections.Count - 1 do
    begin
      ss := TSection(sc.sections[i]);
      mind := True;
      for j := 0 to ss.words.Count - 1 do
      begin
        if (ts_data.IndexOf(ss.words[j]) = -1) then
        begin
          mind := False;
          // Irc_AddText('','','count: %d',[j]);
          Break;
        end;
      end;

      if ss.section = 'REQUESTS' then
        exit;

      if (mind) then
      begin
        try
          ProcessReleaseVege(net, chan, nick, sc.sitename, ss.eventtype,
            ss.section, ts_data);
        except
          on e: Exception do
          begin
            MyDebug('[EXCEPTION] ProcessReleaseVege : %s', [e.Message]);
            Debug(dpError, rsections,
              Format('[EXCEPTION] ProcessReleaseVege : %s',
              [e.Message]));
            ts_data.Free;
            exit;
          end;
        end;
        ts_data.Free;
        exit;
      end;
    end;

    if sc.sections.Count = 0 then
    begin
      try
        ProcessReleaseVege(net, chan, nick, sc.sitename, '', '', ts_data);
      except
        on e: Exception do
        begin
          MyDebug('[EXCEPTION] ProcessReleaseVege : %s', [e.Message]);
          Debug(dpError, rsections,
            Format('[EXCEPTION] ProcessReleaseVege : %s',
            [e.Message]));
          irc_Adderror(Format('<c4>[EXCEPTION]</c> ProcessReleaseVege : %s',
            [e.Message]));
          ts_data.Free;
          exit;
        end;
      end;
    end
    else
    begin
      MyDebug('SiteChan dont look like an Event ...');
    end;

    ts_data.Free;

  end
  else
  begin
    MyDebug('No SiteChan found for %s %s %s', [net, chan, nick]);
  end;
end;

procedure PrecatcherProcess(net, chan, nick, Data: string);
begin
  if not precatcherauto then
    Exit;
  try
    PrecatcherProcessB(net, chan, nick, Data);
  except
    on e: Exception do
    begin
      Debug(dpError, rsections, Format('[EXCEPTION] PrecatcherProcessB : %s',
        [e.Message]));
    end;
  end;
end;

function ProcessChannels(s: string): boolean;
var
  network, chan, nick, sitename, words: string;
  sci: integer;
  sc: TSiteChan;
  section: TSection;
  i, j: integer;
  nickc: integer;
  nickt: string;
begin
  Result := False;
  if (length(s) = 0) then
    exit;

  if (Count(';', s) < 3) then
    exit;

  network := UpperCase(SubString(s, ';', 1));
  chan := LowerCase(SubString(s, ';', 2));
  nickt := LowerCase(SubString(s, ';', 3));
  sitename := SubString(s, ';', 4);

  if (chan[1] <> '#') then
    exit;

  nickc := Count(',', nickt);

  for j := 1 to nickc + 1 do
  begin
    nick := SubString(nickt, ',', j);
    sci := cd.IndexOf(network + chan + nick);
    if (sci = -1) then
    begin
      sc := TSiteChan.Create();
      sc.sitename := sitename;
      cd.AddObject(network + chan + nick, sc);
    end
    else
      sc := TSiteChan(cd.Objects[sci]);

    if ((SubString(s, ';', 5) = '') and (SubString(s, ';', 7) = '')) then
      Continue;

    section := TSection.Create;
    section.section := SubString(s, ';', 7);
    section.eventtype := SubString(s, ';', 5);

    words := SubString(s, ';', 6);

    if (words <> '') then
      for i := 1 to Count(',', words) + 1 do
        section.words.Add(SubString(words, ',', i));

    sc.sections.Add(section);
  end;
  Result := True;
end;

procedure cdClear;
var
  i: integer;
begin
  for i := 0 to cd.Count - 1 do
  begin
    if cd.Objects[i] <> nil then
    begin
      cd.Objects[i].Free;
      cd.Objects[i] := nil;
    end;
  end;
  cd.Clear;
end;

procedure PrecatcherRebuild();
var
  i: integer;
  S: string;
var
  f: TextFile;
begin
  cdClear;

  i := 0;
  while (i < catcherFile.Count) do
  begin
    if not ProcessChannels(catcherFile[i]) then
    begin
      catcherFile.Delete(i);
      Dec(i);
    end;
    Inc(i);
  end;

  if (config.ReadBool('sites', 'split_site_data', False)) then
  begin
    for i := 0 to catcherFile.Count - 1 do // delete all old files first
    begin
      S := catcherFile[i];
      S := SubString(s, ';', 4);
      S := ExtractFilePath(ParamStr(0)) + 'rtpl' + PathDelim + S + '.chans';
      if FileExists(S) then
{$IFDEF MSWINDOWS}
        DeleteFile(PAnsiChar(S));
{$ELSE}
        DeleteFile(S);
{$ENDIF}
    end;

    for i := 0 to catcherFile.Count - 1 do // create if needed and append lines
    begin
      S := catcherFile[i];
      S := SubString(s, ';', 4);
      S := ExtractFilePath(ParamStr(0)) + 'rtpl' + PathDelim + S + '.chans';
      AssignFile(f, S);
      if (FileExists(S)) then
        Append(f)
      else
        Rewrite(f);
      WriteLn(f, catcherFile[i]);
      CloseFile(f);
    end;

    if FileExists(catcherFilename) then // convert to split format
{$IFDEF MSWINDOWS}
      DeleteFile(PAnsiChar(catcherFilename));
{$ELSE}
      DeleteFile(catcherFilename);
{$ENDIF}
  end
  else
  begin
    catcherFile.SaveToFile(catcherFilename);
  end;
end;

procedure ProcessRaceTool(s: string);
begin
  if (SubString(s, '=', 1) = 'minimum_rlsname') then
    minimum_rlsname := StrToIntDef(SubString(s, '=', 2), 10);
end;

procedure ProcessIgnoreList(s: string);
begin
  if (SubString(s, '=', 1) = 'nukewords') then
    ignorelista.DelimitedText := SubString(s, '=', 2)
  else if (SubString(s, '=', 1) = 'tagline') then
    tagline.DelimitedText := SubString(s, '=', 2);

end;

procedure ProcessReplace(s: string);
var
  i, db: integer;
  replacetoline: string;
  rx: TRegexpr;
begin
  rx := TRegexpr.Create;
  rx.ModifierI := True;
  rx.ModifierM := True;
  rx.Expression := '^(\#|\/\/)';
  if rx.Exec(s) then
  begin
    rx.Free;
    exit;
  end;
  rx.Free;

  if (SubString(s, '=', 1) = 'replacefrom') then
    replacefromline := trim(SubString(s, '=', 2))
  else if (SubString(s, '=', 1) = 'replaceto') then
  begin
    replacetoline := trim(SubString(s, '=', 2));
    replacetoline := Csere(replacetoline, '[:space:]', ' ');
    db := Count(';', replacefromline);
    for i := 1 to db + 1 do
    begin
      replacefrom.Add(SubString(replacefromline, ';', i));
      replaceto.Add(replacetoline);
    end;
  end;
end;

procedure ProcessSections(s: string);
var
  v, vv, section: string;
  rx: TRegexpr;
begin
  rx := TRegexpr.Create;
  rx.ModifierI := True;
  rx.ModifierM := True;
  rx.Expression := '^(\#|\/\/)';
  if rx.Exec(s) then
  begin
    rx.Free;
    exit;
  end;
  rx.Free;

  section := UpperCase(SubString(s, '=', 1));
  if (section <> '') then
  begin
    v := SubString(s, '=', 2);
    while (True) do
    begin
      vv := Trim(Fetch(v, ','));
      if ((vv = '') and (v = '')) then
        break;
      if (vv <> '') then
        sectionlist.Add(section + '= ' + vv + ' ');
    end;
  end;
end;

procedure ProcessMappings(s: string);
var
  db, i: integer;
  ss: string;
  rx: TRegexpr;
begin
  rx := TRegexpr.Create;
  rx.ModifierI := True;
  rx.ModifierM := True;
  rx.Expression := '^(\#|\/\/)';
  if rx.Exec(s) then
  begin
    rx.Free;
    exit;
  end;

  if Count(';', s) = 2 then
  begin
    ss := SubString(s, ';', 3);
    rx.Expression := '(\/.*?\/i?)';
    if rx.Exec(ss) then
    begin
      repeat
        mappingslist.Add(TMap.Create(SubString(s, ';', 1), SubString(s, ';', 2),
          rx.Match[1]));
      until not rx.ExecNext;
    end
    else
    begin
      db := Count(',', ss);
      for i := 1 to db + 1 do
        mappingslist.Add(TMap.Create(SubString(s, ';', 1), SubString(s, ';', 2),
          SubString(ss, ',', i)));
    end;
  end;
  rx.Free;
end;

procedure ProcessConfigLine(s: string);
begin
  if s = '[racetool]' then
    huntartunk := racetool
  else if s = '[ignorelist]' then
    huntartunk := ignorelist
  else if s = '[replace]' then
    huntartunk := replace
  else if s = '[sections]' then
    huntartunk := hunsections
  else if s = '[mappings]' then
    huntartunk := mappings
  else if s = '[channels]' then
    huntartunk := channels
  else if s = '[pretime]' then
    huntartunk := pretime;

  case huntartunk of
    racetool: ProcessRaceTool(s);
    ignorelist: ProcessIgnoreList(s);
    replace: ProcessReplace(s);
    hunsections: ProcessSections(s);
    mappings: ProcessMappings(s);
  end;
end;

function Precatcher_Sitehasachan(sitename: string): boolean;
var
  i: integer;
  sc: TSiteChan;
begin
  Result := False;
  for i := 0 to cd.Count - 1 do
  begin
    sc := TSiteChan(cd.Objects[i]);
    if sc.sitename = sitename then
    begin
      Result := True;
      break;
    end;
  end;
end;

procedure Precatcher_DelSiteChans(sitename: string);
var
  i: integer;
  s: string;
begin
  i := 0;

  while (i < catcherFile.Count) do
  begin
    s := catcherFile[i];
    s := SubString(s, ';', 4);
    if s = sitename then
    begin
      catcherFile.Delete(i);
      Dec(i);
    end;

    Inc(i);
  end;
end;

(*
procedure Precatcher_DelSiteChans(sitename: string);
var i: Integer;
    s: string;
begin
catcherFile.BeginUpdate;
try
//for I := 0 to catcherFile.Count - 1 do begin
for I := catcherFile.Count - 1 downto 0 do begin
s:= catcherFile[i];
s:= SubString(s, ';',4);
if s = sitename then catcherFile.Delete(i);
end;
finally
catcherFile.EndUpdate;
end;
end;
*)

function precatcher_logfilename: string;
begin
  Result := config.ReadString(rsections, 'debugfile',
    ExtractFilePath(ParamStr(0)) +
    'precatcher.log');
end;

procedure Precatcher_Init;
begin
  cd := THashedStringList.Create;
  cd.CaseSensitive := False;

  ignorelista := TStringList.Create;
  ignorelista.Delimiter := ' ';
  ignorelista.QuoteChar := '"';
  tagline := TStringList.Create;
  tagline.Delimiter := ' ';
  tagline.QuoteChar := '"';
  sectionlist := TStringList.Create;
  mappingslist := TObjectList.Create;
  skiprlses := THashedStringList.Create;

  replacefrom := TStringList.Create;
  replacefrom.Duplicates := dupAccept;
  replaceto := TStringList.Create;
  replaceto.Duplicates := dupAccept;

  huntartunk := sehun;

  // ezt itt most csak azert hogy jo sorrendben hivodjanak meg az inicializaciok
  catcherFilename := ExtractFilePath(ParamStr(0)) + 'slftp.chans';
  catcherFile := TEncStringList.Create(passphrase);

  precatcher_ircdebug := config.ReadBool(rsections, 'precatcher_debug', False);

  precatcher_debug_lock := TCriticalSection.Create();
  Assignfile(debug_f, precatcher_logfilename);
  try
    if FileExists(precatcher_logfilename) then
      Append(debug_f)
    else
      Rewrite(debug_f);
  except
    begin
      Writeln('Couldnt open logfile! It might be too huge?');
      halt;
    end;
  end;

  precatcher_spamevents := TStringList.Create;
  precatcher_spamevents.CommaText :=
    spamcfg.ReadString('precatcher', 'anounce_event', '');

end;

procedure Precatcher_UnInit;
begin
  Debug(dpSpam, rsections, 'Uninit1');

  ignorelista.Free;

  sectionlist.Free;
  mappingslist.Free;
  skiprlses.Free;
  tagline.Free;
  replacefrom.Free;
  replaceto.Free;

  catcherFile.Free;

  cdClear;
  cd.Free;

  precatcher_spamevents.Free;

  precatcher_debug_lock.Free;
  Closefile(debug_f);

  Debug(dpSpam, rsections, 'Uninit2');
end;

{ TMap }

constructor TMap.Create(origsection, newsection, mask: string);
begin
  self.origsection := origsection;
  self.newsection := newsection;
  self.mask := TslMask.Create(mask);
end;

destructor TMap.Destroy;
begin
  Mask.Free;
  inherited;
end;

constructor TSection.Create;
begin
  words := TStringList.Create;
end;

destructor TSection.Destroy;
begin
  words.Free;
  inherited;
end;

constructor TSiteChan.Create;
begin
  sections := TObjectList.Create;
end;

destructor TSiteChan.Destroy;
begin
  sections.Free;
  inherited;
end;

procedure LoadSplitChanFiles;
var
  fst: TStringList;
  S: string;
  i: Integer;
  intFound: Integer;
  SearchRec: TSearchRec;
  rules_path: string;
begin
  rules_path := ExtractFilePath(ParamStr(0)) + 'rtpl' + PathDelim;

  intFound := FindFirst(rules_path + '*.chans', faAnyFile, SearchRec);
  while intFound = 0 do
  begin
    fst := TStringList.Create();
    fst.LoadFromFile(rules_path + SearchRec.Name);
    for i := 0 to fst.Count - 1 do
    begin
      S := fst[i];
      catcherFile.Add(S);
    end;
    fst.Free;
    intFound := FindNext(SearchRec);
  end;

{$IFDEF MSWINDOWS}
  SysUtils.FindClose(SearchRec);
{$ELSE}
  FindClose(SearchRec);
{$ENDIF}
end;

procedure PrecatcherStart;
begin
  PrecatcherReload;
  catcherFile.LoadFromFile(catcherFileName);

  if (config.ReadBool('sites', 'split_site_data', False)) then
    LoadSplitChanFiles;

  PrecatcherReBuild;
end;

function precatcher_auto: boolean;
begin
  Result := sitesdat.ReadBool('precatcher', 'auto', False);
end;

(*  Old one not able to do a real reload....

procedure PrecatcherReload();
var f: TextFile;
    s: string;
    i: Integer;
begin
  sectionlist.Clear;

  AssignFile(f, ExtractFilePath(ParamStr(0))+'slftp.precatcher');
{$I-} Reset(f); {$I+}
  if IOResult = 0 then
  begin
    while (not Eof(f)) do
    begin
      ReadLn(f,s);
      ProcessConfigLine(s);
    end;
    CloseFile(f);
  end;
  sections.Clear;
  for i:= 0 to sectionlist.Count -1 do
    if sections.Indexof(sectionlist.Names[i]) = -1 then
      sections.Add(sectionlist.Names[i]);

end;

*)

procedure PrecatcherReload();
var
  f: TextFile;
  s: string;
  //    i: Integer;
begin
  mappingslist.Clear;
  sectionlist.Clear;
  ignorelista.Clear;
  replacefrom.Clear;
  replaceto.Clear;
  catcherFile.Clear;

  //catcherFile.LoadFromFile(catcherFileName);
  AssignFile(f, ExtractFilePath(ParamStr(0)) + 'slftp.precatcher');
{$I-}
  Reset(f);
{$I+}
  if IOResult = 0 then
  begin
    while (not EOF(f)) do
    begin
      ReadLn(f, s);
      ProcessConfigLine(s);
    end;
    CloseFile(f);
  end;

  kb_reloadsections;
end;

procedure PrecatcherReload(out status: string);
var
  f: TextFile;
  ss, s: string;
  //    i: Integer;
begin
  ss := '';
  mappingslist.Clear;
  sectionlist.Clear;
  ignorelista.Clear;
  replacefrom.Clear;
  replaceto.Clear;
  PrecatcherRebuild;
  catcherFile.Clear;
  catcherFile.LoadFromFile(catcherFileName);

  if (config.ReadBool('sites', 'split_site_data', False)) then
    LoadSplitChanFiles;

  ss := 'Precatcher Rehash FAILED!';
  try
    AssignFile(f, ExtractFilePath(ParamStr(0)) + 'slftp.precatcher');
{$I-}
    Reset(f);
{$I+}
    if IOResult = 0 then
    begin
      while (not EOF(f)) do
      begin
        ReadLn(f, s);
        ProcessConfigLine(s);
      end;
      CloseFile(f);
    end;
    kb_reloadsections;
  finally
    ss := 'Precatcher Rehash Complete....' + #13#10;
    ss := ss + 'Minimum_rlsname:' + IntToStr(minimum_rlsname) + #13#10;
    //ss:=ss+'COUNTS:'+#13#10;
    ss := ss +
      Format('Sections(%d) Mapping(%d) Replace|from/to:(%d/%d) Ignorlist(%d)',
      [kb_sections.Count, mappingslist.Count, replacefrom.Count,
      replaceto.Count, ignorelista.Count]);
  end;
  status := ss;
end;

function precatcherauto: boolean;
begin
  Result := sitesdat.ReadBool('precatcher', 'auto', False);
end;

function FindSection(section: string): boolean;
begin
  Result := False;
  if - 1 = sectionlist.IndexOf(UpperCase(section)) then
    exit;
  Result := True;
end;

end.

