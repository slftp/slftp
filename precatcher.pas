unit precatcher;

interface

uses Classes, Contnrs, slmasks, encinifile;

type
  TSection = class
    eventtype: AnsiString;
    section: AnsiString;
    words: TStringList;

    constructor Create;
    destructor Destroy; override;
  end;

  TSiteChan = class
    sitename: AnsiString;
    sections: TObjectList;
    constructor Create;
    destructor Destroy; override;
  end;

  huntartunk_tipus = (sehun, racetool, ignorelist, replace, hunsections,
    mappings, channels, pretime);

  TMap = class
    origsection: AnsiString;
    newsection: AnsiString;
    mask: TslMask;
    constructor Create(origsection, newsection, mask: AnsiString);
    destructor Destroy; override;
  end;

function precatcherauto: boolean;

function Precatcher_Sitehasachan(sitename: AnsiString): boolean;
procedure Precatcher_DelSiteChans(sitename: AnsiString);
function PrecatcherReload:AnsiString;
procedure PrecatcherRebuild();
procedure PrecatcherStart;
procedure PrecatcherProcessB(net, chan, nick, Data: AnsiString);
procedure PrecatcherProcess(net, chan, nick, Data: AnsiString);
function precatcher_logfilename: AnsiString;
procedure Precatcher_Init;
procedure Precatcher_Uninit;
function PrecatcherSectionMapping(const rls, section: AnsiString; x_count: integer = 0): AnsiString;

function FindSection(const section: AnsiString): boolean;
function ExtractReleasename(ts_data: TStringList): AnsiString;

function StripNoValidChars(aInput: AnsiString): AnsiString; // { removes all chars from string which are not in array ValidChars }

function KibontasSection(const s, section: AnsiString): AnsiString;
function ProcessDoReplace(const s: AnsiString): AnsiString;

var
  precatcher_debug: boolean = False;
  precatcher_ircdebug: boolean = False;
  precatcher_spamevents: TStringList;
  precatcher_debug_netname, precatcher_debug_channel: AnsiString;
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
  catcherFilename, replacefromline: AnsiString;
  cd, skiprlses: THashedStringList;
  tagline, irclines_ignorewords, replacefrom, replaceto: TStringList;
  huntartunk: huntartunk_tipus;

  debug_f: TextFile;
  precatcher_debug_lock: TCriticalSection;
  precatcher_lock: TCriticalSection;

  ValidChars: set of AnsiChar = ['0'..'9', 'A'..'Z', 'a'..'z', '?', '.', '>', '<', '+', '-', '~', '!', '@', '#', '$', '%', '&', '*', '(', ')', '_', '=', '{', '}', '[', ']', '|', '\',
    '/', ':', ';', ' '];
  StrippingChars: set of AnsiChar = ['(', ')', '_', '-', '.', '&', '*', '<', '>'];

procedure mydebug(s: AnsiString); overload;
var
  nowstr: AnsiString;
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
        Debug(dpError, rsections, Format('[EXCEPTION] mydebug: Exception : %s', [e.Message]));
        irc_Adderror(Format('<c4>[EXCEPTION]</c> mydebug: Exception : %s', [e.Message]));
      end;
    end;
  end;
  if (precatcher_debug) then
  begin
    irc_Addtext(precatcher_debug_netname, precatcher_debug_channel, s);
  end;
end;

procedure mydebug(s: AnsiString; args: array of const); overload;
begin
  myDebug(Format(s, args));
end;


function FindSection(const section: AnsiString): boolean;
begin
  Result := False;
  if sectionlist.IndexOf(UpperCase(section)) = -1 then
    exit;
  Result := True;
end;

function ExtractReleasename(ts_data: TStringList): AnsiString;
var
  k, i: integer;
  maxi: integer;
  maxs: AnsiString;
begin
  Result := '';

  // no need to go further if it's empty
  if ts_data.Count = 0 then
    exit;

  // detect longest entry with '-' --> our releasename
  maxi := 0;
  maxs := '';
  for i := 0 to ts_data.Count - 1 do
  begin
    if ((Length(ts_data[i]) > maxi) and (0 <> Pos('-', ts_data[i]))) then
    begin
      maxi := Length(ts_data[i]);
      maxs := ts_data[i];
    end;
  end;

  Result := maxs;

  // remove '.' from the end of detected releasename if there is one
  k := Length(Result);
  if (k > 0) and (Result[k] = '.') then
  begin
    Dec(k);
    SetLength(Result, k);
  end;

  if (k < minimum_rlsname) then
    Result := '';

  Result := Trim(Result);
end;

function RemoveSpecialCharsAndBareIt(const s: AnsiString): AnsiString;
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
          if (IsALetter(s[i]) or IsANumber(s[i]) or (s[i] in StrippingChars)) then
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
          if IsANumber(s[i + 1]) then
          begin
            if IsANumber(s[i + 2]) then
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

function StripNoValidChars(aInput: AnsiString): AnsiString;
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

function MainStripping(const idata: AnsiString): AnsiString;
begin
  Result := idata;
  try
    Result := RemoveSpecialCharsAndBareIt(Result);
  except
    on e: Exception do
    begin
      Debug(dpError, rsections, Format('[EXCEPTION] RemoveSpecialCharsAndBareIt : %s', [e.Message]));
      irc_Adderror(Format('<c4>[EXCEPTION]</c> RemoveSpecialCharsAndBareIt : %s', [e.Message]));
      Result := '';
      exit;
    end;
  end;

  // this part below is useless, or?
  // above we only allow a-z, A-Z, Numbers and StrippingChars - else we replace the char with ' '
  // then we check the response in StripNoValidChars against ValidChars which includes a lot more chars but our
  // response won't have them in it because it's already replaced with ' '
  try
    Result := StripNoValidChars(Result);
  except
    on e: Exception do
    begin
      Debug(dpError, rsections, Format('[EXCEPTION] StripNoValidChars : %s', [e.Message]));
      irc_Adderror(Format('<c4>[EXCEPTION]</c> StripNoValidChars : %s', [e.Message]));
      Result := '';
      exit;
    end;
  end;
end;

function PrecatcherSectionMapping(const rls, section: AnsiString; x_count: integer = 0): AnsiString;
var
  i: integer;
  x: TMap;
begin
  MyDebug(Format('PrecatcherSectionMapping start testing %s in %s', [rls, section]));

  Inc(x_count);
  if (x_count > 500) then
  begin
    Debug(dpError, rsections, Format('[ERROR] in PrecatcherSectionMapping: big loop %s', [rls]));
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
      if (((x.origsection = '') and (x_count = 1)) or (x.origsection = Result)) then
      begin
        MyDebug(Format('PrecatcherSectionMapping testing %s for %s', [rls, x.newsection]));
        if (x.mask.Matches(rls)) then
        begin
          if ((config.ReadBool(rsections, 'recursiv_mapping', False)) and (x.newsection <> 'TRASH')) then
          begin
            Result := PrecatcherSectionMapping(rls, x.newsection, x_count);
            exit;
          end
          else
          begin
            Result := x.newsection;
            MyDebug(Format('PrecatcherSectionMapping %s mapped to %s', [rls, x.newsection]));
            exit;
          end;
        end;
      end;
    except
      on E: Exception do
      begin
        Debug(dpError, rsections, Format('[EXCEPTION] in PrecatcherSectionMapping: %s', [e.Message]));
        break;
      end;
    end;
  end;

end;

function KibontasSection(const s, section: AnsiString): AnsiString;
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

function _findMP3GenreOnAnnounce(const text: AnsiString; ts_data: TStringList): String;
var
  //i, x: Integer;
  i: Integer;
begin
  Result := '';
  for i := 0 to mp3genres.Count - 1 do
  begin
  {
  * TODO
  * only useful if we add an extra event for GENRE
    * [info][mp3] Keller_Williams_Kwahtro-Sync-WEB-2017-ENTiTLED remaining(122.4MB) Rock(2017)
    * ( MP3 )-( Presk_-_2BXPRZD-(SOHASOMRGWLD01)-WEB-2017-HQEM )-( Expecting 4F of 320kbps Techno from 2017 )
    x := ts_data.IndexOf(mp3genres[i]);
    if x <> -1 then
    begin
      Result := mp3genres[i];
      Debug(dpError, rsections, Format('_findMP3GenreOnAnnounce TStringList %s %s', [text, Result]));
    end;

    note: Can't we !catchadd a line with event UPDATE to handle this line ?

  }

    if (AnsiContainsText(text, mp3genres[i]) or AnsiContainsText(Csere(mp3genres[i], ' ', ''), text)) then
    begin
      Result := mp3genres[i];
      break;
    end;
  end;
end;

function ProcessDoReplace(const s: AnsiString): AnsiString;
var
  i: integer;
  rep_s: AnsiString;
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
end;

procedure ProcessReleaseVege(net, chan, nick, sitename, event, section, rls: AnsiString; ts_data: TStringList);
var
  genre, s, oldsection: AnsiString;
begin
  MyDebug('ProcessReleaseVege %s %s %s %s', [rls, sitename, event, section]);
  Debug(dpSpam, rsections, Format('--> ProcessReleaseVege %s %s %s %s', [rls, sitename, event, section]));

  if event <> 'REQUEST' then
  begin

    if CheckForBadAssGroup(rls) then
    begin
      MyDebug('<c4>[SKIPPED GROUP]</c> detected!: ' + rls);
      Debug(dpSpam, rsections, 'Skipped group detected!: ' + rls);
      if not precatcher_debug then
        irc_addadmin('<b><c14>Info</c></b>: Skipped group detected!: ' + rls);
      skiprlses.Add(rls);
      exit;
    end;

  end;

  // removing double spaces
  s := ts_data.DelimitedText;

  MyDebug('Cleaned up line with rlsname: %s', [s]);
  Debug(dpSpam, rsections, 'Cleaned up line with rlsname: %s', [s]);
  s := ' ' + s + ' ';

  try
    if section = '' then
    begin
      section := KibontasSection(s, section);
    end;
    MyDebug('Section: %s', [section]);
  except
    on E: Exception do
    begin
      Debug(dpError, rsections, Format('[EXCEPTION] KibontasSection: %s', [e.Message]));
    end;
  end;

  if section <> 'REQUEST' then
  begin

    oldsection := section;
    try
      section := PrecatcherSectionMapping(rls, section);
    except
      on e: Exception do
      begin
        section := '';
        Debug(dpError, rsections, Format('[EXCEPTION] PrecatcherSectionMapping: %s', [e.Message]));
      end;
    end;
  end;

  if oldsection <> section then
  begin
    MyDebug('Mapped section: %s', [section]);
    Debug(dpSpam, rsections, 'Mapped section: %s', [section]);
  end;

  if ((section = '') AND (event <> 'COMPLETE') AND (event <> 'NUKE')) then
  begin
    irc_Addadmin('<c14><b>Info</c></b>: Section on %s for %s was not found. Add Sectionname to slftp.precatcher under [sections] and/or [mappings].', [sitename, rls]);
    MyDebug('No section?! ' + sitename + '@' + rls);
    exit;
  end;

  if (event = '') then
    event := 'NEWDIR';

  genre := '';
  if ((event <> 'NEWDIR') and (FindSectionHandler(section).Name = 'TMP3Release')) then
  begin
    genre := _findMP3GenreOnAnnounce(s, ts_data);
    if genre <> '' then
    begin
      MyDebug('Genre: %s', [genre]);
      Debug(dpSpam, rsections, Format('Genre found via _findMP3GenreOnAnnounce: %s', [genre]));
    end;
  end;

  MyDebug('Event: %s', [event]);
  Debug(dpSpam, rsections, 'Event: %s', [event]);

  Debug(dpSpam, rsections, Format('-- ProcessReleaseVege %s %s %s %s', [rls, sitename, event, section]));
  if not precatcher_debug then
  begin
    try
      if (precatcher_spamevents.IndexOf(event) <> -1) then
      begin
        irc_Addtext_by_key('PRECATCHSTATS', Format('<c7>[%s]</c> %s %s @ <b>%s</b>', [event, section, rls, sitename]));
      end;
      kb_Add('', '', sitename, section, genre, event, rls, '');
    except
      on e: Exception do
      begin
        Debug(dpError, rsections, Format('[EXCEPTION] ProcessReleaseVege kb_Add: %s', [e.Message]));
      end;
    end;
  end;

  Debug(dpSpam, rsections, Format('<-- ProcessReleaseVege %s %s %s %s', [rls, sitename, event, section]));
end;

procedure PrecatcherProcessB(net, chan, nick, Data: AnsiString);
var
  igindex, i, j: integer;
  sc: TSiteChan;
  ss: TSection;
  mind: boolean;
  ts_data: TStringList;
  //rls, chno, s: AnsiString; // chno isn't used
  rls, s: AnsiString;
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
    try
      ts_data.CaseSensitive := False;
      ts_data.Delimiter := ' ';
      ts_data.QuoteChar := '"';

      try
        Data := MainStripping(Data); //main Stripping
      except
        on e: Exception do
        begin
          Debug(dpError, rsections, Format('[EXCEPTION] MainStripping : %s', [e.Message]));
          exit;
        end;
      end;

      ts_data.DelimitedText := Data;
      MyDebug('After main stripping line is: %s', [ts_data.DelimitedText]);


      MyDebug('Checking main stripped line for ignore words.');
      // ignorewords check
      // word by word check for single words
      for i := 0 to ts_data.Count - 1 do
      begin
        igindex := irclines_ignorewords.IndexOf(ts_data.Strings[i]);
        if igindex > -1 then
        begin
          MyDebug('Ignoreword ' + irclines_ignorewords[igindex] + ' found in ' + Data);
          Debug(dpSpam, rsections, 'Ignoreword ' + irclines_ignorewords.strings[igindex] + ' found in ' + Data);
          exit;
        end;
      end;

      // fulltext check for quoted phrases (that contains at least one space)
      for i := 0 to irclines_ignorewords.Count - 1 do
      begin
        if AnsiContainsText(irclines_ignorewords[i],' ') and AnsiContainsText(ts_data.DelimitedText, irclines_ignorewords[i]) then
        begin
          MyDebug('Ignoreword (phrase) "' + irclines_ignorewords[i] + '" found in ' + Data);
          Debug(dpSpam, rsections, 'Ignoreword (phrase) "' + irclines_ignorewords[i] + '" found in ' + Data);
          exit;
        end;
      end;



      // Extract the release name, returns '' when no rlsname found
      try
        rls := ExtractReleasename(ts_data);
      except
        on e: Exception do
        begin
          Debug(dpError, rsections, Format('[EXCEPTION] ExtractReleasename : %s (%s)', [e.Message, ts_data.DelimitedText]));
          exit;
        end;
      end;

      if (rls = '') then
      begin
        Debug(dpSpam, rsections, Format('PrecatcherProcessB: Relasename is empty! (%s)', [ts_data.DelimitedText]));
        exit;
      end;

      if (skiprlses.IndexOf(rls) <> -1) then
      begin
        MyDebug('Release found in SkipRlses ...');
        Debug(dpSpam, rsections, Format('Release %s found in SkipRlses (%s) ...', [rls, skiprlses.ValueFromIndex[skiprlses.IndexOf(rls)]]));
        exit;
      end;


      // do the [replace] from slftp.precatcher
      s := Csere(ts_data.DelimitedText, rls, '${RELEASENAMEPLACEHOLDER}$');
      s := ProcessDoReplace(s);
      s := Csere(s, '${RELEASENAMEPLACEHOLDER}$', rls);
      ts_data.DelimitedText := s;

      MyDebug('After replace line is: %s', [ts_data.DelimitedText]);


      // Find section name
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

        if ss.section = 'REQUEST' then
        begin
          // maybe we can do something here to automatically fill requests with a 'site search' like those mirc scripts do
          exit;
        end;

        if (mind) then
        begin
          try

            precatcher_lock.Enter;
            try
               ProcessReleaseVege(net, chan, nick, sc.sitename, ss.eventtype, ss.section, rls, ts_data);
            finally
              precatcher_lock.Leave;
            end;

          except
            on e: Exception do
            begin
              MyDebug('[EXCEPTION] ProcessReleaseVegeB mind = true : %s', [e.Message]);
              Debug(dpError, rsections, Format('[EXCEPTION] ProcessReleaseVegeB mind = true: %s || net: %s, chan: %s, nick: %s || site: %s, event: %s, section: %s, rls: %s || ts_data: %s', [e.Message, net, chan, nick, sc.sitename, ss.eventtype, ss.section, rls, ts_data.Text]));
              exit;
            end;
          end;
          exit;
        end;
      end;


      if sc.sections.Count = 0 then
      begin
        try

          precatcher_lock.Enter;
          try
            ProcessReleaseVege(net, chan, nick, sc.sitename, '', '', rls, ts_data);
          finally
            precatcher_lock.Leave;
          end;


        except
          on e: Exception do
          begin
            MyDebug('[EXCEPTION] ProcessReleaseVegeB section count = 0: %s', [e.Message]);
            Debug(dpError, rsections, Format('[EXCEPTION] ProcessReleaseVegeB section count = 0 : %s', [e.Message]));
            irc_Adderror(Format('<c4>[EXCEPTION]</c> ProcessReleaseVegeB section count = 0 : %s', [e.Message]));
            exit;
          end;
        end;
      end
      else
      begin
        MyDebug('No catcher event found.');
      end;

    finally
      ts_data.Free;
    end;

  end
  else
  begin
    MyDebug('No catchline found for %s %s %s', [net, chan, nick]);
  end;
end;

procedure PrecatcherProcess(net, chan, nick, Data: AnsiString);
begin
  if not precatcherauto then
    Exit;

{
  precatcher_lock.Enter;
  try
}
    try
      PrecatcherProcessB(net, chan, nick, Data);
    except
      on e: Exception do
      begin
        Debug(dpError, rsections, Format('[EXCEPTION] PrecatcherProcess : %s', [e.Message]));
      end;
    end;
{
  finally
    precatcher_lock.Leave;
  end;
}

end;

function ProcessChannels(s: AnsiString): boolean;
var
  network, chan, nick, sitename, words: AnsiString;
  sci: integer;
  sc: TSiteChan;
  section: TSection;
  i, j: integer;
  nickc: integer;
  nickt: AnsiString;
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
  S: AnsiString;
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

procedure ProcessRaceTool(s: AnsiString);
begin
  if (SubString(s, '=', 1) = 'minimum_rlsname') then
    minimum_rlsname := StrToIntDef(SubString(s, '=', 2), 10);
end;

procedure ProcessIgnoreList(s: AnsiString);
begin
  if (SubString(s, '=', 1) = 'ignorewords') then
    irclines_ignorewords.DelimitedText := SubString(s, '=', 2)
  else if (SubString(s, '=', 1) = 'tagline') then
    tagline.DelimitedText := SubString(s, '=', 2);

end;

procedure ProcessReplace(s: AnsiString);
var
  i, db: integer;
  replacetoline: AnsiString;
  rx: TRegexpr;
begin
  rx := TRegexpr.Create;
  try
    rx.ModifierI := True;
    rx.ModifierM := True;
    rx.Expression := '^(\#|\/\/)';

    if rx.Exec(s) then
    begin
      exit;
    end;

  finally
    rx.Free;
  end;

  if (SubString(s, '=', 1) = 'replacefrom') then
  begin
    replacefromline := trim(SubString(s, '=', 2))
  end
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

procedure ProcessSections(s: AnsiString);
var
  v, vv, section: AnsiString;
  rx: TRegexpr;
begin
  rx := TRegexpr.Create;
  try
    rx.ModifierI := True;
    rx.ModifierM := True;
    rx.Expression := '^(\#|\/\/)';

    if rx.Exec(s) then
    begin
      exit;
    end;

  finally
    rx.Free;
  end;

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

procedure ProcessMappings(s: AnsiString);
var
  db, i: integer;
  ss: AnsiString;
  rx: TRegexpr;
begin
  rx := TRegexpr.Create;
  try
    rx.ModifierI := True;
    rx.ModifierM := True;
    rx.Expression := '^(\#|\/\/)';

    if rx.Exec(s) then
    begin
      exit;
    end;

    if Count(';', s) = 2 then
    begin
      ss := SubString(s, ';', 3);
      rx.Expression := '(\/.*?\/i?)';
      if rx.Exec(ss) then
      begin
        repeat
          mappingslist.Add(TMap.Create(UpperCase(SubString(s, ';', 1)), UpperCase(SubString(s, ';', 2)), rx.Match[1]));
        until not rx.ExecNext;
      end
      else
      begin
        db := Count(',', ss);
        for i := 1 to db + 1 do
          mappingslist.Add(TMap.Create(UpperCase(SubString(s, ';', 1)), UpperCase(SubString(s, ';', 2)), SubString(ss, ',', i)));
      end;
    end;

  finally
    rx.Free;
  end;
end;

procedure ProcessConfigLine(s: AnsiString);
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

function Precatcher_Sitehasachan(sitename: AnsiString): boolean;
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

procedure Precatcher_DelSiteChans(sitename: AnsiString);
var
  i: integer;
  s: AnsiString;
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

function precatcher_logfilename: AnsiString;
begin
  Result := ExtractFilePath(ParamStr(0)) + config.ReadString(rsections, 'debugfile', 'precatcher.log');
end;

procedure Precatcher_Init;
begin
  cd := THashedStringList.Create;
  cd.CaseSensitive := False;

  irclines_ignorewords := TStringList.Create;
  irclines_ignorewords.Delimiter := ' ';
  irclines_ignorewords.QuoteChar := '"';
  irclines_ignorewords.Sorted := True;
  irclines_ignorewords.Duplicates := dupIgnore;

  precatcher_lock := TCriticalSection.Create;

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

  // ezt itt most csak azert hogy jo sorrendben hivodjanak meg az inicializaciok -- Now it here just so that good order should call the initialization ??
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
  precatcher_spamevents.CommaText := spamcfg.ReadString('precatcher', 'anounce_event', '');
end;

procedure Precatcher_UnInit;
begin
  Debug(dpSpam, rsections, 'Uninit1');

  irclines_ignorewords.Free;

  precatcher_lock.Free;

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

constructor TMap.Create(origsection, newsection, mask: AnsiString);
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
  S: AnsiString;
  i: Integer;
  intFound: Integer;
  SearchRec: TSearchRec;
  rules_path: AnsiString;
begin
  catcherFile.Clear;
  rules_path := ExtractFilePath(ParamStr(0)) + 'rtpl' + PathDelim;

  intFound := FindFirst(rules_path + '*.chans', faAnyFile, SearchRec);
  while intFound = 0 do
  begin
    fst := TStringList.Create();
    try
      fst.LoadFromFile(rules_path + SearchRec.Name);
      for i := 0 to fst.Count - 1 do
      begin
        S := fst[i];
        catcherFile.Add(S);
      end;
    finally
      fst.Free;
    end;
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
  // Actually starting precatcher is an initial reload
  PrecatcherReload;
end;

function PrecatcherReload:AnsiString;
var
  f: TextFile;
  s: AnsiString;

begin
  // clear in-memory data
  mappingslist.Clear;
  sectionlist.Clear;
  irclines_ignorewords.Clear;
  replacefrom.Clear;
  replaceto.Clear;
  catcherFile.Clear;

  // load slftp.chans
  catcherFile.LoadFromFile(catcherFileName);

  // load rtpl/<site>.chans if split_site_data is enabled
  if (config.ReadBool('sites', 'split_site_data', False)) then
    LoadSplitChanFiles;

  result := 'Precatcher reload FAILED!';
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
    end;
    kb_reloadsections;

  finally
    CloseFile(f);
  end;

  // Rewrite files to disk
  PrecatcherRebuild;

  result := 'Precatcher reloaded successfully.' + sLineBreak;
  result := result + 'Minimum_rlsname: ' + IntToStr(minimum_rlsname) + sLineBreak;
  result := result + Format('Sections (%d) - Mapping (%d) - Replace|from/to: (%d/%d) - Ignorelist (%d)', [kb_sections.Count, mappingslist.Count, replacefrom.Count, replaceto.Count, irclines_ignorewords.Count]);
end;

function precatcherauto: boolean;
begin
  Result := sitesdat.ReadBool('precatcher', 'auto', False);
end;

end.

