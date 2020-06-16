{
  @abstract(Knowledge base classes for release information)
  The classes hold different information which are needed for all the different
  categories of releases
}
unit kb.releaseinfo;

interface

uses
  Classes, knowngroups;

type
  {
  @value(kbeUNKNOWN UNKNOWN event, for anything we don't know or handle)
  @value(kbePRE PRE event, triggered by new pres on sites)
  @value(kbeSPREAD SPREAD event, triggered by !spread)
  @value(kbeNEWDIR NEWDIR event, triggered by new races on sites)
  @value(kbeCOMPLETE COMPLETE event, triggered by completed races on sites)
  @value(kbeREQUEST REQUEST event, triggered by requests on sites)
  @value(kbeNUKE NUKE event, triggered by nukes on sites)
  @value(kbeADDPRE ADDPRE event, triggered by !addpre/!sitepre announces picked up on IRC)
  @value(kbeUPDATE UPDATE event, triggered to re-check rules and routes)
  }
  TKBEventType = (kbeUNKNOWN, kbePRE, kbeSPREAD, kbeNEWDIR, kbeCOMPLETE, kbeREQUEST, kbeNUKE, kbeADDPRE, kbeUPDATE);

  { @abstract(Base class for common release information) }
  TRelease = class
  private
    FCurrentYear: Integer; //< Value of the current year (e.g. 2019)
  public
    aktualizalva: boolean;
    aktualizalasfailed: boolean;
    rlsname: String; //< releasename
    rlsnamewithoutgrp: String; //< @link(rlsname) with removed @link(groupname)
    section: String;
    words: TStringList; //< list of all words which occur in @link(rlsname), firstly removes () and then replaces .-_ with whitespace
    groupname: String; //< name of release group extracted from @link(rlsname) by \-([^\-]+)$ regex
    internal: boolean; //< @true if @link(rlsname) matches [\_\-\.]\(?(internal|int)\)?([\_\-\.]|$) regex, otherwise @false
    disks: integer;
    kb_event: TKBEventType;
    language: String; //< contains the language string which is detected from @link(rlsname)

    legnagyobbcd: integer;
    sample: boolean;
    covers: boolean;
    subs: boolean;

    fake: boolean;
    fakereason: String;

    pretime: int64; //< UTC pretime for release
    pretimefrom: String; // info where we found the pretime (see @link(dbaddpre.TPretimeResult))

    pretimefound: boolean;
    PredOnAnySite: boolean; //< indicates if it's pred on any of your sites

    // for fake checking
    dots: integer; //< amount of dots ('.') in @link(rlsname)
    number_of_chars: integer;
    vowels: integer; //< amount of vowels [aeiouAEIOU] in @link(rlsname)

    year: integer;

    knowngroup: TKnownGroup;

    constructor Create(const rlsname, section: String; FakeChecking: boolean = True; SavedPretime: int64 = -1); virtual;
    destructor Destroy; override;
    function Aktualizald(const extrainfo: String): boolean; virtual;
    function AsText(pazo_id: integer = -1): String; virtual;
    function Aktualizal(p: TObject): boolean; virtual;
    procedure SetPretime(TimeStamp: int64 = 0);
    class function Name: String; virtual;// abstract;
    class function DefaultSections: String; virtual; abstract;
    class function SectionAccepted(const section: String): boolean;

    property CurrentYear: Integer read FCurrentYear;
  end;

  { @abstract(Class with support for 0-DAY release information) }
  T0DayRelease = class(TRelease)
  public
    nulldaysource: String;

    constructor Create(const rlsname, section: String; FakeChecking: boolean = True; SavedPretime: int64 = -1); override;
    class function Name: String; override;
    class function DefaultSections: String; override;
    function AsText(pazo_id: integer = -1): String; override;
  end;

  { @abstract(Class with support for music release information) }
  TMP3Release = class(TRelease)
  private
    { Splits the word into source type and number of disks if word contains at least one number. Extracted values could be garbish, verifying of aSourceType needed!
      @param(aWord Single word from the releasename)
      @param(aSourceType Extracted source type)
      @param(aNumberOfDisks Extracted number) }
    procedure GetNumberOfDisksFromTag(const aWord: String; var aSourceType: String; var aNumberOfDisks: Integer);
    function Evszam(s: String): boolean;
    procedure AddSource(const src: String);
  public
    mp3year: integer;
    mp3lng: String; //< mapped language from @link(TRelease.language), remains for backward compatibility of mp3language rule
    mp3genre: String;
    mp3source: String;
    mp3types1: String;
    mp3types2: String;
    mp3types3: String;
    mp3_numdisks: integer; //< Amount of disks
    mp3_number_of: String;
    mp3_va: boolean; //< @true if made by Various Artists, otherwise @false

    function Bootleg: boolean;
    constructor Create(const rlsname, section: String; FakeChecking: boolean = True; SavedPretime: int64 = -1); override;
    function Aktualizald(const extrainfo: String): boolean; override;
    function AsText(pazo_id: integer = -1): String; override;
    function Numdisks: integer;
    function Aktualizal(p: TObject): boolean; override;
    function mp3type(const s: String): boolean;
    class function Name: String; override;
    class function DefaultSections: String; override;
  end;

  { @abstract(Class with support for release information which are parsed from NFO file) }
  TNFORelease = class(TRelease)
    nfogenre: String;

    constructor Create(const rlsname, section: String; FakeChecking: boolean = True; SavedPretime: int64 = -1); override;
    function Aktualizald(const extrainfo: String): boolean; override;
    function AsText(pazo_id: integer = -1): String; override;
    function Aktualizal(p: TObject): boolean; override;
    class function Name: String; override;
    class function DefaultSections: String; override;
  end;

  { @abstract(Class with support for release information parsed from IMDb) }
  TIMDBRelease = class(TRelease)
    FLookupDone: Boolean;
    imdb_id: String;
    imdb_year: integer;
    imdb_languages: TStringList;
    imdb_countries: TStringList;
    imdb_genres: TStringList;
    imdb_screens: integer;
    imdb_rating: integer;
    imdb_votes: integer;
    CineYear: integer;
    imdb_ldt: boolean;
    imdb_wide: boolean;
    imdb_festival: boolean;
    imdb_stvm: boolean; // TODO: rename this to make it more clear; stvm and stvs aren't clear yet
    imdb_stvs: String;

    constructor Create(const rlsname, section: String; FakeChecking: boolean = True; SavedPretime: int64 = -1); override;
    destructor Destroy; override;
    function Aktualizald(const extrainfo: String): boolean; override;
    function AsText(pazo_id: integer = -1): String; override;
    function Aktualizal(p: TObject): boolean; override;
    class function Name: String; override;
    class function DefaultSections: String; override;

    property IsLookupDone: Boolean read FLookupDone; //< @true of IMDb Lookup is done and infos are fully added to @link(TIMDBRelease), otherwise @false
  end;

  { @abstract(Class with support for release information parsed from TVMaze) }
  TTVRelease = class(TRelease)
    FLookupDone: Boolean;
    showname: String;
    episode: integer;
    season: integer;
    premier_year: integer;
    ended_year: integer;
    country: String;
    classification: String;
    scripted: boolean;
    genres: TStringList;
    network: String;
    runtime: integer;
    seasons: integer;
    status: String;
    running: boolean;
    currentseason: boolean;
    currentepisode: boolean;
    currentair: boolean;
    daily: boolean;
    showid: String; // aka TVMaze ID
    thetvdbid: String;
    tvrageid: String;
    tvtag: String;
    tvlanguage: String;
    tvrating: integer; //< tv rating value (max score is 100, min score is 0)

    constructor Create(const rlsname, section: String; FakeChecking: boolean = True; SavedPretime: int64 = -1); override;
    destructor Destroy; override;
    function Aktualizald(const extrainfo: String): boolean; override;
    function AsText(pazo_id: integer = -1): String; override;
    function Aktualizal(p: TObject): boolean; override;
    class function Name: String; override;
    class function DefaultSections: String; override;

    property IsLookupDone: Boolean read FLookupDone; //< @true of TVMaze Lookup is done and infos are fully added to @link(TTVRelease), otherwise @false
  end;

  { @abstract(Class with support for music video release information) }
  TMVIDRelease = class(TRelease)
    FileCount: integer;
    mvid_Genre: TStringList;
    // TRelease.language is mapped for mvidlanguage rule
    mvid_source: String;
    mvid_pal: boolean;
    mvid_ntsc: boolean;
    mvid_va: boolean;
    mvid_live: boolean;
    mvid_year: integer;

    constructor Create(const rlsname, section: String; FakeChecking: boolean = True; SavedPretime: int64 = -1); override;
    destructor Destroy; override;
    function Aktualizald(const extrainfo: String): boolean; override;
    function AsText(pazo_id: integer = -1): String; override;
    function Aktualizal(p: TObject): boolean; override;
    class function Name: String; override;
    class function DefaultSections: String; override;
  end;

type
  TCRelease = class of TRelease;
  TSectionHandlers = array[0..6] of TCRelease; //< see @link(GlSectionHandlers)

{ Just a helper function to initialize the variables }
procedure KbReleaseInit;

{ Just a helper function to free the variables }
procedure KbReleaseUninit;

{ Converts a stringified event to a real KB Event
  @param(aEvent event name as a string)
  @returns(TKBEventType from input @link(aEvent), defaulting to @link(kbeUNKNOWN)
    if @link(aEvent) can't be turned into a known event.) }
function EventStringToTKBEventType(const aEvent: string): TKBEventType;

{ Converts a KB event to a readable string representation
  @param(aEvent the kb event entry)
  @returns(Eventname as a string) }
function KBEventTypeToString(const aEvent: TKBEventType): String;

var
  GlSectionHandlers: TSectionHandlers = (TRelease, TMP3Release, T0dayRelease, TNFORelease, TIMDBRelease, TTVRelease, TMVIDRelease); //< Array of all release information classes
  GlNullDayPlatformTags: TStringList; //< List with 0Day platform tags which define the platform when tagging releases
  GlTvTags: TStringList; //< List with TV tags which are used for tagging releases
  GlMP3Sources: TStringList; //< List of MP3 sources used for tagging; Names = source & Value = possible release tag(s)
  GlMP3Genres: TStringList; //< List of MP3 genres

implementation

uses
  debugunit, mainthread, taskgenrenfo, taskgenredirlist, configunit, console,
  taskrace, sitesunit, queueunit, pazo, irc, SysUtils, fake, mystrings,
  rulesunit, Math, DateUtils, StrUtils, precatcher, tasktvinfolookup,
  slvision, tasksitenfo, RegExpr, taskpretime, taskgame, mygrouphelpers,
  sllanguagebase, taskmvidunit, dbaddpre, dbaddimdb, dbtvinfo, irccolorunit,
  mrdohutils, ranksunit, tasklogin, dbaddnfo, contnrs, slmasks, dirlist, SyncObjs,
  globalskipunit, irccommandsunit, Generics.Collections {$IFDEF MSWINDOWS}, Windows{$ENDIF};

const
  configsection = 'kb';
  rsections = 'kb.releaseinfo';

var
  kb_sectionhandlers: TStringList;
  // [kb] config vars from inifile
  nomp3dirlistgenre: boolean;
  nonfodirlistgenre: boolean;
  nomvdirlistgenre: boolean;
  glMP3Types: TStringList; //< List of MP3 types

procedure KbReleaseInit;
var
  i, j: integer;
  x: TStringList;
  sectionmasks: TObjectList;
  sectionmask: String;
  ss: String;
  fGenreHelper: String;
begin
  nomp3dirlistgenre := config.ReadBool(configsection, 'nomp3dirlistgenre', False);
  nonfodirlistgenre := config.ReadBool(configsection, 'nonfodirlistgenre', False);
  nomvdirlistgenre := config.ReadBool(configsection, 'nomvdirlistgenre', False);

  kb_sectionhandlers := TStringList.Create;
  for i := 1 to High(GlSectionHandlers) do
  begin
    {
    * some examples for both variables
    GlSectionHandlers: TMP3Release -- x: MP3,FLAC
    GlSectionHandlers: T0dayRelease -- x: 0DAY,PDA
    GlSectionHandlers: TNFORelease -- x: MDVDR,MV,MHD
    GlSectionHandlers: TTVRelease -- x: TVSD*,TV720P-*,TV*BLURAY,TV1080P
    }

    kb_sectionhandlers.Add(GlSectionHandlers[i].Name);

    sectionmasks := TObjectList.Create;

    x := TStringList.Create;
    try
      x.CaseSensitive := False;
      x.Delimiter := ',';
      // ignore dupe entries for lower memory usage
      x.Sorted := True;
      x.Duplicates := dupIgnore;

      x.DelimitedText := config.ReadString(configsection, GlSectionHandlers[i].Name, GlSectionHandlers[i].DefaultSections);

      for j := 0 to x.Count - 1 do
      begin
        sectionmask := x[j];
        sectionmasks.Add(TslMask.Create(sectionmask));
      end;
    finally
      x.Free;
    end;

    kb_sectionhandlers.Objects[kb_sectionhandlers.Count - 1] := sectionmasks;
  end;

  GlMP3Sources := TStringList.Create;
  GlNullDayPlatformTags := TStringList.Create;

  x := TStringList.Create;
  try
    config.ReadSection(configsection, x);
    for i := 0 to x.Count - 1 do
    begin
      if (1 = Pos('mp3source_', x[i])) then
      begin
        GlMP3Sources.Values[UpperCase(Copy(x[i], 11, 20))] := ' ' + config.ReadString(configsection, x[i], '') + ' ';
      end
      else if (1 = Pos('0daysource_', x[i])) then
      begin
        GlNullDayPlatformTags.Values[UpperCase(Copy(x[i], 12, 20))] := ' ' + config.ReadString(configsection, x[i], '') + ' ';
      end;
    end;
  finally
    x.Free;
  end;

  glMP3Types := TStringList.Create;
  glMP3Types.Delimiter := ' ';
  glMP3Types.QuoteChar := '"';
  glMP3Types.DelimitedText := config.ReadString(configsection, 'mp3types', 'Bootleg MAG Advance Bonus CDM CDS Concert Demo Digipak EP Live LP MCD Promo Reissue Remastered Retail Sampler Split Audiobook ABOOK INTERVIEW');

  GlMP3Genres := TStringList.Create;
  GlMP3Genres.Delimiter := ' ';
  GlMP3Genres.QuoteChar := '"';
  fGenreHelper := '"Instrumental Rock" "Techno Industrial" "Instrumental Pop" "Progressive Rock" "Psychedelic Rock" "Native American" "Symphonic Rock" "Easy Listening" "Southern Rock" "Christian Rap" "National Folk" "Chamber Music" "Rhythmic Soul"';
  fGenreHelper := fGenreHelper + '"Classic Rock" "Instrumental" "Power Ballad" "Alternative" "Thrash Metal" "Heavy Metal" "Death Metal" "Black Metal" "Euro Techno" "Psychedelic" "Rock & Roll" "Fast Fusion"';
  fGenreHelper := fGenreHelper + '"Gothic Rock" "Porn Groove" "Industrial" "Soundtrack" "Sound Clip" "AlternRock" "Meditative" "Electronic" "Avantgarde" "Booty Bass" "Euro House" "Dance Hall" "Jazz+Funk" "Classical" "Eurodance"';
  fGenreHelper := fGenreHelper + '"Showtunes" "Acid Punk" "Acid Jazz" "Hard Rock" "Folk Rock" "Bluegrass" "Slow Rock" "Freestyle" "Punk Rock" "Drum Solo" "A capella" "Trip Hop" "Darkwave" "Pop Folk" "Pop Funk" "New Wave" "Big Band"';
  fGenreHelper := fGenreHelper + '"Hardcore" "Acoustic" "Symphony" "Slow Jam" "Folklore" "Country" "Hip-Hop" "Hip Hop" "New Age" "Ambient" "Gangsta" "Cabaret" "Trailer" "Musical" "Revival" "Chanson" "Grunge" "Oldies" "Reggae"';
  fGenreHelper := fGenreHelper + '"Techno" "Pranks" "Fusion" "Trance" "Gospel" "Ethnic" "Gothic" "Comedy" "Top 40" "Jungle"';
  fGenreHelper := fGenreHelper + '"Tribal" "Celtic" "Chorus" "Humour" "Speech" "Sonata" "Primus" "Satire" "Ballad" "Blues" "Dance" "Disco" "Metal" "Other"';
  fGenreHelper := fGenreHelper + '"Vocal" "House" "Noise" "Space" "Dream" "Lo Fi" "Polka" "Retro" "Swing" "Bebob" "Latin" "Indie" "Opera" "Tango" "Samba" "Funk" "Jazz" "Rock" "Acid"';
  fGenreHelper := fGenreHelper + '"Game" "Bass" "Soul" "Punk" "Cult" "Rave" "Folk" "Club" "Duet" "Pop" "R&B" "Rap" "Ska" "CPOP" "KPOP" "JPOP"';
  GlMP3Genres.DelimitedText := config.ReadString(rsections, 'mp3genres', fGenreHelper);
  i := 0;
  while (i < GlMP3Genres.Count) do
  begin
    ss := ReplaceText(GlMP3Genres[i], ' ', '');
    if ss <> GlMP3Genres[i] then
    begin
      GlMP3Genres.Insert(i + 1, ss);
      Inc(i);
    end;
    Inc(i);
  end;

  GlTvTags := TStringList.Create;
  GlTvTags.CaseSensitive := False;
  GlTvTags.DelimitedText := config.ReadString(configsection, 'tvtags', 'AHDTV APDTV ADSR BDRip BluRay DSR DVDR DVDRip HDTV HDTVRip HR.PDTV PDTV WebRip WEB WebHD SATRip dTV');
end;

procedure KbReleaseUninit;
var
  i: integer;
begin
  GlNullDayPlatformTags.Free;
  GlMP3Sources.Free;
  glMP3Types.Free;
  GlMP3Genres.Free;
  GlTvTags.Free;

  for i := 0 to kb_sectionhandlers.Count - 1 do
  begin
    if Assigned(kb_sectionhandlers.Objects[i]) then
    begin
      kb_sectionhandlers.Objects[i].Free;
    end;
  end;
  kb_sectionhandlers.Free;
end;

function EventStringToTKBEventType(const aEvent: string): TKBEventType;
begin
  Result := TEnum<TKBEventType>.FromString('kbe' + aEvent, kbeUNKNOWN);
end;

function KBEventTypeToString(const aEvent: TKBEventType): String;
begin
  Result := ReplaceText(TEnum<TKBEventType>.ToString(aEvent), 'kbe', '');
end;

{ TRelease }

function TRelease.Aktualizal(p: TObject): boolean;
begin
  aktualizalva := True;
  Result := False;
end;

function TRelease.Aktualizald(const extrainfo: String): boolean;
begin
  aktualizalva := False;
  Result := False;
end;

function TRelease.AsText(pazo_id: integer = -1): String;
begin
  Result := '';
  try
    Result := Format('<b>%s</b>', [rlsname]);
    if pazo_id <> -1 then
      Result := Result + Format(' (%d)', [pazo_id]);
    Result := Result + #13#10;

    Result := Result + 'Knowngroup: ';
    if knowngroup = grp_known then
      Result := Result + '1'
    else if knowngroup = grp_unknown then
      Result := Result + '0';
    if knowngroup = grp_notconfigured then
      Result := Result + '?';
    Result := Result + #13#10;

    if (pretime = 0) then
      Result := Result + 'Pretime not found!' + #13#10
    else
      Result := Result + Format('Pretime: %s (%s)', [dbaddpre_GetPreduration(pretime), FormatDateTime('yyyy-mm-dd hh:nn:ss', UnixToDateTime(pretime, False))]) + #13#10;

    if disks <> 1 then
      Result := Result + Format('Disks: %d', [disks]) + #13#10;

    if fake then
      Result := Result + Format('Fake: %s', [fakereason]) + #13#10;

    if language <> '' then
      Result := Result + Format('Language: %s', [language]) + #13#10;

    Result := Result + Format('Internal: %s', [BoolToStr(internal, True)]) + #13#10;
  except
    on e: Exception do
    begin
      Debug(dpError, rsections, 'TRelease.AsText : %s', [e.Message]);
    end;
  end;
end;

constructor TRelease.Create(const rlsname, section: String; FakeChecking: boolean = True; SavedPretime: int64 = -1);
var
  s: String;
  i, j: integer;
  rrgx: TRegExpr;
begin
  try
    aktualizalva := False;
    PredOnAnySite := False;

    Self.section := section;
    Self.rlsname := rlsname;

    if SavedPretime > -1 then
    begin
      try
        self.pretime := SavedPretime;
      except
        on e: Exception do
          irc_Adderror(Format('TRelease.Create: Exception saving pretime %s %d (%s)', [rlsname, SavedPretime, e.Message]));
      end;
    end
    else
    begin
      try
        SetPretime;
      except
        on e: Exception do
          irc_Adderror(Format('TRelease.Create: Exception SetPretime %s (%s)', [rlsname, e.Message]));
      end;
    end;

    words := TStringList.Create;
    words.Delimiter := ' ';
    words.CaseSensitive := False;

    s := ReplaceText(rlsname, '(', '');
    s := ReplaceText(s, ')', '');
    s := ReplaceText(s, '.', ' ');
    s := ReplaceText(s, '-', ' ');
    s := ReplaceText(s, '_', ' ');

    words.DelimitedText := s;

    internal := False;

    rrgx := TRegExpr.Create;
    try
      rrgx.ModifierI := True;

      rrgx.Expression := '[\_\-\.]\(?(internal|int)\)?([\_\-\.]|$)';
      if rrgx.Exec(rlsname) then
        internal := True;

      // detect groupname
      groupname := '';
      rrgx.Expression := '\-([^\-]+)$';
      if rrgx.Exec(rlsname) then
      begin
        groupname := rrgx.Match[1];
      end;
    finally
      rrgx.free;
    end;

    // old way if groupname not found by regex
    if (groupname = '') then
    begin
      if UpperCase(words.strings[words.Count - 1]) = 'INT' then
        groupname := words.strings[words.Count - 2] + '_' + words.strings[words.Count - 1]
      else
        groupname := words.strings[words.Count - 1];
    end;

    dots := 0;
    number_of_chars := 0;
    vowels := 0;
    s := '';
    for i := 1 to length(rlsname) do
    begin
      if 0 = Pos(rlsname[i], s) then
      begin
        Inc(number_of_chars);
        s := s + rlsname[i];
      end;
      if rlsname[i] = '.' then
        Inc(dots);
      if (rlsname[i] in ['a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U']) then
        Inc(vowels);
    end;

    rlsnamewithoutgrp := Copy(rlsname, 1, Length(rlsname) - Length(groupname));

    // language detection
    if ((Self is TMP3Release) or (Self is TMVIDRelease)) then
    begin
      language := FindMusicLanguageOnDirectory(rlsname);
    end
    else
    begin
      language := FindLanguageOnDirectory(rlsname);
    end;

    knowngroup := IsKnownGroup(section, groupname);

    for i := words.Count - 1 downto 0 do
    begin
      year := StrToIntDef(words[i], 0);
      if year > 1900 then
        break;
    end;
    if year < 1900 then
      year := 0;

    FCurrentYear := StrToInt(FormatDateTime('yyyy', Now));

    disks := 1;
    for i := words.Count - 1 downto 0 do
    begin
      if AnsiContainsText(words[i], 'disc') then
      begin
        disks := 0;
        j := 1;
        while (j <= length(words[i])) do
        begin
          if words[i][j] in ['0'..'9'] then
            disks := disks * 10 + Ord(words[i][j]) - 48
          else
            Break;
          Inc(j);
        end;

        Break;
      end;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, rsections, 'TRelease.Create : %s', [e.Message]);
    end;
  end;

  if FakeChecking then
    FakeCheck(self);
end;

destructor TRelease.Destroy;
begin
  words.Free;
  inherited;
end;

procedure TRelease.SetPretime(TimeStamp: int64 = 0);
var
  resu: TPretimeResult;
begin
  Debug(dpSpam, rsections, 'TRelease.SetPretime start');
  if TimeStamp <> 0 then
  begin
    pretime := TimeStamp;
    pretimefrom := 'Parameter';
  end
  else
  begin
    resu := getPretime(rlsname);
    pretime := resu.pretime;
    pretimefrom := resu.mode;
  end;
  Debug(dpSpam, rsections, 'TRelease.SetPretime end');
end;

class function TRelease.SectionAccepted(const section: String): boolean;
var
  i, j: integer;
  sectionmask: TslMask;
  sectionmasks: TObjectList;
begin
  Result := False;

  try
    // check if there is an entry for the TRelease descendent
    i := kb_sectionhandlers.IndexOf(Name);
    if i = -1 then
      exit;

    // use sectionmasks for found TRelease descendent
    sectionmasks := TObjectList(kb_sectionhandlers.Objects[i]);
    for j := 0 to sectionmasks.Count - 1 do
    begin
      sectionmask := TslMask(sectionmasks[j]);

      if sectionmask.Matches(section) then
      begin
        Result := True;
        exit;
      end;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, rsections, 'TRelease.SectionAccepted : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

class function TRelease.Name: String;
begin
  Result := 'TRelease';
end;

{ TMP3Release }

procedure TMP3Release.GetNumberOfDisksFromTag(const aWord: String; var aSourceType: String; var aNumberOfDisks: Integer);
var
  i, fWordLen: Integer;
  fNumber: Integer;
begin
  fWordLen := Length(aWord);
  fNumber := 0;

  aSourceType := '';
  aNumberOfDisks := 0;

  for i := 1 to fWordLen do
  begin
    if IsANumber(aWord[i]) then
      fNumber := fNumber * 10 + Ord(aWord[i]) - 48
    else
      Break;
  end;

  if fNumber = 0 then
    exit; // nothing found

  if ((i <= fWordLen - 2) and (aWord[i] = 'x')) then
    Inc(i);

  aSourceType := Copy(aWord, i, fWordLen);
  aNumberOfDisks := fNumber;
end;

function TMP3Release.Evszam(s: String): boolean;
var
  i: integer;
begin
  Result := False;
  try
    if (length(s) = 4) then
    begin
      i := OccurrencesOfNumbers(s);
      if (i = 4) then
      begin
        mp3Year := StrToIntDef(s, 1900);
        Result := True;
      end
      else if ((i = 3) and ((s[4] = 'x') or (s[4] = 'X'))) then
      begin
        s[4] := '0';
        mp3Year := StrToIntDef(s, 1900);
        Result := True;
      end;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, rsections, 'TMP3Release.Evszam : %s', [e.Message]);
      Result := False;
    end;
  end;
end;

procedure TMP3Release.AddSource(const src: String);
begin
  // TODO: find out what it does, maybe mp3source + src is better?
  if mp3source = '' then
    mp3source := src;
  (*
  case sources of
    1: mp3source1:= src;
    2: mp3source2:= src;
    3: mp3source3:= src;
  end;*)
end;

constructor TMP3Release.Create(const rlsname, section: String; FakeChecking: boolean = True; SavedPretime: int64 = -1);
var
  evszamindex, i: Integer;

  types: Integer;
  j: Integer;
  fWord, fSource: String;

  lrx: TRegexpr;
  fNumberDisks: Integer;
  fNumberOfDashes: Integer;
  fNumDisksAlreadyFound: Boolean;
begin
  inherited Create(rlsname, section, False, savedpretime);
  aktualizalva := False;
  fNumDisksAlreadyFound := False;

  // some kind of fake/scam detection?
  if words.Count < 3 then
    exit;

  fNumberOfDashes := 0;
  for i := 1 to Length(rlsname) do
  begin
    if rlsname[i] = '-' then
    begin
      Inc(fNumberOfDashes);
      if (fNumberOfDashes = 2) then
        Break;
    end;
  end;
  if fNumberOfDashes < 2 then
    exit;

  try
    mp3year := 0;
    evszamindex := 0;
    for i := 1 to 3 do
      if Evszam(words[words.Count - i]) then
      begin
        evszamindex := words.Count - i;
        Break;
      end;

    if mp3year = 0 then
      mp3year := year;
    if mp3year = 0 then
      exit;
    //We did not find out the year. Sucking, useless to continue.

    if ((not internal) and (evszamindex + 3 = words.Count)) then
      groupname := words[evszamindex + 1] + '_' + words[evszamindex + 2]; //tweak

    // use language from TRelease ancestor
    mp3lng := language;

    types := 0;
    mp3_numdisks := 1;

    for i := words.Count - 1 downto 1 do
    begin
      //1CD 99DVD
      fWord := ' ' + words[i] + ' ';
      if not fNumDisksAlreadyFound then
      begin
        fNumberDisks := 0;
        GetNumberOfDisksFromTag(words[i], fSource, fNumberDisks);
      end;
      for j := 0 to GlMP3Sources.Count - 1 do
      begin
        if (AnsiContainsText(GlMP3Sources.ValueFromIndex[j], fWord)) then
        begin
          AddSource(GlMP3Sources.Names[j]);
          Break;
        end
        else if ((fNumberDisks <> 0) and (AnsiContainsText(GlMP3Sources.ValueFromIndex[j], ' ' + fSource + ' '))) then
        begin
          AddSource(GlMP3Sources.Names[j]);
          mp3_numdisks := fNumberDisks;
          mp3_number_of := words[i];
          fNumDisksAlreadyFound := True;
          Break;
        end;
      end;

      if ((types < 3) and (glMP3Types.IndexOf(words[i]) <> -1)) then
      begin
        Inc(types);
        case types of
          1: mp3types1 := words[i];
          2: mp3types2 := words[i];
          3: mp3types3 := words[i];
        end;
      end;
    end;

    lrx := TRegexpr.Create;
    try
      lrx.ModifierI := True;
      lrx.Expression := '^(va[\-\_\.]|Various[\.\_]Artists?)';
      mp3_va := lrx.Exec(rlsname);
    finally
      lrx.Free;
    end;

    AddSource('CD'); // default
  except
    on e: Exception do
    begin
      Debug(dpError, rsections, 'TMP3Release.Create : %s', [e.Message]);
    end;
  end;

  if FakeChecking then
    FakeCheck(self);
end;

function TMP3Release.Aktualizald(const extrainfo: String): boolean;
begin
  Result := False;
  if length(extrainfo) > length(mp3genre) then
  begin
    aktualizalva := True;
    Result := True;
    mp3genre := extrainfo;
  end;
end;

function TMP3Release.AsText(pazo_id: integer = -1): String;
begin
  Result := inherited AsText(pazo_id);

  try
    Result := Result + Format('Year: %d', [mp3year]) + #13#10;
    if mp3genre <> '' then
      Result := Result + Format('Genre: %s', [mp3genre]) + #13#10;
    Result := Result + Format('Source: %s', [mp3source]) + #13#10;
    if mp3types1 <> '' then
      Result := Result + Format('Type1: %s', [mp3types1]) + #13#10;
    if mp3types2 <> '' then
      Result := Result + Format('Type2: %s', [mp3types2]) + #13#10;
    if mp3types3 <> '' then
      Result := Result + Format('Type3: %s', [mp3types3]) + #13#10;
    Result := Result + Format('Disks: %d', [mp3_numdisks]) + #13#10;
    Result := Result + Format('VA: %s', [BoolToStr(mp3_va, True)]) + #13#10;
  except
    on e: Exception do
    begin
      Debug(dpError, rsections, 'TMP3Release.AsText : %s', [e.Message]);
    end;
  end;
end;

function TMP3Release.Bootleg: boolean;
begin
  Result := False;
  if 0 = AnsiCompareText(mp3types1, 'bootleg') then
    Result := True
  else if 0 = AnsiCompareText(mp3types2, 'bootleg') then
    Result := True
  else if 0 = AnsiCompareText(mp3types3, 'bootleg') then
    Result := True;

end;

function TMP3Release.Numdisks: integer;
begin
  Result := mp3_numdisks;
end;

function TMP3Release.Aktualizal(p: TObject): boolean;
var
  pazo: TPazo;
  shot: TPazoSite;
begin
  Result := False;
  aktualizalva := True;

  if 1 = Pos('PRE', section) then
    exit; //itt nem...
  if nomp3dirlistgenre then
    exit;

  pazo := TPazo(p); // ugly shit

  shot := FindMostCompleteSite(pazo);
  if shot <> nil then
  begin
    try
      AddTask(TPazoGenreDirlistTask.Create('', '', shot.Name, pazo, 1));
    except
      on e: Exception do
      begin
        Debug(dpError, rsections, Format('[EXCEPTION] TMP3Release.Aktualizal.AddTask: %s', [e.Message]));
      end;
    end;
    Result := True;
  end;
end;

class function TMP3Release.Name: String;
begin
  Result := 'TMP3Release';
end;

class function TMP3Release.DefaultSections: String;
begin
  Result := 'MP3';
end;

function TMP3Release.mp3type(const s: String): boolean;
begin
  Result := False;
  if ((SameText(mp3types1, s)) or (SameText(mp3types2, s)) or (SameText(mp3types3, s))) then
    Result := True;
end;

{ TNFORelease }

function TNFORelease.Aktualizal(p: TObject): boolean;
var
  pazo: TPazo;
  shot: TPazoSite;
  i: integer;
begin
  Result := False;
  aktualizalva := True;
  if nonfodirlistgenre then
    exit;

  pazo := TPazo(p); // ugly shit

  i := last_addnfo.IndexOf(pazo.rls.rlsname);
  if i <> -1 then
    exit;

  shot := FindMostCompleteSite(pazo);
  if shot <> nil then
  begin
    try
      AddTask(TPazoGenreNfoTask.Create('', '', shot.Name, pazo, 1));
    except
      on e: Exception do
      begin
        Debug(dpError, rsections,
          Format('[EXCEPTION] TNFORelease.Aktualizal.AddTask: %s',
          [e.Message]));
      end;
    end;
    Result := True;
  end;
end;

function TNFORelease.Aktualizald(const extrainfo: String): boolean;
begin
  Result := False;
  if length(extrainfo) > length(nfogenre) then
  begin
    aktualizalva := True;
    Result := True;
    nfogenre := extrainfo;
  end;
end;

function TNFORelease.AsText(pazo_id: integer = -1): String;
begin
  Result := inherited AsText(pazo_id);
  try
    Result := Result + Format('nfo genre: %s', [nfogenre]) + #13#10;
  except
    on e: Exception do
    begin
      Debug(dpError, rsections, 'TNFORelease.AsText : %s', [e.Message]);
    end;
  end;
end;

constructor TNFORelease.Create(const rlsname, section: String;
  FakeChecking: boolean = True; SavedPretime: int64 = -1);
begin
  inherited Create(rlsname, section, False, savedpretime);
  nfogenre := '';
end;

class function TNFORelease.DefaultSections: String;
begin
  Result := 'MDVDR MV MHD';
end;

class function TNFORelease.Name: String;
begin
  Result := 'TNFORelease';
end;

{ TTVRelease }

function TTVRelease.Aktualizal(p: TObject): boolean;
var
  pazo: TPazo;
  db_tvinfo: TTVInfoDB;

  procedure CreateTVLookupTask;
  begin
    try
      AddTask(TPazoTVInfoLookupTask.Create('', '', getAdminSiteName, pazo, 1));
    except
      on e: Exception do
      begin
        Debug(dpError, rsections, Format('[EXCEPTION] TTVRelease.Aktualizal.AddTask: %s', [e.Message]));
      end;
    end;
  end;

begin
  Result := False;

  aktualizalva := True;
  if showname = '' then
    exit;

  // ugly shit
  pazo := TPazo(p);

  // check if we already have this showname in database
  try
    db_tvinfo := getTVInfoByShowName(self.showname);
  except
    on e: Exception do
    begin
      FreeAndNil(db_tvinfo);
      Debug(dpError, rsections, Format('Exception in TTVRelease.Aktualizal.getTVInfoByShowName: %s', [e.Message]));
    end;
  end;

  if (db_tvinfo <> nil) then
  begin
    // showname was found in db
    db_tvinfo.ripname := rlsname;

    if DaysBetween(UnixToDateTime(db_tvinfo.last_updated), now()) >= config.ReadInteger('tasktvinfo', 'days_between_last_update', 6) then
    begin
      // try to update infos in database
      if not db_tvinfo.Update then
      begin
        Debug(dpError, rsections, Format('[ERROR] updating of %s failed.', [showname]));
        irc_AddError(Format('<c4><b>ERROR</c></b>: updating of %s failed.', [showname]));
      end;

      // triggers SetTVDbRelease inside the task to get the updated data set to TTVRelease
      // TODO: the complete behavior should really be overhauled!
      CreateTVLookupTask;
    end
    else
    begin
      // we have a recent set of data
      try
        db_tvinfo.SetTVDbRelease(self);
      except
        on e: Exception do
        begin
          Debug(dpError, rsections, Format('Exception in SetTVDbRelease: %s', [e.Message]));
        end;
      end;
    end;

    FreeAndNil(db_tvinfo);
  end
  else
  begin
    // do websearch for a non existing showname in database
    CreateTVLookupTask;
  end;

  Result := True;
end;

function TTVRelease.Aktualizald(const extrainfo: String): boolean;
begin
  aktualizalva := True;
  Result := False;
end;

function TTVRelease.AsText(pazo_id: integer): String;
var
  fMismatchReason: String;
begin
  Result := inherited AsText(pazo_id);
  try
    Result := Result + Format('Show name: %s', [showname]) + #13#10;
    Result := Result + Format('URL: http://www.tvmaze.com/shows/%s/%s', [showid, Lowercase(ReplaceText(showname, ' ', '-'))]) + #13#10;

    if (season < 0) then
    begin
      fMismatchReason := TEnum<TTVGetShowValuesIdentifier>.ToString(TTVGetShowValuesIdentifier(season)).Replace('tv', '');
      Result := Result + Format('Season: %d (Reason: %s)', [season, fMismatchReason]) + #13#10;
    end
    else
      Result := Result + Format('Season: %d', [season]) + #13#10;

    if (episode < 0) then
    begin
      fMismatchReason := TEnum<TTVGetShowValuesIdentifier>.ToString(TTVGetShowValuesIdentifier(episode)).Replace('tv', '');
      Result := Result + Format('Episode: %d (Reason: %s)', [episode, fMismatchReason]) + #13#10;
    end
    else
      Result := Result + Format('Episode: %d', [episode]) + #13#10;

    if premier_year <> -1 then
      Result := Result + Format('Premier: %d', [premier_year]) + #13#10;
    if ended_year > 0 then
      Result := Result + Format('Ended: %d', [ended_year]) + #13#10;
    if country <> '' then
      Result := Result + Format('Country: %s', [country]) + #13#10;
    if classification <> '' then
      Result := Result + Format('Classification: %s', [classification]) + #13#10;
    Result := Result + Format('Scripted: %s', [BoolToStr(scripted, True)]) + #13#10;
    if genres.Count > 0 then
      Result := Result + Format('Genres: %s', [genres.CommaText]) + #13#10;
    if network <> '' then
      Result := Result + Format('Network: %s', [network]) + #13#10;
    if tvlanguage <> '' then
      Result := Result + Format('TV Language: %s', [tvlanguage]) + #13#10;
    Result := Result + Format('Running: %s', [BoolToStr(running, True)]) + #13#10;
    if status <> '' then
      Result := Result + Format('Status: %s', [status]) + #13#10;
    Result := Result + Format('Rating: %d', [tvrating]) + #13#10;
    Result := Result + Format('Current Season: %s', [BoolToStr(currentseason, True)]) + #13#10;
    Result := Result + Format('Current Episode: %s', [BoolToStr(currentepisode, True)]) + #13#10;
    Result := Result + Format('Current on Air: %s', [BoolToStr(currentair, True)]) + #13#10;
    Result := Result + Format('Daily: %s', [BoolToStr(daily, True)]) + #13#10;
  except
    on e: Exception do
    begin
      Debug(dpError, rsections, 'TTVRelease.AsText : %s', [e.Message]);
    end;
  end;
end;

constructor TTVRelease.Create(const rlsname, section: String; FakeChecking: boolean = True; SavedPretime: int64 = -1);
var
  c_episode: int64;
  i, j: integer;
begin
  inherited Create(rlsname, section, False, savedpretime);

  FLookupDone := False;
  showname := '';
  episode := -1;
  season := -1;
  c_episode := -1;
  genres := TStringList.Create;
  genres.QuoteChar := '"';

  getShowValues(rlsname, showname, season, c_episode);
  episode := c_episode;

  showname := ReplaceText(showname, '.', ' ');
  showname := ReplaceText(showname, '_', ' ');

  for i := 1 to words.Count - 1 do
  begin
    j := GlTvTags.IndexOf(words[i]);
    if j <> -1 then
    begin
      tvtag := GlTvTags[j];
      Break;
    end;
  end;
end;

class function TTVRelease.DefaultSections: String;
begin
  Result := 'TV TVDVDRIP TVDVDR TV720 TV1080';
end;

class function TTVRelease.Name: String;
begin
  Result := 'TTVRelease';
end;

destructor TTVRelease.Destroy;
begin
  genres.Free;
  inherited;
end;

{ T0DayRelease }

function T0DayRelease.AsText(pazo_id: integer): String;
begin
  Result := inherited AsText(pazo_id);
  try
    Result := Result + Format('0daysource: %s', [nulldaysource]) + #13#10;
  except
    on e: Exception do
    begin
      Debug(dpError, rsections, 'T0DayRelease.AsText : %s', [e.Message]);
    end;
  end;
end;

constructor T0DayRelease.Create(const rlsname, section: String;
  FakeChecking: boolean = True; SavedPretime: int64 = -1);
var
  i, j: integer;
begin
  inherited Create(rlsname, section, False, savedpretime);

  for i := words.Count - 1 downto 1 do
  begin
    for j := 0 to GlNullDayPlatformTags.Count - 1 do
    begin
      if (AnsiContainsText(GlNullDayPlatformTags.ValueFromIndex[j], ' ' + words[i] +
        ' ')) then
      begin
        nulldaysource := GlNullDayPlatformTags.Names[j];
        Break;
      end;
    end;
    if nulldaysource <> '' then
      Break;
  end;

  if nulldaysource = '' then
    nulldaysource := 'WIN';

  if FakeChecking then
    FakeCheck(self);
end;

class function T0DayRelease.DefaultSections: String;
begin
  Result := '0DAY,PDA';
end;

class function T0DayRelease.Name: String;
begin
  Result := 'T0dayRelease';
end;

{ TIMDBRelease }

function TIMDBRelease.Aktualizal(p: TObject): boolean;
var
  pazo: TPazo;
  ps: TPazoSite;
  i, j: integer;
  imdbdata: TDbImdbData;
  ir: TIMDBRelease;
begin
  Result := False;
  aktualizalva := True;

  try
    // ugly shit
    pazo := TPazo(p);

    dbaddimdb_cs.Enter;
    try
      i := last_imdbdata.IndexOf(rlsname);
    finally
      dbaddimdb_cs.Leave;
    end;

    if i = -1 then
    begin
      // no imdb infos

      // check if we have a nfo
      i := last_addnfo.IndexOf(rlsname);
      if i <> -1 then
      begin
        // we have the nfo
        Result := True;
        exit;
      end;

      // no nfo, start searching nfo
      for j := pazo.PazoSitesList.Count - 1 downto 0 do
      begin
        ps := TPazoSite(pazo.PazoSitesList[j]);
        try
          AddTask(TPazoSiteNfoTask.Create('', '', ps.Name, pazo, 1));
        except
          on e: Exception do
          begin
            Debug(dpError, rsections, Format('[EXCEPTION] TIMDBRelease.Aktualizal.AddTask: %s', [e.Message]));
          end;
        end;
      end;

      Result := True;
    end
    else
    begin
      // we already have imdb infos
      try
        dbaddimdb_cs.Enter;
        try
          imdbdata := TDbImdbData(last_imdbdata.Objects[i]);
        finally
          dbaddimdb_cs.Leave;
        end;

        if pazo.rls is TIMDBRelease then
        begin
          ir := TIMDBRelease(pazo.rls);
          ir.imdb_id := imdbdata.imdb_id;
          ir.imdb_year := imdbdata.imdb_year;
          ir.imdb_languages := imdbdata.imdb_languages;
          ir.imdb_countries := imdbdata.imdb_countries;
          ir.imdb_genres := imdbdata.imdb_genres;
          ir.imdb_screens := imdbdata.imdb_screens;
          ir.imdb_rating := imdbdata.imdb_rating;
          ir.imdb_votes := imdbdata.imdb_votes;
          ir.CineYear := imdbdata.imdb_cineyear;
          ir.imdb_ldt := imdbdata.imdb_ldt;
          ir.imdb_wide := imdbdata.imdb_wide;
          ir.imdb_festival := imdbdata.imdb_festival;
          ir.imdb_stvm := imdbdata.imdb_stvm;
          ir.imdb_stvs := imdbdata.imdb_stvs;

          ir.FLookupDone := True;
        end;
      except
        on e: Exception do
        begin
          Debug(dpError, rsections, Format('[EXCEPTION] TIMDBRelease.Aktualizal Set: %s', [e.Message]));
        end;
      end;

      Result := True;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, rsections, Format('[EXCEPTION] TIMDBRelease.Aktualizal Set: %s', [e.Message]));
    end;
  end;
end;

function TIMDBRelease.Aktualizald(const extrainfo: String): boolean;
begin
  Result := False;
end;

function TIMDBRelease.AsText(pazo_id: integer): String;
begin
  Result := inherited AsText(pazo_id);
  try
    Result := Result + Format('IMDB id: %s', [imdb_id]) + #13#10;
    Result := Result + Format('IMDB URL: https://imdb.com/title/%s', [imdb_id]) + #13#10;
    Result := Result + Format('IMDB year: %d', [imdb_year]) + #13#10;
    Result := Result + Format('IMDB Cineyear: %d', [cineyear]) + #13#10;
    Result := Result + Format('IMDB languages: %s', [imdb_languages.DelimitedText]) + #13#10;
    Result := Result + Format('IMDB countries: %s', [imdb_countries.DelimitedText]) + #13#10;
    Result := Result + Format('IMDB genres: %s', [imdb_genres.DelimitedText]) + #13#10;
    Result := Result + Format('IMDB screens: %d', [imdb_screens]) + #13#10;
    Result := Result + Format('IMDB rating: %d', [imdb_rating]) + #13#10;
    Result := Result + Format('IMDB votes: %d', [imdb_votes]) + #13#10;
    Result := Result + Format('IMDB Festival: %s', [BoolToStr(imdb_festival, True)]) + #13#10;
    Result := Result + Format('IMDB Limited: %s', [BoolToStr(imdb_ldt, True)]) + #13#10;
    Result := Result + Format('IMDB Natowide: %s', [BoolToStr(imdb_wide, True)]) + #13#10;
    Result := Result + Format('IMDB STV: %s', [BoolToStr(imdb_stvm)]) + #13#10;
    Result := Result + Format('IMDB STVS: %s', [imdb_stvs]) + #13#10;
  except
    on e: Exception do
    begin
      Debug(dpError, rsections, 'TIMDBRelease.AsText : %s', [e.Message]);
    end;
  end;
end;

constructor TIMDBRelease.Create(const rlsname, section: String; FakeChecking: boolean = True; SavedPretime: int64 = -1);
begin
  inherited Create(rlsname, section, False, savedpretime);

  FLookupDone := False;
  imdb_id := '';
  imdb_languages := TStringList.Create;
  imdb_countries := TStringList.Create;
  imdb_genres := TStringList.Create;
end;

class function TIMDBRelease.DefaultSections: String;
begin
  Result := 'DIVX DVDR';
end;

destructor TIMDBRelease.Destroy;
begin
  imdb_languages.Free;
  imdb_countries.Free;
  imdb_genres.Free;
  inherited;
end;

class function TIMDBRelease.Name: String;
begin
  Result := 'TIMDBRelease';
end;

{ TMVIDRelease  }

function TMVIDRelease.Aktualizal(p: TObject): boolean;
var
  pazo: TPazo;
  shot: TPazoSite;
begin
  Result := False;
  aktualizalva := True;
  if nomvdirlistgenre then
    exit;

  pazo := TPazo(p); // ugly shit

  shot := FindMostCompleteSite(pazo);
  if shot <> nil then
  begin
    AddTask(TPazoMVIDTask.Create('', '', shot.Name, pazo, 1));
    Result := True;
  end;

  // aktualizalva := True;
end;

function TMVIDRelease.AsText(pazo_id: integer): String;
begin
  Result := inherited AsText(pazo_id);
  try
    Result := Result + Format('MVID Genre: %s', [mvid_Genre.CommaText]) + #13#10;
    Result := Result + Format('MVID Year: %d', [mvid_year]) + #13#10;
    Result := Result + Format('MVID Files: %d', [FileCount]) + #13#10;
    Result := Result + Format('MVID Source: %s', [mvid_source]) + #13#10;
    Result := Result + Format('MVID Region PAL: %s', [BoolToStr(mvid_pal, True)]) + #13#10;
    Result := Result + Format('MVID Region NTSC: %s', [BoolToStr(mvid_ntsc, True)]) + #13#10;
    Result := Result + Format('VA: %s', [BoolToStr(mvid_va, True)]) + #13#10;
    Result := Result + Format('Live: %s', [BoolToStr(mvid_live, True)]) + #13#10;
  except
    on e: Exception do
    begin
      Debug(dpError, rsections, 'TMVIDRelease.AsText : %s', [e.Message]);
    end;
  end;
end;

function TMVIDRelease.Aktualizald(const extrainfo: String): boolean;
begin
  Result := False;
end;

constructor TMVIDRelease.Create(const rlsname, section: String; FakeChecking: boolean = True; SavedPretime: int64 = -1);
var
  mvrx: TRegexpr;
begin
  inherited Create(rlsname, section, True, savedpretime);
  aktualizalva := False;
  FileCount := 0;
  mvid_Genre := TStringList.Create;
  mvid_source := '';
  mvid_pal := False;
  mvid_ntsc := False;
  mvid_va := False;
  mvid_live := False;
  mvid_year := -1;

  mvrx := TRegexpr.Create;
  try
    mvrx.ModifierI := True;

    mvrx.Expression := '\-((19|20)\d{2})\-';
    if mvrx.Exec(rlsname) then
      mvid_year := StrToIntDef(mvrx.Match[1], 0);

    mvrx.Expression := '^VA[\-\_\.]';
    mvid_va := mvrx.Exec(rlsname);

    mvrx.Expression := '[\-\_\(\)](Festival|Live)[\-\_\(\)]';
    mvid_live := mvrx.Exec(rlsname);
  finally
    mvrx.Free;
  end;
end;

class function TMVIDRelease.DefaultSections: String;
begin
  Result := 'MVID';
end;

destructor TMVIDRelease.Destroy;
begin
  mvid_Genre.Free;
  inherited;
end;

class function TMVIDRelease.Name: String;
begin
  Result := 'TMVIDRelease';
end;

end.

