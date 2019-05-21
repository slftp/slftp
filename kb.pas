{*****************************************************************************

 - Soulless robotic engine aka SLFTP
 - Version 1.3

 - Remarks:          Freeware, Copyright must be included

 - Original Author:  believe

 - Modifications:    aKRAUT

 - Last change:      27/06/2010 - Adding CineYear to TIMDBRelease

 - Description:

 ****************************************************************************

 * THIS SOFTWARE IS PROVIDED BY THE AUTHORS ''AS IS'' AND ANY EXPRESS       *
 * OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED        *
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE       *
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE        *
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR      *
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF     *
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR          *
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,    *
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE     *
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,        *
 * EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                       *

*****************************************************************************}

{ @abstract(Knowledge base for release information)
  The different types hold different infos which are needed for all the different
  categories of releases }

unit kb;

interface

uses Classes, SyncObjs, encinifile, IniFiles, knowngroups;

type
  TRelease = class
    aktualizalva: boolean;
    aktualizalasfailed: boolean;
    rlsname: String;
    rlsnamewithoutgrp: String;
    section: String;
    words: TStringList;
    tags: TStringList;
    groupname: String;
    internal: boolean;
    disks: integer;
    kb_event: String;
    languages: TStringList;

    legnagyobbcd: integer;
    sample: boolean;
    covers: boolean;
    subs: boolean;

    fake: boolean;
    fakereason: String;

    event: String;

    pretime: TDateTime;
    cpretime: int64;
    PredOnAnySite: boolean; // { indicates if it's pred on any of your sites }

    pretimefound: boolean;
    pretimefrom: String;

    //fakecheckinghez
    dots: integer;
    number_of_chars: integer;
    vowels: integer;

    year: integer;

    knowngroup: TKnownGroup;

    constructor Create(const rlsname, section: String; FakeChecking: boolean = True; SavedPretime: int64 = -1); virtual;
    destructor Destroy; override;

    function ExtraInfo: String; virtual;

    function Aktualizald(const extrainfo: String): boolean; virtual;

    function AsText(pazo_id: integer = -1): String; virtual;

    function Aktualizal(p: TObject): boolean; virtual;

    procedure SetPretime(TimeStamp: int64 = 0);
    class function Name: String; virtual;// abstract;
    class function DefaultSections: String; virtual; abstract;
    class function SectionAccepted(const section: String): boolean;
  end;

  T0DayRelease = class(TRelease)
  public
    nulldaysource: String;
    constructor Create(const rlsname, section: String; FakeChecking: boolean = True; SavedPretime: int64 = -1); override;
    class function Name: String; override;
    class function DefaultSections: String; override;
    function AsText(pazo_id: integer = -1): String; override;
  end;

  TMP3Release = class(TRelease)
    mp3year: integer;
    mp3lng: String;
    mp3genre: String;
    mp3source: String;
    mp3types1: String;
    mp3types2: String;
    mp3types3: String;

    //    mp3flac:boolean;
    mp3_numdisks: integer;
    mp3_number_of: String;

    mp3_va: boolean;

    function Bootleg: boolean;
    constructor Create(const rlsname, section: String; FakeChecking: boolean = True; SavedPretime: int64 = -1); override;
    //    destructor Destroy; override;
    function ExtraInfo: String; override;

    function Aktualizald(const extrainfo: String): boolean; override;
    function AsText(pazo_id: integer = -1): String; override;
    function Numdisks: integer;
    function Aktualizal(p: TObject): boolean; override;
    function mp3type(const s: String): boolean;
    class function Name: String; override;
    class function DefaultSections: String; override;
  private
    function Evszam(s: String): boolean;
    procedure AddSource(const src: String);
    procedure NumberOfDisksTag(const tag: String; var Source: String; var disks: integer);
  end;

  TNFORelease = class(TRelease)
    nfogenre: String;
    function ExtraInfo: String; override;
    constructor Create(const rlsname, section: String; FakeChecking: boolean = True; SavedPretime: int64 = -1); override;
    //    destructor Destroy; override;
    function Aktualizald(const extrainfo: String): boolean; override;
    function AsText(pazo_id: integer = -1): String; override;
    function Aktualizal(p: TObject): boolean; override;
    class function Name: String; override;
    class function DefaultSections: String; override;
  end;

  TIMDBRelease = class(TRelease)
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
    imdb_stvm: boolean;
    imdb_stvs: String;

    function ExtraInfo: String; override;
    destructor Destroy; override;
    constructor Create(const rlsname, section: String; FakeChecking: boolean = True; SavedPretime: int64 = -1); override;
    function Aktualizald(const extrainfo: String): boolean; override;
    function AsText(pazo_id: integer = -1): String; override;
    function Aktualizal(p: TObject): boolean; override;
    class function Name: String; override;
    class function DefaultSections: String; override;
  end;

  TTVRelease = class(TRelease)
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
    tvlanguage:String;
    //    currentAir:boolean;
    function ExtraInfo: String; override;
    constructor Create(const rlsname, section: String; FakeChecking: boolean = True; SavedPretime: int64 = -1); override;
    destructor Destroy; override;
    function Aktualizald(const extrainfo: String): boolean; override;
    function AsText(pazo_id: integer = -1): String; override;
    function Aktualizal(p: TObject): boolean; override;
    class function Name: String; override;
    class function DefaultSections: String; override;
  end;

  TMVIDRelease = class(TRelease)
    FileCount: integer;
    //   mvid_Genre:string;
    mvid_Genre: TStringList;
    //   mvid_languages:string;
    mvid_languages: TStringList;
    mvid_source: String;
    mvid_pal: boolean;
    mvid_ntsc: boolean;
    mvid_va: boolean;
    mvid_live: boolean;
    mvid_year: integer;
    function ExtraInfo: String; override;
    destructor Destroy; override;
    constructor Create(const rlsname, section: String; FakeChecking: boolean = True; SavedPretime: int64 = -1); override;
    //    constructor Create(const rlsname, section: String; FakeChecking: Boolean = True); override;
    //    constructor CustomCreate(const rlsname, section: String; FakeChecking: Boolean = True;Pretime:int64 = -1); override;
    function Aktualizald(const extrainfo: String): boolean; override;
    function AsText(pazo_id: integer = -1): String; override;
    function Aktualizal(p: TObject): boolean; override;
    class function Name: String; override;
    class function DefaultSections: String; override;
  end;

  TCRelease = class of TRelease;

  TKBThread = class(TThread)
  private
    kbevent: TEvent;
    function AddCompleteTransfers(pazo: Pointer): boolean;
  public
    constructor Create;
    procedure Execute; override;
    destructor Destroy; override;
  end;

function renameCheck(const pattern, i, len: integer; const rls: String): boolean;
function kb_Add(const netname, channel, sitename, section, genre, event, rls, cdno: String;
  dontFire: boolean = False; forceFire: boolean = False; ts: TDateTime = 0): integer;
function FindReleaseInKbList(const rls: String): String;

function FindSectionHandler(const section: String): TCRelease;

procedure kb_FreeList;
procedure kb_Save;
procedure KB_start;
procedure kb_Init;
procedure kb_Uninit;
procedure kb_Stop;

function kb_reloadsections: boolean;

{ Extracts groupname from release
  @param(rlz releasename)
  @returns(Groupname from input @link(rlz)) }
function GetGroupname(const rlz: String): String;

var
  kb_sections: TStringList;
  nulldaysources: TStringList;
  mp3genres: TStringList;
  mp3languages: TStringList;
  mp3sources: TStringList;
  tvtags: TStringList; //< holds the list of tvtags from config file
  mp3types: TStringList;
  kb_list: TStringList;
  kb_thread: TKBThread;
  kb_last_saved: TDateTime;
  kb_sectionhandlers: TStringList;
  kb_languages: TStringList;

  kb_lock: TCriticalSection;

  noannouncesections: TStringList;

  imdbcountries: TIniFile;
  kbevent: TEvent;

implementation

uses
  debugunit, mainthread, taskgenrenfo, taskgenredirlist, configunit, console,
  taskrace, sitesunit, queueunit, pazo, irc, SysUtils, fake, mystrings,
  rulesunit, Math, DateUtils, StrUtils, precatcher, tasktvinfolookup,
  slvision, tasksitenfo, RegExpr, taskpretime, taskgame,
  sllanguagebase, taskmvidunit, dbaddpre, dbaddimdb, dbtvinfo, irccolorunit,
  mrdohutils, ranksunit, statsunit, tasklogin, dbaddnfo, contnrs, slmasks,
  globalskipunit {$IFDEF MSWINDOWS}, Windows{$ENDIF};

type
  TSectionRelease = record
    section: String;
    r: TCRelease;
  end;
  TSectionHandlers = array[0..6] of TCRelease;

const
  rsections = 'kb';

var
  sectionhandlers: TSectionHandlers = (TRelease, TMP3Release, T0dayRelease, TNFORelease, TIMDBRelease, TTVRelease, TMVIDRelease);

  addpreechocmd: String;

  // TODO: Using THashedStringList does fuckup cleaning because it does not have a constant index which is used to delete oldest (latest) entries
  // but it's much faster and as we use it very often it's worth it...but maybe there is a better solution
  kb_trimmed_rls: THashedStringList;
  kb_groupcheck_rls: THashedStringList;
  kb_latest: THashedStringList; //< holds release and section as rls=section
  kb_skip: THashedStringList;

  // Config vars
  trimmed_shit_checker: boolean;
  renamed_group_checker: boolean;
  renamed_release_checker: boolean;

  use_new_language_base: boolean;
  enable_try_to_complete: boolean;
  try_to_complete_after: integer;
  kb_save_entries: integer;

  rename_patterns: integer;
  taskpretime_mode: integer;

  nomp3dirlistgenre: boolean;
  nonfodirlistgenre: boolean;
  nomvdirlistgenre: boolean;

function FindSectionHandler(const section: String): TCRelease;
var
  i: integer;
begin
  Result := sectionhandlers[0];

  for i := 1 to High(sectionhandlers) do
  begin
    if sectionhandlers[i].SectionAccepted(section) then
    begin
      Result := sectionhandlers[i];
      exit;
    end;
  end;
end;

{ Removes groupname from release
  @param(rlz releasename)
  @returns(Releasename @link(rlz) without groupname) }
function RemoveGroupname(const rlz: String): String;
var
  fGroup: String;
begin
  fGroup := GetGroupname(rlz);
  Result := ReplaceText(rlz, fGroup, '');
end;

function GetGroupname(const rlz: String): String;
var
  x: TStringList;
  s: String;
begin
  s := ReplaceText(rlz, '(', '');
  s := ReplaceText(s, ')', '');
  s := ReplaceText(s, '.', ' ');
  s := ReplaceText(s, '-', ' ');
  s := ReplaceText(s, '_', ' ');

  x := TStringList.Create;
  try
    x.Delimiter := ' ';
    x.DelimitedText := s;
    if uppercase(x.Strings[x.Count - 1]) = 'INT' then
      Result := '-' + x.Strings[x.Count - 2] + '_' + x.Strings[x.Count - 1]
    else
      Result := x.Strings[x.Count - 1];
  finally
    x.Free;
  end;
end;

function renameCheck(const pattern, i, len: integer; const rls: String): boolean;
var
  ss: String;
begin
  Result := False;

  // increase rename_patterns in kb_init by 1 everytime a new pattern emerges

  ss := kb_latest.Names[i];
  if pattern = 0 then
  begin
    // Original: Point_Blank-X_History-2012-C4
    // Rename:   Pnt_t_Blank-X_History-2012-C4
    Delete(ss, 2, 2);
    Insert(Copy(ss, 3, 2), ss, 5);
  end
  else if pattern = 1 then
  begin
    // Original: VA-Soundwave_2013-2CD-2012-MTD
    // Rename:   V-Soundwave_20013-2CD-2012-MTD
    Delete(ss, 2, 1);
    Insert(Copy(ss, 14, 1), ss, 14);
  end
  else if pattern = 2 then
  begin
    // Original: VA-Soundwave_2013-2CD-2012-MTD
    // Rename:   VA-Soudwave_20013-2CD-2012-MTD
    Delete(ss, 7, 1);
    Insert(Copy(ss, 14, 1), ss, 14);
  end
  else if pattern = 3 then
  begin
    // Original: Teleport.Pro.v1.68.Incl.Keygen-BRD
    // Rename:   Teleport.Pro.v1.68.Incl.Keynen-BRD
    Delete(ss, len - 6, 1);
    Insert(Copy(ss, len - 5, 1), ss, len - 6);
  end
  else
    ss := '';

  if AnsiCompareText(ss, rls) = 0 then
    Result := True;
end;

function trimmedShitChecker(section, rls: String): boolean;
begin
  Result := False;
end;

function kb_AddB(const netname, channel, sitename, section, genre, event, rls, cdno: String; dontFire: boolean = False; forceFire: boolean = False; ts: TDateTime = 0): integer;
var
  i, j, len: integer;
  r: TRelease;
  rc: TCRelease;
  s: TSite;
  ss: String;
  added: boolean;
  p: TPazo;
  ps, psource: TPazoSite;
  rule_result: TRuleAction;
  rlz, grp: String;
  dlt: TPazoDirlistTask;
  l: TLoginTask;

  { Removes the oldest knowledge base entries }
  procedure KbListsCleanUp;
  begin
    try
      i := kb_trimmed_rls.Count - 1;
      if i > 200 then
      begin
        while i > 150 do
        begin
          kb_trimmed_rls.Delete(0);
          i := kb_trimmed_rls.Count - 1;
        end;
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, rsections, '[EXCEPTION] kb_AddB clean kb_trimmed_rls : %s', [e.Message]);
      end;
    end;

    try
      i := kb_groupcheck_rls.Count - 1;
      if i > 200 then
      begin
        while i > 150 do
        begin
          kb_groupcheck_rls.Delete(0);
          i := kb_groupcheck_rls.Count - 1;
        end;
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, rsections, '[EXCEPTION] kb_AddB clean kb_groupcheck_rls : %s', [e.Message]);
      end;
    end;

    try
      i := kb_latest.Count - 1;
      if i > 200 then
      begin
        while i > 150 do
        begin
          kb_latest.Delete(i);
          i := kb_latest.Count - 1;
        end;
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, rsections, '[EXCEPTION] kb_AddB clean kb_latest : %s', [e.Message]);
      end;
    end;

    try
      i := kb_skip.Count - 1;
      if i > 300 then
      begin
        while i > 250 do
        begin
          kb_skip.Delete(i);
          i := kb_skip.Count - 1;
        end;
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, rsections, '[EXCEPTION] kb_AddB clean kb_skip : %s', [e.Message]);
      end;
    end;
  end;

begin
  debug(dpSpam, rsections, '--> %s %s %s %s %s %d %d', [sitename, section, event, rls, cdno, integer(dontFire), integer(forceFire)]);

  Result := -1;

  kb_lock.Enter;
  psource := nil;
  try
    // deny adding of a release twice with different section
    if (section <> '') then
    begin
      i := kb_latest.IndexOfName(rls);
      if i <> -1 then
      begin
        ss := kb_latest.ValueFromIndex[i];
        if (ss <> section) then
        begin
          irc_addadmin(Format('<b><c4>%s</c> @ %s </b>was caught as section %s but is already in KB with section %s', [rls, sitename, section, ss]));
          exit;
        end;
      end
    end;

    // check if rls already skiped
    if kb_skip.IndexOf(rls) <> -1 then
    begin
      if spamcfg.readbool(rsections, 'skipped_release', True) then
        irc_addadmin(format('<b><c4>%s</c> @ %s </b>is in skipped releases list!', [rls, sitename]));
      exit;
    end;

    if trimmed_shit_checker then
    begin
      try
        i := kb_trimmed_rls.IndexOf(section + '-' + rls);
        if i <> -1 then
        begin
          irc_addadmin(Format('<b><c4>%s</c> @ %s is trimmed shit!</b>', [rls, sitename]));
          kb_skip.Insert(0, rls);
          exit;
        end;

        kb_trimmed_rls.Add(section + '-' + Copy(rls, 1, Length(rls) - 1));
        kb_trimmed_rls.Add(section + '-' + Copy(rls, 2, Length(rls) - 1));
      except
        on e: Exception do
        begin
          Debug(dpError, rsections, '[EXCEPTION] kb_AddB trimmed_shit_checker : %s', [e.Message]);
        end;
      end;
    end;

    if renamed_group_checker then
    begin
      try
        grp := GetGroupname(rls);
        rlz := RemoveGroupname(rls);
        ss := kb_groupcheck_rls.Values[rlz];
        if ss = '' then
          kb_groupcheck_rls.Values[rlz] := grp
        else
        begin
          if uppercase(grp) <> uppercase(ss) then
          begin
            if spamcfg.readbool(rsections, 'renamed_group', True) then
              irc_addadmin(format('<b><c4>%s</c> @ %s </b>is renamed group shit! %s vs. %s', [rls, sitename, grp, ss]));
            kb_skip.Insert(0, rls);
            exit;
          end;
          if grp <> ss then
          begin
            if spamcfg.readbool(rsections, 'renamed_group', True) then
              irc_addadmin(format('<b><c4>%s</c> @ %s </b>is changed case group shit! %s vs. %s', [rls, sitename, grp, ss]));
            kb_skip.Insert(0, rls);
            exit;
          end;
        end;
      except
        on e: Exception do
        begin
          Debug(dpError, rsections, '[EXCEPTION] kb_AddB renamed_group_checker : %s', [e.Message]);
        end;
      end;
    end;

    // don't even enter the checking code if the release is already in kb_latest, because then we already handled it and it's clean
    // because kb_skip would've prevented kb_addb being called from kb_add
    if (kb_latest.IndexOfName(rls) = -1) then
    begin
      if (renamed_release_checker) then
      begin
        try
          len := Length(rls); // no need to check the release length in every loop
          for i := 0 to kb_latest.Count - 1 do
          begin
            // makes no sense to run this "expensive" operation if both strings aren't equal length
            // since the current pattern shows only strings of equal length being renames of one another
            if Length(kb_latest.Names[i]) <> len then
              Continue;
            if AnsiCompareText(kb_latest.Names[i], rls) <> 0 then
            begin
              // loop through the amount of different patterns, reduces code duplication
              for j := 0 to rename_patterns - 1 do
              begin
                if renameCheck(j, i, len, rls) then
                begin
                  if spamcfg.readbool(rsections, 'renamed_release', True) then
                    irc_addadmin(format('<b><c4>%s</c> @ %s </b>is a rename of %s!', [rls, sitename, kb_latest.Names[i]]));

                  // release is brand-new but a rename of an already existing release
                  kb_latest.Insert(0, rls + '=' + section);
                  // gonna insert this anyway, because there are sometimes renames of renames
                  kb_skip.Insert(0, rls);
                  exit;
                end;
              end;
            end;
          end;
        except
          on e: Exception do
          begin
            Debug(dpError, rsections, '[EXCEPTION] kb_AddB renamed_release_checker : %s', [e.Message]);
          end;
        end;
      end;

      // release is fine and brand-new, add it to kb_latest
      kb_latest.Insert(0, rls + '=' + section);
    end;

    // Start cleanup lists
    KbListsCleanUp; // TODO: maybe run it only every 60mins? not needed to run it every time...

  finally
    kb_lock.Leave;
  end;
  //  i := -1;
  //  added := False;

  kb_lock.Enter;
  try
    i := kb_list.IndexOf(section + '-' + rls);
    if i = -1 then
    begin
      if event = 'NUKE' then
      begin
        // nuking an old rls not in kb
        irc_Addstats(Format('<c4>[NUKE]</c> %s %s @ %s (not in kb)',
          [section, rls, '<b>' + sitename + '</b>']));
        exit;
      end;

      if event = 'COMPLETE' then
      begin
        // complet an old rls not in kb
        irc_Addstats(Format('<c7>[COMPLETE]</c> %s %s @ %s (not in kb)',
          [section, rls, '<b>' + sitename + '</b>']));
        exit;
      end;

      debug(dpSpam, rsections,
        'This NEWDIR [event: %s] task for %s (%s) was the first one to hit kb - checking eljut etc',
        [event, rls, section]);

      // uj joveveny!
      rc := FindSectionHandler(section);
      if event = 'PRE' then
      begin
        // no fakecheck needed, it's a pre from one of our sites
        r := rc.Create(rls, section, False, DateTimeToUnix(Now()));
        irc_SendAddPre(format('%s %s %s', [addpreechocmd, rls, section]));
        if TPretimeLookupMOde(taskpretime_mode) = plmSQLITE then
        begin
          try
            dbaddpre_InsertRlz(rls, section, 'SITE-' + sitename);
          except
            on e: Exception do
            begin
              Debug(dpError, rsections, 'dbaddpre_InsertRlz error : %s', [e.Message]);
            end;
          end;
        end;
      end
      else
      begin
        r := rc.Create(rls, section);
      end;

      r.kb_event := event;

      if genre <> '' then
      begin
        try
          r.Aktualizald(genre);
        except
          on e: Exception do
          begin
            Debug(dpError, rsections, 'r.Aktualizald(genre) : %s', [e.Message]);
          end;
        end;
      end;

      p := PazoAdd(r);

      // meg kell keresni az osszes siteot ahol van ilyen section... //must search for all sites where there is such a section ...
      added := p.AddSites;

      if (ts <> 0) then
        p.autodirlist := True;
      // kulso threadnek kell dirlistelnie vagy hasonlo //dirlistelnie be threaded or similar exterior

      kb_list.BeginUpdate;
      try
        kb_list.AddObject(section + '-' + rls, p);
      finally
        kb_list.EndUpdate;
      end;

      if added then
      begin
        // sorrendezes
        RulesOrder(p);
      end;

      // announce event on admin chan
      if (event = 'ADDPRE') then
      begin
        if spamcfg.ReadBool('kb', 'new_rls', True) then
          irc_Addstats(Format('<c3>[ADDPRE]</c> %s %s', [section, rls]));
      end
      else if (event = 'PRE') then
      begin
        if spamcfg.ReadBool('kb', 'pre_rls', True) then
          irc_Addstats(Format('<c9>[<b>PRE</b>]</c> <b>%s</b> <b>%s</b> @ <b>%s</b>', [section, rls, sitename]));
      end
      else
      begin
        if (DateTimeToUnix(r.pretime) = 0) then
        begin
          if TPretimeLookupMOde(taskpretime_mode) = plmNone then
          begin
            if spamcfg.ReadBool('kb', 'new_rls', True) then
              irc_Addstats(Format('<c7>[<b>NEW</b>]</c> %s %s @ <b>%s</b>', [section, rls, sitename]));
          end
          else
          begin
            if spamcfg.ReadBool('kb', 'new_rls', True) then
              irc_Addstats(Format('<c7>[<b>NEW</b>]</c> %s %s @ <b>%s</b> (<c7><b>Not found in PreDB</b></c>)', [section, rls, sitename]));
          end;
        end
        else
        begin
          if spamcfg.ReadBool('kb', 'new_rls', True) then
            irc_Addstats(Format('<c3>[<b>NEW</b>]</c> %s %s @ <b>%s</b> (<b>%s</b>) (<c3><b>%s ago</b></c>) (%s)', [section, rls, sitename, p.sl.sectionname, dbaddpre_GetPreduration(r.pretime), r.pretimefrom]));
        end;
      end;
    end
    else
    begin
      if (event = 'PRE') then
      begin
        if spamcfg.ReadBool('kb', 'pre_rls', True) then
          irc_Addstats(Format('<c9>[<b>PRE</b>]</c> <b>%s</b> <b>%s</b> @ <b>%s</b>', [section, rls, sitename]));
      end;

      // meg kell tudni mi valtozott //you need to know what's changed
      p := TPazo(kb_list.Objects[i]);
      r := p.rls;

      debug(dpSpam, rsections,
        'This NEWDIR [event: %s] task was not the first one to hit kb as kb_list already contained an entry for %s in %s',
        [event, rls, section]);

      if r.rlsname <> rls then
      begin
        irc_addadmin(Format('<b><c4>%s</c> @ %s changed case!</b>!!', [rls,
          sitename]));
        exit;
      end;

      if genre <> '' then
      begin
        try
          p.rls.Aktualizald(genre);
        except
          on e: Exception do
          begin
            Debug(dpError, rsections, 'p.rls.Aktualizald(genre) : %s',
              [e.Message]);
          end;
        end;
      end;

      if TPretimeLookupMOde(taskpretime_mode) <> plmNone then
      begin
        if (DateTimeToUnix(r.pretime) = 0) then
        begin
          r.SetPretime;
          if (DateTimeToUnix(r.pretime) <> 0) then
          begin
            if spamcfg.ReadBool('kb', 'updated_rls', True) then
              irc_SendUPDATE(Format('<c3>[UPDATE]</c> %s %s @ <b>%s</b> now has pretime (<c3><b>%s ago</b></c>) (%s)', [section, rls, sitename, dbaddpre_GetPreduration(r.pretime), r.pretimefrom]));
            added := p.AddSites;
            if added then
            begin
              // sorrendezes
              RulesOrder(p);
            end;
          end;
        end;
      end;
    end;
  finally
    kb_lock.Leave;
  end;

  Result := p.pazo_id;
  if p.sites.Count = 0 then
    exit;

  if CheckIfGlobalSkippedGroup(rls) then
  begin
    irc_addadmin(format('<b><c4>%s</c> @ %s </b>is a global skipped group!', [grp, rls]));
    debug(dpSpam, rsections, 'Group %s pred %s in %s but it is a global skipped group', [grp, rls, section]);
    exit;
  end;

  if (event <> 'ADDPRE') then
  begin
    psource := p.FindSite(sitename);
    if psource = nil then
    begin
      s := FindSiteByName(netname, sitename);

      // site not found in pazo but we got an event ...
      if spamcfg.ReadBool('kb', 'dont_match_rls', True) then
      begin
        if event = 'NUKE' then
          exit;

        if (s = nil) then
        begin
          irc_Addstats(Format('<c4>[SITE NOT FOUND]</c> : %s %s', [netname, sitename]));
          exit;
        end;

        if (s.markeddown) then
        begin
          irc_Addstats(Format('<c4>[SITE DOWN]</c> : %s %s @ <b>%s</b>', [section, rls, sitename]));
          exit;
        end;

        if (TPretimeLookupMode(taskpretime_mode) <> plmNone) then
        begin
          if (DateTimeToUnix(r.pretime) = 0) then
          begin
            irc_Addstats(Format('<c7>[NO PRETIME]</c> :  %s %s @ <b>%s</b>', [section, rls, sitename]));
            exit;
          end;

          if (not s.IsPretimeOk(p.rls.section, p.rls.pretime)) then
          begin
            irc_Addstats(Format('<c5>[BACKFILL]</c> : %s %s @ <b>%s</b>', [section, rls, sitename]));
            exit;
          end;
        end;

        if ((sitename <> getAdminSiteName) and (not s.PermDown) and (not s.markeddown) and (s.working <> sstDown)) then
        begin
          irc_Addstats(Format('<c5>[SECTION NOT SET]</c> : %s %s @ %s (%s)', [p.rls.section, p.rls.rlsname, sitename, event]));
        end;
      end;

      if ((s <> nil) and (not s.markeddown) and (not s.PermDown) and (s.working = sstDown) and ((event = 'COMPLETE') or (event = 'PRE'))) then
      begin
        try
          l := TLoginTask.Create(netname, channel, sitename, False, False);
          l.noannounce := True;
          AddTask(l);
        except
          on E: Exception do
            Debug(dpError, rsections, '[EXCEPTION] COMPLETE|PRE loginTask : %s', [e.Message]);
        end;
      end;

      exit;
    end;

    s := FindSiteByName(netname, psource.Name);
    if ((s <> nil) and (s.markeddown)) then
      exit;

    psource.ircevent := True;

    if psource.ts < ts then
    begin
      psource.ts := ts;
    end;

    if event = 'PRE' then
    begin
      if 1 <> Pos('PRE', section) then
      begin
        if (s <> nil) then
        begin
          if ((not s.IsAffil(r.groupname)) and (config.ReadBool(rsections, 'auto_add_affils', True))) then
            s.AddAffil(r.groupname);
        end;
      end;
      r.PredOnAnySite := True;
      psource.Status := rssRealPre;
    end
    else if ((event = 'COMPLETE') and (not psource.StatusRealPreOrShouldPre)) then
    begin
      psource.dirlist.SetCompleteInfoFromIrc;
      psource.SetComplete(cdno);
    end;

    if event = 'NUKE' then
    begin
      psource.Status := rssNuked;
      irc_Addstats(Format('<c4>[NUKE]</c> %s %s @ <b>%s</b>',
        [section, rls, sitename]));
      try
        RemovePazoMKDIR(p.pazo_id, psource.Name, rls);
        RemoveRaceTasks(p.pazo_id, psource.Name);
        RemoveDirlistTasks(p.pazo_id, psource.Name);
        psource.dirlistgaveup := True;
      except
        on e: Exception do
        begin
          Debug(dpError, rsections,
            Format('[EXCEPTION] KBAdd RemovePazo on NUKE : %s',
            [e.Message]));
        end;
      end;
    end;
  end;

  if not p.rls.aktualizalva then
  begin
    p.rls.Aktualizal(p);
  end;

  // implement firerules, routes, stb. set rs.srcsite:= rss.sitename;
  if ((event <> 'NUKE') and (event <> 'ADDPRE')) then
  begin
    kb_lock.Enter;
    try
      rule_result := raDrop;
      rule_result := FireRuleSet(p, psource);
    finally
      kb_lock.Leave;
    end;

    // announce SKIP and DONT MATCH only if the site is not a PRE site
    if (psource.status <> rssRealPre) then
    begin
      if (rule_result = raDrop) and (spamcfg.ReadBool('kb', 'skip_rls', True)) then
      begin
        irc_Addstats(Format('<c5>[SKIP]</c> : %s %s @ %s "%s" (%s)',
          [p.rls.section, p.rls.rlsname, psource.Name, psource.reason, event]));
      end
      else if (rule_result = raDontmatch) and (spamcfg.ReadBool('kb', 'dont_match_rls', True)) then
      begin
        irc_Addstats(Format('<c5>[DONT MATCH]</c> : %s %s @ %s "%s" (%s)',
          [p.rls.section, p.rls.rlsname, psource.Name, psource.reason, event]));
      end;
    end;
  end;

  try
    // check rules for site only if needed
    for i := p.sites.Count - 1 downto 0 do
    begin
      try
        if i < 0 then
          Break;
      except
        Break;
      end;
      ps := TPazoSite(p.sites[i]);
      kb_lock.Enter;
      try
        if (ps.status in [rssNotAllowed, rssNotAllowedButItsThere]) then
        begin
          if FireRuleSet(p, ps) = raAllow then
          begin
            ps.status := rssAllowed;
          end;
        end;
      finally
        kb_lock.Leave;
      end;
    end;

    // now add all dst
    for i := p.sites.Count - 1 downto 0 do
    begin
      try
        if i < 0 then
          Break;
      except
        Break;
      end;
      ps := TPazoSite(p.sites[i]);
      kb_lock.Enter;
      try
        FireRules(p, ps);
      finally
        kb_lock.Leave;
      end;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, rsections, Format('[EXCEPTION] KBAdd FireRules : %s',
        [e.Message]));
    end;
  end;

  if dontFire then
    exit;

  // status changed
  ss := p.RoutesText;
  if ss <> '' then
  begin
    irc_SendROUTEINFOS(ss);
  end;

  if (psource <> nil) and (psource.Status = rssNotAllowed) then
  begin
    psource.Status := rssNotAllowedButItsThere;
  end;

  // now add dirlist
  try
    if ((event = 'NEWDIR') or (event = 'PRE') or (event = 'ADDPRE') or (event = 'UPDATE')) then
    begin
      for i := p.sites.Count - 1 downto 0 do
      begin
        try
          if i < 0 then
            Break;
        except
          Break;
        end;
        try
          ps := TPazoSite(p.sites[i]);

          // dirlist not available
          if ps.dirlist = nil then
          begin
            Debug(dpError, section, 'ERROR: ps.dirlist = nil');
            Continue;
          end;

          // dirlist task already added
          if (ps.dirlist.dirlistadded) and (event <> 'UPDATE') then
            Continue;

          // Source site is PRE site for this group
          if ps.status in [rssShouldPre, rssRealPre] then
          begin
            r.PredOnAnySite := True;
            dlt := TPazoDirlistTask.Create(netname, channel, ps.Name, p, '', True);
            irc_Addtext_by_key('PRECATCHSTATS', Format('<c7>[KB]</c> %s %s Dirlist added to : %s (PRESITE) from event %s', [section, rls, ps.Name, event]));
            ps.dirlist.dirlistadded := True;
            AddTask(dlt);
          end;

          // Source site is _not_ a PRE site for this group
          if ps.status in [rssNotAllowedButItsThere, rssAllowed, rssComplete] then
          begin
            dlt := TPazoDirlistTask.Create(netname, channel, ps.Name, p, '', False);
            irc_Addtext_by_key('PRECATCHSTATS', Format('<c7>[KB]</c> %s %s Dirlist added to : %s (NOT PRESITE) from event %s', [section, rls, ps.Name, event]));
            ps.dirlist.dirlistadded := True;
            AddTask(dlt);
          end;

        except
          Continue;
        end;
      end;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] kb_Add add dirlist: %s', [e.Message]));
      exit;
    end;
  end;

  debug(dpSpam, rsections, '<-- %s %s %s %s %s %s %d %d',
    [sitename, section, genre, event, rls, cdno, integer(dontFire),
    integer(forceFire)]);
end;

function kb_Add(const netname, channel, sitename, section, genre, event, rls, cdno: String; dontFire: boolean = False; forceFire: boolean = False; ts: TDateTime = 0): integer;
begin
  Result := 0;
  if (Trim(sitename) = '') then
    exit;
  if (Trim(section) = '') then
    exit;
  if (Trim(rls) = '') then
    exit;
  if (Trim(event) = '') then
    exit;
  if section = 'TRASH' then
    exit;

  if kb_skip.IndexOf(rls) <> -1 then
  begin
    if spamcfg.readbool(rsections, 'skipped_release', True) then
      irc_addadmin(format('<b><c4>%s</c> @ %s </b>is in skipped releases list!',
        [rls, sitename]));
    exit;
  end;

  try
    Debug(dpMessage, 'kb', '--> ' + Format('%s: %s %s @ %s (%s%s)',
      [event, section, rls, sitename, genre, cdno]));
    Result := kb_AddB(netname, channel, sitename, section, genre,
      event, rls, cdno, dontFire, forceFire, ts);
    Debug(dpMessage, 'kb', '<-- ' + Format('%s: %s %s @ %s (%s%s)',
      [event, section, rls, sitename, genre, cdno]));
  except
    on E: Exception do
    begin
      Debug(dpError, 'kb', Format('[EXCEPTION] kb_Add: %s', [e.Message]));
      Result := 0;
      exit;
    end;
  end;

  QueueFire;
end;

function FindReleaseInKbList(const rls: String): String;
var
  i: integer;
begin
  Result := '';
  for i := 0 to kb_list.Count - 1 do
  begin
    if AnsiContainsText(kb_list[i], rls) then
    begin
      Result := kb_list[i];
      break;
    end;
  end;
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
    Result := '<b>' + rlsname + '</b>';
    if pazo_id <> -1 then
      Result := Result + ' (' + IntToStr(pazo_id) + ')';
    Result := Result + #13#10;

    Result := Result + 'Knowngroup: ';
    if knowngroup = grp_known then
      Result := Result + '1'
    else if knowngroup = grp_unknown then
      Result := Result + '0';
    if knowngroup = grp_notconfigured then
      Result := Result + '?';
    Result := Result + #13#10;
    if (DateTimeToUnix(pretime) = 0) then
      Result := Result + 'Pretime not found!' + #13#10
    else
      Result := Result + 'Pretime: ' + dbaddpre_GetPreduration(pretime) +
        ' (' + FormatDateTime('yyyy-mm-dd hh:nn:ss', pretime) + ')' + #13#10;

    if disks <> 1 then
      Result := Result + 'Disks: ' + IntToStr(disks) + #13#10;

    if fake then
      Result := Result + 'Fake: ' + fakereason + #13#10;

    if languages.Count <> 0 then
      Result := Result + 'Language(s): ' + languages.DelimitedText + #13#10;

    Result := Result + 'Internal: ' + IntToStr(integer(internal)) + #13#10;
  except
    on e: Exception do
    begin
      Debug(dpError, rsections, 'TRelease.AsText : %s', [e.Message]);
    end;
  end;
end;

constructor TRelease.Create(const rlsname, section: String; FakeChecking: boolean = True; SavedPretime: int64 = -1);
var
  vlang, s: String;
  i, j: integer;
  rrgx: TRegExpr;
  ii: integer;
begin
  try
    aktualizalva := False;
    PredOnAnySite := False;

    Self.section := section;
    Self.rlsname := rlsname;

    if SavedPretime > -1 then
    begin
      try
        self.pretime := UnixToDateTime(Savedpretime);
        self.cpretime := SavedPretime;
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

    languages := TStringList.Create;
    languages.CaseSensitive := False;

    tags := TStringList.Create;
    tags.Delimiter := ' ';
    tags.CaseSensitive := False;

    words := TStringList.Create;
    words.Delimiter := ' ';
    words.CaseSensitive := False;

    s := ReplaceText(rlsname, '(', '');
    s := ReplaceText(s, ')', '');
    s := ReplaceText(s, '.', ' ');
    s := ReplaceText(s, '-', ' ');
    s := ReplaceText(s, '_', ' ');

    tags.DelimitedText := s;

    words.DelimitedText := s;

    Internal := False;

    rrgx := TRegExpr.Create;
    try
      rrgx.ModifierI := True;

      rrgx.Expression := '[\_\-\.]\(?(internal|int)\)?([\_\-\.]|$)';
      if rrgx.Exec(rlsname) then
        Internal := True;

      //detect groupname
      groupname := '';
      rrgx.Expression := '\-([^\-]+)$';
      if rrgx.Exec(rlsname) then
      begin
        groupname := rrgx.Match[1];
      end;

      //old way if groupname not found by regex
      if (groupname = '') then
      begin
        if uppercase(words.strings[words.Count - 1]) = 'INT' then
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

      if not use_new_language_base then
      begin

        if ((Self is TMP3Release) or (Self is TMVIDRelease)) then
        begin
          for I := 0 to mp3languages.Count - 1 do
          begin
            rrgx.Expression := '[\-](' + mp3languages[i] + ')[\-]';
            if rrgx.Exec(rlsname) then
            begin
              languages.Add(mp3languages.strings[i]);
              Break;
            end;
          end;
        end;

        for i := 0 to kb_languages.Count - 1 do
        begin
          if kb_languages[i] <> '' then
          begin
            for ii := 0 to tags.Count - 1 do
              if uppercase(tags.Strings[ii]) = uppercase(kb_languages.Strings[i]) then
                languages.Add(kb_languages.strings[i]);
          end;
        end;
      end
      else
      begin
        vlang := '';
        if ((Self is TMP3Release) or (Self is TMVIDRelease)) then
        begin
          vlang := FindLanguageOnDirectory(rlsname, True);
        end
        else
        begin
          vlang := FindLanguageOnDirectory(rlsname, False);
        end;
        if vlang <> '' then
          languages.Add(vlang);
      end;

      if (languages.Count = 0) then
      begin
        if ((Self is TMP3Release) or (Self is TMVIDRelease)) then
          languages.Add('EN')
        else
          languages.Add('English');
      end;
    finally
      rrgx.free;
    end;

    knowngroup := IsKnownGroup(section, groupname);

    for i := tags.Count - 1 downto 0 do
    begin
      year := StrToIntDef(tags[i], 0);
      if year > 1900 then
        break;
    end;
    if year < 1900 then
      year := 0;

    disks := 1;
    for i := tags.Count - 1 downto 0 do
    begin
      if AnsiContainsText(tags[i], 'disc') then
      begin
        disks := 0;
        j := 1;
        while (j <= length(tags[i])) do
        begin
          if tags[i][j] in ['0'..'9'] then
            disks := disks * 10 + Ord(tags[i][j]) - 48
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
  tags.Free;
  languages.Free;
  inherited;
end;

procedure TRelease.SetPretime(TimeStamp: int64 = 0);
var
  resu: TPretimeResult;
begin
  Debug(dpSpam, rsections, 'TRelease.SetPretime start');
  if TimeStamp <> 0 then
  begin
    pretime := UnixToDateTime(TimeStamp);
    cpretime := TimeStamp;
    pretimefrom := 'Parameter';
  end
  else
  begin
    resu := getPretime(rlsname);
    pretime := resu.pretime;
    pretimefrom := resu.mode;
    cpretime := datetimetounix(pretime);
  end;
  Debug(dpSpam, rsections, 'TRelease.SetPretime end');
end;

function TRelease.ExtraInfo: String;
begin
  Result := '';
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

procedure TMP3Release.NumberOfDisksTag(const tag: String; var Source: String;
  var disks: integer);
var
  i: integer;
  szam: integer;
begin
  disks := 0;
  Source := '';
  szam := 0;
  try
    i := 1;
    while (i <= length(tag)) do
    begin
      if tag[i] in ['0'..'9'] then
        szam := szam * 10 + Ord(tag[i]) - 48
      else
        Break;
      Inc(i);
    end;
    if szam = 0 then
      exit; // nem nyert
    if ((i <= length(tag) - 2) and (tag[i] = 'x')) then
      Inc(i);

    disks := szam;
    Source := ' ' + Copy(tag, i, 15) + ' ';
  except
    on e: Exception do
    begin
      Debug(dpError, rsections, 'TMP3Release.NumberOfDisksTag : %s',
        [e.Message]);
      disks := 0;
      Source := '';
    end;
  end;
end;

constructor TMP3Release.Create(const rlsname, section: String; FakeChecking: boolean = True; SavedPretime: int64 = -1);
var
  evszamindex, i: integer;
  kotojelekszama: integer;
  types: integer;
  j: integer;
  szo, szamoknelkul: String;
  db: integer;
  lrx: TRegexpr;
begin
  inherited Create(rlsname, section, False, savedpretime);
  aktualizalva := False;

  if tags.Count < 3 then
    exit;

  if words.Count > 3 then
  begin
    try
      mp3year := 0;
      evszamindex := 0;
      for i := 1 to 3 do
        if Evszam(tags[tags.Count - i]) then
        begin
          evszamindex := tags.Count - i;
          Break;
        end;

      if mp3year = 0 then
        mp3year := year;
      if mp3year = 0 then
        exit;
      //We did not find out the year. Sucking, useless to continue.

      if ((not Internal) and (evszamindex + 3 = tags.Count)) then
        groupname := tags[evszamindex + 1] + '_' + tags[evszamindex + 2]; //tweak

      //nyelvkod.
      lrx := TRegexpr.Create;
      try
        lrx.ModifierI := True;
        for I := 0 to mp3languages.Count - 1 do
        begin
          lrx.Expression := '[\-](' + mp3languages[i] + ')[\-]';
          if lrx.Exec(rlsname) then
          begin
            mp3lng := mp3languages[i];
            Break;
          end;
        end;

        if mp3lng = '' then
          mp3lng := 'EN';

        kotojelekszama := 0;
        for i := 1 to length(rlsname) do
        begin
          if rlsname[i] = '-' then
          begin
            Inc(kotojelekszama);
            if (kotojelekszama = 2) then
              Break;
          end;
        end;

        if kotojelekszama < 2 then
          exit;

        types := 0;
        mp3_numdisks := 1;

        //for i:= 0 to words.Count -1 do
        for i := words.Count - 1 downto 1 do //from 0day.
        //for i:= kezdoindex to evszamindex-1 do <-- oSource!
        begin
          //1CD 99DVD

          szo := ' ' + words[i] + ' ';
          db := 0;
          NumberOfDisksTag(words[i], szamoknelkul, db);
          for j := 0 to mp3sources.Count - 1 do
          begin
            if (AnsiContainsText(mp3sources.ValueFromIndex[j], szo)) then
            begin
              AddSource(mp3sources.Names[j]);
              Break;
            end
            else if ((db <> 0) and (AnsiContainsText(mp3sources.ValueFromIndex[j],
              szamoknelkul))) then
            begin
              AddSource(mp3sources.Names[j]);
              mp3_numdisks := db;
              mp3_number_of := words[i];
              Break;
            end;
          end;

          if ((types < 3) and (mp3types.IndexOf(words[i]) <> -1)) then
          begin
            Inc(types);
            case types of
              1: mp3types1 := words[i];
              2: mp3types2 := words[i];
              3: mp3types3 := words[i];
            end;
          end;
        end; // end of tag kereses

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
    Result := Result + 'Year: ' + IntToStr(mp3year) + #13#10;
    Result := Result + 'Language: ' + mp3lng + #13#10;
    if mp3genre <> '' then
      Result := Result + 'Genre: ' + mp3genre + #13#10;
    Result := Result + 'Source: ' + mp3source + #13#10;
    if mp3types1 <> '' then
      Result := Result + 'Type1: ' + mp3types1 + #13#10;
    if mp3types2 <> '' then
      Result := Result + 'Type2: ' + mp3types2 + #13#10;
    if mp3types3 <> '' then
      Result := Result + 'Type3: ' + mp3types3 + #13#10;
    Result := Result + 'Disks: ' + IntToStr(mp3_numdisks) + #13#10;
    Result := Result + 'VA: ' + IntToStr(integer(mp3_va)) + #13#10;
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

function TMP3Release.ExtraInfo: String;
begin
  Result := Mp3genre;
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
    Result := Result + 'nfo genre: ' + nfogenre + #13#10;
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

function TNFORelease.ExtraInfo: String;
begin
  Result := nfogenre;
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
begin
  Result := False;

  aktualizalva := True;
  if showname = '' then
    exit;

  pazo := TPazo(p); // ugly shit

  try
    db_tvinfo := getTVInfoByShowName(self.showname);
  except
    on e: Exception do
    begin
      db_tvinfo := nil;
      Debug(dpError, rsections, Format('Exception in TTVRelease.Aktualizal.getTVInfoByShowName: %s', [e.Message]));
    end;
  end;

  if (db_tvinfo <> nil) then
  begin
    db_tvinfo.ripname := rlsname; // caused the error

    if DaysBetween(UnixToDateTime(db_tvinfo.last_updated), now()) >= config.ReadInteger('tasktvinfo', 'days_between_last_update', 6) then
    begin
      db_tvinfo.ripname := rlsname;
      if not db_tvinfo.Update then
      begin
        Debug(dpError, rsections, Format('[ERROR] updating of %s failed.', [showname]));
        irc_AddError(Format('<c4><b>ERROR</c></b>: updating of %s failed.', [showname]));
      end;
    end
    else
    begin
      try
        db_tvinfo.SetTVDbRelease(self);
      except
        on e: Exception do
        begin
          Debug(dpError, rsections, Format('Exception in SetTVDbRelease: %s',
            [e.Message]));
        end;
      end;
      db_tvinfo.free;
      Result := True;
      exit;
    end;
  end;
  try
    AddTask(TPazoTVInfoLookupTask.Create('', '', getAdminSiteName, pazo, 1));
  except
    on e: Exception do
    begin
      Debug(dpError, rsections, Format('[EXCEPTION] TTVRelease.Aktualizal.AddTask: %s',
        [e.Message]));
    end;
  end;
  Result := True;
end;

function TTVRelease.Aktualizald(const extrainfo: String): boolean;
begin
  aktualizalva := True;
  Result := False;
end;

function TTVRelease.AsText(pazo_id: integer): String;
begin
  Result := inherited AsText(pazo_id);
  try
    Result := Result + 'Show name: ' + showname + #13#10;
    Result := Result + 'http://www.tvmaze.com/shows/' + showid + '/' + lowercase(ReplaceText(showname, ' ', '-')) + #13#10;
    Result := Result + 'Season: ' + IntToStr(season) + #13#10;
    Result := Result + 'Episode: ' + IntToStr(episode) + #13#10;
    if premier_year <> -1 then
      Result := Result + 'Premier: ' + IntToStr(premier_year) + #13#10;
    if ended_year > 0 then
      Result := Result + 'Ended: ' + IntToStr(ended_year) + #13#10;
    if country <> '' then
      Result := Result + 'Country: ' + country + #13#10;
    if classification <> '' then
      Result := Result + 'Classification: ' + classification + #13#10;
    Result := Result + 'Scripted: ' + IntToStr(integer(scripted)) + #13#10;
    if genres.Count > 0 then
      Result := Result + 'Genres: ' + genres.CommaText + #13#10;
    if network <> '' then
      Result := Result + 'Network: ' + network + #13#10;
   if tvlanguage <> '' then Result := Result + 'TV Language: ' + tvlanguage + #13#10;
    Result := Result + 'Running: ' + IntToStr(integer(running)) + #13#10;
    if status <> '' then
      Result := Result + 'Status: ' + status + #13#10;
    Result := Result + 'Current Season: ' + BoolToStr(currentseason, True) + #13#10;
    Result := Result + 'Current Episode: ' + BoolToStr(currentepisode, True) + #13#10;
    Result := Result + 'Current on Air: ' + BoolToStr(currentair, True) + #13#10;
    Result := Result + 'Daily: ' + BoolToStr(daily, True) + #13#10;
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
  j, i: integer;
begin
  inherited Create(rlsname, section, False, savedpretime);
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

  for i:= 1 to tags.Count -1 do
  begin
    j:= tvtags.IndexOf( tags[i] );
    if j <> -1 then
    begin
      tvtag:= tvtags[j];
      Break;
    end;
  end;

end;

class function TTVRelease.DefaultSections: String;
begin
  Result := 'TV TVDVDRIP TVDVDR TV720 TV1080';
end;

function TTVRelease.ExtraInfo: String;
begin
  Result := showname; // todo + egyeb infok, scripted, akarmi
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
    Result := Result + '0daysource: ' + nulldaysource + #13#10;
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
    for j := 0 to nulldaysources.Count - 1 do
    begin
      if (AnsiContainsText(nulldaysources.ValueFromIndex[j], ' ' + words[i] +
        ' ')) then
      begin
        nulldaysource := nulldaysources.Names[j];
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
  try
    aktualizalva := True;

    pazo := TPazo(p); // ugly shit

    dbaddimdb_cs.Enter;
    try
      i := last_imdbdata.IndexOf(rlsname);
    finally
      dbaddimdb_cs.Leave;
    end;

    if i = -1 then
    begin
      // no imdb infos, check if we have a nfo
      i := last_addnfo.IndexOf(rlsname);
      if i <> -1 then
      begin
        // we have the nfo
        Result := True;
        exit;
      end;

      // no nfo start searching nfo
      for j := pazo.sites.Count - 1 downto 0 do
      begin
        try
          if j < 0 then
            Break;
        except
          Break;
        end;
        ps := TPazoSite(pazo.sites[j]);
        try
          AddTask(TPazoSiteNfoTask.Create('', '', ps.Name, pazo, 1));
          Result := True;
        except
          on e: Exception do
          begin
            Debug(dpError, rsections, Format('[EXCEPTION] TIMDBRelease.Aktualizal.AddTask: %s', [e.Message]));
          end;
        end;
      end;
    end
    else
    begin
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
    Result := Result + 'IMDB id: ' + imdb_id + #13#10;
    Result:= Result + 'IMDB URL: <l>http://imdb.com/title/' + imdb_id + '</l>' + #13#10;
    Result := Result + 'IMDB year: ' + IntToStr(imdb_year) + #13#10;
    Result := Result + 'IMDB Cineyear: ' + IntToStr(cineyear) + #13#10;
    Result := Result + 'IMDB languages: ' + imdb_languages.DelimitedText + #13#10;
    Result := Result + 'IMDB countries: ' + imdb_countries.DelimitedText + #13#10;
    Result := Result + 'IMDB genres: ' + imdb_genres.DelimitedText + #13#10;
    Result := Result + 'IMDB screens: ' + IntToStr(imdb_screens) + #13#10;
    Result := Result + 'IMDB rating: ' + IntToStr(imdb_rating) + #13#10;
    Result := Result + 'IMDB votes: ' + IntToStr(imdb_votes) + #13#10;
    Result := Result + 'IMDB Festival: ' + IntToStr(integer(imdb_festival)) + #13#10;
    Result := Result + 'IMDB Limited: ' + IntToStr(integer(imdb_ldt)) + #13#10;
    Result := Result + 'IMDB Natowide: ' + IntToStr(integer(imdb_wide)) + #13#10;
    Result := Result + 'IMDB STV: ' + IntToStr(integer(imdb_stvm)) + #13#10;
    Result := Result + 'IMDB STVS: ' + imdb_stvs + #13#10;
  except
    on e: Exception do
    begin
      Debug(dpError, rsections, 'TIMDBRelease.AsText : %s', [e.Message]);
    end;
  end;
end;

constructor TIMDBRelease.Create(const rlsname, section: String;
  FakeChecking: boolean = True; SavedPretime: int64 = -1);
begin
  inherited Create(rlsname, section, False, savedpretime);
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

function TIMDBRelease.ExtraInfo: String;
begin
  Result := imdb_id;
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
    // Result:= Result + 'Language: '+languages.CommaText+#13#10; since rev 314 we use langeuage from TRelease and mapp it in the rules unit over. so mvidlanguage is still active!
    Result := Result + 'MVID Genre: ' + mvid_Genre.CommaText + #13#10;
    Result := Result + 'MVID Year: ' + IntToStr(mvid_year) + #13#10;
    Result := Result + 'MVID Files: ' + IntToStr(integer(FileCount)) + #13#10;
    Result := Result + 'MVID Source: ' + mvid_source + #13#10;
    Result := Result + 'MVID Region PAL: ' + IntToStr(integer(mvid_pal)) + #13#10;
    Result := Result + 'MVID Region NTSC: ' + IntToStr(integer(mvid_ntsc)) + #13#10;
    Result := Result + 'VA: ' + IntToStr(integer(mvid_va)) + #13#10;
    Result := Result + 'Live: ' + IntToStr(integer(mvid_live)) + #13#10;
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
  //  mvid_languages.Free;
  inherited;
end;

function TMVIDRelease.ExtraInfo: String;
begin
  Result := IntToStr(FileCount);
end;

class function TMVIDRelease.Name: String;
begin
  Result := 'TMVIDRelease';
end;

{!--- KB Utils ---?}

function GetKbPazo(p: TPazo): String;
begin
  Result := p.rls.section + #9 + p.rls.rlsname + #9 + p.rls.ExtraInfo +
    #9 + IntToStr(DateTimeToUnix(p.added)) + #9 +
    IntToStr(DateTimeToUnix(p.rls.pretime)) + #9 + p.rls.kb_event;
end;

procedure AddKbPazo(const line: String);
var
  section, rlsname, extra, event: String;
  added: TDateTime;
  p: TPazo;
  r: TRelease;
  rc: TCRelease;
  ctime: int64;
begin
  section := SubString(line, #9, 1);
  rlsname := SubString(line, #9, 2);
  extra := SubString(line, #9, 3);
  added := UnixToDateTime(StrToInt64(SubString(line, #9, 4)));
  ctime := Strtoint64(SubString(line, #9, 5));
  event := SubString(line, #9, 6);
  kb_trimmed_rls.Add(section + '-' + Copy(rlsname, 1, Length(rlsname) - 1));
  kb_trimmed_rls.Add(section + '-' + Copy(rlsname, 2, Length(rlsname) - 1));

  rc := FindSectionHandler(section);

  if ctime > 0 then
    r := rc.Create(rlsname, section, True, ctime)
  else
    r := rc.Create(rlsname, section);

  //r.pretime:=UnixToDateTime(ctime);
  r.kb_event := event;

  if extra <> '' then
  begin
    r.Aktualizald(extra);
    r.aktualizalva := True;
  end;

  p := PazoAdd(r);

  p.added := added;
  p.stated := True;
  p.cleared := True;
  p.completezve := True;
  kb_list.AddObject(section + '-' + rlsname, p);
end;

procedure KB_start;
var
  x: TEncStringlist;
  i: integer;
  last: TDateTime;
begin
  kb_reloadsections;

  // itt kell betoltenunk az slftp.kb -t
  kb_lock.Enter;
  try
    x := TEncStringlist.Create(passphrase);
    try
      //    Console_Addline('', 'Loading KB entries...');
      x.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'slftp.kb');
      last := Now;
      for i := 0 to x.Count - 1 do
      begin
        //Console_QueueStat(x.Count - i - 1);
        AddKbPazo(x[i]);
        if MilliSecondsBetween(Now, last) > 500 then
        begin
          last := Now;
          slapp.ProcessMessages;
        end;
      end;
      Console_Addline('', 'Ok.');
    finally
      x.Free;
    end;
  finally
    kb_lock.Leave;
  end;

  x := TEncStringlist.Create(passphrase);
  try
    //    Console_Addline('', 'Loading KB renames...');
    x.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'slftp.renames');
    for i := 0 to x.Count - 1 do
    begin
      kb_skip.Insert(0, x[i]);
    end;
    //    Console_Addline('', Format('Ok loaded %d KB renames.', [kb_skip.Count]));
  finally
    x.Free;
  end;

  kb_thread := TKBThread.Create;
end;

procedure kb_Save;
var
  i: integer;
  seconds: integer;
  x: TEncStringList;
  p: TPazo;
begin
  kb_last_saved := Now();
  Debug(dpSpam, rsections, 'kb_Save');
  seconds := config.ReadInteger(rsections, 'kb_keep_entries', 86400 * 7);
  x := TEncStringList.Create(passphrase);
  try
    try
      for i := 0 to kb_list.Count - 1 do
      begin
        p := TPazo(kb_list.Objects[i]);
        if ((p <> nil) and (1 <> Pos('TRANSFER-', kb_list[i])) and
          (1 <> Pos('REQUEST-', kb_list[i])) and
          (SecondsBetween(Now, p.added) < seconds)) then
          x.Add(GetKbPazo(p));
      end;
    except
      exit;
    end;
    x.SaveToFile(ExtractFilePath(ParamStr(0)) + 'slftp.kb');
  finally
    x.Free;
  end;

  debug(dpSpam, rsections, 'kb_Save - saving %d renames', [kb_skip.Count]);
  x := TEncStringList.Create(passphrase);
  try
    try
      for i := 0 to kb_skip.Count - 1 do
      begin
        if i > 249 then
          break;
        x.Add(kb_skip[i]);
      end;
    except
      exit;
    end;
    x.SaveToFile(ExtractFilePath(ParamStr(0)) + 'slftp.renames');
  finally
    x.Free;
  end;
end;

procedure kb_FreeList;
var
  i: integer;
begin
  for i := 0 to kb_list.Count - 1 do
  begin
    try
      if kb_List.Objects[i] <> nil then
      begin
        kb_List.Objects[i].Free;
        kb_List.Objects[i] := nil;
      end;
    except
      continue;
    end;
  end;

  kb_list.Free;
  kb_trimmed_rls.Free;
end;

function kb_reloadsections: boolean;
var
  xin: Tinifile;
  secs: TStringlist;
  r: TRegexpr;
  I: Integer;
begin
  //  Result := False;
  kb_sections.Free;
  kb_sections := TStringList.Create;
  kb_sections.Sorted := True;
  kb_sections.Duplicates := dupIgnore;

  secs := TStringlist.Create;
  r := TRegexpr.Create;
  xin := Tinifile.Create(ExtractFilePath(ParamStr(0)) + 'slftp.precatcher');
  try
    r.ModifierI := True;
    r.ModifierM := True;
    r.Expression := '^(\#|\/\/)';
    xin.ReadSection('sections', secs);
    for I := 0 to secs.Count - 1 do
      if not r.Exec(secs.Strings[i]) then
        kb_sections.Add(secs.Strings[i]);

    for I := 0 to mappingslist.Count - 1 do
    begin
      if TMap(mappingslist.Items[i]).origsection <> '' then
        kb_sections.Add(TMap(mappingslist.Items[i]).origsection);
      kb_sections.Add(TMap(mappingslist.Items[i]).newsection);
    end;

  finally
    xin.Free;
    r.free;
    secs.free;
  end;
  Result := True;
end;

procedure kb_Init;
var
  i, j: integer;
  ss: String;
  //  xin: Tinifile;
  x: TStringList;
  sectionmasks: TObjectList;
  sectionmask: String;
begin
  kb_last_saved := Now();
  //  kbevent:=TEvent.Create(nil,false,false,'PRETIME_WAIT_EVENT');
  noannouncesections := TStringList.Create;
  noannouncesections.DelimitedText := config.ReadString(rsections, 'noannouncesection', '');

  addpreechocmd := config.ReadString('dbaddpre', 'addpreechocmd', '!sitepre');

  kb_lock := TCriticalSection.Create;

  kb_sectionhandlers := TStringList.Create;
  for i := 1 to High(sectionhandlers) do
  begin
    {
    * some examples for both variables
    sectionhandlers: TMP3Release -- x: MP3,FLAC
    sectionhandlers: T0dayRelease -- x: 0DAY,PDA
    sectionhandlers: TNFORelease -- x: MDVDR,MV,MHD
    sectionhandlers: TTVRelease -- x: TVSD*,TV720P-*,TV*BLURAY,TV1080P
    }

    kb_sectionhandlers.Add(sectionhandlers[i].Name);

    sectionmasks := TObjectList.Create;

    x := TStringList.Create;
    try
      x.CaseSensitive := False;
      x.Delimiter := ',';
      // ignore dupe entries for lower memory usage
      x.Sorted := True;
      x.Duplicates := dupIgnore;

      x.DelimitedText := config.ReadString(rsections, sectionhandlers[i].Name, sectionhandlers[i].DefaultSections);

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


  kb_trimmed_rls := THashedStringList.Create;
  kb_trimmed_rls.CaseSensitive := False;

  kb_list := TStringList.Create;
  kb_list.CaseSensitive := False;
  kb_list.Duplicates := dupIgnore;

  kb_sections := TStringList.Create;
  kb_sections.Sorted := True;
  kb_sections.Duplicates := dupIgnore;

  rename_patterns := 4;

  //xin := Tinifile.Create(ExtractFilePath(ParamStr(0)) + 'slftp.precatcher');
  //  xin.ReadSection('sections', kb_sections);
  //  xin.Free;

  mp3genres := TStringList.Create;
  mp3genres.Delimiter := ' ';
  mp3genres.QuoteChar := '"';
  mp3genres.DelimitedText := config.ReadString(rsections, 'mp3genres', '');
  i := 0;
  while (i < mp3genres.Count) do
  begin
    ss := ReplaceText(mp3genres[i], ' ', '');
    if ss <> mp3genres[i] then
    begin
      mp3genres.Insert(i + 1, ss);
      Inc(i);
    end;
    Inc(i);
  end;

  mp3languages := TStringList.Create;
  mp3languages.Delimiter := ' ';
  mp3languages.QuoteChar := '"';
  mp3languages.CaseSensitive := False;
  mp3languages.DelimitedText := UpperCase(config.ReadString(rsections, 'mp3languages', ''));


  tvtags := TStringList.Create;
  tvtags.CaseSensitive := False;
  tvtags.DelimitedText := config.ReadString(rsections, 'tvtags', '');

  mp3sources := TStringList.Create;
  nulldaysources := TStringList.Create;

  x := TStringList.Create;
  try
    config.ReadSection(rsections, x);
    for i := 0 to x.Count - 1 do
    begin
      if (1 = Pos('mp3source_', x[i])) then
      begin
        mp3sources.Values[UpperCase(Copy(x[i], 11, 20))] := ' ' + config.ReadString(rsections, x[i], '') + ' ';
      end
      else if (1 = Pos('0daysource_', x[i])) then
      begin
        nulldaysources.Values[UpperCase(Copy(x[i], 12, 20))] := ' ' + config.ReadString(rsections, x[i], '') + ' ';
      end;
    end;
  finally
    x.Free;
  end;

  mp3types := TStringList.Create;
  mp3types.Delimiter := ' ';
  mp3types.QuoteChar := '"';
  mp3types.DelimitedText := config.ReadString(rsections, 'mp3types', '');

  kb_languages := TStringList.Create;
  kb_languages.CaseSensitive := False;
  kb_languages.DelimitedText := ReplaceText(ReplaceText(GetFileContents(ExtractFilePath(ParamStr(0)) + 'slftp.languages'), #13, ''), #10, '');

  //sectionhelper:= THashedStringList.Create;

  if FileExists(ExtractFilePath(ParamStr(0)) + 'imdbcountrys.nwo') then
  begin
    x := TStringList.Create;
    try
      x.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'imdbcountrys.nwo');
      x.SaveToFile(ExtractFilePath(ParamStr(0)) + 'slftp.imdbcountries');
      {$IFDEF MSWINDOWS}
        {$IFDEF UNICODE}
          DeleteFile(PChar(ExtractFilePath(ParamStr(0)) + 'imdbcountrys.nwo'));
        {$ELSE}
          DeleteFile(PAnsiChar(ExtractFilePath(ParamStr(0)) + 'imdbcountrys.nwo'));
        {$ENDIF}
      {$ELSE}
        DeleteFile(ExtractFilePath(ParamStr(0)) + 'imdbcountrys.nwo');
      {$ENDIF}
    finally
      x.Free;
    end;
  end;

  imdbcountries := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'slftp.imdbcountries');

  kb_groupcheck_rls := THashedStringList.Create;
  kb_latest := THashedStringList.Create;
  kb_skip := THashedStringList.Create;

  trimmed_shit_checker := config.ReadBool(rsections, 'trimmed_shit_checker', True);
  renamed_group_checker := config.ReadBool(rsections, 'renamed_group_checker', True);
  renamed_release_checker := config.ReadBool(rsections, 'renamed_release_checker', True);

  use_new_language_base := config.ReadBool(rsections, 'use_new_language_base', True);

  enable_try_to_complete := config.ReadBool(rsections, 'enable_try_to_complete', False);
  try_to_complete_after := config.ReadInteger(rsections, 'try_to_complete_after', 1100);

  kb_save_entries := config.ReadInteger(rsections, 'kb_save_entries', 3600);

  taskpretime_mode := config.ReadInteger('taskpretime', 'mode', 0);

  nomp3dirlistgenre := config.ReadBool(rsections, 'nomp3dirlistgenre', False);
  nonfodirlistgenre := config.ReadBool(rsections, 'nonfodirlistgenre', False);
  nomvdirlistgenre := config.ReadBool(rsections, 'nomvdirlistgenre', False);
end;

procedure kb_Stop;
begin
  while (kb_thread <> nil) do
    sleep(100);
end;

procedure kb_Uninit;
var
  i: integer;
begin
  Debug(dpSpam, rsections, 'Uninit1');
  kbevent.Free;
  kb_sections.Free;
  mp3genres.Free;
  mp3languages.Free;
  nulldaysources.Free;
  mp3sources.Free;
  mp3types.Free;
  kb_latest.Free;
  kb_skip.Free;
  //sectionhelper.Free;
  tvtags.Free;
  kb_groupcheck_rls.Free;

  for i := 0 to kb_sectionhandlers.Count - 1 do
  begin
    if Assigned(kb_sectionhandlers.Objects[i]) then
    begin
      kb_sectionhandlers.Objects[i].Free;
    end;
  end;
  kb_sectionhandlers.Free;

  kb_languages.Free;

  noannouncesections.Free;
  imdbcountries.Free;

  kb_lock.Free;

  Debug(dpSpam, rsections, 'Uninit2');
end;

{ TKBThread }

constructor TKBThread.Create;
begin
  inherited Create(False);
  FreeOnTerminate := True;
  kbevent := TEvent.Create(nil, False, False, 'kb');
end;

destructor TKBThread.Destroy;
begin
  kbevent.Free;
  inherited;

  // not sure if this is needed/useful?!
  kb_thread := nil;
end;

function TKBThread.AddCompleteTransfers(pazo: Pointer): boolean;
var
  i, j: integer;
  psrc, pdest: TPazoSite;
  ssrc, sdest: TSite;
  p: TPazo;
  ssrc_found: boolean;
begin
  Result := False;
  p := TPazo(pazo);
  Debug(dpMessage, rsections, '<!-- START AddCompleteTransfers %s', [p.rls.rlsname]);

  // check if the release is incomplete on any site and gather valid sites for filling
  for i := 0 to p.sites.Count - 1 do
  begin
    pdest := TPazoSite(p.sites[i]);

    if pdest.Name = getAdminSiteName then
      Continue;
    // if complete, we don't need to fill it there
    if pdest.Complete then
      Continue;
    if pdest.status <> rssAllowed then
      Continue;
    if pdest.error then
    begin
      Debug(dpMessage, rsections, Format('Error AddCompleteTransfers for %s: %s', [pdest.Name, pdest.reason]));
      Continue;
    end;

    sdest := FindSiteByName('', pdest.Name);
    if sdest = nil then
      Continue;
    if sdest.PermDown then
      Continue;

    // iterate through sites and find possible sources where rls is complete and create task
    for j := 0 to p.sites.Count - 1 do
    begin
      ssrc_found := False;
      psrc := TPazoSite(p.sites[j]);

      if psrc.Name = getAdminSiteName then
        Continue;
      if psrc.Name = pdest.Name then
        Continue;
      // if not complete, we cannot use it as source to fill
      if not psrc.Complete then
        Continue;
      if psrc.error then
      begin
        Debug(dpMessage, rsections, Format('Error AddCompleteTransfers for %s: %s', [psrc.Name, psrc.reason]));
        Continue;
      end;

      ssrc := FindSiteByName('', psrc.Name);
      if ssrc = nil then
        Continue;
      if ssrc.PermDown then
        Continue;

      if config.ReadBool(rsections, 'only_use_routable_sites_on_try_to_complete', False) then
        ssrc_found := ssrc.isRouteableTo(sdest.Name)
      else
        ssrc_found := True;

      // continue with next site from list if current site has no route
      if not ssrc_found then
        continue;

      Debug(dpMessage, rsections, 'Trying to complete %s on %s from %s', [p.rls.rlsname, pdest.Name, psrc.Name]);
      try
        pdest.Clear;

        // TODO: we need to check how to get it to work with non routable sites
        // we need to add them manually here but will they be used on race? Or do slftp overwritte them somewhere?

        AddTask(TPazoDirlistTask.Create('', '', psrc.Name, p, '', False, True));
        irc_Addstats(Format(
          '<c11>[<b>iNC</b> <b>%s</b>]</c> Trying to complete <b>%s</b> on <b>%s</b> from <b>%s</b>',
          [p.rls.section, p.rls.rlsname, pdest.Name, psrc.Name]));

        Result := True;
      except
        on e: Exception do
        begin
          Debug(dpError, rsections, Format('[EXCEPTION] TKBThread.AddCompleteTransfers.AddTask: %s', [e.Message]));
          irc_AddError(Format('[EXCEPTION] TKBThread.AddCompleteTransfers.AddTask: %s', [e.Message]));
          Result := False;
        end;
      end;
    end;
  end;

  Debug(dpMessage, rsections, '<-- END AddCompleteTransfers %s', [p.rls.rlsname]);
end;

{
-- PREVIOUS VERSION --
function TKBThread.AddCompleteTransfers(pazo: Pointer): boolean;
var
  j, i: integer;
  ps, pss: TPazoSite;
  p: TPazo;
  inc_srcsite, inc_dstsite: TSite;
  inc_srcdir, inc_dstdir: String;
  inc_rc: TCRelease;
  inc_rls: TRelease;
  inc_p: TPazo;
  inc_ps: TPazoSite;
  inc_pd: TPazoDirlistTask;
  sfound: boolean;
  tts, ts, site: TSite;
begin
  Result := False;
  p := TPazo(pazo);
  //  sfound := False;

  Debug(dpMessage, rsections, '--> AddCompleteTransfers %s', [p.rls.rlsname]);
  irc_Addstats(Format('--> AddCompleteTransfers %s (%d)',
    [p.rls.rlsname, p.sites.Count]));

  for i := 0 to p.sites.Count - 1 do
  begin
    ps := TPazoSite(p.sites[i]);

    irc_Addstats(Format('doing %s - status: %s error: %s',
      [ps.Name, ps.StatusText, BoolToStr(ps.error, True)]));

    (*
      case status of
        rssNotAllowed: Result:= Result + 'N';
        rssNotAllowedButItsThere: Result:= 'NABIT';
        rssAllowed: Result:= Result + 'A';
        rssShouldPre: Result:= Result + 'S';
        rssRealPre: Result:= Result + 'P';
        rssComplete: Result:= Result + 'C';
      end;
    *)
        //will be true if release is complete on site
    if ps.Complete then
    begin
      irc_Addstats(Format('<c4>site %s is complete</c>', [ps.Name]));
      Continue;
    end;

    //There is some error we need to check in later revs!
    if ps.error then
    begin
      irc_Addstats(Format('<c4>Error: AddCompleteTransfers for %s (%s)</c>', [ps.Name, ps.reason]));
      Continue;
    end;

    if ps.Name = config.ReadString('sites', 'admin_sitename', 'SLFTP') then
      Continue;

    //rssNotAllowed, rssNotAllowedButItsThere, rssAllowed, rssShouldPre, rssRealPre, rssComplete, rssNuked
    if ps.status <> rssAllowed then //ps.status != rssAllowed
    begin
      irc_Addstats(Format('<c4>site %s is not rssAllowed (%s)</c>',
        [ps.Name, ps.StatusText]));
      Continue;
    end;

    irc_Addstats(Format('<c8>site %s will be checked</c>', [ps.Name]));

    site := FindSiteByName('', ps.Name);
    if site = nil then
      Continue;
    if site.PermDown then
      Continue;

    irc_Addstats(Format('<c8>site %s is checked</c>', [ps.Name]));

    //checking if a irc chan is added for the site
    if Precatcher_Sitehasachan(ps.Name) then
    begin
      pss := nil;
      //sfound := False;

      for j := 0 to p.sites.Count - 1 do
      begin
        pss := TPazoSite(p.sites[j]);
        irc_Addstats(Format('do %s (%s) - status: %s error: %s Complete: %s',
          [pss.Name, ps.Name, pss.StatusText, BoolToStr(pss.error, True),
          BoolToStr(pss.Complete, True)]));

        if pss.Name = ps.Name then //set pss := nil; too
        begin
          irc_Addstats(Format('<c7>%s = %s</c>', [pss.Name, ps.Name]));
          pss := nil;
          Continue;
        end;

        if not pss.Complete then
        begin
          irc_Addstats(Format('<c7>%s is not complete</c>', [pss.Name]));
          pss := nil;
          Continue;
        end;

        irc_Addstats(Format('<c9>complete site %s found!</c>', [pss.Name]));

        if pss.destinations.IndexOf(ps) = -1 then //do we need that? for what???
        begin
          irc_Addstats(Format('<c7>no index on %s for %s</c>', [pss.Name,
            ps.Name]));
          pss := nil;
          continue;
        end;

        if config.ReadBool(rsections,
          'only_use_routable_sites_on_try_to_complete',
          False) then
        begin
          ts := FindSiteByName('', ps.Name);
          tts := FindSiteByName('', pss.Name);
          sfound := tts.isRouteableTo(ts.Name);
          //TSite.isRouteableFrom check sitesunit.pas
          irc_Addstats(Format('routable part: %s (%s) <- %s - sfound: %s',
            [ps.Name, ts.Name, pss.Name, BoolToStr(sfound, True)]));
        end
        else
        begin
          sfound := True; //use every site to complete
        end;
        (*
                for k := 0 to pss.destinations.Count - 1 do
                begin
                  if TSite(pss.destinations.Items[k]).Name = ps.Name then
                  begin
                    if config.ReadBool(rsections,
                      'only_use_routable_sites_on_try_to_complete', False) then
                    begin
                      sfound := TSite(pss).isRouteableTo(ps.Name);
                      if sfound then
                        break
                      else
                        continue;
                    end
                    else
                    begin//if config.ReadBool(rsections,'only_use_routable_sites_on_try_to_complete',False) then begin
                      sfound := True;
                      break;
                    end;
                  end;//if TSite(pss.destinations.Items[k]).name = ps.name then begin
                  if sfound then
                    break
                  else
                    continue;
                end;//for k := 0 to pss.destinations.Count - 1 do begin
        *)
        if sfound then
          break
        else
        begin
          Debug(dpMessage, rsections,
            '--> site %s is not routable to %s for filling inc - trying next site',
            [pss.Name, ps.Name]);
          irc_Addstats(Format(
            'site %s is not routable to %s for filling inc - trying next site',
            [pss.Name, ps.Name]));
          pss := nil;
          continue;
        end;
      end; //for j:= 0 to p.sites.Count -1 do

      irc_Addstats(Format('done with %s ->', [ps.Name]));

      if pss = nil then
      begin
        irc_Addstats(Format('pss is nil (%s)', [ps.Name]));
        Exit; //Continue; //Exit;
      end;

      site := FindSiteByName('', pss.Name);
      if site = nil then
      begin
        irc_Addstats(Format('site is nil (%s)', [ps.Name]));
        Exit; //Continue; //Exit;
      end;
      if site.PermDown then
      begin
        irc_Addstats(Format('site %s is permdown', [pss.Name]));
        Exit; //Continue; //Exit;
      end;

      irc_Addstats(Format('done with %s and %s', [pss.Name, ps.Name]));

      //if ps.Name = pss.Name then
      //  Exit; //is already checked above in " for j := 0 to p.sites.Count - 1 do "

      //if ((pss = nil) and (not pss.Complete)) then
      // Exit; //is already checked above in " for j := 0 to p.sites.Count - 1 do "

(*
      if ps.Complete then
        Exit; // Release is allready filled and complete!
      if ps.error then
        Exit; //There is some error we need to check in later revs!

      if ps.Name = config.ReadString('sites', 'admin_sitename', 'SLFTP') then
        Continue;
      //rssNotAllowed, rssNotAllowedButItsThere, rssAllowed, rssShouldPre, rssRealPre, rssComplete, rssNuked
      if ps.status <> rssAllowed then
        Continue;

     //if ps.Name = config.ReadString('sites', 'admin_sitename', 'SLFTP') then
       // Exit; //crap! checked seven lines before
*)
      if pss.Name = config.ReadString('sites', 'admin_sitename', 'SLFTP') then
        Continue; //Exit;

      (*
            site := FindSiteByName('', ps.Name);
            if site = nil then
              Exit;
            if site.PermDown then
              Exit;

      (*
            site := FindSiteByName('', pss.Name);
            if site = nil then
              Continue; //Exit;
            if site.PermDown then
              Continue; //Exit;
      *)

            // ok, megvan minden.
      Debug(dpMessage, rsections, 'Trying to complete %s on %s from %s',
        [p.rls.rlsname, ps.Name, pss.Name]);
      irc_Addstats(Format('Trying to complete %s on %s from %s',
        [p.rls.rlsname, ps.Name, pss.Name]));
      try
        inc_srcsite := FindSiteByName('', pss.Name);
        inc_dstsite := FindSiteByName('', ps.Name);
        inc_srcdir := inc_srcsite.sectiondir[p.rls.section];
        inc_srcdir := DatumIdentifierReplace(inc_srcdir);
        inc_dstdir := inc_dstsite.sectiondir[p.rls.section];
        inc_dstdir := DatumIdentifierReplace(inc_dstdir);

        inc_rc := FindSectionHandler(p.rls.section);
        inc_rls := inc_rc.Create(p.rls.rlsname, p.rls.section);
        inc_p := PazoAdd(inc_rls);

        inc_p.AddSite(inc_srcsite.Name, inc_srcdir, False);
        inc_p.AddSite(inc_dstsite.Name, inc_dstdir, False);

        kb_list.AddObject('TRANSFER-' +
          IntToStr(RandomRange(10000000, 99999999)), inc_p);

        inc_ps := inc_p.FindSite(inc_srcsite.Name);
        inc_ps.AddDestination(inc_dstsite.Name, 9);
        inc_ps := inc_p.FindSite(inc_dstsite.Name);
        inc_ps.status := rssAllowed;
        inc_ps.dirlist.need_mkdir := False;

        inc_ps := inc_p.FindSite(inc_srcsite.Name);
        inc_ps.dirlist.dirlistadded := True;
        inc_pd := TPazoDirlistTask.Create('', '', inc_ps.Name, inc_p, '',
          False);

        irc_Addstats(Format(
          '<c11>[<b>iNC %s</b>]</c> Trying to complete <b>%s</b> on %s from %s',
          [p.rls.section, p.rls.rlsname, ps.Name, pss.Name]));

        AddTask(inc_pd);
        QueueFire;
        Result := True;
      except
        on e: Exception do
        begin
          Debug(dpError, rsections,
            Format('[EXCEPTION] TKBThread.AddCompleteTransfers.AddTask: %s',
            [e.Message]));
          irc_Addstats(Format('[EXCEPTION] TKBThread.AddCompleteTransfers.AddTask: %s',
            [e.Message]));
          Result := False;
        end;
      end;
    end;
  end;
  begin
    Debug(dpMessage, rsections, '<-- AddCompleteTransfers %s', [p.rls.rlsname]);
    irc_Addstats(Format('<-- AddCompleteTransfers %s', [p.rls.rlsname]));
  end;
end;
}

procedure TKBThread.Execute;
var
  i, j: integer;
  s: TSite;
  p: TPazo;
  ps: TPazoSite;
begin
  while (not slshutdown) do
  begin
    try
      kb_lock.Enter;
      p := nil;
      try
        for i := 0 to kb_list.Count - 1 do
        begin
          if 1 = Pos('TRANSFER-', kb_list[i]) then
            Continue;
          if 1 = Pos('REQUEST-', kb_list[i]) then
            Continue;

          try
            p := TPazo(kb_list.Objects[i]);
          except
            Continue;
          end;
          if p = nil then
            Continue;
          if p.rls = nil then
            Continue;

          if enable_try_to_complete then
          begin
            if ((not p.completezve) and (not p.stopped) and (SecondsBetween(Now, p.lastTouch) >= try_to_complete_after)) then
            begin
              RemovePazo(p.pazo_id);
              while (not (p.queuenumber.Value <= 0)) do
              begin
                p.queuenumber.Decrease;
              end;
              p.completezve := True;
              Debug(dpSpam, rsections, 'Looking for incomplete sites of %s', [p.rls.rlsname]);
              AddCompleteTransfers(p);
            end;
          end;

          if ((p.ready) and (SecondsBetween(Now, p.lastTouch) > 3600) and
            (not p.stated) and (not p.cleared)) then
          begin
            RemovePazo(p.pazo_id);

            try
              RanksProcess(p);
            except
              on E: Exception do
              begin
                Debug(dpError, rsections, Format('[EXCEPTION] TKBThread.Execute RanksProcess(p) : %s', [e.Message]));
              end;
            end;

            for j := 0 to p.sites.Count - 1 do
            begin
              try
                if j > p.sites.Count then
                  Break;
              except
                Break;
              end;
              try
                ps := TPazoSite(p.sites[j]);
                if (ps.dirlist = nil) then
                  Continue;

                s := FindSiteByName('', ps.Name);
                if s = nil then
                  Continue;

                statsProcessDirlist(ps.dirlist, ps.Name, p.rls.section, s.UserName);
              except
                on E: Exception do
                begin
                  Debug(dpError, rsections,
                    Format('[EXCEPTION] TKBThread.Execute statsProcessDirlist : %s',
                    [e.Message]));
                end;
              end;
            end;

            p.Clear;
            p.stated := True;
          end;
        end;
      finally
        kb_lock.Leave;
      end;
    except
      on e: Exception do
      begin
        Debug(dpError, rsections, '[EXCEPTION] TKBThread.Execute: %s',
          [e.Message]);
      end;
    end;

    if ((kb_save_entries <> 0) and (SecondsBetween(Now(), kb_last_saved) >
      kb_save_entries)) then
    begin
      try
        kb_lock.Enter;
        try
          kb_Save;
        finally
          kb_lock.Leave;
        end;
      except
        on e: Exception do
        begin
          Debug(dpError, rsections, '[EXCEPTION] kb_Save : %s', [e.Message]);
        end;
      end;
    end;

    //sleep(900);
    kbevent.WaitFor(5000);
  end;
end;

end.

