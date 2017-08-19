(*Rule Description's*)
const

  {  Common => TRelease  }
  TagDescription: string = 'You can use tag condition to check if a specified tag exists in the releasename.' + #13#10 +
    'Basicly, tag is a shorthand to *.tag.*, *.tag-*, *-tag.*, *-tag-*' + #13#10 + '!ruleadd HQ PS2 if tag = NTSC then DROP' + #13#10 +
    '!ruleadd HQ TV if not tag in HDTV, PDTV then DROP';
  ReleaseNameDescription: string = 'Returns with the name of the release.' + #13#10 +
    'Example: if releasename =~ *-keygen* then DROP' + #13#10;
  SectionDescription: string = 'Returns with the section name of the release.' + #13#10 +
    'if section = TV then DROP' + #13#10;
  NotCompleteDescription: string = 'Returns with the list of the sites where the release is not yet complete.' + #13#10 +
    'This is the negated complete condition. It is true if the release is not notAllowed at the specified site and it is not pre or complete.' + #13#10 +
    'For eg, you can add this rule on a slow 100mbit site: if notcomplete @ GBITSITE1 && source = GBITSITE2 then DROP' + #13#10 +
    'With this rule, 100mbit site will be started racing after gbitsite1 is finished.' + #13#10;
  CompleteDescription: string = 'Returns with the list of the sites, where the release is complete.' + #13#10 +
    'if complete @ SITENAME && source != SITENAME then DROP' + #13#10;
  PreDescription: string =
    'Returns true, if the release is pred on the specified site.' + #13#10 +
    'If you add this rule on a slow site, pres wont be sent there til gbitsite is incomplete: ' + #13#10 +
    'if pre @ AFFILSITE && incomplete @ GBITSITE then DROP' + #13#10;
  NotAllowedDescription: string =
    'Returns true, if the release is not allowed on the specified site.' + #13#10 +
    'Example: notallowed @ SITENAME' + #13#10;
  AllowedDescription: string = 'Returns true, if the release is not notallowed on the specified site.' + #13#10 +
    'Example: allowed @ SITENAME' + #13#10;
  GroupDescription: string = 'Returns with the groupname.' + #13#10 +
    'Example: group in GRP1, GRP2, GRP3' + #13#10;
  KnownGroupDescription: string = 'Returns true, if the groupname is known (you can set the list in slftp.ini if i remember well).' + #13#10 +
    'Note: If no known groups is set for a section in slftp.knowngroups, this condition wont match at all!' + #13#10 +
    'Example: !ruleadd * * ifnot knowngroup then DROP' + #13#10;
  UnKnownGroupDescription: string = 'Returns true, if the groupname is not known.' + #13#10 +
    'Note: If no known groups is set for a section in slftp.knowngroups, this condition wont match at all!' + #13#10 +
    'Example: !ruleadd * * if unknowngroup then DROP' + #13#10;
  SourceDescription: string = 'Returns with the name of the source site, which is currently fireing the ruleset.' + #13#10 +
    'You can use this function to setup static routing.' + #13#10 +
    'For eg, we dont want to race SITE1' + chr(39) + 's MP3 section from SITE2 (but we want everything else):' + #13#10 +
    'Example: !ruleadd SITE1 MP3 if source = SITE2 then DROP' + #13#10;
  DestinationDescription: string = 'Returns with the name of the destination site.' + #13#10 +
    'You can use this function to make exceptions in generic rules.' + #13#10 +
    'For eg, we want to drop stuffs detected as fake except on a few dumps: '
    + #13#10 +
    '!ruleadd * * if fake && destination notin DUMP1, DUMP2 then DROP' + #13#10;
  NewdirSourceDescription: string = 'Returns true, if source is newdir (not pre or complete).' + #13#10 +
    'Example: if newdirsource then DROP' + #13#10;
  FakeDescription: string = 'Returns true if the release was recognized as fake by the fakechecker.' + #13#10;
  ForeignDescription: string = 'Returns true, if the releasename has a language tag (other than english).' + #13#10 +
    'Note: as mp3 rips have 2 letter codes, use mp3foreign and mp3language conditions.' + #13#10;
  LanguageDescription: string = 'Returns with the recognized language tags of the release.' + #13#10 +
    'Note: as mp3 rips have 2 letter codes, use mp3foreign and mp3language conditions.' + #13#10+
    'ifnot language in English, Slowenian, Chinese then DROP' + #13#10;

  CompleteSourceDescription: string = 'Returns true, if the source site is complete. You can use this rule to fill some sites from complete source only.';
  YearDescription: string = 'Returns with the year tag of the release name. Returns with zero if year tag is not present.';

  AutofollowDescription: string = 'Returns true, if there was already an irc announce on the site (mean: somebody started to send the release there).' + #13#10 +
    'You have to add atleast one NEWDIR irc catchevent to use this rule.' + #13#10 + 'ex: !ruleadd * * if autofollow then ALLOW';
  NukeDescription: string = 'Returns true, if the release was nuked on the site.' + #13#10 + 'ex: !ruleadd * * if nuked then DROP';
  PredDescription: string = 'Returns true, if there was a pre for this release on any of your sites.' + #13#10;

  DisksDescription: string = 'Returns with the number of disks (for eg 3 for Foobar.2008.NTSC.3DiSC.MDVDR-GRP' + #13#10 + 'NOTE: this is NOT for MP3 rips.' + #13#10;

  {  0DAY => T0DAYRelease  }
  zerodaySourceDescription: string = 'Returns with the 0day rip''s source/OS.' + #13#10 + 'Example: if not ( 0daysource in WIN, LINUX ) then DROP' + #13#10;

  {  TV => TTVRelease  }
  TVCurrentEpisiodeDescription: string = 'Returns true when the rip has the same episode number as the database. Returns false when we have no value.';
  TVCurrentSeasonDescription: string = 'Returns true when the rip has the same season number as the database. Returns false when we have no value.';
  TVDailyShowDescription: string = 'Returns true when the rip airs daily. Returns false when we have no value.';
  TVCurrentOnAirDescription: string = 'Returns true when the rip has the same season and episoden number as the database. Returns false when we have no value.';
  TVShowNameDescription: string = 'Returns with the tvshow''s name. This is the part before the S01E01 or 1x12 tag.';
  TVPremierYearDescription: string = 'Returns with the year of premier (based on TVMaze). Returns zero if lookup is not ready yet.';
  TVCountryDescription: string = 'Returns with the Country field parsed from TVMaze.';
  TVClassificationDescription: string = 'Returns with the Classification field parsed from TVMaze.';
  TVScriptedDescription: string = 'Returns with the Classification field parsed from TVMaze. Returns false if lookup is not ready yet.';

  {*
  * old one, deprecated since 1.5.5 dev
  TVGenresDescription: string = 'Returns with the Genres field parsed from TVMaze plus TheTVDb. vailed are: ' + #10#13 +
    'Action, Adult, Adventure, Animals, Anime, Animation, Children, Comedy, Cooking, Crime, DIY, Documentary, Drama, Espionage, Family, ' + #10#13 +
    'Fantasy, Food, Game Show, History, Horror, Home and Garden, News, Medical, Mini-Series, Music, Mystery, Reality, Romance, Science-Fiction, ' + #10#13 +
    'Special Interest, Soap, Sport, Suspense, Talk Show, Thriller, Travel, War, Western';
  *}
  TVGenresDescription: string = 'Returns with the Genres field parsed from TVMaze plus TheTVDb.';

  TVNetworkDescription: string = 'Returns with the Network field parsed from TVMaze.';
  TVRuntimeDescription: string = 'Returns with the Runtime field parsed from TVMaze. Returns 0 if lookup is not ready yet';
  TVEndedYearDescription: string = 'Returns with the end year field parsed from TVMaze. Returns 0 if lookup is not ready yet';
  TVStatusDescription: string = 'Returns with the Status field parsed from TVMaze.';
  TVRunningDescription: string = 'Returns with the Status field parsed from TVMaze. Returns false if lookup is not ready yet.';
  TVtagDescription: string = 'Returns the "tv tag" of the release name. Check slftp.ini, [kb] tvtags.';
  TVLanguageDescription: string = 'Returns with the Language field parsed from TVMaze.';

  {  IMDB => TIMDBRelease  }
  IMDBWideDescription: string = 'Returns with the a boolean for Wide (CINE). Returns false if IMDB lookup is not yet ready.';
  IMDBLimitedDescription: string = 'Returns with the a boolean for Limited. Returns false if IMDB lookup is not yet ready.';
  IMDBFestivalDescription: string = 'Returns with the a boolean for Festival. Returns false if IMDB lookup is not yet ready.';
  IMDBSTVDescription: string = 'Returns true, if the movie is STV (Reads Countries from slftp.imdbcountries). Returns false if IMDB lookup is not yet ready.';
  IMDBCineYearDescription: string = 'Returns the Screeing year of the movie. Returns zero if IMDB lookup is not yet ready.';
  IMDBVotesDescription: string = 'Returns with the number of votes of the movie. Returns zero if IMDB lookup is not yet ready.';
  IMDBRatingDescription: string = 'Returns with the current IMDB rating of the movie MULTIPLIED by ten. (so max score is 100, min is 0) Returns zero if IMDB lookup is not yet ready.';
  IMDBScreensDescription: string = 'Returns with the number of opening screens of the movie. Returns zero if IMDB lookup is not yet ready.';
  IMDBYearDescription: string = 'Returns with the year of the movie''s release date. Returns zero if IMDB lookup is not yet ready.';


  {  MP3 => TMP3Release  }
  {  FO => TNFORelease  }
  {  MVID => TMP3Release  }
  {  n => TnRelease  }
  {  n => TnRelease  }

