(*Rule Description's*)
const
  TagDescription: string = 'You can use tag condition to check if a specified tag exists in the releasename.' + #13#10 +
    'Basicly, tag is a shorthand to *.tag.*, *.tag-*, *-tag.*, *-tag-*' + #13#10 + '!ruleadd HQ PS2 if tag = NTSC then DROP' + #13#10 +
    '!ruleadd HQ TV if not tag in HDTV, PDTV then DROP';

  {   Televison => TTVRelease       }
  TVCurrentEpisiodeDescription: string = 'Returns true when the rip has the same episode number as the database. Returns false when we have no value.';
  TVCurrentSeasonDescription: string = 'Returns true when the rip has the same season number as the database. Returns false when we have no value.';
  TVDailyShowDescription: string = 'Returns true when the rip airs daily. Returns false when we have no value.';
  TVCurrentOnAirDescription: string = 'Returns true when the rip has the same season+episoden number as the database. Returns false when we have no value.';
  TVShowNameDescription: string = 'Returns with the tvshow''s name. This is the part before the S01E01 or 1x12 tag.';
  TVPremierYearDescription: string = 'Returns with the year of premier (based on TVMaze). Returns zero if lookup is not ready yet.' + #10#13 +
    'Scripted, Animation, Reality, Talk Show, Documentary, Game Show, News, Sports, Variety';
  TVCountryDescription: string = 'Returns with the Country field parsed from TVMaze.';
  TVClassificationDescription: string = 'Returns with the Classification field parsed from TVMaze. Vailed are:';
  TVScriptedDescription: string = 'Returns with the Classification field parsed from TVMaze. Returns false if lookup is not ready yet.';
  TVGenresDescription: string = 'Returns with the Genres field parsed from TVMaze plus TheTVDb. vailed are: ' + #10#13 +
  'Action, Adult, Adventure, Animals, Anime, Animation, Children, Comedy, Cooking, Crime, DIY, Documentary, Drama, Espionage, Family, ' + #10#13 +
    'Fantasy, Food, Game Show, History, Horror, Home and Garden, News, Medical, Mini-Series, Music, Mystery, Reality, Romance, Science-Fiction, ' + #10#13 +
    'Special Interest, Soap, Sport, Suspense, Talk Show, Thriller, Travel, War, Western';

  TVNetworkDescription: string = 'Returns with the Network field parsed from TVMaze.';
  TVRuntimeDescription: string = 'Returns with the Runtime field parsed from TVMaze. Returns 0 if lookup is not ready yet';
  TVEndedYearDescription: string = 'Returns with the end year field parsed from TVMaze. Returns 0 if lookup is not ready yet';
  TVStatusDescription: string = 'Returns with the Status field parsed from TVMaze.';
  TVRunningDescription: string = 'Returns with the Status field parsed from TVMaze. Returns false if lookup is not ready yet.';
  TVtagDescription: string = 'Returns the "tv tag" of the release name. Check slftp.ini, [kb] tvtags.';
