{ Rule Descriptions for TV => TTVRelease }
const
  TVShowNameDescription: string = 'Returns with the tvshow''s name. This is the part before the S01E01, 1x12, E01, etc tag.';

  TVtagDescription: string = 'Returns the "tv tag" of the release name if matched with tvtags in slftp.ini.';

  TVPremierYearDescription: string = 'Returns with the year of premiere (based on TVMaze). Returns zero if lookup is not ready yet.';

  TVCountryDescription: string = 'Returns with the Country field parsed from TVMaze.';

  TVLanguageDescription: string = 'Returns with the Language field parsed from TVMaze.';

  TVClassificationDescription: string = 'Returns with the Classification field parsed from TVMaze.';

  TVScriptedDescription: string = 'Returns with the Classification field parsed from TVMaze. Returns false if lookup is not ready yet.';

  TVGenresDescription: string = 'Returns with the Genres field parsed from TVMaze.';

  TVNetworkDescription: string = 'Returns with the Network field parsed from TVMaze.';

  TVRuntimeDescription: string = 'Returns with the Runtime field parsed from TVMaze. Returns 0 if lookup is not ready yet';

  TVEndedYearDescription: string = 'Returns with the end year field parsed from TVMaze. Returns 0 if lookup is not ready yet';

  TVRunningDescription: string = 'Returns with the Status field parsed from TVMaze. Returns false if lookup is not ready yet.';

  TVStatusDescription: string = 'Returns with the Status field parsed from TVMaze.';

  TVCurrentSeasonDescription: string = 'Returns true when the rip has the same season number as the database. Returns false when we have no value.';

  TVCurrentEpisodeDescription: string = 'Returns true when the rip has the same episode number as the database. Returns false when we have no value.';

  TVCurrentOnAirDescription: string = 'Returns true when the rip has the same season and episode number as the database. Returns false when we have no value.';

  TVDailyShowDescription: string = 'Returns true when the show airs daily. Returns false when we have no value.';
