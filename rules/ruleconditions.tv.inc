{ Rule Descriptions for TV => TTVRelease }
const
  TVLookupDoneDescription: string = 'Returns true when the TV lookup has been performed, otherwise false.';

  TVShowNameDescription: string = 'Returns with the tvshow''s name. This is the part before the S01E01, 1x12, E01, etc tag.';

  TVtagDescription: string = 'Returns the "tv tag" of the releasename, tries to match with tvtags in slftp.ini.';

  TVPremierYearDescription: string = 'Returns with the year of premiere (based on TVMaze).';

  TVCountryDescription: string = 'Returns with the Country field parsed from TVMaze.';

  TVLanguageDescription: string = 'Returns with the Language field parsed from TVMaze.';

  TVClassificationDescription: string = 'Returns with the Classification field parsed from TVMaze.';

  TVScriptedDescription: string = 'Returns true if the show is scripted, otherwise false.';

  TVGenresDescription: string = 'Returns with the Genres field parsed from TVMaze.';

  TVNetworkDescription: string = 'Returns with the Network field parsed from TVMaze.';

  TVRuntimeDescription: string = 'Returns with the Runtime field parsed from TVMaze.';

  TVEndedYearDescription: string = 'Returns with the end year field parsed from TVMaze.';

  TVRunningDescription: string = 'Returns true if the show is still running, otherwise false.';

  TVStatusDescription: string = 'Returns with the Status field parsed from TVMaze.';

  TVCurrentSeasonDescription: string = 'Returns true when the releasename has the same season number as the database, otherwise false.';

  TVCurrentEpisodeDescription: string = 'Returns true when the releasename has the same episode number as the database, otherwise false.';

  TVCurrentOnAirDescription: string = 'Returns true when the releasename has the same season and episode number as the database, otherwise false.';

  TVDailyShowDescription: string = 'Returns true when the show airs daily, otherwise false.';

  TVRatingDescription: string = 'Returns with the TMaze rating of the show. (MULTIPLIED by ten: max score is 100, min score is 0)';
