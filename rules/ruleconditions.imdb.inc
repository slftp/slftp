{ Rule Descriptions for IMDB => TIMDBRelease }
const
  IMDBLookupDoneDescription: string = 'Returns true when the IMDB lookup has been performed, otherwise false.';

  IMDBYearDescription: string = 'Returns with the year of the movie''s release date.';

  IMDBCurrentYearDescription: string = 'Returns true, if the year of the movie''s release date equals the current year.';

  IMDBLanguagesDescription: string = 'Returns with the list of the movie''s languages.';

  IMDBCountriesDescription: string = 'Returns with the list of the countries which cooperated in recording the movie.';

  IMDBGenresDescription: string = 'Returns with the list of the movie''s genres.';

  IMDBScreensDescription: string = 'Returns with the number of opening screens of the movie.';

  IMDBRatingDescription: string = 'Returns with the IMDB rating of the movie. (MULTIPLIED by ten: max score is 100, min score is 0)';

  IMDBVotesDescription: string = 'Returns with the number of IMDB votes of the movie.';

  IMDBLimitedDescription: string = 'Returns true if the movie is Limited, otherwise false.';

  IMDBWideDescription: string = 'Returns true if the movie is Wide (CINE), otherwise false.';

  IMDBFestivalDescription: string = 'Returns true if the movie was aired on a Festival, otherwise false.';

  IMDBSTVDescription: string = 'Returns true, if the movie is STV (Reads Countries from slftp.imdbcountries), otherwise false.';

  IMDBCineYearDescription: string = 'Returns the screening year of the movie.';

  IMDBCurrentCineYearDescription: string = 'Returns true, if the screening year of the movie equals the current year.';
