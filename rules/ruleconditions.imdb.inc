{ Rule Descriptions for IMDB => TIMDBRelease }
const
  IMDBYearDescription: string = 'Returns with the year of the movie''s release date. Returns zero if IMDB lookup is not yet ready.';

  IMDBLanguagesDescription: string = 'Returns with the list of the movie''s languages.';

  IMDBCountriesDescription: string = 'Returns with the list of the countries which cooperated in recording the movie.';

  IMDBGenresDescription: string = 'Returns with the list of the movie''s genres.';

  IMDBScreensDescription: string = 'Returns with the number of opening screens of the movie. Returns zero if IMDB lookup is not yet ready.';

  IMDBRatingDescription: string = 'Returns with the current IMDB rating of the movie MULTIPLIED by ten. (so max score is 100, min is 0) Returns zero if IMDB lookup is not yet ready.';

  IMDBVotesDescription: string = 'Returns with the number of votes of the movie. Returns zero if IMDB lookup is not yet ready.';

  IMDBLimitedDescription: string = 'Returns with the a boolean for Limited. Returns false if IMDB lookup is not yet ready.';

  IMDBWideDescription: string = 'Returns with the a boolean for Wide (CINE). Returns false if IMDB lookup is not yet ready.';

  IMDBFestivalDescription: string = 'Returns with the a boolean for Festival. Returns false if IMDB lookup is not yet ready.';

  IMDBSTVDescription: string = 'Returns true, if the movie is STV (Reads Countries from slftp.imdbcountries). Returns false if IMDB lookup is not yet ready.';

  IMDBCineYearDescription: string = 'Returns the Screeing year of the movie. Returns zero if IMDB lookup is not yet ready.';
