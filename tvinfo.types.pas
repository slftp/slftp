unit tvinfo.types;

interface

type
  { @abstract(Possible return values for special cases in getShowValues procedure)
    @value(tvInitialValue Initial value which is set as default value)
    @value(tvNotMatched For cases where main regex matched but single matches don't contain useful values)
    @value(tvConversionError Value if StrToIntDef failed to convert input)
    @value(tvDatedShow Season value for dated shows)
    @value(tvRegularSerieWithoutSeason Season value for shows which only have an episode tag)
    @value(tvNoExplicitShowTag Shows without season/episode/dated tag (mostly tv movies or sports))
    @value(tvNoEpisodeTag Shows without episode tag (mostly full season releases)) }
  TTVGetShowValuesIdentifier = (tvNoEpisodeTag = -110, tvNoExplicitShowTag = -100, tvRegularSerieWithoutSeason = -90,
    tvDatedShow = -80, tvConversionError = -70, tvNotMatched = -60, tvInitialValue = -50);

  { @abstract(Possible 'error' values for season and episode info lookups on the web)
    @value(tvSeEpInitialValue Initial value which is set as default value)
    @value(tvSeEpAirdatePrevAndNextOnSameDay Airdate of previous and next episode are on the same day)
    @value(tvSeEpShowEnded Show ended)
    @value(tvSeEpNoNextOrPrev No information about the next episode and next season) }
  TTVSeasonEpisodeWebInfo = (tvSeEpNoNextOrPrev = -6, tvSeEpShowEnded = -5, tvSeEpAirdatePrevAndNextOnSameDay = -4, tvSeEpInitialValue = -3);

implementation

end.
