{ Rule Descriptions for MVID/MDVDR/MBLURAY => TMVIDRelease }
const
  MVIDLookupDoneDescription: string = 'Returns true when the mvid lookup has been performed. Returns false if lookup is not yet ready.';

  MVIDGenreDescription: string = 'Returns the Genre(s) and Subgenre(s) parsed from NFO (needs mvidlookupdone!).';

  MVIDFilesDescription: string = 'Returns the number of files parsed from SFV (needs mvidlookupdone!).';

  MVIDYearDescription: string = 'Returns with the year of the release name. Returns with zero if year tag is not present.';

  MVIDCurrentYearDescription: string = 'Returns true if the year of the MVID release equals the current year.';

  MVIDVADescription: string = 'Returns true if release is from Various Artists (VA).';

  MVIDPALDescription: string = 'Returns true if the NFO indicates PAL region (needs mvidlookupdone!).';

  MVIDNTSCDescription: string = 'Returns true if the NFO indicates NTSC region (needs mvidlookupdone!).';

  MVIDLIVEDescription: string = 'Returns true if release is a LIVE performance.';

  MVIDLanguageDescription: string = 'Returns with the MVID language tag. Language is EN by default.';
