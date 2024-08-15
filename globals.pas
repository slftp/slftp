unit globals;

interface

type
  TDirType = (IsUnknown, IsMain, IsMultiCD, IsSample, IsProof, IsCovers, IsSubs);

const
  SiteColorOnline = 'c3'; //< Green color for @link(sitesunit.TSiteStatus.sstUp)
  SiteColorOffline = 'c4'; //< Light Red color for @link(sitesunit.TSiteStatus.sstDown)
  SiteColorPermdown = 'c5'; //< Brown color if site is set to permdown
  SiteColorUnknown = 'c14'; //< Grey color for @link(sitesunit.TSiteStatus.sstUnknown)
  DATABASEFOLDERNAME = 'databases'; //< foldername for all internal database files
  CONST_RAR_FILES = '_RAR_'; //< pseudo file extension to cover all RAR files
  CONST_NFO_FAILED_THRESHOLD = 4; //< number of consecutive failures when downloading NFO (or SFV) files to set ufnAutoDisabled on a site

implementation

end.
