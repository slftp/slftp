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

var
  GlobalSkiplistRegex: String; //< global_skip regex from slftp.ini

implementation

end.
