unit globals;

interface

type
  TDirType = (IsUnknown, IsMain, IsMultiCD, IsSample, IsProof, IsCovers, IsSubs);

const
  SiteColorOnline = 'c3'; //< Green color for sstUp @link(sitesunit.TSiteStatus)
  SiteColorOffline = 'c4'; //< Light Red color for sstDown @link(sitesunit.TSiteStatus)
  SiteColorPermdown = 'c5'; //< Brown color if site is set to permdown
  SiteColorUnknown = 'c14'; //< Grey color for sstUnknown @link(sitesunit.TSiteStatus)
  DATABASEFOLDERNAME = 'databases'; //< foldername for all internal database files

implementation

end.
