unit globals;

interface

uses
  SysUtils;

type
  TDirType = (IsUnknown, IsMain, IsMultiCD, IsSample, IsProof, IsCovers, IsSubs);

const
  SiteColorOnline = 'c3'; //< Green color for sstUp @link(sitesunit.TSiteStatus)
  SiteColorOffline = 'c4'; //< Light Red color for sstDown @link(sitesunit.TSiteStatus)
  SiteColorPermdown = 'c5'; //< Brown color if site is set to permdown
  SiteColorUnknown = 'c14'; //< Grey color for sstUnknown @link(sitesunit.TSiteStatus)

var
  formatSettings: TFormatSettings; //< TFormatSettings for date/time handling (formatted as in official or technical documents)

procedure InitGlobalValues;

implementation

procedure InitGlobalValues;
begin
  {$IFDEF MSWINDOWS}
    GetLocaleFormatSettings(1033, formatSettings);
  {$ELSE}
    formatSettings := DefaultFormatSettings;
  {$ENDIF}
  formatSettings.ShortDateFormat := 'yyyy-mm-dd'; // Year-Month-Day order
  formatSettings.ShortTimeFormat := 'hh:mm';
  formatSettings.DateSeparator := '-';
  formatSettings.TimeSeparator := ':';
end;

end.
