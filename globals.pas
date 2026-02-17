unit globals;

interface

uses
  SysUtils, Generics.Collections, SyncObjs;

type
  TDirType = (IsUnknown, IsMain, IsMultiCD, IsSample, IsProof, IsCovers, IsSubs);
  
var
  { Slot monitoring enabled slots - only specific slots when opened in WebUI }
  GlSlotMonitoredSlots: TDictionary<string, Boolean>;
  GlSlotMonitoredSlotsLock: TCriticalSection;
  
{ Check if a specific slot should be monitored (history recorded) }
function IsSlotMonitored(const aSiteName: string; aSlotNumber: integer): Boolean;

{ Enable/disable monitoring for a specific slot }
procedure SetSlotMonitored(const aSiteName: string; aSlotNumber: integer; aEnabled: Boolean);

const
  SiteColorOnline = 'c3'; //< Green color for @link(sitesunit.TSiteStatus.sstUp)
  SiteColorOffline = 'c4'; //< Light Red color for @link(sitesunit.TSiteStatus.sstDown)
  SiteColorPermdown = 'c5'; //< Brown color if site is set to permdown
  SiteColorUnknown = 'c14'; //< Grey color for @link(sitesunit.TSiteStatus.sstUnknown)
  DATABASEFOLDERNAME = 'databases'; //< foldername for all internal database files
  CONST_RAR_FILES = '_RAR_'; //< pseudo file extension to cover all RAR files
  CONST_NFO_FAILED_THRESHOLD = 4; //< number of consecutive failures when downloading NFO (or SFV) files to set ufnAutoDisabled on a site

implementation

function IsSlotMonitored(const aSiteName: string; aSlotNumber: integer): Boolean;
var
  key: string;
begin
  Result := False;
  if GlSlotMonitoredSlots = nil then
    Exit;
    
  key := UpperCase(aSiteName) + '/' + IntToStr(aSlotNumber);
  GlSlotMonitoredSlotsLock.Acquire;
  try
    Result := GlSlotMonitoredSlots.ContainsKey(key);
  finally
    GlSlotMonitoredSlotsLock.Release;
  end;
end;

procedure SetSlotMonitored(const aSiteName: string; aSlotNumber: integer; aEnabled: Boolean);
var
  key: string;
begin
  if GlSlotMonitoredSlots = nil then
  begin
    GlSlotMonitoredSlotsLock.Acquire;
    try
      if GlSlotMonitoredSlots = nil then
        GlSlotMonitoredSlots := TDictionary<string, Boolean>.Create;
    finally
      GlSlotMonitoredSlotsLock.Release;
    end;
  end;
    
  key := UpperCase(aSiteName) + '/' + IntToStr(aSlotNumber);
  GlSlotMonitoredSlotsLock.Acquire;
  try
    if aEnabled then
      GlSlotMonitoredSlots.AddOrSetValue(key, True)
    else
      GlSlotMonitoredSlots.Remove(key);
  finally
    GlSlotMonitoredSlotsLock.Release;
  end;
end;

initialization
  GlSlotMonitoredSlotsLock := TCriticalSection.Create;
  GlSlotMonitoredSlots := nil;

finalization
  if GlSlotMonitoredSlots <> nil then
    GlSlotMonitoredSlots.Free;
  GlSlotMonitoredSlotsLock.Free;

end.
