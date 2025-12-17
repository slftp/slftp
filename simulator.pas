{ @abstract(Simulator for testing release routing and rule evaluation)
  This unit provides functionality to simulate how a release would be processed
  without actually adding it to the Knowledge Base or triggering any real transfers. }
unit simulator;

interface

uses
  Classes, kb.releaseinfo, pazo, Generics.Collections, taskpretime;

type
  { @abstract(Result of a site simulation)
    Contains information about whether a release would be allowed on a site and why }
  TSiteSimulationResult = class
  private
    FSitename: String;
    FSection: String;
    FAllowed: Boolean;
    FReason: String;
    FRuleAction: String;
    FIsAffil: Boolean;
    FHasSection: Boolean;
    FSiteDown: Boolean;
    FPretimeOk: Boolean;
  public
    property Sitename: String read FSitename write FSitename;
    property Section: String read FSection write FSection;
    property Allowed: Boolean read FAllowed write FAllowed;
    property Reason: String read FReason write FReason;
    property RuleAction: String read FRuleAction write FRuleAction;
    property IsAffil: Boolean read FIsAffil write FIsAffil;
    property HasSection: Boolean read FHasSection write FHasSection;
    property SiteDown: Boolean read FSiteDown write FSiteDown;
    property PretimeOk: Boolean read FPretimeOk write FPretimeOk;
  end;

  { @abstract(Result of a route simulation)
    Contains information about which routes would be built between sites }
  TRouteSimulationResult = class
  private
    FSourceSite: String;
    FDestinationSite: String;
    FRank: Integer;
    FRouteWeight: Integer;
  public
    property SourceSite: String read FSourceSite write FSourceSite;
    property DestinationSite: String read FDestinationSite write FDestinationSite;
    property Rank: Integer read FRank write FRank;
    property RouteWeight: Integer read FRouteWeight write FRouteWeight;
  end;

  { @abstract(Complete simulation result)
    Contains all simulation results for a release }
  TSimulationResult = class
  private
    FReleasename: String;
    FSection: String;
    FSiteResults: TObjectList<TSiteSimulationResult>;
    FRouteResults: TObjectList<TRouteSimulationResult>;
    FTotalSites: Integer;
    FAllowedSites: Integer;
    FErrorMessage: String;
  public
    constructor Create;
    destructor Destroy; override;

    property Releasename: String read FReleasename write FReleasename;
    property Section: String read FSection write FSection;
    property SiteResults: TObjectList<TSiteSimulationResult> read FSiteResults;
    property RouteResults: TObjectList<TRouteSimulationResult> read FRouteResults;
    property TotalSites: Integer read FTotalSites write FTotalSites;
    property AllowedSites: Integer read FAllowedSites write FAllowedSites;
    property ErrorMessage: String read FErrorMessage write FErrorMessage;
  end;

{ Simulate how a release would be processed
  @param(aSection The section name for the release)
  @param(aReleasename The release name to simulate)
  @param(aSimulatePre If @true, simulate as a PRE, otherwise as NEWDIR)
  @returns(Simulation result object containing all information) }
function SimulateRelease(const aSection, aReleasename: String; const aSimulatePre: Boolean = False): TSimulationResult;

{ Format simulation results as text for IRC output
  @param(aResult The simulation result to format)
  @returns(List of text lines ready for IRC output) }
function FormatSimulationResult(const aResult: TSimulationResult): TStringList;

procedure SimulatorInit;
procedure SimulatorUninit;

implementation

uses
  SysUtils, TypInfo, sitesunit, rulesunit, routeconfig, debugunit, DateUtils, StrUtils, configunit, kb, encinifile {$IFDEF MSWINDOWS}, Windows{$ENDIF};

const
  rsections = 'simulator';

var
  glSimulatorPazoIdCounter: Integer = 0;

function CalculateRank(const aDestSite: TSite; const aSpeedFrom: integer; const aSection: string; const aIsPre: boolean): integer;
begin
  if (aDestSite.ReducedSpeedstatWeight) then
    Result := aSpeedFrom + aDestSite.GetRank(aSection) * 10
  else
    Result := aSpeedFrom * aDestSite.GetRank(aSection);

  if (aIsPre) then
    Result := Result + 100;
end;

constructor TSimulationResult.Create;
begin
  FSiteResults := TObjectList<TSiteSimulationResult>.Create(True);
  FRouteResults := TObjectList<TRouteSimulationResult>.Create(True);
  FTotalSites := 0;
  FAllowedSites := 0;
  FErrorMessage := '';
end;

destructor TSimulationResult.Destroy;
begin
  FSiteResults.Free;
  FRouteResults.Free;
  inherited;
end;

procedure LoadAffilRoutes(ps: TPazoSite; p: TPazo; Result: TSimulationResult);
var
  affilRoutesList: TStringList;
  i: Integer;
  sitename: String;
  speed: Integer;
  dstps: TPazoSite;
  dstps_s: TSite;
  calculatedRank: Integer;
  routeResult: TRouteSimulationResult;
begin
  affilRoutesList := TStringList.Create;
  try
    sitesdat.ReadSectionValues('affilspeed-from-' + ps.Name, affilRoutesList);
    Debug(dpSpam, rsections, 'Site %s has %d affil routes configured', [ps.Name, affilRoutesList.Count]);

    for i := 0 to affilRoutesList.Count - 1 do
    begin
      try
        sitename := affilRoutesList.Names[i];
        speed := StrToIntDef(affilRoutesList.ValueFromIndex[i], 0);

        if speed = 0 then
          Continue;

        if sitesdat.ReadInteger('speed-from-' + ps.Name, sitename, 0) > 0 then
        begin
          Debug(dpSpam, rsections, 'Skipping affil route %s -> %s (already in normal routes)', [ps.Name, sitename]);
          Continue;
        end;

        Debug(dpSpam, rsections, 'Checking affil route %s -> %s (speed: %d)', [ps.Name, sitename, speed]);

        dstps := p.FindSite(sitename);
        if (dstps <> nil) and (dstps.Name <> ps.Name) and (dstps.status = rssAllowed) then
        begin
          dstps_s := FindSiteByName('', dstps.Name);
          if dstps_s <> nil then
          begin
            calculatedRank := CalculateRank(dstps_s, speed, p.rls.section, True);
            Debug(dpSpam, rsections, 'Adding affil route %s -> %s (weight: %d, rank: %d)', [ps.Name, dstps.Name, speed, calculatedRank]);

            if ps.AddDestination(dstps, calculatedRank) then
            begin
              routeResult := TRouteSimulationResult.Create;
              routeResult.SourceSite := ps.Name;
              routeResult.DestinationSite := dstps.Name;
              routeResult.RouteWeight := speed;
              routeResult.Rank := calculatedRank;
              Result.RouteResults.Add(routeResult);
            end
            else
              Debug(dpSpam, rsections, 'AddDestination returned false for affil route %s -> %s', [ps.Name, dstps.Name]);
          end;
        end;
      except
        on e: Exception do
          Debug(dpError, rsections, '[EXCEPTION] Loading affil route %s (index %d): %s', [ps.Name, i, e.Message]);
      end;
    end;
  finally
    affilRoutesList.Free;
  end;
end;

function SimulateRelease(const aSection, aReleasename: String; const aSimulatePre: Boolean = False): TSimulationResult;
var
  rls: TRelease;
  rc: TCRelease;
  p: TPazo;
  ps: TPazoSite;
  s: TSite;
  i: Integer;
  sectiondir: String;
  siteResult: TSiteSimulationResult;
  routeResult: TRouteSimulationResult;
  ruleAction: TRuleAction;
  fSpeedInfo: TSpeedFromRouteInfo;
  dstps: TPazoSite;
  dstps_s: TSite;
  calculatedRank: Integer;
begin
  Debug(dpMessage, rsections, 'SimulateRelease started: %s in section %s', [aReleasename, aSection]);
  Result := TSimulationResult.Create;
  Result.Releasename := aReleasename;
  Result.Section := aSection;

  try
    rc := FindSectionHandler(aSection);
    if rc = nil then
    begin
      Result.ErrorMessage := Format('No section handler found for section: %s', [aSection]);
      Exit;
    end;

    try
      rls := rc.Create(aReleasename, aSection, True, 0);
    except
      on e: Exception do
      begin
        Result.ErrorMessage := Format('Failed to create release object: %s', [e.Message]);
        Exit;
      end;
    end;

    Inc(glSimulatorPazoIdCounter);
    Debug(dpSpam, rsections, 'Creating pazo with ID %d', [glSimulatorPazoIdCounter]);
    p := TPazo.Create(rls, glSimulatorPazoIdCounter);
    try

      if aSimulatePre then
        p.rls.kb_event := kbePRE
      else
        p.rls.kb_event := kbeNEWDIR;

      for i := sitesunit.sites.Count - 1 downto 0 do
      begin
        s := TSite(sitesunit.sites[i]);
        Inc(Result.FTotalSites);

        siteResult := TSiteSimulationResult.Create;
        siteResult.Sitename := s.Name;
        siteResult.Section := aSection;

        if not (s.WorkingStatus in [sstUnknown, sstUp]) then
        begin
          siteResult.Allowed := False;
          siteResult.SiteDown := True;
          siteResult.Reason := Format('Site is %s', [GetEnumName(TypeInfo(TSiteStatus), Ord(s.WorkingStatus))]);
          Result.SiteResults.Add(siteResult);
          Continue;
        end;

        if s.PermDown then
        begin
          siteResult.Allowed := False;
          siteResult.SiteDown := True;
          siteResult.Reason := 'Site is permanently down';
          Result.SiteResults.Add(siteResult);
          Continue;
        end;

        sectiondir := s.sectiondir[aSection];
        if sectiondir = '' then
        begin
          siteResult.Allowed := False;
          siteResult.HasSection := False;
          siteResult.Reason := 'Section not configured on site';
          Result.SiteResults.Add(siteResult);
          Continue;
        end;

        siteResult.HasSection := True;
        siteResult.PretimeOk := True;

        Debug(dpSpam, rsections, 'Creating TPazoSite for %s', [s.Name]);
        try
          ps := TPazoSite.Create(p, s.Name, sectiondir, s);
        except
          on e: Exception do
          begin
            Debug(dpError, rsections, '[EXCEPTION] Creating TPazoSite for %s: %s', [s.Name, e.Message]);
            siteResult.Allowed := False;
            siteResult.Reason := Format('Error creating pazo site: %s', [e.Message]);
            Result.SiteResults.Add(siteResult);
            Continue;
          end;
        end;

        Debug(dpSpam, rsections, 'TPazoSite created for %s, speed_from is %s', [s.Name, IfThen(ps.speed_from <> nil, 'not nil', 'NIL!')]);
        try
          ps.status := rssNotAllowed;

          if s.IsAffil(rls.groupname) then
          begin
            ps.status := rssShouldPre;
            siteResult.IsAffil := True;
          end;

          p.PazoSitesList.Add(ps);

          try
            ruleAction := FireRuleSet(p, ps);

            siteResult.RuleAction := GetEnumName(TypeInfo(TRuleAction), Ord(ruleAction));

            case ruleAction of
              raAllow:
                begin
                  siteResult.Allowed := True;
                  if ps.reason <> '' then
                    siteResult.Reason := ps.reason
                  else
                    siteResult.Reason := 'Allowed by rules';
                  if ps.status in [rssNotAllowed, rssNotAllowedButItsThere] then
                    ps.status := rssAllowed;
                  Inc(Result.FAllowedSites);
                end;
              raDrop:
                begin
                  siteResult.Allowed := False;
                  if ps.reason <> '' then
                    siteResult.Reason := ps.reason
                  else
                    siteResult.Reason := 'Dropped by rules';
                end;
            else
              begin
                siteResult.Allowed := False;
                if ps.reason <> '' then
                  siteResult.Reason := ps.reason
                else
                  siteResult.Reason := Format('Rule action: %s', [siteResult.RuleAction]);
              end;
            end;
          except
            on e: Exception do
            begin
              siteResult.Allowed := False;
              siteResult.Reason := Format('Error evaluating rules: %s', [e.Message]);
            end;
          end;

        finally
        end;

        Result.SiteResults.Add(siteResult);
      end;

      Debug(dpSpam, rsections, 'Building routes for %d allowed sites', [p.PazoSitesList.Count]);
      for ps in p.PazoSitesList do
      begin
        if ps.status in [rssAllowed, rssShouldPre, rssRealPre] then
        begin
          Debug(dpSpam, rsections, 'Processing routes for site %s (PRE: %s, Affil: %s)',
            [ps.Name, IfThen(aSimulatePre, 'yes', 'no'), IfThen(ps.StatusRealPreOrShouldPre, 'yes', 'no')]);

          if ps.speed_from = nil then
          begin
            Debug(dpError, rsections, 'ERROR: speed_from is nil for site %s', [ps.Name]);
            Continue;
          end;

          Debug(dpSpam, rsections, 'Site %s has %d normal routes configured', [ps.Name, ps.speed_from.Count]);

          for i := 0 to ps.speed_from.Count - 1 do
          begin
            try
              fSpeedInfo := ps.speed_from[i];
              Debug(dpSpam, rsections, 'Checking route %s -> %s (speed: %d)', [ps.Name, fSpeedInfo.Sitename, fSpeedInfo.Speed]);

              dstps := p.FindSite(fSpeedInfo.Sitename);

              if (dstps <> nil) and (dstps.Name <> ps.Name) and (dstps.status = rssAllowed) then
              begin
                if fSpeedInfo.AffilOnly and not ps.StatusRealPreOrShouldPre then
                begin
                  Debug(dpSpam, rsections, 'Skipping affil-only route %s -> %s', [ps.Name, dstps.Name]);
                  Continue;
                end;

                if fSpeedInfo.NoAffil and ps.StatusRealPreOrShouldPre then
                begin
                  Debug(dpSpam, rsections, 'Skipping no-affil route %s -> %s', [ps.Name, dstps.Name]);
                  Continue;
                end;

                dstps_s := FindSiteByName('', dstps.Name);
                if dstps_s <> nil then
                begin
                  calculatedRank := CalculateRank(dstps_s, fSpeedInfo.Speed, p.rls.section, ps.status in [rssShouldPre, rssRealPre]);

                  Debug(dpSpam, rsections, 'Adding route %s -> %s (weight: %d, rank: %d)', [ps.Name, dstps.Name, fSpeedInfo.Speed, calculatedRank]);

                  if ps.AddDestination(dstps, calculatedRank) then
                  begin
                    routeResult := TRouteSimulationResult.Create;
                    routeResult.SourceSite := ps.Name;
                    routeResult.DestinationSite := dstps.Name;
                    routeResult.RouteWeight := fSpeedInfo.Speed;
                    routeResult.Rank := calculatedRank;
                    Result.RouteResults.Add(routeResult);
                  end
                  else
                    Debug(dpSpam, rsections, 'AddDestination returned false for %s -> %s', [ps.Name, dstps.Name]);
                end
                else
                  Debug(dpError, rsections, 'ERROR: Could not find site object for %s', [dstps.Name]);
              end
              else
              begin
                if dstps = nil then
                  Debug(dpSpam, rsections, 'Destination site %s not found in pazo', [fSpeedInfo.Sitename])
                else if dstps.Name = ps.Name then
                  Debug(dpSpam, rsections, 'Skipping self-route %s -> %s', [ps.Name, dstps.Name])
                else if dstps.status <> rssAllowed then
                  Debug(dpSpam, rsections, 'Destination site %s is not allowed (status: %d)', [dstps.Name, Ord(dstps.status)]);
              end;
            except
              on e: Exception do
              begin
                Debug(dpError, rsections, '[EXCEPTION] Building route %s (index %d): %s', [ps.Name, i, e.Message]);
              end;
            end;
          end;

          if aSimulatePre and ps.StatusRealPreOrShouldPre then
          begin
            Debug(dpSpam, rsections, 'Loading affil-only routes for PRE from %s', [ps.Name]);
            LoadAffilRoutes(ps, p, Result);
          end;
        end;
      end;
      Debug(dpSpam, rsections, 'Route building completed, total routes: %d', [Result.RouteResults.Count]);

    finally
      Debug(dpSpam, rsections, 'Cleaning up pazo and release objects');
      try
        p.Free;
      except
        on e: Exception do
          Debug(dpError, rsections, '[EXCEPTION] Freeing pazo: %s', [e.Message]);
      end;
    end;

  except
    on e: Exception do
    begin
      Result.ErrorMessage := Format('Simulation error: %s', [e.Message]);
      Debug(dpError, rsections, '[EXCEPTION] SimulateRelease: %s', [e.Message]);
    end;
  end;
  Debug(dpMessage, rsections, 'SimulateRelease completed');
end;

function FormatSimulationResult(const aResult: TSimulationResult): TStringList;
var
  siteRes: TSiteSimulationResult;
  routeRes: TRouteSimulationResult;
  allowedSites, droppedSites, otherSites: TStringList;
begin
  Result := TStringList.Create;

  if aResult.ErrorMessage <> '' then
  begin
    Result.Add(Format('<c4>ERROR:</c> %s', [aResult.ErrorMessage]));
    Exit;
  end;

  Result.Add(Format('<b>Simulation Results for:</b> <c7>%s</c> (<c8>%s</c>)', [aResult.Releasename, aResult.Section]));
  Result.Add(Format('<b>Summary:</b> %d/%d sites would allow this release', [aResult.AllowedSites, aResult.TotalSites]));
  Result.Add('');

  allowedSites := TStringList.Create;
  droppedSites := TStringList.Create;
  otherSites := TStringList.Create;
  try
    for siteRes in aResult.SiteResults do
    begin
      if siteRes.Allowed then
      begin
        if siteRes.IsAffil then
          allowedSites.Add(Format('  <c9>%s</c> - %s (AFFIL)', [siteRes.Sitename, siteRes.Reason]))
        else
          allowedSites.Add(Format('  <c9>%s</c> - %s', [siteRes.Sitename, siteRes.Reason]));
      end
      else if siteRes.SiteDown then
      begin
        otherSites.Add(Format('  <c8>%s</c> - %s', [siteRes.Sitename, siteRes.Reason]));
      end
      else if not siteRes.HasSection then
      begin
        otherSites.Add(Format('  <c8>%s</c> - %s', [siteRes.Sitename, siteRes.Reason]));
      end
      else
      begin
        droppedSites.Add(Format('  <c4>%s</c> - %s', [siteRes.Sitename, siteRes.Reason]));
      end;
    end;

    if allowedSites.Count > 0 then
    begin
      Result.Add(Format('<b><c9>ALLOWED Sites (%d):</c></b>', [allowedSites.Count]));
      Result.AddStrings(allowedSites);
      Result.Add('');
    end;

    if droppedSites.Count > 0 then
    begin
      Result.Add(Format('<b><c4>DROPPED Sites (%d):</c></b>', [droppedSites.Count]));
      Result.AddStrings(droppedSites);
      Result.Add('');
    end;

    if otherSites.Count > 0 then
    begin
      Result.Add(Format('<b><c8>OTHER Sites (%d):</c></b>', [otherSites.Count]));
      Result.AddStrings(otherSites);
      Result.Add('');
    end;

  finally
    allowedSites.Free;
    droppedSites.Free;
    otherSites.Free;
  end;

  if aResult.RouteResults.Count > 0 then
  begin
    Result.Add(Format('<b>ROUTES (%d):</b>', [aResult.RouteResults.Count]));
    for routeRes in aResult.RouteResults do
    begin
      Result.Add(Format('  <c7>%s</c> -> <c7>%s</c> (weight: %d, rank: %d)',
        [routeRes.SourceSite, routeRes.DestinationSite, routeRes.RouteWeight, routeRes.Rank]));
    end;
  end
  else if aResult.AllowedSites > 0 then
  begin
    Result.Add('<b>ROUTES:</b> <c4>No routes configured between allowed sites</c>');
  end;
end;

procedure SimulatorInit;
begin
  Debug(dpSpam, rsections, 'SimulatorInit');
end;

procedure SimulatorUninit;
begin
  Debug(dpSpam, rsections, 'SimulatorUninit');
end;

end.
