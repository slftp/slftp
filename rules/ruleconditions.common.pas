unit ruleconditions.common;

interface

uses
  Classes, pazo, rulesunit;

type
  TConditionReleaseName = class(TStringCondition)
    function SupplyValue(r: TPazo): String; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionSection = class(TStringCondition)
    function SupplyValue(r: TPazo): String; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionInternal = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionAge = class(TIntCondition)
    function SupplyValue(r: TPazo): integer; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionComplete = class(TAtCondition)
    procedure SupplyValues(r: TPazo; re: TStringList); override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionNotComplete = class(TAtCondition)
    procedure SupplyValues(r: TPazo; re: TStringList); override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionPre = class(TAtCondition)
    procedure SupplyValues(r: TPazo; re: TStringList); override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionAllowed = class(TAtCondition)
    procedure SupplyValues(r: TPazo; re: TStringList); override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionNotAllowed = class(TAtCondition)
    procedure SupplyValues(r: TPazo; re: TStringList); override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionGroup = class(TStringCondition)
    function SupplyValue(r: TPazo): String; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionFake = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionForeign = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionLanguage = class(TMultiStringCondition)
    procedure SupplyValues(r: TPazo; re: TStringList); override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionYear = class(TIntCondition)
    function SupplyValue(r: TPazo): integer; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionCurrentYear = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionKnownGroup = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionUnKnownGroup = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionSource = class(TStringCondition)
    function SupplyValue(r: TPazo): String; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionDestination = class(TStringCondition)
    function SupplyValue(r: TPazo): String; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionCompleteSource = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionNewdirSource = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionNuked = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionTag = class(TMultiStringCondition)
    procedure SupplyValues(r: TPazo; re: TStringList); override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionDisks = class(TIntCondition)
    function SupplyValue(r: TPazo): integer; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionAutofollow = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionPred = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

  TConditionDefault = class(TBooleanCondition)
    function SupplyValue(r: TPazo): boolean; override;
    class function Name: String; override;
    class function Description: String; override;
  end;

implementation

uses
  SysUtils, Contnrs, DateUtils, kb, debugunit, knowngroups;

const
  dsection = 'rules.common';

{$I ruleconditions.common.inc}

{ TConditionReleaseName }

function TConditionReleaseName.SupplyValue(r: TPazo): String;
begin
  try
    Result := r.rls.rlsname;
  except
    Result := '';
  end;
end;

class function TConditionReleaseName.Name: String;
begin
  Result := 'releasename';
end;

class function TConditionReleaseName.Description: String;
begin
  Result := ReleaseNameDescription;
end;

{ TConditionSection }

function TConditionSection.SupplyValue(r: TPazo): String;
begin
  try
    Result := r.rls.section;
  except
    Result := '';
  end;
end;

class function TConditionSection.Name: String;
begin
  Result := 'section';
end;

class function TConditionSection.Description: String;
begin
  Result := SectionDescription;
end;

{ TConditionInternal }

function TConditionInternal.SupplyValue(r: TPazo): boolean;
begin
  try
    Result := r.rls.internal;
  except
    Result := False;
  end;
end;

class function TConditionInternal.Name: String;
begin
  Result := 'internal';
end;

class function TConditionInternal.Description: String;
begin
  Result := InternalDescription;
end;

{ TConditionAge }

function TConditionAge.SupplyValue(r: TPazo): integer;
begin
  try
    Result := r.Age;
  except
    Result := 0;
  end;
end;

class function TConditionAge.Name: String;
begin
  Result := 'age';
end;

class function TConditionAge.Description: String;
begin
  Result := AgeDescription;
end;

{ TConditionComplete }

procedure TConditionComplete.SupplyValues(r: TPazo; re: TStringList);
var
  ps: TPazoSite;
  i: integer;
begin
  try
    for i := 0 to r.sites.Count - 1 do
    begin
      if i > r.sites.Count then
        Break;
      try
        ps := TPazoSite(r.sites[i]);
        if ps.Complete then
          re.Add(ps.Name);
      except
        Continue;
      end;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, dsection, Format('[EXCEPTION] TConditionComplete.GetSupplyValues: %s', [e.Message]));
      re.Clear;
      exit;
    end;
  end;
end;

class function TConditionComplete.Name: String;
begin
  Result := 'complete';
end;

class function TConditionComplete.Description: String;
begin
  Result := CompleteDescription;
end;

{ TConditionNotComplete }

procedure TConditionNotComplete.SupplyValues(r: TPazo; re: TStringList);
var
  ps: TPazoSite;
  i: integer;
begin
  try
    for i := 0 to r.sites.Count - 1 do
    begin
      if i > r.sites.Count then
        Break;
      try
        ps := TPazoSite(r.sites[i]);
        if ((ps.status <> rssNotAllowed) and (not ps.Complete)) then
          re.Add(ps.Name);
      except
        Continue;
      end;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, dsection, Format('[EXCEPTION] TConditionNotComplete.GetSupplyValues: %s', [e.Message]));
      re.Clear;
      exit;
    end;
  end;
end;

class function TConditionNotComplete.Name: String;
begin
  Result := 'notcomplete';
end;

class function TConditionNotComplete.Description: String;
begin
  Result := NotCompleteDescription;
end;

{ TConditionPre }

procedure TConditionPre.SupplyValues(r: TPazo; re: TStringList);
var
  ps: TPazoSite;
  i: integer;
begin
  try
    for i := 0 to r.sites.Count - 1 do
    begin
      if i > r.sites.Count then
        Break;
      try
        ps := TPazoSite(r.sites[i]);
        if ps.status = rssRealPre then
          re.Add(ps.Name);
      except
        Continue;
      end;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, dsection, Format('[EXCEPTION] TConditionPre.GetSupplyValues: %s', [e.Message]));
      re.Clear;
      exit;
    end;
  end;
end;

class function TConditionPre.Name: String;
begin
  Result := 'pre';
end;

class function TConditionPre.Description: String;
begin
  Result := PreDescription;
end;

{ TConditionAllowed }

procedure TConditionAllowed.SupplyValues(r: TPazo; re: TStringList);
var
  ps: TPazoSite;
  i: integer;
begin
  try
    for i := 0 to r.sites.Count - 1 do
    begin
      if i > r.sites.Count then
        Break;
      try
        ps := TPazoSite(r.sites[i]);
        if ps.status = rssAllowed then
          re.Add(ps.Name);
      except
        Continue;
      end;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, dsection, Format('[EXCEPTION] TConditionAllowed.GetSupplyValues: %s', [e.Message]));
      re.Clear;
      exit;
    end;
  end;
end;

class function TConditionAllowed.Name: String;
begin
  Result := 'allowed';
end;

class function TConditionAllowed.Description: String;
begin
  Result := AllowedDescription;
end;

{ TConditionNotAllowed }

procedure TConditionNotAllowed.SupplyValues(r: TPazo; re: TStringList);
var
  ps: TPazoSite;
  i: integer;
begin
  try
    for i := 0 to r.sites.Count - 1 do
    begin
      if i > r.sites.Count then
        Break;
      try
        ps := TPazoSite(r.sites[i]);
        if ps.status = rssNotAllowed then
          re.Add(ps.Name);
      except
        Continue;
      end;
    end;
  except
    on E: Exception do
    begin
      Debug(dpError, dsection, Format('[EXCEPTION] TConditionNotAllowed.GetSupplyValues: %s', [e.Message]));
      re.Clear;
      exit;
    end;
  end;
end;

class function TConditionNotAllowed.Name: String;
begin
  Result := 'notallowed';
end;

class function TConditionNotAllowed.Description: String;
begin
  Result := NotAllowedDescription;
end;

{ TConditionGroup }

function TConditionGroup.SupplyValue(r: TPazo): String;
begin
  try
    Result := r.rls.groupname;
  except
    Result := '';
  end;
end;

class function TConditionGroup.Name: String;
begin
  Result := 'group';
end;

class function TConditionGroup.Description: String;
begin
  Result := GroupDescription;
end;

{ TConditionFake }

function TConditionFake.SupplyValue(r: TPazo): boolean;
begin
  try
    Result := r.rls.Fake;
  except
    Result := False;
  end;
end;

class function TConditionFake.Name: String;
begin
  Result := 'fake';
end;

class function TConditionFake.Description: String;
begin
  Result := FakeDescription;
end;

{ TConditionForeign }

function TConditionForeign.SupplyValue(r: TPazo): boolean;
begin
  Result := False;

  try
    if r.rls is TMP3Release then
      exit;
    if r.rls.languages.Count = 0 then
      exit;
    if (r.rls.languages[0] = 'English') then
      exit;

    Result := True;
  except
    Result := False;
  end;
end;

class function TConditionForeign.Name: String;
begin
  Result := 'foreign';
end;

class function TConditionForeign.Description: String;
begin
  Result := ForeignDescription;
end;

{ TConditionLanguageA }

procedure TConditionLanguage.SupplyValues(r: TPazo; re: TStringList);
begin
  try
    re.Assign(r.rls.languages);
  except
    on E: Exception do
    begin
      Debug(dpError, dsection, Format('[EXCEPTION] TConditionLanguage.GetSupplyValues: %s', [e.Message]));
      re.Clear;
      exit;
    end;
  end;
end;

class function TConditionLanguage.Name: String;
begin
  Result := 'language';
end;

class function TConditionLanguage.Description: String;
begin
  Result := LanguageDescription;
end;

{ TConditionYear }

function TConditionYear.SupplyValue(r: TPazo): integer;
begin
  try
    Result := r.rls.year;
  except
    Result := 0;
  end;
end;

class function TConditionYear.Name: String;
begin
  Result := 'year';
end;

class function TConditionYear.Description: String;
begin
  Result := YearDescription;
end;

{ TConditionCurrentYear }

function TConditionCurrentYear.SupplyValue(r: TPazo): boolean;
begin
  Result := (r.rls.year = r.rls.CurrentYear);
end;

class function TConditionCurrentYear.Name: String;
begin
  Result := 'currentyear';
end;

class function TConditionCurrentYear.Description: String;
begin
  Result := CurrentYearDescription;
end;

{ TConditionKnownGroup }

function TConditionKnownGroup.SupplyValue(r: TPazo): boolean;
begin
  try
    Result := r.rls.knowngroup = grp_known;
  except
    Result := False;
  end;
end;

class function TConditionKnownGroup.Name: String;
begin
  Result := 'knowngroup';
end;

class function TConditionKnownGroup.Description: String;
begin
  Result := KnownGroupDescription;
end;

{ TConditionUnKnownGroup }

function TConditionUnKnownGroup.SupplyValue(r: TPazo): boolean;
begin
  try
    Result := r.rls.knowngroup = grp_unknown;
  except
    Result := False;
  end;
end;

class function TConditionUnKnownGroup.Name: String;
begin
  Result := 'unknowngroup';
end;

class function TConditionUnKnownGroup.Description: String;
begin
  Result := UnKnownGroupDescription;
end;

{ TConditionSource }

function TConditionSource.SupplyValue(r: TPazo): String;
begin
  Result := r.srcsite;
end;

class function TConditionSource.Name: String;
begin
  Result := 'source';
end;

class function TConditionSource.Description: String;
begin
  Result := SourceDescription;
end;

{ TConditionDestination }

function TConditionDestination.SupplyValue(r: TPazo): String;
begin
  Result := r.dstsite;
end;

class function TConditionDestination.Name: String;
begin
  Result := 'destination';
end;

class function TConditionDestination.Description: String;
begin
  Result := DestinationDescription;
end;

{ TConditionCompleteSource }

function TConditionCompleteSource.SupplyValue(r: TPazo): boolean;
var
  x: TPazoSite;
begin
  Result := False;
  try
    x := r.FindSite(r.srcsite);
    if (x <> nil) then
      Result := x.Complete;
  except
    Result := False;
  end;
end;

class function TConditionCompleteSource.Name: String;
begin
  Result := 'completesource';
end;

class function TConditionCompleteSource.Description: String;
begin
  Result := CompleteSourceDescription;
end;

{ TConditionNewdirSource }

function TConditionNewdirSource.SupplyValue(r: TPazo): boolean;
var
  x: TPazoSite;
begin
  Result := False;
  try
    x := r.FindSite(r.srcsite);
    if (x <> nil) then
      Result := x.status = rssAllowed;
  except
    Result := False;
  end;
end;

class function TConditionNewdirSource.Name: String;
begin
  Result := 'newdirsource';
end;

class function TConditionNewdirSource.Description: String;
begin
  Result := NewdirSourceDescription;
end;

{ TConditionNuked }

function TConditionNuked.SupplyValue(r: TPazo): boolean;
var
  ps: TPazoSite;
begin
  Result := False;
  try
    ps := r.FindSite(r.dstsite);
    if ps <> nil then
    begin
      if ps.status = rssNuked then
        Result := True;
    end;
  except
    Result := False;
  end;
end;

class function TConditionNuked.Name: String;
begin
  Result := 'nuked';
end;

class function TConditionNuked.Description: String;
begin
  Result := NukedDescription;
end;

{ TConditionTag }

procedure TConditionTag.SupplyValues(r: TPazo; re: TStringList);
begin
  try
    re.Assign(r.rls.words);
  except
    on E: Exception do
    begin
      Debug(dpError, dsection, Format('[EXCEPTION] TConditionTag.GetSupplyValues: %s', [e.Message]));
      re.Clear;
      exit;
    end;
  end;
end;

class function TConditionTag.Name: String;
begin
  Result := 'tag';
end;

class function TConditionTag.Description: String;
begin
  Result := TagDescription;
end;

{ TConditionDisks }

function TConditionDisks.SupplyValue(r: TPazo): integer;
begin
  try
    Result := r.rls.disks;
  except
    Result := 1;
  end;
end;

class function TConditionDisks.Name: String;
begin
  Result := 'discs';
end;

class function TConditionDisks.Description: String;
begin
  Result := DisksDescription;
end;

{ TConditionAutofollow }

function TConditionAutofollow.SupplyValue(r: TPazo): boolean;
var
  ps: TPazoSite;
begin
  Result := False;
  try
    ps := r.FindSite(r.dstsite);
    if ps <> nil then
    begin
      Result := ps.ircevent;
      if ((Result) and (not ps.Complete)) then
        ps.firesourcesinstead := True;
    end;
  except
    Result := False;
  end;
end;

class function TConditionAutofollow.Name: String;
begin
  Result := 'autofollow';
end;

class function TConditionAutofollow.Description: String;
begin
  Result := AutofollowDescription;
end;

{ TConditionPred }

function TConditionPred.SupplyValue(r: TPazo): boolean;
begin
  try
    Result := r.rls.PredOnAnySite;
  except
    Result := False;
  end;
end;

class function TConditionPred.Name: String;
begin
  Result := 'pred';
end;

class function TConditionPred.Description: String;
begin
  Result := PredDescription;
end;

{ TConditionDefault }

function TConditionDefault.SupplyValue(r: TPazo): boolean;
begin
  Result := True;
end;

class function TConditionDefault.Name: String;
begin
  Result := 'default';
end;

class function TConditionDefault.Description: String;
begin
  Result := DefaultDescription;
end;

end.
