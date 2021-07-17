unit tasksearchrelease;

interface

uses
  taskraw;

type
  TSearchReleaseTask = class(TRawTask)
  private
    FReleaseName: String;
  public
    constructor Create(const aNetname, aChannel, aSite, aRls: String);
    function Name: String; override;
  end;

implementation

uses
  sitesunit, SysUtils, mystrings, DebugUnit;

const
  section = 'raw';

{ TRawTask }

constructor TSearchReleaseTask.Create(const aNetname, aChannel, aSite, aRls: String);
begin
  inherited Create(aNetname, aChannel, aSite, '/', 'SITE SEARCH ' + aRls);
  self.FReleaseName := aRls;
end;

function TSearchReleaseTask.Name: String;
begin
  try
    Result := Format('SEARCH RLS %s (%s)', [site1, FReleaseName]);
  except
    Result := 'SEARCH RLS';
  end;
end;

end.
