unit tasksearchrelease;

interface

uses
  taskraw;

type
  TSearchReleaseTask = class(TRawTask)
  private
    FReleaseName: String;
    FAnnounceCWDFailure: boolean;
  public
    constructor Create(const aNetname, aChannel, aSite, aRls: String; const aAnnounceCWDFailure: boolean);
    function Name: String; override;
    function Execute(slot: Pointer): Boolean; override;
  end;

implementation

uses
  sitesunit, SysUtils, mystrings, DebugUnit, irc, Classes;

const
  section = 'raw';

{ TRawTask }

constructor TSearchReleaseTask.Create(const aNetname, aChannel, aSite, aRls: String; const aAnnounceCWDFailure: boolean);
begin
  inherited Create(aNetname, aChannel, aSite, '/', 'SITE SEARCH ' + aRls);
  self.FReleaseName := aRls;
  self.FAnnounceCWDFailure := aAnnounceCWDFailure;
end;

function TSearchReleaseTask.Execute(slot: Pointer): Boolean;
var
  fFoundPaths: TStringList;
  fRawResponse, fPath: String;
  fSlot: TSiteSlot;
begin
  Result := inherited Execute(slot);
  fRawResponse := self.response;
  fSlot := slot;

  if Result then
  begin
    fFoundPaths := TStringList.Create;
    try
      fFoundPaths.Text := ParsePathFromSiteSearchResult(self.response, self.FReleaseName);

      // now check if the directory is actually there by trying to CWD into it
      self.response := '';
      for fPath in fFoundPaths do
      begin
        if fSlot.Cwd(fPath, true) then
        begin
          self.response := self.response + fPath + #13#10;
        end
        else
        begin
          if FAnnounceCWDFailure then
            irc_Addadmin(Format('<c8>[SITESEARCH]</c> Cannot CWD into path on %s: %s (%s)', [site1, fPath, fSlot.lastResponse]));
        end;
      end;
    finally
      fFoundPaths.Free;
    end;
  end;
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
