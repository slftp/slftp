unit tasksitesfv;

interface

uses
  Classes, pazo, taskrace;

type
  TPazoSiteSfvTask = class(TPazoPlainTask)
  private
    FAttempt: Integer;
    FDir, FSFVFilename: String;
    FInitialTaskCreationTime: TDateTime;
  public
    constructor Create(const netname, channel, site: String; pazo: TPazo; const aDir, aSFVFilename: String; const aAttempt: Integer; const aInitialTaskCreationTime: TDateTime); overload;
    constructor Create(const netname, channel, site: String; pazo: TPazo; const aDir, aSFVFilename: String; const aAttempt: Integer); overload;
    function Execute(slot: Pointer): boolean; override;
    function Name: String; override;
    property Dir: String read FDir;
    property SFVFilename: String read FSFVFilename;
  end;

implementation

uses
  SysUtils;

{ TPazoSiteSfvTask }

constructor TPazoSiteSfvTask.Create(const netname, channel, site: String; pazo: TPazo; const aDir, aSFVFilename: String; const aAttempt: Integer);
begin
  Create(netname, channel, site, pazo, aDir, aSFVFilename, aAttempt, Now());
end;

constructor TPazoSiteSfvTask.Create(const netname, channel, site: String; pazo: TPazo; const aDir, aSFVFilename: String; const aAttempt: Integer; const aInitialTaskCreationTime: TDateTime);
begin
  self.FAttempt := aAttempt;
  self.FDir := aDir;
  self.FSFVFilename := aSFVFilename;
  self.FInitialTaskCreationTime := aInitialTaskCreationTime;
  inherited Create(netname, channel, site, '', pazo);
end;

function TPazoSiteSfvTask.Execute(slot: Pointer): boolean;
begin
  ready := True;
  Result := True;
end;

function TPazoSiteSfvTask.Name: String;
begin
  Result := 'SITESFV';
end;

end.
