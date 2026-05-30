unit taskspeedtest;

interface

uses
  tasksunit;

type
  TDelSpeedtestFileTask = class(TTask)
    constructor Create(const netname, channel, site: String);
    function Execute(slot: Pointer): Boolean; override;
    function Name: String; override;
  end;

  TUploadSpeedtestFileTask = class(TTask)
  public
    constructor Create(const netname, channel, site: String);
    destructor Destroy; override;
    function Execute(slot: Pointer): Boolean; override;
    function Name: String; override;
  end;

procedure SpeedTestInit;

var
  speedtestfilename: String;

implementation

procedure SpeedTestInit;
begin
end;

{ TDelSpeedtestFileTask }

constructor TDelSpeedtestFileTask.Create(const netname, channel, site: String);
begin
  inherited Create(netname, channel, site);
end;

function TDelSpeedtestFileTask.Execute(slot: Pointer): Boolean;
begin
  ready := True;
  Result := True;
end;

function TDelSpeedtestFileTask.Name: String;
begin
  Result := 'DELSPEEDTESTFILE';
end;

{ TUploadSpeedtestFileTask }

constructor TUploadSpeedtestFileTask.Create(const netname, channel, site: String);
begin
  inherited Create(netname, channel, site);
end;

destructor TUploadSpeedtestFileTask.Destroy;
begin
  inherited;
end;

function TUploadSpeedtestFileTask.Execute(slot: Pointer): Boolean;
begin
  ready := True;
  Result := True;
end;

function TUploadSpeedtestFileTask.Name: String;
begin
  Result := 'UPLOADSPEEDTESTFILE';
end;

end.