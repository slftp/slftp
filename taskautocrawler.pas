unit taskautocrawler;

interface

uses tasksunit;

type
  TAutoCrawlerTask = class(TTask)
    constructor Create(const netname, channel: string; site1: string); overload;
    constructor Create(const netname, channel: string; site1, section: string; datum: TDateTime); overload;
  end;

procedure AutoCrawlerInit;
procedure AutoCrawlerUnInit;
procedure AutoCrawlerStart;
procedure AutoCrawlerStop;

var crawler_enabled: Boolean = False;  
    confirmer_announce: Boolean = False;  
  
implementation

constructor TAutoCrawlerTask.Create(const netname, channel: string;
  site1: string);
begin
end;
constructor TAutoCrawlerTask.Create(const netname, channel: string; site1,
  section: string; datum: TDateTime);
begin
end;
procedure AutoCrawlerInit;
begin
end;
procedure AutoCrawlerUnInit;
begin
end;
procedure AutoCrawlerStart;
begin
end;
procedure AutoCrawlerStop;
begin
end;

end.
