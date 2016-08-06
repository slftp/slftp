unit taskautocrawler;

interface

uses tasksunit;

type
  TAutoCrawlerTask = class(TTask)
    constructor Create(const netname, channel: AnsiString; site1: AnsiString); overload;
    constructor Create(const netname, channel: AnsiString; site1, section: AnsiString; datum: TDateTime); overload;
  end;

procedure AutoCrawlerInit;
procedure AutoCrawlerUnInit;
procedure AutoCrawlerStart;
procedure AutoCrawlerStop;

var crawler_enabled: Boolean = False;  
    confirmer_announce: Boolean = False;  
  
implementation

constructor TAutoCrawlerTask.Create(const netname, channel: AnsiString;
  site1: AnsiString);
begin
end;
constructor TAutoCrawlerTask.Create(const netname, channel: AnsiString; site1,
  section: AnsiString; datum: TDateTime);
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
