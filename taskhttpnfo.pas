unit taskhttpnfo;

interface

uses Classes, tasksunit, sltcp;

type
  TPazoHTTPNfoTask = class(TTask)
  private
    nfo_rls: String;
    nfo_url: String;
    nfo_name: String;
  public
    constructor Create(const nfo_rls, nfo_url, nfo_name: String);
    destructor Destroy; override;
    function Execute(slot: Pointer): Boolean; override;
    function Name: String; override;
  end;

implementation

uses
  SysUtils, irc, debugunit, dateutils, dirlist, dbaddnfo, http, sitesunit;

const
  section = 'taskhttpnfo';

{ TPazoHTTPNfoTask }

constructor TPazoHTTPNfoTask.Create(const nfo_rls, nfo_url, nfo_name: String);
begin
  self.nfo_rls:= nfo_rls;
  self.nfo_url:= nfo_url;
  self.nfo_name:= nfo_name;
  inherited Create('', '', getAdminSiteName);
end;

function TPazoHTTPNfoTask.Execute(slot: Pointer): Boolean;
var
  nfo_data: String;
  fHttpGetErrMsg: String;
begin
  Result:= False;

  if not HttpGetUrl(nfo_url, nfo_data, fHttpGetErrMsg) then
  begin
    Debug(dpError, section, Format('[FAILED] TPazoHTTPNfoTask for %s --> %s ', [nfo_rls, fHttpGetErrMsg]));
    irc_Adderror(Format('<c4>[FAILED]</c> TPazoHTTPNfoTask for %s --> %s', [nfo_rls, fHttpGetErrMsg]));
    Result := True;
    ready := True;
    exit;
  end;

  if (length(nfo_data) < 10) then
  begin
    Result:= True;
    ready:= True;
    irc_Adderror(Format('<c7>[WARNING]</c> NFO Size (%d) not enought %s URL : %s', [length(nfo_data), nfo_rls, nfo_url]));
    exit;
  end;
  try
    dbaddnfo_SaveNfo(nfo_rls, nfo_name, nfo_data);
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TPazoHTTPNfoTask: Exception : %s', [e.Message]));
      Result:= True;
      ready:= True;
      exit;
    end;
  end;
  Result:= True;
  ready:= True;
end;

function TPazoHTTPNfoTask.Name: String;
begin
  try
    Result:= Format('HTTPNfo %s : %s',[nfo_url, nfo_name]);
  except
    Result:= 'HTTPNfo';
  end;
end;

destructor TPazoHTTPNfoTask.Destroy;
begin
  inherited;
end;

end.
