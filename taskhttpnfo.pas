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
    function Name: string; override;
  end;

implementation

uses SysUtils, irc, debugunit, dateutils, tags,
     configunit, dirlist, dbaddnfo, slhttp;

const
  section = 'taskhttpnfo';

{ TPazoHTTPNfoTask }

constructor TPazoHTTPNfoTask.Create(const nfo_rls, nfo_url, nfo_name: String);
begin
  self.nfo_rls:= nfo_rls;
  self.nfo_url:= nfo_url;
  self.nfo_name:= nfo_name;
  inherited Create('', '', config.ReadString('sites', 'admin_sitename', 'SLFTP'));
end;

function TPazoHTTPNfoTask.Execute(slot: Pointer): Boolean;
var nfo_data: String;
begin
//  Result:= False;

  try
    nfo_data:= slUrlGet(nfo_url);
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TPazoHTTPNfoTask slUrlGet: Exception : %s', [e.Message]));
      Result:= True;
      ready:= True;
      exit;
    end;
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

function TPazoHTTPNfoTask.Name: string;
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
