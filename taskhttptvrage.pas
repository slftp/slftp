unit taskhttptvrage;

interface

uses Classes, pazo, tasksunit, sltcp;

type
  TPazoHTTPTVRageTask = class(TTask)
  private
    rls: String;
    tv_showid: String;
  public
    constructor Create(const tv_showid: String; rls : String = '');
    destructor Destroy; override;
    function Execute(slot: Pointer): Boolean; override;
    function Name: string; override;
  end;

implementation

uses SysUtils, irc, StrUtils, kb, debugunit, dateutils, queueunit, tags,
     configunit, dirlist, mystrings, sitesunit, leechfileunit, console,
     slhttp, regexpr, dbaddtvrage, Contnrs;

const
  section = 'taskhttpimdb';

{ TPazoHTTPTVRageTask }

constructor TPazoHTTPTVRageTask.Create(const tv_showid: String; rls : String = '');
begin
  self.tv_showid:= tv_showid;
  self.rls := rls;
  inherited Create('', '', config.ReadString('sites', 'admin_sitename', 'SLFTP'));
end;

function TPazoHTTPTVRageTask.Execute(slot: Pointer): Boolean;
var tvrage: TDbTVRage;
    uurl: String;
xx,    x:TRegExpr;
begin
  Result:=False;

  tvrage:= TDbTVRage.Create(tv_showid);

  uurl:='sid='+tv_showid;
  response:= slUrlGet('http://services.tvrage.com/tools/quickinfo.php', uurl);

  x:=TRegexpr.Create;
  x.ModifierI:=True;
  x.ModifierM:=True;

try

  {###Read  ShowID  ###}
  x.Expression:='Show ID\@(\d{4,7})$';
  if x.Exec(response) then
    tvrage.tv_showid:= x.Match[1];

  {###Read  ShowName  ###}
  x.Expression:='^Show Name\@(.*?)$';
  if x.Exec(response) then
    tvrage.tv_showname:=x.Match[1];

  {###Read  ShowURL  ###}
  x.Expression:='^Show URL\@(.*?)$';
  if x.Exec(response) then
    tvrage.tv_showurl:=x.Match[1];

  {###Read  ShowPremiered  ###}
  x.Expression:='^Premiered\@(\d{4})$';
  if x.Exec(response) then
    tvrage.tv_premiered_year:=strtoint(x.Match[1]);

	{###Read  ShowEnded  ###}
	x.Expression:='^Ended\@\w+\/(\d{4})$';
    tvrage.tv_running:=False;
    tvrage.tv_endedyear:=-1;
    if x.Exec(response) then begin
    tvrage.tv_running:=True;
    tvrage.tv_endedyear:=strtointdef(x.Match[1],-1);
    end;

  {###Read  ShowCountry  ###}
  x.Expression:='^Country\@(.*?)$';
  if x.Exec(response) then
    tvrage.tv_country:=x.Match[1];

  {###Read  ShowStatusAsString  ###}
  x.Expression:='^Status\@(.*?)$';
  if x.Exec(response) then
    tvrage.tv_status:=x.Match[1];

  {###Read  ShowClassification  ###}
  x.Expression:='^Classification\@(.*?)$';
  if x.Exec(response) then
    tvrage.tv_classification:=x.Match[1];

  {###Read  ShowGenres  ###}
  x.Expression:='^Genres\@(.*?)$';
  if x.Exec(response) then
  splitString(x.Match[1],'|',tvrage.tv_genres);
//tvrage.tv_genres.DelimitedText:=x.Match[1];

  {###Read  ShowNetwork  ###}
  x.Expression:='^Network\@(.*?)$';
  if x.Exec(response) then
    tvrage.tv_network:=x.Match[1];

   {###Read  ShowRuntime  ###}
  x.Expression:='^Runtime\@(.*?)$';
  if x.Exec(response) then
    tvrage.tv_runtime:=StrToIntDef(x.Match[1], 0);

finally
  x.free;
end;



  try
    dbaddtvrage_SaveTVRage(tv_showid, tvrage, rls);
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] TPazoHTTPTVRageTask dbaddimdb_SaveTVRage: %s ', [e.Message]));
      irc_Adderror(Format('<c4>[EXCEPTION]</c> TPazoHTTPTVRageTask dbaddimdb_SaveTVRage: %s', [e.Message]));
    end;
  end;

  ready:= True;
  Result:= True;
end;

function TPazoHTTPTVRageTask.Name: string;
begin
  try
    Result:=Format('HTTPTVRage : %s',[tv_showid]);
  except
    Result:= 'HTTPTVRage';
  end;
end;

destructor TPazoHTTPTVRageTask.Destroy;
begin
  inherited;
end;

end.
