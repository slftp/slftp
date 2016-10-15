unit slhttp;

interface

uses Classes, sltcp;

type
  TslHTTP = class(TslTCPSocket)
  private
    procedure Cleanup;
    function GetProxyName: AnsiString;
    procedure SetProxyName(value: AnsiString);
  public
    CustomHeaders: TStringList;
    ResponseHeaders: TStringList;
    ResponseCode: Integer;
    ResponseStartsAt: Integer;
    Response: TStringStream;
    ContentLength: Integer;

    Timeout: Integer;
    constructor Create;
    destructor Destroy; override;
    function Get(const url: AnsiString): Boolean; overload;
    function Get(const url: AnsiString; params: TStringList; output: TStream = nil):
      Boolean; overload;
    function Post(const url: AnsiString; content: AnsiString): Boolean; overload;
    function Post(const url: AnsiString; content: TStringList): Boolean; overload;

    property ProxyName: AnsiString read GetProxyName write SetProxyName;
  end;

function slUrlGet(url: AnsiString): AnsiString; overload;
function slUrlGet(url, params: AnsiString): AnsiString; overload;

implementation

uses SysUtils, slhelper, socks5, configunit, debugunit,regexpr, irc;

const
  section = 'http';

function slUrlGet(url: AnsiString): AnsiString;
begin
  Result := slUrlGet(url, '');
end;

function slUrlGet(url, params: AnsiString): AnsiString;
var
  y: TStringList;
begin
  Result := '';
  try
    y := TStringList.Create;
    try
      y.Text := params;
      with TslHTTP.Create do
      begin
        if Get(url, y) then
          Result := Response.ReadString(Response.Size); // read all
        Free;
      end;
    finally
      y.Free;
    end;
  except
    on e: Exception do
    begin
      Debug(dpError, section, Format('[EXCEPTION] slUrlGet %s : %s', [url, e.Message]));
    end;
  end;
end;

{ TslHTTP }

procedure TslHTTP.Cleanup;
begin
  ResponseCode := 0;
  Response.Size := 0;
  ResponseHeaders.Clear;
  ResponseStartsAt := 0;

end;

constructor TslHTTP.Create;
begin
  //Timeout:= slDefaultTimeout;
  Timeout := 30000;
  Response := TStringStream.Create('');
  CustomHeaders := TStringList.Create;
  ResponseHeaders := TStringList.Create;

  (*
 if ((ProxyName = '!!NOIN!!') or (ProxyName = '0') or (ProxyName = '')) then
 SetupSocks5(self, config.ReadBool(section, 'socks5', False)) else
 mSLSetupSocks5(proxyname,self, True);
*)
//  SetupSocks5(self, config.ReadBool(section, 'socks5', False));

  inherited Create();

  // SetupSocks5(self, True);
end;

destructor TslHTTP.Destroy;
begin
  Response.Free;
  CustomHeaders.Free;
  ResponseHeaders.Free;

  inherited;
end;

function TslHTTP.GetProxyName: AnsiString;
begin
  Result := '';
end;

procedure TslHTTP.SetProxyName(value: AnsiString);
begin
  //
end;

function TslHTTP.Get(const url: AnsiString; params: TStringList; output: TStream =
  nil): Boolean;
const
  headernekszantresz = 8192;
var
rx:TRegexpr;
sss,  s, ss, paramsstr: AnsiString;
  uri: AnsiString;
  i: Integer;
  olvass: Integer;
  httpsmode: Boolean;
  get_count: Integer;
begin
  Result := False;
  Cleanup;

  paramsstr := '';

  if params <> nil then
  begin
    for i := 0 to params.Count - 1 do
    begin
      if i <> 0 then
        paramsstr := paramsstr + '&';
      paramsstr := paramsstr + UrlEncode(params.Names[i]) + '=' +
        UrlEncode(params.ValueFromIndex[i],true);
    end;
  end;

  if (1 = Pos('https://', url)) then
  begin
    httpsmode := True;
    host := Copy(url, 9, 1000);
  end
  else if (1 = Pos('http://', url)) then
  begin
    httpsmode := False;
    host := Copy(url, 8, 1000);
  end
  else
    exit;

  i := Pos('/', host);
  if i <> 0 then
  begin
    uri := Copy(host, i, 1000);
    host := Copy(host, 1, i - 1);
  end
  else
    uri := '/';

  if paramsstr <> '' then
    uri := uri + '?' + paramsstr;

  i := Pos(':', host);
  if i <> 0 then
  begin
    s := Copy(host, i + 1, 1000);
    port := StrToInt(s);
    Delete(host, i, length(s) + 1);
  end
  else
  begin
    if httpsmode then
      Port := 443
    else
      Port := 80;
  end;

  if not Connect(Timeout) then
    exit;

  if httpsmode then
    if not TurnToSSL(Timeout) then
      exit;

  if not WriteLn('GET ' + uri + ' HTTP/1.0') then
    exit;
  if not WriteLn('Host: ' + host) then
    exit;
  for i := 0 to CustomHeaders.Count - 1 do
    if not WriteLn(CustomHeaders.Names[i] + ': ' +
      CustomHeaders.ValueFromIndex[i]) then
      exit;

  if not WriteLn('') then
    exit;

  if output = nil then
    olvass := 0
  else
    olvass := headernekszantresz;
  if not Read(Response, Timeout, olvass, True) then
    exit;

  Response.Position := 0;
  sss:= Response.DataString;

  rx:=TRegexpr.Create;
  try
rx.ModifierI:=True;
rx.ModifierM:=True;
rx.Expression:='HTTPS?\/[\d\.]+\s301\sMoved\sPermanently';
if rx.Exec(sss) then begin
rx.Expression:='^Location\:\s(.*?)$';
if rx.Exec(sss) then begin

uri:=rx.Match[1];
Cleanup;
  if not Connect(Timeout) then
    exit;

  if not WriteLn('GET ' + uri + ' HTTP/1.0') then
    exit;
  if not WriteLn('Host: ' + host) then
    exit;
  for i := 0 to CustomHeaders.Count - 1 do
    if not WriteLn(CustomHeaders.Names[i] + ': ' +
      CustomHeaders.ValueFromIndex[i]) then
      exit;

  if not WriteLn('') then
    exit;

  if output = nil then
    olvass := 0
  else
    olvass := headernekszantresz;
  if not Read(Response, Timeout, olvass, True) then
    exit;

  Response.Position := 0;
  sss:= Response.DataString;

end;
end;
  finally
   rx.Free;
//   result:=True;
//   exit;
  end;

  s := Response.ReadString(16384);

  ResponseStartsAt := Pos(#13#10#13#10, s);
  if ResponseStartsAt = 0 then
    exit;
  inc(ResponseStartsAt, 4);


  ResponseCode := StrToIntDef(Copy(s, 10, 3), 0);
  if ResponseCode = 0 then
    exit;

  s := Copy(s, 1, ResponseStartsAt - 2);

  get_count := 0;
  while (true) do
  begin
    inc(get_count);
    if get_count > 250 then
    begin
      Debug(dpError, section, '[iNFO] TslHTTP.Get count break', []);
      exit;
    end;
    ss := Elsosor(s);
    if ss = '' then
      Break;
    i := Pos(': ', ss);
    if i <> 0 then
      ResponseHeaders.Add(Copy(ss, 1, i - 1) + '=' + Copy(ss, i + 2,
        Length(ss)));
  end;
  if ResponseHeaders.Count = 0 then
    exit;

  contentlength := StrToIntDef(ResponseHeaders.Values['Content-Length'], 0);

  Response.Position := ResponseStartsAt - 1;

  if output <> nil then
  begin
    output.CopyFrom(Response, Response.Size - Response.Position);
    if Response.Size = headernekszantresz then
    begin
      // lementjuk a maradekot
      if not Read(output, Timeout, 0, True) then
        exit;
    end;

    Response.Size := ResponseStartsAt;
  end;

  (*
  for i:= 0 to ResponseHeaders.Count-1 do
  begin
    system.writeln(ResponseHeaders.Names[i]);
    system.writeln(ResponseHeaders.ValueFromIndex[i]);
  end;
  *)

  Result := True;

end;

function TslHTTP.Post(const url: AnsiString; content: AnsiString): Boolean;
var
  s, ss: AnsiString;
  uri: AnsiString;
  i: Integer;
  httpsmode: Boolean;
  post_count: Integer;
begin
  Result := False;
  Cleanup;

  if (1 = Pos('https://', url)) then
  begin
    httpsmode := True;
    host := Copy(url, 9, 1000);
  end
  else if (1 = Pos('http://', url)) then
  begin
    httpsmode := False;
    host := Copy(url, 8, 1000);
  end
  else
    exit;

  i := Pos('/', host);
  if i <> 0 then
  begin
    uri := Copy(host, i, 1000);
    host := Copy(host, 1, i - 1);
  end
  else
    uri := '/';

  i := Pos(':', host);
  if i <> 0 then
  begin
    s := Copy(host, i + 1, 1000);
    port := StrToInt(s);
    Delete(host, i, length(s) + 1);
  end
  else
  begin
    if httpsmode then
      Port := 443
    else
      Port := 80;
  end;

  if not Connect(Timeout) then
    exit;

  if httpsmode then
    if not TurnToSSL(Timeout) then
      exit;

  if not WriteLn('POST ' + uri + ' HTTP/1.0') then
    exit;
  if not WriteLn('Host: ' + host) then
    exit;
  if not WriteLn('Content-Length: ' + IntToStr(Length(content))) then
    exit;
  for i := 0 to CustomHeaders.Count - 1 do
    if not WriteLn(CustomHeaders.Names[i] + ': ' +
      CustomHeaders.ValueFromIndex[i]) then
      exit;
  if not WriteLn('') then
    exit;

  if not WriteLn(content) then
    exit;
  if not WriteLn('') then
    exit;

  if not Read(Response, Timeout, 0, True) then
    exit;

  Response.Position := 0;
  s := Response.ReadString(16384);

  ResponseStartsAt := Pos(#13#10#13#10, s);
  if ResponseStartsAt = 0 then
    exit;
  inc(ResponseStartsAt, 4);

  ResponseCode := StrToIntDef(Copy(s, 10, 3), 0);
  if ResponseCode = 0 then
    exit;

  s := Copy(s, 1, ResponseStartsAt - 2);

  post_count := 0;
  while (true) do
  begin
    inc(post_count);
    if post_count > 250 then
    begin
      Debug(dpError, section, '[iNFO] TslHTTP.Post count break', []);
      exit;
    end;

    ss := Elsosor(s);
    if ss = '' then
      Break;
    i := Pos(': ', ss);
    if i <> 0 then
      ResponseHeaders.Add(Copy(ss, 1, i - 1) + '=' + Copy(ss, i + 2,
        Length(ss)));
  end;
  if ResponseHeaders.Count = 0 then
    exit;

  contentlength := StrToIntDef(ResponseHeaders.Values['Content-Length'], 0);

  Response.Position := ResponseStartsAt - 1;

  Result := True;
end;

function TslHTTP.Post(const url: AnsiString; content: TStringList): Boolean;
var
  contentstr: AnsiString;
  i: Integer;
begin

  contentstr := '';
  for i := 0 to content.Count - 1 do
  begin
    if i <> 0 then
      contentstr := contentstr + '&';
    contentstr := contentstr + UrlEncode(content.Names[i]) + '=' +
      UrlEncode(content.ValueFromIndex[i]);
  end;

  Result := Post(url, contentstr);

end;

function TslHTTP.Get(const url: AnsiString): Boolean;
begin
  Result := Get(url, nil);
end;

end.
 
