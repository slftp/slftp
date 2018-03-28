unit taskspeedtest;

interface

uses
  tasksunit, sltcp;

type
  TDelSpeedtestFileTask = class(TTask)
    constructor Create(const netname, channel: String;site: String);
    function Execute(slot: Pointer): Boolean; override;
    function Name: String; override;
  end;

  TUploadSpeedtestFileTask = class(TTask)
  private
    idTCP: TslTCPSocket;
  public
    constructor Create(const netname, channel: String;site: String);
    destructor Destroy; override;
    function Execute(slot: Pointer): Boolean; override;
    function Name: String; override;
  end;


procedure SpeedTestInit;

var speedtestfilename: String;


implementation

uses
  DateUtils, configunit, SysUtils, debugunit, irc, sitesunit, mystrings;

const section = 'speedtest';

{ TDelSpeedtestFileTask }

constructor TDelSpeedtestFileTask.Create(const netname, channel: String;
  site: String);
begin
  inherited Create(netname, channel, site);
end;

function TDelSpeedtestFileTask.Execute(slot: Pointer): Boolean;
label ujra;
var s: TSiteSlot;
    dir: String;
begin
  Result:= False;
  s:= slot;
  Debug(dpMessage, section, Name);

  dir:=  s.site.sectiondir['SPEEDTEST'];
  if dir = '' then
  begin
    ready:= True;
    exit;
  end;

ujra:
  if s.status <> ssOnline then
    if not s.ReLogin then
    begin
      readyerror:= True;
      exit;
    end;

  if not s.Send('DELE %s%s', [MyIncludeTrailingSlash(dir), speedtestfilename]) then
    goto ujra;
  if not s.Read('DELE') then
    goto ujra;

  ready:= True;
  Result:= True;
end;

function TDelSpeedtestFileTask.Name: String;
begin
  try
    Result:= 'DELSPEEDTESTFILE '+site1;
  except
    Result:= 'DELSPEEDTESTFILE';
  end;
end;

{ TUploadSpeedtestFileTask }

constructor TUploadSpeedtestFileTask.Create(const netname, channel: String;
  site: String);
begin
  idTCP:= TslTCPSocket.Create;
  self.wanted_up:= True;
  inherited Create(netname, channel, site);
end;

destructor TUploadSpeedtestFileTask.Destroy;
begin
  idTCP.Free;
  inherited;
end;

function TUploadSpeedtestFileTask.Execute(slot: Pointer): Boolean;
label ujra;
var s: TSiteSlot;
    host, dir: String;
    port, probak: Integer;
    buffer: array[0..65535] of Byte;
    i, j, mb: Integer;
    d: Double;
    lastAnnounce, uploadStarted: TDateTime;
begin
  Result:= False;
  s:= slot;
  Debug(dpMessage, section, Name);

  dir:=  s.site.sectiondir['SPEEDTEST'];
  if dir = '' then
  begin
    ready:= True;
    irc_addtext(s.todotask, 'No Speedtest dir found for %s',[s.site.name]);
    exit;
  end;

  probak:= 0;

ujra:
  if s.status <> ssOnline then
    if not s.ReLogin then
    begin
      readyerror:= True;
      exit;
    end;

  s.uploadingto:= True;

  FillChar(buffer, SizeOf(buffer), 0);

  if (not s.Cwd(dir, true)) then goto ujra;

  if (not s.Send('DELE '+speedtestfilename)) then goto ujra;
  if (not s.Read('DELE')) then goto ujra;

  if not s.SendProtP then goto ujra;

    if s.site.sw = sswDrftpd then
    begin
      if not s.Send('PRET STOR %s', [speedtestfilename]) then goto ujra;
      if not s.Read('PRET STOR') then goto ujra;
    end;

    if not s.Send('PASV') then goto ujra;
    if not s.Read('PASV') then goto ujra;

    if (s.lastResponseCode <> 227) then
    begin
      irc_addtext(s.todotask, Trim(s.lastResponse));
      if probak = 0 then
      begin
        inc(probak);
        goto ujra;
      end else
      begin
        irc_addtext(s.todotask, 'Couldnt use passive mode');
        readyerror:= True;
        exit;
      end;
    end;

    ParsePasvString(s.lastResponse, host, port);
    if port = 0 then
    begin
        irc_addtext(s.todotask, 'Couldnt parse passive string');
        readyerror:= True;
        exit;
    end;

      idTCP.Host:= host;
      idTCP.Port:= port;
      idtcp.Disconnect;

      // itt lameszarokat kell olvasni az elejerol
      if not s.Send('REST 0') then goto ujra;
      if not s.Read() then goto ujra;

      if not s.Send('STOR '+speedtestfilename) then
        goto ujra;



      if not idTCP.Connect(s.site.connect_timeout * 1000) then
      begin
        if probak = 0 then
        begin
          inc(probak);
          irc_addtext(s.todotask, idTCP.error);
          s.DestroySocket(False);
          goto ujra;
        end else
        begin
          irc_addtext(s.todotask, 'Couldnt connect to site ('+idTCP.error+')');
          s.DestroySocket(False);
          readyerror:= True;
          exit;
        end;
      end;
//      idtcp.SendBuffer(16384);

      if (not s.Read()) then
      begin
        if probak = 0 then
        begin
          inc(probak);
          irc_addtext(s.todotask, s.error);
          s.DestroySocket(False);
          goto ujra;
         end else
        begin
          irc_addtext(s.todotask, 'Couldnt read response of site');
          s.DestroySocket(False);
          readyerror:= True;
          exit;
        end;
      end;

      if (s.lastResponseCode <> 150) then
      begin
          irc_addtext(s.todotask, 'ERROR: %s', [s.lastResponse]);
          s.DestroySocket(False);
          readyerror:= True;
          exit;
      end;


      if not idTCP.TurnToSSL(s.site.io_timeout * 1000) then
      begin
        if probak = 0 then
        begin
          inc(probak);
          irc_addtext(s.todotask, idTCP.error);
          s.DestroySocket(False);
          goto ujra;
        end else
        begin
          irc_addtext(s.todotask, 'Couldnt negotiate SSL connection ('+idTCP.error+')');
          s.DestroySocket(False);
          readyerror:= True;
          exit;
        end;
      end;


      mb:= config.ReadInteger(section, 'local_upload_mb', 50);
      irc_addtext(netname, channel, 'Uploading %d mb to %s...', [mb, site1]);
      uploadStarted:= Now;
      lastAnnounce:= uploadStarted;
      for i:= 1 to mb*1024*1024 div SizeOf(buffer) do
      begin
        if not idTCP.WriteBuffer(buffer, Sizeof(buffer)) then
        begin
          irc_addtext(s.todotask, 'Couldnt send to site ('+s.error+')');
          s.DestroySocket(False);
          readyerror:= True;
          exit;
        end;

        if SecondsBetween(Now, lastAnnounce) > config.ReadInteger(section, 'announce_interval', 20) then
        begin
          j:= MillisecondsBetween(Now,uploadStarted);
          lastAnnounce:= Now;
          d:= i * SizeOf(buffer);
          if j <> 0 then
            irc_addtext(netname, channel, 'slFtp -> %s => %.1f kB/s (uploaded %.1f/%dmB in %.1fs)', [site1, d*1000/1024/j, d / 1024 / 1024, mb, j / 1000]);
        end;
      end;
      idTCP.Disconnect;
      lastAnnounce:= Now();

      if not s.Read() then goto ujra;

      // kesz!
          j:= MillisecondsBetween(lastAnnounce, uploadStarted);
          d:= j;
          if j <> 0 then
            irc_addtext(netname, channel, 'slFtp -> %s is %.1f kB/s (uploaded %dmb in %.1fs)', [site1, 50*1024*1000/d, mb, d / 1000]);

  ready:= True;
  Result:= True;
end;

function TUploadSpeedtestFileTask.Name: String;
begin
  try
    Result:= 'UPLOADSPEEDTESTFILE '+site1;
  except
    Result:= 'UPLOADSPEEDTESTFILE';
  end;
end;

procedure SpeedTestInit;
begin
  speedtestfilename:= config.ReadString(section, 'speedtest_filename_suffix', 'lamer')
end;

end.
