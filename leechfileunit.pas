unit leechfileunit;

interface

uses sitesunit, Classes, Debugunit;

function LeechFile(s: TSiteSlot; dest: TStream; const filename: string; restFrom: Integer = 0; maxRead: Integer = 0): Integer;

implementation

uses SysUtils, sltcp, irc, mystrings, slssl;

// 1 : file was retrieved successfully
// 0 : some error occoured
//-1 : fatal error occoured
function Leechfile(s: TSiteSlot; dest: TStream; const filename: string; restFrom: Integer = 0; maxRead: Integer = 0): Integer;
var
    idTCP: TslTCPSocket;
    host: string;
    port: Integer;
begin
  Result:= 0;
  try
  idTCP:= TslTCPSocket.Create;

  try

    // most fetchelnunk kell a fajlt...
    if not s.SendProtP then exit;

    if s.site.sw = sswDrftpd then
    begin
      if not s.Send('PRET RETR %s', [s.TranslateFilename(filename)]) then exit;
      if not s.Read('PRET RETR %s') then exit;
    end;

    if not s.Send('PASV') then exit;
    if not s.Read('PASV') then exit;

    if (s.lastResponseCode <> 227) then
    begin
      irc_addtext(s.todotask, Trim(s.lastResponse));
      Result:= -1;
      exit;
    end;
    ParsePasvString(s.lastResponse, host, port);
    if port = 0 then
    begin
        irc_AddText(s.todotask, s.site.name+': couldnt parse passive string / '+filename);
        Result:= -1;
        exit;
    end;

      idTCP.Host:= host;
      idTCP.Port:= port;

      if not s.Send('REST %d', [restFrom]) then exit;
      if not s.Read('REST') then exit;

      if not s.Send('RETR %s', [s.TranslateFilename(filename)]) then exit;


      if not idTCP.Connect(s.site.connect_timeout * 1000) then
      begin
        irc_AddText(s.todotask, s.site.name+': couldnt connect to site ('+idTCP.error+') / '+filename);
        s.DestroySocket(False);
        Result:= -1;
        exit;
      end;

      if not idTCP.TurnToSSL(slssl_ctx_tlsv1_2_client,s.site.io_timeout * 1000) then
      begin
        irc_AddText(s.todotask, s.site.name+': couldnt negotiate the SSL connection ('+idTCP.error+') / '+filename);
        s.DestroySocket(False);
        Result:= -1;
        exit;
      end;

      if not s.Read('RETR') then
      begin
        irc_AddText(s.todotask, s.site.name+': couldnt read response of site / '+filename);
        Result:= -1;
        exit;
      end;

      if not idTCP.Read(dest, s.site.io_timeout * 1000, maxRead, True) then
      begin
        irc_AddText(s.todotask, s.site.name+': couldnt fetch content ('+idTCP.error+') / '+filename);
        s.DestroySocket(False);
        Result:= -1;
        exit;
      end;

      idTCP.Disconnect;

      Result:= 1;
  finally
    idTCP.Free;
  end;

 except
    on e: Exception do
    begin
      Debug(dpError, 'LeechFile', Format('[EXCEPTION] TPazoSiteNfoTask: LeechFile : %s', [e.Message]));
      exit;
    end;
  end;

end;

end.
