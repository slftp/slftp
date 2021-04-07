unit sitesunitTests;

interface

uses
  {$IFDEF FPC}
    TestFramework;
  {$ELSE}
    DUnitX.TestFramework, DUnitX.DUnitCompatibility;
  {$ENDIF}

type
  TTestSitesunit = class(TTestCase)
  published
    procedure TestParseSiteSoftwareGLFTPDStat1;
    procedure TestParseSiteSoftwareGLFTPDStat2;
    procedure TestParseSiteSoftwareDRFTPDStat1;
    procedure TestParseSiteSoftwareDRFTPDStat2;
    procedure TestParseSiteSoftwareGLFTPDVERS1;
    procedure TestParseSiteSoftwareIOFTPDVERS1;
    procedure TestParseSiteSoftwareRaidenFTPDBANNER1;
    procedure TestParseSiteSoftwarePureFTPDBANNER1;
  end;

implementation

uses
  SysUtils, sitesunit;

{ TTestSitesunit }

procedure TTestSitesunit.TestParseSiteSoftwareGLFTPDStat1;
var
  fText, fSoftwareVersion: String;
  fSiteSoftware: TSiteSW;
begin
  fText := 'FC server status:' + sLineBreak +
    'glFTPd 2.11a (Jan  3 2021) 64BiT Linux+TLS(OpenSSL 1.1.1i  8 Dec 2020)+SSP' + sLineBreak +
    'Connected to test.domain.com (133.71.33.71)' + sLineBreak +
    'Logged in as XXX' + sLineBreak +
    'TYPE: Image; STRUcture: File; transfer MODE: Stream' + sLineBreak +
    'No data connection';

  fSiteSoftware := StringToSiteSoftWare(fText);
  CheckEqualsString(SiteSoftWareToString(sswGlFTPD), SiteSoftWareToString(fSiteSoftware));
  fSoftwareVersion := ParseSiteSoftwareVersionFromString(fSiteSoftware, fText);
  CheckEqualsString('2.11a', fSoftwareVersion);
end;

procedure TTestSitesunit.TestParseSiteSoftwareGLFTPDStat2;
var
  fText, fSoftwareVersion: String;
  fSiteSoftware: TSiteSW;
begin
  fText := 'XX server status:' + sLineBreak +
    'glFTPd 2.11 (Dec 22 2020) 64BiT Linux+TLS(OpenSSL 1.1.1i  8 Dec 2020)+SSP' + sLineBreak +
    'Connected to test.domain.com (133.71.33.71)' + sLineBreak +
    'Logged in as XXX' + sLineBreak +
    'TYPE: Image; STRUcture: File; transfer MODE: Stream' + sLineBreak +
    'No data connection';

  fSiteSoftware := StringToSiteSoftWare(fText);
  CheckEqualsString(SiteSoftWareToString(sswGlFTPD), SiteSoftWareToString(fSiteSoftware));
  fSoftwareVersion := ParseSiteSoftwareVersionFromString(fSiteSoftware, fText);
  CheckEqualsString('2.11', fSoftwareVersion);
end;

procedure TTestSitesunit.TestParseSiteSoftwareDRFTPDStat1;
var
  fText, fSoftwareVersion: String;
  fSiteSoftware: TSiteSW;
begin
  fText := 'FTP server status:' + sLineBreak +
    'Logged in as XXX' + sLineBreak +
    'Connected from 133.71.33.71' + sLineBreak +
    'Session timeout in seconds is 300' + sLineBreak +
    'Is this a secure connection? Yes' + sLineBreak +
    'Client count is: 1337' + sLineBreak +
    'DrFTPD 3.2.0 - The Distributed FTPD';

  fSiteSoftware := StringToSiteSoftWare(fText);
  CheckEqualsString(SiteSoftWareToString(sswDrftpd), SiteSoftWareToString(fSiteSoftware));
  fSoftwareVersion := ParseSiteSoftwareVersionFromString(fSiteSoftware, fText);
  CheckEqualsString('3.2.0', fSoftwareVersion);
end;

procedure TTestSitesunit.TestParseSiteSoftwareDRFTPDStat2;
var
  fText, fSoftwareVersion: String;
  fSiteSoftware: TSiteSW;
begin
  fText := 'FTP server status:' + sLineBreak +
    'Logged in as XXX' + sLineBreak +
    'Connected from 133.71.33.71' + sLineBreak +
    'Session timeout in seconds is 300' + sLineBreak +
    'Is this a secure connection? Yes' + sLineBreak +
    'Client count is: 1337' + sLineBreak +
    'DrFTPD 4.0.1-SNAPSHOT - The Distributed FTPD';

  fSiteSoftware := StringToSiteSoftWare(fText);
  CheckEqualsString(SiteSoftWareToString(sswDrftpd), SiteSoftWareToString(fSiteSoftware));
  fSoftwareVersion := ParseSiteSoftwareVersionFromString(fSiteSoftware, fText);
  CheckEqualsString('4.0.1-SNAPSHOT', fSoftwareVersion);
end;

procedure TTestSitesunit.TestParseSiteSoftwareGLFTPDVERS1;
var
  fText, fSoftwareVersion: String;
  fSiteSoftware: TSiteSW;
begin
  fText := 'glFTPd 2.11a (Jan  3 2021) 64BiT Linux+TLS(OpenSSL 1.1.1i  8 Dec 2020)+SSP';

  fSiteSoftware := StringToSiteSoftWare(fText);
  CheckEqualsString(SiteSoftWareToString(sswGlFTPD), SiteSoftWareToString(fSiteSoftware));
  fSoftwareVersion := ParseSiteSoftwareVersionFromString(fSiteSoftware, fText);
  CheckEqualsString('2.11a', fSoftwareVersion);
end;

procedure TTestSitesunit.TestParseSiteSoftwareIOFTPDVERS1;
var
  fText, fSoftwareVersion: String;
  fSiteSoftware: TSiteSW;
begin
  fText := 'ioFTPD version: 7-7-3r, OpenSSL v1.0.2n , BioNiNJA v1.0b5 Release 2018-11-13 Custom';

  fSiteSoftware := StringToSiteSoftWare(fText);
  CheckEqualsString(SiteSoftWareToString(sswIoftpd), SiteSoftWareToString(fSiteSoftware));
  fSoftwareVersion := ParseSiteSoftwareVersionFromString(fSiteSoftware, fText);
  CheckEqualsString('7-7-3r', fSoftwareVersion);
end;

procedure TTestSitesunit.TestParseSiteSoftwareRaidenFTPDBANNER1;
var
  fText, fSoftwareVersion: String;
  fSiteSoftware: TSiteSW;
begin
  fText := 'Welcome to RaidenFTPD32 FTP server';

  fSiteSoftware := StringToSiteSoftWare(fText);
  CheckEqualsString(SiteSoftWareToString(sswRaidenftpd), SiteSoftWareToString(fSiteSoftware));
end;

procedure TTestSitesunit.TestParseSiteSoftwarePureFTPDBANNER1;
var
  fText, fSoftwareVersion: String;
  fSiteSoftware: TSiteSW;
begin
  fText := '---------- Welcome to Pure-FTPd [privsep] [TLS] ----------';

  fSiteSoftware := StringToSiteSoftWare(fText);
  CheckEqualsString(SiteSoftWareToString(sswPureFTPd), SiteSoftWareToString(fSiteSoftware));
end;

initialization
  {$IFDEF FPC}
    RegisterTest('sitesunit', TTestSitesunit.Suite);
  {$ELSE}
    TDUnitX.RegisterTestFixture(TTestSitesunit);
  {$ENDIF}
end.
