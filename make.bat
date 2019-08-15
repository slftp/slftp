@echo off
REM
REM Quick and dirty SLFTP Delphi compilation script
REM
for /f "delims=" %%a in ('where dcc32.exe') do @set CC_32=%%a
for /f "delims=" %%a in ('where dcc64.exe') do @set CC_64=%%a
set CC_EXTRAS=-NSWinapi;System.Win;Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;Bde;System;Xml;Data;Datasnap;Web;Soap
set CFLAGS=-B -$O+,C+,D-,L-
set CDBFLAGS=-B -$O+,C+,D+,L+
set CINCLUDES=-Uirccommands -Urules -Ulibs/FastMM4 -Ulibs/BeRoHighResolutionTimer -Ulibs/FLRE -Ulibs/rcmdline -Ulibs/lkJSON -Ulibs/TRegExpr -Ulibs/pasmp -Ulibs/Indy10/Core -Ulibs/Indy10/Protocols -Ulibs/Indy10/System -Ulibs/LibTar -Ulibs/mORMot -Ulibs/mORMot/SQLite3 -Ulibs/mORMot/CrossPlatform -Ulibs/ZeosLib -Ulibs/ZeosLib/core -Ulibs/ZeosLib/dbc -Ulibs/ZeosLib/parsesql -Ulibs/ZeosLib/plain
set UnitTestAppName="tests\slftpUnitTests.exe --exitbehavior:Continue"
set CTESTINCLUDES=-Utests/DUnitX

REM
REM OpenSSL version, depending names for 32/64bit will be added later
REM
set OPENSSL_NAME=openssl-1.0.2r

REM
REM Inject git commit into slftp.inc if .git exists
REM
if exist .git\ (
  powershell "& {$gitrevv = git rev-parse --short HEAD; (Get-Content .\slftp.inc) -replace \"SL_REV: string.*\", \"SL_REV: string = '$gitrevv';\" | Set-Content .\slftp.inc }"
)

REM
REM default: 64bit (2018!)
REM
if /I "%~1" == "" goto :slftp_64
if /I "%~1" == "slftp" goto :slftp_64
if /I "%~1" == "slftp_debug" goto :slftp_64_debug
if /I "%~1" == "slftp_32" goto :slftp_32
if /I "%~1" == "slftp_64" goto :slftp_64
if /I "%~1" == "slftp_32_debug" goto :slftp_32_debug
if /I "%~1" == "slftp_64_debug" goto :slftp_64_debug
if /I "%~1" == "clean" goto :clean
if /I "%~1" == "test" goto :test_64
if /I "%~1" == "test_32" goto :test_32
if /I "%~1" == "test_64" goto :test_64

goto :error

:slftp_32
del /q /s *slftp*.exe *.dcu
echo --- Compiling Win32 RELEASE slftp.exe ---
echo "%CC_32%" %CFLAGS% %CC_EXTRAS% %CINCLUDES% slftp.dpr
"%CC_32%" %CFLAGS% %CC_EXTRAS% %CINCLUDES% slftp.dpr
if errorlevel 1 (
   echo Failure reason given is %errorlevel%
   exit /b %errorlevel%
)
goto :eof;

:slftp_64
del /q /s *slftp*.exe *.dcu
echo --- Compiling Win64 RELEASE slftp.exe ---
echo "%CC_64%" %CFLAGS% %CC_EXTRAS% %CINCLUDES% slftp.dpr
"%CC_64%" %CFLAGS% %CC_EXTRAS% %CINCLUDES% slftp.dpr
if errorlevel 1 (
   echo Failure reason given is %errorlevel%
   exit /b %errorlevel%
)
goto :eof;

:slftp_32_debug
del /q /s *slftp*.exe *.dcu
echo --- Compiling Win32 DEBUG slftp.exe ---
echo "%CC_32%" %CDBFLAGS% %CC_EXTRAS% %CINCLUDES% slftp.dpr
"%CC_32%" %CDBFLAGS% %CC_EXTRAS% %CINCLUDES% slftp.dpr
if errorlevel 1 (
   echo Failure reason given is %errorlevel%
   exit /b %errorlevel%
)
goto :eof;

:slftp_64_debug
del /q /s *slftp*.exe *.dcu
echo --- Compiling Win64 DEBUG slftp.exe ---
echo "%CC_64%" %CDBFLAGS% %CC_EXTRAS% %CINCLUDES% slftp.dpr
"%CC_64%" %CDBFLAGS% %CC_EXTRAS% %CINCLUDES% slftp.dpr
if errorlevel 1 (
   echo Failure reason given is %errorlevel%
   exit /b %errorlevel%
)
goto :eof;

:clean
echo --- Cleaning files ---
del /q /s *slftp*.exe *.dcu
goto :eof;

:test_32
del /q /s *slftp*.exe *.dcu *.dll
echo -- Testing Win32 ---
cd tests
echo - Downloading OpenSSL %OPENSSL_NAME% libraries -
powershell -Command "(New-Object Net.WebClient).DownloadFile('https://indy.fulgan.com/SSL/%OPENSSL_NAME%-i386-win32.zip', '%OPENSSL_NAME%-i386-win32.zip')"
if errorlevel 1 (
echo Failure reason for downloading OpenSSL is %errorlevel%
exit /b %errorlevel%
)
echo - Extracting OpenSSL libraries -
powershell expand-archive %OPENSSL_NAME%-i386-win32.zip
if errorlevel 1 (
echo Failure reason for extracting is %errorlevel%
exit /b %errorlevel%
)
echo - Copying OpenSSL libraries -
copy /Y %OPENSSL_NAME%-i386-win32\libeay32.dll libeay32.dll
copy /Y %OPENSSL_NAME%-i386-win32\ssleay32.dll ssleay32.dll
if errorlevel 1 (
echo Failure reason for copying is %errorlevel%
exit /b %errorlevel%
)
echo - Removing temp OpenSSL stuff -
del /Q %OPENSSL_NAME%-i386-win32.zip
rmdir /Q /S %OPENSSL_NAME%-i386-win32
if errorlevel 1 (
echo Failure reason for deleting is %errorlevel%
exit /b %errorlevel%
)
cd ..
echo - Compiling -
echo "%CC_32%" %CFLAGS% %CC_EXTRAS% %CINCLUDES% %CTESTINCLUDES% tests\slftpUnitTests.dpr
"%CC_32%" %CFLAGS% %CC_EXTRAS% %CINCLUDES% %CTESTINCLUDES% tests\slftpUnitTests.dpr
if errorlevel 1 (
   echo Failure reason for compiling tests is %errorlevel%
   exit /b %errorlevel%
)
"%UnitTestAppName%"
if errorlevel 1 (
   echo Failure reason for running tests is %errorlevel%
   exit /b %errorlevel%
)
REM - do a cleanup -
goto :clean;

:test_64
del /q /s *slftp*.exe *.dcu *.dll
echo -- Testing Win64 ---
cd tests
echo - Downloading OpenSSL %OPENSSL_NAME% libraries -
powershell -Command "(New-Object Net.WebClient).DownloadFile('https://indy.fulgan.com/SSL/%OPENSSL_NAME%-x64_86-win64.zip', '%OPENSSL_NAME%-x64_86-win64.zip')"
if errorlevel 1 (
   echo Failure reason for downloading OpenSSL is %errorlevel%
   exit /b %errorlevel%
)
echo - Extracting OpenSSL libraries -
powershell expand-archive %OPENSSL_NAME%-x64_86-win64.zip
if errorlevel 1 (
   echo Failure reason for extracting is %errorlevel%
   exit /b %errorlevel%
)
echo - Copying OpenSSL libraries -
copy /Y %OPENSSL_NAME%-x64_86-win64\libeay32.dll libeay32.dll /Y
copy /Y %OPENSSL_NAME%-x64_86-win64\ssleay32.dll ssleay32.dll /Y
if errorlevel 1 (
   echo Failure reason for copying is %errorlevel%
   exit /b %errorlevel%
)
echo - Removing temp OpenSSL stuff -
del /Q %OPENSSL_NAME%-x64_86-win64.zip
rmdir /Q /S %OPENSSL_NAME%-x64_86-win64
if errorlevel 1 (
   echo Failure reason for deleting is %errorlevel%
   exit /b %errorlevel%
)
cd ..
echo - Compiling -
echo "%CC_64%" %CFLAGS% %CC_EXTRAS% %CINCLUDES% %CTESTINCLUDES% tests\slftpUnitTests.dpr
"%CC_64%" %CFLAGS% %CC_EXTRAS% %CINCLUDES% %CTESTINCLUDES% tests\slftpUnitTests.dpr
if errorlevel 1 (
   echo Failure reason for compiling tests is %errorlevel%
   exit /b %errorlevel%
)
"%UnitTestAppName%"
if errorlevel 1 (
   echo Failure reason for running tests is %errorlevel%
   exit /b %errorlevel%
)
REM - do a cleanup -
goto :clean;

:error
echo Unknown target!
echo Valid targets: slftp slftp_debug slftp_32 slftp_64 slftp_32_debug slftp_64_debug clean test test_32 test_64
echo Default: slftp_64
goto :eof;

REM
REM Remove injected build rev from slftp.inc
REM
:eof
if exist .git\ (
  powershell "& {(Get-Content .\slftp.inc) -replace \"SL_REV: string.*\", \"SL_REV: string = '';\" | Set-Content .\slftp.inc }"
)
exit /B
