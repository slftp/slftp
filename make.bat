@echo off
REM
REM Quick and dirty SLFTP Delphi compilation script
REM
set CC=C:\Program Files (x86)\CodeGear\RAD Studio\5.0\bin\dcc32.exe
set CC_ND_32=C:\Program Files (x86)\Embarcadero\Studio\19.0\bin\dcc32.exe
set CC_ND_64=C:\Program Files (x86)\Embarcadero\Studio\19.0\bin\dcc64.exe
set CFLAGS=-B -$O+,C+,D-,L-
set CDBFLAGS=-B -$O+,C+,D+,L+
set CINCLUDES=-Ulibs/FastMM4 -Ulibs/BeRoHighResolutionTimer -Ulibs/FLRE -Ulibs/rcmdline -Ulibs/DFFLibV15_UIntList -Ulibs/lkJSON -Ulibs/TRegExpr -Ulibs/pasmp -Ulibs/Compvers -Ulibs/Indy10_5448/Core -Ulibs/Indy10_5448/Protocols -Ulibs/Indy10_5448/System

if /I "%~1" == "" goto :slftp
if /I "%~1" == "slftp" goto :slftp
if /I "%~1" == "slftp_debug" goto :slftp_debug
if /I "%~1" == "slftp_nd" goto :slftp_nd_32
if /I "%~1" == "slftp_nd_64" goto :slftp_nd_64
if /I "%~1" == "slftp_nd_debug" goto :slftp_nd_32_debug
if /I "%~1" == "slftp_nd_64_debug" goto :slftp_nd_64_debug
if /I "%~1" == "clean" goto :clean

goto :error

:slftp
del /q *.exe *.dcu
echo Compiling RELEASE Win32 slftp.exe
"%CC%" %CFLAGS% %CINCLUDES% slftp.dpr
if errorlevel 1 (
   echo Failure Reason Given is %errorlevel%
   exit /b %errorlevel%
)
goto :eof

:slftp_debug
del /q *.exe *.dcu
echo Compiling DEBUG Win32 slftp.exe
"%CC%" %CDBFLAGS% %CINCLUDES% slftp.dpr
if errorlevel 1 (
   echo Failure Reason Given is %errorlevel%
   exit /b %errorlevel%
)
goto :eof

:slftp_nd_32
del /q *.exe *.dcu
echo Compiling NEWDELPHI RELEASE Win32 slftp.exe
"%CC_ND_32%" %CFLAGS% %CINCLUDES% slftp.dpr
if errorlevel 1 (
   echo Failure Reason Given is %errorlevel%
   exit /b %errorlevel%
)
goto :eof

:slftp_nd_64
del /q *.exe *.dcu
echo Compiling NEWDELPHI RELEASE Win64 slftp.exe
"%CC_ND_64%" %CFLAGS% %CINCLUDES% slftp.dpr
if errorlevel 1 (
   echo Failure Reason Given is %errorlevel%
   exit /b %errorlevel%
)g
oto :eof

:slftp_nd_32_debug
del /q *.exe *.dcu
echo Compiling NEWDELPHI DEBUG Win32 slftp.exe
"%CC_ND_32%" %CDBFLAGS% %CINCLUDES% slftp.dpr
if errorlevel 1 (
   echo Failure Reason Given is %errorlevel%
   exit /b %errorlevel%
)
goto :eof

:slftp_nd_64_debug
del /q *.exe *.dcu
echo Compiling NEWDELPHI DEBUG Win64 slftp.exe
"%CC_ND_64%" %CDBFLAGS% %CINCLUDES% slftp.dpr
if errorlevel 1 (
   echo Failure Reason Given is %errorlevel%
   exit /b %errorlevel%
)
goto :eof

:clean
echo Cleaning files
del /q *.exe *.dcu
goto :eof

:error
echo Unknown target!
echo Valid targets: slftp slftp_debug slftp_nd slftp_nd_64 slftp_nd_debug slftp_nd_64_debug clean
echo Default: slftp
goto :eof
