@echo off
REM
REM Quick and dirty SLFTP Delphi compilation script
REM
set CC_32=C:\Program Files (x86)\Embarcadero\Studio\19.0\bin\dcc32.exe
set CC_64=C:\Program Files (x86)\Embarcadero\Studio\19.0\bin\dcc64.exe
set CC_EXTRAS=-NSWinapi;System.Win;Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;Bde;System;Xml;Data;Datasnap;Web;Soap;DUnitX.MemoryLeakMonitor
set CFLAGS=-B -$O+,C+,D-,L-
set CDBFLAGS=-B -$O+,C+,D+,L+
set CINCLUDES=-Ulibs/FastMM4 -Ulibs/BeRoHighResolutionTimer -Ulibs/FLRE -Ulibs/rcmdline -Ulibs/DFFLibV15_UIntList -Ulibs/lkJSON -Ulibs/TRegExpr -Ulibs/pasmp -Ulibs/Compvers -Ulibs/Indy10/Core -Ulibs/Indy10/Protocols -Ulibs/Indy10/System -Ulibs/LibTar -Ulibs/mORMot -Ulibs/ZeosLib -Ulibs/ZeosLib/core -Ulibs/ZeosLib/dbc -Ulibs/ZeosLib/plain

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

goto :error

:slftp_32
del /q *.exe *.dcu
echo Compiling Win32 RELEASE slftp.exe
echo "%CC_32%" %CFLAGS% %CC_EXTRAS% %CINCLUDES% slftp.dpr
"%CC_32%" %CFLAGS% %CC_EXTRAS% %CINCLUDES% slftp.dpr
if errorlevel 1 (
   echo Failure Reason Given is %errorlevel%
   exit /b %errorlevel%
)
goto :eof

:slftp_64
del /q *.exe *.dcu
echo Compiling Win64 RELEASE slftp.exe
echo "%CC_64%" %CFLAGS% %CC_EXTRAS% %CINCLUDES% slftp.dpr
"%CC_64%" %CFLAGS% %CC_EXTRAS% %CINCLUDES% slftp.dpr
if errorlevel 1 (
   echo Failure Reason Given is %errorlevel%
   exit /b %errorlevel%
)
goto :eof

:slftp_32_debug
del /q *.exe *.dcu
echo Compiling Win32 DEBUG slftp.exe
echo "%CC_32%" %CDBFLAGS% %CC_EXTRAS% %CINCLUDES% slftp.dpr
"%CC_32%" %CDBFLAGS% %CC_EXTRAS% %CINCLUDES% slftp.dpr
if errorlevel 1 (
   echo Failure Reason Given is %errorlevel%
   exit /b %errorlevel%
)
goto :eof

:slftp_64_debug
del /q *.exe *.dcu
echo Compiling Win64 DEBUG slftp.exe
echo "%CC_64%" %CDBFLAGS% %CC_EXTRAS% %CINCLUDES% slftp.dpr
"%CC_64%" %CDBFLAGS% %CC_EXTRAS% %CINCLUDES% slftp.dpr
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
echo Valid targets: slftp slftp_debug slftp_32 slftp_64 slftp_32_debug slftp_64_debug clean
echo Default: slftp_64
goto :eof
