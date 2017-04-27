@echo off
REM
REM Quick and dirty SLFTP Delphi compilation script
REM
set CC=dcc32.exe
set CFLAGS=-B -$O+,C+,D-,L- 
set CDBFLAGS=-B -$O+,C+,D+,L+
set CINCLUDES=-Ulibs/FastMM4

if /I "%~1" == "" goto :slftp
if /I "%~1" == "slftp" goto :slftp
if /I "%~1" == "slftp_debug" goto :slftp_debug
if /I "%~1" == "clean" goto :clean

goto :error

:slftp
del /q *.exe *.dcu
echo Compiling RELEASE Windows slftp.exe
%CC% %CFLAGS% %CINCLUDES% slftp.dpr
goto :eof

:slftp_debug
del /q *.exe *.dcu
echo Compiling DEBUG Windows slftp.exe
%CC% %CDBFLAGS% %CINCLUDES% slftp.dpr
goto :eof

:clean
echo Cleaning files
del /q *.exe *.dcu
goto :eof

:error
echo Unknown make target!
echo Valid targets: slftp slftp_debug clean
goto :eof