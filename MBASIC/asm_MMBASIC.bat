@echo off
set ZMAC=..\ZMAC\zmac

set NAME=MMBASIC
if "%1" == "" cls
%ZMAC% -c --od . -o %NAME%.cim -o %NAME%.lst %NAME%.asm
if errorlevel 1 if not "%1" == "" goto :eof
if errorlevel 1 pause && goto :eof

move /y %NAME%.cim %NAME%.COM
if errorlevel 1 if not "%1" == "" goto :eof
if errorlevel 1 pause && goto :eof
echo ALL OK.
if "%1" == "" pause
goto :eof
