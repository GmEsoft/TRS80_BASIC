@echo off
set ZMAC=..\ZMAC\zmac

set NAME=MBASIC
if "%1" == "" cls
%ZMAC% -c --od . -o %NAME%.cim -o %NAME%521.lst %NAME%.asm
if errorlevel 1 if not "%1" == "" goto :eof
if errorlevel 1 pause && goto :eof

fc /A /B %NAME%.cim %NAME%.com
if errorlevel 1 if not "%1" == "" goto :eof
if errorlevel 1 pause && goto :eof

echo ALL OK.
if "%1" == "" pause
goto :eof
