@echo off
set ZMAC=..\ZMAC\zmac

set NAME=LEVEL2
%ZMAC% -c --od . -o %NAME%.cim -o %NAME%.lst %NAME%.asm -P0=102
if errorlevel 1 pause && goto :eof

fc /B %NAME%.cim %NAME%.rom
if errorlevel 1 pause && goto :eof

