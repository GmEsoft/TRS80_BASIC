@echo off
set ZMAC=..\ZMAC\zmac

set NAME=LEVEL2
%ZMAC% -c --od . -o %NAME%N.cim -o %NAME%N.lst %NAME%.asm -P0=103
if errorlevel 1 pause && goto :eof

fc /B %NAME%N.cim %NAME%N.rom
if errorlevel 1 pause && goto :eof

