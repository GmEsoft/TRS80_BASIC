@echo off
set ZMAC=..\ZMAC\zmac

set NAME=LEVEL2
%ZMAC% -c --od . -o %NAME%R0.cim -o %NAME%R0.lst %NAME%.asm -P0=100
if errorlevel 1 pause && goto :eof

fc /B %NAME%R0.cim %NAME%R0.rom
if errorlevel 1 pause && goto :eof

