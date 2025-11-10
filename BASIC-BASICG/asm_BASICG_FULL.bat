@echo off
set ZMAC=..\ZMAC\zmac

set NAME=BASICG_FULL
set TARGET=%NAME%

set GFX=1

%ZMAC% -c --od . -o %TARGET%.cmd -o %TARGET%.lst -o %TARGET%.bds %NAME%.asm -P0=%GFX%
if errorlevel 1 if not "%1" == "" goto :eof
if errorlevel 1 pause && goto :eof

echo ALL OK.
if "%1" == "" pause
goto :eof

