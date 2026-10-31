@echo off
rem ANCHOR Studio - troubleshooting start: runs the app with its messages visible in this window.
title ANCHOR Studio - troubleshoot
set "D=%~dp0"
set "PATH=%D%runtime;%D%runtime\Library\mingw-w64\bin;%D%runtime\Library\usr\bin;%D%runtime\Library\bin;%D%runtime\Scripts;%D%runtime\lib\R\bin\x64;%PATH%"
set "R_HOME=%D%runtime\lib\R"
set R_LIBS=
set "R_LIBS_USER=NULL"
set "R_LIBS_SITE=NULL"
echo ANCHOR Studio - troubleshooting start. Keep this window open; it shows every message from R.
echo Folder: "%D%"
echo.
if not exist "%D%runtime\lib\R\bin\x64\Rscript.exe" goto missing
"%D%runtime\lib\R\bin\x64\Rscript.exe" --vanilla "%D%app\launch.R"
echo.
echo R stopped with code %ERRORLEVEL%. If you need help, copy the text in this window.
goto end
:missing
echo The runtime folder is missing or incomplete. Reinstall ANCHOR Studio.
:end
pause
