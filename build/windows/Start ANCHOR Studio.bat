@echo off
rem Portable version: starts ANCHOR Studio with the R that ships in this folder.
start "ANCHOR Studio" /min "%~dp0R\bin\Rscript.exe" --no-save --no-restore "%~dp0app\launch.R"
