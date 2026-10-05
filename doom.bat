@echo off
setlocal
set "RBIN=C:\Program Files\R\R-4.6.1\bin"
cd /d "%~dp0"
call build_sdl.bat
if errorlevel 1 exit /b 1
"%RBIN%\Rscript.exe" --vanilla main.R %*
exit /b %ERRORLEVEL%
