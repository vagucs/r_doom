@echo off
setlocal
set "RHOME=C:\Program Files\R\R-4.6.1"
set "GCCBIN=C:\rtools45\x86_64-w64-mingw32.static.posix\bin"
set "PATH=%GCCBIN%;C:\rtools45\usr\bin;%PATH%"
set "GCC=%GCCBIN%\gcc.exe"
set "SDLINC=C:\rtools45\x86_64-w64-mingw32.static.posix\include\SDL2"
set "SDLLIB=C:\msys64\ucrt64\lib"
set "SDL2DLL=C:\msys64\ucrt64\bin\SDL2.dll"
cd /d "%~dp0"
if not exist "%GCC%" (
  echo gcc do Rtools nao encontrado: %GCC%
  exit /b 1
)
if not exist "%SDL2DLL%" (
  echo SDL2.dll nao encontrada: %SDL2DLL%
  exit /b 1
)
if not exist bin mkdir bin
"%GCC%" -shared -O2 -o bin\rdoom_sdl.dll src\rdoom_sdl.c -I"%RHOME%\include" -I"%SDLINC%" -L"%SDLLIB%" -lSDL2 -lwinmm "%RHOME%\bin\x64\R.dll" -static-libgcc
if errorlevel 1 exit /b 1
copy /Y "%SDL2DLL%" bin\SDL2.dll >nul
echo bin\rdoom_sdl.dll
exit /b 0
