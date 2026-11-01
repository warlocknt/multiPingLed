@echo off
setlocal

cd /d "%~dp0"
set PINGLED_BUILD_TARGET=application
echo Building multiPingLed...

set FPC=C:\lazarus\fpc\3.2.2\bin\x86_64-win64\fpc.exe
set STRIP=C:\lazarus\fpc\3.2.2\bin\x86_64-win64\strip.exe
set SRC=multiPingLed.lpr
set OUT=multiPingLed.exe
if not "%~1"=="" set "OUT=%~1"
set LIBDIR=lib\x86_64-win64

set LCL=C:\lazarus\lcl\units\x86_64-win64
set LCLWIDGET=C:\lazarus\lcl\units\x86_64-win64\win32
set COMP=C:\lazarus\components\lazutils\lib\x86_64-win64

REM Create output directories
if not exist "%LIBDIR%" mkdir "%LIBDIR%"

set SQLITE_REBUILD=
for %%L in (sqlite3.obj libgcc.a libmingwex.a libkernel32_sqlite.a libmsvcrt_sqlite.a) do if not exist "vendor\sqlite\%%L" set SQLITE_REBUILD=1
if defined SQLITE_REBUILD (
    call vendor\sqlite\build_static.bat
    if errorlevel 1 exit /b 1
)
echo Compiling with O2 optimization...
call "%~dp0tools\compile.bat" -B "-Fo%~dp0vendor\sqlite" "-Fl%~dp0vendor\sqlite" -MObjFPC -Scghi -O2 -Xs -CX -XX -WG -vewnh -Fi. -Fu. -Fu%LCL% -Fu%LCLWIDGET% -Fu%COMP% %SRC% -o%OUT% -FU%LIBDIR%

if errorlevel 1 (
    echo.
    echo Build failed!
    exit /b 1
)

echo.
echo Stripping debug symbols...
if exist "%STRIP%" (
    "%STRIP%" --strip-all %OUT%
    echo Symbols stripped.
)

for %%I in (%OUT%) do set FILESIZE=%%~zI
echo.
echo Build successful: %OUT%
echo File size: %FILESIZE% bytes
echo.

exit /b 0
