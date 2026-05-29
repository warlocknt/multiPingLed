@echo off
setlocal enabledelayedexpansion
set "LOG=%~dp0command.log"

>> "%LOG%" echo.
>> "%LOG%" echo === [%date% %time%] ===
>> "%LOG%" echo CMDCMDLINE:  %CMDCMDLINE%
>> "%LOG%" echo Script:      %~f0
>> "%LOG%" echo Full args:   %*
>> "%LOG%" echo CWD:         %CD%

set i=0
:loop
if "%~1"=="" goto :done
set /a i+=1
>> "%LOG%" echo   arg!i!: [%~1]
shift
goto :loop
:done

endlocal