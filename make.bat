@echo off
setlocal EnableDelayedExpansion

:: Name of the output executable
set OUTPUT=z80.exe

:: List of all .c source files
set SOURCES=main.c src\libz80.c src\rom_loader.c src\parity.c

:: Compiler flags (you can add -Wall -O2 etc.)
:: set CFLAGS=

:: Linker flags (libraries to link)
:: set LIBS=

:: Build

echo Compiling...
gcc %CFLAGS% %SOURCES% %LIBS% -o %OUTPUT%

if errorlevel 1 (
    echo Build failed.
    exit /b 1
)

echo Build succeeded. Output: %OUTPUT%
