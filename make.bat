@echo off
setlocal

set AppName=rtic

set Output=-Fe:%AppName% -Fa%AppName% -FAasu
set Debug=-fp:precise -Od
set Release=-fp:fast -O2 -openmp

set CompilerFlags=%Output% %Debug% -analyze- -FC -MTd -std:c17 -TC -utf-8 -W3 -Zi
set LinkerFlags=-subsystem:console

if not exist build mkdir build
pushd build

call cl ../code/main.c %CompilerFlags% -link -libpath:../libs/ %LinkerFlags%

if "%~1" == "-r" call main.exe

popd
