@echo off
setlocal

set CompilerFlags=-analyze- -FAasu -FC -fp:precise -MTd -Od -openmp -std:c17 -TC -utf-8 -W3 -Zi
set LinkerFlags=-subsystem:console

if not exist build mkdir build
pushd build

call cl ../code/main.c %CompilerFlags% -link -libpath:../libs/ %LinkerFlags%

if "%~1" == "-r" call main.exe

popd
