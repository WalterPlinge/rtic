@echo off
setlocal

set AppName=rtic
set CodeFile=../code/main.c
set Compiler=cl

set Debug=-fp:precise -Od
set Release=-fp:fast -O2 -openmp

set Output=-Fe:%AppName% -Fa%AppName% -FAasu
set CompilerFlags=%Output% -analyze- -FC -MTd -std:c17 -TC -utf-8 -W3 -Zi
set LinkerFlags=-link -libpath:../libs/ -subsystem:console

if not exist build mkdir build
pushd build

if "%~1" == "-r" (
	call %Compiler% %CodeFile% %Release% %CompilerFlags% %LinkerFlags%
) else (
	call %Compiler% %CodeFile% %Debug%   %CompilerFlags% %LinkerFlags%
)

popd
