@echo off
setlocal

set AppName=rtic
set CodeFile=../code/main.c

set Compiler=cl

set Output=-Fe:%AppName% -Fa%AppName% -FAasu -Zi
set Warnings=-W4 -WX

set CompilerFlags=%Output% %Warnings% -nologo -analyze- -FC -GR- -GS- -Gw -Gy -std:c17 -TC -utf-8
set CompilerDebug=%CompilerFlags% -fp:precise -MTd -Oi
set CompilerRelease=%CompilerFlags% -fp:fast -GL -MT -O2 -openmp

set LinkerFlags=-link -nologo -libpath:../libs/ -subsystem:console
set LinkerDebug=%LinkerFlags% -debug
set LinkerRelease=%LinkerFlags% -LTCG

if not exist build mkdir build
pushd build

if "%~1" == "-r" (
	call %Compiler% %CodeFile% %CompilerRelease% %LinkerRelease%
) else (
	call %Compiler% %CodeFile% %CompilerDebug% %LinkerDebug%
)

popd
