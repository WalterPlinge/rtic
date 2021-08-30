@echo off
setlocal

set AppName=rtic
set CodeFile=../code/main.c

set Compiler=cl

set Output=-Fe:%AppName% -Fa%AppName% -FAasu -Fm%AppName% -Zi
set Warnings=-WX -W4 -wd4668 -wd4820 -wd5045

set CompilerFlags=%Output% %Warnings% -nologo -analyze- -EHa- -FC -Gm- -GR- -GS- -Gw -Gy -std:c17 -TC -utf-8
set CompilerDebug=%CompilerFlags% -fp:precise -MTd -Oi
set CompilerRelease=%CompilerFlags% -fp:fast -GL -MT -O2 -openmp

set LinkerFlags=-link -nologo -incremental:no -opt:ref -subsystem:console -libpath:../libs/
set LinkerDebug=%LinkerFlags% -debug:full
set LinkerRelease=%LinkerFlags% -LTCG

if not exist build mkdir build
pushd build

if "%~1" == "-r" (
	call %Compiler% %CodeFile% %CompilerRelease% %LinkerRelease%
) else (
	call %Compiler% %CodeFile% %CompilerDebug% %LinkerDebug%
)

popd
