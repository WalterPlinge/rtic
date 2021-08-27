#!/bin/sh

AppName="rtic"

Output="-o $AppName"
Debug="-O0 -g"
Release="-ffast-math -O2 -fopenmp"

CompilerFlags="$Output -std=c11 -pedantic -fno-gnu-keywords -fno-ms-extensions"
LinkerFlags="--ld-path=mold -L../libs/ -lm"

if [ ! -d "build" ]; then mkdir build; fi
pushd build

if [ "$1" == "-r" ]; then
	clang ../code/main.c $Release $CompilerFlags $LinkerFlags
else
	clang ../code/main.c $Debug   $CompilerFlags $LinkerFlags
fi

popd
