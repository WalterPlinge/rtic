#!/bin/bash

AppName="rtic"
CodeFile="../code/main.c"

Compiler="clang"
if command -v mold &> /dev/null; then Compiler="mold --run $Compiler"; fi

# TODO: generate assembly
Output="-o $AppName"
Warnings="-Wall -Werror -Wno-missing-braces -Wno-unused-function"

CompilerFlags="$Output $Warnings -std=c17 -pedantic -fno-gnu-keywords"
CompilerDebug="$CompilerFlags -O0 -g"
# TODO: openmp seems to be a major cause of slowdown, especially when printing progress
CompilerRelease="$CompilerFlags -ffast-math -O2" # -fopenmp"

LinkerFlags="-L../libs/ -lm"
LinkerDebug="$LinkerFlags -debug"
LinkerRelease="$LinkerFlags"



if [ ! -d "build" ]; then mkdir build; fi
pushd build

if [ "$1" = "-r" ]; then
	$Compiler $CodeFile $CompilerRelease $LinkerRelease
else
	$Compiler $CodeFile $CompilerDebug $LinkerDebug
fi

popd
