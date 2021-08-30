# rtic
ray tracer in c

## so far
- spheres
- diffuse, metal, dielectric materials
- multi-sampling (naive sampling tbh)
- openmp multithreading
- png output
- command line config (set width, height, samples, depth)
- loads world info from file (see build/test.world)

## building
- platform specific:
	1. run `build.bat` or `build.sh` for debug build
	- pass `-r` for release build
- platform agnostic
	1. `cd` into build directory
	2. compile `../build.c`
	3. run resulting binary to build
	- pass `-r` for release build
	- pass `-e` to run app after build
	- pass `-mold` to use the mold linker
	- pass `-<msvc|clang|mingw>` to build using specific toolchain
