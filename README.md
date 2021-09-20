# rtic
ray tracer in c

## so far
- spheres, planes, triangles
- diffuse, metal, dielectric materials
- multi-sampling (naive sampling tbh)
- png output
- command line config (set width, height, samples, depth, world file)
- loads world info from file (see example/test.world)

## building
1. make a build directory
2. `cd` into build directory
3. compile `../build.c`
4. now you can run the build executable
	- pass `-r` for release build
	- pass `-e` to run app after build
	- pass `-mold` to use the mold linker
	- pass `-[msvc|clang|gcc|mingw]` to build using specific toolchain

- A basic CMakeLists.txt file is provided
