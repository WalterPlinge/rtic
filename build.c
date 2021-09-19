#define _CRT_SECURE_NO_WARNINGS
#include <iso646.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define global static

#define DEBUG_STRING(String) printf("[%zi] %s\n", strlen(String), String);
#define STRING_EQUAL(A, B) (strcmp(A, B) == 0)



#define APP_NAME  "rtic"
#define CODE_FILE "../code/main.c"



enum TOOLCHAIN {
	TOOLCHAIN_MSVC,
	TOOLCHAIN_CLANG,
	TOOLCHAIN_GCC,
	TOOLCHAIN_MINGW,
	TOOLCHAIN_PELLESC,
	TOOLCHAIN_COUNT,
} typedef TOOLCHAIN;

#if defined(_WIN32)
	#define DEFAULT_TOOLCHAIN TOOLCHAIN_MSVC
#else
	#define DEFAULT_TOOLCHAIN TOOLCHAIN_CLANG
#endif



enum COMMAND_TYPE {
	COMMAND_COMPILER,
	COMMAND_CODE_FILE,
	COMMAND_OUTPUT,
	COMMAND_WARNINGS,
	COMMAND_FLAGS,
	COMMAND_DEBUG_FLAGS,
	COMMAND_RELEASE_FLAGS,
	COMMAND_LINKER,
	COMMAND_DEBUG_LINKER,
	COMMAND_RELEASE_LINKER,
	COMMAND_COUNT,
} typedef COMMAND_TYPE;

global char* Strings[TOOLCHAIN_COUNT][COMMAND_COUNT];



int main(int argc, char** argv) {
	bool Release   = false;
	bool Execute   = false;
	bool Mold      = false;
	int  Toolchain = DEFAULT_TOOLCHAIN;

	for (int a = 1 ; a < argc ; ++a) {
		char* arg = argv[a];
		if (STRING_EQUAL("-r", arg)) {
			Release = true;
		} else
		if (STRING_EQUAL("-e", arg)) {
			Execute = true;
		} else
		if (STRING_EQUAL("-mold", arg)) {
			Mold = true;
		} else
		if (STRING_EQUAL("-msvc", arg)) {
			Toolchain = TOOLCHAIN_MSVC;
		} else
		if (STRING_EQUAL("-clang", arg)) {
			Toolchain = TOOLCHAIN_CLANG;
		} else
		if (STRING_EQUAL("-gcc", arg)) {
			Toolchain = TOOLCHAIN_GCC;
		} else
		if (STRING_EQUAL("-mingw", arg)) {
			Toolchain = TOOLCHAIN_MINGW;
		} else
		if (strcmp("-pellesc", arg) == 0) {
			Toolchain = TOOLCHAIN_PELLESC;
		} else
		{
			printf(
				"build:\n"
				"\t-r           release\n"
				"\t-e           execute\n"
				"\t-mold        use mold linker\n"
				"\t-<toolchain> [ msvc* | clang* | gcc | mingw ]\n"
				"\t             (* default for [ windows | other ])\n"
				);
			return 0;
		}
	}

	char Command[0xFFF] = {0};
	if (Mold) strcat(Command, "mold --run ");
	strcat(Command, Strings[Toolchain][COMMAND_COMPILER ]);
	strcat(Command, Strings[Toolchain][COMMAND_CODE_FILE]);
	strcat(Command, Strings[Toolchain][COMMAND_OUTPUT   ]);
	strcat(Command, Strings[Toolchain][COMMAND_WARNINGS ]);
	strcat(Command, Strings[Toolchain][COMMAND_FLAGS    ]);
	strcat(Command, Strings[Toolchain][Release ? COMMAND_RELEASE_FLAGS  : COMMAND_DEBUG_FLAGS ]);
	strcat(Command, Strings[Toolchain][COMMAND_LINKER   ]);
	strcat(Command, Strings[Toolchain][Release ? COMMAND_RELEASE_LINKER : COMMAND_DEBUG_LINKER]);

	DEBUG_STRING(Command);

	system(Command);

	if (Execute) {
		#if defined(_WIN32)
			system(APP_NAME ".exe");
		#else
			if (Toolchain == TOOLCHAIN_MINGW) {
				system(APP_NAME ".exe");
			} else {
				system("./"APP_NAME);
			}
		#endif
	}

	return 0;
}



global char* Strings[TOOLCHAIN_COUNT][COMMAND_COUNT] = {
	[TOOLCHAIN_MSVC] = {
		[COMMAND_COMPILER] = ""
			" cl", // msvc
		[COMMAND_CODE_FILE] = ""
			" " CODE_FILE,
		[COMMAND_OUTPUT] = ""
			" -FAsu"        // generate assembly with utf8 source
			" -Fa" APP_NAME // output assembly name
			" -Fe" APP_NAME // output executable name
			" -Fm" APP_NAME // output map file name
			" -Fo" APP_NAME // output object file name
			" -Zi"          // generate debug info
			,
		[COMMAND_WARNINGS] = ""
			" -WX"     // treat all warnings as errors
			" -W4"     // warning level 4
			" -wd4668" // 'symbol' is not defined as a preprocessor macro, replacing with '0' for 'directives'
			" -wd4820" // 'bytes' bytes padding added after construct 'member_name'
			" -wd5045" // Compiler will insert Spectre mitigation for memory load if /Qspectre switch specified
			,
		[COMMAND_FLAGS] = ""
			" -nologo"  // Suppresses display of sign-on banner.
			//" -analyze" // Enables code analysis.
			" -EHa-"    // Disables exception handling
			" -FC"      // Displays the full path of source code files passed to cl.exe in diagnostic text.
			" -GL"      // Enables whole program optimization.
			" -Gm-"     // Disables minimal rebuild.
			" -GR-"     // Disables run-time type information (RTTI).
			" -GS-"     // Disables checking buffer security.
			" -Gw"      // Enables whole-program global data optimization.
			" -Gy"      // Enables function-level linking.
			" -std:c17" // Enables ISO C17 conformance
			" -TC"      // Specifies all source files are C.
			" -utf-8"   // Set source and execution character sets to UTF-8.
			,
		[COMMAND_DEBUG_FLAGS] = ""
			" -fp:precise" // Specifies how the compiler treats floating-point expressions, optimizations, and exceptions.
			" -MTd"        // Compiles to create a debug multithreaded executable file, by using LIBCMTD.lib.
			" -Oi"         // Generates intrinsic functions.
			,
		[COMMAND_RELEASE_FLAGS] = ""
			" -fp:fast" // Specifies how the compiler treats floating-point expressions, optimizations, and exceptions.
			" -MT"      // Compiles to create a multithreaded executable file, by using LIBCMT.lib.
			" -O2"      // Creates fast code.
			//" -openmp"  // Enables #pragma omp in source code.
			,
		[COMMAND_LINKER] = ""
			" -link"              // linker flag
			" -nologo"            // Suppresses the startup banner.
			" -incremental:no"    // Controls incremental linking.
			" -opt:ref,icf=4"     // Eliminates functions and data that are never referenced, perform identical COMDAT folding
			" -subsystem:console" // Tells the operating system how to run the .exe file.
			" -libpath:../libs/"  // Specifies a path to search before the environmental library path.
			,
		[COMMAND_DEBUG_LINKER] = ""
			" -debug:full" // moves all private symbol information from individual compilation products (object files and libraries) into a single PDB
			,
		[COMMAND_RELEASE_LINKER] = ""
			" -LTCG" // Specifies link-time code generation.
			,
	},
	[TOOLCHAIN_CLANG] = {
		[COMMAND_COMPILER ] = ""
			" clang", // clang with mold linker
		[COMMAND_CODE_FILE] = ""
			" " CODE_FILE,
		[COMMAND_OUTPUT] = ""
			" -o " APP_NAME // output executable name
			,
		[COMMAND_WARNINGS] = ""
			" -Werror"              // treat all warnings as errors
			" -Wall"                // all warnings (not all warnings)
			" -Wno-missing-braces"  //
			" -Wno-unused-function" //
			,
		[COMMAND_FLAGS] = ""
			" -std=c17"          // C17 language standard
			" -pedantic"         // ISO conformance
			" -fno-gnu-keywords" // disables gnu extensions
			,
		[COMMAND_DEBUG_FLAGS] = ""
			" -O0"
			" -g"
			,
		[COMMAND_RELEASE_FLAGS] = ""
			" -ffast-math" // enables fast math
			" -O3"         // level 3 optimisation
			//" -fopenmp"    // TODO: openmp seems to be a major cause of slowdown, especially when printing progress
			,
		[COMMAND_LINKER] = ""
			" -L../libs/" // add library path
			" -lm"        // link to maths library
			,
		[COMMAND_DEBUG_LINKER] = ""
			" -debug" // debug linking
			,
		[COMMAND_RELEASE_LINKER] = ""
			,
	},
	[TOOLCHAIN_GCC] = {
		[COMMAND_COMPILER ] = ""
			" gcc", // clang with mold linker
		[COMMAND_CODE_FILE] = ""
			" " CODE_FILE,
		[COMMAND_OUTPUT] = ""
			" -o " APP_NAME // output executable name
			,
		[COMMAND_WARNINGS] = ""
			" -Werror"              // treat all warnings as errors
			" -Wall"                // all warnings (not all warnings)
			" -Wno-missing-braces"  //
			" -Wno-unused-function" //
			" -Wno-unknown-pragmas" //
			,
		[COMMAND_FLAGS] = ""
			" -std=c17"          // C17 language standard
			" -pedantic"         // ISO conformance
			,
		[COMMAND_DEBUG_FLAGS] = ""
			" -O0"
			" -g"
			,
		[COMMAND_RELEASE_FLAGS] = ""
			" -ffast-math" // enables fast math
			" -O3"         // level 3 optimisation
			//" -fopenmp"    // openmp supprot
			,
		[COMMAND_LINKER] = ""
			" -L../libs/" // add library path
			" -lm"        // link to maths library
			,
		[COMMAND_DEBUG_LINKER] = ""
			,
		[COMMAND_RELEASE_LINKER] = ""
			,
	},
	[TOOLCHAIN_MINGW] = {
		[COMMAND_COMPILER ] = ""
			" x86_64-w64-mingw32-gcc", // clang with mold linker
		[COMMAND_CODE_FILE] = ""
			" " CODE_FILE,
		[COMMAND_OUTPUT] = ""
			" -o " APP_NAME // output executable name
			,
		[COMMAND_WARNINGS] = ""
			" -Werror"              // treat all warnings as errors
			" -Wall"                // all warnings (not all warnings)
			" -Wno-missing-braces"  //
			" -Wno-unused-function" //
			" -Wno-unknown-pragmas" //
			,
		[COMMAND_FLAGS] = ""
			" -std=c17"          // C17 language standard
			" -pedantic"         // ISO conformance
			,
		[COMMAND_DEBUG_FLAGS] = ""
			" -O0"
			" -g"
			,
		[COMMAND_RELEASE_FLAGS] = ""
			" -ffast-math" // enables fast math
			" -O3"         // level 3 optimisation
			//" -fopenmp"    // openmp support
			,
		[COMMAND_LINKER] = ""
			" -L../libs/" // add library path
			" -lm"        // link to maths library
			,
		[COMMAND_DEBUG_LINKER] = ""
			,
		[COMMAND_RELEASE_LINKER] = ""
			,
	},
	[TOOLCHAIN_PELLESC] = {
		[COMMAND_COMPILER] = ""
			" pocc"
			,
		[COMMAND_CODE_FILE] = ""
			" " CODE_FILE
			,
		[COMMAND_OUTPUT] = ""
			" -Fo" APP_NAME
			,
		[COMMAND_WARNINGS] = ""
			" -W2"    // Set warning level 0, 1 or 2 (default: 1)
			//" -Wd<n>" // Disable warning #n
			,
		[COMMAND_FLAGS] = ""
			" -arch:SSE2" // Select X64 architecture AVX, AVX2, or SSE2 (default: SSE2)
			" -GT"        // Generate fiber-safe access for thread local storage
			//" -I<path>"   // Add a search path for #include files
			//" -J"         // Default char type is unsigned
			//" -MT"        // Enable multi-threading support (CRTMT*.LIB)
			" -std:C17"   // Select language mode C17, C11 or C99 (default: C17)
			,
		[COMMAND_DEBUG_FLAGS] = ""
			//" -fp:PRECISE" // Set floating-point model PRECISE or FAST (default: PRECISE)
			" -Gi"         // Enable trap for signed integer overflow
			" -Zi"         // Enable full debugging information
			,
		[COMMAND_RELEASE_FLAGS] = ""
			" -fp:FAST" // Set floating-point model PRECISE or FAST (default: PRECISE)
			//" -openmp"  // Enable OpenMP 3.1 extensions
			" -Ot"      // Optimise (favouring: Os/Ot - space/speed)
			" -Ox"      // Perform maximum optimisations
			,
		[COMMAND_LINKER] = ""
			,
		[COMMAND_DEBUG_LINKER] = ""
			,
		[COMMAND_RELEASE_LINKER] = ""
			,
	},
};
