#define STB_IMAGE_WRITE_IMPLEMENTATION
#define STBIW_WINDOWS_UTF8
#include "stb_image_write.h"

#include "types.h"



struct image_buffer {
	// NOTE: Always 4 bytes in RR GG BB AA order
	void* Buffer;
	int   Width;
	int   Height;
	int   Pitch;
} typedef image_buffer;

internal void
RenderGradient (
	image_buffer Image
) {
	byte* Row = Image.Buffer;
	for ( int y = 0;
		y < Image.Height;
		y += 1, Row += Image.Pitch // NOTE: increment row by pitch
	) {
		u4* Pixel = (u4*) Row;
		for( int x = 0;
			x < Image.Width;
			x += 1, Pixel += 1 // NOTE: increment pixel
		) {
			u1 Alpha = UCHAR_MAX;
			u1 Red   = x;
			u1 Green = y;
			u1 Blue  = 1 << 4;
			*Pixel   = Red   << 0
			         | Green << 8
			         | Blue  << 16
			         | Alpha << 24;
		}
	}
}



int
main (
) {
	#define WIDTH 200
	#define HEIGHT 100
	#define RGBA 4
	byte Buffer[ WIDTH * HEIGHT * RGBA ] = {0};

	image_buffer Image = {
		.Buffer = Buffer,
		.Width  = WIDTH,
		.Height = HEIGHT,
		.Pitch  = WIDTH * RGBA
	};

	RenderGradient( Image );

	{ // Save buffer to PNG file
		byte Filename[ UCHAR_MAX ];
		stbiw_convert_wchar_to_utf8(
			Filename, sizeof( Filename ),
			L"output.png" );
		stbi_write_png(
			Filename,
			Image.Width, Image.Height,
			RGBA,
			Image.Buffer,
			Image.Pitch );
	}
}
