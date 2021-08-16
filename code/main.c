#define _CRT_SECURE_NO_WARNINGS
#include <iso646.h>
#include <stdbool.h>
#include <stdint.h>
#include <tgmath.h>

#define STB_IMAGE_WRITE_IMPLEMENTATION
#define STBIW_WINDOWS_UTF8
#include "stb_image_write.h"
#define STB_DS_IMPLEMENTATION
#include "stb_ds.h"



#define global     static
#define internal   static inline
#define persistent static



#define BETWEEN( x, a, b ) ( (a) < (x) and (x) < (b) )



int_least64_t  typedef s8;
int_least32_t  typedef s4;
int_least16_t  typedef s2;
int_least8_t   typedef s1;

uint_least64_t typedef u8;
uint_least32_t typedef u4;
uint_least16_t typedef u2;
uint_least8_t  typedef u1;

u1       typedef byte;

double   typedef f8; // 8 bytes
float    typedef f4; // 4 bytes
double   typedef f2; // double precision
float    typedef f1; // single precision

f2       typedef real; // easy to swap precision



union v3 { real e[3]; struct { real X, Y, Z; }; struct { real R, G, B; }; } typedef v3;
v3 typedef colour;

internal real Sum       ( v3   A                 ) { return         A.X +       A.Y +       A.Z        ; }
internal v3   Negate    ( v3   A                 ) { return (v3) { -A.X      , -A.Y      , -A.Z       }; }
internal v3   Add       ( v3   A, v3   B         ) { return (v3) {  A.X + B.X,  A.Y + B.Y,  A.Z + B.Z }; }
internal v3   Sub       ( v3   A, v3   B         ) { return (v3) {  A.X - B.X,  A.Y - B.Y,  A.Z - B.Z }; }
internal v3   Mul       ( v3   A, v3   B         ) { return (v3) {  A.X * B.X,  A.Y * B.Y,  A.Z * B.Z }; }
internal v3   Div       ( v3   A, v3   B         ) { return (v3) {  A.X / B.X,  A.Y / B.Y,  A.Z / B.Z }; }
internal v3   AddS      ( v3   A, real B         ) { return (v3) {  A.X + B  ,  A.Y + B  ,  A.Z + B   }; }
internal v3   SubS      ( v3   A, real B         ) { return (v3) {  A.X - B  ,  A.Y - B  ,  A.Z - B   }; }
internal v3   MulS      ( v3   A, real B         ) { return (v3) {  A.X * B  ,  A.Y * B  ,  A.Z * B   }; }
internal v3   DivS      ( v3   A, real B         ) { return (v3) {  A.X / B  ,  A.Y / B  ,  A.Z / B   }; }
internal real Dot       ( v3   A, v3   B         ) { return Sum( Mul( A, B ) ); }
internal real Length2   ( v3   A                 ) { return Dot(      A, A   ); }
internal real Length    ( v3   A                 ) { return sqrt(    Length2( A ) ); }
internal v3   Normalise ( v3   A                 ) { return DivS( A, Length ( A ) ); }
internal v3   Lerp      ( v3   A, v3   B, real T ) { return Add( MulS( A, 1.0 - T ), MulS( B, T ) ); }



struct ray { v3 Pos, Dir; } typedef ray;

internal ray NewRay       ( v3  Pos, v3   Dir ) { return (ray) {     Pos, Normalise(     Dir      ) }; }
internal v3  PointOnRayAt ( ray Ray, real T   ) { return  Add  ( Ray.Pos, MulS     ( Ray.Dir,  T  ) ); }



struct hit_info { real Distance; v3 Point, Normal; bool FrontFace; } typedef hit_info;

internal void
SetFaceNormal (
	hit_info* Info,
	ray       Ray,
	v3        OutwardNormal
) {
	Info->FrontFace = Dot( Ray.Dir, OutwardNormal ) < 0.0;
	Info->Normal    = Info->FrontFace
		? OutwardNormal
		: Negate( OutwardNormal );
}



struct sphere { v3 Center; real Radius; } typedef sphere;

internal bool
HitSphere (
	ray       Ray,
	sphere    Sphere,
	real      Near,
	real      Far,
	hit_info* Info
) {
	v3   p = Sub( Sphere.Center, Ray.Pos );
	real m = Dot( Ray.Dir, p );
	real c = Dot( p, p ) - Sphere.Radius * Sphere.Radius;
	real d = m * m - c;
	if ( d < 0.0 ) {
		return false;
	}
	d = sqrt( d );
	real Root = m - d;
	if ( not BETWEEN( Root, Near, Far ) ) {
		Root = m + d;
		if ( not BETWEEN( Root, Near, Far ) ) {
			return false;
		}
	}

	Info->Distance   = Root;
	Info->Point      = PointOnRayAt( Ray, Root );
	SetFaceNormal( Info, Ray, DivS( Sub( Info->Point, Sphere.Center ), Sphere.Radius ) );
	return true;
}



struct world { sphere* Spheres; } typedef world;

internal void FreeTheWorld ( world World ) { arrfree( World.Spheres ); }

internal bool
HitWorld (
	ray       Ray,
	world     World,
	real      Near,
	real      Far,
	hit_info* Info
) {
	hit_info Tmp = { 0 };
	bool     Hit = false;
	real Closest = Far;

	for ( int i = 0; i < arrlen( World.Spheres ); ++i ) {
		if ( HitSphere( Ray, World.Spheres[i], Near, Closest, &Tmp ) ) {
			Hit = true;
			Closest = Tmp.Distance;
			*Info = Tmp;
		}
	}

	return Hit;
}



internal colour
RayColour (
	ray   Ray,
	world World
) {
	persistent real Near = 0.0001;
	persistent real Far  = 1000.0;

	hit_info Info = { 0 };
	if ( HitWorld( Ray, World, Near, Far, &Info ) ) {
		return MulS( AddS( Info.Normal, 1.0), 0.5 );
	}

	persistent colour Sky1 = { .R = 1.0, .G = 0.7, .B = 0.5 };
	persistent colour Sky2 = { .R = 0.5, .G = 0.7, .B = 1.0 };
	real T = 0.5 * ( Ray.Dir.Z + 1.0 );
	return Lerp( Sky1, Sky2, T );
}



struct image_buffer {
	// NOTE: Always 4 bytes in RR GG BB AA order
	void* Buffer;
	int   Width;
	int   Height;
	int   Pitch;
} typedef image_buffer;

internal void
RenderWorld (
	image_buffer Image,
	world        World
) {
	real AspectRatio    = (real) Image.Width / Image.Height;
	real ViewportHeight = 2.0;
	real ViewportWidth  = AspectRatio * ViewportHeight;
	real FocalLength    = 1.0;

	v3 Origin  = { 0.0           , 0.0         , 0.0            };
	v3 Right   = { ViewportWidth , 0.0         , 0.0            };
	v3 Up      = { 0.0           , 0.0         , ViewportHeight };
	v3 Forward = { 0.0           , FocalLength , 0.0            };

	// Top Left = Origin - Half Right + Half Up + Forward
	v3 TopLeft   = Origin;
	   TopLeft   = Sub( TopLeft, DivS( Right  , 2.0 ) );
	   TopLeft   = Add( TopLeft, DivS( Up     , 2.0 ) );
	   TopLeft   = Add( TopLeft,       Forward        );

	byte* Row = Image.Buffer;
	for ( int y = 0;
		y < Image.Height;
		y += 1, Row += Image.Pitch // NOTE: increment row by pitch
	) {
		u4* Pixel = (u4*) Row;
		for ( int x = 0;
			x < Image.Width;
			x += 1, Pixel += 1 // NOTE: increment pixel
		) {
			real u = (real) x / ( Image.Width  - 1 );
			real v = (real) y / ( Image.Height - 1 );
			// Direction = Top Left + U * Right - V * Up - Origin
			v3   d = TopLeft;
			     d = Add( d, MulS( Right , u ) );
			     d = Sub( d, MulS( Up    , v ) );
			     d = Sub( d,       Origin      );
			ray  r = NewRay( Origin, d );

			colour PixelColour = RayColour( r, World );

			u1 Red   = (u1) ( 255.999 * PixelColour.R );
			u1 Green = (u1) ( 255.999 * PixelColour.G );
			u1 Blue  = (u1) ( 255.999 * PixelColour.B );
			u1 Alpha = UCHAR_MAX;
			*Pixel   = Red   << 0
			         | Green << 8
			         | Blue  << 16
			         | Alpha << 24;
		}
	}
}



internal world
AWholeNewWorld (
) {
	world World = {
		.Spheres = NULL,
	};

	for ( int i = 0; i < 3; i += 1 ) {
		sphere Sphere = { .Center = { -1.0 + (real) i, 1.0, 0.0 }, .Radius = 0.5 };
		arrput( World.Spheres, Sphere );
	}

	return World;
}



int
main (
) {
	puts( "START" );

	#define WIDTH 256
	#define HEIGHT 144
	#define RGBA 4

	image_buffer Image = {
		.Buffer = malloc( WIDTH * HEIGHT * RGBA ),
		.Width  = WIDTH,
		.Height = HEIGHT,
		.Pitch  = WIDTH * RGBA
	};

	world World = AWholeNewWorld();

	puts( "RENDER" );

	RenderWorld( Image, World );

	puts( "SAVE" );

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

	puts( "END" );

	FreeTheWorld( World );
	free( Image.Buffer );
}
