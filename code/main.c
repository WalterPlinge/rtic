#define _CRT_SECURE_NO_WARNINGS
#include <iso646.h>
#include <omp.h>
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



#define PI 3.14159265358979323846



#define BETWEEN( x, a, b ) ( (a) < (x) and (x) < (b) )

#define CLAMP(   x, a, b ) min( max( (x), (a) ), (b) )
#define CLAMP01( x )          CLAMP( (x),  0,     1  )



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



internal real
Random (
) {
#if 1
	return (real) rand() / ( RAND_MAX + 1 );
#else
	persistent real Temp;
	persistent int  RandomC = 0;

	real p = (real) ++RandomC;
	p = modf( p * .1031, &Temp );
	p *= p + 33.33;
	p *= p + p;
	return modf( p, &Temp );
#endif
}

internal real
RandomRange (
	real Min,
	real Max
) {
	return Min + ( Max - Min ) * Random();
}



internal real DegToRad ( real Degrees ) { return ( Degrees * PI ) / 180.0; }
internal real RadToDeg ( real Radians ) { return ( Radians * 180.0 ) / PI; }


internal real
ReflectanceSchlick (
	real Cosine,
	real Index
) {
	real r0 = ( 1 - Index ) / ( 1 + Index );
	r0 = r0 * r0;
	return r0 + ( 1 - r0 ) * pow( 1 - Cosine, 5 );
}



union v3 { real e[3]; struct { real X, Y, Z; }; struct { real R, G, B; }; } typedef v3;
v3 typedef colour;

internal bool NearZero  ( v3   A                 ) { return A.X < 1e-9 and A.Y < 1e-9 and A.Z < 1e-9;    }
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
internal v3   Sqrt      ( v3   A                 ) { return (v3) { sqrt(A.X) , sqrt(A.Y) , sqrt(A.Z)  }; }
internal real Dot       ( v3   A, v3   B         ) { return Sum( Mul( A, B ) ); }
internal v3   Cross     ( v3   A, v3   B         ) { return (v3) { A.Y*B.Z-A.Z*B.Y, A.Z*B.X-A.X*B.Z, A.X*B.Y-A.Y*B.X }; }
internal real Length2   ( v3   A                 ) { return Dot(      A, A   ); }
internal real Length    ( v3   A                 ) { return sqrt(    Length2( A ) ); }
internal v3   Normalise ( v3   A                 ) { return DivS( A, Length ( A ) ); }
internal v3   Lerp      ( v3   A, v3   B, real T ) { return Add( MulS( A, 1.0 - T ), MulS( B, T ) ); }
internal v3   Reflect   ( v3   A, v3   N         ) { return Sub( A, MulS( N, 2.0 * Dot( A, N ) ) ); }
internal v3   Refract   ( v3   A, v3   N, real E ) {
	real CosTheta = Dot( Negate( A ), N );
	v3   OutPerp  = MulS( Add( A, MulS( N, CosTheta ) ) , E );
	v3   OutPara  = MulS( N, -sqrt( fabs( 1.0 - Length2( OutPerp ) ) ) );
	return Add( OutPerp, OutPara );
}

internal v3 RandomV3() { return (v3) { Random(), Random(), Random() }; }
internal v3 RandomRangeV ( real Min, real Max ) {
	return (v3) {
		RandomRange( Min, Max ),
		RandomRange( Min, Max ),
		RandomRange( Min, Max ),
	};
}
internal v3 RandomInUnitSphere() {
	while ( true ) {
		v3 p = { RandomRange( -1.0, 1.0 ), RandomRange( -1.0, 1.0 ), RandomRange( -1.0, 1.0 ) };
		if ( Length2( p ) >= 1.0 ) continue;
		return p;
	}
}
internal v3 RandomInHemisphere( v3 Normal ) {
	v3 p = RandomInUnitSphere();
	if ( Dot( p, Normal ) > 0.0 ) {
		return p;
	} else {
		return Negate( p );
	}
}
internal v3 RandomUnitVector() { return Normalise( RandomInUnitSphere() ); }



struct ray { v3 Pos, Dir; } typedef ray;

internal ray NewRay       ( v3  Pos, v3   Dir ) { return (ray) {     Pos, Normalise(     Dir      ) }; }
internal v3  PointOnRayAt ( ray Ray, real T   ) { return  Add  ( Ray.Pos, MulS     ( Ray.Dir,  T  ) ); }



enum material_type { Lambertian, Metal, Dielectric } typedef material_type;

struct material { material_type Type; colour Albedo; real Roughness; real IndexOfRefraction; } typedef material;



struct hit_info { real Distance; v3 Point, Normal; bool FrontFace; material Material; } typedef hit_info;

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



internal bool
Scatter (
	ray      Ray,
	hit_info Info,
	colour*  Attenuation,
	ray*     Scattered
) {
	switch ( Info.Material.Type ) {
		case Lambertian: {
			v3 Dir = Add( Info.Normal, RandomUnitVector() );
			while ( NearZero( Dir ) ) {
				Dir = Add( Info.Normal, RandomUnitVector() );
			}
			*Scattered   = NewRay( Info.Point, Dir );
			*Attenuation = Info.Material.Albedo;
			return true;
		} break;

		case Metal: {
			v3 Reflected = Reflect( Ray.Dir, Info.Normal );
			v3 Direction = Add( Reflected, MulS( RandomInUnitSphere(), Info.Material.Roughness ) );
			*Scattered   = NewRay( Info.Point, Direction );
			*Attenuation = Info.Material.Albedo;
			return Dot( Scattered->Dir, Info.Normal ) > 0.0;
		} break;

		case Dielectric: {
			*Attenuation      = (colour) { 1.0, 1.0, 1.0 };
			real RefractRatio = Info.FrontFace
				? ( 1.0 / Info.Material.IndexOfRefraction )
				: Info.Material.IndexOfRefraction;

			real CosTheta = Dot( Negate( Ray.Dir ), Info.Normal );
			real SinTheta = sqrt( 1.0 - CosTheta * CosTheta );

			bool CannotRefract = RefractRatio * SinTheta > 1.0;

			v3 Direction;
			if ( CannotRefract or ReflectanceSchlick( CosTheta, RefractRatio ) > Random() ) {
				Direction = Reflect( Ray.Dir, Info.Normal );
			} else {
				Direction = Refract( Ray.Dir, Info.Normal, RefractRatio );
			}
			*Scattered = NewRay( Info.Point, Direction );
			return true;
		} break;
	}

	return false;
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

	Info->Distance = Root;
	Info->Point    = PointOnRayAt( Ray, Root );
	SetFaceNormal( Info, Ray, DivS( Sub( Info->Point, Sphere.Center ), Sphere.Radius ) );
	return true;
}



struct world { sphere* Spheres; material* SphereMaterials; } typedef world;

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
			Hit          = true;
			Closest      = Tmp.Distance;
			Tmp.Material = World.SphereMaterials[i];
			*Info        = Tmp;
		}
	}

	return Hit;
}

internal world
AWholeNewWorld (
) {
	world World = {
		.Spheres = NULL,
	};

	material GroundMat = { .Albedo = { 0.8, 0.8, 0.0 }, .Type = Lambertian };
	material CenterMat = { .Albedo = { 0.1, 0.2, 0.5 }, .Type = Lambertian };
	material LeftMat   = { .Type = Dielectric, .IndexOfRefraction = 1.5 };
	material RightMat  = { .Albedo = { 0.8, 0.6, 0.2 }, .Type = Metal, .Roughness = 0.0 };

	sphere GroundSphere = { .Center = {  0.0, 1.0, -100.5 }, .Radius =  100.0  };
	sphere CenterSphere = { .Center = {  0.0, 1.0,    0.0 }, .Radius =    0.5  };
	sphere LeftSphere   = { .Center = { -1.0, 1.0,    0.0 }, .Radius =    0.5  };
	sphere LeftSphereIn = { .Center = { -1.0, 1.0,    0.0 }, .Radius = -  0.45 };
	sphere RightSphere  = { .Center = {  1.0, 1.0,    0.0 }, .Radius =    0.5  };

	arrput( World.Spheres        , GroundSphere );
	arrput( World.SphereMaterials, GroundMat    );
	arrput( World.Spheres        , CenterSphere );
	arrput( World.SphereMaterials, CenterMat    );
	arrput( World.Spheres        , LeftSphere   );
	arrput( World.SphereMaterials, LeftMat      );
	arrput( World.Spheres        , LeftSphereIn );
	arrput( World.SphereMaterials, LeftMat      );
	arrput( World.Spheres        , RightSphere  );
	arrput( World.SphereMaterials, RightMat     );

	return World;
}



internal colour
RayColour (
	ray   Ray,
	world World,
	int   Depth
) {
	persistent real Near = 0.0001;
	persistent real Far  = 1000.0;

	if ( Depth <= 0 ) {
		return (colour) { 0.0, 0.0, 0.0 };
	}

	hit_info Info = { 0 };
	if ( not HitWorld( Ray, World, Near, Far, &Info ) ) {
		persistent colour Sky1 = { .R = 1.0, .G = 0.7, .B = 0.5 };
		persistent colour Sky2 = { .R = 0.5, .G = 0.7, .B = 1.0 };
		real T = ( Ray.Dir.Z + 1.0 ) / 2.0;
		return Lerp( Sky1, Sky2, T );
	}

	// Render normals
	//return DivS( AddS( Info.Normal, 1.0), 2.0 );

	//v3 Target = Add( Info.Point, RandomInHemisphere( Info.Normal ) ); // Old papers
	//v3 Target = Add( Add( Info.Point, Info.Normal ), RandomInUnitSphere() ); // Inaccurate lambertian
	//v3 Target = Add( Add( Info.Point, Info.Normal ), RandomUnitVector() );
	//v3 Dir    = Sub( Target, Info.Point );
	//return DivS( RayColour( NewRay( Info.Point, Dir ), World, Depth - 1 ), 2.0 );

	ray Scattered;
	colour Attenuation;
	if ( not Scatter( Ray, Info, &Attenuation, &Scattered ) ) {
		return (colour) { 0.0, 0.0, 0.0 };
	}

	return Mul( Attenuation, RayColour( Scattered, World, Depth - 1 ) );
}



struct camera { v3 Pos, Width, Height, TopLeft; } typedef camera;

internal camera
NewCamera (
	v3   Position,
	v3   Target,
	v3   WorldUp,
	real VerticalFieldOfView,
	real AspectRatio
) {
	real Theta = DegToRad( VerticalFieldOfView );
	real H     = tan( Theta / 2.0 );

	real ViewportHeight = 2.0 * H;
	real ViewportWidth  = AspectRatio * ViewportHeight;

	// Normals
	v3 Forward = Normalise( Sub  ( Target , Position    ) );
	v3 Right   = Normalise( Cross( Forward, WorldUp     ) );
	v3 Up      =            Cross( Right  , Forward     )  ;

	v3 Width   = MulS( Right  , ViewportWidth  );
	v3 Height  = MulS( Up     , ViewportHeight );

	// Top Left = Position - Half Width + Half Height + Forward
	v3 TopLeft = Position;
	   TopLeft = Sub( TopLeft, DivS( Width  , 2.0         ) );
	   TopLeft = Add( TopLeft, DivS( Height , 2.0         ) );
	   TopLeft = Add( TopLeft,       Forward                );

	return (camera) {
		.Pos     = Position,
		.Width   = Width,
		.Height  = Height,
		.TopLeft = TopLeft,
	};
}

internal ray
CameraRay (
	camera Cam,
	real   U,
	real   V
) {
	// Direction = Top Left + U * Width - V * Height - Origin
	v3 Dir = Cam.TopLeft;
	   Dir = Add( Dir, MulS( Cam.Width , U ) );
	   Dir = Sub( Dir, MulS( Cam.Height, V ) );
	   Dir = Sub( Dir,       Cam.Pos         );
	return NewRay( Cam.Pos, Dir );
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
	persistent int Samples = 1 << 3;

	camera Cam   = NewCamera(
		(v3) { -2.0, -1.0, 2.0 },
		(v3) {  0.0,  1.0, 0.0 },
		(v3) {  0.0,  0.0, 1.0 },
		20.0, (real) Image.Width / Image.Height );
	int MaxDepth = 10;

	byte* Row = Image.Buffer;
	for ( int y = 0;
		y < Image.Height;
		y += 1, Row += Image.Pitch // NOTE: increment row by pitch
	) {
		printf_s( "\rProgress: %.0f%% ", ( 100.0 * y ) / Image.Height );
		u4* Pixel = (u4*) Row;
		for ( int x = 0;
			x < Image.Width;
			x += 1, Pixel += 1 // NOTE: increment pixel
		) {
			colour PixelColour = { 0 };

			int sx;
			//#pragma omp parallel for
			for ( sx = 0; sx < Samples; ++sx )
			for ( int sy = 0; sy < Samples; ++sy ) {
				real su = (real) sx / Samples;
				real sv = (real) sy / Samples;

				real u = ( x + su ) / ( Image.Width  - 1 );
				real v = ( y + sv ) / ( Image.Height - 1 );

				ray  r = CameraRay( Cam, u, v );

				PixelColour = Add( PixelColour, RayColour( r, World, MaxDepth ) );
			}

			PixelColour = DivS( PixelColour, Samples * Samples );
			PixelColour = Sqrt( PixelColour );

			u1 Red   = (u1) ( 255.999 * CLAMP01( PixelColour.R ) );
			u1 Green = (u1) ( 255.999 * CLAMP01( PixelColour.G ) );
			u1 Blue  = (u1) ( 255.999 * CLAMP01( PixelColour.B ) );
			u1 Alpha = UCHAR_MAX;
			*Pixel   = Red   << 0
			         | Green << 8
			         | Blue  << 16
			         | Alpha << 24;
		}
	}
	puts( "\rProgress: DONE" );
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
