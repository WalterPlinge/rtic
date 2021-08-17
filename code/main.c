#define _CRT_SECURE_NO_WARNINGS
#include <iso646.h>
#include <omp.h>
#include <stdbool.h>
#include <stdint.h>
#include <tgmath.h>
#include <time.h>

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
#define CLAMP_01( x )         CLAMP( (x),  0,     1  )



#define TIMER_INIT() clock_t _timer = clock();
#define TIMER_STAMP(s) printf_s( s ": %.3fs\n", (real) ( clock() - _timer ) / CLOCKS_PER_SEC ); _timer = clock();



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

u4       typedef rgba8;



internal real Random     (                    ) { return (real) rand() / ( RAND_MAX + 1 ); }
internal real RandomRange( real Min, real Max ) { return Min + ( Max - Min ) * Random(  ); }



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
internal real Length2   ( v3   A                 ) { return Dot ( A,          A   ); }
internal real Length    ( v3   A                 ) { return sqrt(    Length2( A ) ); }
internal v3   Normalise ( v3   A                 ) { return DivS( A, Length ( A ) ); }
internal v3   Lerp      ( v3   A, v3   B, real T ) { return Add ( MulS( A, 1.0 - T ) , MulS( B, T )   ); }
internal v3   Reflect   ( v3   A, v3   N         ) { return Sub ( A,    MulS( N, 2.0 * Dot ( A, N ) ) ); }
internal v3   Refract   ( v3   A, v3   N, real E ) {
	real CosTheta = Dot ( Negate( A ),       N                               );
	v3   OutPerp  = MulS( Add   ( A  , MulS( N,    CosTheta           ) ), E );
	v3   OutPara  = MulS( N, -sqrt(    fabs( 1.0 - Length2( OutPerp ) ) )    );
	return Add( OutPerp, OutPara );
}

internal v3 RandomV     (                    ) { return (v3) { Random(), Random(), Random() }; }
internal v3 RandomRangeV( real Min, real Max ) {
	return (v3) {
		RandomRange( Min, Max ),
		RandomRange( Min, Max ),
		RandomRange( Min, Max ),
	};
}
internal v3 RandomInUnitSphere() { // HACK: infinite loop
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
internal v3 RandomInUnitDisc() { // HACK: infinite loop
	while ( true ) {
		v3 p = { RandomRange( -1.0, 1.0 ), RandomRange( -1.0, 1.0 ), 0.0 };
		if ( Length2( p ) >= 1.0 ) continue;
		return p;
	}
}



struct ray { v3 Pos, Dir; } typedef ray;

internal ray NewRay       ( v3  Pos, v3   Dir ) { return (ray) {     Pos, Normalise(     Dir      ) }; }
internal v3  PointOnRayAt ( ray Ray, real T   ) { return  Add  ( Ray.Pos, MulS     ( Ray.Dir,  T  ) ); }



enum material_type { Lambertian, Metal, Dielectric } typedef material_type;

struct material { material_type Type; colour Albedo; real Roughness; real RefractiveIndex; } typedef material;



struct hit_info { real Distance; v3 Point, Normal; bool FrontFace; material Material; } typedef hit_info;

internal void
SetFaceNormal (
	hit_info* Info,
	ray       Ray,
	v3        OutwardNormal
) {
	Info->FrontFace = Dot( Ray.Dir, OutwardNormal ) < 0.0;
	Info->Normal    = Info->FrontFace
		?         OutwardNormal
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
			while ( NearZero( Dir ) ) { // HACK: infinite loop
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
				? ( 1.0 / Info.Material.RefractiveIndex )
				:         Info.Material.RefractiveIndex;

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

internal void FreeTheWorld ( world World ) { arrfree( World.Spheres ); arrfree( World.SphereMaterials ); }

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
		.Spheres         = NULL,
		.SphereMaterials = NULL,
	};

#if 0
	material GroundMat = { .Albedo = { 0.8, 0.8, 0.0 }, .Type = Lambertian };
	material CenterMat = { .Albedo = { 0.1, 0.2, 0.5 }, .Type = Lambertian };
	material LeftMat   = { .Type = Dielectric, .RefractiveIndex = 1.5 };
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
#else
	sphere   GroundS = { .Center = { 0.0, 0.0, -1000.0 }, .Radius = 1000.0     };
	material GroundM = { .Albedo = { 0.5, 0.5,     0.5 }, .Type   = Lambertian };
	arrput( World.Spheres        , GroundS );
	arrput( World.SphereMaterials, GroundM );

	for ( int a = -11; a < 11; ++a )
	for ( int b = -11; b < 11; ++b ) {
		real ChooseMat = Random();

		material Mat;
		sphere   Sphere = {
			.Center = { a + 0.9 * Random(), b * 0.9 * Random(), 0.2 },
			.Radius = 0.2
		};

		if ( Length( Sub( Sphere.Center, (v3) { 4.0, 0.0, 0.2 } ) ) > 0.9 ) {
			if ( ChooseMat < 0.8 ) {
				Mat.Type   = Lambertian;
				Mat.Albedo = Mul( RandomV(), RandomV() );
			} else if ( ChooseMat < 0.95 ) {
				Mat.Type      = Metal;
				Mat.Albedo    = RandomRangeV( 0.5, 1.0 );
				Mat.Roughness = RandomRange ( 0.0, 0.5 );
			} else {
				Mat.Type            = Dielectric;
				Mat.RefractiveIndex = 1.5;
			}

			arrput( World.Spheres        , Sphere );
			arrput( World.SphereMaterials, Mat    );
		}
	}

	sphere   Sphere1   = { .Center = {  0.0, 0.0, 1.0 }, .Radius = 1.0 };
	material Material1 = { .Type = Dielectric,  .RefractiveIndex = 1.5 };
	arrput( World.Spheres        , Sphere1   );
	arrput( World.SphereMaterials, Material1 );

	sphere   Sphere2   = { .Center = { -4.0, 0.0, 1.0 }, .Radius = 1.0        };
	material Material2 = { .Albedo = {  0.4, 0.2, 0.1 }, .Type   = Lambertian };
	arrput( World.Spheres        , Sphere2   );
	arrput( World.SphereMaterials, Material2 );

	sphere   Sphere3   = { .Center = {  4.0, 0.0, 1.0 }, .Radius    = 1.0                };
	material Material3 = { .Albedo = {  0.7, 0.6, 0.5 }, .Roughness = 0.0, .Type = Metal };
	arrput( World.Spheres        , Sphere3   );
	arrput( World.SphereMaterials, Material3 );
#endif

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
		persistent colour Sky1 = { .R = 1.0, .G = 1.0, .B = 1.0 };
		persistent colour Sky2 = { .R = 0.5, .G = 0.7, .B = 1.0 };
		real T = ( Ray.Dir.Z + 1.0 ) / 2.0;
		return Lerp( Sky1, Sky2, T );
	}

	// Render normals
	//return DivS( AddS( Info.Normal, 1.0), 2.0 );

	// Only lambertian diffuse
	//v3 Target = Add( Info.Point, RandomInHemisphere( Info.Normal ) );        // Old papers
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



struct camera { v3 Pos, TopLeft, Width, Height, Right, Forward, Up; real LensRadius; } typedef camera;

internal camera
NewCamera (
	v3   Position,
	v3   Target,
	v3   WorldUp,
	real VerticalFieldOfView,
	real AspectRatio,
	real Aperture
) {
	real Theta = DegToRad( VerticalFieldOfView );
	real H     = tan     ( Theta     /     2.0 );

	real ViewportHeight = 2.0         * H             ;
	real ViewportWidth  = AspectRatio * ViewportHeight;

	v3   LookVector = Sub   ( Target    , Position );
	real FocusDist  = Length( LookVector           );

	// Normals
	v3 Forward = DivS     (        LookVector, FocusDist     );
	v3 Right   = Normalise( Cross( Forward   , WorldUp     ) );
	v3 Up      =            Cross( Right     , Forward     )  ;

	v3 Width   = MulS( Right, FocusDist * ViewportWidth  );
	v3 Height  = MulS( Up   , FocusDist * ViewportHeight );

	// Top Left = Position - Half Focus Width + Half Focus Height + Focus Forward
	v3 TopLeft = Position;
	   TopLeft = Sub( TopLeft, DivS( Width  , 2.0       ) );
	   TopLeft = Add( TopLeft, DivS( Height , 2.0       ) );
	   TopLeft = Add( TopLeft, MulS( Forward, FocusDist ) );

	return (camera) {
		.Pos        = Position,
		.TopLeft    = TopLeft,
		.Width      = Width,
		.Height     = Height,
		.Right      = Right,
		.Forward    = Forward,
		.Up         = Up,
		.LensRadius = Aperture / 2.0,
	};
}

internal ray
CameraRay (
	camera Cam,
	real   U,
	real   V
) {
	v3 rd     = MulS( RandomInUnitDisc()     ,       Cam.LensRadius );
	v3 Offset = Add ( MulS( Cam.Right, rd.X ), MulS( Cam.Up, rd.Y ) );
	// Direction = Top Left + U * Width - V * Height - Origin - Offset
	v3 Pos = Add( Cam.Pos, Offset );
	v3 Dir = Cam.TopLeft;
	   Dir = Add( Dir, MulS( Cam.Width , U ) );
	   Dir = Sub( Dir, MulS( Cam.Height, V ) );
	   Dir = Sub( Dir,       Cam.Pos         );
	   Dir = Sub( Dir,       Offset          );
	return NewRay( Pos, Dir );
}



// NOTE: Always 4 bytes in RR GG BB AA order
struct image_buffer { rgba8* Buffer; int Width, Height, Pitch; } typedef image_buffer;



internal void
RenderWorld (
	image_buffer Image,
	world        World
) {
	v3     Position   = { 13.0, -3.0, 2.0 };
	v3     Target     = {  0.0,  0.0, 0.0 };
	v3     WorldUp    = {  0.0,  0.0, 1.0 };
	real   vfov       = 20.0;
	real   Aspect     = (real) Image.Width / Image.Height;
	real   Aperture   = 0.2;
	camera Cam        = NewCamera( Position, Target, WorldUp, vfov, Aspect, Aperture );

	int Samples  = 1 << 1;
	int MaxDepth = 1 << 3;

	int y;
	#pragma omp parallel for
	for ( y = 0; y < Image.Height; y += 1 ) {
		persistent int Complete = 0;
		printf_s( "\rProgress: %.0f%% ", ( 100.0 * ++Complete ) / Image.Height );

		rgba8* Row  = (u4*) ( (byte*) Image.Buffer + y * Image.Pitch );

		for ( int x = 0; x < Image.Width; x += 1 ) {
			rgba8* Pixel = Row + x;

			colour PixelColour = { 0 };

			for ( int sx = 0; sx < Samples; ++sx )
			for ( int sy = 0; sy < Samples; ++sy ) {
				real su = (real) sx   /   Samples;
				real sv = (real) sy   /   Samples;
				real  u = ( x +  su ) / ( Image.Width  - 1 );
				real  v = ( y +  sv ) / ( Image.Height - 1 );
				ray   r = CameraRay( Cam, u, v );

				PixelColour = Add( PixelColour, RayColour( r, World, MaxDepth ) );
			}

			PixelColour = DivS( PixelColour, Samples * Samples );
			PixelColour = Sqrt( PixelColour );

			u1 Red   = (u1) ( 255.999 * CLAMP_01( PixelColour.R ) );
			u1 Green = (u1) ( 255.999 * CLAMP_01( PixelColour.G ) );
			u1 Blue  = (u1) ( 255.999 * CLAMP_01( PixelColour.B ) );
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

	srand( 2 );

	TIMER_INIT();

	#define WIDTH  256 * 2
	#define HEIGHT 144 * 2
	#define RGBA     4

	image_buffer Image = {
		.Buffer = malloc( WIDTH * HEIGHT * RGBA ),
		.Width  = WIDTH,
		.Height = HEIGHT,
		.Pitch  = WIDTH * RGBA
	};

	world World = AWholeNewWorld();

	TIMER_STAMP( "INIT  " );

	RenderWorld( Image, World );

	TIMER_STAMP( "RENDER" );

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

	TIMER_STAMP( "SAVE  " );


	FreeTheWorld( World );
	free( Image.Buffer );

	puts( "END" );
}
