#define _CRT_SECURE_NO_WARNINGS
#define _UNICODE
#include <iso646.h>
#include <limits.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#define STB_IMAGE_WRITE_IMPLEMENTATION
#include "stb_image_write.h"
#define STB_DS_IMPLEMENTATION
#include "stb_ds.h"



#define global     static
#define internal   static
#define persistent static



#define EPS 1e-9
#define PI  3.14159265358979323846



#define BETWEEN( V, A, B ) ( (A) < (V) and (V) < (B) )

#define CLAMP(    V, A, B )  fmin( fmax( (V), (A) ), (B) )
#define CLAMP_01( V       ) CLAMP(       (V),  0   ,  1  )

#define NEAR_ZERO( V ) BETWEEN( V, -EPS, EPS )

#define STRINGS_EQUAL( A, B ) ( strcmp( A, B ) == 0 )



#define TIMER_INIT() clock_t _Timer = clock();
#define TIMER_STAMP( Str ) printf( "%s: %.3fs\n", Str, (real) ( clock() - _Timer ) / CLOCKS_PER_SEC ); _Timer = clock();



int_least64_t  typedef s8;
int_least32_t  typedef s4;
int_least16_t  typedef s2;
int_least8_t   typedef s1;

uint_least64_t typedef u8;
uint_least32_t typedef u4;
uint_least16_t typedef u2;
uint_least8_t  typedef u1;

double         typedef f8; // 8 bytes
float          typedef f4; // 4 bytes
double         typedef f2; // double precision
float          typedef f1; // single precision

unsigned int   typedef uint;
f2             typedef real; // easy to swap precision
u1             typedef byte;

u4             typedef rgba8;



internal real Random      ( void               ) { return (real) rand() / ( (real) RAND_MAX + 1.0 ); }
internal real RandomRange ( real Min, real Max ) { return Min + ( Max - Min ) * Random( ); }



internal real DegToRad ( real Degrees ) { return ( Degrees * PI ) / 180.0; }
internal real RadToDeg ( real Radians ) { return ( Radians * 180.0 ) / PI; }



internal real
ReflectanceSchlick (
	real Cosine,
	real Index
) {
	real r0 = ( 1.0 - Index ) / ( 1.0 + Index );
	     r0 = r0 * r0;
	real c0 = 1.0 - Cosine;
	     c0 = c0 * c0 * c0 * c0 * c0; // pow( c0, 5 );
	return r0 + ( 1.0 - r0 ) * c0;
}



union v3 { real E[3]; struct { real X, Y, Z; }; struct { real R, G, B; }; struct { real I, J, K; }; } typedef v3;
v3 typedef colour;

internal bool NearZero  ( v3   A                 ) { return NEAR_ZERO( A.X ) and NEAR_ZERO( A.Y ) and NEAR_ZERO( A.Z ); }
internal real Sum       ( v3   A                 ) { return         A.X +       A.Y +       A.Z        ; }
internal v3   Negate    ( v3   A                 ) { return (v3) { -A.X      , -A.Y      , -A.Z       }; }
internal real Length2   ( v3   A                 ) { return         A.X * A.X + A.Y * A.Y + A.Z * A.Z  ; }
internal real Length    ( v3   A                 ) { return sqrt (  A.X * A.X + A.Y * A.Y + A.Z * A.Z ); }
internal v3   Add       ( v3   A, v3   B         ) { return (v3) {  A.X + B.X,  A.Y + B.Y,  A.Z + B.Z }; }
internal v3   Sub       ( v3   A, v3   B         ) { return (v3) {  A.X - B.X,  A.Y - B.Y,  A.Z - B.Z }; }
internal v3   Mul       ( v3   A, v3   B         ) { return (v3) {  A.X * B.X,  A.Y * B.Y,  A.Z * B.Z }; }
internal v3   Div       ( v3   A, v3   B         ) { return (v3) {  A.X / B.X,  A.Y / B.Y,  A.Z / B.Z }; }
internal v3   AddS      ( v3   A, real B         ) { return (v3) {  A.X + B  ,  A.Y + B  ,  A.Z + B   }; }
internal v3   SubS      ( v3   A, real B         ) { return (v3) {  A.X - B  ,  A.Y - B  ,  A.Z - B   }; }
internal v3   MulS      ( v3   A, real B         ) { return (v3) {  A.X * B  ,  A.Y * B  ,  A.Z * B   }; }
internal v3   DivS      ( v3   A, real B         ) { return (v3) {  A.X / B  ,  A.Y / B  ,  A.Z / B   }; }
internal real Dot       ( v3   A, v3   B         ) { return         A.X * B.X + A.Y * B.Y + A.Z * B.Z  ; }
internal v3   Cross     ( v3   A, v3   B         ) { return (v3) { A.Y*B.Z-A.Z*B.Y, A.Z*B.X-A.X*B.Z, A.X*B.Y-A.Y*B.X }; }
internal v3   Sqrt      ( v3   A                 ) { return (v3) { sqrt(A.X) , sqrt(A.Y) , sqrt(A.Z)  }; }
internal v3   Normalise ( v3   A                 ) { return DivS ( A, Length ( A ) ); }
internal v3   Lerp      ( v3   A, v3   B, real T ) { return Add  ( MulS( A, 1.0 - T ), MulS( B, T ) ); }
internal v3   Reflect   ( v3   A, v3   N         ) { return Sub  ( A, MulS( N, 2.0 * Dot ( A, N ) ) ); }
internal v3   Refract   ( v3   A, v3   N, real E ) {
	real CosTheta = Dot ( Negate  ( A ), N );
	v3   OutPerp  = MulS( Add     ( A  , MulS( N, CosTheta ) ), E );
	v3   OutPara  = MulS( N, -sqrt( fabs( 1.0 - Length2( OutPerp ) ) ) );
	return Add( OutPerp, OutPara );
}

internal v3 RandomV            ( void                  ) { return (v3) { Random(),   Random(),   Random()  }; }
internal v3 RandomRangeV       ( real Min   , real Max ) { return AddS ( MulS( RandomV(), Max - Min ), Min ); }
internal v3 RandomInUnitSphere ( void                  ) { // HACK: infinite loop
	while ( true ) {
		v3 p = RandomRangeV( -1.0, 1.0 );
		if ( Length2( p ) >= 1.0 ) {
			continue;
		}
		return p;
	}
}
internal v3 RandomUnitVector   ( void                  ) { return Normalise( RandomInUnitSphere() ); }
internal v3 RandomInHemisphere ( v3   Normal           ) {
	v3 p = RandomInUnitSphere();
	if ( Dot( p, Normal ) > 0.0 ) {
		return p;
	} else {
		return Negate( p );
	}
}
internal v3 RandomInUnitDisc   ( void                  ) { // HACK: infinite loop
	while ( true ) {
		v3 p = { RandomRange( -1.0, 1.0 ), RandomRange( -1.0, 1.0 ), 0.0 };
		if ( Length2( p ) >= 1.0 ) {
			continue;
		}
		return p;
	}
}



enum primitive_type {
	P_RAY,
	P_SPHERE,
	P_PLANE,
	P_TRIANGLE,
	P_EMISSIVE,
	P_DIFFUSE,
	P_DIELECTRIC,
	P_METAL,
	P_COUNT,
} typedef primitive_type;

struct primitive { primitive_type T; union { real E[9]; v3 V[3]; struct { v3 A, B, C; }; }; } typedef primitive;
/*
ray : A = position, B = direction
shapes
	sphere   : A = position, B.R = radius
	plane    : A = position, B   = normal
	triangle : A = point 1,  B   = point 2, C = point 3
materials
	emissive   : A   = colour
	diffuse    : A   = colour
	dielectric : A.R = refractive index
	metal      : A   = colour, B.R = roughness
*/



internal primitive NewRay       ( v3        Pos, v3   Dir ) { return (primitive) { .T = P_RAY, .A = Pos, .B = Normalise( Dir ) }; }
internal v3        PointOnRayAt ( primitive Ray, real T   ) { return Add( Ray.A, MulS( Ray.B, T ) ); }



struct hit_info { real Distance; v3 Point, Normal; bool FrontFace; primitive Material; } typedef hit_info;

internal void
SetFaceNormal (
	hit_info* Info,
	primitive Ray,
	v3        OutwardNormal
) {
	Info->FrontFace = Dot( Ray.B, OutwardNormal ) < 0.0;
	Info->Normal    = Info->FrontFace
		?         OutwardNormal
		: Negate( OutwardNormal );
}



internal bool
Scatter (
	primitive  Ray,
	hit_info   Info,
	colour*    Attenuation,
	primitive* Scattered
) {
	switch ( Info.Material.T ) {
		case P_DIFFUSE: {
			v3 Dir = Add( Info.Normal, RandomUnitVector() );
			while ( NearZero( Dir ) ) { // HACK: infinite loop
				Dir = Add( Info.Normal, RandomUnitVector() );
			}
			*Scattered   = NewRay( Info.Point, Dir );
			*Attenuation = Info.Material.A;
			return true;
		} break;

		case P_METAL: {
			v3 Reflected = Reflect( Ray.B, Info.Normal );
			v3 Direction = Add( Reflected, MulS( RandomInUnitSphere(), Info.Material.B.I ) );
			*Scattered   = NewRay( Info.Point, Direction );
			*Attenuation = Info.Material.A;
			return Dot( Scattered->B, Info.Normal ) > 0.0;
		} break;

		case P_DIELECTRIC: {
			*Attenuation      = (colour) { 1.0, 1.0, 1.0 };
			real RefractRatio = Info.FrontFace
				? ( 1.0 / Info.Material.A.R )
				:         Info.Material.A.R;

			real CosTheta = Dot( Negate( Ray.B ), Info.Normal );
			real SinTheta = sqrt( 1.0 - CosTheta * CosTheta );

			bool CannotRefract = RefractRatio * SinTheta > 1.0;

			v3 Direction;
			if ( CannotRefract or ReflectanceSchlick( CosTheta, RefractRatio ) > Random() ) {
				Direction = Reflect( Ray.B, Info.Normal );
			} else {
				Direction = Refract( Ray.B, Info.Normal, RefractRatio );
			}
			*Scattered = NewRay( Info.Point, Direction );
			return true;
		} break;

		case P_EMISSIVE: {
			*Attenuation = Info.Material.A;
			return false;
		} break;

		default: {} break;
	}

	return false;
}



internal bool
HitSphere (
	primitive Ray,
	primitive Sphere,
	real      Near,
	real      Far,
	hit_info* Info
) {
	v3   p = Sub( Sphere.A, Ray.A );
	real m = Dot( Ray.B, p );
	real c = Dot( p      , p ) - Sphere.B.R * Sphere.B.R;
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
	SetFaceNormal( Info, Ray, DivS( Sub( Info->Point, Sphere.A ), Sphere.B.R ) );
	return true;
}



internal bool
HitPlane (
	primitive Ray,
	primitive Plane,
	real      Near,
	real      Far,
	hit_info* Info
) {
	real d = Dot( Plane.B, Ray.B );
	if ( d == 0.0 ) {
		return false;
	}
	real t = Dot( Sub( Plane.A, Ray.A ), Plane.B ) / d;
	if ( not BETWEEN( t, Near, Far ) ) {
		return false;
	}
	Info->Distance = t;
	Info->Point    = PointOnRayAt( Ray, t );
	SetFaceNormal( Info, Ray, Plane.B );
	return true;
}



internal bool
HitTriangle (
	primitive Ray,
	primitive Tri,
	real      Near,
	real      Far,
	hit_info* Info
) {
	v3   e1  = Sub  ( Tri.B, Tri.A );
	v3   e2  = Sub  ( Tri.C, Tri.A );
	v3   p   = Cross( Ray.B, e2    );
	real det = Dot  ( p    , e1    );
	if ( det > -EPS and det < EPS ) {
		return false;
	}
	real inv = 1.0 / det;
	v3   t   = Sub( Ray.A, Tri.A );
	real u   = Dot( t, p ) * inv;
	if ( u < 0.0 or u > 1.0 ) {
		return false;
	}
	v3   q = Cross( t, e1 );
	real v = Dot( Ray.B, q ) * inv;
	if ( v < 0.0 or u + v > 1.0 ) {
		return false;
	}
	real Distance = Dot( e2, q ) * inv;
	if ( not BETWEEN( Distance, Near, Far ) ) {
		return false;
	}
	v3 Normal = Normalise( Cross( e1, e2 ) );

	Info->Distance = Distance;
	Info->Point    = PointOnRayAt( Ray, Distance );
	SetFaceNormal( Info, Ray, Normal );
	return true;
}



struct world {
	primitive* Sphere;
	primitive* SphereMats;
	primitive* Plane;
	primitive* PlaneMats;
	primitive* Triangle;
	primitive* TriangleMats;
} typedef world;

internal void
FreeTheWorld (
	world World
) {
	arrfree( World.Sphere       );
	arrfree( World.SphereMats   );
	arrfree( World.Plane        );
	arrfree( World.PlaneMats    );
	arrfree( World.Triangle     );
	arrfree( World.TriangleMats );
}

internal bool
HitWorld (
	primitive Ray,
	world     World,
	real      Near,
	real      Far,
	hit_info* Info
) {
	hit_info Tmp = {0};
	bool     Hit = false;
	real Closest = Far;

	for ( int i = 0; i < arrlen( World.Sphere ); ++i ) {
		if ( not HitSphere( Ray, World.Sphere[i], Near, Closest, &Tmp ) ) {
			continue;
		}
		Hit          = true;
		Closest      = Tmp.Distance;
		Tmp.Material = World.SphereMats[i];
		*Info        = Tmp;
	}

	for ( int i = 0; i < arrlen( World.Plane ); ++i ) {
		if ( not HitPlane( Ray, World.Plane[i], Near, Closest, &Tmp ) ) {
			continue;
		}
		Hit          = true;
		Closest      = Tmp.Distance;
		Tmp.Material = World.PlaneMats[i];
		*Info        = Tmp;
	}

	for ( int i = 0; i < arrlen( World.Triangle ); ++i ) {
		if ( not HitTriangle( Ray, World.Triangle[i], Near, Closest, &Tmp ) ) {
			continue;
		}
		Hit          = true;
		Closest      = Tmp.Distance;
		Tmp.Material = World.TriangleMats[i];
		*Info        = Tmp;
	}

	return Hit;
}

internal world
AWholeNewWorld (
	bool Randomise
) {
	world World = {0};

	v3        GroundN = { 0.0, 0.0, 1.0 };
	primitive GroundP = { .T = P_PLANE,   .A = { 0.0, 0.0, 0.0 }, .B = Normalise( GroundN ) };
	primitive GroundM = { .T = P_DIFFUSE, .A = { 0.5, 0.5, 0.5 } };
	arrput( World.Plane    , GroundP );
	arrput( World.PlaneMats, GroundM );

	if  ( Randomise )
	for ( int a = -11; a < 11; ++a )
	for ( int b = -11; b < 11; ++b ) {
		real ChooseMat = Random();

		primitive Mat = {0};
		primitive Sphere = {
			.T   = P_SPHERE,
			.A   = { a + 0.9 * Random(), b * 0.9 * Random(), 0.2 },
			.B.R = 0.2
		};

		if ( Length( Sub( Sphere.A, (v3) { 4.0, 0.0, 0.2 } ) ) > 0.9 ) {
			if ( ChooseMat < 0.8 ) {
				Mat.T = P_DIFFUSE;
				Mat.A = Mul( RandomV(), RandomV() );
			} else if ( ChooseMat < 0.95 ) {
				Mat.T   = P_METAL;
				Mat.A   = RandomRangeV( 0.5, 1.0 );
				Mat.B.R = RandomRange ( 0.0, 0.5 );
			} else {
				Mat.T   = P_DIELECTRIC;
				Mat.B.I = 1.5;
			}

			arrput( World.Sphere    , Sphere );
			arrput( World.SphereMats, Mat    );
		}
	}

	primitive Sphere1   = { .T = P_SPHERE    , .A   = {  0.0, 0.0, 1.0 }, .B.R = 1.0 };
	primitive Material1 = { .T = P_DIELECTRIC, .B.I = 1.5 };
	arrput( World.Sphere    , Sphere1   );
	arrput( World.SphereMats, Material1 );

	primitive Sphere2   = { .T = P_SPHERE , .A = { -4.0, 0.0, 1.0 }, .B.R = 1.0 };
	primitive Material2 = { .T = P_DIFFUSE, .A = {  0.4, 0.2, 0.1 } };
	arrput( World.Sphere    , Sphere2   );
	arrput( World.SphereMats, Material2 );

	primitive Sphere3   = { .T = P_SPHERE, .A = { 4.0, 0.0, 1.0 }, .B.R = 1.0 };
	primitive Material3 = { .T = P_METAL , .A = { 0.7, 0.6, 0.5 }, .B.R = 0.0 };
	arrput( World.Sphere    , Sphere3   );
	arrput( World.SphereMats, Material3 );

	return World;
}

internal bool
LoadWorldFile (
	FILE*  File,
	world* World
) {
	struct primitive_map {
		char           Name[12];
		int            Count;
		primitive_type Type;
		primitive**    PrimList;
		primitive**    MatList;
	} typedef primitive_map;
	primitive_map PrimitiveMap[] = {
		{ "sphere"    , 4, P_SPHERE    , &World->Sphere  , &World->SphereMats   },
		{ "plane"     , 6, P_PLANE     , &World->Plane   , &World->PlaneMats    },
		{ "triangle"  , 9, P_TRIANGLE  , &World->Triangle, &World->TriangleMats },
		{ "emissive"  , 3, P_EMISSIVE  , NULL, NULL },
		{ "diffuse"   , 3, P_DIFFUSE   , NULL, NULL },
		{ "metal"     , 4, P_METAL     , NULL, NULL },
		{ "dielectric", 1, P_DIELECTRIC, NULL, NULL },
	};

	primitive** MatList = NULL;

	FILE* F    = File;
	int   Scan = 1;
	char  Buffer[UCHAR_MAX] = {0};

	while ( Scan != EOF ) {
		// HACK: provide length in string, equal to `(uint) sizeof( Buffer ) - 1`
		Scan = fscanf( F, "%254s", Buffer );

		if ( Scan == EOF ) {
			break;
		}

		if ( Buffer[0] == '#' ) {
			char C = 0;
			while ( C != '\n' and C != EOF ) {
				C = (char) fgetc( F );
			}
			continue;
		}

		for ( int p = 0; p < sizeof( PrimitiveMap ) / sizeof( *PrimitiveMap ); ++p ) {
			primitive_map* PM = &PrimitiveMap[p];
			if ( not STRINGS_EQUAL( PM->Name, Buffer ) ) {
				continue;
			}
			primitive Prim = { .T = PM->Type };
			for ( int e = 0; e < PM->Count; ++e ) {
				Scan = fscanf( F, "%lf", &Prim.E[e] );
				if ( Scan != 1 ) {
					printf( "Could not read value %i for '%s'\n",
					e + 1, PM->Name );
					return false;
				}
			}
			if ( PM->PrimList != NULL ) {
				if ( Prim.T == P_PLANE ) {
					Prim.B = Normalise( Prim.B );
				}
				arrput( *PM->PrimList, Prim );
				MatList = PM->MatList;
			} else if ( MatList != NULL ) {
				arrput( *MatList, Prim );
				MatList = NULL;
			}
			break;
		}
	}

	return true;
}



internal colour
RayColour (
	primitive Ray,
	world     World,
	int       Depth
) {
	persistent real Near = 0.0001;
	persistent real Far  = 1000.0;

	colour Colour = { 1.0, 1.0, 1.0 };

	int d;
	for ( d = 0; d < Depth; ++d ) {
		hit_info Info = {0};
		if ( not HitWorld( Ray, World, Near, Far, &Info ) ) {
			persistent colour Sky1 = { .R = 1.0, .G = 1.0, .B = 1.0 };
			persistent colour Sky2 = { .R = 0.5, .G = 0.7, .B = 1.0 };
			real T = ( Ray.B.Z + 1.0 ) / 2.0;
			Colour = Mul( Colour, Lerp( Sky1, Sky2, T ) );
			break;
		}

		colour Attenuation;
		if ( not Scatter( Ray, Info, &Attenuation, &Ray ) ) {
			if ( Info.Material.T == P_EMISSIVE ) {
				// HACK: I don't know how bright colours are supposed to work here
				v3 Mod = MulS( Attenuation, Dot( Negate( Ray.B ), Info.Normal ) );
				Colour = Mul( Colour, Mod );
			} else {
				Colour = MulS( Colour, 0.0 );
			}
			break;
		}

		Colour = Mul( Colour, Attenuation );
	}

	if ( d == Depth ) {
		Colour = MulS( Colour, 0.0 );
	}

	return Colour;
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

internal primitive
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
	world        World,
	int          Samples,
	int          Depth
) {
	v3     Position   = { 13.0, -3.0, 2.0 };
	v3     Target     = {  0.0,  0.0, 0.0 };
	v3     WorldUp    = {  0.0,  0.0, 1.0 };
	real   vfov       = 20.0;
	real   Aspect     = (real) Image.Width / Image.Height;
	real   Aperture   = 0.2;
	camera Cam        = NewCamera( Position, Target, WorldUp, vfov, Aspect, Aperture );

	for ( int y = 0; y < Image.Height; y += 1 ) {
		rgba8* Row  = (rgba8*) ( (byte*) Image.Buffer + y * Image.Pitch );
		for ( int x = 0; x < Image.Width; x += 1 ) {
			rgba8* Pixel = Row + x;

			colour PixelColour = {0};

			for ( int sx = 0; sx < Samples; ++sx )
			for ( int sy = 0; sy < Samples; ++sy ) {
				real     su = (real) sx   /   Samples;
				real     sv = (real) sy   /   Samples;
				real      u = ( x +  su ) / ( Image.Width  - 1 );
				real      v = ( y +  sv ) / ( Image.Height - 1 );
				primitive r = CameraRay( Cam, u, v );

				PixelColour = Add( PixelColour, RayColour( r, World, Depth ) );
			}

			PixelColour = DivS( PixelColour, Samples * Samples );
			PixelColour = Sqrt( PixelColour );

			//real HDR = fmax(fmax(PixelColour.R, PixelColour.G), PixelColour.B);
			//if ( HDR > 1.0 ) {
			//	PixelColour = DivS( PixelColour, HDR );
			//}

			u1 Red   = (u1) ( 255.999 * CLAMP_01( PixelColour.R ) );
			u1 Green = (u1) ( 255.999 * CLAMP_01( PixelColour.G ) );
			u1 Blue  = (u1) ( 255.999 * CLAMP_01( PixelColour.B ) );
			u1 Alpha = UCHAR_MAX;
			*Pixel   = Red   << 0
			         | Green << 8
			         | Blue  << 16
			         | Alpha << 24;
		}

		persistent int Complete = 0;
		printf( "\rPROGRESS: %.0f%% ", ( 100.0 * ++Complete ) / Image.Height );
		fflush( stdout );
	}
	puts( "\rPROGRESS: DONE" );
}



struct config { bool Error; int Width, Height, Samples, Depth; char* WorldFile; } typedef config;

internal config
ParseArgs (
	int    argc,
	char** argv
) {
	char* HelpMessage =
		"Usage: rtic [options]                                   \n"
		"    -?           - print this help message.             \n"
		"    -w [width]   - set image width.       default:  160 \n"
		"    -h [height]  - set image height.      default:   90 \n"
		"    -s [samples] - set samples per pixel. default:    2 \n"
		"    -d [depth]   - set max bounce depth.  default:   20 \n"
		"    -f [file]    - load world from file.  default: none \n";

	config Config = {
		.Error     = false,
		.Width     = 160,
		.Height    = 90,
		.Samples   = 2,
		.Depth     = 20,
		.WorldFile = NULL,
	};

	bool Help = false;
	for ( int a = 1; a < argc; ++a ) {
		char* arg = argv[a];

		     if ( STRINGS_EQUAL( "-?", arg ) or a >= argc - 1 ) { Help  = true; break; }
		else if ( STRINGS_EQUAL( "-w", arg ) ) { Config.Width     = atoi( argv[++a] ); }
		else if ( STRINGS_EQUAL( "-h", arg ) ) { Config.Height    = atoi( argv[++a] ); }
		else if ( STRINGS_EQUAL( "-s", arg ) ) { Config.Samples   = atoi( argv[++a] ); }
		else if ( STRINGS_EQUAL( "-d", arg ) ) { Config.Depth     = atoi( argv[++a] ); }
		else if ( STRINGS_EQUAL( "-f", arg ) ) { Config.WorldFile =       argv[++a]  ; }
		else { Help = true; break; }
	}

	if ( Config.Width <= 0 or Config.Height <= 0 or Config.Samples <= 0 or Config.Depth <= 0 ) {
		Help = true;
		printf( "Invalid parameter values recieved\n" );
	}

	if ( Help ) {
		printf( "%s", HelpMessage );
		Config.Error = true;
	}

	return Config;
}



int
main (
	int    argc,
	char** argv
) {
	config Config = ParseArgs( argc, argv );
	if ( Config.Error ) return( EXIT_FAILURE );

	// HACK: padding?
	printf( "Width      = %10u\n", Config.Width   );
	printf( "Height     = %10u\n", Config.Height  );
	printf( "Samples    = %10u\n", Config.Samples );
	printf( "Depth      = %10u\n", Config.Depth   );
	printf( "World File = %10s\n", Config.WorldFile ? Config.WorldFile : "none" );

	puts( "START" );

	srand( 2 );

	TIMER_INIT();

	world World = {0};
	if ( Config.WorldFile != NULL ) {
		FILE* File = fopen( Config.WorldFile, "r" );
		if ( not File ) {
			printf( "Could not open file '%s'\n", Config.WorldFile );
			return( EXIT_FAILURE );
		}
		bool OK = LoadWorldFile( File, &World );
		fclose( File );
		if ( not OK ) {
			printf( "Failed to load world from file '%s'", Config.WorldFile );
			FreeTheWorld( World );
			return( EXIT_FAILURE );
		}
	} else {
		World = AWholeNewWorld( true );
	}

	image_buffer Image = {
		.Buffer = malloc( Config.Width * Config.Height * sizeof( rgba8 ) ),
		.Width  = Config.Width,
		.Height = Config.Height,
		.Pitch  = Config.Width * sizeof( rgba8 ),
	};

	TIMER_STAMP( "LOAD    " );

	RenderWorld( Image, World, Config.Samples, Config.Depth );

	TIMER_STAMP( "RENDER  " );

	{ // Save buffer to PNG file
		// FIXME: UTF-8 support in any way?
		//char Filename[ UCHAR_MAX ];
		//stbiw_convert_wchar_to_utf8(
		//	Filename, sizeof( Filename ),
		//	L"output.png" );
		stbi_write_png(
			"output.png",
			Image.Width, Image.Height,
			sizeof( rgba8 ),
			Image.Buffer,
			Image.Pitch );
	}

	TIMER_STAMP( "SAVE    " );


	FreeTheWorld( World );
	free( Image.Buffer );

	puts( "END" );

	return( EXIT_SUCCESS );
}
