/*
 *   lucille | Global Illumination renderer
 *
 *             written by Syoyo Fujita.
 *
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#ifdef WITH_SSE
#include <xmmintrin.h>
#endif

#include "vector.h"
#include "list.h"
#include "log.h"
#include "raytrace.h"
#include "polygon.h"
#include "util.h"
#include "accel.h"
#include "render.h"
#include "memory.h"
#include "reflection.h"
#include "thread.h"

#define EPS 1.0e-6
#define EPSILON 1.0e-6
#define EPSILON2 1.0e-6
#define DIREPS 1.0e-6

#ifdef HAVE_RESTRICT    /* The compiler supports restrict keyword. */
#define RESTRICT restrict
#else
#define RESTRICT
#endif

/* use z curve order for addressing volxel memory. */
#define USE_ZORDER 1

#ifdef WITH_SSE

static inline __m128 _mm_sel_ps( __m128 a, __m128 b, __m128 mask )
{
	b = _mm_and_ps( b, mask );
	a = _mm_andnot_ps( mask, a );

	return _mm_or_ps( a, b );
}

#define cross_sse( a, b, c, d ) (                                         \
		_mm_sub_ps( _mm_mul_ps( ( a ), ( c ) ),                   \
			   _mm_mul_ps( ( b ), ( d ) ) ) )

#define dot_sse( ax, ay, az, bx, by, bz ) (                               \
		_mm_add_ps(                                               \
			_mm_add_ps(                                       \
				_mm_mul_ps( ( ax ), ( bx ) ),             \
				_mm_mul_ps( ( ay ), ( by ) ) ),           \
			_mm_mul_ps( ( az ), ( bz ) ) ) )
#endif

/* for block'ed memory access */
#define BLKIDX( x, y, z, shift, blksize ) (                               \
		( ( z >> shift ) * blksize * blksize ) +                  \
		( ( y >> shift ) * blksize ) + ( x >> shift ) )

#define SUBIDX( x, y, z, width, mask ) (                                  \
		( z & mask ) * width * width +                            \
		( y & mask ) * width + ( x & mask ) )

#define MAP_XYZ( x, y, z, shift, blksize, width, mask )                     \
	BLKIDX( ( x ), ( y ), ( z ), ( shift ), ( blksize ) ) *             \
	( width ) * ( width ) * ( width ) +                                 \
	SUBIDX( ( x ), ( y ), ( z ), ( width ), ( mask ) )

#define MAP_Z3D( x, y, z ) (                                                  \
		g_z_table[( x )] | ( g_z_table[( y )] << 1 ) |                \
		( g_z_table[( z )] << 2 ) )

static uint32_t g_z_table[256];

static uint32_t
triangle_intersect(
	const ri_vector_t *orig,
	const ri_vector_t *dir,
	const ri_vector_t *v0,
	const ri_vector_t *v1,
	const ri_vector_t *v2,
	ri_float_t curr_t,
	uint32_t tid,
	ri_float_t *newt,
	ri_float_t *newu,
	ri_float_t *newv,
	uint32_t *newtid );

static int			intersect_with_ugrid(
	ri_ugrid_t              *ugrid,
	const ri_vector_t       *eye,
	const ri_vector_t       *dir,
	ri_intersection_state_t *state_out );

static void			calculate_normal(
	ri_vector_t       *n,
	const ri_vector_t *v0,
	const ri_vector_t *v1,
	const ri_vector_t *v2 );

static void			lerp_normal(
	ri_vector_t *n,
	const ri_vector_t *n0,
	const ri_vector_t *n1,
	const ri_vector_t *n2,
	ri_float_t u,
	ri_float_t v );

static void			lerp_uv(
	ri_float_t *newu, 
	ri_float_t *newv,
	const ri_float_t *uv0, 
	const ri_float_t *uv1, 
	const ri_float_t *uv2,
	ri_float_t u, 
	ri_float_t v );

static int inside(
	const ri_vector_t *pos,
	const ri_float_t bmin[3],
	const ri_float_t bmax[3] );

static int intersect_ray_bbox(
	const ri_vector_t *eye,
	const ri_vector_t *dir,
	const ri_float_t   bmin[3],
	const ri_float_t   bmax[3],
	ri_float_t         isectt[2] );

static int intersect_foreach_trilist(
	ri_tri_list_t           *list,
	const ri_vector_t       *eye,
	const ri_vector_t       *dir,
	ri_intersection_state_t *state_out );

static int inside_voxel(
	const ri_vector_t *pos,
	int                x,
	int                y,
	int                z,
	const ri_ugrid_t  *grid );

static void build_state(
	ri_intersection_state_t *state,	/* [out] */
	const ri_vector_t       *eye,
	const ri_vector_t       *dir,
	const ri_float_t         t,
	const ri_geom_t         *geom,
	uint32_t                 index,
	ri_float_t               u,
	ri_float_t               v );

/* build the table for z curve order */
static void			build_z_table();

#if defined ( WITH_SSE )
static int intersect_with_ugrid_simd(
	const ri_ugrid_t        *ugrid,
	const ri_ray_t          *ray,
	ri_intersection_state_t *state);

static int intersect_foreach_trilist_simd(
	const ri_simd_tri_info_t *list,
	const ri_vector_t ray_org,
	const ri_vector_t ray_dir,
	int tnum,                                       /* thread number */
	ri_tri_info_t *hittri,
	ri_float_t *t, ri_float_t *u, ri_float_t *v );

#endif

#ifdef WITH_SSE

/*
 * Do 1 ray - 4 triangle intersection test
 */
static FORCE_INLINE void isect_sse(
	const ri_vector_t *rox,
	const ri_vector_t *roy,
	const ri_vector_t *roz,
	const ri_vector_t *rdx,
	const ri_vector_t *rdy,
	const ri_vector_t *rdz,
	const ri_vector_t *v0x,
	const ri_vector_t *v0y,
	const ri_vector_t *v0z,
	const ri_vector_t *e1x,
	const ri_vector_t *e1y,
	const ri_vector_t *e1z,
	const ri_vector_t *e2x,
	const ri_vector_t *e2y,
	const ri_vector_t *e2z,
	ri_vector_t       hit_info[4])	// {t, u, v, det}
{

	const __m128 one  = _mm_set_ps1( 1.0f + EPSILON );
	const __m128 zero = _mm_set_ps1( 0.0f );

#if 0	/* original code. no buckface culling. */
	const __m128 eps2 = _mm_set_ps1( EPSILON2 );
#else	/* do buckface culling. */
	const __m128 eps  = _mm_set_ps1( EPSILON );
#endif

#ifdef ENABLE_DOUBLE_PRECISION

	// TODO

#else	// single precison

	/* cross product p = d x e2 */
	const __m128 px = cross_sse( e2z.v, e2y.v, rdy.v, rdz.v );
	const __m128 py = cross_sse( e2x.v, e2z.v, rdz.v, rdx.v );
	const __m128 pz = cross_sse( e2y.v, e2x.v, rdx.v, rdy.v );

	__m128       a = dot_sse( px, py, pz, e1x.v, e1y.v, e1z.v );

	/* constructs the vector s = (ray origin) - v0. */
	const __m128 sx = _mm_sub_ps( rox.v, v0x.v );
	const __m128 sy = _mm_sub_ps( roy.v, v0y.v );
	const __m128 sz = _mm_sub_ps( roz.v, v0z.v );

	/*
	 * Simple trick that avoid division for each ray-triangle intersection.
	 *
	 * t = t' / det (where t' = e2 x q) and if we use back-face culling,
	 * we assume t > 0 and det > 0 for valid intersection.
	 * thus, if we have previous nearest hit point `t_prev', comparison
	 *
	 *   t < t_prev
	 *
	 * is equivalent to
	 *
	 *     (t' / det) < (t_prev' / det_prev)
	 * <=> (t' * det_prev) < (t_prev' * det)
	 *
	 * Finally if we got nearest hit point t' and it's determinant,
	 * we just compute t = t' / det, u = u' / det, v = v' / det.
	 *
	 */

	/* q = s x e1 */
	const __m128 qx = cross_sse( e1z.v, e1y.v, sy, sz );
	const __m128 qy = cross_sse( e1x.v, e1z.v, sz, sx );
	const __m128 qz = cross_sse( e1y.v, e1x.v, sx, sy );

	/* calculate u', v' and t' for all triangles. */
	//const __m128 uu = _mm_mul_ps( dot_sse( sx, sy, sz, px, py, pz ), rpa );
	//const __m128 vv = _mm_mul_ps( dot_sse( rdx, rdy, rdz, qx, qy, qz ), rpa );
	const __m128 uu = dot_sse( sx, sy, sz, px, py, pz );
	const __m128 vv = dot_sse( rdx.v, rdy.v, rdz.v, qx, qy, qz );

	/* run all rejection tests. if a triangle passes all test,
	 * it will be marked by all ones in the SSE-register, if it fails
	 * it will be marked by zeros. if triangle 1 and 3 passes
	 * the resulting register would look like
	 *
	 * 0xFFFFFFFF00000000FFFFFFFF00000000
	 *
	 */
	a = _mm_and_ps(
		_mm_and_ps(
#if 0	/* original code. no buck face culling */
			_mm_cmpgt_ps( _mm_mul_ps( a, a ), eps2 ),
#else	/* do back face culling */
			_mm_cmpgt_ps( a, eps ),
#endif
			_mm_cmpngt_ps( _mm_add_ps( uu, vv ), one )
		),
		_mm_and_ps(
			_mm_cmpnlt_ps( uu, zero ),
			_mm_cmpnlt_ps( vv, zero )
		)
	    );

	const __m128 tt = dot_sse( e2x.v, e2y.v, e2z.v, qx, qy, qz );

	/*
	 * if ( (tt * det_prev) < (t_prev * det) ) {
	 *     update t, u, v, det
	 * }
	 *
	 */
	const __m128 t_prev   = hit_info[0].v;
	const __m128 u_prev   = hit_info[1].v;
	const __m128 v_prev   = hit_info[2].v;
	const __m128 det_prev = hit_info[3].v;

	const __m128 tt_mul_det_prev = _mm_mul_ps(tt, det_prev);
	const __m128 t_prev_mul_det  = _mm_mul_ps(t_prev, a);
	const __m128 update_mask     = _mm_cmplt_ps(tt_mul_det_prev,
						    t_prev_mul_det);

	
	hit_info[0].v   = _mm_sel_ps(t_prev,   tt, update_mask);
	hit_info[1].v   = _mm_sel_ps(u_prev,   uu, update_mask);
	hit_info[2].v   = _mm_sel_ps(v_prev,   vv, update_mask);
	hit_info[3].v   = _mm_sel_ps(det_prev,  a, update_mask);

#endif	// ENABLE_DOUBLE_PRECISION
}

#endif	// WITH_SSE

void
ri_raytrace_setup()
{
	build_z_table();
}

int
ri_raytrace(
	ri_render_t             *render,
	ri_ray_t                *ray,
	ri_intersection_state_t *state_out )
{
	int                     hit = 0;
	int                     hashit = 0;
	ri_intersection_state_t state;

	ray->t = 0.0;

	render->stat.nrays++;

	state.inside = 0;

	if (render->context->option->accel_method == ACCEL_GRID) {
#if defined ( WITH_SSE )
		hit = intersect_with_ugrid_simd( render->accel_grid,
						ray,
						&state );
#else
		hit = intersect_with_ugrid( render->accel_grid,
					   &ray->org, &ray->dir,
					   &state );
#endif
	} else {

		ri_log( LOG_ERROR, "No accel structure!" );

	}

	if (hit) {
		hashit  = 1;

		memcpy( state_out, &state, sizeof( ri_intersection_state_t ) );

	}


	return hashit;
}

void
ri_raytrace_statistics()
{
	uint64_t ngridtravs;
	uint64_t ntesttris;
	uint64_t nrays;

	ri_float_t             elapsed = 0.0;

	ngridtravs   = ri_render_get()->stat.ngridtravs;
	ntesttris    = ri_render_get()->stat.ntesttris;
	nrays        = ri_render_get()->stat.nrays;

	elapsed = ri_timer_elapsed( ri_render_get()->context->timer,
				   "Render frame" );

	printf( "\n" );
	printf( "/= Raytracing statistics =================="
		"====================================\n" );
	printf( "| %-48s:  %20llu\n", "Total rays", nrays );
	printf( "| %-48s:  %20llu\n", "Total grid cell traversals", ngridtravs );
	printf( "| %-48s:  %20llu\n", "Total triangle tests", ntesttris );
	//printf("total mailboxing hits:  %20llu\n", nmailboxhits);
	printf( "| %-48s:  %20.6f\n", "The number of tests per ray",
	       ( double )ntesttris / ( double )nrays );
	printf( "| %-48s:  %20.6f\n", "The number of travs per ray",
	       ( double )ngridtravs / ( double )nrays );
	printf( "| %-48s:  %20.6f\n", "Rays/sec", ( double )nrays /
	       ( double )elapsed );
	printf(
		"\\------------------------------------------------------------------------------\n" );
	fflush( stdout );
}


/*
 * --- private functions ---
 */

void
lerp_uv(
	ri_float_t *newu,
	ri_float_t *newv,
	const ri_float_t *uv0,
	const ri_float_t *uv1,
	const ri_float_t *uv2,
	ri_float_t u,
	ri_float_t v )
{
	/* n = (1 - u - v) n0 + u n1 + v n2 */

	*newu = ( 1 - u - v ) * uv0[0] + u * uv1[0] + v * uv2[0];
	*newv = ( 1 - u - v ) * uv0[1] + u * uv1[1] + v * uv2[1];
}

int
intersect_with_ugrid(
	ri_ugrid_t              *ugrid,
	const ri_vector_t       *eye,
	const ri_vector_t       *dir,
	ri_intersection_state_t *state_out )
{
	int                     x, y, z;
	int                     hit;
	ri_float_t                   rayt = ( ri_float_t )RI_INFINITY;
	ri_float_t                   maxt = ( ri_float_t )RI_INFINITY;
	ri_float_t                   tvals[2];
	ri_float_t                   raypos[3];
	ri_float_t                  nextx, nexty, nextz;
	ri_float_t                  deltax, deltay, deltaz;
	int                     stepx, stepy, stepz;
	int                     outx, outy, outz;
	ri_intersection_state_t state;
	ri_vector_t             isectpoint;
	ri_float_t                   nearest = ( ri_float_t )RI_INFINITY;

	if (dir->f[0] == 0.0 && dir->f[1] == 0.0 && dir->f[2] == 0.0) return 0;

	if (inside( eye, ugrid->bboxmin, ugrid->bboxmax ) )
		rayt = 0.0;
	else {
		if (!intersect_ray_bbox( eye, dir,
					ugrid->bboxmin, ugrid->bboxmax, tvals ) )
			return 0;
		rayt = tvals[0];
		maxt = tvals[1];
	}

	raypos[0] = eye->f[0] + dir->f[0] * rayt;
	raypos[1] = eye->f[1] + dir->f[1] * rayt;
	raypos[2] = eye->f[2] + dir->f[2] * rayt;

	x = ( int )( ( raypos[0] - ugrid->bboxmin[0] ) * ugrid->invwidth[0] );
	if (x == ugrid->voxels[0]) x--;

	if (fabs( dir->f[0] ) < DIREPS) {
		nextx  = RI_INFINITY;
		deltax = 0.0;
		stepx  = 0;
		outx   = -1;
	} else if (dir->f[0] > 0) {
		nextx = rayt +
			( ( ( x + 1 ) * ugrid->width[0] + ugrid->bboxmin[0] ) -
			 raypos[0] ) / dir->f[0];

		deltax = ugrid->width[0] / dir->f[0];
		stepx = 1;
		outx = ugrid->voxels[0];
	} else {
		nextx = rayt +
			( ( x * ugrid->width[0] + ugrid->bboxmin[0] ) -
			 raypos[0] ) / dir->f[0];

		deltax = -ugrid->width[0] / dir->f[0];
		stepx = -1;
		outx = -1;
	}

	y = ( int )( ( raypos[1] - ugrid->bboxmin[1] ) * ugrid->invwidth[1] );
	if (y == ugrid->voxels[1]) y--;

	if (fabs( dir->f[1] ) < DIREPS) {
		nexty  = RI_INFINITY;
		deltay = 0.0;
		stepy  = 0;
		outy   = -1;
	} else if (dir->f[1] > 0) {
		nexty = rayt +
			( ( ( y + 1 ) * ugrid->width[1] + ugrid->bboxmin[1] ) -
			 raypos[1] ) / dir->f[1];

		deltay = ugrid->width[1] / dir->f[1];
		stepy = 1;
		outy = ugrid->voxels[1];
	} else {
		nexty = rayt +
			( ( y * ugrid->width[1] + ugrid->bboxmin[1] ) -
			 raypos[1] ) / dir->f[1];

		deltay = -ugrid->width[1] / dir->f[1];
		stepy = -1;
		outy = -1;
	}

	z = ( int )( ( raypos[2] - ugrid->bboxmin[2] ) * ugrid->invwidth[2] );
	if (z == ugrid->voxels[2]) z--;

	if (fabs( dir->f[2] ) < DIREPS) {
		nextz  = RI_INFINITY;
		deltaz = 0.0;
		stepz  = 0;
		outz   = -1;
	} else if (dir->f[2] < 0) {
		nextz = rayt +
			( ( z * ugrid->width[2] + ugrid->bboxmin[2] ) -
			 raypos[2] ) / dir->f[2];

		deltaz = -ugrid->width[2] / dir->f[2];
		stepz = -1;
		outz = -1;
	} else {
		nextz = rayt +
			( ( ( z + 1 ) * ugrid->width[2] + ugrid->bboxmin[2] ) -
			 raypos[2] ) / dir->f[2];

		deltaz = ugrid->width[2] / dir->f[2];
		stepz = 1;
		outz = ugrid->voxels[2];
	}

	hit = 0;

	if (x < 0) x = 0;
	if (x > ugrid->voxels[0] - 1) x = ugrid->voxels[0] - 1;
	if (y < 0) y = 0;
	if (y > ugrid->voxels[1] - 1) y = ugrid->voxels[1] - 1;
	if (z < 0) z = 0;
	if (z > ugrid->voxels[2] - 1) z = ugrid->voxels[2] - 1;

	state.inside = 0;

	while (1) {
		//if (g_profile) g_ngridtravcells++;
		//ri_render_get()->stat.ngridtravs++;

		if (ugrid->cell[z][y][x]) {
			if (intersect_foreach_trilist( ugrid->cell[z][y][x],
						      eye, dir,
						      &state ) ) {
				if (state.t > EPS && state.t <= nearest) {

					hit = 1;

					nearest = state.t;

					ri_mem_copy( state_out, &state,
						    sizeof(
							    ri_intersection_state_t ) );

					/* if intersection point is in this
					 * voxel, terminate traversal.
					 */
					ri_vector_copy( &isectpoint, dir );
					ri_vector_scale( &isectpoint, &isectpoint, state.t );
					ri_vector_add(&isectpoint,
						      &isectpoint,
						       eye );

					if (inside_voxel( &isectpoint, x, y, z,
							 ugrid ) )
						break;
				}
			}
		}

		/* 3D DDA */
		if ( ( nextx < nexty ) && ( nextx < nextz ) ) {
			if (maxt < nextx) break;
			x += stepx;
			if (x == outx) break;
			nextx += deltax;
		} else if ( ( nextz < nexty ) ) {
			if (maxt < nextz) break;
			z += stepz;
			if (z == outz) break;
			nextz += deltaz;
		} else {
			if (maxt < nexty) break;
			y += stepy;
			if (y == outy) break;
			nexty += deltay;
		}
	}

	state_out->t = nearest;

	return hit;

}

static void
calculate_normal(
	ri_vector_t       *n,
	const ri_vector_t *v0,
	const ri_vector_t *v1,
	const ri_vector_t *v2 )
{
	ri_vector_t v01, v02;

	ri_vector_sub( &v01, v1, v0 );
	ri_vector_sub( &v02, v2, v0 );
	ri_vector_cross3( n, &v01, &v02 );
	ri_vector_normalize3( n );
}

static int
inside( 
	const ri_vector_t *pos,
	const ri_float_t bmin[3],
	const ri_float_t bmax[3] )
{
	if (pos->f[0] < bmin[0]) return 0;
	if (pos->f[1] < bmin[1]) return 0;
	if (pos->f[2] < bmin[2]) return 0;

	if (pos->f[0] > bmax[0]) return 0;
	if (pos->f[1] > bmax[1]) return 0;
	if (pos->f[2] > bmax[2]) return 0;

	return 1;
}

int
intersect_ray_bbox( 
	const ri_vector_t *eye,
	const ri_vector_t *dir,
	const ri_float_t   bmin[3],
	const ri_float_t   bmax[3], 
	ri_float_t         isectt[2])		/* [out] */
{
	ri_float_t t0, t1;
	ri_float_t invraydir;
	ri_float_t neart, fart;
	ri_float_t tmp;

	t0 = 0.0; t1 = RI_INFINITY;

	if (dir->f[0] == 0.0 && dir->f[1] == 0.0 && dir->f[2] == 0.0) return 0;

	if (dir->f[0] != 0.0) {
		invraydir = 1.0 / dir->f[0];

		neart = ( bmin[0] - eye->f[0] ) * invraydir;
		fart  = ( bmax[0] - eye->f[0] ) * invraydir;
		if (neart > fart) {
			tmp = neart; neart = fart; fart = tmp;
		}

		t0 = neart > t0 ? neart : t0;
		t1 = fart  < t1 ? fart  : t1;
		if (t0 > t1) return 0;
	}

	if (dir->f[1] != 0.0) {
		invraydir = 1.0 / dir->f[1];

		neart = ( bmin[1] - eye->f[1] ) * invraydir;
		fart  = ( bmax[1] - eye->f[1] ) * invraydir;
		if (neart > fart) {
			tmp = neart; neart = fart; fart = tmp;
		}

		t0 = neart > t0 ? neart : t0;
		t1 = fart  < t1 ? fart  : t1;
		if (t0 > t1) return 0;
	}

	if (dir->f[2] != 0.0) {
		invraydir = 1.0 / dir->f[2];

		neart = ( bmin[2] - eye->f[2] ) * invraydir;
		fart  = ( bmax[2] - eye->f[2] ) * invraydir;
		if (neart > fart) {
			tmp = neart; neart = fart; fart = tmp;
		}

		t0 = neart > t0 ? neart : t0;
		t1 = fart  < t1 ? fart  : t1;
		if (t0 > t1) return 0;
	}

	isectt[0] = t0;
	isectt[1] = t1;

	return 1;
}

int
intersect_foreach_trilist(
	ri_tri_list_t           *list,
	const ri_vector_t       *eye,
	const ri_vector_t       *dir,
	ri_intersection_state_t *state_out )
{
	int                     i;
	uint32_t                hit = 0;
	uint32_t                hashit = 0;
	uint32_t            tid;
	uint32_t            i0, i1, i2;
	ri_vector_t            v0, v1, v2;
	ri_float_t                 u, v;
	ri_geom_t     *RESTRICT geom;
	ri_geom_t     *RESTRICT hitgeom = NULL;
	ri_tri_info_t *RESTRICT triinfo;
	ri_float_t    t = 0.0;

	t = state_out->t;

	for (i = 0; i < list->ntris; i++) {

		triinfo = &list->tris[i];

		geom = triinfo->geom;

#if 0
		if (triinfo->index >= geom->nindices) {
			printf( "triinfo->index = %d\n", triinfo->index );
			exit( -1 );
		}
#endif

		i0 = geom->indices[triinfo->index + 0];
		i1 = geom->indices[triinfo->index + 1];
		i2 = geom->indices[triinfo->index + 2];
		v0 = geom->positions[i0];
		v1 = geom->positions[i1];
		v2 = geom->positions[i2];

		hit |= triangle_intersect( eye, dir, &v0, &v1, &v2, t,
					 triinfo->index, &t, &u, &v, &tid );

		ri_render_get()->stat.ntesttris++;

		if (hashit) {
			hitgeom = geom;
		}
	}

	if (hashit) {
		build_state(state_out, eye, dir, t, hitgeom, tid, u, v);
	}

	return hashit;
}

static void
lerp_normal(
	ri_vector_t *n,
	const ri_vector_t *n0,
	const ri_vector_t *n1,
	const ri_vector_t *n2,
	ri_float_t u,
	ri_float_t v )
{
	ri_vector_t ns0, ns1, ns2;

	/* n = (1 - u - v) n0 + u n1 + v n2 */

	ri_vector_copy( &ns0, n0 );
	ri_vector_scale( &ns0, &ns0, ( 1.0 - u - v ) );
	ri_vector_copy( &ns1, n1 );
	ri_vector_scale( &ns1, &ns1, u );
	ri_vector_copy( &ns2, n2 );
	ri_vector_scale( &ns2, &ns2, v );

	ri_vector_add( n, &ns0, &ns1 );
	ri_vector_add( n, n, &ns2 );
}


int
inside_voxel(
	const ri_vector_t *pos,
	int                x,
	int                y,
	int                z,
	const ri_ugrid_t  *grid )
{
	ri_float_t bmin[3];
	ri_float_t bmax[3];

	bmin[0] = x * grid->width[0] + grid->bboxmin[0];
	bmin[1] = y * grid->width[1] + grid->bboxmin[1];
	bmin[2] = z * grid->width[2] + grid->bboxmin[2];

	bmax[0] = ( x + 1 ) * grid->width[0] + grid->bboxmin[0];
	bmax[1] = ( y + 1 ) * grid->width[1] + grid->bboxmin[1];
	bmax[2] = ( z + 1 ) * grid->width[2] + grid->bboxmin[2];

	if (inside( pos, bmin, bmax ) ) return 1;

	return 0;
}

#if defined ( WITH_SSE )
static int
intersect_with_ugrid_simd( const ri_ugrid_t *ugrid,
			   const ri_ray_t *ray,
			   ri_intersection_state_t *state )
{
	int                 x, y, z;
	int                 hit;
	int                 index;
	ri_vector_t         eye, dir;
	ri_float_t          rayt = ( ri_float_t )RI_INFINITY;
	ri_float_t          maxt = ( ri_float_t )RI_INFINITY;
	ri_float_t          tvals[2];
	ri_float_t          raypos[3];
	ri_float_t          nextx, nexty, nextz;
	ri_float_t          deltax, deltay, deltaz;
	int                 stepx = 0, stepy = 0, stepz = 0;
	int                 outx, outy, outz;
	ri_vector_t         isectpoint;
	ri_tri_info_t       tri;
	ri_float_t          nearest = ( ri_float_t )RI_INFINITY;
	ri_float_t          isectt;
	ri_float_t          u, v;
	const ri_ugrid_t   *grid;
	ri_render_t        *render;
	int                 tnum;

	int                 hitindex = -1;
	ri_float_t          hitu = 0.0f, hitv = 0.0f;
	ri_geom_t *RESTRICT hitgeom = NULL;

#if !defined(USE_ZORDER)
	int                 blkwidth  = ugrid->blkwidth;
	int                 blksize   = ugrid->blksize;
	int                 blkshift  = ugrid->shiftsize;
	int                 blkmask   = ugrid->bitmask;
#endif

	eye = ray->org;
	dir = ray->dir;

	if (dir.f[0] == 0.0 && dir.f[1] == 0.0 && dir.f[2] == 0.0) return 0;

	grid = ugrid;

	if (inside( eye, grid->bboxmin, grid->bboxmax ) )
		rayt = 0.0;
	else {
		if (!intersect_ray_bbox( eye, dir,
					grid->bboxmin, grid->bboxmax, tvals ) )
			return 0;
		rayt = tvals[0];
		maxt = tvals[1];
	}

	raypos[0] = eye.f[0] + dir.f[0] * rayt;
	raypos[1] = eye.f[1] + dir.f[1] * rayt;
	raypos[2] = eye.f[2] + dir.f[2] * rayt;

	tnum = ray->thread_num;
	assert( tnum >= 0 && tnum < RI_MAX_THREADS );

	x = ( int )( ( raypos[0] - grid->bboxmin[0] ) * grid->invwidth[0] );
	if (x >= grid->voxels[0]) x = grid->voxels[0] - 1;

	if (fabs( dir.f[0] ) < DIREPS) {
		nextx  = RI_INFINITY;
		deltax = 0.0;
		stepx  = 0;
		outx   = -1;
	} else if (dir.f[0] > 0) {
		nextx = rayt +
			( ( ( x + 1 ) * grid->width[0] + grid->bboxmin[0] ) -
			 raypos[0] ) / dir.f[0];

		deltax = grid->width[0] / dir.f[0];
		stepx = 1;
		outx = grid->voxels[0];
	} else {
		nextx = rayt +
			( ( x * grid->width[0] + grid->bboxmin[0] ) -
			 raypos[0] ) / dir.f[0];

		deltax = -grid->width[0] / dir.f[0];
		stepx = -1;
		outx = -1;
	}

	y = ( int )( ( raypos[1] - grid->bboxmin[1] ) * grid->invwidth[1] );
	if (y >= grid->voxels[1]) y = grid->voxels[1] - 1;

	if (fabs( dir.f[1] ) < DIREPS) {
		nexty  = RI_INFINITY;
		deltay = 0.0;
		stepy  = 0;
		outy   = -1;
	} else if (dir.f[1] > 0) {
		nexty = rayt +
			( ( ( y + 1 ) * grid->width[1] + grid->bboxmin[1] ) -
			 raypos[1] ) / dir.f[1];

		deltay = grid->width[1] / dir.f[1];
		stepy = 1;
		outy = grid->voxels[1];
	} else {
		nexty = rayt +
			( ( y * grid->width[1] + grid->bboxmin[1] ) -
			 raypos[1] ) / dir.f[1];

		deltay = -grid->width[1] / dir.f[1];
		stepy = -1;
		outy = -1;
	}

	z = ( int )( ( raypos[2] - grid->bboxmin[2] ) * grid->invwidth[2] );
	if (z >= grid->voxels[2]) z = grid->voxels[2] - 1;

	if (fabs( dir.f[2] ) < DIREPS) {
		nextz  = RI_INFINITY;
		deltaz = 0.0;
		stepz  = 0;
		outz   = -1;
	} else if (dir.f[2] > 0) {
		nextz = rayt +
			( ( ( z + 1 ) * grid->width[2] + grid->bboxmin[2] ) -
			 raypos[2] ) / dir.f[2];

		deltaz = grid->width[2] / dir.f[2];
		stepz = 1;
		outz = grid->voxels[2];
	} else {
		nextz = rayt +
			( ( z * grid->width[2] + grid->bboxmin[2] ) -
			 raypos[2] ) / dir.f[2];

		deltaz = -grid->width[2] / dir.f[2];
		stepz = -1;
		outz = -1;
	}

	hit = 0;

	if (x < 0) x = 0;
	if (x > grid->voxels[0] - 1) x = grid->voxels[0] - 1;
	if (y < 0) y = 0;
	if (y > grid->voxels[1] - 1) y = grid->voxels[1] - 1;
	if (z < 0) z = 0;
	if (z > grid->voxels[2] - 1) z = grid->voxels[2] - 1;

	render = ri_render_get();


	while (1) {

		render->stat.ngridtravs++;

#if USE_ZORDER	/* use z order memory access */
		index = MAP_Z3D( x, y, z );
#else		/* blocked memory access */
		index = MAP_XYZ( x, y, z, blkshift, blksize, blkwidth, blkmask );
#endif

		if (grid->cdat[index]) {
			//if (grid->cell[z][y][x]) {
			//printf("isect cell[%d][%d][%d]\n", z, y, x);
			//fflush(stdout);
			if (intersect_foreach_trilist_simd(
				    //grid->cell[z][y][x]->simdtris,
				    grid->cdat[index]->simdtris,
				    eye, dir,
				    tnum,
				    &tri,
				    &isectt, &u, &v ) ) {
				if (isectt > EPS && isectt <= nearest) {

					hit = 1;

					nearest = isectt;

					hitindex = tri.index;
					hitgeom  = tri.geom;
					hitu     = u;
					hitv     = v;

					/* if intersection point is in this
					 * voxel, terminate traversal.
					 */
					ri_vector_copy( &isectpoint, dir );
					ri_vector_scale( &isectpoint,
							  isectpoint,
							  isectt );
					ri_vector_add( &isectpoint,
						        isectpoint,
						        eye );

					if (inside_voxel( isectpoint, x, y, z,
							  grid ) )
						break;
				}
			}
		}

		/* 3D DDA */
		if ( ( nextx < nexty ) && ( nextx < nextz ) ) {
			if (maxt < nextx) break;
			x += stepx;
			if (x == outx) break;
			nextx += deltax;
		} else if ( ( nextz < nexty ) ) {
			if (maxt < nextz) break;
			z += stepz;
			if (z == outz) break;
			nextz += deltaz;
		} else {
			if (maxt < nexty) break;
			y += stepy;
			if (y == outy) break;
			nexty += deltay;
		}
	}

	if (hit) {
#if 0	// TODO: implement
		( *t ) = nearest;
		build_surfinfo( info, &isectpoint, hitgeom, hitindex, hitu,
			       hitv );
#endif
	}

	return hit;

}

static int
intersect_foreach_trilist_simd(
	const ri_simd_tri_info_t *list,
	const ri_vector_t         ray_org,
	const ri_vector_t         ray_dir,
	int                       thread_num,
	ri_tri_info_t            *hittri,
	ri_float_t               *t,
	ri_float_t               *u,
	ri_float_t               *v )
{
	int                i; //, j;
	int                hashit;
	ri_float_t              nearest = RI_INFINITY;

	ri_vector_t hit_t;
	ri_vector_t hit_u;
	ri_vector_t hit_v;
	ri_vector_t hit_det;

	ri_vector_t rox, roy, roz;
	ri_vector_t rdx, rdy, rdz;

	ri_vector_set1(&rox, ray_org.f[0]);
	ri_vector_set1(&roy, ray_org.f[1]);
	ri_vector_set1(&roz, ray_org.f[2]);

	ri_vector_set1(&rdx, ray_dir.f[0]);
	ri_vector_set1(&rdy, ray_dir.f[1]);
	ri_vector_set1(&rdz, ray_dir.f[2]);

	( *t ) = -1.0;
	hashit = 0;

	hit_det.f[0] = hit_det.f[1] = hit_det.f[2] = hit_det.f[3] = 1.0;
	hit_t.f[0]   = hit_t.f[1]   = hit_t.f[2]   = hit_t.f[3]   = RI_INFINITY;

	ri_render_get()->stat.ntesttris += list->nblocks * 4;

	for (i = 0; i < list->nblocks; i++) {

		v0x = _mm_load_ps(list->tridataptr + 36 * i + 0);
		v0y = _mm_load_ps(list->tridataptr + 36 * i + 4);
		v0z = _mm_load_ps(list->tridataptr + 36 * i + 8);

		e1x = _mm_load_ps(list->tridataptr + 36 * i + 12);
		e1y = _mm_load_ps(list->tridataptr + 36 * i + 16);
		e1z = _mm_load_ps(list->tridataptr + 36 * i + 20);

		e2x = _mm_load_ps(list->tridataptr + 36 * i + 24);
		e2y = _mm_load_ps(list->tridataptr + 36 * i + 28);
		e2z = _mm_load_ps(list->tridataptr + 36 * i + 32);

		isect_sse( rox, roy, roz,
			   rdx, rdy, rdz,
			   v0x, v0y, v0z,
			   e1x, e1y, e1z,
			   e2x, e2y, e2z,
			   hit_t


		/* unroll loop */
		/* for (j = 0; j < 4; j++) */

		/* (FP1 >= FP2)  ==  !(FP1 < FP2) */
		if (!( hitt[0] < EPS ) &&
		    !( hitt[0] > nearest ) ) {
			( *t ) = hitt[0];
			( *u ) = hitu[0];
			( *v ) = hitv[0];
			nearest = hitt[0];
			hashit  = 1;

			hittri->index = list->indices[4 * i + 0];
			hittri->geom  = list->geoms[4 * i + 0];
		}

		if (!( hitt[1] < EPS ) &&
		    !( hitt[1] > nearest ) ) {
			( *t ) = hitt[1];
			( *u ) = hitu[1];
			( *v ) = hitv[1];
			nearest = hitt[1];
			hashit  = 1;

			hittri->index = list->indices[4 * i + 1];
			hittri->geom  = list->geoms[4 * i + 1];
		}

		if (!( hitt[2] < EPS ) &&
		    !( hitt[2] > nearest ) ) {
			( *t ) = hitt[2];
			( *u ) = hitu[2];
			( *v ) = hitv[2];
			nearest = hitt[2];
			hashit  = 1;

			hittri->index = list->indices[4 * i + 2];
			hittri->geom  = list->geoms[4 * i + 2];
		}

		if (!( hitt[3] < EPS ) &&
		    !( hitt[3] > nearest ) ) {
			( *t ) = hitt[3];
			( *u ) = hitu[3];
			( *v ) = hitv[3];
			nearest = hitt[3];
			hashit  = 1;

			hittri->index = list->indices[4 * i + 3];
			hittri->geom  = list->geoms[4 * i + 3];
		}
	}

	return hashit;
}
#endif

void
build_state(
	ri_intersection_state_t *state,	/* [out] */
	const ri_vector_t       *eye,
	const ri_vector_t       *dir,
	const ri_float_t         t,
	const ri_geom_t         *geom,
	uint32_t                 index,
	ri_float_t               u,
	ri_float_t               v )
{
	ri_vector_t           tmpbasis[3];
	ri_vector_t           defcol;
	uint32_t          i0, i1, i2;
	ri_vector_t v0;
	ri_vector_t v1;
	ri_vector_t v2;
	ri_vector_t n0;
	ri_vector_t n1;
	ri_vector_t n2;
	ri_vector_t c0;
	ri_vector_t c1;
	ri_vector_t c2;

	ri_vector_t isectpoint;

	ri_vector_copy( &isectpoint, dir );
	ri_vector_scale( &isectpoint, &isectpoint, t );
	ri_vector_add( &isectpoint, &isectpoint, eye );

	ri_vector_copy( &( state->P ), &isectpoint );

	i0 = geom->indices[index + 0];
	i1 = geom->indices[index + 1];
	i2 = geom->indices[index + 2];

	v0 = geom->positions[i0];
	v1 = geom->positions[i1];
	v2 = geom->positions[i2];

	if (geom->normals) {
		n0 = geom->normals[i0];
		n1 = geom->normals[i1];
		n2 = geom->normals[i2];

		lerp_normal( &( state->Ng ), &n0, &n1, &n2, u, v );

		if (geom->tangents && geom->binormals) {

			n0 = geom->tangents[i0];
			n1 = geom->tangents[i1];
			n2 = geom->tangents[i2];

			lerp_normal( &( state->tangent ),
				    &n0, &n1, &n2, u, v );

			n0 = geom->binormals[i0];
			n1 = geom->binormals[i1];
			n2 = geom->binormals[i2];

			lerp_normal( &( state->binormal ),
				    &n0, &n1, &n2, u, v );
		} else {

			ri_ortho_basis( tmpbasis, &state->Ng );
			ri_vector_copy( &( state->tangent ),
				       &tmpbasis[0] );
			ri_vector_copy( &( state->binormal ),
				       &tmpbasis[1] );
		}

	} else {

		calculate_normal( &( state->Ng ), &v0, &v1, &v2 );
		ri_ortho_basis( tmpbasis, &state->Ng );
		ri_vector_copy( &( state->tangent ),
			       &tmpbasis[0] );
		ri_vector_copy( &( state->binormal ),
			       &tmpbasis[1] );
	}

	if (geom->colors) {
		c0 = geom->colors[i0];
		c1 = geom->colors[i1];
		c2 = geom->colors[i2];

		lerp_normal( &( state->color ),
			    &c0, &c1, &c2, u, v );
	} else {
		defcol.f[0] = 1.0;
		defcol.f[1] = 1.0;
		defcol.f[2] = 1.0;
		defcol.f[3] = 1.0;

		ri_vector_copy( &( state->color ), &defcol );
	}

#if 0
	if (geom->opacities)
		state->opacity = geom->opacities[i0].f[0];
	else
		state->opacity = 1.0;
#endif

	if (geom->texcoords) {
		lerp_uv( &state->u, &state->v,
			&geom->texcoords[2 * i0],
			&geom->texcoords[2 * i1],
			&geom->texcoords[2 * i2],
			u, v );
	} else {
		state->u = 0.0;
		state->v = 0.0;
	}

	if (geom->two_side) {
		if (index < geom->nindices / 2) {
			state->inside = 0;
		} else {
			/* surface is inside of geometry */
			state->inside = 1;
		}
	} else {
		state->inside = 0;
	}

	state->geom  = (ri_geom_t *)geom;
	state->index = index;
	//state->kd    = geom->kd;
	//state->ks    = geom->ks;
}

static void
build_z_table()
{
	uint32_t i, j;
	uint32_t bit;
	uint32_t v;
	uint32_t ret;
	uint32_t mask = 0x1;
	int          shift;

	for (i = 0; i < 256; i++) {
		v = i;
		shift = 0;
		ret = 0;
		for (j = 0; j < 8; j++) {
			/* extract (j+1)'th bit */
			bit = ( v >> j ) & mask;

			ret += bit << shift;
			shift += 3;
		}

		g_z_table[i] = ret;
	}
}

/*
 * codes from
 * "Practical Analysis of Optimized Ray-Triangle Intersection"
 * Tomas Moeller
 * http://www.ce.chalmers.se/staff/tomasm/raytri/
 *
 * : we use opti1 version(divide at end).
 *   this version seems faster on x86 CPU.
 */
uint32_t
triangle_intersect(
	const ri_vector_t *orig,
	const ri_vector_t *dir,
	const ri_vector_t *v0,
	const ri_vector_t *v1,
	const ri_vector_t *v2,
	ri_float_t curr_t,
	uint32_t tid,
	ri_float_t *newt, 
	ri_float_t *newu, 
	ri_float_t *newv, 
	uint32_t *newtid )
{
	/* ray-triangle intersection test with back face culling test */

	ri_vector_t edge1, edge2, tvec, pvec, qvec;
	ri_float_t       det, inv_det;
	ri_float_t       u, v, t;

	ri_vector_sub( &edge1, v1, v0 );
	ri_vector_sub( &edge2, v2, v0 );

	/* begin calculation determinant */
	ri_vector_cross3( &pvec, dir, &edge2 );

	/* if determinant is near zero, ray lies in plane of triangle */
	det = ri_vector_dot3( &edge1, &pvec );

	if (det > EPSILON) {

		ri_vector_sub( &tvec, orig, v0 );

		u = ri_vector_dot3( &tvec, &pvec );
		if (u < 0.0 || u > det) return 0;

		ri_vector_cross3( &qvec, &tvec, &edge1 );

		v = ri_vector_dot3( dir, &qvec );
		if (v < 0.0 || u + v > det) return 0;
	} else
		/* ray is parallel to the plane of the triangle or
		 * the ray hits back-faced triangle. */
		return 0;

	inv_det = 1.0 / det;

	t = ri_vector_dot3( &edge2, &qvec );

	if (t < curr_t && t > EPS) {

		( *newt ) = t * inv_det;
		( *newu ) = u * inv_det;
		( *newv ) = v * inv_det;
		( *newtid ) = tid;

	}

	return 1;       /* hit! */
}

