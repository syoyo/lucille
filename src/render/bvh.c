/*
 *   lucille | Global Illumination renderer
 *
 *             written by Syoyo Fujita.
 *
 */

/*
 * Bounding Volume Hierarchies implementation.
 * It is optimized for SSE operation.
 * For more details, see
 *
 * - Ray Tracing Deformable Scenes using Dynamic Bounding Volume Hierarchies
 *   (revised version)
 *   Ingo Wald, Solomon Boulos, and Peter Shirley
 *   Technical Report, SCI Institute, University of Utah, No UUSCI-2006-023
 *   (conditionally accepted at ACM Transactions on Graphics), 2006
 *   <http://www.sci.utah.edu/~wald/Publications/index.html>
 *
 */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#ifdef WITH_SSE
#include <xmmintrin.h>
#include <emmintrin.h>
#endif

#include "bvh.h"
#include "accel.h"
#include "memory.h"
#include "timer.h"
#include "render.h"
#include "log.h"

//#define LOCAL_TEST
#define LOCAL_DEBUG

#define BVH_MAXDEPTH 100

// sizeof(triangle4_t) = 144 when float precision, 288 when double precision.
typedef struct BVH_DECL_ALIGN(32) _triangle4_t {

	ri_vector_t p0x, p0y, p0z;
	ri_vector_t e1x, e1y, e1z;
	ri_vector_t e2x, e2y, e2z;

	BVH_PAD( 32 );

} triangle4_t BVH_ATTRIB_ALIGN (32);

typedef struct BVH_DECL_ALIGN(32) _leaf_info_t {

	triangle4_t   *tris;	/* ptr to array of triangle data	*/
	ri_geom_t    **geoms;	/* ptr array of geometry		*/
	uint32_t     **indices; /* offset index in the geometry data	*/

	uint32_t       ntris;	/* # of triangles			*/
	uint32_t       ntris4;	/* = ceil(ntris / 4)			*/

	BVH_PAD( 32 );

} leaf_info_t BVH_ATTRIB_ALIGN ( 32 );

/*
 * Primitive information. Only used when BVH is constructed.
 *
 * sizeof(ri_bvh_primitive_t) = 16*3. Up to 800M primitives in 32-bit.
 */
typedef struct BVH_DECL_ALIGN(16) _ri_bvh_primitive_t {

	ri_vector_t       bmin_and_left_area;
	ri_vector_t       bmax_and_right_area;

	/* ptr to triangle data */
	ri_geom_t   *geom;
	uint32_t     index;

#if defined ( __x86_64__ )
	uint32_t     tmp[1];
#else
	uint32_t     tmp[2];
#endif

	BVH_PAD(16);

} ri_bvh_primitive_t BVH_ATTRIB_ALIGN ( 16 );

typedef struct _interval_t {
	ri_float_t min, max;
} interval_t;

typedef struct _bvh_stack_t {
	ri_bvh_node_t *node;

} bvh_stack_t;

bvh_stack_t bvh_stack[BVH_MAXDEPTH + 1];
int         bvh_stack_depth;

typedef struct _bvh_stat_t {
	uint64_t ntraversals;
	uint64_t ntested_tris;
	uint64_t nfailed_isects;
} bvh_stat_t;


bvh_stat_t  g_stat;


#define BVH_NODE_SIZE sizeof(ri_bvh_node_t)

#define BVH_ISLEAF( node ) ( (node)->child_node_ptr == 0 )
#define BVH_ISEMPTY( node ) ( (node)->ntriangles == 0 )
#define BVH_NTRIS( node ) ( (node)->ntriangles )
#define BVH_LEFTNODE( node ) ( (ri_bvh_node_t *)( (node)->child_node_ptr ))
#define BVH_RIGHTNODE( node ) (ri_bvh_node_t *)( (node)->child_node_ptr + BVH_NODE_SIZE )


/* ----------------------------------------------------------------------------
 *
 * Static functions
 *
 * ------------------------------------------------------------------------- */
static void calc_polybbox(
	const ri_vector_t       *v,
	ri_vector_t              min,
	ri_vector_t              max );

static void calc_scene_bbox(
	ri_vector_t      *min,				/* [out] */
	ri_vector_t      *max,				/* [out] */
	ri_list_t        *geom_list);

static uint64_t	calc_sum_triangles(
	ri_list_t *geom_list );

static void create_primitive_info_array(
	ri_bvh_primitive_t  *prims,			/* [out] */
	uint64_t             nprims,
	ri_list_t           *geomslist );

static void calc_bbox_of_primitives(
	ri_vector_t               bmin,
	ri_vector_t               bmax,
	const ri_bvh_primitive_t *prims,
	uint64_t start, uint64_t  end );

static int get_longetst_axis(
	ri_vector_t bmin,
	ri_vector_t bmax );

static ri_bvh_node_t *	bvh_pair_node_aloc();

static void bvh_build_tree_median(
	ri_bvh_node_t      *tree,
	ri_bvh_primitive_t *prims,
	uint64_t            start,
	uint64_t            end,
	ri_vector_t         scene_bmin,
	ri_vector_t         scene_bmax,
	int                 depth );

#if 0	// TODO
static ri_float_t		SAH(
	int   N1,
	ri_float_t left_area,
	int   N2,
	ri_float_t right_area,
	ri_float_t S,
	ri_float_t Taabb,
	ri_float_t Ttri );
#endif



/*
 * Function: ri_bvh_build
 *
 *     Builds Bounded Volume Hierarchy data structure for accelerated ray tracing.
 *
 * Parameters:
 *
 *     scenegeoms - geometry in the scene.
 *     method     - bvh construction method. default is BVH_MEDIAN
 *
 * Returns:
 *
 *     Built BVH data strucure.
 */
void *
ri_bvh_build()
{
	ri_bvh_t           *bvh;
	ri_bvh_node_t      *root;
	ri_timer_t         *timer;
	ri_bvh_primitive_t *prims;
	ri_vector_t         bmin, bmax;
	uint64_t            ntris;

	ri_scene_t         *scene;

	timer = ri_render_get()->context->timer;

	ri_log( LOG_INFO, "Building BVH ... " );
	ri_timer_start( timer, "BVH Construction" );

	bvh = ( ri_bvh_t * )ri_mem_alloc( sizeof( ri_bvh_t ) );

	//
	// 1. Compute scene bounding box.
	//
	scene = ri_render_get()->scene;
	calc_scene_bbox( &bmin, &bmax, scene->geom_list );

	bvh->bmin.f[0] = bmin.f[0];
	bvh->bmin.f[1] = bmin.f[1];
	bvh->bmin.f[2] = bmin.f[2];

	bvh->bmax.f[0] = bmax.f[0];
	bvh->bmax.f[1] = bmax.f[1];
	bvh->bmax.f[2] = bmax.f[2];

	ri_log(LOG_INFO, "  bmin (%f, %f, %f)",
		bvh->bmin.f[0], bvh->bmin.f[1], bvh->bmin.f[2]);
	ri_log(LOG_INFO, "  bmax (%f, %f, %f)",
		bvh->bmax.f[0], bvh->bmax.f[1], bvh->bmax.f[2]);

	// 2. Create 1D array of triangle primitive infos.
	//    This information is just used through BVH construction phase,
	//     not through traversal phase.
	
	ntris = calc_sum_triangles( scene->geom_list );

	ri_log(LOG_INFO, "  # of tris = %d", ntris);

	prims = ( ri_bvh_primitive_t * )
		ri_aligned_alloc( sizeof(ri_bvh_primitive_t ) * ntris, 32 );

	create_primitive_info_array(
		prims,	 /* output */
		ntris,
		scene->geom_list );

	//
	// 3. Create root node.
	//
	root = ri_bvh_node_new();

	root->bmin[0] = bmin.f[0];
	root->bmin[1] = bmin.f[1];
	root->bmin[2] = bmin.f[2];

	root->bmax[0] = bmax.f[0];
	root->bmax[1] = bmax.f[1];
	root->bmax[2] = bmax.f[2];

	bvh->root = root;
	

	//
	// 3. Build BVH!
	//
	bvh_build_tree_median(
		bvh->root,
		prims,
		0,
		ntris,
		bvh->bmin,
		bvh->bmax,
		0 );

	ri_timer_end( timer, "BVH Construction" );

	ri_log( LOG_INFO, "BVH Construction time: %f sec",
	       ri_timer_elapsed( timer, "BVH Construction" ) );

	ri_log( LOG_INFO, "Built BVH." );

	return (void *)bvh;
}

void
ri_bvh_free( void *accel )
{
	ri_bvh_t *bvh = (ri_bvh_t *)accel;
}

int
ri_bvh_intersect(
	void                    *accel,
	ri_ray_t                *ray,
	ri_intersection_state_t *state)
{
	ri_bvh_t *bvh = (ri_bvh_t *)accel;
}

ri_bvh_node_t *
ri_bvh_node_new()
{
	ri_bvh_node_t *node;

	node = (ri_bvh_node_t *)ri_aligned_alloc(sizeof(ri_bvh_node_t), 32);

	assert(((uintptr_t)node % 32) == 0);	// check align

	memset(node, 0, sizeof(ri_bvh_node_t));

	return node;
}



/* ===========================================================================
 *
 * Private functions
 *
 */

int
bvh_stack_pop( 
ri_bvh_node_t **node,
ri_float_t t )
{
	( void )t;

	if (bvh_stack_depth < 1)
		return 0;
	else {
		bvh_stack_depth--;

		( *node ) = bvh_stack[bvh_stack_depth].node;
		return 1;
	}
}


void
ri_bvh_traverse(
ri_bvh_node_t *node, 
interval_t *initial_interval )
{
	interval_t interval;
	int        hit = 0;
	uint32_t   ntris;
	ri_float_t      t = 0.0f;

	interval = ( *initial_interval );

	while (node != NULL) {
		g_stat.ntraversals++;

		if (BVH_ISLEAF( node ) ) {

			if (!BVH_ISEMPTY( node ) ) {

				ntris = BVH_NTRIS( node );
				g_stat.ntested_tris += ntris;

				// isect test.

				if (!hit)
					g_stat.nfailed_isects += ntris;

			}

			if (!bvh_stack_pop( &node, t ) )
				return;

		} else {        // internal node

			bvh_stack[bvh_stack_depth].node = BVH_RIGHTNODE( node );
			bvh_stack_depth++;

			node = BVH_LEFTNODE( node );
		}
	}
}

static void
calc_polybbox( const ri_vector_t *v, ri_vector_t min, ri_vector_t max )
{
	min.f[0] = v[0].f[0] < v[1].f[0] ? v[0].f[0] : v[1].f[0];
	min.f[0] = min.f[0] < v[2].f[0] ? min.f[0] : v[2].f[0];
	min.f[1] = v[0].f[1] < v[1].f[1] ? v[0].f[1] : v[1].f[1];
	min.f[1] = min.f[1] < v[2].f[1] ? min.f[1] : v[2].f[1];
	min.f[2] = v[0].f[2] < v[1].f[2] ? v[0].f[2] : v[1].f[2];
	min.f[2] = min.f[2] < v[2].f[2] ? min.f[2] : v[2].f[2];

	max.f[0] = v[0].f[0] > v[1].f[0] ? v[0].f[0] : v[1].f[0];
	max.f[0] = max.f[0] > v[2].f[0] ? max.f[0] : v[2].f[0];
	max.f[1] = v[0].f[1] > v[1].f[1] ? v[0].f[1] : v[1].f[1];
	max.f[1] = max.f[1] > v[2].f[1] ? max.f[1] : v[2].f[1];
	max.f[2] = v[0].f[2] > v[1].f[2] ? v[0].f[2] : v[1].f[2];
	max.f[2] = max.f[2] > v[2].f[2] ? max.f[2] : v[2].f[2];
}

leaf_info_t *
make_leaf( int                ntriangles,
	  ri_bvh_primitive_t *prims,
	  uint64_t            start,
	  uint64_t            end )
{
	int          i;
	int          n;
	triangle4_t *p;
	leaf_info_t *info;

	( void )prims;
	( void )start;
	( void )end;

	n = ( int )ceil( ntriangles / ( ri_float_t )4 );

	info = ri_aligned_alloc( sizeof( leaf_info_t ), 16 );
	p = ri_aligned_alloc( sizeof( triangle4_t ) * n, 16 );

	for (i = 0; i < n; i++) {

		ri_vector_set4( &p[i].p0x, 0.0f, 0.0f, 0.0f, 0.0f );
		ri_vector_set4( &p[i].p0y, 0.0f, 0.0f, 0.0f, 0.0f );
		ri_vector_set4( &p[i].p0z, 0.0f, 0.0f, 0.0f, 0.0f );

		ri_vector_set4( &p[i].e1x, 0.0f, 0.0f, 0.0f, 0.0f );
		ri_vector_set4( &p[i].e1y, 0.0f, 0.0f, 0.0f, 0.0f );
		ri_vector_set4( &p[i].e1z, 0.0f, 0.0f, 0.0f, 0.0f );

		ri_vector_set4( &p[i].e2x, 0.0f, 0.0f, 0.0f, 0.0f );
		ri_vector_set4( &p[i].e2y, 0.0f, 0.0f, 0.0f, 0.0f );
		ri_vector_set4( &p[i].e2z, 0.0f, 0.0f, 0.0f, 0.0f );

	}

	info->tris   = p;
	info->ntris  = ntriangles;
	info->ntris4 = n;

	return info;

}


static void
calc_scene_bbox(
	ri_vector_t *bmin,		/* [out] */
	ri_vector_t *bmax,		/* [out] */
	ri_list_t   *geom_list)		/* [in]  */
{
	uint32_t     i;
	ri_vector_t  v;
	ri_list_t   *itr;
	ri_geom_t   *geom;

	bmin->f[0] = bmin->f[1] = bmin->f[2] =  RI_INFINITY;
	bmax->f[0] = bmax->f[1] = bmax->f[2] = -RI_INFINITY;

	for (itr  = ri_list_first( geom_list );
	     itr != NULL;
	     itr  = ri_list_next( itr ) ) {

		geom = ( ri_geom_t * )itr->data;

		for (i = 0; i < geom->npositions; i++) {
			v = geom->positions[i];

			if (bmin->f[0] > v.f[0]) bmin->f[0] = v.f[0];
			if (bmin->f[1] > v.f[1]) bmin->f[1] = v.f[1];
			if (bmin->f[2] > v.f[2]) bmin->f[2] = v.f[2];

			if (bmax->f[0] < v.f[0]) bmax->f[0] = v.f[0];
			if (bmax->f[1] < v.f[1]) bmax->f[1] = v.f[1];
			if (bmax->f[2] < v.f[2]) bmax->f[2] = v.f[2];
		}
	}
}

#if 0 
/* Calculate SAH cost. */
ri_float_t
SAH(
	int N1,
	ri_float_t left_area,
	int N2, 
	ri_float_t right_area, 
	ri_float_t S,
	ri_float_t Taabb, 
	ri_float_t Ttri )
{
	ri_float_t T;

	T = 2.0f * Taabb
	    + ( left_area / S ) * ( ri_float_t )N1 * Ttri
	    + ( right_area / S ) * ( ri_float_t )N2 * Ttri;

	return T;
}
#endif

static uint64_t
calc_sum_triangles(
	ri_list_t *geom_list )
{
	uint64_t   ntris;
	ri_list_t *itr;
	ri_geom_t *geom;

	ntris = 0;

	for (itr  = ri_list_first( geom_list );
	     itr != NULL;
	     itr  = ri_list_next( itr ) ) {
		geom = ( ri_geom_t * )itr->data;

		ntris += geom->nindices / 3;
	}

	return ntris;
}

static void
create_primitive_info_array(
	ri_bvh_primitive_t  *prims, /* [out] */
	uint64_t             nprims,
	ri_list_t           *geom_list )
{
	uint64_t         i, n;
	uint64_t         ntris = 0;

	ri_list_t       *itr;
	ri_geom_t       *geom;
	ri_vector_t      v[3];

	/*
	 * Precompute triangle's bounding box.
	 */
	for (itr  = ri_list_first( geom_list );
	     itr != NULL;
	     itr  = ri_list_next( itr ) ) {
		geom = ( ri_geom_t * )itr->data;

		n = geom->nindices / 3;
		for (i = 0; i < n; i++) {

			prims[i].geom  = geom;
			prims[i].index = i;

			v[0] = geom->positions[geom->indices[3 * i + 0]];
			v[1] = geom->positions[geom->indices[3 * i + 1]];
			v[2] = geom->positions[geom->indices[3 * i + 2]];

			calc_polybbox(
				v,
				prims[i].bmin_and_left_area,
				prims[i].bmax_and_right_area );

			prims[i].bmin_and_left_area.f[3]  = 0.0f;
			prims[i].bmax_and_right_area.f[3] = 0.0f;

		}

		ntris += n;
	}


	assert( ntris == nprims );

}

int sorter_x( const void *arg0, const void *arg1 )
{
	const ri_bvh_primitive_t *a = ( const ri_bvh_primitive_t * )arg0;
	const ri_bvh_primitive_t *b = ( const ri_bvh_primitive_t * )arg1;

	ri_float_t                     a_center, b_center;

	a_center = a->bmin_and_left_area.f[0] +
		0.5 * ( a->bmax_and_right_area.f[0] - a->bmin_and_left_area.f[0] );
	b_center = b->bmin_and_left_area.f[0] +
		0.5 * ( b->bmax_and_right_area.f[0] - b->bmin_and_left_area.f[0] );

	if (a_center < b_center) return -1;
	if (a_center > b_center) return 1;
	return 0;
}

int sorter_y( const void *arg0, const void *arg1 )
{
	const ri_bvh_primitive_t *a = ( const ri_bvh_primitive_t * )arg0;
	const ri_bvh_primitive_t *b = ( const ri_bvh_primitive_t * )arg1;

	ri_float_t                     a_center, b_center;

	a_center = a->bmin_and_left_area.f[1] + 0.5 * ( a->bmax_and_right_area.f[1] - a->bmin_and_left_area.f[1] );
	b_center = b->bmin_and_left_area.f[1] + 0.5 * ( b->bmax_and_right_area.f[1] - b->bmin_and_left_area.f[1] );

	if (a_center < b_center) return -1;
	if (a_center > b_center) return 1;
	return 0;
}

int sorter_z( const void *arg0, const void *arg1 )
{
	const ri_bvh_primitive_t *a = ( const ri_bvh_primitive_t * )arg0;
	const ri_bvh_primitive_t *b = ( const ri_bvh_primitive_t * )arg1;

	ri_float_t                     a_center, b_center;

	a_center = a->bmin_and_left_area.f[2] + 0.5 * ( a->bmax_and_right_area.f[2] - a->bmin_and_left_area.f[2] );
	b_center = b->bmin_and_left_area.f[2] + 0.5 * ( b->bmax_and_right_area.f[2] - b->bmin_and_left_area.f[2] );

	if (a_center < b_center) return -1;
	if (a_center > b_center) return 1;
	return 0;
}

#define BVH_MAXDEPTH 100

typedef int ( *sortfunc )( const void *a, const void *b );

void
bvh_build_tree_median(
	ri_bvh_node_t      *tree,
	ri_bvh_primitive_t *prims,
	uint64_t            start,
	uint64_t            end,
	ri_vector_t         scene_bmin,
	ri_vector_t         scene_bmax,
	int                 depth )
{
	uint32_t       ntriangles;
	ri_vector_t    node_bmin, node_bmax;
	ri_vector_t    bmin, bmax;
	ri_vector_t    bbox_center;
	int            axis_longest;

	sortfunc sorter[] = { sorter_x, sorter_y, sorter_z };

#ifdef LOCAL_DEBUG
	printf("tree: start = 0x%016x, end = 0x%016x\n", start, end);
#endif

#if 0	// TODO

	/*
	 * Terminate recursion and make the leaf node if # of tris == 1 or
	 * the depth exceeds max tree depth limit.
	 */
	if ( ( ( end - start ) <= 1 ) || ( depth > BVH_MAXDEPTH ) ) {

		bvh_node_set_flag(tree, BVH_NODE_LEAF);

		/*
		 * Up to 2^30
		 */
		if ( ( end - start ) >= ( 0x0000000100000000ULL >> 2ULL ) ) {
			fprintf( stderr,
				"Too large triangles in one leaf. \
				Such a large value is not supported yet...\n"                                         );
			exit( -1 );
		}

		ntriangles = ( uint32_t )( end - start );

		bvh_node_set_flag(tree, BVH_NODE_LEAF);

		if (ntriangles == 0)  // empty node
			bvh_node_set_data(tree, NULL);
		else {

			/*
			 * If we assume that the pointer address is a multiple
			 * of sizoef(leaf_info_t), we can store the value of
			 * the pointer address divided by sizeof(leaf_info_t)
			 * to save the data range.
			 */
			bvh_node_set_data(
				tree, 
				make_leaf( ntriangles, prims, start, end ) );

		}

	}

	/*
	 * Recursively subdivide the bouding box.
	 */

	calc_bbox_of_primitives( node_bmin, node_bmax, prims, start, end );

	/*
	 * Narrow bbox until it contains node's bbox
	 */
	bmin.f[0] = scene_bmin.f[0]; bmax.f[0] = scene_bmax.f[0];
	bmin.f[1] = scene_bmin.f[1]; bmax.f[1] = scene_bmax.f[1];
	bmin.f[2] = scene_bmin.f[2]; bmax.f[2] = scene_bmax.f[2];

	while (1) {
		bbox_center.f[0] = bmin.f[0] + 0.5 * ( bmax.f[0] - bmin.f[0] );
		bbox_center.f[1] = bmin.f[1] + 0.5 * ( bmax.f[1] - bmin.f[1] );
		bbox_center.f[2] = bmin.f[2] + 0.5 * ( bmax.f[2] - bmin.f[2] );

		axis_longest = get_longetst_axis( bmin, bmax );

		/*
		 * Halve the bbox when the halve of the scene bbox
		 * (splitted by longest axis) doesn't contain the node's bbox,
		 * then continue narrowing the scene bbox.
		 */
		if (bbox_center.f[axis_longest] < node_bmin.f[axis_longest])
			bmin.f[axis_longest] = bbox_center.f[axis_longest];
		else if (bbox_center.f[axis_longest] > node_bmax.f[axis_longest])
			bmax.f[axis_longest] = bbox_center.f[axis_longest];
		else
			break;
	}

	/*
	 * find split axis
	 */

	int      k;
	uint64_t i;
	int      axis_try;
	ri_float_t    center, prim_center;
	int64_t  idx;

	int      found;
	int      right_is_zero, left_is_zero;
	int      split_axis;
	int64_t  split_idx = 0;

	found = 0;
	right_is_zero = 0; left_is_zero = 0;
	split_axis = -1;

	/* test for all axis */
	for (k = 0; k < 3; k++) {
		axis_try = ( axis_longest + k ) % 3; /* start from longest axis */

		center = bmin.f[axis_try] + (ri_float_t)0.5 *
			 ( bmax.f[axis_try] - bmin.f[axis_try] );

		/* sort primitive list in axis `axis_try' */
		qsort( prims + start, ( end - start ),
		      sizeof( ri_bvh_primitive_t ), sorter[axis_try] );

		/* try to split primitive list with spatial median in axis
		 * `axis_try'
		 */
		idx = -1;
		for (i = start; i < end; i++) {
			prim_center = (ri_float_t)0.5f *
				( prims[i].bmax_and_right_area.f[axis_try] -
				  prims[i].bmin_and_left_area.f[axis_try] );

			prims[i].bmin_and_left_area.f[axis_try] = prim_center;

			if (prim_center < center)
				idx = i + 1;
		}

		if (idx == -1) { /* left is empty */

			if (k == 0) left_is_zero = 1;

		} else if (idx == ( int64_t )end) { /* right is empty */

			if (k == 0) right_is_zero = 1;

		} else {

			found = 1;
			split_axis = axis_try;
			split_idx  = idx;

			break;
		}
	}

	ri_vector_t    left_bmin,  left_bmax;
	ri_vector_t    right_bmin, right_bmax;

	left_bmin.f[0] = bmin.f[0]; left_bmax.f[0] = bmax.f[0];
	left_bmin.f[1] = bmin.f[1]; left_bmax.f[1] = bmax.f[1];
	left_bmin.f[2] = bmin.f[2]; left_bmax.f[2] = bmax.f[2];

	right_bmin.f[0] = bmin.f[0]; right_bmax.f[0] = bmax.f[0];
	right_bmin.f[1] = bmin.f[1]; right_bmax.f[1] = bmax.f[1];
	right_bmin.f[2] = bmin.f[2]; right_bmax.f[2] = bmax.f[2];

	if (!found) {   /* no split axis candidate found */

		if (left_is_zero) {

			right_bmin.f[axis_longest] = bbox_center.f[axis_longest];

			/* Recurse building by halving the scene bbox */
			bvh_build_tree_median( tree, prims, start, end,
					      right_bmin, right_bmax, depth + 1 );

			return;

		} else if (right_is_zero) {

			left_bmax.f[axis_longest] = bbox_center.f[axis_longest];

			/* Recurse building by halving the scene bbox */
			bvh_build_tree_median( tree, prims, start, end,
					      left_bmin, left_bmax, depth + 1 );

			return;

		} else {

			/*
			 * NEVER come here, I think...
			 */
			assert(0);

			/* No good paritition found. split at half index
			 * of the primitive list.
			 */
			split_axis = axis_longest;

			qsort( prims + start, ( end - start ),
			      sizeof( ri_bvh_primitive_t ),
			      sorter[split_axis] );

			split_idx = start + ( end - start ) / 2;

		}

	} else {        /* found good split position */

		if (split_axis == -1) {
			ri_log( LOG_ERROR, "???\n" );
			exit( -1 );
		}

		center = bmin.f[split_axis] + 0.5f *
			 ( bmax.f[split_axis] - bmin.f[split_axis] );
		left_bmax.f[split_axis] = center;
		right_bmin.f[split_axis] = center;

	}

	/*
	 * Recurse building BVH tree.
	 */
	bvh_node_set_data(tree,  bvh_pair_node_aloc());

	tree->bmin[0] = bmin.f[0];
	tree->bmin[1] = bmin.f[1];
	tree->bmin[2] = bmin.f[2];

	tree->bmax[0] = bmax.f[0];
	tree->bmax[1] = bmax.f[1];
	tree->bmax[2] = bmax.f[2];


	/* left  = prims[start, ..., split_idx)
	 * right = prims[split_idx, ..., end)
	 */
	assert( split_idx > ( int64_t )start );
	assert( split_idx < ( int64_t )end );

	ri_bvh_node_t *left_node, *right_node;

	left_node  = BVH_LEFTNODE(tree);
	right_node = BVH_RIGHTNODE(tree);

	bvh_build_tree_median( left_node, prims, start, split_idx,
			      left_bmin, left_bmax, depth + 1 );

	bvh_build_tree_median( right_node, prims, split_idx, end,
			      right_bmin, right_bmax, depth + 1 );

#endif	// TODO


}

void
calc_bbox_of_primitives(
	ri_vector_t bmin,					/* [out] */
	ri_vector_t bmax,					/* [out] */
	const ri_bvh_primitive_t *prims,
	uint64_t start, uint64_t end )
{
	uint64_t i;

	bmin.f[0] = prims[start].bmin_and_left_area.f[0];
	bmin.f[1] = prims[start].bmin_and_left_area.f[1];
	bmin.f[2] = prims[start].bmin_and_left_area.f[2];

	bmax.f[0] = prims[start].bmax_and_right_area.f[0];
	bmax.f[1] = prims[start].bmax_and_right_area.f[1];
	bmax.f[2] = prims[start].bmax_and_right_area.f[2];

	for (i = start + 1; i < end; i++) {
		if (bmin.f[0] > prims[start].bmin_and_left_area.f[0]) {
			bmin.f[0] = prims[start].bmin_and_left_area.f[0];
		}

		if (bmin.f[1] > prims[start].bmin_and_left_area.f[1]) {
			bmin.f[1] = prims[start].bmin_and_left_area.f[1];
		}

		if (bmin.f[2] > prims[start].bmin_and_left_area.f[2]) {
			bmin.f[2] = prims[start].bmin_and_left_area.f[2];
		}

		if (bmax.f[0] < prims[start].bmax_and_right_area.f[0]) {
			bmax.f[0] = prims[start].bmax_and_right_area.f[0];
		}

		if (bmax.f[1] < prims[start].bmax_and_right_area.f[1]) {
			bmax.f[1] = prims[start].bmax_and_right_area.f[1];
		}
		
		if (bmax.f[2] < prims[start].bmax_and_right_area.f[2]) {
			bmax.f[2] = prims[start].bmax_and_right_area.f[2];
		}
	}
}

int
get_longetst_axis( ri_vector_t bmin, ri_vector_t bmax )
{
	ri_float_t width[3];
	ri_float_t longest;
	int   axis;

	width[0] = bmax.f[0] - bmin.f[0];
	width[1] = bmax.f[1] - bmin.f[1];
	width[2] = bmax.f[2] - bmin.f[2];

	longest = width[0]; axis = 0;
	if (width[1] > longest) {
		longest = width[1]; axis = 1;
	}
	if (width[2] > longest) {
		longest = width[2]; axis = 2;
	}

	return axis;
}


/*
 * Allocate pair of nodes(left and right) at once.
 */
ri_bvh_node_t *
bvh_pair_node_aloc()
{
	ri_bvh_node_t *ptr;

	ptr = ri_aligned_alloc( sizeof( ri_bvh_node_t ) * 2,
			       sizeof( ri_bvh_node_t ) );

	return ptr;
}


#ifdef LOCAL_TEST
int
main( int argc, char **argv )
{
	return 0;
}
#endif





