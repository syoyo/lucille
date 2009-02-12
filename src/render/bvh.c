/*
 *   lucille | Global Illumination renderer
 *
 *             written by Syoyo Fujita.
 *
 */

/*
 * Bounding Volume Hierarchies for raytracing acceleration.
 *
 * BVH Construction uses binning-based SAH [2] for fast and robust BVH
 * constriction.
 *
 * At this time, 2-ary BVH is constructed while data structure uses qbvh node.
 *
 * TODO:
 *
 *   - Construct 4-ary BVH [1].
 *   - Optimize BVH construction and traversal by MUDA language.
 *   - Reduce memory consuption for large data.
 *     - e.g. LBVH(lightweight BVH)
 *   - Out-of-core construction and traversal for massive data?
 *
 *
 * References:
 *  
 * [1] 4-ary BVH by Kimura's thesis.
 *     <http://www.jaist.ac.jp/library/thesis/ks-master-2007/paper/
 *      h-kimura/paper.pdf>
 *
 * [2] Highly Parallel Fast KD-tree Construction for Interactive Ray Tracing
 *     of Dynamic Scenes
 *     Maxim Shevtsov, Alexei Soupikov and Alexander Kapustin.
 *     EUROGRAPHICS 2007
 *     <http://graphics.cs.uni-sb.de/Courses/ss07/sem/index.html>
 *
 * [3] Ray Tracing Deformable Scenes using Dynamic Bounding Volume Hierarchies
 *     (revised version)
 *     Ingo Wald, Solomon Boulos, and Peter Shirley
 *     Technical Report, SCI Institute, University of Utah, No UUSCI-2006-023
 *     (conditionally accepted at ACM Transactions on Graphics), 2006
 *     <http://www.sci.utah.edu/~wald/Publications/index.html>
 *
 * $Id$
 *
 */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "common.h"
#include "bvh.h"
#include "accel.h"
#include "memory.h"
#include "timer.h"
#include "render.h"
#include "beam.h"
#include "raster.h"
#include "log.h"

#ifdef WITH_SSE
#include <xmmintrin.h>
#endif

#ifdef WITH_MUDA
#include "muda/rayaabb.c"
#endif  /* WITH_MUDA */

/*
 * Local flags
 */
//#define BVH_ENABLE_VOLUME_HEURISTIC_SPLIT

/*
 * BVH settings
 */
#define BVH_MAXDEPTH          100
#define BVH_NTRIS_LEAF          2        /* TODO: parameterize.  */
#define BVH_BIN_SIZE           64
#define BVH_MAXMISSBEAMS     1024

/*
 * Buffer for binning to compute approximated SAH 
 */
typedef struct _bvh_bin_buffer_t {

    uint32_t bin[2][3][BVH_BIN_SIZE];  /* (min, max) * xyz * binsize   */

} bvh_bin_buffer_t;

bvh_bin_buffer_t g_binbuf;


typedef struct _triangle4_t {

    ri_vector_t p0x, p0y, p0z;
    ri_vector_t e1x, e1y, e1z;
    ri_vector_t e2x, e2y, e2z;

} triangle4_t;


typedef struct _tri_bbox_t {

    ri_vector_t bmin;
    ri_vector_t bmax;

    uint64_t    index;          /* TODO: compaction.   */

} tri_bbox_t;



typedef struct _interval_t {
    ri_float_t min, max;
} interval_t;

/*
 * Traversal stack
 */
typedef struct _bvh_stack_t {

    ri_qbvh_node_t *nodestack[BVH_MAXDEPTH + 1];
    int             depth;

} bvh_stack_t;

/*
 * Miss beam stack
 */
typedef struct _bvh_miss_beam_stack_t {

    int        stack[BVH_MAXDEPTH + 1];      /* store offset for miss_beams */
    int        depth;

} bvh_miss_beam_stack_t;

ri_beam_t  g_miss_beams[BVH_MAXMISSBEAMS];

/*
 * Singleton
 */
ri_bvh_stat_traversal_t       g_stattrav;
ri_bvh_stat_beam_traversal_t  g_beamstattrav;


/* ----------------------------------------------------------------------------
 *
 * Static functions
 *
 * ------------------------------------------------------------------------- */

static ri_qbvh_node_t *ri_qbvh_node_new();

static void get_bbox_of_triangle(
          ri_vector_t        bmin_out,           /* [out] */  
          ri_vector_t        bmax_out,           /* [out] */  
    const ri_triangle_t     *triangle);

static void calc_scene_bbox(
          ri_vector_t        bmin_out,           /* [out]    */
          ri_vector_t        bmax_out,           /* [out]    */
    const tri_bbox_t        *tri_bboxes,
          uint64_t           ntriangles);

static void create_triangle_list(
          ri_triangle_t    **triangles_out,      /* [out]    */
          tri_bbox_t       **tri_bboxes_out,     /* [out]    */
          uint64_t          *ntriangles,         /* [out]    */
    const ri_list_t         *geom_list);

static void bbox_add_margin(
          ri_vector_t        bmin,               /* [inout]  */
          ri_vector_t        bmax);              /* [inout]  */

static int bin_triangle_edge(
          bvh_bin_buffer_t  *binbuf,             /* [inout]  */
    const ri_vector_t        scene_bmin,
    const ri_vector_t        scene_bmax,
    const tri_bbox_t        *tri_bboxes,
          uint64_t           ntriangles);

static void calc_bbox_of_triangles(
          ri_vector_t        bmin_out,           /* [out]    */
          ri_vector_t        bmax_out,           /* [out]    */
    const tri_bbox_t        *tri_bboxes,
          uint64_t           ntriangles);

static int gather_triangles(
          ri_triangle_t     *triangles_to_out,   /* [out]    */
    const ri_triangle_t     *triangles_from,
    const tri_bbox_t        *tri_bboxes,
          uint64_t           ntriangles);

static inline int test_ray_aabb(
          ri_float_t        *tmin_out,           /* [out]    */
          ri_float_t        *tmax_out,           /* [out]    */
    const ri_vector_t        bmin,
    const ri_vector_t        bmax,
          ri_ray_t          *ray);

static int bvh_construct(
          ri_qbvh_node_t    *root,
          ri_vector_t        bmin,
          ri_vector_t        bmax,
          ri_triangle_t     *triangles,
          ri_triangle_t     *triangles_buf,
          tri_bbox_t        *tri_bboxes,
          tri_bbox_t        *tri_bboxes_buf,
          uint64_t           index_left,
          uint64_t           index_right);

static int bvh_traverse(
          ri_intersection_state_t *state_out,   /* [out]                */
    const ri_qbvh_node_t          *root,
          ri_bvh_diag_t           *diag,        /* [modified]           */
          ri_ray_t                *ray,
          bvh_stack_t             *stack );     /* [buffer]             */


static int bvh_traverse_beam(
          ri_raster_plane_t       *raster_inout,/* [inout]              */
          ri_qbvh_node_t          *root,
          ri_bvh_diag_t           *diag,        /* [modified]           */
          ri_beam_t               *beam,
          bvh_stack_t             *stack );     /* [buffer]             */

static int bvh_traverse_beam_visibility(
          ri_qbvh_node_t          *root,
          ri_bvh_diag_t           *diag,        /* [modified]           */
          ri_beam_t               *beam,
          bvh_stack_t             *stack );     /* [buffer]             */

static void project_triangles(
          ri_triangle2d_t *tri2d_out,              /* [out]                */
    const ri_triangle_t   *triangles,              /* [in]                 */
          uint32_t         ntriangles,
          int              axis,
          ri_float_t       d,
          ri_vector_t      org);

static int test_beam_aabb(
          ri_vector_t  bmin,
          ri_vector_t  bmax,
    const ri_beam_t   *beam);


static void bvh_invalidate_cache_node( ri_qbvh_node_t *node );


static ri_bvh_diag_t *gdiag;                    /* TODO: thread-safe    */


/* ----------------------------------------------------------------------------
 *
 * Public functions
 *
 * ------------------------------------------------------------------------- */

/*
 * Function: ri_bvh_build
 *
 *     Builds Bounded Volume Hierarchy data structure for accelerated ray tracing.
 *
 * Parameters:
 *
 *     data - scene data of type ri_scene_t.
 *
 * Returns:
 *
 *     Built BVH data strucure.
 */
void *
ri_bvh_build(
    const void *data)
{
    ri_bvh_t           *bvh;
    ri_qbvh_node_t     *root;
    ri_timer_t         *tm;
    ri_vector_t         bmin, bmax;

    ri_scene_t         *scene = (ri_scene_t *)data;
    
    ri_triangle_t      *triangles;
    ri_triangle_t      *triangles_buf;          /* temporal buffer  */
    tri_bbox_t         *tri_bboxes;
    tri_bbox_t         *tri_bboxes_buf;         /* temporal buffer  */
    uint64_t            ntriangles;

    tm = ri_render_get()->context->timer;

    ri_log( LOG_INFO, "(BVH   ) Building BVH ... " );
    ri_timer_start( tm, "BVH Construction" );

    bvh = ( ri_bvh_t * )ri_mem_alloc( sizeof( ri_bvh_t ) );
    memset( bvh, 0, sizeof( ri_bvh_t ));


    /*
     * 1. Create 1D array of triangle and its bbox.
     */
    {
        create_triangle_list(&triangles,
                             &tri_bboxes,
                             &ntriangles,
                              scene->geom_list);

        if (ntriangles == 0) {
            /* No geometry in the scene. We build empty bvh structure. */
            bvh->empty = 1;
            return bvh;
        }

        tri_bboxes_buf = ri_mem_alloc(sizeof(tri_bbox_t) * ntriangles);
        ri_mem_copy(tri_bboxes_buf, tri_bboxes, sizeof(tri_bbox_t)*ntriangles);

        triangles_buf = ri_mem_alloc(sizeof(ri_triangle_t) * ntriangles);
        ri_mem_copy(triangles_buf, triangles, sizeof(ri_triangle_t)*ntriangles);
    }

    /*
     * 2. Calculate bounding box of the scene.
     */
    {
        calc_scene_bbox( bmin, bmax, tri_bboxes, ntriangles );

        bbox_add_margin( bmin, bmax );

        bvh->bmin[0] = bmin[0];
        bvh->bmin[1] = bmin[1];
        bvh->bmin[2] = bmin[2];

        bvh->bmax[0] = bmax[0];
        bvh->bmax[1] = bmax[1];
        bvh->bmax[2] = bmax[2];

        ri_log(LOG_INFO, "(BVH   )   bmin (%f, %f, %f)",
            bvh->bmin[0], bvh->bmin[1], bvh->bmin[2]);
        ri_log(LOG_INFO, "(BVH   )   bmax (%f, %f, %f)",
            bvh->bmax[0], bvh->bmax[1], bvh->bmax[2]);
    }
    
    ri_log(LOG_INFO, "(BVH   )    # of input tris = %d", ntriangles);


    /*
     * 3. Construct BVH.
     */
    root      = ri_qbvh_node_new();
    bvh->root = root;
    
    bvh_construct(
        bvh->root,
        bvh->bmin,
        bvh->bmax,
        triangles,
        triangles_buf,
        tri_bboxes,
        tri_bboxes_buf,
        0,
        ntriangles);


    ri_mem_free( triangles_buf );
    ri_mem_free( tri_bboxes );
    ri_mem_free( tri_bboxes_buf );

    ri_timer_end( tm, "BVH Construction" );

    ri_log( LOG_INFO, "(BVH   ) Construction time: %f sec",
           ri_timer_elapsed( tm, "BVH Construction" ) );

    ri_log( LOG_INFO, "(BVH   ) Built BVH." );

    return (void *)bvh;
}

void
ri_bvh_free( void *accel )
{
    ri_bvh_t *bvh = (ri_bvh_t *)accel;

    ri_mem_free(bvh);
}

void
bvh_invalidate_cache_node( ri_qbvh_node_t *node )
{
    int   i;
    void *ptr;

    if (node->is_leaf) {

        for (i = 0; i < 3; i++) {   /* for each axis */

            ptr = (void *)node->child[1+i];

            if (ptr) {  /* 2D triangle cache was created, release it */

                ri_mem_free(ptr);

                node->child[1+i] = NULL;
            }

        }

    } else {

        for (i = 0; i < 2; i++) {
            bvh_invalidate_cache_node(node->child[i]);
        }

    }

}

void
ri_bvh_invalidate_cache( void *accel )
{
    assert( accel != NULL );

    ri_bvh_t *bvh = (ri_bvh_t *)accel;

    bvh_invalidate_cache_node(bvh->root);
}

int
ri_bvh_intersect(
    void                    *accel,
    ri_ray_t                *ray,
    ri_intersection_state_t *state_out,
    void                    *user)
{
    ri_bvh_diag_t *diag_ptr;
    ri_bvh_t      *bvh;

    assert( accel     != NULL );
    assert( ray       != NULL );
    assert( state_out != NULL );

    bvh = (ri_bvh_t *)accel;

    if (bvh->empty) {
        /* Always no hit for empty accel structure. */
        return 0;
    }

    if (user) {
        diag_ptr = (ri_bvh_diag_t *)user;
        memset( diag_ptr, 0, sizeof(ri_bvh_diag_t));
    } else {
        diag_ptr = NULL;
    }
    
    int ret;

#ifdef RI_BVH_TRACE_STATISTICS
    g_stattrav.nrays++;
#endif

    /*
     * TODO: Provide indivisual stack for each thread.
     */
    bvh_stack_t stack;
    stack.depth = 0;

    /*
     * Precalculate ray coefficient.
     */
    ray->dir_sign[0] = (ray->dir[0] < 0.0) ? 1 : 0;
    ray->dir_sign[1] = (ray->dir[1] < 0.0) ? 1 : 0;
    ray->dir_sign[2] = (ray->dir[2] < 0.0) ? 1 : 0;

    if (fabs(ray->dir[0]) > RI_EPS) {
        ray->invdir[0] = 1.0 / ray->dir[0];
    } else {
        ray->invdir[0] = RI_FLT_MAX;        /* FIXME: make this +Inf ? */
    }

    if (fabs(ray->dir[1]) > RI_EPS) {
        ray->invdir[1] = 1.0 / ray->dir[1];
    } else {
        ray->invdir[1] = RI_FLT_MAX;
    }

    if (fabs(ray->dir[2]) > RI_EPS) {
        ray->invdir[2] = 1.0 / ray->dir[2];
    } else {
        ray->invdir[2] = RI_FLT_MAX;
    }

    ray->dir_sign[0] = (ray->dir[0] < 0.0) ? 1 : 0;
    ray->dir_sign[1] = (ray->dir[1] < 0.0) ? 1 : 0;
    ray->dir_sign[2] = (ray->dir[2] < 0.0) ? 1 : 0;

#if defined(WITH_MUDA) || defined(WITH_SSE)

    {
        uint64_t ones;
        uint64_t *ptr;

        ones = 0xffffffffffffffffULL;
        ptr = &ray->dir_signv[0];

        (*ptr++) = (ray->dir[0] < 0.0) ? ones : 0;
        (*ptr++) = (ray->dir[1] < 0.0) ? ones : 0;
        (*ptr  ) = (ray->dir[2] < 0.0) ? ones : 0;

    }

#endif

    /*
     * Firstly check if the ray hits scene bbox.
     */
    int hit;
    ri_float_t tmin, tmax;
    
    hit = test_ray_aabb( &tmin, &tmax, bvh->bmin, bvh->bmax, ray );
        
    if (!hit) {
        return 0;
    }

    ret = bvh_traverse(  state_out,
                         bvh->root,
                         diag_ptr, 
                         ray,
                        &stack );

    /*
     * If there's a hit, build intersection state.
     */
    if (ret) {
        ri_intersection_state_build( state_out, ray->org, ray->dir );
    }
                        
    return ret;
}

int
ri_bvh_intersect_beam(
    void                    *accel,
    ri_beam_t               *beam,
    ri_raster_plane_t       *raster_out,
    void                    *user)
{
    ri_bvh_diag_t *diag_ptr;
    ri_bvh_t      *bvh;

    assert( accel      != NULL );
    assert( beam       != NULL );
    assert( raster_out != NULL );

    bvh = (ri_bvh_t *)accel;

    if (bvh->empty) {
        /* Always no hit for empty accel structure. */
        return 0;
    }

    if (user) {
        diag_ptr = (ri_bvh_diag_t *)user;
        memset( diag_ptr, 0, sizeof(ri_bvh_diag_t));
    } else {
        diag_ptr = NULL;
    }
    
    int ret;

#ifdef RI_BVH_TRACE_STATISTICS
    g_stattrav.nbeams++;
#endif

    /*
     * TODO: Provide indivisual stack for each thread.
     */
    bvh_stack_t stack;
    stack.depth = 0;

    /*
     * Firstly check if the beam hits scene bbox.
     */
    int hit;
    
    hit = test_beam_aabb( bvh->bmin, bvh->bmax, beam );
        
    if (!hit) {
        return 0;
    }

    ret = bvh_traverse_beam(  raster_out,
                              bvh->root,
                              diag_ptr, 
                              beam,
                             &stack );

    /*
     * If there's a hit, build intersection state.
     */
    if (ret) {
        // ri_intersection_state_build( state_out, ray->org, ray->dir );
    }
                        
    return ret;
}


int
ri_bvh_intersect_beam_visibility(
    void                    *accel,
    ri_beam_t               *beam,
    void                    *user)
{
    ri_bvh_diag_t *diag_ptr;
    ri_bvh_t      *bvh;

    assert( accel      != NULL );
    assert( beam       != NULL );

    bvh = (ri_bvh_t *)accel;

    if (bvh->empty) {
        /* Always no hit for empty accel structure. */
        return 0;
    }

    if (user) {
        diag_ptr = (ri_bvh_diag_t *)user;
        memset( diag_ptr, 0, sizeof(ri_bvh_diag_t));
    } else {
        diag_ptr = NULL;
    }
    
    int ret;

#ifdef RI_BVH_TRACE_STATISTICS
    g_stattrav.nbeams++;
#endif

    /*
     * TODO: Provide indivisual stack for each thread.
     */
    bvh_stack_t stack;
    stack.depth = 0;

    /*
     * Firstly check if the beam hits scene bbox.
     */
    int hit;
    
    hit = test_beam_aabb( bvh->bmin, bvh->bmax, beam );
        
    if (!hit) {
        return RI_BEAM_MISS_COMPLETELY;       /* Completely misses */
    }

    ret = bvh_traverse_beam_visibility(  bvh->root,
                                         diag_ptr, 
                                         beam,
                                        &stack );

    return ret;
}

void
ri_bvh_clear_stat_traversal()
{
    memset( &g_stattrav, 0, sizeof(ri_bvh_stat_traversal_t));
}

static double
percentage( double val, double maxval )
{
    return 100.0 * (val / maxval);
}

void
ri_bvh_report_stat_traversal()
{
    double nrays = g_stattrav.nrays;

    double inner_node_travs_per_ray = g_stattrav.ninner_node_traversals / nrays;
    double leaf_node_travs_per_ray  = g_stattrav.nleaf_node_traversals / nrays;
    double tested_triangles_per_ray = g_stattrav.ntested_triangles / nrays;
    double actually_hit_triangles_per_ray = g_stattrav.nactually_hit_triangles / nrays;

    printf("== BVH traversal statistiscs ==================================================\n");
    printf("# of rays                    %lld\n", g_stattrav.nrays);
    printf("# of inner node travs        %lld\n", g_stattrav.ninner_node_traversals);
    printf("  Per ray                    %f\n", inner_node_travs_per_ray);
    printf("# of leaf node travs         %lld\n", g_stattrav.nleaf_node_traversals);
    printf("  Per ray                    %f\n", leaf_node_travs_per_ray);
    printf("# of tested triangles        %lld\n", g_stattrav.ntested_triangles);
    printf("  Per ray                    %f\n", tested_triangles_per_ray);
    printf("# of actually hit triangles  %lld\n", g_stattrav.nactually_hit_triangles);
    printf("  Per ray                    %f\n", actually_hit_triangles_per_ray);
    printf("  Hit rate                   %f %%\n", percentage(actually_hit_triangles_per_ray, tested_triangles_per_ray));


    printf("===============================================================================\n");

}


/* ---------------------------------------------------------------------------
 *
 * Private functions
 *
 * ------------------------------------------------------------------------ */

ri_qbvh_node_t *
ri_qbvh_node_new()
{
    ri_qbvh_node_t *node;

    node = (ri_qbvh_node_t *)ri_mem_alloc(sizeof(ri_qbvh_node_t));

    memset( node, 0, sizeof( ri_qbvh_node_t ));

    return node;
}

/*
 * TODO: SIMD optimzation.
 */
static inline int
triangle_isect(
    uint32_t            *tid_inout,
    ri_float_t          *t_inout,
    ri_float_t          *u_inout,
    ri_float_t          *v_inout,
    const ri_triangle_t *triangle,
    ri_vector_t          rayorg,
    ri_vector_t          raydir,
    uint32_t             tid)
{
    ri_vector_t v0, v1, v2; 
    ri_vector_t e1, e2; 
    ri_vector_t p, s, q;
    ri_float_t  a, inva;
    ri_float_t  t, u, v;
    double      eps = 1.0e-14;

    vcpy( v0, triangle->v[0] );
    vcpy( v1, triangle->v[1] );
    vcpy( v2, triangle->v[2] );

    vsub( e1, v1, v0 );
    vsub( e2, v2, v0 );

    vcross( p, raydir, e2 );

    a = vdot( e1, p );

    if (fabs(a) > eps) {
        inva = 1.0 / a;
    } else {
        return 0;
    }

    vsub( s, rayorg, v0 );
    vcross( q, s, e1 );

    u = vdot( s, p ) * inva;    
    v = vdot( q, raydir ) * inva;    
    t = vdot( e2, q ) * inva;    

    if ( (u < 0.0) || (u > 1.0)) {
        return 0;
    }

    if ( (v < 0.0) || ((u + v) > 1.0)) {
        return 0;
    }

    if ( (t < 0.0) || (t > (*t_inout)) ) {
        return 0;
    }

    (*t_inout)   = t;
    (*u_inout)   = u;
    (*v_inout)   = v;
    (*tid_inout) = tid;

    return 1;   /* hit */

}

int
bvh_intersect_leaf_node(
    ri_intersection_state_t *state_out,     /* [out]    */
    const ri_qbvh_node_t    *node,
    ri_ray_t                *ray )
{
    double         t, u, v;
    uint32_t       tid;

    int            hit;
    int            hitsum;
    uint32_t       i;
    uint32_t       ntriangles;
    ri_triangle_t *triangles;

    ri_vector_t rayorg; 
    ri_vector_t raydir; 

    /*
     * Init
     */
    t      = RI_INFINITY;
    u      = 0.0;
    v      = 0.0;
    tid    = 0;
    hit    = 0;
    hitsum = 0;
    vcpy( rayorg, ray->org );
    vcpy( raydir, ray->dir );

    triangles  = (ri_triangle_t *)node->child[0];
    ntriangles = *((uint32_t *)&node->bbox[0]);

#ifdef RI_BVH_ENABLE_DIAGNOSTICS
    if (gdiag) gdiag->ntriangle_isects++;
#endif
#ifdef RI_BVH_TRACE_STATISTICS
    g_stattrav.ntested_triangles += ntriangles;
#endif

    for (i = 0; i < ntriangles; i++) {

        hit = triangle_isect(
                    &tid, &t, &u, &v,
                    &triangles[i],
                    rayorg, raydir,
                    i);

#ifdef RI_BVH_TRACE_STATISTICS
        if (hit) {
            g_stattrav.nactually_hit_triangles++;
        }
#endif
       
        hitsum |= hit;
    }

    if (hitsum && (t < state_out->t)) {

        /* 
         * Update isect state
         */
        state_out->t     = t;
        state_out->u     = u;
        state_out->v     = v;
        state_out->geom  = triangles[tid].geom;
        state_out->index = triangles[tid].index;

    }

    return hitsum;
}

/*
 * Ray - AABB intersection test
 */
static inline int
test_ray_aabb(
    ri_float_t        *tmin_out,             /* [out]    */
    ri_float_t        *tmax_out,             /* [out]    */
    const ri_vector_t  bmin,
    const ri_vector_t  bmax,
    ri_ray_t          *ray)
{
    /* TODO: handle extreme case: NaN, Inf */

    double tmin, tmax;
    
    const double min_x = ray->dir_sign[0] ? bmax[0] : bmin[0];
    const double min_y = ray->dir_sign[1] ? bmax[1] : bmin[1];
    const double min_z = ray->dir_sign[2] ? bmax[2] : bmin[2];
    const double max_x = ray->dir_sign[0] ? bmin[0] : bmax[0];
    const double max_y = ray->dir_sign[1] ? bmin[1] : bmax[1];
    const double max_z = ray->dir_sign[2] ? bmin[2] : bmax[2];

    /*
     * X
     */
    const double tmin_x = (min_x - ray->org[0]) * ray->invdir[0];
    const double tmax_x = (max_x - ray->org[0]) * ray->invdir[0];

    /*
     * Y
     */
    const double tmin_y = (min_y - ray->org[1]) * ray->invdir[1];
    const double tmax_y = (max_y - ray->org[1]) * ray->invdir[1];

    /* Early exit: (tmin_x > tmax_y) || (tmin_y > tmax_x) => false  */
    // if ( (tmin_x > tmax_y) || (tmin_y > tmax_x) ) {
    //     return 0;
    // }

    tmin = (tmin_x > tmin_y) ? tmin_x : tmin_y;
    tmax = (tmax_x < tmax_y) ? tmax_x : tmax_y;

    /*
     * Z
     */
    const double tmin_z = (min_z - ray->org[2]) * ray->invdir[2];
    const double tmax_z = (max_z - ray->org[2]) * ray->invdir[2];

    /* Early exit: (tmin > tmax_z) || (tmin_z > tmax) => false  */
    // if ( (tmin > tmax_z) || (tmin_z > tmax) ) {
    //     return 0;
    // }

    tmin = (tmin > tmin_z) ? tmin : tmin_z;
    tmax = (tmax < tmax_z) ? tmax : tmax_z;

    /* (tmax > 0.0) && (tmin <= tmax) => hit
     *
     * Include tmin == tmax case(hit 2D plane).
     */
    if ( (tmax > 0.0) && (tmin <= tmax) ) {

        (*tmin_out) = tmin;
        (*tmax_out) = tmax;

        return 1;
    }

    return 0;   /* no hit */

}

static inline int
test_ray_node(
    int                  *order_out,      /* [out]    */
    ri_float_t            tmax,
    const ri_qbvh_node_t *node,
    ri_ray_t             *ray)
{
    ri_vector_t bmin_left  , bmax_left;
    ri_vector_t bmin_right , bmax_right;

    ri_float_t  tmin_left  = 0.0, tmax_left  = 0.0;
    ri_float_t  tmin_right = 0.0, tmax_right = 0.0;

    int         hit_left   , hit_right;

    int         retcode = 0;

    bmin_left[0]  = node->bbox[BMIN_X0];
    bmin_left[1]  = node->bbox[BMIN_Y0];
    bmin_left[2]  = node->bbox[BMIN_Z0];
    bmax_left[0]  = node->bbox[BMAX_X0];
    bmax_left[1]  = node->bbox[BMAX_Y0];
    bmax_left[2]  = node->bbox[BMAX_Z0];
    bmin_right[0] = node->bbox[BMIN_X1];
    bmin_right[1] = node->bbox[BMIN_Y1];
    bmin_right[2] = node->bbox[BMIN_Z1];
    bmax_right[0] = node->bbox[BMAX_X1];
    bmax_right[1] = node->bbox[BMAX_Y1];
    bmax_right[2] = node->bbox[BMAX_Z1];

#ifdef WITH_SSE

#define VSEL(a, b, mask) _mm_or_pd(_mm_and_pd(b, mask), _mm_andnot_pd(mask, a))

#if 0
    __m128d vbmin[3], vbmax[3];

    vbmin[0]   = _mm_loadu_pd(&node->bbox[BMIN_X0]); // 0, 1
    vbmin[1]   = _mm_loadu_pd(&node->bbox[BMIN_Y0]); // 0, 1
    vbmin[2]   = _mm_loadu_pd(&node->bbox[BMIN_Z0]); // 0, 1
    vbmax[0]   = _mm_loadu_pd(&node->bbox[BMAX_X0]); // 0, 1
    vbmax[1]   = _mm_loadu_pd(&node->bbox[BMAX_Y0]); // 0, 1
    vbmax[2]   = _mm_loadu_pd(&node->bbox[BMAX_Z0]); // 0, 1

    __m128d vminval[3], vmaxval[3];
    __m128d vraysign[3];

    vraysign[0] = _mm_set_pd1(ray->dir_signv[0]);
    vraysign[1] = _mm_set_pd1(ray->dir_signv[1]);
    vraysign[2] = _mm_set_pd1(ray->dir_signv[2]);

    vminval[0] = VSEL(vbmin[0], vbmax[0], vraysign[0]);
    vminval[1] = VSEL(vbmin[1], vbmax[1], vraysign[1]);
    vminval[2] = VSEL(vbmin[2], vbmax[2], vraysign[2]);

    vmaxval[0] = VSEL(vbmax[0], vbmin[0], vraysign[0]);
    vmaxval[1] = VSEL(vbmax[1], vbmin[1], vraysign[1]);
    vmaxval[2] = VSEL(vbmax[2], vbmin[2], vraysign[2]);

    __m128d vtmin[3], vtmax[3];
    __m128d vrayorg[3], vrayinvdir[3];

    vrayorg[0] = _mm_set_pd1(ray->org[0]);
    vrayorg[1] = _mm_set_pd1(ray->org[1]);
    vrayorg[2] = _mm_set_pd1(ray->org[2]);

    vrayinvdir[0] = _mm_set_pd1(ray->invdir[0]);
    vrayinvdir[1] = _mm_set_pd1(ray->invdir[1]);
    vrayinvdir[2] = _mm_set_pd1(ray->invdir[2]);

    vtmin[0] = _mm_mul_pd(_mm_sub_pd(vminval[0], vrayorg[0]), vrayinvdir[0]);
    vtmin[1] = _mm_mul_pd(_mm_sub_pd(vminval[1], vrayorg[1]), vrayinvdir[1]);
    vtmin[2] = _mm_mul_pd(_mm_sub_pd(vminval[2], vrayorg[2]), vrayinvdir[2]);

    vtmax[0] = _mm_mul_pd(_mm_sub_pd(vmaxval[0], vrayorg[0]), vrayinvdir[0]);
    vtmax[1] = _mm_mul_pd(_mm_sub_pd(vmaxval[1], vrayorg[1]), vrayinvdir[1]);
    vtmax[2] = _mm_mul_pd(_mm_sub_pd(vmaxval[2], vrayorg[2]), vrayinvdir[2]);

    __m128d vtmin_max = _mm_max_pd(vtmin[0], _mm_max_pd(vtmin[1], vtmin[2]));
    __m128d vtmax_min = _mm_min_pd(vtmax[0], _mm_min_pd(vtmax[1], vtmax[2]));

    // tmax_min > 0.0 & tmin_max <= tmax_min && tmin_max < tmax
    __m128d vmask = _mm_and_pd(
                        _mm_and_pd(
                            _mm_cmpgt_pd(vtmax_min, _mm_set_pd1(0.0)),
                            _mm_cmple_pd(vtmin_max, vtmax_min)),
                        _mm_cmplt_pd(vtmin_max, _mm_set_pd1(tmax)));
                        
    retcode = _mm_movemask_pd(vmask);   // 00, 01, 10 or 11

#endif

    hit_left = test_ray_aabb( &tmin_left, &tmax_left,
                               bmin_left,  bmax_left,
                               ray );

    hit_right = test_ray_aabb( &tmin_right, &tmax_right,
                                bmin_right,  bmax_right,
                                ray );

    if ( hit_left && (tmin_left < tmax) ) {
        retcode |= 1;
    }

    if ( hit_right && (tmin_right < tmax) ) {
        retcode |= 2;
    }

#else




#if defined(WITH_MUDA)

    ray_aabb_mu(&hit_left, &tmin_left, &tmax_left,
                ray->org, ray->dir_signv, ray->invdir, bmin_left, bmax_left);
    ray_aabb_mu(&hit_right, &tmin_right, &tmax_right,
                ray->org, ray->dir_signv, ray->invdir, bmin_right, bmax_right);

#else

    hit_left = test_ray_aabb( &tmin_left, &tmax_left,
                               bmin_left,  bmax_left,
                               ray );

    hit_right = test_ray_aabb( &tmin_right, &tmax_right,
                                bmin_right,  bmax_right,
                                ray );
#endif

    if ( hit_left && (tmin_left < tmax) ) {
        retcode |= 1;
    }

    if ( hit_right && (tmin_right < tmax) ) {
        retcode |= 2;
    }

#endif
    

    (*order_out) = ray->dir_sign[node->axis0];

    return retcode;
}

/*
 * BVH traversal routine.
 *
 * Returns:
 *
 *   1 if ray hits any occluder, 0 if no hit.
 */
int
bvh_traverse(
    ri_intersection_state_t *state_out,     /* [out]        */
    const ri_qbvh_node_t    *root,
    ri_bvh_diag_t           *diag,          /* [modified]   */
    ri_ray_t                *ray,
    bvh_stack_t             *stack )        /* [buffer]     */
{
    
    const ri_qbvh_node_t *node;
    int                   ret;
    int                   order;            /* traversal order  */

    assert( root != NULL );

#ifdef RI_BVH_ENABLE_DIAGNOSTICS
    gdiag = diag;
#endif

    /*
     * Initialize intersection state.
     */ 
    state_out->t     = RI_INFINITY;
    state_out->u     = 0.0;
    state_out->v     = 0.0;
    state_out->geom  = NULL;
    state_out->index = 0;

    node = root;

    while (1) {

        if ( node->is_leaf ) {

#ifdef RI_BVH_ENABLE_DIAGNOSTICS
            if (gdiag) gdiag->nleaf_node_traversals++;
#endif
#ifdef RI_BVH_TRACE_STATISTICS
            g_stattrav.nleaf_node_traversals++;
#endif

            bvh_intersect_leaf_node( state_out,
                                     node,
                                     ray );

            /* pop */
            if (stack->depth < 1) goto end_traverse;
            stack->depth--;
            assert(stack->depth >= 0);
            node = stack->nodestack[stack->depth];

        } else {

#ifdef RI_BVH_ENABLE_DIAGNOSTICS
            if (gdiag) gdiag->ninner_node_traversals++;
#endif

#ifdef RI_BVH_TRACE_STATISTICS
            g_stattrav.ninner_node_traversals++;
#endif

            ret = test_ray_node( &order, state_out->t, node, ray );

            if (ret == 0) {

                /* pop */
                if (stack->depth < 1) goto end_traverse;
                stack->depth--;
                assert(stack->depth >= 0);
                node = stack->nodestack[stack->depth];

            } else if (ret == 1) {

                node = node->child[0];

            } else if (ret == 2) {

                node = node->child[1];

            } else {    /* traverse both nodes */

                /* push */
                stack->nodestack[stack->depth] = node->child[1 - order];
                stack->depth++;
                assert( stack->depth < BVH_MAXDEPTH );
                node = node->child[order];
            
            }

        }

    }
    
end_traverse:

    return (state_out->t < RI_INFINITY);
}

static inline ri_float_t
calc_surface_area(
    ri_vector_t bmin,
    ri_vector_t bmax)
{
    ri_float_t sa;

    assert( bmax[0] >= bmin[0] );
    assert( bmax[1] >= bmin[1] );
    assert( bmax[2] >= bmin[2] );

    sa = (bmax[0] - bmin[0]) * (bmax[1] - bmin[1]) +
         (bmax[1] - bmin[1]) * (bmax[2] - bmin[2]) +
         (bmax[2] - bmin[2]) * (bmax[0] - bmin[0]);

    sa *= 2.0;

    return sa;
}

static inline ri_float_t
SAH(
    int        ns1,
    ri_float_t left_area,
    int        ns2,
    ri_float_t right_area,
    ri_float_t s)
{
    const float Taabb = 0.2f;
    const float Ttri  = 0.8f;

    float T;

    T = 2.0f * Taabb
      + (left_area / s) * (ri_float_t)ns1 * Ttri
      + (right_area / s) * (ri_float_t)ns2 * Ttri; 

    return T;
}

int 
find_cut_from_bin(
    ri_float_t             *cut_pos_out,        /* [out]    */
    int                    *cut_axis_out,       /* [out]    */ 
    const bvh_bin_buffer_t *binbuf,
    ri_vector_t             bmin,
    ri_vector_t             bmax,
    uint64_t                ntriangles)
{
    int         i, j;

    ri_float_t  cost;
    int         min_cost_axis = 0;
    ri_float_t  min_cost_pos  = 0.0;
    ri_float_t  min_cost      = RI_INFINITY;
        
    uint64_t    tris_left; 
    uint64_t    tris_right;

    ri_vector_t bsize;
    ri_vector_t bstep;

    ri_vector_t bmin_left, bmax_left;
    ri_vector_t bmin_right, bmax_right;
    ri_float_t  sa_left, sa_right, sa_total;

    ri_float_t  pos;

    bsize[0]    = bmax[0] - bmin[0];
    bsize[1]    = bmax[1] - bmin[1];
    bsize[2]    = bmax[2] - bmin[2];

    bstep[0] = bsize[0] / (ri_float_t)BVH_BIN_SIZE;
    bstep[1] = bsize[1] / (ri_float_t)BVH_BIN_SIZE;
    bstep[2] = bsize[2] / (ri_float_t)BVH_BIN_SIZE;

    sa_total    = calc_surface_area( bmin, bmax );

    for (j = 0; j < 3; j++) {   /* axis */

        /*
         *  Compute SAH cost for right side of each cell of the bbox.
         *  Exclude both extreme side of the bbox.
         *     
         *  i:      0    1    2    3    
         *     +----+----+----+----+----+
         *     |    |    |    |    |    |
         *     +----+----+----+----+----+
         *
         */

        tris_left  = 0; 
        tris_right = ntriangles;

        vcpy( bmin_left, bmin ); vcpy( bmin_right, bmin );
        vcpy( bmax_left, bmax ); vcpy( bmax_right, bmax );

        for (i = 0; i < BVH_BIN_SIZE - 1; i++) {

            tris_left  += binbuf->bin[0][j][i];
            tris_right -= binbuf->bin[1][j][i];

            assert(tris_left  <=  ntriangles);
            assert(tris_right <=  ntriangles);

            /*
             * split pos = bmin + (i + 1) * (bsize / BIN_SIZE)
             * +1 for i since we want a position on right side of the cell.
             */
            

            pos = bmin[j] + (i + 1) * bstep[j];

            bmax_left[j]  = pos;
            bmin_right[j] = pos;

            sa_left  = calc_surface_area( bmin_left, bmax_left );
            sa_right = calc_surface_area( bmin_right, bmax_right );

            cost = SAH( tris_left, sa_left, tris_right, sa_right, sa_total );
            
            if (cost < min_cost) {  /* update mincost   */
                min_cost      = cost;
                min_cost_axis = j;
                min_cost_pos  = pos; 
            }
        }

    }

    (*cut_axis_out) = min_cost_axis;
    (*cut_pos_out)  = min_cost_pos;


    return 0;   /* OK */
   
}

int
bvh_construct(
    ri_qbvh_node_t *root,
    ri_vector_t     bmin,
    ri_vector_t     bmax,
    ri_triangle_t  *triangles,
    ri_triangle_t  *triangles_buf,
    tri_bbox_t     *tri_bboxes,
    tri_bbox_t     *tri_bboxes_buf,
    uint64_t        index_left,             /* [index_left, index_right)    */
    uint64_t        index_right)
{

    uint64_t n;
    uint32_t n32;

    n = index_right - index_left;

    ri_float_t cut_pos;
    int        cut_axis;

    /*
     * 1. If # of triangles are less than threshold, make a leaf node.
     */
    if (n <= BVH_NTRIS_LEAF) {

        /*
         * trianble bbox list is sorted at (3.), but triangle data itself is
         * not.
         * Defer sorting triangle data until making a leaf.
         *
         * 'triangles' contains sorted list of triangles.
         * 'triangles_buf' contains unmodifiled list of triangles.
         * if the tri_bbox has index 'i', we can pick up triangle data
         * the tri_bbox points to by expressing triangles_buf[i].
         */

        gather_triangles(
            triangles + index_left,
            triangles_buf,                          /* No offset    */
            tri_bboxes + index_left,
            n);

        /*
         * After gather_triangles() was called,
         * triangles[index_left, index_right) contains sorted list of
         * triangles.
         */

        /*
         * bbox[0]    -> # of triangles.
         * child[0]   -> ptr to 3D triangles.
         * child[1:3] -> ptr to 2D triangles.
         */
        
        assert( n < 0x100000000ULL );
        n32 = (uint32_t)n;

        uint32_t *ptr;

        ptr = (uint32_t *)&root->bbox[0];

        (*ptr)         = n32;       /* root->bbox[0] = n32  */
        root->child[0] = (ri_qbvh_node_t *)(triangles + index_left);

        /*
         * 2D triangles are generated on demand during beam traing.
         */
        root->child[1] = NULL;
        root->child[2] = NULL;
        root->child[3] = NULL;

        root->is_leaf = 1;

        return 0;
    }


    /*
     * 2. Bin edges of triangles.
     */
    {
        bin_triangle_edge(
            &g_binbuf,
            bmin,
            bmax,
            tri_bboxes + index_left, 
            n);
             

        find_cut_from_bin(
            &cut_pos,
            &cut_axis,
            &g_binbuf,
             bmin,
             bmax,
             n);
    }

    uint64_t i;
    uint64_t ntris_left  = 0;
    uint64_t ntris_right = n - 1;

    /*
     * 3. Partition triangles into left and right.
     * 
     * bbox data is read from tri_bboxes_buf, then left-right separated
     * bbox data is wrote to tri_bboxes.
     */
    {
        memcpy(tri_bboxes_buf, tri_bboxes + index_left, sizeof(tri_bbox_t) * n);

        for (i = 0; i < n; i++) {

            if (tri_bboxes_buf[i].bmax[cut_axis] < cut_pos) {

                /* left   */

                assert(ntris_left < n);

                memcpy(tri_bboxes + index_left + ntris_left,
                       tri_bboxes_buf + i,
                       sizeof(tri_bbox_t));

                ntris_left++;
        
            } else {

                /* right    */

                assert(ntris_right < n);

                memcpy(tri_bboxes + index_left + ntris_right,
                       tri_bboxes_buf + i,
                       sizeof(tri_bbox_t));

                ntris_right--;

            }

        }


        if( ntris_left == 0 || ntris_left == n ) {

            /* Couldn't partition bboxes into left and right.
             * Force split at object median.
             */
            ntris_left = n / 2;
            
        }

    }


    /*
     * 4. Subdivide.
     */
    {
        ri_vector_t     bmin_left ,  bmax_left;
        ri_vector_t     bmin_right,  bmax_right;
        ri_qbvh_node_t *node_left , *node_right;

        node_left      = ri_qbvh_node_new();
        root->child[0] = node_left;

        node_right     = ri_qbvh_node_new();
        root->child[1] = node_right;

        root->axis0    = cut_axis;

        /*
         * left
         */
        calc_bbox_of_triangles(
            bmin_left,
            bmax_left, 
            tri_bboxes + index_left,
            ntris_left);

        /*
         * Slightly extend bbox to avoid numeric error.
         */
        bbox_add_margin( bmin_left, bmax_left );

        root->bbox[BMIN_X0] = bmin_left[0];
        root->bbox[BMIN_Y0] = bmin_left[1];
        root->bbox[BMIN_Z0] = bmin_left[2];
        root->bbox[BMAX_X0] = bmax_left[0];
        root->bbox[BMAX_Y0] = bmax_left[1];
        root->bbox[BMAX_Z0] = bmax_left[2];

        bvh_construct( 
            node_left,
            bmin_left,
            bmax_left,
            triangles,
            triangles_buf,
            tri_bboxes,
            tri_bboxes_buf,
            index_left,
            index_left + ntris_left);


        /*
         * right
         */
        calc_bbox_of_triangles(
            bmin_right,
            bmax_right, 
            tri_bboxes + index_left + ntris_left,
            n - ntris_left);

        bbox_add_margin( bmin_right, bmax_right );

        root->bbox[BMIN_X1] = bmin_right[0];
        root->bbox[BMIN_Y1] = bmin_right[1];
        root->bbox[BMIN_Z1] = bmin_right[2];
        root->bbox[BMAX_X1] = bmax_right[0];
        root->bbox[BMAX_Y1] = bmax_right[1];
        root->bbox[BMAX_Z1] = bmax_right[2];

        bvh_construct( 
            node_right,
            bmin_right,
            bmax_right,
            triangles,
            triangles_buf,
            tri_bboxes,
            tri_bboxes_buf,
            index_left + ntris_left,
            index_right);

    }

    return 0;   /* OK */
}



/*
 * Record the edge of triangle into the bin buffer.
 */
int 
bin_triangle_edge(
    bvh_bin_buffer_t  *binbuf,          /* [inout] */
    const ri_vector_t  scene_bmin,
    const ri_vector_t  scene_bmax,
    const tri_bbox_t  *tri_bboxes,
    uint64_t           ntriangles)
{
    uint64_t i;

    double      eps = 1.0e-14;

    ri_vector_t bmin;
    ri_vector_t bmax;

    ri_vector_t quantized_bmin;
    ri_vector_t quantized_bmax;

    ri_float_t binsize = (ri_float_t)BVH_BIN_SIZE;

    /*
     * calculate extent
     */
    ri_vector_t scene_size;
    ri_vector_t scene_invsize;

    {

        scene_size[0] = scene_bmax[0] - scene_bmin[0];
        scene_size[1] = scene_bmax[1] - scene_bmin[1];
        scene_size[2] = scene_bmax[2] - scene_bmin[2];


        /* [0, BIN_SIZE) */
        assert(scene_size[0] >= 0.0);
        assert(scene_size[1] >= 0.0);
        assert(scene_size[2] >= 0.0);
        if (scene_size[0] > eps) {
            scene_invsize[0] = binsize / scene_size[0];
        } else {
            scene_invsize[0] = 0.0;
        }
        
        if (scene_size[1] > eps) {
            scene_invsize[1] = binsize / scene_size[1];
        } else {
            scene_invsize[1] = 0.0;
        }
        
        if (scene_size[2] > eps) {
            scene_invsize[2] = binsize / scene_size[2];
        } else {
            scene_invsize[2] = 0.0;
        }

    }

    /* clear bin data */
    memset(binbuf, 0, sizeof(bvh_bin_buffer_t));


    uint32_t idx_bmin[3];
    uint32_t idx_bmax[3];

    for (i = 0; i < ntriangles; i++) {

        /*
         * Quantize the position into [0, BIN_SIZE)
         *
         *  q[i] = (int)(p[i] - scene_bmin) / scene_size
         */

        vcpy(bmin, tri_bboxes[i].bmin);
        vcpy(bmax, tri_bboxes[i].bmax);


        quantized_bmin[0] = (bmin[0] - scene_bmin[0]) * scene_invsize[0];
        quantized_bmin[1] = (bmin[1] - scene_bmin[1]) * scene_invsize[1];
        quantized_bmin[2] = (bmin[2] - scene_bmin[2]) * scene_invsize[2];

        quantized_bmax[0] = (bmax[0] - scene_bmin[0]) * scene_invsize[0];
        quantized_bmax[1] = (bmax[1] - scene_bmin[1]) * scene_invsize[1];
        quantized_bmax[2] = (bmax[2] - scene_bmin[2]) * scene_invsize[2];

        /* idx is now in [0, BIN_SIZE) */
        idx_bmin[0] = (uint32_t)(quantized_bmin[0]);
        idx_bmin[1] = (uint32_t)(quantized_bmin[1]);
        idx_bmin[2] = (uint32_t)(quantized_bmin[2]);

        idx_bmax[0] = (uint32_t)(quantized_bmax[0]);
        idx_bmax[1] = (uint32_t)(quantized_bmax[1]);
        idx_bmax[2] = (uint32_t)(quantized_bmax[2]);

        if (idx_bmin[0] >= BVH_BIN_SIZE) idx_bmin[0] = BVH_BIN_SIZE - 1;
        if (idx_bmin[1] >= BVH_BIN_SIZE) idx_bmin[1] = BVH_BIN_SIZE - 1;
        if (idx_bmin[2] >= BVH_BIN_SIZE) idx_bmin[2] = BVH_BIN_SIZE - 1;
        if (idx_bmax[0] >= BVH_BIN_SIZE) idx_bmax[0] = BVH_BIN_SIZE - 1;
        if (idx_bmax[1] >= BVH_BIN_SIZE) idx_bmax[1] = BVH_BIN_SIZE - 1;
        if (idx_bmax[2] >= BVH_BIN_SIZE) idx_bmax[2] = BVH_BIN_SIZE - 1;
            
        assert(idx_bmin[0] < BVH_BIN_SIZE);
        assert(idx_bmin[1] < BVH_BIN_SIZE);
        assert(idx_bmin[2] < BVH_BIN_SIZE);

        assert(idx_bmax[0] < BVH_BIN_SIZE);
        assert(idx_bmax[1] < BVH_BIN_SIZE);
        assert(idx_bmax[2] < BVH_BIN_SIZE);

        /* Increment bin counter */
        binbuf->bin[0][0][idx_bmin[0]]++;
        binbuf->bin[0][1][idx_bmin[1]]++;
        binbuf->bin[0][2][idx_bmin[2]]++;

        binbuf->bin[1][0][idx_bmax[0]]++;
        binbuf->bin[1][1][idx_bmax[1]]++;
        binbuf->bin[1][2][idx_bmax[2]]++;

    }

    return 0;   /* OK */

}

/*
 * Add margin for bbox to avoid numeric problem.
 */
static void
bbox_add_margin(
    ri_vector_t        bmin,                /* [inout]  */
    ri_vector_t        bmax)                /* [inout]  */
{
    int i;

    ri_float_t scale[3];
    ri_float_t scene_scale[3];

    ri_float_t eps = RI_EPS;

    /*
     * Choose scale factor according to bbox's size.
     */
    for (i = 0; i < 3; i++) {
        scale[i] = bmax[i] - bmin[i];
        assert(scale[i] >= 0.0);

        if (scale[i] < eps) {
            scene_scale[i] = eps;
        } else {
            scene_scale[i] = eps * scale[i];
        }
    }
    

    bmin[0] -= scene_scale[0];
    bmin[1] -= scene_scale[1];
    bmin[2] -= scene_scale[2];
    bmax[0] += scene_scale[0];
    bmax[1] += scene_scale[1];
    bmax[2] += scene_scale[2];

}

/*
 * Create an array of triangles and its bbox from the list of geometory.
 */
void
create_triangle_list(
    ri_triangle_t    **triangles_out,       /* [out] */
    tri_bbox_t       **tri_bboxes_out,      /* [out] */
    uint64_t          *ntriangles,          /* [out] */
    const ri_list_t   *geom_list)
{
    uint32_t     i;
    ri_vector_t  v[3];
    ri_list_t   *itr;
    ri_geom_t   *geom;

    uint64_t     n = 0;
    uint64_t     idx;

    assert(geom_list != NULL);

    /*
     * find # of triangles in the geometry list.
     */

    for (itr  = ri_list_first( (ri_list_t *)geom_list );
         itr != NULL;
         itr  = ri_list_next( itr ) ) {

        geom = ( ri_geom_t * )itr->data;

        assert(geom != NULL);

        n += geom->nindices / 3;

    }

    if (n == 0) {

        /* Empty scene */

        (*triangles_out)  = NULL;
        (*tri_bboxes_out) = NULL;
        (*ntriangles)     = 0;

        return;
    }


    (*triangles_out)  = ri_mem_alloc(sizeof(ri_triangle_t) * n);
    (*tri_bboxes_out) = ri_mem_alloc(sizeof(tri_bbox_t) * n);
    (*ntriangles)     = n;


    /*
     * Construct array of triangles and array of bbox of triangles.
     */

    ri_vector_t bmin, bmax;

    idx = 0;
    vzero( bmin );
    vzero( bmax );

    for (itr  = ri_list_first( (ri_list_t *)geom_list );
         itr != NULL;
         itr  = ri_list_next( itr ) ) {

        geom = ( ri_geom_t * )itr->data;

        for (i = 0; i < geom->nindices / 3; i++) {

            vcpy(v[0], geom->positions[geom->indices[3 * i + 0]]);    
            vcpy(v[1], geom->positions[geom->indices[3 * i + 1]]);    
            vcpy(v[2], geom->positions[geom->indices[3 * i + 2]]);    

            vcpy( (*triangles_out)[idx].v[0], v[0] );
            vcpy( (*triangles_out)[idx].v[1], v[1] );
            vcpy( (*triangles_out)[idx].v[2], v[2] );

            (*triangles_out)[idx].geom  = geom;
            (*triangles_out)[idx].index = 3 * i;

            get_bbox_of_triangle( bmin, bmax, &((*triangles_out)[idx]) );

            vcpy( (*tri_bboxes_out)[idx].bmin, bmin );
            vcpy( (*tri_bboxes_out)[idx].bmax, bmax );
            (*tri_bboxes_out)[idx].index = idx;

            idx++;

        }

    }
}


static void
calc_scene_bbox(
    ri_vector_t        bmin_out,        /* [out] */
    ri_vector_t        bmax_out,        /* [out] */
    const tri_bbox_t  *tri_bboxes,
    uint64_t           ntriangles)
{
    uint64_t i;

    assert(tri_bboxes != NULL);

    vcpy( bmin_out, tri_bboxes[0].bmin );
    vcpy( bmax_out, tri_bboxes[0].bmax );

    for (i = 1; i < ntriangles; i++) {

        vmin( bmin_out, bmin_out, tri_bboxes[i].bmin );
        vmax( bmax_out, bmax_out, tri_bboxes[i].bmax );

    }
}


static void
get_bbox_of_triangle(
    ri_vector_t           bmin_out,         /* [out] */  
    ri_vector_t           bmax_out,         /* [out] */  
    const ri_triangle_t  *triangle)
{

    vcpy( bmin_out, triangle->v[0] );
    vcpy( bmax_out, triangle->v[0] );

    vmin( bmin_out, bmin_out, triangle->v[1] );
    vmin( bmin_out, bmin_out, triangle->v[2] );

    vmax( bmax_out, bmax_out, triangle->v[1] );
    vmax( bmax_out, bmax_out, triangle->v[2] );

}

static void
calc_bbox_of_triangles(
    ri_vector_t        bmin_out,         /* [out] */  
    ri_vector_t        bmax_out,         /* [out] */  
    const tri_bbox_t  *tri_bboxes,
    uint64_t           ntriangles)
{
    uint64_t i;

    ri_vector_t bmin;
    ri_vector_t bmax;

    vcpy( bmin, tri_bboxes[0].bmin );
    vcpy( bmax, tri_bboxes[0].bmax );

    for (i = 1; i < ntriangles; i++) {

        vmin( bmin, bmin, tri_bboxes[i].bmin );
        vmax( bmax, bmax, tri_bboxes[i].bmax );

    }

    vcpy( bmin_out, bmin );
    vcpy( bmax_out, bmax );

}

int
gather_triangles(
    ri_triangle_t       *triangles_to_out,    /* [out]    */
    const ri_triangle_t *triangles_from,
    const tri_bbox_t    *tri_bboxes,
    uint64_t             ntriangles)
{
    uint64_t i;
    uint64_t j;

    for (i = 0; i < ntriangles; i++) {

        j = tri_bboxes[i].index;

        memcpy( triangles_to_out + i, triangles_from + j, sizeof(ri_triangle_t));
        

    }

    return 0;   /* OK */
}

#if 0   /* Future */
static inline void
get_bbox_of_triangle4(
    ri_vector_t        bminx_out,        /* [out] */  
    ri_vector_t        bminy_out,        /* [out] */  
    ri_vector_t        bminz_out,        /* [out] */  
    ri_vector_t        bmaxx_out,        /* [out] */  
    ri_vector_t        bmaxy_out,        /* [out] */  
    ri_vector_t        bmaxz_out,        /* [out] */  
    const triangle4_t *triangles)
{
    vec bminx, bminy, bminz;
    vec bmaxx, bmaxy, bmaxz;

    vec v0x; vcpy(v0x, triangles->v0x);
    vec v1x; vcpy(v1x, triangles->v1x);
    vec v2x; vcpy(v2x, triangles->v2x);

    vec v0y; vcpy(v0y, triangles->v0y);
    vec v1y; vcpy(v1y, triangles->v1y);
    vec v2y; vcpy(v2y, triangles->v2y);

    vec v0z; vcpy(v0z, triangles->v0z);
    vec v1z; vcpy(v1z, triangles->v1z);
    vec v2z; vcpy(v2z, triangles->v2z);

    vcpy(bminx, v0x);
    vcpy(bminy, v0y);
    vcpy(bminz, v0z);

    vcpy(bmaxx, v0x);
    vcpy(bmaxy, v0y);
    vcpy(bmaxz, v0z);

    vmin(bminx, bminx, v1x);
    vmin(bminy, bminy, v1y);
    vmin(bminz, bminz, v1z);

    vmin(bminx, bminx, v2x);
    vmin(bminy, bminy, v2y);
    vmin(bminz, bminz, v2z);

    vmax(bmaxx, bmaxx, v1x);
    vmax(bmaxy, bmaxy, v1y);
    vmax(bmaxz, bmaxz, v1z);

    vmax(bmaxx, bmaxx, v2x);
    vmax(bmaxy, bmaxy, v2y);
    vmax(bmaxz, bmaxz, v2z);

    vcpy(bminx_out, bminx);
    vcpy(bminy_out, bminy);
    vcpy(bminz_out, bminz);

    vcpy(bmaxx_out, bmaxx);
    vcpy(bmaxy_out, bmaxy);
    vcpy(bmaxz_out, bmaxz);

}
#endif

/*
 * Find n-vertex relative to the plane normal
 *
 * n-vertex : nearest vertex to the plane defined by `normal'
 *
 *                       
 *   \                     
 *    \    ^              +------+
 *     \  / normal        |      |
 *      \/                |      |
 *       \                o------+
 *        \              n-vertex
 *         \
 *
 * Reference:
 *   - http://www.cescg.org/CESCG-2002/DSykoraJJelinek/index.html
 */
static void
get_n_point(
    ri_vector_t np,
    ri_vector_t bmin,
    ri_vector_t bmax,
    ri_vector_t plane)
{
    np[0] = (plane[0] > 0.0) ? bmin[0] : bmax[0]; 
    np[1] = (plane[1] > 0.0) ? bmin[1] : bmax[1]; 
    np[2] = (plane[2] > 0.0) ? bmin[2] : bmax[2]; 
} 

/*
 * 1 if beam misses bbox, 0 if not.
 */
static int
test_beam_aabb_misses(
          ri_vector_t  bmin,
          ri_vector_t  bmax,
    const ri_beam_t   *beam)
{
    ri_vector_t np;
    ri_vector_t no;
    ri_float_t  d;

    ri_vector_t n;
    
    int i;

    for (i = 0; i < 4; i++) {           /* for each frustum plane   */
        
        vcpy( n, beam->normal[i] ); 

        get_n_point( np, bmin, bmax, n );

        /*
         * d = (np - org) . normal
         *
         * If any of d is greater than 0.0, bbox is completely
         * outside of the beam frustum.
         */
        vsub( no, np, beam->org );
        d = vdot( no, n );

        if (d > 0.0) return 1;

    }
    
    return 0;
    
}

/*
 * 0 if no hit.
 * 1 if potentially hit.
 */
static int
test_beam_aabb(
          ri_vector_t  bmin,
          ri_vector_t  bmax,
    const ri_beam_t   *beam)
{

    int axis;
    int ret;

    axis = beam->dominant_axis;

#if 0
    /*
     * Cull by t.
     * If beam's maximum hit distance t_max is less than the min of bbox,
     * beam does not hit the bbox.
     */
    if ( beam->t_max < (bmin[axis] - beam->org[axis]) ) {
        return 0;
    }
#endif

    /*
     * 2. Check if beam compiletely misses the bbox.
     */

    ret = test_beam_aabb_misses( bmin, bmax, beam );

    if (ret) return 0;

    /*
     * Beam may hit the bbox.
     */

    return 1;
}

/*
 * 0 - no hit.
 * 1 - hit left node
 * 2 - hit right node
 * 3 - hit both
 */
int
test_beam_node(
    const ri_qbvh_node_t *node,
    const ri_beam_t      *beam)
{
    ri_vector_t bmin_left , bmax_left;
    ri_vector_t bmin_right, bmax_right;

    bmin_left[0]  = node->bbox[ BMIN_X0 ];
    bmin_left[1]  = node->bbox[ BMIN_Y0 ];
    bmin_left[2]  = node->bbox[ BMIN_Z0 ];
    bmax_left[0]  = node->bbox[ BMAX_X0 ];
    bmax_left[1]  = node->bbox[ BMAX_Y0 ];
    bmax_left[2]  = node->bbox[ BMAX_Z0 ];

    bmin_right[0] = node->bbox[ BMIN_X1 ];
    bmin_right[1] = node->bbox[ BMIN_Y1 ];
    bmin_right[2] = node->bbox[ BMIN_Z1 ];
    bmax_right[0] = node->bbox[ BMAX_X1 ];
    bmax_right[1] = node->bbox[ BMAX_Y1 ];
    bmax_right[2] = node->bbox[ BMAX_Z1 ];
    
    int hit_left, hit_right;

    hit_left  = test_beam_aabb( bmin_left , bmax_left , beam );
    hit_right = test_beam_aabb( bmin_right, bmax_right, beam );

    return hit_left | (hit_right << 1);

}

/*
 * Do ray - triangle intersection for each corner ray of beam.
 * There are 3 cases of result.
 *
 * - Beam completely misses the triangle. 
 * - Beam completely hits the triangle(inside of triangle). 
 * - Beam partially hits the triangle(needs beam splitting)
 *
 * u_out, v_out and t_out will be filled only when beam completely hits
 * the triangle.
 */
int
test_beam_triangle(
          ri_vector_t     u_out,       /* [out]    */
          ri_vector_t     v_out,       /* [out]    */
          ri_vector_t     t_out,       /* [out]    */
    const ri_triangle_t  *triangle,
    const ri_beam_t      *beam)
{

    int         i;
    int         mask;
    int         cnt;
    ri_float_t  u[4], v[4], t[4];

    ri_vector_t v0, v1, v2;
    ri_vector_t e1, e2;
    ri_vector_t p, q, s; 
    ri_float_t  a, inva;

    vcpy( v0, triangle->v[0] );
    vcpy( v1, triangle->v[1] );
    vcpy( v2, triangle->v[2] );

    vsub( e1, v1, v0 );
    vsub( e2, v2, v0 );

    mask = 0;

    for (i = 0; i < 4; i++) {

        vcross( p, beam->dir[i], e2 );

        a = vdot( e1, p );

        if (fabs(a) > RI_EPS) {
            inva = 1.0 / a;
        } else {
            inva = 0.0;
        }

        vsub( s, beam->org, v0 );
        vcross( q, s, e1 );

        u[i] = vdot( s, p ) * inva;    
        v[i] = vdot( q, beam->dir[i] ) * inva;    
        t[i] = vdot( e2, q ) * inva;    

        if ( (u[i] < 0.0) || (u[i] > 1.0)) {
            continue;
        }

        if ( (v[i] < 0.0) || ((u[i] + v[i]) > 1.0)) {
            continue;
        }

        if ( (t[i] < 0.0) || (t[i] > beam->t_max) ) {
            continue;
        }

        mask |= (1 << i);
    }

    if (mask == 0) {

        /* 
         * Check the case that all ray misses the triangle, but the beam
         * contains the triangle.
         * TODO: SIMDize.
         */

        /* Firstly, check T */
        cnt = 0;
        for (i = 0; i < 4; i++) {
            if (t[i] < 0.0) cnt++;
        }

        if (cnt == 4) {
            return RI_BEAM_MISS_COMPLETELY;
        }
        

        /* Check U */
        cnt = 0;
        for (i = 0; i < 4; i++) {
            if (u[i] < 0.0) cnt++;
        }

        if ( (cnt != 0) && (cnt != 4) ) {
            return RI_BEAM_HIT_PARTIALLY;
        }

        cnt = 0;
        for (i = 0; i < 4; i++) {
            if (u[i] > 1.0) cnt++;
        }

        if ( (cnt != 0) && (cnt != 4) ) {
            return RI_BEAM_HIT_PARTIALLY;
        }

        /* Check V */
        cnt = 0;
        for (i = 0; i < 4; i++) {
            if (v[i] < 0.0) cnt++;
        }

        if ( (cnt != 0) && (cnt != 4) ) {
            return RI_BEAM_HIT_PARTIALLY;
        }

        /* Check U + V */
        cnt = 0;
        for (i = 0; i < 4; i++) {
            if ((u[i] + v[i]) >= 1.0) cnt++;
        }

        if ( (cnt != 0) && (cnt != 4) ) {
            return RI_BEAM_HIT_PARTIALLY;
        }
            

        return RI_BEAM_MISS_COMPLETELY;
        //return RI_BEAM_HIT_COMPLETELY;

    } else if (mask == 0xf) {

        /* Beam completely hits the triangle. */
        for (i = 0; i < 4; i++) {
            u_out[i] = u[i];
            v_out[i] = v[i];
            t_out[i] = t[i];
        }

        return RI_BEAM_HIT_COMPLETELY;

    } else {

        /* Needs beam splitting   */
        return RI_BEAM_HIT_PARTIALLY;

    }

}

#if 0
int
test_triangle2d_beam2d_cull_x(
    triangle2d_t *triangle,
    ri_vector_t   normal[4],
    ri_vector_t   org[4])
{
    int        i;
    int        axis0 = 1;
    int        axis1 = 2;
    ri_float_t d[3];

    for (i = 0; i < 4; i++){

        d[0] = (triangle->v0u - org[i][axis0]) * normal[i][axis0]
             + (triangle->v0v - org[i][axis1]) * normal[i][axis1];
        d[1] = (triangle->v1u - org[i][axis0]) * normal[i][axis0]
             + (triangle->v1v - org[i][axis1]) * normal[i][axis1];
        d[2] = (triangle->v2u - org[i][axis0]) * normal[i][axis0]
             + (triangle->v2v - org[i][axis1]) * normal[i][axis1];

        if ( (d[0] > 0.0) || (d[1] > 0.0) || (d[2] > 0.0) ) {

            return 0;   /* culled   */

        }

    }

}
#endif

int
bvh_intersect_leaf_node_beam(
    ri_raster_plane_t       *plane_inout,
    ri_qbvh_node_t          *node,
    ri_beam_t               *beam )
{
    uint32_t         tid;
    uint32_t         i;
    uint32_t         ntriangles;
    ri_triangle_t   *triangles;
    ri_triangle2d_t *triangle2ds;

    (void)plane_inout;

    /*
     * Init
     */
    tid    = 0;

    ntriangles = *((uint32_t *)&node->bbox[0]);
    triangles  = (ri_triangle_t *)node->child[0];

    /*
     * Firstly, check if it is the first time visiting to this node.
     * This is determined whether child[1:3] is NULL or not.
     * If NULL, it is the first time, so we create list of 2d projected
     * triangles.
     */
    triangle2ds = (ri_triangle2d_t *)node->child[beam->dominant_axis + 1];
    if (triangle2ds == NULL) {

        printf("proj: %p, n = %d\n", &node->child[beam->dominant_axis + 1], ntriangles);

        triangle2ds = ri_mem_alloc(sizeof(ri_triangle2d_t) * ntriangles);
        
        project_triangles( triangle2ds,
                           triangles,
                           ntriangles,
                           beam->dominant_axis,
                           beam->d,
                           beam->org );

        /*
         * TODO: sort triangles in its projectd size.
         */

        node->child[beam->dominant_axis + 1] = (ri_qbvh_node_t *)triangle2ds;

        printf("node set: %p\n", node->child[beam->dominant_axis + 1]);
    }

#ifdef RI_BVH_ENABLE_DIAGNOSTICS
    if (gdiag) gdiag->ntriangle_isects++;
#endif

#ifdef RI_BVH_TRACE_STATISTICS
    g_stattrav.ntested_triangles += ntriangles;
#endif

    {
        int         j;
        int         nhit_beams;
        int         nmiss_beams;
        int         nouter_beams;
        int         ninner_beams;
        ri_beam_t   outer_beams[8];
        ri_beam_t   inner_beams[8];
        ri_beam_t   hit_beams[8];

        for (i = 0; i < ntriangles; i++) {

            ri_beam_clip_by_triangle2d(
                &hit_beams[0], &g_miss_beams[0],
                &nhit_beams, &nmiss_beams,
                &triangle2ds[i], beam);

            printf("hit beams = %d, miss beams = %d\n", nhit_beams, nmiss_beams);

            /*
             * Raster hit beam
             */
            for (j = 0; j < nhit_beams; j++) {

                // Provide unprojectd(3D) triangle.
                ri_rasterize_beam( plane_inout, &hit_beams[j], &triangles[i]);
                    
            }

        }

    }

#if 0
        ret = test_beam_triangle( u, v, t, triangles + i, beam );

        switch (ret) {
        case RI_BEAM_HIT_COMPLETELY:
            printf("hit completely. tri = %d of %d\n", i, ntriangles);
            break;

        case RI_BEAM_MISS_COMPLETELY:
        case RI_BEAM_HIT_PARTIALLY:
            printf("miss or hit partially. tri = %d of %d\n", i, ntriangles);

            /* Subdivide beam.  */
            break;
        }
#endif
            

    return 0;
}


/* Retuns one of them.
 *
 *   RI_BEAM_MISS_COMPLETELY
 *   RI_BEAM_HIT_PARTIALLY
 *   RI_BEAM_HIT_COMPLETELY
 */
int
bvh_intersect_leaf_node_beam_visibility(
    ri_qbvh_node_t          *node,
    ri_beam_t               *beam )
{
    uint32_t         tid;
    uint32_t         i;
    uint32_t         ntriangles;
    ri_triangle_t   *triangles;
    ri_triangle2d_t *triangle2ds;

    /*
     * Init
     */
    tid    = 0;

    ntriangles = *((uint32_t *)&node->bbox[0]);
    triangles  = (ri_triangle_t *)node->child[0];

#if 0   // TODO: remove

    /*
     * Firstly, check if it is the first time visiting to this node.
     * This is determined whether child[1:3] is NULL or not.
     * If NULL, it is the first time, so we create list of 2d projected
     * triangles.
     */
    triangle2ds = (ri_triangle2d_t *)node->child[beam->dominant_axis + 1];
    if (triangle2ds == NULL) {

        printf("proj: %p, n = %d\n", &node->child[beam->dominant_axis + 1], ntriangles);

        triangle2ds = ri_mem_alloc(sizeof(ri_triangle2d_t) * ntriangles);
        
        project_triangles( triangle2ds,
                           triangles,
                           ntriangles,
                           beam->dominant_axis,
                           beam->d,
                           beam->org );

        /*
         * TODO: sort triangles in its projectd size.
         */

        node->child[beam->dominant_axis + 1] = (ri_qbvh_node_t *)triangle2ds;

        printf("node set: %p\n", node->child[beam->dominant_axis + 1]);
    }
#endif

#ifdef RI_BVH_ENABLE_DIAGNOSTICS
    if (gdiag) gdiag->ntriangle_isects++;
#endif

#ifdef RI_BVH_TRACE_STATISTICS
    g_stattrav.ntested_triangles += ntriangles;
#endif

    {

        int             ret;
        ri_vector_t     u;
        ri_vector_t     v;
        ri_vector_t     t;
        
#if 0    
        int         nouter_beams;
        int         ninner_beams;
        ri_beam_t   outer_beams[8];
        ri_beam_t   inner_beams[8];
#endif

        for (i = 0; i < ntriangles; i++) {

            ret = test_beam_triangle(
                        u, v, t,
                        &triangles[i],
                        beam);

            if ((ret == RI_BEAM_HIT_COMPLETELY) ||
                (ret == RI_BEAM_HIT_PARTIALLY)) {
                return ret;
            }

                
#if 0
            ri_beam_clip_by_triangle2d(
                &outer_beams[0], &inner_beams[0],
                &nouter_beams, &ninner_beams,
                &triangle2ds[i], beam);

            if (ninner_beams > 0) {
                if (nouter_beams > 0) {
                    return RI_BEAM_HIT_PARTIALLY;
                } else {
                    return RI_BEAM_HIT_COMPLETELY;
                }
            }
#endif
        }

        // The beam completely miss the obstacles.

    }

    return RI_BEAM_MISS_COMPLETELY;
}

/*
 * Traverse BVH with beam
 */
int
bvh_traverse_beam(
    ri_raster_plane_t       *raster_inout,  /* [inout]      */
    ri_qbvh_node_t          *root,
    ri_bvh_diag_t           *diag,          /* [modified]   */
    ri_beam_t               *beam,
    bvh_stack_t             *stack )        /* [buffer]     */
{

    ri_qbvh_node_t       *node;
    int                   ret;
    int                   order;            /* traversal order  */

    assert( root != NULL );

#ifdef RI_BVH_ENABLE_DIAGNOSTICS
    gdiag = diag;
#endif

    /*
     * Initialize intersection state.
     */ 
    memset( raster_inout->t,
            0,
            sizeof(ri_float_t) * raster_inout->width * raster_inout->height ); 

    node = root;

    while (1) {

        if ( node->is_leaf ) {

#ifdef RI_BVH_ENABLE_DIAGNOSTICS
            if (gdiag) gdiag->nleaf_node_traversals++;
#endif
#ifdef RI_BVH_TRACE_STATISTICS
            g_stattrav.nleaf_node_traversals++;
#endif

            bvh_intersect_leaf_node_beam( raster_inout,
                                          node,
                                          beam );

            /* pop */
            if (stack->depth < 1) goto end_traverse;
            stack->depth--;
            assert(stack->depth >= 0);
            node = stack->nodestack[stack->depth];

        } else {

#ifdef RI_BVH_ENABLE_DIAGNOSTICS
            if (gdiag) gdiag->ninner_node_traversals++;
#endif

#ifdef RI_BVH_TRACE_STATISTICS
            g_stattrav.ninner_node_traversals++;
#endif

            ret = test_beam_node( node, beam );

            if (ret == 0) {

                /* pop */
                if (stack->depth < 1) goto end_traverse;
                stack->depth--;
                assert(stack->depth >= 0);
                node = stack->nodestack[stack->depth];

            } else if (ret == 1) {

                node = node->child[0];

            } else if (ret == 2) {

                node = node->child[1];

            } else {    /* both */

                order = beam->dirsign[beam->dominant_axis];

                /* push */
                stack->nodestack[stack->depth] = node->child[1 - order];
                stack->depth++;
                assert( stack->depth < BVH_MAXDEPTH );
                node = node->child[order];
            
            }

        }

    }
    
end_traverse:

    return 0;
}

/*
 * Traverse BVH with beam, just checking whether beam hits obstacles or not.
 */
int
bvh_traverse_beam_visibility(
    ri_qbvh_node_t          *root,
    ri_bvh_diag_t           *diag,          /* [modified]   */
    ri_beam_t               *beam,
    bvh_stack_t             *stack )        /* [buffer]     */
{

    ri_qbvh_node_t       *node;
    int                   ret;
    int                   order;            /* traversal order  */

    assert( root != NULL );

#ifdef RI_BVH_ENABLE_DIAGNOSTICS
    gdiag = diag;
#endif

    node = root;

    while (1) {

        if ( node->is_leaf ) {

#ifdef RI_BVH_ENABLE_DIAGNOSTICS
            if (gdiag) gdiag->nleaf_node_traversals++;
#endif
#ifdef RI_BVH_TRACE_STATISTICS
            g_stattrav.nleaf_node_traversals++;
#endif

            ret = bvh_intersect_leaf_node_beam_visibility( node, beam );

            if ((ret == RI_BEAM_HIT_PARTIALLY) ||
                (ret == RI_BEAM_HIT_COMPLETELY)) {

                /* The beam hits something. Early exit. */
                return ret;

            }

            /* pop */
            if (stack->depth < 1) {
                return RI_BEAM_MISS_COMPLETELY;
            }

            stack->depth--;
            assert(stack->depth >= 0);
            node = stack->nodestack[stack->depth];

        } else {

#ifdef RI_BVH_ENABLE_DIAGNOSTICS
            if (gdiag) gdiag->ninner_node_traversals++;
#endif

#ifdef RI_BVH_TRACE_STATISTICS
            g_stattrav.ninner_node_traversals++;
#endif

            ret = test_beam_node( node, beam );

            if (ret == 0) {

                /* pop */
                if (stack->depth < 1) {
                    return RI_BEAM_MISS_COMPLETELY;
                }

                stack->depth--;
                assert(stack->depth >= 0);
                node = stack->nodestack[stack->depth];

            } else if (ret == 1) {

                node = node->child[0];

            } else if (ret == 2) {

                node = node->child[1];

            } else {    /* both */

                order = beam->dirsign[beam->dominant_axis];

                /* push */
                stack->nodestack[stack->depth] = node->child[1 - order];
                stack->depth++;
                assert( stack->depth < BVH_MAXDEPTH );
                node = node->child[order];
            
            }

        }

    }
    
    return RI_BEAM_MISS_COMPLETELY;
}

/*
 * Project vertices of triangle onto axis-aligned plane.
 */
void
project_triangles(
          ri_triangle2d_t *tri2d_out,      /* [out]                */
    const ri_triangle_t   *triangles,      /* [in]                 */
          uint32_t         ntriangles,
          int              axis,
          ri_float_t       d,              /* distant to the plane         */
          ri_vector_t      org)            /* origin of the beam           */
{

    int uv[3][2] = {
        { 1, 2 }, { 2, 0 }, { 0, 1 } };

    ri_vector_t planes[3] = {
        { 1.0, 0.0, 0.0 },              /* x    */
        { 0.0, 1.0, 0.0 },              /* y    */
        { 0.0, 0.0, 1.0 } };            /* z    */

    uint32_t    i, j;
    ri_vector_t n;
    ri_vector_t vo;
    ri_float_t  t;
    ri_float_t  k;

    assert( axis < 3 );
    assert( ntriangles > 0 );
    assert( triangles != NULL );
    assert( tri2d_out != NULL );


    vcpy( n, planes[axis] );


    for (i = 0; i < ntriangles; i++) {

        /*
         * pv = prjected point of triangle's vertex P.
         * pv = O + vO * (d / (vO . N))
         */

        for (j = 0; j < 3; j++) {

            vsub( vo, triangles[i].v[j], org );

            t = vdot( vo, n );

            if (fabs(t) > RI_EPS) {
                k = d / t;
            } else {
                k = 0.0;
            }

            //tri2d_out[i].v[j][0] = org[uv[axis][0]] + k * vo[uv[axis][0]];
            //tri2d_out[i].v[j][1] = org[uv[axis][1]] + k * vo[uv[axis][1]];
            tri2d_out[i].v[j][0] = k * triangles[i].v[j][uv[axis][0]];
            tri2d_out[i].v[j][1] = k * triangles[i].v[j][uv[axis][1]];

            printf("tri[%d] v[%d] = %f, %f, %f\n",  i, j,
                triangles[i].v[j][0],
                triangles[i].v[j][1],
                triangles[i].v[j][2]);

            printf("tri[%d] pv[%d] = %f, %f\n",  i, j,
                tri2d_out[i].v[j][0],
                tri2d_out[i].v[j][1]);
        }

    }

}


#if 0   // TODO
void
split(
    ri_triangle_t tri,
    ri_float_t volume_threshold)
{
    /*
     * Calculate bbox for each edge of triangle.
     * (And the edge is a diagonal axis for bbox)
     */ 

    int         i;

    ri_vector_t bmin[3];
    ri_vector_t bmax[3];
    
    ri_float_t  volume;
    ri_float_t  maxvolume;
    int         axis;

    
    bmin[0][0] = (tri.v0x  < tri.v1x) ? tri.v0x : tri.v1x;
    bmin[0][1] = (tri.v0y  < tri.v1y) ? tri.v0y : tri.v1y;
    bmin[0][2] = (tri.v0z  < tri.v1z) ? tri.v0z : tri.v1z;
    bmax[0][0] = (tri.v0x >= tri.v1x) ? tri.v0x : tri.v1x;
    bmax[0][1] = (tri.v0y >= tri.v1y) ? tri.v0y : tri.v1y;
    bmax[0][2] = (tri.v0z >= tri.v1z) ? tri.v0z : tri.v1z;

    bmin[1][0] = (tri.v1x  < tri.v2x) ? tri.v1x : tri.v2x;
    bmin[1][1] = (tri.v1y  < tri.v2y) ? tri.v1y : tri.v2y;
    bmin[1][2] = (tri.v1z  < tri.v2z) ? tri.v1z : tri.v2z;
    bmax[1][0] = (tri.v1x >= tri.v2x) ? tri.v1x : tri.v2x;
    bmax[1][1] = (tri.v1y >= tri.v2y) ? tri.v1y : tri.v2y;
    bmax[1][2] = (tri.v1z >= tri.v2z) ? tri.v1z : tri.v2z;

    bmin[2][0] = (tri.v2x  < tri.v0x) ? tri.v2x : tri.v0x;
    bmin[2][1] = (tri.v2y  < tri.v0y) ? tri.v2y : tri.v0y;
    bmin[2][2] = (tri.v2z  < tri.v0z) ? tri.v2z : tri.v0z;
    bmax[2][0] = (tri.v2x >= tri.v0x) ? tri.v2x : tri.v0x;
    bmax[2][1] = (tri.v2y >= tri.v0y) ? tri.v2y : tri.v0y;
    bmax[2][2] = (tri.v2z >= tri.v0z) ? tri.v2z : tri.v0z;

    maxvolume = (bmax[0][0] - bmin[0][0])
              * (bmax[0][1] - bmin[0][1])
              * (bmax[0][2] - bmin[0][2]);
              
    axis      = 0;
          
    for (i = 1; i < 3; i++) { 

        volume = (bmax[i][0] - bmin[i][0])
               * (bmax[i][1] - bmin[i][1])
               * (bmax[i][2] - bmin[i][2]);

        if (volume > maxvolume) {

            maxvolume = volume;
            axis      = i;

        }            

    }

    {
        ri_vector_t  m; 
        ri_triangle_t   newtri;

        if (maxvolume > volume_threshold) {

            /* split    */

            switch (axis) {

            case 0:

                /* split mid of v1 - v0 */ 

                m[0] = tri.v0x + 0.5 * (tri.v1x - tri.v0x);
                m[1] = tri.v0y + 0.5 * (tri.v1y - tri.v0y);
                m[2] = tri.v0z + 0.5 * (tri.v1z - tri.v0z);

                newtri.v0x = m[0]; 
                newtri.v0y = m[1]; 
                newtri.v0z = m[2]; 
                newtri.v1x = tri.v1x; 
                newtri.v1y = tri.v1y; 
                newtri.v1z = tri.v1z; 
                newtri.v2x = tri.v2x; 
                newtri.v2y = tri.v2y; 
                newtri.v2z = tri.v2z; 

                break;

            case 1:
                /* split mid of v2 - v1 */ 

                m[0] = tri.v1x + 0.5 * (tri.v2x - tri.v1x);
                m[1] = tri.v1y + 0.5 * (tri.v2y - tri.v1y);
                m[2] = tri.v1z + 0.5 * (tri.v2z - tri.v1z);

                newtri.v0x = tri.v0x; 
                newtri.v0y = tri.v0y; 
                newtri.v0z = tri.v0z; 
                newtri.v1x = m[0]; 
                newtri.v1y = m[1]; 
                newtri.v1z = m[2]; 
                newtri.v2x = tri.v2x; 
                newtri.v2y = tri.v2y; 
                newtri.v2z = tri.v2z; 

                break;

            default:
                /* split mid of v0 - v2 */ 

                m[0] = tri.v2x + 0.5 * (tri.v0x - tri.v2x);
                m[1] = tri.v2y + 0.5 * (tri.v0y - tri.v2y);
                m[2] = tri.v2z + 0.5 * (tri.v0z - tri.v2z);

                newtri.v0x = tri.v0x; 
                newtri.v0y = tri.v0y; 
                newtri.v0z = tri.v0z; 
                newtri.v1x = tri.v1x; 
                newtri.v1y = tri.v1y; 
                newtri.v1z = tri.v1z; 
                newtri.v2x = m[0]; 
                newtri.v2y = m[1]; 
                newtri.v2z = m[2]; 

                break;
            }

        }

    }
}
#endif

