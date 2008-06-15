/*
 *   lucille | Global Illumination renderer
 *
 *             written by Syoyo Fujita.
 *
 */

/*
 * Bounding Volume Hierarchies for raytracing acceleration.
 *
 * References.
 *  
 * - 4-ary BVH by Kimura's thesis.
 *   <http://www.jaist.ac.jp/library/thesis/ks-master-2007/paper/h-kimura/paper.pdf>
 *
 * - Highly Parallel Fast KD-tree Construction for Interactive Ray Tracing
 *   of Dynamic Scenes
 *   Maxim Shevtsov, Alexei Soupikov and Alexander Kapustin.
 *   EUROGRAPHICS 2007
 *   <http://graphics.cs.uni-sb.de/Courses/ss07/sem/index.html>
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

#include "common.h"
#include "bvh.h"
#include "accel.h"
#include "memory.h"
#include "timer.h"
#include "render.h"
#include "log.h"

//#define LOCAL_TEST
#define LOCAL_DEBUG

#define BVH_MAXDEPTH   100
#define BVH_NTRIS_LEAF 4        /* TODO: parameterize.  */


#define BVH_BIN_SIZE  64

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

typedef struct _triangle_t {

    ri_float_t v0x, v0y, v0z;
    ri_float_t v1x, v1y, v1z;
    ri_float_t v2x, v2y, v2z;

    ri_geom_t  *geom;
    uint32_t    index;

} triangle_t;

typedef struct _tri_bbox_t {

    ri_vector_t bmin;
    ri_vector_t bmax;

    uint64_t    index;          // TODO: compaction.

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
 * Singleton
 */
ri_bvh_stat_traversal_t  g_stattrav;


/* ----------------------------------------------------------------------------
 *
 * Static functions
 *
 * ------------------------------------------------------------------------- */

static void get_bbox_of_triangle(
    ri_vector_t        bmin_out,            /* [out] */  
    ri_vector_t        bmax_out,            /* [out] */  
    const triangle_t  *triangle);

static void calc_scene_bbox(
    ri_vector_t        bmin_out,            /* [out]    */
    ri_vector_t        bmax_out,            /* [out]    */
    const tri_bbox_t  *tri_bboxes,
    uint64_t           ntriangles);

static void create_triangle_list(
    triangle_t       **triangles_out,       /* [out]    */
    tri_bbox_t       **tri_bboxes_out,      /* [out]    */
    uint64_t          *ntriangles,          /* [out]    */
    const ri_list_t   *geom_list);

static void bbox_add_margin(
    ri_vector_t        bmin,                /* [inout]  */
    ri_vector_t        bmax);               /* [inout]  */

static int bin_triangle_edge(
    bvh_bin_buffer_t  *binbuf,              /* [inout]  */
    const ri_vector_t  scene_bmin,
    const ri_vector_t  scene_bmax,
    const tri_bbox_t  *tri_bboxes,
    uint64_t           ntriangles);

static void calc_bbox_of_triangles(
    ri_vector_t        bmin_out,            /* [out]    */
    ri_vector_t        bmax_out,            /* [out]    */
    const tri_bbox_t  *tri_bboxes,
    uint64_t           ntriangles);

static int gather_triangles(
    triangle_t       *triangles_to_out,    /* [out]    */
    const triangle_t *triangles_from,
    const tri_bbox_t *tri_bboxes,
    uint64_t          ntriangles);

static inline int test_ray_aabb(
    ri_float_t        *tmin_out,             /* [out]    */
    ri_float_t        *tmax_out,             /* [out]    */
    const ri_vector_t  bmin,
    const ri_vector_t  bmax,
    ri_ray_t          *ray);

static int bvh_construct(
    ri_qbvh_node_t    *root,
    ri_vector_t        bmin,
    ri_vector_t        bmax,
    triangle_t        *triangles,
    triangle_t        *triangles_buf,
    tri_bbox_t        *tri_bboxes,
    tri_bbox_t        *tri_bboxes_buf,
    uint64_t           index_left,          /* [index_left, index_right)    */
    uint64_t           index_right);

static int bvh_traverse(
    ri_intersection_state_t *state_out,     /* [out]        */
    const ri_qbvh_node_t    *root,
    ri_bvh_diag_t           *diag,          /* [modified]   */
    ri_ray_t                *ray,
    bvh_stack_t             *stack );       /* [buffer]     */


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
 *     scenegeoms - geometry in the scene.
 *     method     - bvh construction method. default is BVH_MEDIAN
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
    
    triangle_t         *triangles;
    triangle_t         *triangles_buf;          /* temporal buffer  */
    tri_bbox_t         *tri_bboxes;
    tri_bbox_t         *tri_bboxes_buf;         /* temporal buffer  */
    uint64_t            ntriangles;

    tm = ri_render_get()->context->timer;

    ri_log( LOG_INFO, "Building BVH ... " );
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

        tri_bboxes_buf = ri_mem_alloc(sizeof(tri_bbox_t) * ntriangles);
        ri_mem_copy(tri_bboxes_buf, tri_bboxes, sizeof(tri_bbox_t)*ntriangles);

        triangles_buf = ri_mem_alloc(sizeof(triangle_t) * ntriangles);
        ri_mem_copy(triangles_buf, triangles, sizeof(triangle_t)*ntriangles);
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

        ri_log(LOG_INFO, "  bmin (%f, %f, %f)",
            bvh->bmin[0], bvh->bmin[1], bvh->bmin[2]);
        ri_log(LOG_INFO, "  bmax (%f, %f, %f)",
            bvh->bmax[0], bvh->bmax[1], bvh->bmax[2]);
    }
    
    ri_log(LOG_INFO, "  # of tris = %d", ntriangles);


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

    ri_log( LOG_INFO, "BVH Construction time: %f sec",
           ri_timer_elapsed( tm, "BVH Construction" ) );

    ri_log( LOG_INFO, "Built BVH." );

    return (void *)bvh;
}

void
ri_bvh_free( void *accel )
{
    ri_bvh_t *bvh = (ri_bvh_t *)accel;

    ri_mem_free(bvh);
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

    if (user) {
        diag_ptr = (ri_bvh_diag_t *)user;
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
        ray->invdir[0] = RI_FLT_MAX;        // FIXME: make this +Inf ?
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

    /*
     * First check if the ray hits scene bbox.
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
    uint32_t         *tid_inout,
    ri_float_t       *t_inout,
    ri_float_t       *u_inout,
    ri_float_t       *v_inout,
    const triangle_t *triangle,
    ri_vector_t       rayorg,
    ri_vector_t       raydir,
    uint32_t          tid)
{
    ri_vector_t v0, v1, v2; 
    ri_vector_t e1, e2; 
    ri_vector_t p, s, q;
    ri_float_t  a, inva;
    ri_float_t  t, u, v;
    double      eps = 1.0e-14;

    v0[0] = triangle->v0x;
    v0[1] = triangle->v0y;
    v0[2] = triangle->v0z;
    v1[0] = triangle->v1x;
    v1[1] = triangle->v1y;
    v1[2] = triangle->v1z;
    v2[0] = triangle->v2x;
    v2[1] = triangle->v2y;
    v2[2] = triangle->v2z;

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
    double      t, u, v;
    uint32_t    tid;

    int         hit;
    int         hitsum;
    uint32_t    i;
    uint32_t    ntriangles;
    triangle_t *triangles;

    ri_vector_t rayorg; 
    ri_vector_t raydir; 

    //
    // Init
    //
    t      = RI_INFINITY;
    u      = 0.0;
    v      = 0.0;
    tid    = 0;
    hit    = 0;
    hitsum = 0;
    vcpy( rayorg, ray->org );
    vcpy( raydir, ray->dir );

    triangles  = (triangle_t *)node->child[0];
    ntriangles = (uintptr_t)node->child[1];

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
    if ( (tmin_x > tmax_y) || (tmin_y > tmax_x) ) {
        return 0;
    }

    tmin = (tmin_x > tmin_y) ? tmin_x : tmin_y;
    tmax = (tmax_x < tmax_y) ? tmax_x : tmax_y;

    /*
     * Z
     */
    const double tmin_z = (min_z - ray->org[2]) * ray->invdir[2];
    const double tmax_z = (max_z - ray->org[2]) * ray->invdir[2];

    /* Early exit: (tmin > tmax_z) || (tmin_z > tmax) => false  */
    if ( (tmin > tmax_z) || (tmin_z > tmax) ) {
        return 0;
    }

    tmin = (tmin > tmin_z) ? tmin : tmin_z;
    tmax = (tmax > tmax_z) ? tmax : tmax_z;

    /* (tmax > 0.0) && (tmin < tmax) => hit */
    if ( (tmax > 0.0) && (tmin < tmax) ) {

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

    ri_float_t  tmin_left  , tmax_left;
    ri_float_t  tmin_right , tmax_right;

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

    //
    // Initialize intersection state.
    // 
    state_out->t     = RI_INFINITY;
    state_out->u     = 0.0;
    state_out->v     = 0.0;
    state_out->geom  = NULL;
    state_out->index = 0;

    node = root;

    while (1) {

        if ( node->is_leaf ) {

#ifdef RI_BVH_ENABLE_DIAGNOSTICS
            diag->nleaf_node_traversals++;
#endif
#ifdef RI_BVH_TRACE_STATISTICS
            g_stattrav.nleaf_node_traversals++;
#endif

            bvh_intersect_leaf_node( state_out,
                                     node,
                                     ray );

            // pop
            if (stack->depth < 1) goto end_traverse;
            stack->depth--;
            assert(stack->depth >= 0);
            node = stack->nodestack[stack->depth];

        } else {

#ifdef RI_BVH_ENABLE_DIAGNOSTICS
            diag->ninner_node_traversals++;
#endif

#ifdef RI_BVH_TRACE_STATISTICS
            g_stattrav.ninner_node_traversals++;
#endif

            ret = test_ray_node( &order, state_out->t, node, ray );

            if (ret == 0) {

                // pop
                if (stack->depth < 1) goto end_traverse;
                stack->depth--;
                assert(stack->depth >= 0);
                node = stack->nodestack[stack->depth];

            } else if (ret == 1) {

                node = node->child[0];

            } else if (ret == 2) {

                node = node->child[1];

            } else {    // both

                // push
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
    // param
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
    uint32_t    min_cost_idx  = 0;
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

    for (j = 0; j < 3; j++) {   // axis

        /*
         *  Compute SAH cost for right side of bbox cell.
         *  Exclude both extreme side of bbox.
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

            // printf("[%d], left = %lld, right = %lld\n", i, tris_left, tris_right);

            //assert(tris_left  <= tris_right);
            assert(tris_left  <=  ntriangles);
            assert(tris_right <=  ntriangles);

            /*
             * split pos = bmin + (i + 1) * (bsize / BIN_SIZE)
             * (i + 1) because we want right side of the cell.
             */
            

            pos = bmin[j] + (i + 1) * bstep[j];

            bmax_left[j]  = pos;
            bmin_right[j] = pos;

            sa_left  = calc_surface_area( bmin_left, bmax_left );
            sa_right = calc_surface_area( bmin_right, bmax_right );

            cost = SAH( tris_left, sa_left, tris_right, sa_right, sa_total );
            
            if (cost < min_cost) {
                min_cost      = cost;
                min_cost_axis = j;
                min_cost_idx  = i + 1;  // TODO: remove
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
    triangle_t     *triangles,
    triangle_t     *triangles_buf,
    tri_bbox_t     *tri_bboxes,
    tri_bbox_t     *tri_bboxes_buf,
    uint64_t        index_left,             /* [index_left, index_right)    */
    uint64_t        index_right)
{

    uint64_t n;

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
         * [0] ptr to triangle list.
         * [1] ntriangles
         */
        root->child[0] = (ri_qbvh_node_t *)(triangles + index_left);
        root->child[1] = (ri_qbvh_node_t *)(uint32_t)n; /* FIXME: support 64bit */

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

        // printf("left = %d, right = %d\n", ntris_left, n - ntris_right );

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


        // [0, BIN_SIZE)
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

    // clear bin data
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

        // printf("min = %d, %d, %d, max = %d, %d, %d\n",
        //     idx_bmin[0], idx_bmin[1], idx_bmin[2],
        //     idx_bmax[0], idx_bmax[1], idx_bmax[2]);

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

    return 0;   // OK

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
    triangle_t       **triangles_out,       /* [out] */
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

    (*triangles_out)  = ri_mem_alloc(sizeof(triangle_t) * n);
    (*tri_bboxes_out) = ri_mem_alloc(sizeof(tri_bbox_t) * n);
    (*ntriangles)     = n;


    /*
     * Construct array of triangles and array of bbox of triangles.
     */

    ri_vector_t bmin, bmax;

    idx = 0;

    for (itr  = ri_list_first( (ri_list_t *)geom_list );
         itr != NULL;
         itr  = ri_list_next( itr ) ) {

        geom = ( ri_geom_t * )itr->data;

        for (i = 0; i < geom->nindices / 3; i++) {

            vcpy(v[0], geom->positions[geom->indices[3 * i + 0]]);    
            vcpy(v[1], geom->positions[geom->indices[3 * i + 1]]);    
            vcpy(v[2], geom->positions[geom->indices[3 * i + 2]]);    

            (*triangles_out)[idx].v0x = v[0][0];
            (*triangles_out)[idx].v0y = v[0][1];
            (*triangles_out)[idx].v0z = v[0][2];

            (*triangles_out)[idx].v1x = v[1][0];
            (*triangles_out)[idx].v1y = v[1][1];
            (*triangles_out)[idx].v1z = v[1][2];

            (*triangles_out)[idx].v2x = v[2][0];
            (*triangles_out)[idx].v2y = v[2][1];
            (*triangles_out)[idx].v2z = v[2][2];

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
    ri_vector_t        bmin_out,         /* [out] */  
    ri_vector_t        bmax_out,         /* [out] */  
    const triangle_t  *triangle)
{
    bmin_out[0] = triangle->v0x; 
    bmin_out[1] = triangle->v0y; 
    bmin_out[2] = triangle->v0z; 
    bmax_out[0] = triangle->v0x; 
    bmax_out[1] = triangle->v0y; 
    bmax_out[2] = triangle->v0z; 

    bmin_out[0] = (bmin_out[0] < triangle->v1x) ? bmin_out[0] : triangle->v1x;
    bmin_out[1] = (bmin_out[1] < triangle->v1y) ? bmin_out[1] : triangle->v1y;
    bmin_out[2] = (bmin_out[2] < triangle->v1z) ? bmin_out[2] : triangle->v1z;

    bmin_out[0] = (bmin_out[0] < triangle->v2x) ? bmin_out[0] : triangle->v2x;
    bmin_out[1] = (bmin_out[1] < triangle->v2y) ? bmin_out[1] : triangle->v2y;
    bmin_out[2] = (bmin_out[2] < triangle->v2z) ? bmin_out[2] : triangle->v2z;

    bmax_out[0] = (bmax_out[0] > triangle->v1x) ? bmax_out[0] : triangle->v1x;
    bmax_out[1] = (bmax_out[1] > triangle->v1y) ? bmax_out[1] : triangle->v1y;
    bmax_out[2] = (bmax_out[2] > triangle->v1z) ? bmax_out[2] : triangle->v1z;

    bmax_out[0] = (bmax_out[0] > triangle->v2x) ? bmax_out[0] : triangle->v2x;
    bmax_out[1] = (bmax_out[1] > triangle->v2y) ? bmax_out[1] : triangle->v2y;
    bmax_out[2] = (bmax_out[2] > triangle->v2z) ? bmax_out[2] : triangle->v2z;
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
    triangle_t       *triangles_to_out,    /* [out]    */
    const triangle_t *triangles_from,
    const tri_bbox_t *tri_bboxes,
    uint64_t          ntriangles)
{
    uint64_t i;
    uint64_t j;

    for (i = 0; i < ntriangles; i++) {

        j = tri_bboxes[i].index;

        memcpy( triangles_to_out + i, triangles_from + j, sizeof(triangle_t));
        

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
