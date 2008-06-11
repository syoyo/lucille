/*
 * $Id$
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <math.h>

#include "ri.h"
#include "vector.h"
#include "list.h"
#include "memory.h"
#include "log.h"
#include "render.h"
#include "parallel.h"
#include "ugrid.h"

/* use z curve order for addressing voxel memory. */
#define USE_ZORDER 1

#define MAX_OCTREE_DEPTH 6

#define BLKIDX(x, y, z, shift, blksize) (                 \
        ((z >> shift) * blksize * blksize) +            \
        ((y >> shift) * blksize) + (x >> shift))

#define SUBIDX(x, y, z, width, mask) (                     \
        (z & mask) * width * width +                \
        (y & mask) * width + (x & mask))

#define MAP_XYZ(x, y, z, shift, blksize, width, mask)             \
        BLKIDX((x), (y), (z), (shift), (blksize)) *        \
        (width) * (width) * (width) +                \
        SUBIDX((x), (y), (z), (width), (mask))

#define MAP_Z3D(x, y, z) (                        \
        g_z_table[(x)] | (g_z_table[(y)] << 1) |        \
        (g_z_table[(z)] << 2)) 



static void calc_bbox(  ri_list_t        *geom_list,
                        double            min[3],
                        double            max[3]);

static uint32_t calc_sum_ntriangles(
                        ri_list_t        *geom_list);

static void calc_polybbox(
                        ri_vector_t       v[3],
                        double            min[3],
                        double            max[3]);

static void         conv_cell(ri_tri_list_t *dst[GRIDSIZE][GRIDSIZE][GRIDSIZE],
                  ri_array_t    **src,
                    int xvoxels, int yvoxels, int zvoxels);
#if defined(WITH_SSE) || defined(WITH_ALTIVEC)
/* table for z curve order */
static unsigned int g_z_table[256];
static void         conv_simd(ri_ugrid_t *ugrid,
                    int xvoxels, int yvoxels, int zvoxels);
static void         copy_simd(ri_tri_list_t *dst);
static void         free_cell_simd(ri_tri_list_t *cell);
static void         copy_simd_flat(ri_ugrid_t *ugrid,
                   int xvoxels, int yvoxels, int zvoxels);
static void         build_z_table();
#endif
static void         free_cell(ri_tri_list_t *cell);


/*
 * Function: ri_ugrid_build
 *
 *     Builds uniform grid data structure for accelerating ray-tracing.
 *     Scene data are read from rendering context.
 *
 * Parameters:
 *
 *     None.
 *
 * Returns:
 *
 *     Built uniform grid data strucure.
 */
void *
ri_ugrid_build()
{
    unsigned int i, j, k;
    int x, y, z;
    int idx;
    int cuberoot;
    unsigned int maxtrisincell = 0;    /* maximum number of polygons
                     * one cell contains. */
    unsigned int ntriangles;
    ri_tri_info_t triinfo;
    
    double  bmin[3], bmax[3];
    double  pmin[3], pmax[3];
    double  delta;        /* maximum absolute length in bmin & bmax */
    double  dx, dy, dz;
    double  xw, yw, zw;
    double invxw, invyw, invzw;
    int    xvoxels, yvoxels, zvoxels;
    double maxwidth, invmaxwidth;
    int    x0, x1, y0, y1, z0, z1;
    ri_vector_t v[3];
    ri_ugrid_t *ugrid;
    ri_list_t  *geomitr;
    ri_geom_t  *geom;
    ri_array_t  **cell;

    ri_scene_t  *scene;

    /* Partition grids into 4x4x4 sub blocks */
    int    blkwidth = 4;

    ri_log(LOG_INFO, "Building accel grid ... ");

    ri_timer_start(ri_render_get()->context->timer, "Uniform grid building");
    ugrid = (ri_ugrid_t *)ri_mem_alloc(sizeof(ri_ugrid_t));

    ugrid->blkwidth  = blkwidth;
    ugrid->blksize   = GRIDSIZE / blkwidth;
    ugrid->shiftsize = (int)(log(blkwidth) / log(2)); /* log(2, blkwidth) */
    ugrid->bitmask   = blkwidth - 1;

    cell = (ri_array_t **)ri_mem_alloc(sizeof(ri_array_t *) *
                      GRIDSIZE * GRIDSIZE * GRIDSIZE);

    for (i = 0; i < GRIDSIZE; i++) {
        for (j = 0; j < GRIDSIZE; j++) {
            for (k = 0; k < GRIDSIZE; k++) {
                idx = i*GRIDSIZE*GRIDSIZE+j*GRIDSIZE+k;
                ugrid->cell[i][j][k] = NULL;    
                cell[idx] = NULL;
            }
        }
    }

    ugrid->cdat = (ri_tri_list_t **)
                ri_mem_alloc(sizeof(ri_tri_list_t *) *
                         GRIDSIZE * GRIDSIZE * GRIDSIZE);
    for (i = 0; i < GRIDSIZE * GRIDSIZE * GRIDSIZE; i++) {
        ugrid->cdat[i] = NULL;
    }

    scene = ri_render_get()->scene;

    calc_bbox( scene->geom_list, bmin, bmax);

    /* calc maximun length, delta */
    if (fabs(bmin[0]) > fabs(bmin[1]))   delta = fabs(bmin[0]);
    else                     delta = fabs(bmin[1]);
    if (delta < fabs(bmin[2]))          delta = fabs(bmin[2]);

    if (delta < fabs(bmax[0]))         delta = fabs(bmax[0]);
    if (delta < fabs(bmax[1]))         delta = fabs(bmax[1]);
    if (delta < fabs(bmax[2]))         delta = fabs(bmax[2]);

    delta *= 1.0e-4;

    bmin[0] -= delta; bmin[1] -= delta; bmin[2] -= delta;
    bmax[0] += delta; bmax[1] += delta; bmax[2] += delta;

    dx = 1.00001 * (bmax[0] - bmin[0]);
    dy = 1.00001 * (bmax[1] - bmin[1]);
    dz = 1.00001 * (bmax[2] - bmin[2]);

    /* calc inverse of maximum cube width */
    maxwidth = dx > dy ? dx : dy;
    maxwidth = maxwidth > dz ? maxwidth : dz;

    if (maxwidth != 0.0) {
        invmaxwidth = 1.0 / maxwidth;
    } else {
        ri_log(LOG_ERROR, "maxwidth = 0.0");
        return NULL;
    }

    ntriangles = calc_sum_ntriangles(scene->geom_list);
    cuberoot = (int)pow(ntriangles, 0.333333);

    xvoxels = 3 * (int)ceil(cuberoot * dx * invmaxwidth);
    yvoxels = 3 * (int)ceil(cuberoot * dy * invmaxwidth);
    zvoxels = 3 * (int)ceil(cuberoot * dz * invmaxwidth);

    if (xvoxels < 1       ) xvoxels = 1;
    if (xvoxels > GRIDSIZE) xvoxels = GRIDSIZE;
    if (yvoxels < 1          )    yvoxels = 1;
    if (yvoxels > GRIDSIZE) yvoxels = GRIDSIZE;
    if (zvoxels < 1       )    zvoxels = 1;
    if (zvoxels > GRIDSIZE) zvoxels = GRIDSIZE;

    xw = dx / xvoxels;
    yw = dy / yvoxels;
    zw = dz / zvoxels;
    
    invxw = (xw == 0.0) ? 0.0 : 1.0 / xw;
    invyw = (yw == 0.0) ? 0.0 : 1.0 / yw;
    invzw = (zw == 0.0) ? 0.0 : 1.0 / zw;

    for (geomitr  = ri_list_first(scene->geom_list);
         geomitr != NULL;
         geomitr  = ri_list_next(geomitr)) {
        geom = (ri_geom_t *)geomitr->data;

        for (i = 0; i < geom->nindices / 3; i++) {

            triinfo.index = 3 * i;
            triinfo.geom = geom;
            triinfo.id = -1;

            ri_vector_copy(v[0], geom->positions[geom->indices[3 * i + 0]]);    
            ri_vector_copy(v[1], geom->positions[geom->indices[3 * i + 1]]);    
            ri_vector_copy(v[2], geom->positions[geom->indices[3 * i + 2]]);    

            calc_polybbox(v, pmin, pmax);

            x0 = (int)((pmin[0] - bmin[0]) * invxw);
            if (x0 < 0) x0 = 0;
            if (x0 > xvoxels - 1) x0 = xvoxels - 1;

            x1 = (int)((pmax[0] - bmin[0]) * invxw);
            if (x1 < 0) x1 = 0;
            if (x1 > xvoxels - 1) x1 = xvoxels - 1;

            y0 = (int)((pmin[1] - bmin[1]) * invyw);
            if (y0 < 0) y0 = 0;
            if (y0 > yvoxels - 1) y0 = yvoxels - 1;

            y1 = (int)((pmax[1] - bmin[1]) * invyw);
            if (y1 < 0) y1 = 0;
            if (y1 > yvoxels - 1) y1 = yvoxels - 1;

            z0 = (int)((pmin[2] - bmin[2]) * invzw);
            if (z0 < 0) z0 = 0;
            if (z0 > zvoxels - 1) z0 = zvoxels - 1;

            z1 = (int)((pmax[2] - bmin[2]) * invzw);
            if (z1 < 0) z1 = 0;
            if (z1 > zvoxels - 1) z1 = zvoxels - 1;

            for (z = z0; z <= z1; z++) {
                for (y = y0; y <= y1; y++) {
                    for (x = x0; x <= x1; x++) {
                        idx = z * GRIDSIZE * GRIDSIZE
                            + y * GRIDSIZE + x;

                        if (cell[idx] == NULL) {
                            cell[idx] =
                            ri_array_new(
                            sizeof(ri_tri_info_t));
                        }
            
                        ri_array_insert(
                            cell[idx],
                            cell[idx]->nelems,
                            (void *)&(triinfo));

                        if (maxtrisincell < 
                            cell[idx]->nelems) {
                            maxtrisincell = 
                                cell[idx]->nelems;    
                        }
                    }
                }
            }
        }
    }

    conv_cell(ugrid->cell, cell,
          xvoxels, yvoxels, zvoxels);

    ri_mem_free(cell);

#if defined(WITH_SSE) || defined(WITH_ALTIVEC)
    //calc_hilbert_table(ugrid->hilbtable, GRIDSIZE, 
    //           ugrid->shiftsize, ugrid->blksize,
    //           ugrid->blkwidth, ugrid->bitmask);
    conv_simd(ugrid, xvoxels, yvoxels, zvoxels);
#endif
    
    ugrid->bboxmin[0] = (float)bmin[0];
    ugrid->bboxmin[1] = (float)bmin[1];
    ugrid->bboxmin[2] = (float)bmin[2];

    ugrid->bboxmax[0] = (float)bmax[0];
    ugrid->bboxmax[1] = (float)bmax[1];
    ugrid->bboxmax[2] = (float)bmax[2];

    ri_log(LOG_INFO, "Built accel grid.");

    if (ri_parallel_taskid() == 0 &&
        ri_log_get_debug() ) {
        printf("--- Uniform grid building statistics ---\n");
        printf("bmin = [ %f, %f, %f ]\n", bmin[0], bmin[1], bmin[2]);
        printf("bmax = [ %f, %f, %f ]\n", bmax[0], bmax[1], bmax[2]);
        printf("voxles = [ %d, %d, %d ]\n", xvoxels, yvoxels, zvoxels);
        printf("ntriangles = [ %u ]\n", ntriangles);
        printf("maximum tris in cell = [ %d ]\n", maxtrisincell);
        printf("width = [ %f, %f, %f ]\n", xw, yw, zw);
        printf("----------------------------------------\n");
    }

    ugrid->voxels[0] = xvoxels;
    ugrid->voxels[1] = yvoxels;
    ugrid->voxels[2] = zvoxels;

    ugrid->width[0] = (float)xw;
    ugrid->width[1] = (float)yw;
    ugrid->width[2] = (float)zw;

    ugrid->invwidth[0] = (float)invxw;
    ugrid->invwidth[1] = (float)invyw;
    ugrid->invwidth[2] = (float)invzw;

    ugrid->curr_rayid  = 0;

    ri_timer_end(ri_render_get()->context->timer, "Uniform grid building");

    return (void *)ugrid;
}

/*
 * Function: ri_ugrid_free
 *
 *     Frees ugrid data.
 *
 * Parameters:
 *
 *     *accel - Uniform grid data strucure to be freed.
 *
 * Returns:
 *
 *     None.
 */   
void
ri_ugrid_free(void *accel)
{
    unsigned int i, j, k;

    ri_ugrid_t *ugrid = (ri_ugrid_t *)accel;

    if (ugrid == NULL) {
        ri_log(LOG_WARN, "Trying to free NULL pointer\n");
        return;
    }

    for (i = 0; i < GRIDSIZE; i++) {
        for (j = 0; j < GRIDSIZE; j++) {
            for (k = 0; k < GRIDSIZE; k++) {
                if (ugrid->cell[i][j][k] &&
                    ugrid->cell[i][j][k]->ntris > 0) {
                    free_cell(ugrid->cell[i][j][k]);
#if defined(WITH_SSE) || defined(WITH_ALTIVEC)
                    free_cell_simd(ugrid->cell[i][j][k]);
#endif
                    ri_mem_free(ugrid->cell[i][j][k]);    
                }
            }
        }
    }

    ri_mem_free(ugrid->tridata);

    ri_mem_free(ugrid);
}

int
ri_ugrid_intersect(
    void                    *accel,
    ri_ray_t                *ray,
    ri_intersection_state_t *state)
{
    // TODO:

    return 0;
} 


/* --- private functions --- */
static void
calc_bbox(ri_list_t *geom_list, double min[3], double max[3])
{
    unsigned int i;
    ri_vector_t  v;
    ri_list_t   *itr;
    ri_geom_t   *geom;


    min[0] = min[1] = min[2] =  RI_INFINITY;
    max[0] = max[1] = max[2] = -RI_INFINITY;

    for (itr  = ri_list_first(geom_list);
         itr != NULL;
         itr  = ri_list_next(itr)) {

        geom = (ri_geom_t *)itr->data;

        for (i = 0; i < geom->npositions; i++) {
            ri_vector_copy(v, geom->positions[i]);

            if (min[0] > v[0]) min[0] = v[0];    
            if (min[1] > v[1]) min[1] = v[1];    
            if (min[2] > v[2]) min[2] = v[2];    

            if (max[0] < v[0]) max[0] = v[0];    
            if (max[1] < v[1]) max[1] = v[1];    
            if (max[2] < v[2]) max[2] = v[2];    
        }
    }
}

static void
calc_polybbox(
    ri_vector_t       v[3],
    double            min[3],
    double            max[3])
{
    min[0] = v[0][0] < v[1][0] ? v[0][0] : v[1][0];
    min[0] = min[0]  < v[2][0] ? min[0]  : v[2][0];
    min[1] = v[0][1] < v[1][1] ? v[0][1] : v[1][1];
    min[1] = min[1]  < v[2][1] ? min[1]  : v[2][1];
    min[2] = v[0][2] < v[1][2] ? v[0][2] : v[1][2];
    min[2] = min[2]  < v[2][2] ? min[2]  : v[2][2];

    max[0] = v[0][0] > v[1][0] ? v[0][0] : v[1][0];
    max[0] = max[0]  > v[2][0] ? max[0]  : v[2][0];
    max[1] = v[0][1] > v[1][1] ? v[0][1] : v[1][1];
    max[1] = max[1]  > v[2][1] ? max[1]  : v[2][1];
    max[2] = v[0][2] > v[1][2] ? v[0][2] : v[1][2];
    max[2] = max[2]  > v[2][2] ? max[2]  : v[2][2];
}

static unsigned int
calc_sum_ntriangles(ri_list_t *geom_list)
{
    unsigned int  ntris;
    ri_list_t    *itr;
    ri_geom_t    *geom;

    ntris = 0;

    for (itr  = ri_list_first(geom_list);
         itr != NULL;
         itr  = ri_list_next(itr)) {
        geom = (ri_geom_t *)itr->data;

        ntris += geom->nindices / 3;
    }

    return ntris;
}

static void
conv_cell(ri_tri_list_t *dst[GRIDSIZE][GRIDSIZE][GRIDSIZE], 
      ri_array_t    **src,
      int xvoxels, int yvoxels, int zvoxels)
{
    int x, y, z;
    int i;
    int n;
    int pos;
    ri_tri_info_t *tmpinfo;
    //int idx;

    for (z = 0; z < zvoxels; z++) {
        for (y = 0; y < yvoxels; y++) {
            for (x = 0; x < xvoxels; x++) {
                pos = z * GRIDSIZE * GRIDSIZE + 
                      y * GRIDSIZE + x;
                if (!src[pos]) continue;

                n = src[pos]->nelems;
                if (n == 0) continue;

                dst[z][y][x] = (ri_tri_list_t *)ri_mem_alloc(
                        sizeof(ri_tri_list_t));

                dst[z][y][x]->tris = (ri_tri_info_t *)
                       ri_mem_alloc(
                       sizeof(ri_tri_info_t) * n);
                dst[z][y][x]->ntris = n;

                for (i = 0; i < n; i++) { 
                    tmpinfo = (ri_tri_info_t *)
                            ri_array_at(src[pos],
                                    i);

                    dst[z][y][x]->tris[i].index =
                        tmpinfo->index;

                    dst[z][y][x]->tris[i].geom =
                        tmpinfo->geom;

                    dst[z][y][x]->tris[i].id =
                        tmpinfo->id;
                }

                ri_array_free(src[pos]);
            }
        }
    }

}


#if defined(WITH_SSE) || defined(WITH_ALTIVEC)
/* construct SIMD friendy data structure */
static void
conv_simd(ri_ugrid_t *ugrid,
      int xvoxels, int yvoxels, int zvoxels)
{
    int          x, y, z;
    unsigned int  index;
    unsigned long nblocks = 0;
    ri_tri_list_t *list;

    /* Frist, count number of triangles in the scene.  */
    for (z = 0; z < zvoxels; z++) {
        for (y = 0; y < yvoxels; y++) {
            for (x = 0; x < xvoxels; x++) {
                list = ugrid->cell[z][y][x];

                if (!list) continue;

                /* SIMD array must be a multiple of 4 */
                nblocks += (unsigned long)
                        ceil((double)list->ntris /
                             (double)4);
            }
        }
    }

    /* 4 = 4 triangles,
     * 9 = (xyz 3 components) * (3 vertices) = compose 1 triangle
     */
    /* allocate 16-byte aligned memory */
    ugrid->tridata = ri_mem_alloc_aligned(4 * 9 * nblocks, 16);

    for (z = 0; z < zvoxels; z++) {
        for (y = 0; y < yvoxels; y++) {
            for (x = 0; x < xvoxels; x++) {
                if (!ugrid->cell[z][y][x]) {
                    continue;
                }

                copy_simd(ugrid->cell[z][y][x]);

                free_cell(ugrid->cell[z][y][x]);
            }
        }
    }

    //printf("orgazize block cdat\n");
    //printf("voxels = %d, %d, %d\n", zvoxels, yvoxels, xvoxels);

    build_z_table();

    /* Organize block'ed voxel array.  */
    for (z = 0; z < zvoxels; z++) {
        for (y = 0; y < yvoxels; y++) {
            for (x = 0; x < xvoxels; x++) {
                list = ugrid->cell[z][y][x];

#if USE_ZORDER        /* z curve order access */

                index = MAP_Z3D(x, y, z);

#else            /* blocked access */

                index = MAP_XYZ(x, y, z,
                        ugrid->shiftsize,
                        ugrid->blksize,
                        ugrid->blkwidth,
                        ugrid->bitmask);
#endif

                if (!list) {
                    if (ugrid->cdat[index]) {
                        printf("???\n");
                    }
                    ugrid->cdat[index] = NULL;
                    continue;
                }

                ugrid->cdat[index] = list;
            }
        }
    }
        
    copy_simd_flat(ugrid, xvoxels, yvoxels, zvoxels);
}

static void
copy_simd(ri_tri_list_t *dst)
{
    int          i, j;
    int          index;
    int          offset;
    int          nextra;
    ri_vector_t v0, v1, v2;
    ri_vector_t e1, e2;
    unsigned int i0, i1, i2;
    int nblocks;
    ri_geom_t *geom;
    ri_simd_tri_info_t *simdinfo;

    if (!dst->tris) return;
    if (dst->ntris < 1) return;

    dst->simdtris = (ri_simd_tri_info_t *)ri_mem_alloc(
                sizeof(ri_simd_tri_info_t));

    /* simd array must be a multiple of 4 */
    nblocks = (int)ceil((double)dst->ntris / (double)4);
    if (nblocks < 1) nblocks = 1;

    simdinfo = dst->simdtris;

    simdinfo->nblocks = nblocks;

    /* 4 = 4 triangles,
     * 9 = (xyz 3 components) * (3 vertices) = 1 triangle
     */
    /* allocate 16-byte aligned memory */
    simdinfo->tridata = ri_mem_alloc_aligned(4 * 9 * nblocks, 16);

    simdinfo->geoms = (ri_geom_t **)ri_mem_alloc(
                sizeof(ri_geom_t *) * nblocks * 4);
    simdinfo->indices = (unsigned int *)ri_mem_alloc(
                sizeof(unsigned int) * nblocks * 4);

    for (j = 0; j < nblocks - 1; j++) {
        for (i = 0; i < 4; i++) {
            offset = 4 * j + i;
            geom = dst->tris[offset].geom;
            index = dst->tris[offset].index;
            i0 = geom->indices[index + 0];
            i1 = geom->indices[index + 1];
            i2 = geom->indices[index + 2];

            ri_vector_copy(v0, geom->positions[i0]);
            ri_vector_copy(v1, geom->positions[i1]);
            ri_vector_copy(v2, geom->positions[i2]);

            ri_vector_sub(e1, v1, v0);
            ri_vector_sub(e2, v2, v0);

            simdinfo->tridata[36 * j +  0 + i] = v0[0];
            simdinfo->tridata[36 * j +  4 + i] = v0[1];
            simdinfo->tridata[36 * j +  8 + i] = v0[2];
            simdinfo->tridata[36 * j + 12 + i] = e1[0];
            simdinfo->tridata[36 * j + 16 + i] = e1[1];
            simdinfo->tridata[36 * j + 20 + i] = e1[2];
            simdinfo->tridata[36 * j + 24 + i] = e2[0];
            simdinfo->tridata[36 * j + 28 + i] = e2[1];
            simdinfo->tridata[36 * j + 32 + i] = e2[2];
            simdinfo->indices[offset] = index;
            simdinfo->geoms[offset]   = geom;
        }
    }

    /* the reminder */
    nextra = dst->ntris - (nblocks - 1) * 4;
    for (i = 0; i < nextra; i++) {
        offset = (nblocks - 1);
        geom = dst->tris[4 * offset + i].geom;
        index = dst->tris[4 * offset + i].index;
        i0 = geom->indices[index + 0];
        i1 = geom->indices[index + 1];
        i2 = geom->indices[index + 2];

        ri_vector_copy(v0, geom->positions[i0]);
        ri_vector_copy(v1, geom->positions[i1]);
        ri_vector_copy(v2, geom->positions[i2]);

        ri_vector_sub(e1, v1, v0);
        ri_vector_sub(e2, v2, v0);

        simdinfo->tridata[36 * offset +  0 + i] = v0[0];
        simdinfo->tridata[36 * offset +  4 + i] = v0[1];
        simdinfo->tridata[36 * offset +  8 + i] = v0[2];
        simdinfo->tridata[36 * offset + 12 + i] = e1[0];
        simdinfo->tridata[36 * offset + 16 + i] = e1[1];
        simdinfo->tridata[36 * offset + 20 + i] = e1[2];
        simdinfo->tridata[36 * offset + 24 + i] = e2[0];
        simdinfo->tridata[36 * offset + 28 + i] = e2[1];
        simdinfo->tridata[36 * offset + 32 + i] = e2[2];
        simdinfo->indices[4 * offset + i] = index;
        simdinfo->geoms[4 * offset + i]   = geom;

    }

    /* fill the rest with last triangle data */
    if (nextra != 0) {
        for (i = nextra; i < 4; i++) {
            offset = (nblocks - 1);
            geom = dst->tris[4 * offset + nextra - 1].geom;
            index = dst->tris[4 * offset + nextra - 1].index;
            i0 = geom->indices[index + 0];
            i1 = geom->indices[index + 1];
            i2 = geom->indices[index + 2];

            ri_vector_copy(v0, geom->positions[i0]);
            ri_vector_copy(v1, geom->positions[i1]);
            ri_vector_copy(v2, geom->positions[i2]);

            ri_vector_sub(e1, v1, v0);
            ri_vector_sub(e2, v2, v0);

            simdinfo->tridata[36*offset+ 0+i] = v0[0];
            simdinfo->tridata[36*offset+ 4+i] = v0[1];
            simdinfo->tridata[36*offset+ 8+i] = v0[2];
            simdinfo->tridata[36*offset+12+i] = e1[0];
            simdinfo->tridata[36*offset+16+i] = e1[1];
            simdinfo->tridata[36*offset+20+i] = e1[2];
            simdinfo->tridata[36*offset+24+i] = e2[0];
            simdinfo->tridata[36*offset+28+i] = e2[1];
            simdinfo->tridata[36*offset+32+i] = e2[2];
            simdinfo->indices[4 * offset + i] = index;
            simdinfo->geoms[4 * offset + i]   = geom;
        }
    }
}

static void
free_cell_simd(ri_tri_list_t *cell)
{
    if (!cell->simdtris) return;

    //ri_aligned_float_free(&(cell->simdtris->tridata));
    ri_mem_free(cell->simdtris->geoms);
    ri_mem_free(cell->simdtris->indices);
}

static void
copy_simd_flat(ri_ugrid_t *ugrid, int xvoxels, int yvoxels, int zvoxels)
{
    unsigned int        index;
    int                 x, y, z;
    ri_simd_tri_info_t *simdinfo;
    unsigned long       array_idx = 0;

    for (z = 0; z < zvoxels; z++) {
        for (y = 0; y < yvoxels; y++) {
            for (x = 0; x < xvoxels; x++) {
#if USE_ZORDER     /* z curve order access */
                index = MAP_Z3D(x, y, z);
#else
                index = MAP_XYZ(x, y, z,
                        ugrid->shiftsize,
                        ugrid->blksize,
                        ugrid->blkwidth,
                        ugrid->bitmask);

#endif

                if (!ugrid->cdat[index]) continue;

                simdinfo = ugrid->cdat[index]->simdtris;

                /* one 4-triangle data structure occupies
                 * 4 * 9 floats */

                ri_mem_copy(
                    &(ugrid->tridata[array_idx]),
                    simdinfo->tridata,
                    sizeof(float) * simdinfo->nblocks *
                    4 * 9);     

                /* used in raytrace.c */
                simdinfo->tridataptr =
                    &(ugrid->tridata[array_idx]);

                array_idx += simdinfo->nblocks * 4 * 9;

                /* frees unflatten simd data array */
                ri_mem_free(simdinfo->tridata);
            }
        }
    }
}

#endif

static void
free_cell(ri_tri_list_t *cell)
{
    ri_mem_free(cell->tris);
    cell->tris = NULL;
}




#if defined(WITH_SSE) || defined(WITH_ALTIVEC)
static void
build_z_table()
{
    unsigned int   i, j;
    unsigned int   bit;
    unsigned int   v;
    unsigned int   ret;
    unsigned int   mask = 0x1;
    int            shift;

    for (i = 0; i < 256; i++) {
        v = i;
        shift = 0;
        ret = 0;
        for (j = 0; j < 8; j++) {
            /* extract (j+1)'th bit */
            bit = (v >> j) & mask;

            ret += bit << shift;
            shift += 3;
        }

        g_z_table[i] = ret;
    }
}
#endif


