#include <stdio.h>
#include <stdlib.h>

#include "raster.h"

/* ----------------------------------------------------------------------------
 *
 * Public functions
 *
 * ------------------------------------------------------------------------- */

ri_raster_plane_t *
ri_raster_plane_new()
{

    ri_raster_plane_t *p;

    p = (ri_raster_plane_t *)ri_mem_alloc(sizeof(ri_raster_plane_t));

    return p;
}

int
ri_raster_plane_init(
    ri_raster_plane_t *plane_inout,     /* [inout]  */
    int                width,
    int                height,
    ri_vector_t        frame[3],
    ri_vector_t        corner,
    ri_vector_t        org,
    ri_float_t         fov)
{
    
    size_t sz;

    sz = width * height;

    plane_inout->width  = width;
    plane_inout->height = height;

    plane_inout->t      = (ri_float_t  *)ri_mem_alloc(sizeof(ri_float_t ) * sz);
    plane_inout->u      = (ri_float_t  *)ri_mem_alloc(sizeof(ri_float_t ) * sz);
    plane_inout->v      = (ri_float_t  *)ri_mem_alloc(sizeof(ri_float_t ) * sz);
    plane_inout->geom   = (ri_geom_t  **)ri_mem_alloc(sizeof(ri_geom_t *) * sz);
    plane_inout->index  = (uint32_t    *)ri_mem_alloc(sizeof(uint32_t   ) * sz);

    memset(plane_inout->t    , 0, sizeof(ri_float_t ) * sz);
    memset(plane_inout->u    , 0, sizeof(ri_float_t ) * sz);
    memset(plane_inout->v    , 0, sizeof(ri_float_t ) * sz);
    memset(plane_inout->geom , 0, sizeof(ri_geom_t *) * sz);
    memset(plane_inout->index, 0, sizeof(uint32_t   ) * sz);

    memcpy(plane_inout->frame, frame, sizeof(ri_vector_t) * 3);
    vcpy( plane_inout->corner, corner );
    vcpy( plane_inout->org   , org    );
    plane_inout->fov = fov;

    return 0;
}

int
ri_raster_plane_free(
    ri_raster_plane_t *plane)
{

    if (plane == NULL) return -1;

    ri_mem_free(plane->t);
    ri_mem_free(plane->u);
    ri_mem_free(plane->v);
    ri_mem_free(plane->geom); 
    ri_mem_free(plane->index);

    return 0;   // OK
}


/*
 * Rasterize triangle in 3D into 2D raster plane.
 */
void
ri_rasterize_triangle(
    ri_raster_plane_t *plane,
    ri_triangle_t     *triangle)
{
    int         s, t;

    /*
     * Use ray casting for robust rasterization.
     */

    int         i;
    int         hit;
    uint32_t    tid;
    ri_float_t  tparam, uparam, vparam;

    ri_vector_t dir;

    /*
     * p  = M F E v
     *
     *      | w                              w      |
     * M  = | - ----------        0        - -   0  |
     *      | 2 tan(fov/2)                   2      |
     *      |                                       |
     *      |                h               h      |
     *      |      0         - ----------  - -   0  |
     *      |                2 tan(fov/2)    2      |
     *      |                                       |
     *      |      0              0          1   0  |
     *      |                                       |
     *      |      0              0         -1   0  |
     *
     *  F = | du       0 |
     *      | dv       0 |
     *      | -dw      0 |
     *      | 0  0  0  0 |
     *
     *  E = | 1  0  0 -org |
     *      | 0  1  0      |
     *      | 0  0  1      |
     *      | 0  0  0  1   |
     *
     *  v = | x |
     *      | y |
     *      | z |
     *      | w |
     */

    ri_vector_t vo;
    ri_vector_t w;
    ri_vector_t p[3];

    int width      = plane->width;
    int height     = plane->height;
    ri_float_t fov = plane->fov;

    for (i = 0; i < 3; i++) {

        /* vo = E v */
        vsub( vo, triangle->v[i], plane->org );
        
        /* w = F E v */
        w[0] =  plane->frame[0][0] * vo[0]
             +  plane->frame[0][1] * vo[1] 
             +  plane->frame[0][2] * vo[2];
        w[1] =  plane->frame[1][0] * vo[0] 
             +  plane->frame[1][1] * vo[1] 
             +  plane->frame[1][2] * vo[2];
        w[2] = -plane->frame[2][0] * vo[0] 
             -  plane->frame[2][1] * vo[1] 
             -  plane->frame[2][2] * vo[2];

        /* p = M F E v */
        p[i][0] = 0.5 * width  * tan(0.5 * fov) * w[0] - 0.5 * width  * w[2];
        p[i][1] = 0.5 * height * tan(0.5 * fov) * w[1] - 0.5 * height * w[2];
        p[i][2] = w[2];

        printf("[raster] proj p = %f, %f, %f\n", p[i][0], p[i][1], p[i][2]);

    }

    /*
     * Compute 2D bbox.
     */
    ri_float_t bmin[2], bmax[2];

    bmin[0] = bmax[0] = p[0][0];
    bmin[1] = bmax[1] = p[0][1];

    bmin[0] = (p[1][0] < bmin[0]) ? p[1][0] : bmin[0];
    bmax[0] = (p[1][0] > bmax[0]) ? p[1][0] : bmax[0];
    bmin[1] = (p[2][1] < bmin[1]) ? p[2][1] : bmin[1];
    bmax[1] = (p[2][1] > bmax[1]) ? p[2][1] : bmax[1];

    printf("[raster] bbox = (%d, %d) - (%d, %d)\n",
        (int)bmin[0], (int)bmin[1],
        (int)bmax[0], (int)bmax[1] );
        

    for (t = (int)bmin[1]; t < (int)bmax[1]; t++) {

        for (s = (int)bmin[0]; s < (int)bmax[0]; s++) {

            /* corner + s * frame[0] + t * frame[1] */
            dir[0] = plane->corner[0]
                   + s * plane->frame[0][0] + t * plane->frame[1][0];
            dir[1] = plane->corner[1] 
                   + s * plane->frame[0][1] + t * plane->frame[1][1];
            dir[2] = plane->corner[2] 
                   + s * plane->frame[0][2] + t * plane->frame[1][2];

            hit = ri_triangle_isect( &tid, &tparam, &uparam, &vparam,
                                      triangle, plane->org, dir, 0 );

            if (hit) {
#ifdef RI_BVH_TRACE_BEAM_STATISTICS
                g_beamstattrav.nrasterpixels++;
                if ( image[ t * width + s ] < tparam ) {
                    g_beamstattrav.noverdraws++;
                }
#endif
                plane->t[ t * width + s ] = tparam; 
            }
                
        }

    }

}
