#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "raster.h"

/* ----------------------------------------------------------------------------
 *
 * Private function definitions
 *
 * ------------------------------------------------------------------------- */
static int find_isect_pos_onto_the_triangle_plane(
    ri_vector_t   *points,      /* [out]    */
    ri_beam_t     *beam,
    ri_triangle_t *triangle);

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

    p->t     = NULL;
    p->u     = NULL;
    p->v     = NULL;
    p->geom  = NULL;
    p->index = NULL;

    return p;
}

int
ri_raster_plane_setup(
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

    if (plane_inout->t    ) ri_mem_free(plane_inout->t);
    if (plane_inout->u    ) ri_mem_free(plane_inout->u);
    if (plane_inout->v    ) ri_mem_free(plane_inout->v);
    if (plane_inout->geom ) ri_mem_free(plane_inout->geom);
    if (plane_inout->index) ri_mem_free(plane_inout->index);

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

    /*
     * Project lower-left point onto NDC coord. 
     */

    /*
     * p  = M F v
     * p' = p / p.z        - [0, 1)^2 
     *
     *      |     1                                |
     * M  = | ----------         0          0   0  |
     *      | tan(fov/2)                           |
     *      |                                      |
     *      |                    1                 |
     *      |      0         ----------     0   0  |
     *      |                tan(fov/2)            |
     *      |                                      |
     *      |      0             0          1   0  |
     *      |                                      |
     *      |      0             0         -1   0  |
     *
     *  F = |  du_x  du_y  du_z     0 |
     *      |  dv_x  dv_y  dv_z     0 |
     *      | -dw_x -dw_y -dw_z     0 |
     *      |     0     0     0     0 |
     *
     *  v = | x |
     *      | y |
     *      | z |
     *      | w |
     */

    {
        vec        vo;
        vec        w;
        vec        p;
        ri_float_t fov_rad  = plane_inout->fov * M_PI / 180.0;

        /* vo = v */
        vcpy( vo, plane_inout->corner );
            
        /* w = F v */
        w[0] =  plane_inout->frame[0][0] * vo[0]
             +  plane_inout->frame[0][1] * vo[1] 
             +  plane_inout->frame[0][2] * vo[2];
        w[1] =  plane_inout->frame[1][0] * vo[0] 
             +  plane_inout->frame[1][1] * vo[1] 
             +  plane_inout->frame[1][2] * vo[2];
        w[2] = -plane_inout->frame[2][0] * vo[0] 
             -  plane_inout->frame[2][1] * vo[1] 
             -  plane_inout->frame[2][2] * vo[2];

        /* p = M F v */
        p[0] = (1.0 / tan(0.5 * fov_rad)) * w[0];
        p[1] = (1.0 / tan(0.5 * fov_rad)) * w[1];
        p[2] = w[2];

        p[0] /= -w[2];       /* w = -z   */
        p[1] /= -w[2];

        printf("[raster] lowerleft = %f, %f\n", p[0], p[1]);
    }


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
     *      | w     1                        w      |
     * M  = | - ----------        0        - -   0  |
     *      | 2 tan(fov/2)                   2      |
     *      |                                       |
     *      |                h     1         h      |
     *      |      0         - ----------  - -   0  |
     *      |                2 tan(fov/2)    2      |
     *      |                                       |
     *      |      0              0          1   0  |
     *      |                                       |
     *      |      0              0         -1   0  |
     *
     *  F = |  du_x  du_y  du_z     0 |
     *      |  dv_x  dv_y  dv_z     0 |
     *      | -dw_x -dw_y -dw_z     0 |
     *      |     0     0     0     0 |
     *
     *  E = | 1  0  0 -org_x |
     *      | 0  1  0 -org_y |
     *      | 0  0  1 -org_z |
     *      | 0  0  0  1     |
     *
     *  v = | x |
     *      | y |
     *      | z |
     *      | w |
     */

    ri_vector_t vo;
    ri_vector_t w;
    ri_vector_t p[3];

    int width           = plane->width;
    int height          = plane->height;
    ri_float_t fov      = plane->fov;           /* in degree    */
    ri_float_t fov_rad  = fov * M_PI / 180.0;
    

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
        p[i][0] = 0.5 * width  * (1.0 / tan(0.5 * fov_rad)) * w[0]
                - 0.5 * width  * w[2];
        p[i][1] = 0.5 * height * (1.0 / tan(0.5 * fov_rad)) * w[1]
                - 0.5 * height * w[2];
        p[i][2] = w[2];

        printf("[raster] proj p = %f, %f, %f\n", p[i][0], p[i][1], p[i][2]);

        p[i][0] /= -w[2];       /* w = -z   */
        p[i][1] /= -w[2];

        printf("[raster] proj p/w = %f, %f\n", p[i][0], p[i][1]);

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

            tparam = RI_INFINITY;

            printf("dir = %f, %f, %f\n", dir[0], dir[1], dir[2]);

            hit = ri_triangle_isect( &tid, &tparam, &uparam, &vparam,
                                      triangle, plane->org, dir, 0 );

            if (hit) {
                printf("hit [%d, %d] t = %f\n", s, t, tparam);

#ifdef RI_BVH_TRACE_BEAM_STATISTICS
                g_beamstattrav.nrasterpixels++;
                if ( image[ t * width + s ] < tparam ) {
                    g_beamstattrav.noverdraws++;
                }
#endif
                plane->t[ t * width + s ] = tparam; 
            } else {
                printf("nohit[%d][%d]\n", s, t);
            }
        }
    }
}

/*
 * Rasterize beam into 2D raster plane.
 */
void
ri_rasterize_beam(
    ri_raster_plane_t *plane,
    ri_beam_t         *beam,
    ri_triangle_t     *triangle)
{
    int           i;
    int           s, t;
    int           n;

    ri_vector_t   points[4];            /* Points onto the plane defined by
                                           the triangle.  */

    ri_triangle_t triangles[2];         /* up to 2 triangles.   */

    //
    // Find hit points onto triangle plane.
    // 
    find_isect_pos_onto_the_triangle_plane(
        points,
        beam,
        triangle);

    if (beam->is_tetrahedron) {
        // Raster single triangle.
        
        vcpy(triangles[0].v[0], points[0]);
        vcpy(triangles[0].v[1], points[1]);
        vcpy(triangles[0].v[2], points[2]);
        n = 1;

    } else {
        // Raster 2 triangles.
        
        vcpy(triangles[0].v[0], points[0]);
        vcpy(triangles[0].v[1], points[1]);
        vcpy(triangles[0].v[2], points[2]);
        vcpy(triangles[1].v[0], points[0]);
        vcpy(triangles[1].v[1], points[2]);
        vcpy(triangles[1].v[2], points[3]);
        n = 2;

    }
        
    for (i = 0; i < n; i++) {

        ri_rasterize_triangle(plane, &triangles[i]);

    }

}

/*
 * Find a intersection point for each beam's corner ray onto the triangle
 * plane.
 * Points found may not lie within the triangle.
 */
int
find_isect_pos_onto_the_triangle_plane(
    ri_vector_t   *points,      /* [out]    */
    ri_beam_t     *beam,
    ri_triangle_t *triangle)
{
    int         i;
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

    vsub( s, beam->org, v0 );

    for (i = 0; i < 4; i++) {

        vcross( p, beam->dir[i], e2 );

        a = vdot( e1, p );

        if (fabs(a) > eps) {
            inva = 1.0 / a;
        } else {
            inva = 1.0;
        }

        vcross( q, s, e1 );

        t = vdot( e2, q ) * inva;    
        
        points[i][0] = beam->org[0] + t * beam->dir[i][0];
        points[i][1] = beam->org[1] + t * beam->dir[i][1];
        points[i][2] = beam->org[2] + t * beam->dir[i][2];

    }

    return 0;

}
