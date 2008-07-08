#ifndef LUCILLE_RASTER_H
#define LUCILLE_RASTER_H

#ifdef __cplusplus
extern "C" {
#endif

#include "triangle.h"

typedef struct _ri_raster_plane_t
{

    ri_float_t  *t;             /* [width * height] */ 
    ri_float_t  *u;             /* [width * height] */  
    ri_float_t  *v;             /* [width * height] */ 
    ri_geom_t  **geom;          /* [width * height] */ 
    uint32_t    *index;         /* [width * height] */ 
    
    int          width;
    int          height;

    /*
     * Raster coord.
     * TODO: It is a good place to store raster coord here?
     */
    ri_vector_t   frame[3];     /* XYZ coordination frame               */
    ri_vector_t   corner;       /* Lower-left of raster plane in 3D.    */
    ri_vector_t   org;          /* eye origion                          */
    ri_float_t    fov;          /* Field of view                        */

} ri_raster_plane_t;

extern ri_raster_plane_t *ri_raster_plane_new();

extern int                ri_raster_plane_init(
                                ri_raster_plane_t *plane,
                                int                width,
                                int                height,
                                ri_vector_t        frame[3],
                                ri_vector_t        corner,
                                ri_vector_t        org,
                                ri_float_t         fov);
    
extern int                ri_raster_plane_free(
                                ri_raster_plane_t *plane);

extern void               ri_raster_triangle( 
                                ri_raster_plane_t *plane,
                                ri_triangle_t     *triangle);

#ifdef __cplusplus
}
#endif

#endif  /* LUCILLE_RASTER_H */
