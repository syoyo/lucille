/*
 *   lucille | Global Illumination renderer
 *
 *             written by Syoyo Fujita.
 *
 */

/*
 * Triangle rasterizer for beam tracing.
 *
 * $Id: raster.h 259 2008-08-01 16:28:06Z syoyo $
 *
 */
#ifndef LUCILLE_RASTER_H
#define LUCILLE_RASTER_H

#ifdef __cplusplus
extern "C" {
#endif

#include "triangle.h"
#include "beam.h"

typedef struct _ri_raster_plane_t
{

    ri_float_t   *t;             /* [width * height] */ 
    ri_float_t   *u;             /* [width * height] */  
    ri_float_t   *v;             /* [width * height] */ 
    ri_geom_t   **geom;          /* [width * height] */ 
    uint32_t     *index;         /* [width * height] */ 
   
    int           width;
    int           height;

    /*
     * Raster coord.
     * TODO: It is a good place to store raster coord here?
     */
    ri_vector_t   frame[3];     /* XYZ coordination frame               */
    ri_vector_t   corner;       /* Lower-left of raster plane in 3D.    */
    ri_vector_t   org;          /* eye origion                          */
    ri_float_t    fov;          /* Field of view                        */

    /*
     * Local coord.
     */
    ri_float_t   offset[2];     /* Defines lower-left offset defined in
                                 * raster 2D plane                      */ 

    ri_float_t   scale[2];      /* scaling factor                       */

} ri_raster_plane_t;

/* Allocates a memory for a raster plane object */
extern ri_raster_plane_t *ri_raster_plane_new();

/* Initializes the raster plane with arguments */
extern int                ri_raster_plane_setup(
                                ri_raster_plane_t *plane,
                                int                width,
                                int                height,
                                ri_vector_t        frame[3],
                                ri_vector_t        corner,
                                ri_vector_t        org,
                                ri_float_t         fov);
    
/* Frees a memory of the raster plane */
extern int                ri_raster_plane_free(
                                ri_raster_plane_t *plane);

/* Rasterizes a triangle onto the raster plane. */
extern void               ri_rasterize_triangle( 
                                ri_raster_plane_t *plane,
                                ri_triangle_t     *triangle);

/* Rasterizes a beam onto the raster plane. */
extern void               ri_rasterize_beam( 
                                ri_raster_plane_t *plane,
                                ri_beam_t         *beam,
                                ri_triangle_t     *triangle);
#ifdef __cplusplus
}
#endif

#endif  /* LUCILLE_RASTER_H */
