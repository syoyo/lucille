/*
 *   lucille | Global Illumination renderer
 *
 *             written by Syoyo Fujita.
 *
 */

/*
 * Beam tracing facility.
 *
 * $Id$
 */

#ifndef LUCILLE_BEAM_H
#define LUCILLE_BEAM_H

#ifdef __cplusplus
extern "C" {
#endif

#include "vector.h"
#include "geom.h"
#include "raster.h"

typedef struct _ri_triangle2d_t {

    ri_float_t  v[3][2];

    ri_geom_t  *geom;
    uint32_t    index;

} ri_triangle2d_t;


#if 0
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
#endif

/*
 * Beam is defined as a frustum
 * (Beam sharing its origin).
 */
typedef struct _ri_beam_t
{

    ri_vector_t     org;

    /*
     * P[i] - org, where P[i] is lied onto the axis-aligned plane.
     */
    ri_vector_t     dir[4];

    ri_vector_t     length;                     /* Side length(xyz)         */

    ri_float_t      d;                          /* distant to axis-aligned
                                                 * plane                    */

    //i_float_t      t_min;

    /* Farthest t value where intersection was found.  */
    ri_float_t      t_max;

    int             is_tetrahedron;            

    /*
     * Precomputed coefficients
     */
    ri_vector_t     invdir[4];
    int             dominant_axis;
    int             dirsign[3];                 /* xyz                      */
    ri_vector_t     normal[4];

    
    /*
     * Ptr to subdivided beam.
     * Beam is subdivived at the traversal phase.
     */
    struct _ri_beam_t *children;
    int                nchildren;


} ri_beam_t;


extern int  ri_beam_set (       ri_beam_t   *beam,      /* [inout]  */
                                ri_vector_t  org,
                                ri_vector_t  dir[4]);
extern void ri_beam_free(       ri_beam_t   *beam ); 

extern void ri_beam_copy(       ri_beam_t   *dst,
                          const ri_beam_t   *src );

/*
 * Caller should provide enough strage space for outer_out and inner_out.
 */
extern void ri_beam_clip_by_triangle2d(
                                ri_beam_t       *outer_out,     /* [out]    */
                                ri_beam_t       *inner_out,     /* [out]    */
                                int             *nouter_out,    /* [out]    */
                                int             *ninner_out,    /* [out]    */
                                ri_triangle2d_t *triangle2d,
                          const ri_beam_t       *beam);


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
#ifdef __cplusplus
}
#endif

#endif  /* LUCILLE_BEAM_H */
