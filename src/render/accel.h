/*
 * data structure for accelarating ray-polygon intersection.
 *
 * $Id: accel.h,v 1.3 2004/04/16 13:46:45 syoyo Exp $
 */

#ifndef ACCEL_H
#define ACCEL_H

#ifdef __cplusplus
extern "C" {
#endif

#include "ray.h"
#include "intersection_state.h"

/*
 * Predefined acceleration methods
 */
#define RI_ACCEL_UGRID          0
#define RI_ACCEL_BVH            1


typedef void *( *accel_build_func )
              ( const void              *data);

typedef void  ( *accel_free_func )
              ( void                    *accel);

typedef int   ( *accel_intersect_func )
              ( void                    *accel,
                ri_ray_t                *ray,           /* [in]         */
                ri_intersection_state_t *state,         /* [inout]      */
                void                    *user );        /* user data    */



/*
 * Struct: ri_accel_t
 *
 *     Interface for spatial data accelerator
 *
 */
typedef struct _ri_accel_t
{
    /*
     * Methods
     */

    /*
     * Scene data and construction parameters are grabbed from g_render,
     * thus no arg for build().
     */

    accel_build_func     build;
    accel_free_func      free;
    accel_intersect_func intersect;

#if 0
    void *( *build     )(const void              *data);
    void  ( *free      )(void                    *accel);
    int   ( *intersect )(void                    *accel,   /* [in]    */
                             ri_ray_t                *ray,     /* [in]    */
                 ri_intersection_state_t *state);  /* [inout] */
#endif
    
    /*
     * Members
     */

    void *data;        /* spatial data structure */

} ri_accel_t;

extern ri_accel_t *ri_accel_new();
extern void        ri_accel_free(ri_accel_t *accel);

/*
 * Bind accelerator with predefined accelerator method.
 */
extern int         ri_accel_bind(ri_accel_t *accel,
                                 int         method);


#ifdef __cplusplus
}    /* extern "C" */
#endif


#endif
