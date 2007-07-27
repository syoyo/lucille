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
 * Acceleration methods
 */
#define RI_ACCEL_UGRID          0
#define RI_ACCEL_BVH            1
//#define RI_ACCEL_BIH           2		// TODO


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

	void *( *build     )();
	void  ( *free      )(void                    *accel);
	int   ( *intersect )(void                    *accel,   /* [in]    */
                             ri_ray_t                *ray,     /* [in]    */
			     ri_intersection_state_t *state);  /* [inout] */
	
	/*
	 * Members
	 */

	void *accel;		/* spatial data structure */

} ri_accel_t;

extern ri_accel_t *ri_accel_new();
extern void        ri_accel_free(ri_accel_t *accel);
extern int         ri_accel_bind(ri_accel_t *accel,
                                 int         method);


#ifdef __cplusplus
}	/* extern "C" */
#endif


#endif
