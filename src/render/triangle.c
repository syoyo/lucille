#include <stdio.h>
#include <stdlib.h>

#include "triangle.h"
#include "beam.h"

int
ri_triangle_isect(
          uint32_t      *tid_inout,
          ri_float_t    *t_inout,
          ri_float_t    *u_inout,
          ri_float_t    *v_inout,
    const ri_triangle_t *triangle,
          ri_vector_t    rayorg,
          ri_vector_t    raydir,
          uint32_t       tid)
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
        return 0;   /* the ray is parallel to the triangle. */
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

    if ( (t < eps) || (t > (*t_inout)) ) {
        return 0;
    }

    (*t_inout)   = t;
    (*u_inout)   = u;
    (*v_inout)   = v;
    (*tid_inout) = tid;

    return 1;   /* hit */

}
