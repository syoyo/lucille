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

#include "memory.h"
#include "log.h"
#include "accel.h"

#include "ugrid.h"
#include "bvh.h"


/* ---------------------------------------------------------------------------
 *
 * Public functions
 *
 * ------------------------------------------------------------------------ */

ri_accel_t *
ri_accel_new()
{
    ri_accel_t *p;

    p = ri_mem_alloc(sizeof(ri_accel_t));

    memset(p, 0, sizeof(ri_accel_t));
    
    return p;
}

void
ri_accel_free(
    ri_accel_t *accel)
{
    if (accel) {

        if (accel->free) {
            
            /* Free spatial data content */
            accel->free( accel->data );

        }

        ri_mem_free(accel);

    }
}

/*
 *  Function: ri_accel_bind()
 *
 *      Binds an implementation specified by `method' to the interface `accel'.
 *
 *  Parameters:
 *
 *      *accel  - Accelerator interface to bind.
 *       method - The algorithm of spatial data structure.
 *
 *  Returns:
 *
 *      0 if success, -1 not.
 */
int
ri_accel_bind(
    ri_accel_t *accel,
    int         method)
{

    assert(accel != NULL);

    switch( method ) {

        case RI_ACCEL_UGRID:

            ri_log(LOG_DEBUG, "Use ugrid");

            accel->build     = ri_ugrid_build;
            accel->free      = ri_ugrid_free;
            accel->intersect = ri_ugrid_intersect;

            break;

        case RI_ACCEL_BVH:

            ri_log(LOG_DEBUG, "Use BVH");

            accel->build     = ri_bvh_build;
            accel->free      = ri_bvh_free;
            accel->intersect = ri_bvh_intersect;

            break;

        default:

            ri_log(LOG_ERROR, "Unknown accel method");
            return -1;
    }

    return 0;
}
