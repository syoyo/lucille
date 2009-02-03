/*
 *   lucille | Global Illumination renderer
 *
 *             written by Syoyo Fujita.
 *
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "vector.h"
#include "list.h"
#include "log.h"
#include "raytrace.h"
#include "util.h"
#include "accel.h"
#include "render.h"


void
ri_raytrace_setup()
{
    // build_z_table();
}

int
ri_raytrace(
    ri_render_t             *render,
    ri_ray_t                *ray,
    ri_intersection_state_t *state_out )
{
    int                     hit = 0;
    ri_intersection_state_t state;

    /*
     * Statistics
     */
    render->stat.nrays++;

    /*
     * Initialize
     */
    state.inside = 0;
    ray->t = 0.0;

    assert(render->scene);
    assert(render->scene->accel);
    assert(render->scene->accel->intersect);

    /*
     * Trace ray into the scene with spatial data structure for accelerated
     * raytracing.
     */
    hit = render->scene->accel->intersect(  render->scene->accel->data,
                                            ray,
                                           &state,
                                            NULL );

    if (hit) {
        memcpy( state_out, &state, sizeof( ri_intersection_state_t ) );
    }

    return hit;
}

void
ri_raytrace_statistics()
{
    uint64_t    ngridtravs;
    uint64_t    ntesttris;
    uint64_t    nrays;
    double      nmrays;

    double      elapsed = 0.0;

    elapsed = ri_timer_elapsed( ri_render_get()->context->timer,
                   "Render frame" );

    ngridtravs   = ri_render_get()->stat.ngridtravs;
    ntesttris    = ri_render_get()->stat.ntesttris;
    nrays        = ri_render_get()->stat.nrays;

    // Mega rays
    nmrays       = (double)nrays / (1000.0 * 1000.0);


    printf( "\n" );
    printf( "/= Raytracing statistics =================="
        "====================================\n" );
    printf( "| %-48s:  %20llu\n", "Total rays", nrays );
    printf( "| %-48s:  %20llu\n", "Total grid cell traversals", ngridtravs );
    printf( "| %-48s:  %20llu\n", "Total triangle tests", ntesttris );
    //printf("total mailboxing hits:  %20llu\n", nmailboxhits);
    printf( "| %-48s:  %20.6f\n", "The number of tests per ray",
           ( double )ntesttris / ( double )nrays );
    printf( "| %-48s:  %20.6f\n", "The number of travs per ray",
           ( double )ngridtravs / ( double )nrays );
    printf( "| %-48s:  %20.6f\n", "Render time(sec)",
           elapsed );
    printf( "| %-48s:  %20.6f\n", "M Rays/sec", nmrays /
           ( double )elapsed );
    printf(
        "\\------------------------------------------------------------------------------\n" );
    fflush( stdout );
}


/* ---------------------------------------------------------------------------
 *
 * Private functions
 *
 * ------------------------------------------------------------------------ */

