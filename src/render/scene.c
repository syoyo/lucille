#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <assert.h>

#include "scene.h"
#include "memory.h"
#include "list.h"
#include "hash.h"
#include "log.h"
#include "geom.h"
#include "render.h"

/*
 * Predefined accelerator.
 */
#include "ugrid.h"
#include "bvh.h"

static void calc_scene_bbox(const ri_list_t *geom_list,
                            ri_vector_t      bmin,
                            ri_vector_t      bmax,
                            ri_float_t      *maxwidth);

/* ---------------------------------------------------------------------------
 *
 * Public functions
 *
 * ------------------------------------------------------------------------ */

ri_scene_t *
ri_scene_new()
{
    ri_scene_t *p;

    p = ( ri_scene_t * )ri_mem_alloc( sizeof( ri_scene_t ) );

    p->geom_list  = ri_list_new();
    p->light_list = ri_list_new();

    p->accel      = NULL;

    return p;
}

void
ri_scene_free( ri_scene_t * scene )
{
    
    ri_log_and_return_if(scene == NULL);

    ri_list_free( scene->geom_list );
    ri_list_free( scene->light_list );

    //assert(scene->accel);
    //assert(scene->accel->free);
    //assert(scene->accel->accel);

    // if (scene->accel->free) {
    //     if (scene->accel->accel) {
    //         scene->accel->free(scene->accel->accel);
    //     }
    // }

    ri_accel_free(scene->accel);

    ri_mem_free( scene );
}

/*
 * Function: ri_scene_setup
 *
 *     Setups scene data and builds spatial data structure for the scene.
 *
 */
void
ri_scene_setup( ri_scene_t * scene )
{
    calc_scene_bbox(
        scene->geom_list,
        scene->bmin,
        scene->bmax,
        &scene->maxwidth );

    ri_accel_bind( scene->accel,    
                   ri_render_get()->context->option->accel_method);

    scene->accel->data = scene->accel->build((const void *)scene);

}

void
ri_scene_parse_geom(
    ri_scene_t *scene,
    ri_hash_t  *geom_drivers,
    const char *type,
    RtInt       nverts,
    RtInt       n,
    RtToken     tokens[],
    RtPointer   params[] )
{
    ri_geom_drv_t  *drv = NULL;

    drv = ( ri_geom_drv_t * ) ri_hash_lookup( geom_drivers, type );

    ri_log_and_return_if( NULL == drv );

    ri_scene_add_geom( scene, drv->parse( nverts, n, tokens, params ) );
}

void
ri_scene_add_geom( ri_scene_t *scene, const ri_geom_t * geom )
{
    ri_list_append( scene->geom_list, ( void * ) geom );
}

void
ri_scene_add_light( ri_scene_t *scene, const ri_light_t * light )
{
    ri_list_append( scene->light_list, ( void * ) light );
}

int
ri_scene_set_accel(
    ri_scene_t *scene,
    ri_accel_t *accel)
{ 
    assert( scene != NULL );
    assert( accel != NULL );

    scene->accel = accel;

    return 0;   /* OK */

}

int
ri_scene_build_accel(
    ri_scene_t *scene)
{ 
    assert( scene != NULL);

    if (!scene->accel->build) {
        ri_log( LOG_FATAL, "No spatial accelerator is assigned to the scene.");
        return -1;
    }

    scene->accel->data = scene->accel->build( (const void *)scene );

    return 0;   /* OK */
}

/* ---------------------------------------------------------------------------
 *
 * Private functions
 *
 * ------------------------------------------------------------------------ */

static void
calc_scene_bbox(
    const ri_list_t *geom_list,
    ri_vector_t      bmin,
    ri_vector_t      bmax,
    ri_float_t      *maxwidth )
{
    unsigned int    i;
    ri_vector_t     v;
    ri_list_t      *itr;
    ri_geom_t      *geom;

    bmin[0] = bmin[1] = bmin[2] = RI_INFINITY;
    bmax[0] = bmax[1] = bmax[2] = -RI_INFINITY;

    for ( itr = ri_list_first( (ri_list_t *)geom_list );
          itr != NULL;
          itr = ri_list_next( itr ) ) {

        geom = ( ri_geom_t * ) itr->data;

        for ( i = 0; i < geom->npositions; i++ ) {

            ri_vector_copy(v, geom->positions[i]);

            if ( bmin[0] > v[0] ) bmin[0] = v[0];
            if ( bmin[1] > v[1] ) bmin[1] = v[1];
            if ( bmin[2] > v[2] ) bmin[2] = v[2];

            if ( bmax[0] < v[0] ) bmax[0] = v[0];
            if ( bmax[1] < v[1] ) bmax[1] = v[1];
            if ( bmax[2] < v[2] ) bmax[2] = v[2];

        }
    }

    ( *maxwidth ) = bmax[0] - bmin[0];

    if ( ( *maxwidth ) < ( bmax[1] - bmin[1] ) ) {
            ( *maxwidth ) = bmax[1] - bmin[1];
    }

    if ( ( *maxwidth ) < ( bmax[2] - bmin[2] ) ) {
            ( *maxwidth ) = bmax[2] - bmin[2];
    }
}
