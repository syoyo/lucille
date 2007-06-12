#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "scene.h"
#include "memory.h"
#include "list.h"
#include "hash.h"
#include "log.h"

#include "geom.h"

static void calc_scene_bbox(const ri_list_t *geom_list,
                            ri_vector_t     *bmin,
                            ri_vector_t     *bmax,
                            ri_float_t      *maxwidth);

ri_scene_t *
ri_scene_new()
{
        ri_scene_t *p;

        p = ( ri_scene_t * )ri_mem_alloc( sizeof( ri_scene_t ) );

        p->geom_list  = ri_list_new();
        p->light_list = ri_list_new();

        return p;
}

void
ri_scene_free( ri_scene_t * scene )
{
	ri_list_free( scene->geom_list );
	ri_list_free( scene->light_list );

        ri_mem_free( scene );
}

void
ri_scene_setup( ri_scene_t * scene )
{
	calc_scene_bbox(
		scene->geom_list,
		&scene->bmin,
		&scene->bmax,
		&scene->maxwidth );

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

static void
calc_scene_bbox(
	const ri_list_t *geom_list,
	ri_vector_t     *bmin,
	ri_vector_t     *bmax,
	ri_float_t      *maxwidth )
{
        unsigned int    i;
        ri_vector_t     v;
        ri_list_t      *itr;
        ri_geom_t      *geom;

        bmin->f[0] = bmin->f[1] = bmin->f[2] = RI_INFINITY;
        bmax->f[0] = bmax->f[1] = bmax->f[2] = -RI_INFINITY;

        for ( itr = ri_list_first( (ri_list_t *)geom_list );
              itr != NULL;
	      itr = ri_list_next( itr ) ) {

                geom = ( ri_geom_t * ) itr->data;

                for ( i = 0; i < geom->npositions; i++ ) {

                        v = geom->positions[i];

                        if ( bmin->f[0] > v.f[0] ) bmin->f[0] = v.f[0];
                        if ( bmin->f[1] > v.f[1] ) bmin->f[1] = v.f[1];
                        if ( bmin->f[2] > v.f[2] ) bmin->f[2] = v.f[2];

                        if ( bmax->f[0] < v.f[0] ) bmax->f[0] = v.f[0];
                        if ( bmax->f[1] < v.f[1] ) bmax->f[1] = v.f[1];
                        if ( bmax->f[2] < v.f[2] ) bmax->f[2] = v.f[2];
                }
        }

        ( *maxwidth ) = bmax->f[0] - bmin->f[0];

        if ( ( *maxwidth ) < ( bmax->f[1] - bmin->f[1] ) ) {
                ( *maxwidth ) = bmax->f[1] - bmin->f[1];
	}

        if ( ( *maxwidth ) < ( bmax->f[2] - bmin->f[2] ) ) {
                ( *maxwidth ) = bmax->f[2] - bmin->f[2];
	}
}
