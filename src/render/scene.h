/*
 *   lucille | Global Illumination render
 *
 *             written by Syoyo.
 *
 */

#ifndef LUCILLE_SCENE_H
#define LUCILLE_SCENE_H

#ifdef __cplusplus
extern "C" {
#endif

#include "ri.h"

#include "vector.h"
#include "list.h"
#include "hash.h"

#include "geom.h"
#include "light.h"
#include "accel.h"

/*
 * Struct: ri_scene_t
 *
 *     Structure for the scene data of a frame.
 *
 */
typedef struct _ri_scene_t
{
	ri_list_t      *geom_list;		/* geoms in the scene	*/
	ri_list_t      *light_list;		/* lights in the scene	*/

	/*
	 * background map(environment map)
	 */
	ri_texture_t   *background_map;

	/*
	 * Scene bounding box
	 */
	ri_vector_t     bmin;
	ri_vector_t     bmax;
	ri_float_t      maxwidth;

	/*
	 * Spatial data accelerator of the scene for raytracing
	 */
	ri_accel_t     *accel;

} ri_scene_t;

extern ri_scene_t *ri_scene_new       ();
extern void        ri_scene_free      (ri_scene_t *scene);
extern void        ri_scene_setup     (ri_scene_t *scene);
extern void        ri_scene_parse_geom(ri_scene_t *scene,	/* [inout] */
                                       ri_hash_t  *geom_drivers,
                                       const char *type,
                                       RtInt       nverts,
                                       RtInt       n,
                                       RtToken     tokens[],
                                       RtPointer   params[] );

extern void        ri_scene_add_geom  (ri_scene_t *scene,
                                       const ri_geom_t *geom );
extern void        ri_scene_add_light (ri_scene_t *scene,
                                       const ri_light_t *light );

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif	/* LUCILLE_SCENE_H */

