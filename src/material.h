/*
 * material routines
 *
 * $Id: material.h,v 1.2 2004/05/04 02:27:31 syoyo Exp $
 */

#ifndef MATERIAL_H
#define MATERIAL_H

#include <stdarg.h>
#include <math.h>

#include "ri.h"
#include "vector.h"
#include "texture.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _ri_material_t
{
	ri_vector_t kd;		/* diffuse reflectance[0,1]	*/
	ri_vector_t ks;		/* specular reflectance[0,1]	*/
	ri_vector_t kt;		/* transmittance [0,1]		*/
	float       ior;	/* relative Index Of Refraction	*/
	int         fresnel;	/* do fresnel reflection?	*/

	ri_texture_t *texture;
} ri_material_t;

extern ri_material_t *ri_material_new();
extern void           ri_material_free(ri_material_t *material);

extern void           ri_material_copy(ri_material_t *dst,
				       const ri_material_t *src);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif

