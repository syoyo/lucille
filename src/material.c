/*
 * $Id: material.c,v 1.2 2004/05/04 02:27:31 syoyo Exp $
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "material.h"

ri_material_t *
ri_material_new()
{
	ri_material_t *material = NULL;

	material = (ri_material_t *)ri_mem_alloc(sizeof(ri_material_t));

	/*
	 * Set default value.
	 */
	material->kd.e[0] = 1.0;
	material->kd.e[1] = 1.0;
	material->kd.e[2] = 1.0;

	material->ks.e[0] = 0.0;
	material->ks.e[1] = 0.0;
	material->ks.e[2] = 0.0;

	material->kt.e[0] = 0.0;
	material->kt.e[1] = 0.0;
	material->kt.e[2] = 0.0;

	material->ior     = 1.0;		/* air */

	material->fresnel = 0;

	material->texture = NULL;

	return material;
}

void
ri_material_free(ri_material_t *material)
{
	ri_texture_free(material->texture);
	ri_mem_free(material);
}

void
ri_material_copy(ri_material_t *dst, const ri_material_t *src)
{
	memcpy((void *)dst, (const void *)src, sizeof(ri_material_t));
}

