/*
 * texture data structure.
 *
 * $Id: texture.h,v 1.2 2004/02/08 16:38:48 syoyo Exp $
 */

#ifndef TEXTURE_H
#define TEXTURE_H

#ifdef __cplusplus
extern "C" {
#endif

#include "vector.h"

typedef struct _ri_texture_t
{
	float	*data;
	int      width, height; 
	int      maxsize_pow2n;		/* nearest 2^n value of
					 * width or height.
					 */
} ri_texture_t;

typedef struct _ri_rawtexture_t
{
	float	*data;
	int      width, height; 
	int      maxsize_pow2n;		/* nearest 2^n value of
					 * width or height.
					 */
} ri_rawtexture_t;

extern ri_texture_t *ri_texture_load(const char *filename);
extern void          ri_texture_free(ri_texture_t *texture);
extern void          ri_texture_fetch(ri_vector_t *color,
				      const ri_texture_t *texture,
				      double u, double v);
extern void          ri_texture_ibl_fetch(ri_vector_t *color,
				          const ri_texture_t *texture,
				          const ri_vector_t *dir);
extern void          ri_texture_scale(ri_texture_t *texture, float scale);
#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif

