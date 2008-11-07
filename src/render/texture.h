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

#define MAPPING_ANGULAR_MAP
#define MAPPING_LATLONG

#define RI_MAX_MIPMAP_SIZE 16           /* Up to 65536x65536                */

typedef struct _ri_texture_t
{
    float         *data;                /* texel is strictly fp32 value.    */
    int            width, height; 
    int            maxsize_pow2n;       /* nearest 2^n value of
                                         * width or height.
                                         */
    int            mapping;             /* mapping method for IBL           */
} ri_texture_t;

typedef struct _ri_mipmap_t
{
    int         nlevels;

    int         width[RI_MAX_MIPMAP_SIZE];
    int         height[RI_MAX_MIPMAP_SIZE];

    float      *data[RI_MAX_MIPMAP_SIZE];

} ri_mipmap_t;

typedef struct _ri_rawtexture_t
{
    ri_float_t    *data;
    int            width, height; 
    int            maxsize_pow2n;       /* nearest 2^n value of
                                         * width or height.
                                         */
} ri_rawtexture_t;

extern ri_texture_t *ri_texture_load (const char *filename);

extern void          ri_texture_free (ri_texture_t *texture);

// TODO: LOD
extern void          ri_texture_fetch(ri_vector_t         color,   /* [out] */
                                      const ri_texture_t *texture,
                                      ri_float_t           u,
                                      ri_float_t           v);

extern void          ri_texture_ibl_fetch(
                                      ri_vector_t         color,
                                      const ri_texture_t *texture,
                                      const ri_vector_t   dir);

extern void          ri_texture_scale(ri_texture_t *texture,
                                      ri_float_t scale);

extern ri_mipmap_t  *ri_texture_make_mipmap(
                                      const ri_texture_t *texture);
                        

#ifdef __cplusplus
}    /* extern "C" */
#endif

#endif

