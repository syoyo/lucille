/*
 * texture mapping routine.
 *
 * texture.c contains texture filtering codes.
 * Loading texture data is coded in texture_loader.c.
 *
 * $Id$
 *
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>


#ifdef HAVE_ZLIB
#include <zlib.h>        /* Blocked texture is saved with zlib comporession. */
#endif

#include "memory.h"
#include "log.h"
#include "texture.h"
#include "hash.h"
#include "render.h"

#define TEXBLOCKSIZE 64        /* block map size in miplevel 0. */
#define MAXMIPLEVEL  16        /* 16 can represent a mipmap for 65536x65536. */

typedef struct _texblock_t
{
    int          x, y;        /* upper-left position in original texture map. 
                               * (in miplevel 0)
                               */

    ri_vector_t *image;

} texblock_t;

typedef struct _blockedmipmap_t
{
    int nmiplevels;             /* The number of mipmap levels              */

    int width, height;          /* Original texture size                    */

    int nxblocks, nyblocks;     /* The number of texture blocks.
                                 * Counted in miplevel 0.
                                 * Thus e.g. miplevel 1 contains
                                 * (nxblocks/2) * (nyblocks/2) texture blocks.
                                 */

    texblock_t *blocks[MAXMIPLEVEL];    /* list of texture blocks
                                         * in each miplevel.
                                         */

} blockedmipmap_t;

//#define LOCAL_DEBUG
#undef LOCAL_DEBUG
#define USE_ZORDER 0

#define MAX_TEXTURE_PATH 1024

/* input x and y must be up to 2^16 */
#define MAP_Z2D(x, y) ((unsigned int)(g_z_table[(x) & 0xFF] | \
               (g_z_table[(y) & 0xFF] << 1)) | \
               ((unsigned int)((g_z_table[((x) >> 8) & 0xFF]) | \
                (g_z_table[((y) >> 8) & 0xFF] << 1)) << 16))

/* table for z curve order */
static unsigned short g_z_table[256];

static ri_hash_t    *texture_cache;
static FILE         *open_file(const char *filename);

/* store texel memory in scanline order -> z curve order. */ 
static void build_z_table();


void
ri_texture_fetch(
    ri_vector_t         color_out,      /* [out] */
    const ri_texture_t *texture,
    ri_float_t          u,
    ri_float_t          v)
{
    int         i;
    int         idx;
    int         x, y;
    ri_float_t sx, sy;
    ri_float_t w[4];
    ri_float_t texel[4][4];
    ri_float_t px, py;
    ri_float_t dx, dy;

    sx = floor(u); sy = floor(v);

    u = u - sx; v = v - sy;

    if (u < 0.0) u = 0.0; if (u >= 1.0) u = 1.0;
    if (v < 0.0) v = 0.0; if (v >= 1.0) v = 1.0;

    px = u * (texture->width  - 1);
    py = v * (texture->height  - 1);

    x = (int)px; y = (int)py;

    dx = px - x; dy = py - y;

    w[0] = (1.0 - dx) * (1.0 - dy);
    w[1] = (1.0 - dx) *        dy ;
    w[2] =        dx  * (1.0 - dy);
    w[3] =        dx  *        dy ;

#if USE_ZORDER

    idx = MAP_Z2D(x, y);
    texel[0][0] = texture->data[4 * idx + 0];
    texel[0][1] = texture->data[4 * idx + 1];
    texel[0][2] = texture->data[4 * idx + 2];
    texel[0][3] = texture->data[4 * idx + 3];

    if (y < texture->height - 1 && x < texture->width - 1) {
        idx = MAP_Z2D(x, y+1);
        texel[1][0] = texture->data[4 * idx + 0];
        texel[1][1] = texture->data[4 * idx + 1];
        texel[1][2] = texture->data[4 * idx + 2];
        texel[1][3] = texture->data[4 * idx + 3];

        idx = MAP_Z2D(x+1, y);
        texel[2][0] = texture->data[4 * idx + 0];
        texel[2][1] = texture->data[4 * idx + 1];
        texel[2][2] = texture->data[4 * idx + 2];
        texel[2][3] = texture->data[4 * idx + 3];

        idx = MAP_Z2D(x+1, y+1);
        texel[3][0] = texture->data[4 * idx + 0];
        texel[3][1] = texture->data[4 * idx + 1];
        texel[3][2] = texture->data[4 * idx + 2];
        texel[3][3] = texture->data[4 * idx + 3];
    } else if (y < texture->height - 1) {
        idx = MAP_Z2D(x, y+1);
        texel[1][0] = texture->data[4 * idx + 0];
        texel[1][1] = texture->data[4 * idx + 1];
        texel[1][2] = texture->data[4 * idx + 2];
        texel[1][3] = texture->data[4 * idx + 3];

        for (i = 0; i < 4; i++) {
            texel[2][i] = 0.0;
            texel[3][i] = 0.0;
        }
    } else if (x < texture->width - 1) {
        idx = MAP_Z2D(x+1, y);
        texel[2][0] = texture->data[4 * idx + 0];
        texel[2][1] = texture->data[4 * idx + 1];
        texel[2][2] = texture->data[4 * idx + 2];
        texel[2][3] = texture->data[4 * idx + 3];

        for (i = 0; i < 4; i++) {
            texel[1][i] = 0.0;
            texel[3][i] = 0.0;
        }
    } else {
        for (i = 0; i < 4; i++) {
            texel[1][i] = 0.0;
            texel[2][i] = 0.0;
            texel[3][i] = 0.0;
        }
    }    

#else

    idx = y * texture->width + x;
    for (i = 0; i < 4; i++) {
        texel[0][i] = texture->data[4 * idx + i];
    }

    if (y < texture->height - 1 && x < texture->width - 1) {
        idx = (y + 1) * texture->width + x;
        for (i = 0; i < 4; i++) {
            texel[1][i] = texture->data[4 * idx + i];
        }

        idx = y * texture->width + x + 1;
        for (i = 0; i < 4; i++) {
            texel[2][i] = texture->data[4 * idx + i];
        }

        idx = (y + 1) * texture->width + x + 1;
        for (i = 0; i < 4; i++) {
            texel[3][i] = texture->data[4 * idx + i];
        }
    } else if (y < texture->height - 1) {
        idx = (y + 1) * texture->width + x;
        for (i = 0; i < 4; i++) {
            texel[1][i] = texture->data[4 * idx + i];
        }

        for (i = 0; i < 4; i++) {
            texel[2][i] = 0.0;
            texel[3][i] = 0.0;
        }
    } else if (x < texture->width - 1) {
        idx = y * texture->width + x + 1;
        for (i = 0; i < 4; i++) {
            texel[2][i] = texture->data[4 * idx + i];
        }

        for (i = 0; i < 4; i++) {
            texel[1][i] = 0.0;
            texel[3][i] = 0.0;
        }
    } else {
        for (i = 0; i < 4; i++) {
            texel[1][i] = 0.0;
            texel[2][i] = 0.0;
            texel[3][i] = 0.0;
        }
    }    

#endif
   
    for (i = 0; i < 4; i++) {
        color_out[i] = (ri_float_t)(
                  w[0] * texel[0][i] +
                  w[1] * texel[1][i] +
                  w[2] * texel[2][i] +
                  w[3] * texel[3][i]);
    }
}

void
ri_texture_ibl_fetch(
    ri_vector_t         color_out,
    const ri_texture_t *texture,
    const ri_vector_t   dir)
{
    const ri_float_t pi = 3.1415926535;
    ri_float_t u, v;
    ri_float_t r;
    ri_float_t norm2;
    ri_vector_t ndir;

    ri_vector_copy(ndir, dir);
    ri_vector_normalize(ndir);

    if (ndir[2] >= -1.0 && ndir[2] < 1.0) {
        /* Angular Map is defined in left-handed coordinate,
         * RenderMan is defined in right-handed coordinate(default).
         * +z in dir corresponds to -z in angular map coord.
         * (actually, no sign flipping occur in this case)
         * TODO: consider ribs which defined the coords in left-handed.
         */
        r = (1.0 / pi) * acos(ndir[2]);
    } else {
        r = 0.0;
    }

    norm2 = ndir[0] * ndir[0] + ndir[1] * ndir[1];
    if (norm2 > 1.0e-6) {
        r /= sqrt(norm2);
    }
    
    u = ndir[0] * r;
    v = ndir[1] * r;

    u = 0.5 * u + 0.5;
    v = 0.5 - 0.5 * v;

    ri_texture_fetch(color_out, texture, u, v);
}

void
ri_texture_scale(ri_texture_t *texture, ri_float_t scale)
{
    int i;

    for (i = 0; i < texture->width * texture->height * 4; i++) {
        texture->data[i] *= scale;
    }
}


static void
build_z_table()
{
    unsigned int   i, j;
    unsigned int   bit;
    unsigned int   v;
    unsigned int   ret;
    unsigned int   mask = 0x1;
    int            shift;

    for (i = 0; i < 256; i++) {
        v = i;
        shift = 0;
        ret = 0;
        for (j = 0; j < 8; j++) {
            /* extract (j+1)'th bit */
            bit = (v >> j) & mask;

            ret += bit << shift;
            shift += 2;
        }

        g_z_table[i] = (unsigned short)ret;
    }
}
