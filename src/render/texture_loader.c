/*
 * texture data loader.
 *
 * TODO:
 * 
 * To handle large texture maps,
 * a imagemap(e.g. jpeg) is first converted into blocked,
 * mipmapped and hierarchical manner, and saved it to disk.
 * In rendering time, a portion(block) of texture map to be texture-mapped
 * is load into memory from file.
 *
 *
 *  +-----------------+         +-----+-----+-----+
 *  |                 |         |     |     |     |
 *  |                 |         |     |     |     |
 *  |  input texture  |      -> +-----+-----+-----+ 
 *  |                 |         |     |     |     |
 *  |                 |         |     |     |     |
 *  +-----------------+         +-----+-----+-----+
 *                                          <----->
 *                                           TEXBLOCKSIZE
 *
 *
 * -> Generate mipmaps and store it to disk as a 1D array manner.
 *
 * +-----------------------+---------------+------------+--||--+ 
 * | miplevel 0 blocks     | lv 1 blks     | lv 2 blks  |      |
 * +-----------------------+---------------+------------+--||--+
 *
 *
 *  o Use rip-map for anisotropic texturing.
 *
 *
 * $Id: texture.c,v 1.9 2004/08/15 05:19:39 syoyo Exp $
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
#include <assert.h>


#ifdef HAVE_ZLIB
#include <zlib.h>        /* Blocked texture is saved with zlib comporession. */
#endif

#include "memory.h"
#include "log.h"
#include "texture.h"
#include "hash.h"
#include "render.h"
#include "image_loader.h"       /* ../imageo                                */

#define TEXBLOCKSIZE 64         /* block map size in miplevel 0. */
#define MAXMIPLEVEL  16         /* 16 can represent a mipmap for 65536x65536. */

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
#undef  LOCAL_DEBUG
#define USE_ZORDER 0

#define MAX_TEXTURE_PATH 1024

static ri_hash_t    *texture_cache;

/* input x and y must be up to 2^16 */
#define MAP_Z2D(x, y) ((unsigned int)(g_z_table[(x) & 0xFF] | \
               (g_z_table[(y) & 0xFF] << 1)) | \
               ((unsigned int)((g_z_table[((x) >> 8) & 0xFF]) | \
                (g_z_table[((y) >> 8) & 0xFF] << 1)) << 16))

#if USE_ZORDER
/* table for z curve order */
static unsigned short g_z_table[256];

/* store texel memory in scanline order -> z curve order. */ 
static void          remapping(ri_texture_t *src);
static void          build_z_table();
#endif  /* USE_ZORDER */


#if 0 /* REMOVE */
static void endconv(void *data)
{
#if defined(WORDS_BIGENDIAN) || defined(__APPLE__)
    char tmp[4];
    tmp[0] = ((char *)data)[0];
    tmp[1] = ((char *)data)[1];
    tmp[2] = ((char *)data)[2];
    tmp[3] = ((char *)data)[3];

    ((char *)data)[0] = tmp[3];
    ((char *)data)[1] = tmp[2];
    ((char *)data)[2] = tmp[1];
    ((char *)data)[3] = tmp[0];
#else
    (void)data;
#endif
}
#endif

ri_texture_t *
ri_texture_load(const char *filename)
{
    static int      initialized = 0;
    ri_texture_t   *p;
    char            fullpath[4096];

    if (!initialized) {
        texture_cache = ri_hash_new();
#if USE_ZORDER
        build_z_table();
#endif
        initialized = 1;
    }

    if (ri_hash_lookup(texture_cache, filename)) {
        /* hit texture cache! */
        p = (ri_texture_t *)ri_hash_lookup(texture_cache, filename);

        return p;
    }

    if (!ri_option_find_file(fullpath,
                             ri_render_get()->context->option,
                             filename)) {
        ri_log(LOG_FATAL, "Can't find textue file \"%s\"", filename);
        ri_log(LOG_FATAL, "Searched following pathes.");
        ri_option_show_searchpath(ri_render_get()->context->option);
        exit(-1);
    }
    
    {
        unsigned int  width;
        unsigned int  height;
        unsigned int  component;
        float        *image = NULL;

        image = ri_image_load(fullpath, &width, &height, &component);
        if (!image) {
            ri_log(LOG_WARN, "Can't load textue file \"%s\"", fullpath);
            exit(-1);
        }    

        p = ri_mem_alloc(sizeof(ri_texture_t));
        assert(p != NULL);
        memset(p, 0, sizeof(ri_texture_t));
        
        p->width  = width;
        p->height = height;
        p->data   = image;

        ri_log(LOG_INFO, "Loaded texture [ %s ] size = %d x %d", fullpath, width, height);
    }


#if USE_ZORDER
    /* reorder texel memory with z curve order to exploit cache coherency. */
    remapping(p);
#endif

    /* add to texture cache */
    ri_hash_insert(texture_cache, filename, p);

    return p;
}

void
ri_texture_free(ri_texture_t *texture)
{
    ri_mem_free(texture->data);
    ri_mem_free(texture);
}


#if USE_ZORDER

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

static void
remapping(ri_texture_t *src)
{
    unsigned int    x, y;
    unsigned int    i;
    unsigned int    pow2n;
    unsigned int    maxpixlen;
    unsigned int    idx;
    float          *newdata;

    if (src->width > src->height) {
        maxpixlen = src->width;
    } else {
        maxpixlen = src->height;
    } 

    /* find nearest 2^n value against maxpixlen.
     * I think there exists more sofisticated way for this porpose ...
     */
    pow2n = 1;
    for (i = 1; i < maxpixlen; i++) {
        if (maxpixlen <= pow2n) break;
        pow2n *= 2;
    }
    
    src->maxsize_pow2n = pow2n;

#ifdef LOCAL_DEBUG
    fprintf(stderr, "width = %d, height = %d\n", src->width, src->height);
    fprintf(stderr, "maxsize_pow2n = %d\n", src->maxsize_pow2n);
#endif

    newdata = (float *)ri_mem_alloc(4 * sizeof(float) *
                    src->maxsize_pow2n *
                    src->maxsize_pow2n);

    if (src->maxsize_pow2n > 65536) {
        fprintf(stderr,
            "texture size must be < 65536. maxsize_pow2n = %d\n",
            src->maxsize_pow2n);
        exit(-1);
    }

    for (i = 0;
         i < (unsigned int)(4 * src->maxsize_pow2n * src->maxsize_pow2n);
         i++) {
        newdata[i] = 0.0;
    }

    for (y = 0; y < (unsigned int)src->height; y++) {
        for (x = 0; x < (unsigned int)src->width; x++) {
            /* compute z curve order index for (x, y). */
            idx = MAP_Z2D(x, y);
#ifdef LOCAL_DEBUG
            fprintf(stderr, "(%d, %d) = %d\n", x, y, idx);
#endif

            newdata[4 * idx + 0] =
                src->data[4 *(y * src->width + x) + 0];
            newdata[4 * idx + 1] =
                src->data[4 *(y * src->width + x) + 1];
            newdata[4 * idx + 2] =
                src->data[4 *(y * src->width + x) + 2];
            newdata[4 * idx + 3] =
                src->data[4 *(y * src->width + x) + 3];
        }
    }    

    ri_mem_free(src->data);

    src->data = newdata;
}
#endif  /* USE_ZORDER */

void
make_texture(ri_rawtexture_t *rawtex)
{
    int s, t;
    int i;
    int blocksize = 0;

    int nxblocks = 0;    // Number of blocks in x-direction
    int nyblocks = 0;    // Number of blocks in y-direction

    ri_vector_t *image[MAXMIPLEVEL];    // block map images.
    
    for (i = 0; i < blocksize; i++) {
        blocksize = TEXBLOCKSIZE >> i;
        image[i] = (ri_vector_t *)ri_mem_alloc(sizeof(ri_vector_t) *
                blocksize * blocksize);
    }

    nxblocks = (int)ceil(rawtex->width / TEXBLOCKSIZE);
    nyblocks = (int)ceil(rawtex->height / TEXBLOCKSIZE);

    printf("w, h = %d, %d. nxblk, nyblk = %d, %d\n",
        rawtex->width, rawtex->height, nxblocks, nyblocks);

    for (t = 0; t < nyblocks; t++) {
        for (s = 0; s < nxblocks; s++) {

        }
    }
    
    
}

#if 0   // TODO
// Generate blocked mipmap from raw texture map.
// TODO: implement.
static blockedmipmap_t *
gen_blockedmipmap(const char *filename, ri_rawtexture_t *texture)
{
    int i;
    int w, h;
    int xblocks, yblocks;
    int miplevels;
    int minsize;
    int u, v;

    w = texture->width; h = texture->height;

    xblocks = (int)ceil(w / TEXBLOCKSIZE);
    yblocks = (int)ceil(h / TEXBLOCKSIZE);

    ri_log(LOG_INFO, "texsize = (%d, %d). blocks = (%d, %d)\n",
        w, h, xblocks, yblocks);

    minsize = (w < h) ? w : h;
    miplevels = (int)ceil(log((ri_float_t)minsize) / log(2.0));

    ri_log(LOG_INFO, "miplevels = %d\n", miplevels);

    for (i = 0; i < miplevels; i++) {

        for (v = 0; v < yblocks; v++) {
            for (u = 0; u < xblocks; u++) {
                
            }
        }

    }
    
    return NULL;    // not yet implemented.
}

// Write mipmap to disk with zlib compression.
static void
write_blockedmipmap(const char *filename, blockedmipmap_t *blkmipmap)
{
    int i, j;
    int u, v;
    int size;
    int xblocks, yblocks;

    gzFile *fp;
    
    fp = gzopen(filename, "wb");
    if (!fp) {
        
        ri_log(LOG_ERROR, "Can't write file [%s]", filename);
    }

    // Write header
    gzwrite(fp, &blkmipmap->nmiplevels, sizeof(int));
    gzwrite(fp, &blkmipmap->width, sizeof(int));
    gzwrite(fp, &blkmipmap->height, sizeof(int));
    gzwrite(fp, &blkmipmap->nxblocks, sizeof(int));
    gzwrite(fp, &blkmipmap->nyblocks, sizeof(int));
    
    size = sizeof(ri_float_t) * TEXBLOCKSIZE * TEXBLOCKSIZE;
    
    for (i = 0; i < blkmipmap->nmiplevels; i++) {

        xblocks = blkmipmap->nxblocks >> i;
        yblocks = blkmipmap->nyblocks >> i;

        for (v = 0; v < yblocks; v++) {
            for (u = 0; u < xblocks; u++) {

                j = v * xblocks + u;

                // Write texture block.
                gzwrite(fp, blkmipmap->blocks[i][j].image,
                    size);

            }
        }

    }

    gzclose(fp);
}
#endif
