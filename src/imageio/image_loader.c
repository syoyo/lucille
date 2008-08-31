/*
 *   lucille | Global Illumination renderer
 *
 *             written by Syoyo Fujita.
 *
 */

/*
 * Image loader routine. Image format is determined by seeing file extension.
 *
 * $Id$
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "image_loader.h"

#include "log.h"
#include "memory.h"

#include "rgbe.h"
#ifdef WITH_JPEGLIB
#include "jpeg.h"
#endif

/* ---------------------------------------------------------------------------
 *
 * Private function definitions
 *
 * ------------------------------------------------------------------------ */

static int         casecmp(const char *s1, const char *s2);
static void        endconv4(void *data);

static float      *load_tex(FILE            *fp,
                            unsigned int    *width_out,
                            unsigned int    *height_out,
                            unsigned int    *component_out);

static float      *load_hdr(FILE            *fp,
                            unsigned int    *width_out,
                            unsigned int    *height_out,
                            unsigned int    *component_out);

#ifdef HAVE_LIBJPEG
static float      *load_jpeg(FILE           *fp,
                            unsigned int    *width_out,
                            unsigned int    *height_out,
                            unsigned int    *component_out);
#endif


/* --------------------------------------------------------------------------
 *
 * Public functions
 *
 * ----------------------------------------------------------------------- */

/*
 * Function: ri_image_load
 *
 *   Loads image from `filename'. File format is determined by string of file
 *   extension.
 */
float *
ri_image_load(
    const char      *filename,
    unsigned int    *width_out,
    unsigned int    *height_out,
    unsigned int    *component_out)
{
    FILE        *fp;
    char        *ext;
    float       *image;

    fp = fopen(filename, "rb");
    if (!fp) {
        ri_log(LOG_WARN, "Can't open textue file \"%s\"", filename);
        exit(-1);
    }

    ext = strrchr(filename, '.');           /* get file extension string */
    if (casecmp(ext, ".hdr") == 0) {        /* Ward's .hdr */
        image = load_hdr(fp, width_out, height_out, component_out);
    } else if (casecmp(ext, ".tex") == 0) { /* native .tex */
        image = load_tex(fp, width_out, height_out, component_out);
#ifdef WITH_LIBJPEG
    } else if (casecmp(ext, ".jpg")  == 0 ||
           casecmp(ext, ".jpeg") == 0) {
        p = load_jpeg(fp, width_out, height_out, component_out);
#endif
    } else {
        ri_log(LOG_WARN, "Can't understand format of image \"%s\". extension = \"%s\"", filename, ext);
        exit(-1);
    }

    return image;
    
}



float *
load_tex(
    FILE            *fp,
    unsigned int    *width_out,
    unsigned int    *height_out,
    unsigned int    *component_out)
{
    int             i;
    int             w, h;
    float          *data;

    /* texture data is stored in little-endian */
    fread(&w, sizeof(int), 1, fp); endconv4(&w); 
    fread(&h, sizeof(int), 1, fp); endconv4(&h);

    data = (float *)ri_mem_alloc(sizeof(float) * w * h * 4);
    fread(data, sizeof(float), w * h * 4, fp);

    for (i = 0; i < w * h * 4; i++) {
        endconv4(&(data[i]));
        if (data[i] < 0.0) data[i] = 0.0;
        /* TODO: Add NaN & Inf handler here */ 
    }

    (*width_out)     = w;
    (*height_out)    = h;
    (*component_out) = 4;

    return data;
}

float *
load_hdr(
    FILE            *fp,
    unsigned int    *width_out,
    unsigned int    *height_out,
    unsigned int    *component_out)
{
    int               ret;
    int               i;
    int               width, height;
    rgbe_header_info  info;
    float            *img;
    float            *data;

    ret = RGBE_ReadHeader(fp, &width, &height, &info);
    if (ret != RGBE_RETURN_SUCCESS) {
        fprintf(stderr, "can't read .hdr header\n");
        exit(-1);
    }

    img = (float *)ri_mem_alloc(sizeof(float) * width * height * 3);

    ret = RGBE_ReadPixels_RLE(fp, img, width, height);
    if (ret != RGBE_RETURN_SUCCESS) {
        fprintf(stderr, "can't read .hdr content\n");
        exit(-1);
    }

    data = (float *)ri_mem_alloc(sizeof(float) * width * height * 4);

    /* RGB -> RGBA conversion */
    for (i = 0; i < width * height; i++) {
        data[4 * i + 0] = img[3 * i + 0];
        data[4 * i + 1] = img[3 * i + 1];
        data[4 * i + 2] = img[3 * i + 2];
        data[4 * i + 3] = 1.0;
    }

    (*width_out)     = width;
    (*height_out)    = height;
    (*component_out) = 4;

    ri_mem_free(img);

    return data;
}


/* --------------------------------------------------------------------------
 *
 * Private functions
 *
 * ----------------------------------------------------------------------- */

static float *
load_jpeg(
    FILE           *fp,
    unsigned int    *width_out,
    unsigned int    *height_out,
    unsigned int    *component_out)
{
    int               i;
    int               width, height;
    int               compos;        /* components in the image */
    float             w = 1.0f / 256.0f;
    unsigned char    *img = NULL;
    float            *fimage;

    img = jpeg_load(fp, &width, &height, &compos);
    if (img == NULL) {
        return NULL;
    }

    fimage = (float *)ri_mem_alloc(sizeof(float) * width * height * 4);

    for (i = 0; i < width * height; i++) {
        switch (compos) {
        case 1:
            fimage[4 * i + 0] = (img[i] + 0.5f) * w;
            fimage[4 * i + 1] = (img[i] + 0.5f) * w;
            fimage[4 * i + 2] = (img[i] + 0.5f) * w;
            fimage[4 * i + 3] = 1.0f;
            break;

        case 2:
            fimage[4 * i + 0] = (img[2*i+0] + 0.5f) * w;
            fimage[4 * i + 1] = (img[2*i+1] + 0.5f) * w;
            fimage[4 * i + 2] = 0.0f;
            fimage[4 * i + 3] = 1.0f;
            break;
    
        case 3:
            fimage[4 * i + 0] = (img[3*i+0] + 0.5f) * w;
            fimage[4 * i + 1] = (img[3*i+1] + 0.5f) * w;
            fimage[4 * i + 2] = (img[3*i+2] + 0.5f) * w;
            fimage[4 * i + 3] = 1.0f;
            break;

        default:
            fimage[4 * i + 0] = 0.0f;
            fimage[4 * i + 1] = 0.0f;
            fimage[4 * i + 2] = 0.0f;
            fimage[4 * i + 3] = 1.0f;
            break;
        
        }
    }

    (*width_out)     = width;
    (*height_out)    = height;
    (*component_out) = compos;      /* FIXME!  Make this 4? */

    ri_mem_free(img);

    return fimage;
}

static void endconv4(void *data)
{
#if defined(WORDS_BIGENDIAN) || defined(__ppc__)
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


/* Compare two strings ignoring differences in case.
 * Same as strcasecmp(), but strcasecmp() seems not posix or ansi-C function.
 */
static int
casecmp(const char *s1, const char *s2)
{
    const char *k1 = s1, *k2 = s2;

    while (k1 != NULL && *k1 != '\0' &&
           k2 != NULL && *k2 != '\0') {
        if (tolower((int)*k1) != tolower((int)*k2)) {
            return -1;
        }

        k1++; k2++;
    }
    
    return 0;

}
