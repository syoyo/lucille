/*
 *   lucille | Global Illumination renderer
 *
 *             written by Syoyo Fujita.
 *
 */

/*
 * Image saver.
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

static void        endconv4(void *data);


/* --------------------------------------------------------------------------
 *
 * Public functions
 *
 * ----------------------------------------------------------------------- */

/*
 * Function: ri_image_save_hdr
 *
 *   Saves image as RGBE format.
 */
extern int ri_image_save_hdr(
    const char      *filename,
    float           *image,
    unsigned int     width,
    unsigned int     height)
{

    FILE        *fp;
    float       *buf_image;
    int          buf_width;
    int          buf_height;

    fp = fopen(filename, "wb");
    if (!fp) {
        ri_log(LOG_WARN, "Can't write image file \"%s\"", filename);
        exit(-1);
    }

    buf_image = (float *)ri_mem_alloc(sizeof(float) * width * height * 3);

    
    
    buf_width  = width;
    buf_height = height;
    endconv4(&buf_width);
    endconv4(&buf_height);
	RGBE_WriteHeader(fp, buf_width, buf_height, NULL);

    {
        uint32_t i;

        memcpy(buf_image, image, sizeof(float) * width * height * 3);
        for (i = 0; i < width * height * 3; i++) {
            endconv4(&buf_image[i]); 
        }
    }
	RGBE_WritePixels_RLE(fp, buf_image, width, height);

    fclose(fp);

    ri_mem_free(buf_image);

    return 0;   /* OK */
}


/* --------------------------------------------------------------------------
 *
 * Private functions
 *
 * ----------------------------------------------------------------------- */

void endconv4(void *data)
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
