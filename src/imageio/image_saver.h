/*
 *   lucille | Global Illumination renderer
 *
 *             written by Syoyo Fujita.
 *
 */

/*
 * Image saver routine.
 *
 * $Id$
 */

#ifndef LUCILLE_IMAGE_SAVER_H
#define LUCILLE_IMAGE_SAVER_H

#ifdef __cplusplus
extern "C" {
#endif

#include "vector.h"

/* Save image as .hdr */
extern int ri_image_save_hdr(
    const char      *filename,
    float           *image,
    unsigned int     width,
    unsigned int     height);

#ifdef __cplusplus
}
#endif

#endif  /* LUCILLE_IMAGE_SAVER_H */


