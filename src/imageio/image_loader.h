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

#ifndef LUCILLE_IMAGE_LOADER_H
#define LUCILLE_IMAGE_LOADER_H

#ifdef __cplusplus
extern "C" {
#endif

#include "vector.h"

extern float *ri_image_load(
    const char      *filename,
    unsigned int    *width_out,
    unsigned int    *height_out,
    unsigned int    *component_out);

#ifdef __cplusplus
}
#endif

#endif  /* LUCILLE_IMAGE_LOADER_H */


