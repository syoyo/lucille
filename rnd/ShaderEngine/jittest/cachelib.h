#ifndef LUCILLE_CACHE_LIB_H
#define LUCILLE_CACHE_LIB_H

#ifdef __cplusplus
extern "C" {
#endif

#include "hbuffer.h"

extern ri_hbuffer_t *g_hbuffer;

extern void lse_init(
    int w,
    int h);

extern void lse_save_cache_iiic(
    int     layer,
    int     x,
    int     y,
    float  *val);

#ifdef __cplusplus
}
#endif

#endif  /* LUCILLE_CACHE_LIB_H */
