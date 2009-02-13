#include <stdio.h>
#include <stdlib.h>

#include "hbuffer.h"
#include "cachelib.h"

ri_hbuffer_t *g_hbuffer[MAX_LAYER];

void
lse_init(int w, int h)
{
    int i;

    for (i = 0; i < MAX_LAYER; i++) {
        g_hbuffer[i]           = ri_hbuffer_new();
        g_hbuffer[i]->id       = 0;
        g_hbuffer[i]->width    = w;
        g_hbuffer[i]->height   = h;
        g_hbuffer[i]->buffer   = malloc(4 * sizeof(float) * w * h);
    }

}

void
lse_save_cache_iiic(int layer, int x, int y, float *val)
{
    float *buf;

    buf = (float *)g_hbuffer[layer]->buffer;

    buf[4 * (y * g_hbuffer[layer]->width + x) + 0] = val[0];
    buf[4 * (y * g_hbuffer[layer]->width + x) + 1] = val[1];
    buf[4 * (y * g_hbuffer[layer]->width + x) + 2] = val[2];
    buf[4 * (y * g_hbuffer[layer]->width + x) + 3] = val[3];

}

void
lse_load_cache_iiic(int layer, int x, int y, float *val)
{
    float *buf;

    buf = (float *)g_hbuffer[layer]->buffer;

    val[0] = buf[4 * (y * g_hbuffer[layer]->width + x) + 0];
    val[1] = buf[4 * (y * g_hbuffer[layer]->width + x) + 1];
    val[2] = buf[4 * (y * g_hbuffer[layer]->width + x) + 2];
    val[3] = buf[4 * (y * g_hbuffer[layer]->width + x) + 3];

}

