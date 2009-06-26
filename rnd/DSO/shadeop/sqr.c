#include "shadeop.h"
#include <stdio.h>

/* simple shadeop DSO example */

SHADEOP_TABLE(sqr) = {
    {"float sqr(float)", "sqr_init", "sqr_cleanup"},
    {"point sqr_p(point)", "sqr_init", "sqr_cleanup"},
    {""}
};

SHADEOP_INIT(sqr_init)
{
    return NULL;
}

SHADEOP( sqr )
{
    float *result = (float *)argv[0];
    float f = *((float*)argv[1]);
    *result = f * f;

    return 0;
}

SHADEOP(sqr_p)
{
    float *result = (float *)argv[0];
    float *f = ((float*)argv[1]);

    result[0] = f[0] * f[0];
    result[1] = f[1] * f[1];
    result[2] = f[2] * f[2];

    return 0;
}

SHADEOP_CLEANUP( sqr_cleanup )
{
}

