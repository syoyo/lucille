#ifndef VEC_H
#define VEC_H

#include <math.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef float vec[4];

static inline void vcpy(vec r, vec a) 
{
    r[0] = a[0];
    r[1] = a[1];
    r[2] = a[2];
    r[3] = a[3];
}

static inline void vadd(vec r, vec a, vec b) 
{
    r[0] = a[0] + b[0];
    r[1] = a[1] + b[1];
    r[2] = a[2] + b[2];
    r[3] = a[3] + b[3];
}

static inline void vsub(vec r, vec a, vec b) 
{
    r[0] = a[0] - b[0];
    r[1] = a[1] - b[1];
    r[2] = a[2] - b[2];
    r[3] = a[3] - b[3];

}

static inline float vdot(vec a, vec b) 
{
    return a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
}

static inline void vcross(vec r, vec a, vec b) 
{
    r[0] = a[1] * b[2] - a[2] * b[1];
    r[1] = a[2] * b[0] - a[0] * b[2];
    r[2] = a[0] * b[1] - a[1] * b[0];

}

static inline void vnormalize(vec r) 
{
    float len = vdot(r, r);

    if (fabsf(len) > 1.0e-6f) {
        r[0] /= sqrtf(len);
        r[1] /= sqrtf(len);
        r[2] /= sqrtf(len);
        r[3] /= sqrtf(len);
    }

}

#ifdef __cplusplus
}
#endif

#endif      /* VEC_H    */
