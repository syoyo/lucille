/*
 *
 *                   lucille | Global Illumination Renderer
 *
 *         Copyright 2003-2203 Syoyo Fujita (syoyo@lucillerender.org)
 *
 *
 */

/*
 * Copyright 2003-2203 Syoyo Fujita.  All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the names of the authors nor the names of their contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS
 * OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
 * OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
 * OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
 * EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

/* ---------------------------------------------------------------------------
 *
 * File: vector.h
 *
 *   Headef file for vector operation.
 *   vector.h provides generic, target CPU independent representation of vector
 *   data.
 *   Target CPU dependent SIMD optiomization is done by MUDA and such a code
 *   are embedded into specific location of the source code, not here.
 *
 * ------------------------------------------------------------------------ */


#ifndef LUCILLE_VECTOR_H
#define LUCILLE_VECTOR_H

#include "ri.h"

#include "common.h"
#include "matrix.h"

typedef double ri_vector_t[4];
#define vec ri_vector_t                /* Abbr */

#ifdef __cplusplus
extern "C" {
#endif



static FORCE_INLINE ri_float_t ri_vector_length(const ri_vector_t src)
{
    return sqrt(src[0] * src[0] + src[1] * src[1] + src[2] * src[2]);
}

static FORCE_INLINE void ri_vector_normalize(ri_vector_t dst)
{
    ri_float_t norm2;
    ri_float_t rsq;                                   

    norm2 = dst[0] * dst[0] + dst[1] * dst[1] + dst[2] * dst[2];            
    if (norm2 > 1.0e-17f) {                            
        rsq = 1.0 / sqrt( norm2 );            
        dst[0] *= rsq;                     
        dst[1] *= rsq;                     
        dst[2] *= rsq;                     
    }                                                 
}
#define vnormalize(dst) ri_vector_normalize(dst)

#define ri_vector_add(dst, a, b) do { \
    (dst)[0] = (a)[0] + (b)[0]; \
    (dst)[1] = (a)[1] + (b)[1]; \
    (dst)[2] = (a)[2] + (b)[2]; \
} while (0);
#define vadd(dst, a, b) ri_vector_add(dst, a, b)

#define ri_vector_sub(dst, a, b) do { \
    (dst)[0] = (a)[0] - (b)[0]; \
    (dst)[1] = (a)[1] - (b)[1]; \
    (dst)[2] = (a)[2] - (b)[2]; \
} while (0);
#define vsub(dst, a, b) ri_vector_sub(dst, a, b)

#define ri_vector_mul(dst, a, b) do { \
    (dst)[0] = (a)[0] * (b)[0]; \
    (dst)[1] = (a)[1] * (b)[1]; \
    (dst)[2] = (a)[2] * (b)[2]; \
} while (0);
#define vmul(dst, a, b) ri_vector_mul(dst, a, b)

#define ri_vector_scale(dst, a, s) do { \
    (dst)[0] = (a)[0] * (s); \
    (dst)[1] = (a)[1] * (s); \
    (dst)[2] = (a)[2] * (s); \
} while (0);
#define vscale(dst, a, s) ri_vector_scale(dst, a, s)

#define ri_vector_dot(a, b) ((a)[0] * (b)[0] + (a)[1] * (b)[1] + (a)[2] * (b)[2])
#define vdot(a, b) ri_vector_dot(a, b)

#define ri_vector_cross(dst, a, b) do { \
    (dst)[0] = (a)[1] * (b)[2] - (a)[2] * (b)[1]; \
    (dst)[1] = (a)[2] * (b)[0] - (a)[0] * (b)[2]; \
    (dst)[2] = (a)[0] * (b)[1] - (a)[1] * (b)[0]; \
} while (0);
#define vcross(dst, a, b) ri_vector_cross(dst, a, b);

#define ri_vector_set1(dst, f) do { \
    (dst)[0] = (f); \
    (dst)[1] = (f); \
    (dst)[2] = (f); \
    (dst)[3] = 0.0; \
} while (0);

#define ri_vector_set4(dst, a, b, c, d) do { \
    (dst)[0] = (a); \
    (dst)[1] = (b); \
    (dst)[2] = (c); \
    (dst)[3] = (d); \
} while (0);

#define ri_vector_copy(dst, src) do { \
    (dst)[0] = (src)[0]; \
    (dst)[1] = (src)[1]; \
    (dst)[2] = (src)[2]; \
    (dst)[3] = (src)[3]; \
} while (0);
#define vcpy(dst, src) ri_vector_copy(dst, src)

#define ri_vector_setzero(dst) do { \
    (dst)[0] = 0.0; \
    (dst)[1] = 0.0; \
    (dst)[2] = 0.0; \
    (dst)[3] = 0.0; \
} while (0);
#define vzero(dst) ri_vector_setzero(dst)

#define ri_vector_neg(dst) do { \
    (dst)[0] = -(dst)[0]; \
    (dst)[1] = -(dst)[1]; \
    (dst)[2] = -(dst)[2]; \
    (dst)[3] = -(dst)[3]; \
} while (0);
#define vneg(dst) ri_vector_neg(dst)

#define ri_vector_max(dst, a, b) do { \
    (dst)[0] = ((a)[0] > (b)[0]) ? (a)[0] : (b)[0]; \
    (dst)[1] = ((a)[1] > (b)[1]) ? (a)[1] : (b)[1]; \
    (dst)[2] = ((a)[2] > (b)[2]) ? (a)[2] : (b)[2]; \
    (dst)[3] = ((a)[3] > (b)[3]) ? (a)[3] : (b)[3]; \
} while (0);
#define vmax(dst, a, b) ri_vector_max(dst, a, b)

#define ri_vector_min(dst, a, b) do { \
    (dst)[0] = ((a)[0] < (b)[0]) ? (a)[0] : (b)[0]; \
    (dst)[1] = ((a)[1] < (b)[1]) ? (a)[1] : (b)[1]; \
    (dst)[2] = ((a)[2] < (b)[2]) ? (a)[2] : (b)[2]; \
    (dst)[3] = ((a)[3] < (b)[3]) ? (a)[3] : (b)[3]; \
} while (0);
#define vmin(dst, a, b) ri_vector_min(dst, a, b)

static FORCE_INLINE void ri_vector_transform(
    ri_vector_t        dst,
    const ri_vector_t  src,
    const ri_matrix_t *mat )
{
    /*
     * Matrix is defined in row-major order, and vector is treated as a row
     * vector.
     *
     * i.e.
     *                                    | m00 m01 m02 m03 |
     *                                    | m10 m11 m12 m13 |
     * (d0, d1, d2, d3) = (s0, s1, s2, s3)| m20 m21 m22 m23 |
     *                                    | m30 m31 m32 m33 |
     */

    int i, j;

    /* Avoid the problem in the case of addr(dst) == addr(src). */
    ri_vector_t v;
    vcpy(v, src);
    v[3] = 1.0;

    for (j = 0; j < 4; j++) {
        dst[j] = 0.0;
        for (i = 0; i < 4; i++)
            dst[j] += v[i] * mat->f[i][j];
    }
}


/* RenderMan vector float -> lucille internal vector float conversion */
static FORCE_INLINE void ri_vector_set_from_rman(
    ri_vector_t    dst,
    const RtVector src )
{
    dst[0] = (ri_float_t)src[0];
    dst[1] = (ri_float_t)src[1];
    dst[2] = (ri_float_t)src[2];
    dst[3] = (ri_float_t)0.0;
}

static FORCE_INLINE ri_float_t ri_vector_ave(const ri_vector_t src)
{
    return ( src[0] + src[1] + src[2] ) / (ri_float_t)3.0;
}


extern DLLEXPORT void ri_vector_print(
    const ri_vector_t vec );


#ifdef __cplusplus
}       /* extern "C" */
#endif


#endif  /* LUCILLE_VECTOR_H */

