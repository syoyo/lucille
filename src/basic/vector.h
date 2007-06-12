/*
 *   lucille | Global Illumination renderer
 *
 *             written by Syoyo Fujita.
 *
 */

/*
 * 4 component vector calculation.
 *
 * $Id: vector.h,v 1.10 2004/08/15 05:19:39 syoyo Exp $
 */

#ifndef VECTOR_H
#define VECTOR_H

#include "ri.h"

#include "common.h"
#include "matrix.h"

#ifdef WITH_SSE
#include <xmmintrin.h>

#ifdef ENABLE_DOUBLE_PRECISION
#include <emmintrin.h>	/* SSE2 */
#endif

#endif	/* WITH_SSE */

/*
 * We use union for easy use of mixed access to AltiVec or SSE's vector type
 * and traditional array of float type.
 */
typedef union {

#ifdef ENABLE_DOUBLE_PRECISION


#if defined ( WITH_ALTIVEC )
	vector double v0;		/* AltiVec's vector double type	*/
	vector double v1;		/* AltiVec's vector double type	*/
#elif defined ( WITH_SSE )
	__m128d       v0;		/* SSE's vector double type 	*/
	__m128d       v1;		/* SSE's vector double type 	*/
#endif
	double        f[4];		/* vector elements in traditional form	*/

#else	/* !ENABLE_DOUBLE_PRECISION */

#if defined ( WITH_ALTIVEC )
	vector float v;		/* AltiVec's vector float type 		*/
#elif defined ( WITH_SSE )
	__m128       v;		/* SSE's vector float type 		*/
#endif
	float        f[4];	/* vector elements in traditional form	*/


#endif	/* ENABLE_DOUBLE_PRECISION */

} ri_vector_t;

#ifdef __cplusplus
extern "C" {
#endif

#define xcomp(src)    (src)->f[0]
#define ycomp(src)    (src)->f[1]
#define zcomp(src)    (src)->f[2]
#define wcomp(src)    (src)->f[3]



static FORCE_INLINE ri_float_t ri_vector_length3(const ri_vector_t *src)
{
	ri_float_t f;

	f =  ri_sqrt(src->f[0] * src->f[0] +
		     src->f[1] * src->f[1] +
		     src->f[2] * src->f[2]);

	return f;
}

static FORCE_INLINE void ri_vector_normalize3(ri_vector_t *dst)
{
	ri_float_t norm2;
	ri_float_t rsq;                                   

	norm2 = dst->f[0] * dst->f[0] +           
		dst->f[1] * dst->f[1] +           
		dst->f[2] * dst->f[2];            
	if (norm2 > 1.0e-6f) {                            
		rsq = 1.0f / ri_sqrt( norm2 );            
		dst->f[0] *= rsq;                     
		dst->f[1] *= rsq;                     
		dst->f[2] *= rsq;                     
	}                                                 
}

static FORCE_INLINE void ri_vector_add(
	ri_vector_t *dst,
	const ri_vector_t *a,
	const ri_vector_t *b)
{

#if defined(WITH_SSE)

#ifdef ENABLE_DOUBLE_PRECISION
	dst->v0 = _mm_add_pd(a->v0, b->v0);
	dst->v1 = _mm_add_pd(a->v1, b->v1);
#else 	/* !ENABLE_DOUBLE_PRECISION */
	dst->v = _mm_add_ps(a->v, b->v);
#endif	/* ENABLE_DOUBLE_PRECISION */

#else	/* !WITH_SSE */
	dst->f[0] = a->f[0] + b->f[0];
	dst->f[1] = a->f[1] + b->f[1];
	dst->f[2] = a->f[2] + b->f[2];
	dst->f[3] = a->f[3] + b->f[3];
#endif	/* WITH_SSE */
}

static FORCE_INLINE void ri_vector_sub(
	ri_vector_t *dst,
	const ri_vector_t *a,
	const ri_vector_t *b)
{

#if defined(WITH_SSE)

#ifdef ENABLE_DOUBLE_PRECISION
	dst->v0 = _mm_sub_pd(a->v0, b->v0);
	dst->v1 = _mm_sub_pd(a->v1, b->v1);
#else 	/* !ENABLE_DOUBLE_PRECISION */
	dst->v = _mm_sub_ps(a->v, b->v);
#endif	/* ENABLE_DOUBLE_PRECISION */

#else	/* !WITH_SSE */
	dst->f[0] = a->f[0] - b->f[0];
	dst->f[1] = a->f[1] - b->f[1];
	dst->f[2] = a->f[2] - b->f[2];
	dst->f[3] = a->f[3] - b->f[3];
#endif	/* WITH_SSE */
}

static FORCE_INLINE void ri_vector_mul(ri_vector_t *dst,
	const ri_vector_t *a,
	const ri_vector_t *b)
{

#if defined(WITH_SSE)

#ifdef ENABLE_DOUBLE_PRECISION
	dst->v0 = _mm_mul_pd(a->v0, b->v0);
	dst->v1 = _mm_mul_pd(a->v1, b->v1);
#else 	/* !ENABLE_DOUBLE_PRECISION */
	dst->v = _mm_mul_ps(a->v, b->v);
#endif	/* ENABLE_DOUBLE_PRECISION */

#else	/* !WITH_SSE */
	dst->f[0] = a->f[0] * b->f[0];
	dst->f[1] = a->f[1] * b->f[1];
	dst->f[2] = a->f[2] * b->f[2];
	dst->f[3] = a->f[3] * b->f[3];
#endif	/* WITH_SSE */
}

static FORCE_INLINE void ri_vector_scale(ri_vector_t *dst,
	const ri_vector_t *a,
	ri_float_t  scale)
{

#if defined(WITH_SSE)

#ifdef ENABLE_DOUBLE_PRECISION

	ri_vector_t vs0, vs1;

	vs0.v  = _mm_set_pd1(scale);
	vs1.v  = _mm_set_pd1(scale);

	dst->v0 = _mm_mul_pd(a->v0, vs0);
	dst->v1 = _mm_mul_pd(a->v1, vs1);

#else 	/* !ENABLE_DOUBLE_PRECISION */

	ri_vector_t vs;

	vs.v  = _mm_set_ps1(scale);

	dst->v = _mm_mul_ps(a->v, vs.v);

#endif	/* ENABLE_DOUBLE_PRECISION */

#else	/* !WITH_SSE */
	dst->f[0] = a->f[0] * scale;
	dst->f[1] = a->f[1] * scale;
	dst->f[2] = a->f[2] * scale;
	dst->f[3] = a->f[3] * scale;
#endif	/* WITH_SSE */
}

static FORCE_INLINE ri_float_t ri_vector_dot3(
	const ri_vector_t *a,
	const ri_vector_t *b)
{
	ri_float_t d;

	d = a->f[0] * b->f[0] + a->f[1] * b->f[1] + a->f[2] * b->f[2];

	return d;
}

static FORCE_INLINE ri_float_t ri_vector_dot4(
	const ri_vector_t *a,
	const ri_vector_t *b)
{
	ri_float_t d;

	d = a->f[0] * b->f[0] + a->f[1] * b->f[1] + a->f[2] * b->f[2] + a->f[3] * b->f[3];

	return d;
}

static FORCE_INLINE void ri_vector_cross3(
	ri_vector_t *dst,
	const ri_vector_t *a,
	const ri_vector_t *b)
{
	dst->f[0] = a->f[1] * b->f[2] - a->f[2] * b->f[1];
	dst->f[1] = a->f[2] * b->f[0] - a->f[0] * b->f[2];
	dst->f[2] = a->f[0] * b->f[1] - a->f[1] * b->f[0];
}

static FORCE_INLINE void ri_vector_transform(
	ri_vector_t       *dst,
	const ri_vector_t *src,
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

	for (j = 0; j < 4; j++) {
		dst->f[j] = 0.0f;
		for (i = 0; i < 4; i++)
			dst->f[j] += src->f[i] * mat->f[i][j];
	}
}

/* float to vector. x -> xxxx */
static FORCE_INLINE void ri_vector_splats3(
	ri_vector_t   *dst,
	ri_float_t f )
{
#if defined(WITH_SSE)

#ifdef ENABLE_DOUBLE_PRECISION

	dst->v0 = _mm_set_pd1(f);
	dst->v1 = _mm_set_pd1(f);

#else 	/* !ENABLE_DOUBLE_PRECISION */

	dst->v = _mm_set_ps1(f);

#endif	/* ENABLE_DOUBLE_PRECISION */

#else	/* !WITH_SSE */

	dst->f[0] = f;
	dst->f[1] = f;
	dst->f[2] = f;
	dst->f[3] = (ri_float_t)0.0;

#endif	/* WITH_SSE */
	
}

/* RenderMan vector float -> lucille internal vector float conversion */
static FORCE_INLINE void ri_vector_set_rman(ri_vector_t   *dst,
	const RtVector src )
{
	dst->f[0] = (ri_float_t)src[0];
	dst->f[1] = (ri_float_t)src[1];
	dst->f[2] = (ri_float_t)src[2];
	dst->f[3] = (ri_float_t)1.0;
}

static FORCE_INLINE void ri_vector_set4(ri_vector_t *dst,
	const ri_float_t x,
	const ri_float_t y,
	const ri_float_t z, 
	const ri_float_t w )
{
	dst->f[0] = x;
	dst->f[1] = y;
	dst->f[2] = z;
	dst->f[3] = w;
}

static FORCE_INLINE void ri_vector_set3(ri_vector_t *dst,
	const ri_float_t x,
	const ri_float_t y,
	const ri_float_t z)
{
	dst->f[0] = x;
	dst->f[1] = y;
	dst->f[2] = z;
}

static FORCE_INLINE void ri_vector_set1(ri_vector_t *dst,
	const ri_float_t v)
{
	dst->f[0] = v;
	dst->f[1] = v;
	dst->f[2] = v;
	dst->f[3] = v;
}

static FORCE_INLINE void ri_vector_copy(ri_vector_t *dst,
	const ri_vector_t *src)
{
	dst->f[0] = src->f[0];
	dst->f[1] = src->f[1];
	dst->f[2] = src->f[2];
	dst->f[3] = src->f[3];
}

static FORCE_INLINE void ri_vector_zero(ri_vector_t *dst)
{
#if defined(WITH_SSE)

#ifdef ENABLE_DOUBLE_PRECISION

	dst->v0 = _mm_setzero_pd();
	dst->v1 = _mm_setzero_pd();

#else 	/* !ENABLE_DOUBLE_PRECISION */

	dst->v = _mm_setzero_ps();

#endif	/* ENABLE_DOUBLE_PRECISION */

#else	/* !WITH_SSE */

	dst->f[0] = (ri_float_t)0.0;
	dst->f[1] = (ri_float_t)0.0;
	dst->f[2] = (ri_float_t)0.0;
	dst->f[3] = (ri_float_t)0.0;

#endif	/* WITH_SSE */
}

static FORCE_INLINE void ri_vector_neg(ri_vector_t *dst)
{
#if defined(WITH_SSE)

#ifdef ENABLE_DOUBLE_PRECISION

	dst->v0 = _mm_xor_pd(dst->v0, _mm_set_pd(-0.0, -0.0));
	dst->v1 = _mm_xor_pd(dst->v1, _mm_set_pd(-0.0, -0.0));

#else 	/* !ENABLE_DOUBLE_PRECISION */

	dst->v = _mm_xor_ps(dst->v, _mm_set_ps(-0.0, -0.0, -0.0, -0.0));

#endif	/* ENABLE_DOUBLE_PRECISION */

#else	/* !WITH_SSE */

	dst->f[0] = -dst->f[0];
	dst->f[1] = -dst->f[1];
	dst->f[2] = -dst->f[2];
	dst->f[3] = -dst->f[3];

#endif	/* WITH_SSE */
}

static FORCE_INLINE ri_float_t ri_vector_ave(const ri_vector_t *src)
{
	return ( src->f[0] + src->f[1] + src->f[2] ) / (ri_float_t)3.0;
}


extern DLLEXPORT void ri_vector_print(
	const ri_vector_t vec );


#ifdef __cplusplus
}       /* extern "C" */
#endif



#endif

