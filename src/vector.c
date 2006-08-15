/* File: vector.c

     This file contains vector operation routines.
     In lucille, vector data is represented by 4 component.
     This is for SIMD coding. Although, it is sometimes a waste of
     memory when vector operation needs only 3 components(e.g. x, y and z).

 */
 
/*
 * One big coding restrictions in lucille.
 * You should pass vector data structure as pointer always!!
 *
 * e.g.
 *
 *     void func(ri_vector_t *arg);
 *
 * This is due to some compiler's limitation when using SIMD data type.
 */

/*
 *    To exploid benefits from SIMD and ordinal array based vector storage,
 *    lucille has 2 member in ri_vector_t(lucille's native vector data
 *    structure).
 *
 *    If you turn on WITH_SSE or WITH_ALTIVEC define, you can access
 *    native vector data type(in SSE, it is __m128. in AltiVec, it is vector
 *    float) as member 'v'(e.g. vect->v).
 *
 *    If you want access vector compoennt through ordinally array based style,
 *    you can do it through member 'e'(e.g. vect->e[0]). This is also true when
 *    WITH_SSE or WITH_ALTIVEC is on.
 *
 */
/*
 * $Id: vector.c,v 1.6 2004/07/07 11:06:41 syoyo Exp $
 */

 

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <math.h>
#include <stdio.h>

#ifdef WITH_SSE
#include <xmmintrin.h>
#endif

#include "vector.h"

#ifndef NDEBUG

/*
 * Function: ri_vector_length
 * 
 *     Calculates the norm of vector in 3D.
 *
 * Parameters:
 *
 *     *src - Input vector.
 *
 * Returns:
 *
 *     The norm of vector src in 3D.
 */
RtFloat
ri_vector_length(const ri_vector_t *src)
{
	float length;
	
	length = ri_vector_dot3(src, src);

	return (RtFloat)sqrt(length);
}
#endif

#ifndef NDEBUG
/*
 * Function: ri_vector_normalize
 * 
 *     Normalizes the vector.
 *
 * Parameters:
 *
 *     *dst - The vector normalized. 
 *
 * Returns:
 *
 *     None.
 */
void
ri_vector_normalize(ri_vector_t *dst)
{
#ifdef WITH_ALTIVEC
	float length2;
	ri_vector_t tmp, rsqrt;
	static vector float zero = (vector float)(0.0f, 0.0f, 0.0f, 0.0f);

	length2 = ri_vector_dot3(dst, dst);
	if ( length2 != 0.0f) {
		tmp.e[0] = tmp.e[1] = tmp.e[2] = tmp.e[3] = length2;
		
		/*
		 * calculate reciprocal square root. 
		 * vec_rsqrt() returns a half precision result.
		 */
		rsqrt.v = vec_rsqrte(tmp.v);

		dst->v = vec_madd(dst->v, rsqrt.v, zero);
	}
#else
	float length;
	length = ri_vector_length(dst);

	if (length != 0.0f) length = 1.0f / length; /* calc inverse. */

	dst->e[0] *= length;
	dst->e[1] *= length;
	dst->e[2] *= length;
	dst->e[3] *= length;
#endif
}
#endif

#ifndef NDEBUG
/*
 * Function: ri_vector_add
 * 
 *     Adds two vectors.
 *
 * Parameters:
 *
 *     *dst - The result vector of a + b. 
 *     *a   - The first vector.
 *     *b   - The second vector.
 *
 * Returns:
 *
 *     None.
 */
void
ri_vector_add(ri_vector_t *dst, const ri_vector_t *a, const ri_vector_t *b)
{

#ifdef WITH_ALTIVEC
	dst->v = vec_add(a->v, b->v);
//#elif defined(WITH_SSE)
//	dst->v = _mm_add_ps(a->v, b->v);
#else
	dst->e[0] = a->e[0] + b->e[0];
	dst->e[1] = a->e[1] + b->e[1];
	dst->e[2] = a->e[2] + b->e[2];
	dst->e[3] = a->e[3] + b->e[3];
#endif
}
#endif

#ifndef NDEBUG
void
ri_vector_sub(ri_vector_t *dst, const ri_vector_t *a, const ri_vector_t *b)
{
#ifdef WITH_ALTIVEC
	dst->v = vec_sub(a->v, b->v);
//#elif defined(WITH_SSE)
//	dst->v = _mm_sub_ps(a->v, b->v);
#else
	dst->e[0] = a->e[0] - b->e[0];
	dst->e[1] = a->e[1] - b->e[1];
	dst->e[2] = a->e[2] - b->e[2];
	dst->e[3] = a->e[3] - b->e[3];
#endif
}
#endif

#ifndef NDEBUG
RtFloat
ri_vector_dot(const ri_vector_t *a, const ri_vector_t *b)
{
	float result;
#ifdef WITH_ALTIVEC
	/* This code is not so faster than the scalar code... */
	ri_vector_t ta, tb;
	vector unsigned int tmp = vec_splat_u32(-1);
	vector float minus_zero = (vector float) vec_sl(tmp, tmp);
	vector float length;

	ta.v = a->v; tb.v = b->v;
	ta.e[3] = 0.0f; tb.e[3] = 0.0f;

	/* find the dot product of the two vectors */
	length = vec_madd(ta.v, tb.v, minus_zero);

	/* Sum across all elements */

	length = vec_add(length, vec_sld(length, length, 4));
	length = vec_add(length, vec_sld(length, length, 8));

	vec_ste(length, 0, &result);
#else
	result = a->e[0] * b->e[0] +
	 	 a->e[1] * b->e[1] +
		 a->e[2] * b->e[2];
#endif
	return result;
}

RtFloat
ri_vector_dot3(const ri_vector_t *a, const ri_vector_t *b)
{
	float result;
#ifdef WITH_ALTIVEC
	/* This code is not so faster than the scalar code... */
	ri_vector_t ta, tb;
	vector unsigned int tmp = vec_splat_u32(-1);
	vector float minus_zero = (vector float) vec_sl(tmp, tmp);
	vector float length;

	ta.v = a->v; tb.v = b->v;
	ta.e[3] = 0.0f; tb.e[3] = 0.0f;

	/* find the dot product of the two vectors */
	length = vec_madd(ta.v, tb.v, minus_zero);

	/* Sum across all elements */

	length = vec_add(length, vec_sld(length, length, 4));
	length = vec_add(length, vec_sld(length, length, 8));

	vec_ste(length, 0, &result);
#else
	result = a->e[0] * b->e[0] +
	 	 a->e[1] * b->e[1] +
		 a->e[2] * b->e[2];
#endif
	return result;
}
#endif

RtFloat
ri_vector_dot4(const ri_vector_t *a, const ri_vector_t *b)
{
	RtFloat result;
#ifdef WITH_ALTIVEC
	vector unsigned int tmp = vec_splat_u32(-1);
	vector float minus_zero = (vector float) vec_sl(tmp, tmp);
	
	/* find the dot product of the two vectors */
	vector float length = vec_madd(a->v, b->v, minus_zero);

	/* Sum across all elements */
	length = vec_add(length, vec_sld(length, length, 4));
	length = vec_add(length, vec_sld(length, length, 8));

	vec_ste(length, 0, &result);
#else
	result = a->e[0] * b->e[0] + a->e[1] * b->e[1] +
		 a->e[2] * b->e[2] + a->e[3] * b->e[3];
#endif
	return result;
}

#ifndef NDEBUG
void
ri_vector_cross3(ri_vector_t *dst, const ri_vector_t *a, const ri_vector_t *b)
{
	/* TODO: implement AltiVec version. */
	dst->e[0] = a->e[1] * b->e[2] - a->e[2] * b->e[1];
	dst->e[1] = a->e[2] * b->e[0] - a->e[0] * b->e[2];
	dst->e[2] = a->e[0] * b->e[1] - a->e[1] * b->e[0];
}
#endif

void
ri_vector_cross4(ri_vector_t *dst, const ri_vector_t *a, const ri_vector_t *b)
{
	/* TODO: implement AltiVec version. */
	dst->e[0] = a->e[1] * b->e[2] - a->e[2] * b->e[1];
	dst->e[1] = a->e[2] * b->e[3] - a->e[3] * b->e[2];
	dst->e[2] = a->e[3] * b->e[0] - a->e[0] * b->e[3];
	dst->e[3] = a->e[0] * b->e[1] - a->e[1] * b->e[0];
}
 
void ri_vector_transform(ri_vector_t *dst, const ri_vector_t *src, const ri_matrix_t *mat)
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
			dst->e[j] = 0.0f;
		for (i = 0; i < 4; i++) {
			dst->e[j] += src->e[i] * mat->e[i][j]; 
		}
	}
}	

void
ri_vector_print(const ri_vector_t *vec)
{
	int i;

	for (i = 0; i < 4; i++) {
		printf("%f ", vec->e[i]);
	}

	printf("\n");
}

void
ri_vector_set(ri_vector_t *dst, const RtVector src)
{
	dst->e[0] = src[0];
	dst->e[1] = src[1];
	dst->e[2] = src[2];
	dst->e[3] = 1.0f;
}

void
ri_vector_set4(ri_vector_t *dst,
	       const RtFloat x, const RtFloat y,
	       const RtFloat z, const RtFloat w)
{
	dst->e[0] = x;
	dst->e[1] = y;
	dst->e[2] = z;
	dst->e[3] = w;
}

#ifndef NDEBUG
void
ri_vector_copy(ri_vector_t *dst, const ri_vector_t *src)
{
#ifdef WITH_ALTIVEC
	dst->v = src->v;
#else
	dst->e[0] = src->e[0];
	dst->e[1] = src->e[1];
	dst->e[2] = src->e[2];
	dst->e[3] = src->e[3];
#endif
}
#endif

#ifndef NDEBUG
void
ri_vector_scale(ri_vector_t *dst, float f)
{
	dst->e[0] *= f;
	dst->e[1] *= f;
	dst->e[2] *= f;
	dst->e[3] *= f;
}
#endif

void
ri_vector_zero(ri_vector_t *dst)
{
	dst->e[0] = 0.0;
	dst->e[1] = 0.0;
	dst->e[2] = 0.0;
	dst->e[3] = 0.0;
}

void
ri_vector_neg(ri_vector_t *dst)
{
	dst->e[0] = -dst->e[0];
	dst->e[1] = -dst->e[1];
	dst->e[2] = -dst->e[2];
	dst->e[3] = -dst->e[3];
}

void
ri_vector_mul(ri_vector_t *dst, const ri_vector_t *a, const ri_vector_t *b)
{
	dst->e[0] = a->e[0] * b->e[0];
	dst->e[1] = a->e[1] * b->e[1];
	dst->e[2] = a->e[2] * b->e[2];
	dst->e[3] = a->e[3] * b->e[3];
}

double
ri_vector_ave(const ri_vector_t *src)
{
	return (src->e[0] + src->e[1] + src->e[2]) / 3.0;
}
