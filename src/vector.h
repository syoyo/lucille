/*
 * 4 component vector calculation.
 * 
 * $Id: vector.h,v 1.10 2004/08/15 05:19:39 syoyo Exp $
 */

#ifndef VECTOR_H
#define VECTOR_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "ri.h"
#include "matrix.h"

#ifdef WITH_SSE
#include <xmmintrin.h>
#endif

#ifdef WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT 
#endif

/*
 * One coding restriction when using SSE(WITH_SSE is defined).
 * You should pass a vector value by reference in function argument
 * because of alignment problem.
 * This restriction is not true when using AltiVec.
 *
 * e.g.
 *
 * void func(ri_vector_t v)  <-- NG!
 *
 * void func(ri_vector_t *v) <-- OK
 */

/*
 * We use union for easy use of AltiVec's vector type, SSE's vector type
 * and traditional array of float type.
 *
 * Note: sizeof(ri_vector_t) is always 16 byte.
 */
#ifdef _MSC_VER	/* Visual C++ */
__declspec(align(16)) typedef union
#else
typedef union
#endif
{
#if defined(WITH_ALTIVEC)
	vector float v; /* AltiVec's vector float type */
#elif defined(WITH_SSE)
	__m128 v;	/* SSE's vector float type */
#endif
	float e[4];	/* elements of traditional form */
}ri_vector_t;

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Fast reciprocal square root 1/sqrt(x).
 *
 * from flipCode.
 *
 */
#define fastrsqrt(dst, number)	do {					\
        long i;								\
        float x2, y;							\
        const float threehalfs = 1.5f;					\
	union {								\
		long l;							\
		float f;						\
	} data32;							\
									\
	/*printf("fast rsqrt. num = %f\n", (number));*/			\
        x2 = (number) * 0.5f;						\
        y  = (number);							\
	data32.f = y;							\
	i = data32.l;							\
        /* i  = *(long *)&y; */						\
        i  = 0x5f3759df - (i >> 1);					\
	data32.l = i;							\
	y  = data32.f;							\
        /* y  = *(float *)&i; */						\
        y  = y * (threehalfs - (x2 * y * y));				\
									\
	(dst) = y;							\
	/*printf("dst = %f\n", (dst));*/				\
} while (0)

#ifdef NDEBUG
#define ri_vector_length(src) (RtFloat)sqrt((src)->e[0] * (src)->e[0] +	\
					    (src)->e[1] * (src)->e[1] +	\
		      			    (src)->e[2] * (src)->e[2])
#else
extern DLLEXPORT RtFloat ri_vector_length	 (const ri_vector_t *src);
#endif

#ifdef NDEBUG
#define ri_vector_normalize(dst) {					\
	/* float length;	*/					\
	float norm2;							\
	float rsq;							\
									\
	norm2 = (dst)->e[0] * (dst)->e[0] + (dst)->e[1] * (dst)->e[1] 	\
	      + (dst)->e[2] * (dst)->e[2];				\
									\
	if (norm2 > 1.0e-6) {						\
		/*printf("normalize\n"); */					\
		/*printf("e = %f, %f, %f\n", (dst)->e[0], (dst)->e[1], (dst)->e[2]);*/									\
		/*fastrsqrt(rsq, norm2);*/  				\
		rsq = 1.0 / sqrtf(norm2);				\
		(dst)->e[0] *= rsq;					\
		(dst)->e[1] *= rsq;					\
		(dst)->e[2] *= rsq;					\
	} 								\
}
#else
extern DLLEXPORT void    ri_vector_normalize(	ri_vector_t *dst);
#endif

#ifdef NDEBUG
#ifdef WITH_ALTIVEC
#define ri_vector_add(dst, a, b) (dst)->v = vec_add((a)->v, (b)->v)
#elif defined(WITH_SSE)
#define ri_vector_add(dst, a, b) (dst)->v = _mm_add_ps((a)->v, (b)->v)
#else
#define ri_vector_add(dst, a, b) {					\
	(dst)->e[0] = (a)->e[0] + (b)->e[0];				\
	(dst)->e[1] = (a)->e[1] + (b)->e[1];				\
	(dst)->e[2] = (a)->e[2] + (b)->e[2];				\
	(dst)->e[3] = (a)->e[3] + (b)->e[3];				\
}
#endif
#else
extern DLLEXPORT void    ri_vector_add	 (	ri_vector_t *dst,
	 			  const ri_vector_t *a,
 				  const ri_vector_t *b);

#endif

#ifdef NDEBUG
#ifdef WITH_ALTIVEC
#define ri_vector_sub(dst, a, b) (dst)->v = vec_sub((a)->v, (b)->v)
#elif defined(WITH_SSE)
#define ri_vector_sub(dst, a, b) (dst)->v = _mm_sub_ps((a)->v, (b)->v)
#else
#define ri_vector_sub(dst, a, b) {					\
	(dst)->e[0] = (a)->e[0] - (b)->e[0];				\
	(dst)->e[1] = (a)->e[1] - (b)->e[1];				\
	(dst)->e[2] = (a)->e[2] - (b)->e[2];				\
	(dst)->e[3] = (a)->e[3] - (b)->e[3];				\
}
#endif
#else
extern DLLEXPORT void    ri_vector_sub	 (	ri_vector_t *dst,
 				  const ri_vector_t *a,
 				  const ri_vector_t *b);
#endif

#ifdef NDEBUG
#define ri_vector_scale(dst, f) {					\
	(dst)->e[0] *= (f);						\
	(dst)->e[1] *= (f);						\
	(dst)->e[2] *= (f);						\
	(dst)->e[3] *= (f);						\
}
#else
extern DLLEXPORT void    ri_vector_scale	 (	ri_vector_t *dst,
					float	     s);
#endif

/* 3 component dot product. */
#ifdef NDEBUG
#define ri_vector_dot(a, b) ((a)->e[0] * (b)->e[0] +			\
			     (a)->e[1] * (b)->e[1] +			\
			     (a)->e[2] * (b)->e[2])
#define ri_vector_dot3(a, b) ((a)->e[0] * (b)->e[0] +			\
			      (a)->e[1] * (b)->e[1] +			\
			      (a)->e[2] * (b)->e[2])
#else
extern DLLEXPORT RtFloat ri_vector_dot	 (const ri_vector_t *a,
				  const ri_vector_t *b);
extern DLLEXPORT RtFloat ri_vector_dot3	 (const ri_vector_t *a,
				  const ri_vector_t *b);
#endif
/* 4 component dot product. */
extern DLLEXPORT RtFloat ri_vector_dot4	 (const ri_vector_t *a,
       				  const ri_vector_t *b);
#ifdef NDEBUG
#define ri_vector_cross3(dst, a, b) {					\
	(dst)->e[0] = (a)->e[1] * (b)->e[2] - (a)->e[2] * (b)->e[1];	\
	(dst)->e[1] = (a)->e[2] * (b)->e[0] - (a)->e[0] * (b)->e[2];	\
	(dst)->e[2] = (a)->e[0] * (b)->e[1] - (a)->e[1] * (b)->e[0];	\
}
#else
extern DLLEXPORT void    ri_vector_cross3	 (	ri_vector_t *dst,
				  const ri_vector_t *a,
				  const ri_vector_t *b);
#endif
extern DLLEXPORT void    ri_vector_cross4	 (	ri_vector_t *dst,
 				  const ri_vector_t *a,
 				  const ri_vector_t *b);
extern DLLEXPORT void    ri_vector_transform(	ri_vector_t *dst,
				  const ri_vector_t *src,
 				  const ri_matrix_t *mat);
extern DLLEXPORT void    ri_vector_print	 (const ri_vector_t *vec);
/* set internal vector type from RtVector */
extern DLLEXPORT void    ri_vector_set	 (	ri_vector_t *dst,
 				  const RtVector     src);
extern DLLEXPORT void    ri_vector_set4	 (	ri_vector_t *dst,
 				  const RtFloat      x,
				  const RtFloat      y,
				  const RtFloat      z,
				  const RtFloat      w);
#ifdef NDEBUG
#ifdef WITH_ALTIVEC
#define ri_vector_copy(dst, src) (dst)->v = (src)->v
#else
#define ri_vector_copy(dst, src) {					\
	(dst)->e[0] = (src)->e[0];					\
	(dst)->e[1] = (src)->e[1];					\
	(dst)->e[2] = (src)->e[2];					\
	(dst)->e[3] = (src)->e[3];					\
}
#endif
#else 
extern DLLEXPORT void    ri_vector_copy	 (	ri_vector_t *dst,
 				  const ri_vector_t *src);
#endif
extern DLLEXPORT void    ri_vector_zero	 (	ri_vector_t *dst);
extern DLLEXPORT void    ri_vector_neg     (	ri_vector_t *dst);
extern DLLEXPORT void    ri_vector_mul     (	ri_vector_t *dst,
 				  const ri_vector_t *a,
 				  const ri_vector_t *b);
extern DLLEXPORT double  ri_vector_ave     (const ri_vector_t *src);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#define xcomp(src) (src)->e[0]
#define ycomp(src) (src)->e[1]
#define zcomp(src) (src)->e[2]
#define wcomp(src) (src)->e[3] 


#endif

