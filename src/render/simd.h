#ifndef SIMD_H
#define SIMD_H

/*
 * Some useful macros(x86 sse only)
 */
#ifdef WITH_SSE

#include <xmmintrin.h>

#define fvec4 __m128

#define fvec4_add(a, b) _mm_add_ps((a), (b))


#endif	/* WITH_SSE */

#endif
