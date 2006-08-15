/*
 *
 * Memory manager.
 *
 * $Id: memory.h,v 1.1.1.1 2004/01/06 13:57:09 syoyo Exp $
 */

#ifndef RI_MEMORY_H
#define RI_MEMORY_H

#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

/* this structure is used for SIMD arthmetric operation */
typedef struct _ri_aligned_float_t
{
	float *aligned;			/* points 16-byte alighed location */
	float *real;			/* real memory allocated  */
} ri_aligned_float_t;

#ifdef WITH_DMALLOC
#include <dmalloc.h>

#define ri_mem_alloc(x) malloc((x))
#define ri_mem_free(x)  free((void *)(x))
#define ri_mem_copy(dst, src, size)  memcpy((dst), (src), (size))
#else
extern void *ri_mem_alloc(long byte);
extern void  ri_mem_free (void *ptr);
extern void *ri_mem_copy (void *dest, const void *src, 	size_t n);
#endif

/* allocates 16-byte aligned memory */
extern void ri_aligned_float_alloc(ri_aligned_float_t *dst, long size);
extern void ri_aligned_float_free(ri_aligned_float_t *src);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif

