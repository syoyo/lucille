/*
 * $Id: memory.c,v 1.2 2004/07/09 12:15:43 syoyo Exp $
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#if defined(_MSC_VER)
#include <malloc.h>
#endif
#include "memory.h"

static void muda();

#ifndef WITH_DMALLOC
void *
ri_mem_alloc(long byte)
{
	/* \todo more elegant... */
	void *ptr = NULL;

#if defined(WITH_SSE) && defined(_MSC_VER)
	// force 16byte aligned
	ptr = _aligned_malloc(byte, 16);
#elif defined(__APPLE__) && defined(__MACH__)
	//ptr = valloc(byte);
	ptr = malloc(byte);
#else
	ptr = malloc(byte);
#endif

	if (ptr == NULL) muda();
	
	return ptr;
}

void
ri_mem_free(void *ptr)
{
	if (ptr != NULL) {
#if defined(WITH_SSE) && defined(_MSC_VER)
		_aligned_free(ptr);
#else
		free(ptr);
#endif
	}

	return;
}

void *
ri_mem_copy(void *dest, const void *src, size_t n)
{
	memcpy(dest, src, n);

	return dest;
}

static void
muda()
{
	printf("muda muda muda!!!\n");
	exit(-1);
}
#endif

void
ri_aligned_float_alloc(ri_aligned_float_t *dst, long size)
{
	dst->real = (void *)ri_mem_alloc(sizeof(float) * size + 16);
	dst->aligned = (void *)((long)(dst->real) +
				(16 - ((long)(dst->real) - 1) % 16) - 1);

}

void 
ri_aligned_float_free(ri_aligned_float_t *src)
{
	if (src && src->real) ri_mem_free(src->real);
}
