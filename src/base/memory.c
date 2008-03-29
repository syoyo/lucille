/*
 * $Id: memory.c,v 1.2 2004/07/09 12:15:43 syoyo Exp $
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#if defined(_MSC_VER)
#include <malloc.h>
#endif
#include "memory.h"
#include "list.h"

static void muda();

//#define LOCAL_DEBUG


typedef struct _ptr_info_t
{
	void *aligned_ptr;	// points aligned addr
	void *base_ptr;		// used for free()
} ptr_info_t;

ri_list_t *g_large_mem_list;	// TODO: use binary tree for fast look up.

typedef struct _mem_block_t
{

} mem_block_t;

// ceil 'val' by 'c'
// Ex. ceil(14, 16) = 16
unsigned long myceil(unsigned long val, unsigned long c)
{
    return ((val + (c - 1)) & (~(c - 1)));
}

void *
aligned_malloc(size_t size, unsigned int align)
{
    //
    //  +-- malloc addr
    //  |
    //  |           +-- aligned addr
    //  |           |
    //  |/          |/
    //  +---+-------+--------------------
    //  | ^ | diff  |
    //  +-|-+---|---+--------------------
    //    |     |
    //    +-----+ aligned addr - diff
    //
    //

    assert(align < 1024 * 1024);

    unsigned int align16 = myceil(align, 16);
    if (align16 == 0) align16 = 16;

    void *ptr = malloc(size + align16 + sizeof(int));
    if (ptr == NULL) return NULL;

    void *aligned_ptr = (void *)(myceil((unsigned long)ptr, align16) + align16);

    // Embed distance(difference) between ptr and aligned_ptr to the address
    // addr(aligned_ptr) - sizeof(int).
    // sizeof(int) is suffice because difference is small.

    int diff = (int)((unsigned long)aligned_ptr - (unsigned long)ptr);

    int *embed_ptr = (int *)((unsigned long)aligned_ptr - sizeof(int));
    (*embed_ptr) = diff;

    return aligned_ptr;

}

//
// addr must be the pointer allocated by aligned_malloc()
//
int
aligned_free(void *addr)
{

    int *embed = (int *)((unsigned long)addr - sizeof(int));
    int  diff  = (*embed);

    void *freed_add = (void *)((unsigned long)addr - diff);

    free(freed_add);

    return 0;

}


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

int
ri_mem_free(void *ptr)
{
	if (ptr != NULL) {
#if defined(WITH_SSE) && defined(_MSC_VER)
		_aligned_free(ptr);
#else
		free(ptr);
#endif
	}

	return 0;
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

static __inline void *
aligned_addr(void *ptr, unsigned int align)
{
#if defined(__x86_64__)
	unsigned long long val;
	val = (unsigned long long)ptr + (align-1);
	val = val & (~((unsigned long long)(align-1)));

	return (void *)val;
#else
	return (void *)(((unsigned long)ptr + (align-1)) & (~(align-1)));
#endif
}

static __inline size_t
aligned_size(size_t size, unsigned int align)
{
	return ((size + (align-1)) & (~(align-1)));
}

void *
ri_aligned_alloc(size_t size, unsigned int align)
{
	size_t sz;
	ptr_info_t *info;

	if (g_large_mem_list == NULL) {
		g_large_mem_list = ri_list_new();
	}
	
	info = (ptr_info_t *)malloc(sizeof(ptr_info_t));
	assert(info);

	sz = aligned_size(size, align);

	info->base_ptr = malloc(sz+align);
	info->aligned_ptr = aligned_addr(info->base_ptr, align);

	ri_list_append(g_large_mem_list, info);

#ifdef LOCAL_DEBUG
#if defined(__x86_64__)
	fprintf(stderr, "aligned_alloc: base = %p, aligned = %p, alignment=%d\n", info->base_ptr, info->aligned_ptr, align);
#else
	fprintf(stderr, "aligned_alloc: base = %p, aligned = %p, alignment=%d\n", info->base_ptr, info->aligned_ptr, align);
#endif
#endif

	return info->aligned_ptr;
}

void
ri_aligned_free(void *ptr)
{
	int found = 0;
	ri_list_t *p;
	ptr_info_t *info;

	for (p = ri_list_first(g_large_mem_list);
	     p != NULL;
	     p = ri_list_next(p)) {
		info = (ptr_info_t *)p->data;
		if (info->aligned_ptr == ptr) {
			// TODO
			//free(info->base_ptr);
			found = 1;
			break;
		}
	}

	if (found) {
		// TODO:
		// g_large_mem_list = ri_list_delete(p);
#ifdef LOCAL_DEBUG
		fprintf(stderr, "aligned_free() OK\n");
#endif

	} else {
		fprintf(stderr, "ri_aligned_free() called with unknown ptr addr.\n");
	}
}
