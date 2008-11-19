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

#include <stdint.h>



static inline uint64_t alignsize(uint64_t sz, uint32_t align)
{
    return (sz + (align - 1)) & ~(align - 1);
}

static inline uint64_t alignup(uint64_t sz, uint32_t align)
{
    uint64_t s = alignsize(sz, align);

    if (s == sz) return s + align;      /* align up */
    else         return s;
}

static inline void *alignptr(void *addr, uint32_t align)
{
    uintptr_t mask = ~((uintptr_t)(align - 1));

    return (void *)(((uintptr_t)addr + (align - 1)) & mask);
}

void *
ri_mem_alloc(size_t byte)
{
    void *p;

    assert(byte != 0);

    p = malloc(byte);
    assert(p != NULL);

    return p;
}

/*
 *    _ malloc() point
 *   /
 *  /          _ aligned point
 * /          /
 * +----+-----+---------------------------------------
 * |    |     |
 * +----+--|--+---------------------------------------
 * ^       |
 * |       | embeded address of malloc().
 * +-------+
 * 
 */
void *
ri_mem_alloc_aligned(size_t sz, uint32_t align)
{
    uint64_t  size;
    void     *p;
    void     *aligned;
    uint64_t  diff;

    assert(align > 0);
    assert(align % 16 == 0);

    size = alignup((uint64_t)sz, align);

    size += 8;   /* room for embed the address */

    p       = malloc(size);
    aligned = alignptr(p, align);
    if (aligned == p) {
        aligned += align;       // align up
    }

    diff = (uintptr_t)(aligned - p);
    /*
    printf("malloc  point : %p\n", p);
    printf("aligned point : %p\n", aligned);
    printf("diff          : %llu\n", diff);
    */


    /*
     * Embed malloc() address
     */
    *((uint64_t *)aligned - 1) = (uintptr_t)p;

    assert(aligned != NULL);

    return aligned;
}

int
ri_mem_free(void *ptr)
{

    if (ptr == NULL) {
        return -1;
    }

    free(ptr);

    return 0;   // OK

}

int
ri_mem_free_aligned(void *ptr)
{
    void *free_addr;

    if (ptr == NULL) {
        return -1;
    }

    // printf("free: in ptr = %p\n", ptr);

#ifdef __64bit__
    free_addr = (void *)(*((uint64_t *)ptr - 1));
#else
    free_addr = (void *)(uint32_t)(*((uint64_t *)ptr - 1));
#endif

    if (free_addr == NULL) return -1;

    // printf("free: %p\n", free_addr);

    free(free_addr);

    return 0;
}


void *
ri_mem_copy(void *dest, const void *src, size_t n)
{
    memcpy(dest, src, n);

    return dest;
}
