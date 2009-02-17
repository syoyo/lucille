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
 * File: memory.c
 *
 *   Provides memory manager over lucille rendering system.
 *   Also provides align-assured malloc() which is required for SIMD coding.
 *
 * ------------------------------------------------------------------------ */

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
