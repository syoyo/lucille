/*
 *
 * Memory manager.
 *
 * $Id: memory.h,v 1.1.1.1 2004/01/06 13:57:09 syoyo Exp $
 */

#ifndef RI_MEMORY_H
#define RI_MEMORY_H

#include <stdlib.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define RI_MEM_DEFAULT_ALIGN   (16)

/* Allocates 16 byte aligned memory */
extern void *ri_mem_alloc(size_t byte);

/* `align' will be ceiled to the multiple of 16. */
extern void *ri_mem_alloc_aligned(size_t byte, uint32_t align);

extern int   ri_mem_free (void *ptr);

/* free a memory allocated by ri_mem_alloc_aligned() */
extern int   ri_mem_free_aligned (void *ptr);

extern void *ri_mem_copy (void *dest, const void *src,     size_t n);

#ifdef __cplusplus
}    /* extern "C" */
#endif

#endif

