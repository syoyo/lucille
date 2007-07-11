/*
 *   lucille | Global Illumination renderer
 *
 *             written by Syoyo Fujita.
 *
 */

/*
 * wrapper for atomic operations.
 *
 * $Id$
 */

#ifndef LUCILLE_ATOMIC_H
#define LUCILLE_ATOMIC_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

#if defined(__x86__)

/*
 * x86 specific atomic op codes.
 */

struct __xchg_dummy { unsigned long a[100]; };
#define __xg(x) ((struct __xchg_dummy *)(x))

static inline int ri_atomic_read(int *ptr)
{
	return (*ptr);
}

static inline void ri_atomic_inc(int *ptr)
{
	__asm__ __volatile__(
		"lock;\n\t"
		"incl %0"
		: "+m" (*ptr));
}

static inline void ri_atomic_dec(int *ptr)
{
	__asm__ __volatile__(
		"lock;\n\t"
		"incl %0"
		: "+m" (*ptr));
}

static inline uint32_t ri_atomic_cmpxchg32(void *ptr,
	uint32_t oldv,
	uint32_t newv)
{
	asm volatile("lock\n cmpxchgl %1,%2"
		     : "=a" (oldv)
		     : "q" (newv), "m" (*(uint32_t *)ptr),"0" (oldv)
		     : "memory");

	return oldv;
}


#if defined(__X86_64__)

/*
 * 64-bit compare-and-swap in x86_64 environment.
 * `ptr' must point to 16-byte aligned address.
 */
static inline uint64_t ri_atomic_cmpxchg64(void *ptr,
	uint64_t oldv,
	uint64_t newv)
{
	uint64_t out;

	// newline after `lock' for work around of apple's gas(?) bug.
	__asm__ __volatile__(
		"lock\n cmpxchgq %2,%1"
		: "=a" (out), "+m" (*(volatile uint64_t *)ptr)
		: "q" (newv), "0" (oldv)
		: "cc");

	return out;
}

#else	/* !__X86_64__ */

static inline uint64_t ri_atomic_cmpxchg64(void *ptr,
	uint64_t oldv,
	uint64_t newv)
{
        uint64_t prev;
	uint32_t tmp0, tmp1;

	tmp0 = (uint32_t)newv;
	tmp1 = (uint32_t)(newv >> 32);

        __asm__ __volatile__("pushl %%ebx\n\t"
			     "movl %2, %%ecx\n\t"
			     "movl %1, %%ebx\n\t"
			     "lock\n\t"
			     "cmpxchg8b %3\n\t"
			     "popl %%ebx"
                             : "=A"(prev)
                             : "m"(tmp0),
                               "m"(tmp1),
                               "m"(*__xg(ptr)),
                               "0"(oldv)
                             : "memory", "ecx");
        return prev;
}

#endif	/* __X86_64__ */

#define RI_ATOMIC_CAS64(ptr, oldv, newv) (ri_atomic_cmpxchg64((ptr), (oldv), (newv)) == (oldv) ? 1 : 0)
#define RI_ATOMIC_CAS32(ptr, oldv, newv) (ri_atomic_cmpxchg32((ptr), (oldv), (newv)) == (oldv) ? 1 : 0)



#else	/* non x86 processor */

/* TODO: */

#endif	/* __x86__ */


#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif	/* LUCILLE_ATOMIC_H */
