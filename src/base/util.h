/*
 * utility routines
 *
 * $Id: util.h,v 1.1.1.1 2004/01/06 13:57:13 syoyo Exp $
 */

#ifndef UTIL_H
#define UTIL_H

#include <stdarg.h>
#include <math.h>

#include "ri.h"

#ifdef __cplusplus
extern "C" {
#endif

/* used for hash routine */
extern unsigned int ri_util_closest_prime(unsigned int n);
extern unsigned int ri_util_min_prime    ();
extern unsigned int ri_util_max_prime    ();

extern unsigned int ri_util_paramlist_build(va_list     arg,
                        RtToken   **tokens,
                        RtPointer **values);
extern void         ri_util_paramlist_free (RtToken    *tokens,
                        RtPointer  *values);

/* 1 if little-endian, 0 if big-endian */
extern int ri_util_is_little_endian();

#define floateq(a,b) fabs((a) - (b)) < RI_EPSILON ? 1 : 0

#ifdef __cplusplus
}    /* extern "C" */
#endif

#endif

