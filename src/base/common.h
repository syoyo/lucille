/*
 *   lucille | Global Illumination renderer
 *
 *             written by Syoyo Fujita.
 *
 */

/*
 * Common defs.
 *
 * $Id: vector.h,v 1.10 2004/08/15 05:19:39 syoyo Exp $
 */
#ifndef LUCILLE_COMMON_H
#define LUCILLE_COMMON_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdint.h>
#include <math.h>
#include <float.h>

#define RI_DOUBLE_PRECISION
#define ri_float_t double
#define RI_FLT_MAX   (DBL_MAX)
#define RI_EPS       (1.0e-14)

#ifdef WIN32
#define DLLEXPORT         __declspec(dllexport )
#else
#define DLLEXPORT
#endif

#ifdef _MSC_VER /* Visual C++ */

#define DECL_ALIGN(x)      __declspec(align((x)))
#define ATTRIB_ALIGN(x)

#else   /* Assume GCC */

#define DECL_ALIGN(x)
#define ATTRIB_ALIGN(x)   __attribute__((aligned((x))))

#endif	/* _MSC_VER */

#ifdef __GNUC__
#define FORCE_INLINE inline __attribute((always_inline))
#else
#define FORCE_INLINE inline
#endif

#ifdef HAVE_RESTRICT    /* The compiler supports restrict keyword. */
#define RESTRICT restrict
#else
#define RESTRICT
#endif


#endif	/* LUCILLE_COMMON_H */
