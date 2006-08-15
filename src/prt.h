/*
 * precompute radiance transfer code.
 *
 * $Id: prt.h,v 1.1.1.1 2004/01/06 13:57:11 syoyo Exp $
 */

#ifndef PRT_H
#define PRT_H

#ifdef __cplusplus
extern "C" {
#endif

/* do global illumination simulation over an object. */
extern void         ri_prt_sample();

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif

