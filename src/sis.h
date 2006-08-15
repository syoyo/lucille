/*
 * An Implementation of
 * "Structured Importance Smapling of Environment Maps"
 *
 * $Id: sis.h,v 1.1.1.1 2004/01/06 13:57:13 syoyo Exp $
 */

#ifndef SIS_H
#define SIS_H

#include "vector.h"
#include "texture.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _ri_sis_t
{
	ri_texture_t *texture;		/* Environment Map	*/
	int           nstrata;		/* number of strata	*/
} ri_sis_t;

extern void ri_sis_setup(ri_sis_t *sis);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif

