/*
 * $Id: loader_obj.h,v 1.1.1.1 2004/01/06 13:57:09 syoyo Exp $
 *
 * tab=8, indent=8
 *
 * smf loader routine.
 *
 */

#ifndef LOADER_OBJ_H
#define LOADER_OBJ_H

#ifdef __cplusplus
extern "C" {
#endif

#include "geom.h"
#include "render.h"

extern void load_obj(ri_render_t *render, const char *filename, int debug);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif
