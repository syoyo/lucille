/*
 * RiDeclare implementaion.
 *
 * $Id: declare.h,v 1.1.1.1 2004/01/06 13:57:08 syoyo Exp $
 */

#ifndef DECLARE_H
#define DECLARE_H

#include "ri.h"
#include "render.h"

#ifdef __cplusplus
extern "C" {
#endif

void ri_declare_get(ri_context_t *ctx, const char *name);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif

