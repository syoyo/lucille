/*
 * ILM's OpenEXR image format display driver.
 *
 * $Id: openexrdrv.h,v 1.1 2004/04/29 16:09:55 syoyo Exp $
 */
#ifndef OPENEXRDRV_H
#define OPENEXRDRV_H

#include "ri.h"

#ifdef __cplusplus
extern "C" {
#endif

int openexr_dd_open(const char *name, int width, int height,
		    int bits, RtToken component, const char *format);
int openexr_dd_write(int x, int y, const void *pixel);
int openexr_dd_close(void);
int openexr_dd_progress(void);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif
