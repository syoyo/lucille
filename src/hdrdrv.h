#ifndef HDRDRV_H
#define HDRDRV_H

/*
 * Greg Ward's Radiance .hdr(rgbe format) file format display driver.
 */

#include "ri.h"

#ifdef __cplusplus
extern "C" {
#endif

int hdr_dd_open(const char *name, int width, int height,
		 int bits, RtToken component, const char *format);
int hdr_dd_write(int x, int y, const void *pixel);
int hdr_dd_close(void);
int hdr_dd_progress(void);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif
