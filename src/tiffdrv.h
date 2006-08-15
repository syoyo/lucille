#ifndef TIFFDRV_H
#define TIFFDRV_H

#include "ri.h"

#ifdef __cplusplus
extern "C" {
#endif

int tiff_dd_open(const char *name, int width, int height,
		 int bits, RtToken component, const char *format);
int tiff_dd_write(int x, int y, const void *pixel);
int tiff_dd_close(void);
int tiff_dd_progress(void);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif
