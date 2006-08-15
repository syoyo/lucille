/*
 * $Id: framebufferdrv.h,v 1.1.1.1 2004/01/06 13:57:08 syoyo Exp $
 *
 * framebuffer(through OpenGL+glut) display driver.
 */
#ifndef FRAMEBUFFERDRV_H
#define FRAMEBUFFERDRV_H

#include "ri.h"

#ifdef __cplusplus
extern "C" {
#endif

int fb_dd_open(const char *name, int width, int height,
		 int bits, RtToken component, const char *format);
int fb_dd_write(int x, int y, const void *pixel);
int fb_dd_close(void);
int fb_dd_progress(void);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif
