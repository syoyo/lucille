#ifndef SOCKDRV_H
#define SOCKDRV_H

/*
 * Socket(inter process communication) display driver.
 */

#include "ri.h"

#ifdef __cplusplus
extern "C" {
#endif

int sock_dd_open(const char *name, int width, int height,
		 int bits, RtToken component, const char *format);
int sock_dd_write(int x, int y, const void *pixel);
int sock_dd_close();
int sock_dd_progress();

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif
