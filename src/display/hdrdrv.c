/*
 * $Id: hdrdrv.c,v 1.4 2004/04/16 13:46:45 syoyo Exp $
 *
 * Gred Ward's RGBE high dynamic range image format display driver.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "hdrdrv.h"
#include "rgbe.h"
#include "log.h"

static FILE  *gfp;
static float *gbuf;
static int    gwidth, gheight;

int
hdr_dd_open(const char *name, int width, int height,
	     int bits, RtToken component, const char *format)
{
	gfp = NULL;
	gfp = fopen(name, "wb");
	if (!gfp) return 0;

	if (strcmp(format, "float") != 0) {
		ri_log(LOG_ERROR, "(Disp) .hdr format requires float format for input pixel");
		return 0;
	}

	if (bits != 32) {
		ri_log(LOG_ERROR, "(Disp) .hdr format requires 32 bit per component for input pixel");
		return 0;
	}

	if (strcmp(component, RI_RGB) != 0) {
		ri_log(LOG_WARN, "(Disp) Component is not RI_RGB");
	}

	RGBE_WriteHeader(gfp, width, height, NULL);
	
	gbuf = (float *)ri_mem_alloc(width * height * sizeof(float) * 3);
	if (!gbuf) {
		ri_log(LOG_ERROR, "(Disp) Can't alloc memory for the buffer.");
		return 0;
	}

	memset(gbuf, 0, width * height * sizeof(float) * 3);
	gwidth     = width;	
	gheight    = height;

	ri_log(LOG_INFO, "(Disp) Output written to \"%s\"", name);

	return 1;
}

int
hdr_dd_write(int x, int y, const void *pixel)
{
	float col[3];
	int index;
	if (x < 0) return 0;
	if (x >= gwidth) return 0;
	if (y < 0) return 0;
	if (y >= gheight) return 0;

	index = 3 * (x + y * gwidth);

	col[0] = ((float *)pixel)[0];
	col[1] = ((float *)pixel)[1];
	col[2] = ((float *)pixel)[2];

	if (col[0] < 0.0) col[0] = 0.0;
	if (col[1] < 0.0) col[1] = 0.0;
	if (col[2] < 0.0) col[2] = 0.0;

	/* additive pixel writing */
	gbuf[index + 0] += col[0];
	gbuf[index + 1] += col[1];
	gbuf[index + 2] += col[2];

	return 1;
}

int
hdr_dd_close()
{
	/* rle encoded */
	RGBE_WritePixels_RLE(gfp, gbuf, gwidth, gheight);
	//RGBE_WritePixels(gfp, gbuf, gwidth * gheight);
	
	fclose(gfp);

	ri_mem_free(gbuf);

	return 1;
}

int
hdr_dd_progress()
{
	return 1;
}
