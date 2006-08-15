/*
 * ILM's OpenEXR image format display driver
 *
 * $Id: openexrdrv.c,v 1.2 2004/05/04 02:28:45 syoyo Exp $
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef HAVE_OPENEXR
#include <OpenEXR/ImfCRgbaFile.h>
#endif

#include "memory.h"
#include "openexrdrv.h"
#include "log.h"

#ifdef HAVE_OPENEXR
static ImfRgba       *gbuf;			/* pixel buffer */
static ImfOutputFile *gfp;
static int            gwidth, gheight;
#endif

int
openexr_dd_open(const char *name, int width, int height,
	     int bits, RtToken component, const char *format)
{
#if HAVE_OPENEXR

	int i;
	ImfHeader     *header;

	(void)bits;

	if (strcmp(format, "float") != 0 ) {
		ri_log(LOG_ERROR, "currently openexr output supports float format");
		return 0;
	}

	if (strcmp(component, RI_RGB) != 0) {
		ri_log(LOG_WARN, "currently only supports rgb component");
	}

	header = ImfNewHeader();
	ImfHeaderSetDisplayWindow(header, 0, 0, width - 1 , height - 1);
	ImfHeaderSetDataWindow(header, 0, 0, width - 1, height - 1);
	ImfHeaderSetScreenWindowWidth(header, width);
	ImfHeaderSetStringAttribute(header, "owner", "lucille");

	gfp = ImfOpenOutputFile(name, header, IMF_WRITE_RGBA);
	if (!gfp) {
		printf("[error] Can't open file [ %s ] for save.\n", name);
		return 0;
	}
	ImfDeleteHeader(header);


	gbuf = (ImfRgba *)ri_mem_alloc(width * height * sizeof(ImfRgba));
	for (i = 0; i < width * height; i++) {
		ImfFloatToHalf(0.0f, &gbuf[i].r);
		ImfFloatToHalf(0.0f, &gbuf[i].g);
		ImfFloatToHalf(0.0f, &gbuf[i].b);
		ImfFloatToHalf(1.0f, &gbuf[i].a);
	}

	ImfOutputSetFrameBuffer(gfp,
				gbuf,		/* pointer to pixels	*/
				1,		/* xstride		*/
				width);		/* ystride		*/ 

	gwidth     = width;	
	gheight    = height;

#endif

	return 1;
}

int
openexr_dd_write(int x, int y, const void *pixel)
{
#if HAVE_OPENEXR
	float *col;
	ImfHalf hf[3];

	if (x < 0) return 0;
	if (x >= gwidth) return 0;
	if (y < 0) return 0;
	if (y >= gheight) return 0;

	col = (float *)pixel;

	ImfFloatToHalf(col[0], &hf[0]);
	ImfFloatToHalf(col[1], &hf[1]);
	ImfFloatToHalf(col[2], &hf[2]);

	gbuf[x + y * gwidth].r += hf[0];
	gbuf[x + y * gwidth].g += hf[1];
	gbuf[x + y * gwidth].b += hf[2];

#else

	(void)x;
	(void)y;
	(void)pixel;

#endif

	return 1;
}

int
openexr_dd_close()
{
#if HAVE_OPENEXR

	ImfOutputWritePixels(gfp, gheight);

	ImfCloseOutputFile(gfp); gfp = NULL;

	ri_mem_free(gbuf);

#endif
	return 1;
}

int
openexr_dd_progress()
{
	/* progress() is currently not used. */
	return 1;
}
