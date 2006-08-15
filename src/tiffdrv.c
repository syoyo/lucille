#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef HAVE_LIBTIFF
#include <tiffio.h>
#endif

#include "memory.h"
#include "tiffdrv.h"
#include "log.h"

#ifdef HAVE_LIBTIFF
static TIFF *gtif;
static char *gbuf;
static int   gscanwidth;
static int   gwidth, gheight;
static int   gbytes;
#endif

int
tiff_dd_open(const char *name, int width, int height,
	     int bits, RtToken component, const char *format)
{
#if HAVE_LIBTIFF
	int i;
	int bytes = 1;
	int sampleformat = 3;

	gtif = NULL;
	gtif = TIFFOpen(name, "w");
	if (!gtif) return 0;

	if (strcmp(format, "byte")  != 0 && 
	    strcmp(format, "float") != 0) {
		ri_log(LOG_WARN, "currently tiff output supports byte & float format");
	}

	if (strcmp(format, "float") == 0) {
		bytes = 4;
		sampleformat = SAMPLEFORMAT_IEEEFP;
	}

	if (strcmp(component, RI_RGB) != 0) {
		ri_log(LOG_WARN, "currently only supports rgb component");
	}

	TIFFSetField(gtif, TIFFTAG_COMPRESSION, COMPRESSION_NONE);
	TIFFSetField(gtif, TIFFTAG_ORIENTATION, ORIENTATION_TOPLEFT);
	TIFFSetField(gtif, TIFFTAG_SUBFILETYPE, (unsigned short)0);
	TIFFSetField(gtif, TIFFTAG_SOFTWARE, "lucille");
	TIFFSetField(gtif, TIFFTAG_BITSPERSAMPLE, bits);


	TIFFSetField(gtif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_RGB);
	TIFFSetField(gtif, TIFFTAG_SAMPLESPERPIXEL, 3);
	TIFFSetField(gtif, TIFFTAG_SAMPLEFORMAT, sampleformat);
	TIFFSetField(gtif, TIFFTAG_IMAGEWIDTH, width);
	TIFFSetField(gtif, TIFFTAG_IMAGELENGTH, height);
	TIFFSetField(gtif, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
	TIFFSetField(gtif, TIFFTAG_XRESOLUTION, 1.0);
	TIFFSetField(gtif, TIFFTAG_YRESOLUTION, 1.0);
	TIFFSetField(gtif, TIFFTAG_RESOLUTIONUNIT, 1);
	TIFFSetField(gtif, TIFFTAG_ROWSPERSTRIP, TIFFDefaultStripSize(gtif, -1));

	gbuf = (char *)ri_mem_alloc(width * height * 3 * bytes);
	for (i = 0; i < width * height * 3 * bytes; i++) {
		gbuf[i] = 0;
	}

	gbytes     = bytes;
	gscanwidth = width * 3 * bytes;
	gwidth     = width;	
	gheight    = height;

#endif
	return 1;
}

int
tiff_dd_write(int x, int y, const void *pixel)
{
#if HAVE_LIBTIFF
	if (x < 0) return 0;
	if (x >= gwidth) return 0;
	if (y < 0) return 0;
	if (y >= gheight) return 0;

	memcpy(&(gbuf[3 * gbytes * (x + y * gwidth)]), pixel, 3 * gbytes);

#else
	(void)x;
	(void)y;
	(void)pixel;
#endif

	return 1;
}

int
tiff_dd_close()
{
#if HAVE_LIBTIFF
	int i;
	unsigned char *data;
	
	for (i = 0; i < gheight; i++) {
		data =  gbuf + (i * gscanwidth);
		TIFFWriteScanline(gtif, data, i, 0);
	}

	TIFFClose(gtif);

	ri_mem_free(gbuf);

#endif
	return 1;
}

int
tiff_dd_progress()
{
	return 1;
}
