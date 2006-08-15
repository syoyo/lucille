/*
 * RenderMan Display options
 *
 * $Id: display.h,v 1.5 2004/04/16 13:46:45 syoyo Exp $
 */

#ifndef DISPLAY_H
#define DISPLAY_H

#include "ri.h"
#include "matrix.h"

typedef struct _quantizer_t
{
	RtInt   one;
	RtInt   maximum;
	RtInt   minimum;
	RtFloat dither_amplitude;
} ri_quantizer_t;

typedef struct _ri_display_t
{
	/* 
	 * Estimated variance of the computed pixel value from the
	 * true pixel value.
	 */
	RtFloat pixel_variance;

	/*
	 * Effective sampling rate in the horizontal and vertical
	 * directions. 
	 */
	RtFloat sampling_rates[2];

	/*
	 * Type of filterig and width of the fiter in the horizontal and
	 * vertical directions.
	 */
	RtFilterFunc filter;
	RtFloat filter_width[2];

	/* Gain and gamma of the exposure process. */
	RtFloat gain;
	RtFloat gamma;

	/* A procedure defining an image or pixel operator. */
	/* Not implemented yet. */
	/* RtToken imager */

	/* Color and opacity quantization parameters. */
	ri_quantizer_t color_quantizer;

	/* Depth quantization parameters. */
	ri_quantizer_t depth_quantizer;

	/* Whether the display is a framebuffer or a file. */
	RtToken display_type;

	/* Name of the display device or file. */
	RtString display_name;

	/* Image output type. */
	RtToken display_mode;

	/* Image output format */
	RtToken display_format;
} ri_display_t;

/*
 * display driver structure
 */
typedef struct _ri_display_drv_t
{
	int (* open)(const char *name, int width, int height,
		     int bits, RtToken component, const char *format);
	int (* close)(void);
	int (* write)(int x, int y, const void *pixel);
	int (* progress)(void);
	char *name;
	char *info;			/* DD information string */
} ri_display_drv_t;

#ifdef __cplusplus
extern "C" {
#endif

ri_display_t *ri_display_new ();
void          ri_display_free(ri_display_t *display);


/* hack for user callback display driver. */
void ri_dd_callback(int (*opencb)(const char *name,
				  int width, int height,
			          int bits, RtToken component,
				  const char *format),
		    int (*closecb)(void),
		    int (*writecb)(int x, int y, const void *pixel));
 
#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif
