/*
 * Filter functions.
 *
 * $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <math.h>

#include "ri.h"

#if 0
/*
 * Function ri_filter_sample
 *
 *     Filters a sample using a filter specified by RiPixelFilter.
 *     Then output filtered sample color to dipsplay driver.
 *
 * Parameters:
 *
 *     *pixels    - An array of pixels which points to the image plane
 *                  in a bucket.
 *      width     - The width of image plane.
 *      height    - The height of image plane.
 *      samplex   - The X position of a sample. 
 *      sampley   - The Y position of a sample. 
 *     *samplecol - The color of a sample.
 *
 * Returns:
 * 
 *     O if no error occurs.
 */
int
ri_filter_sample(ri_vector_t *pixels,			/* output */
		 int   width, int height,
		 float samplex, float sampley,
		 const ri_vector_t *samplecol)
{
	ri_option_t *opt;
	RtFilterFunc filter;
	float        widthx;
	float        widthy;
	int          sx, sy;	/* start pixel the filter covers	*/
	int          ex, ey;	/* end pixel the filter covers		*/
	int          x, y;
	float        w;		/* filter weight			*/
	float        wcol[3];	/* weigted color of sample		*/

	opt = ri_render_get()->context->option;

	filter = opt->pixel_filter;
	if (filter == NULL) {
		fprintf(stderr, "No pixel filter!!\n");
		exit(-1);
	}

	widthx = opt->pixel_filter_widthx;
	widthy = opt->pixel_filter_widthy;

	sx = (int)ceil(samplex - widthx - 0.5f);
	ex = (int)floor(samplex + widthx - 0.5f);
	sy = (int)ceil(sampley - widthy - 0.5f);
	ey = (int)floor(sampley + widthy - 0.5f);

	if (sx < 0) sx = 0;
	if (ex > width - 1) ex = width - 1;
	if (sy < 0) sy = 0;
	if (ey > height - 1) ey = height - 1;

	printf("sx, sy = %d, %d\n", sx, sy);
	printf("ex, ey = %d, %d\n", ex, ey);

	/*
	 * distribute weighted sample color to the pixels
	 * where the filter covers.
	 */
	for (y = sy; y < ex; y++) {
		for (x = sx; x < ex; x++) {
			/* evaluate the filter */
			w = filter(samplex - (float)x + 0.5f,
				   sampley - (float)y + 0.5f,
				   widthx, widthy);

			wcol[0] = samplecol->e[0] * w;
			wcol[1] = samplecol->e[1] * w;
			wcol[2] = samplecol->e[2] * w;

			pixels[y * width + x].e[0] += wcol[0];
			pixels[y * width + x].e[1] += wcol[1];
			pixels[y * width + x].e[2] += wcol[2];
		}
	}

	return 0;
}
#endif

/*
 * All filter assumes that input values should be in the
 * ([-xwidth/2, xwidth/2], [-ywidth/2, ywidth/2]) range.
 */

RtFloat
RiBoxFilter(RtFloat x, RtFloat y, RtFloat xwidth, RtFloat ywidth)
{
	(void)x;
	(void)y;
	(void)xwidth;
	(void)ywidth;

	return 1.0;
}

RtFloat
RiTriangleFilter(RtFloat x, RtFloat y, RtFloat xwidth, RtFloat ywidth)
{
	return  ((1.0 - fabs(x)) / (xwidth * 0.5))
	       *((1.0 - fabs(y)) / (ywidth * 0.5)); 
}

RtFloat
RiCatmullRomFilter(RtFloat x, RtFloat y, RtFloat xwidth, RtFloat ywidth)
{
	RtFloat r2 = (x * x + y * y);
	RtFloat r  = sqrt(r2);

	(void)xwidth;
	(void)ywidth;

	return (r >= 2.0) ? 0.0 :
               (r <  1.0) ? (3.0 * r * r2 - 5.0 * r2 + 2.0)
                          : (-r * r2 + 5.0 * r2 - 8.0 * r + 4.0);
}

RtFloat
RiGaussianFilter(RtFloat x, RtFloat y, RtFloat xwidth, RtFloat ywidth)
{
	x *= 2.0 / xwidth;
	y *= 2.0 / ywidth;

	return exp(-2.0 * (x * x + y * y));
}

RtFloat
RiSincFilter(RtFloat x, RtFloat y, RtFloat xwidth, RtFloat ywidth)
{
	RtFloat s, t;

	if (x > -0.001 && x < 0.001)
		s = 1.0;
	else
		s = sin(x) / x;

	if (y > -0.001 && y < 0.001)
		t = 1.0;
	else
		t = sin(y) / y;

	(void)xwidth;
	(void)ywidth;

	return s * t;
}
