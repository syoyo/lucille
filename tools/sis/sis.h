/*
 * An Implementation of
 * "Structured Importance Smapling of Environment Maps"
 *
 * $Id: sis.h,v 1.1.1.1 2004/01/06 13:57:18 syoyo Exp $
 */

#ifndef SIS_H
#define SIS_H

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _pixelinfo_t
{
	float c[3];		/* RGB color			*/
	float intensity;	/* Y portion of RGB -> YCbCr 	*/
	float domega;		/* differential solid angle	*/
	int   level;		/* layer ID			*/
	int   label;		/* connected componets ID	*/
	int   valid;		/* Is this pixel is usabe?	*/
	int   x, y;		/* pixel position in the map	*/
} pixelinfo_t;

extern pixelinfo_t *ri_sis(float *texels, int nsamples, int width, int height);
extern double impfunc(float intensity, float domega);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif

