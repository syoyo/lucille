/*
 * $Id: jpeg.h,v 1.1.1.1 2004/01/06 13:57:17 syoyo Exp $
 *
 * tab=8, indent=8
 *
 * jpeg image load utility.
 */
#ifndef JPEG_H
#define JPEG_H

#ifdef __cplusplus
extern "C" {
#endif

extern unsigned char *jpeg_load(const char *filename,
				int *width, int *height, int *compos);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif
