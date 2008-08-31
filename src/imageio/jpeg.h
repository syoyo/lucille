/*
 * $Id: jpeg.h,v 1.1.1.1 2004/01/06 13:57:09 syoyo Exp $
 *
 * jpeg image loader.
 *
 */
#ifndef LUCILLE_JPEG_H
#define LUCILLE_JPEG_H

#ifdef __cplusplus
extern "C" {
#endif

extern unsigned char *jpeg_load(
    FILE *fp,
    int  *width,
    int  *height,
    int  *compos);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif
