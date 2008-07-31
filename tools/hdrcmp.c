/*
 * $Id: hdrcmp.c,v 1.2 2004/01/16 07:12:53 syoyo Exp $
 *
 * Compare two Radiance .hdr image.
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "rgbe.h"

double compare(float *img1, float *img2, int len, int l2)
{
	int i;
	double diff = 0.0;
	double x;

	for (i = 0; i < len; i++) {
		if (isnan(img1[i]) || isnan(img2[i])) continue;

		if (l2) {
			x = (double)img1[i] - (double)img2[i];
			diff += x * x;
		} else {
			diff += fabs((double)img1[i] - (double)img2[i]);
		}

	}

	if (l2) {
		diff /= (double)len;
	}

	return diff;
}

int
main(int argc, char **argv)
{
	int              w1, w2;
	int              h1, h2;
	int              ret;
	int              l2;
	double           diff;
	rgbe_header_info info;
	float            *img1, *img2;
	FILE             *fp1, *fp2;
 
	if (argc < 3) {
		printf("usage: hdrcmp image1.hdr image2.hdr\n");
		exit(-1);
	}

	l2 = 0;
	if (argc > 3) {
		if (strcmp(argv[3], "-l2") == 0) {
			/* compute L2 norm. */
			l2 = 1;
		}
	}

	fp1 = fopen(argv[1], "rb");
	if (!fp1) {
		printf("-1\n");
		exit(-1);
	}

	fp2 = fopen(argv[2], "rb");
	if (!fp2) {
		printf("-1\n");
		exit(-1);
	}

	ret = RGBE_ReadHeader(fp1, &w1, &h1, &info);
	if (ret != RGBE_RETURN_SUCCESS) {
		printf("-1\n");
		exit(-1);
	}

	ret = RGBE_ReadHeader(fp2, &w2, &h2, &info);
	if (ret != RGBE_RETURN_SUCCESS) {
		printf("-1\n");
		exit(-1);
	}

	if ((w1 != w2) || (h1 != h2)) {
		printf("-1\n");
		exit(-1);
	}

	img1 = (float *)malloc(sizeof(float) * w1 * h1 * 3);
	img2 = (float *)malloc(sizeof(float) * w2 * h2 * 3);

	ret = RGBE_ReadPixels_RLE(fp1, img1, w1, h1);
	if (ret != RGBE_RETURN_SUCCESS) {
		printf("-1\n");
		exit(-1);
	}

	ret = RGBE_ReadPixels_RLE(fp2, img2, w2, h2);
	if (ret != RGBE_RETURN_SUCCESS) {
		printf("-1\n");
		exit(-1);
	}

	diff = compare(img1, img2, w1 * h1, l2);
	printf("%16.16lf\n", diff);

	exit(0);
}
