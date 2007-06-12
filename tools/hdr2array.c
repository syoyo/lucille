/*
 * $Id: hdr2png.c,v 1.1 2004/02/11 05:32:12 syoyo Exp $
 *
 * Compare two Radiance .hdr image.
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define CIE_rf 0.265074126
#define CIE_gf 0.670114631
#define CIE_bf 0.064811243
#define INTENSITY(v) (CIE_rf * (v)[0] + CIE_gf  * (v)[1] + CIE_bf * (v)[2])


#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "rgbe.h"

float scale = 1.0; 

void
write_array(const char *filename, float *img, int width, int height)
{
	int i, j;
	FILE *fp;
	float intensity;
	float maxval = -1.0;

	fp = fopen(filename, "wb");
	if (!fp) {
		fprintf(stderr, "can't open file [ %s ] to save.\n", filename);
		exit(-1);
	}

	for (j = 0; j < height; j++) {
		for (i = 0; i < width; i++) {
			intensity = INTENSITY(&img[3 * (j * width + i)]);
			fprintf(fp, "%f ", intensity);
			if (maxval < intensity) maxval = intensity;
		}
		fprintf(fp, "\n");
	}

	printf("max = %f\n", maxval);

	fclose(fp);
}

char
clamp(float f)
{
	int val;

	val = (int)(f * 255.5f + 0.5f);
	if (val > 255) val = 255;
	if (val < 0) val = 0;

	return (char)val;
}

void
tonemap(char *bytes, float *data, int len)
{
	int i;

	for (i = 0; i < len; i++) {
		bytes[i] = clamp(data[i] * scale);	
	}
}

int
main(int argc, char **argv)
{
	int              w;
	int              h;
	int              ret;
	rgbe_header_info info;
	float            *img;
	char             *uimg;
	FILE             *fp;
 
	if (argc < 3) {
		printf("usage: hdr2array input.hdr output.dat [scale]\n");
		exit(-1);
	}

	if (argc > 3) {
		scale = atof(argv[3]);
		printf("scaling = %f\n", scale);
	}

	fp = fopen(argv[1], "rb");
	if (!fp) {
		printf("can't open file [ %s ]\n", argv[1]);
		exit(-1);
	}

	ret = RGBE_ReadHeader(fp, &w, &h, &info);
	if (ret != RGBE_RETURN_SUCCESS) {
		exit(-1);
	}

	img = (float *)malloc(sizeof(float) * w * h * 3);

	ret = RGBE_ReadPixels_RLE(fp, img, w, h);
	if (ret != RGBE_RETURN_SUCCESS) {
		exit(-1);
	}

	uimg = (char *)malloc(sizeof(char) * w * h * 3);

	tonemap(uimg, img, w * h * 3);

	//write_png(argv[2], uimg, w, h);
	write_array(argv[2], img, w, h);

	exit(0);
}
