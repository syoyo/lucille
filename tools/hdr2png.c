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


#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <png.h>

#include "rgbe.h"

float scale = 1.0; 

void
write_png(const char *filename, char *img, int width, int height)
{
	int i;
	png_structp png_ptr;
	png_infop   info_ptr;
	FILE *fp;
	png_bytep   *rowpointers;

	fp = fopen(filename, "wb");
	if (!fp) {
		fprintf(stderr, "can't open file [ %s ] to save.\n", filename);
		exit(-1);
	}

	png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING,
					  NULL, NULL, NULL);
	if (!png_ptr) {
		fprintf(stderr, "can't create png write struct.\n");
		exit(-1);
	}

	info_ptr = png_create_info_struct(png_ptr);
	if (!info_ptr) {
		fprintf(stderr, "can't create png info struct.\n");
		exit(-1);
	}

	rowpointers = (png_bytep *)malloc(sizeof(png_bytep) * height);

	for (i = 0; i < height; i++) {
		rowpointers[i] = &(img[3 * width * i]);
	}

	png_init_io(png_ptr, fp);
	png_set_IHDR(png_ptr, info_ptr, width, height, 8, PNG_COLOR_TYPE_RGB,
		     PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_DEFAULT,
		     PNG_FILTER_TYPE_DEFAULT);
	png_write_info(png_ptr, info_ptr);
	png_write_image(png_ptr, rowpointers);
	png_write_end(png_ptr, info_ptr);
	png_destroy_write_struct(&png_ptr, &info_ptr);
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
		printf("usage: hdr2png input.hdr output.png [scale]\n");
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

	write_png(argv[2], uimg, w, h);

	exit(0);
}
