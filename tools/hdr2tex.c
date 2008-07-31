/*
 * $Id: hdr2tex.c,v 1.1.1.1 2004/01/06 13:57:17 syoyo Exp $
 *
 * HDRI(raw float format) to texture converter.
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

static int swapflag = 0;
static int flipflag = 0;

static int is_little_endian()
{
	int x = 1;
	if (*(char *)&x) {
		return 1;	/* little */
	} else {
		return 0;
	}
}

static void swap(void *data)
{
	char tmp[4];
	tmp[0] = ((char *)data)[0];
	tmp[1] = ((char *)data)[1];
	tmp[2] = ((char *)data)[2];
	tmp[3] = ((char *)data)[3];

	((char *)data)[0] = tmp[3];
	((char *)data)[1] = tmp[2];
	((char *)data)[2] = tmp[1];
	((char *)data)[3] = tmp[0];
}

static void endconv(void *data)
{
	char tmp[4];
	tmp[0] = ((char *)data)[0];
	tmp[1] = ((char *)data)[1];
	tmp[2] = ((char *)data)[2];
	tmp[3] = ((char *)data)[3];

	if (!is_little_endian()) {
		((char *)data)[0] = tmp[3];
		((char *)data)[1] = tmp[2];
		((char *)data)[2] = tmp[1];
		((char *)data)[3] = tmp[0];
	}
}

void
write_texture(const char *filename, const char *hdrfile, int width)
{
	FILE *fp;
	FILE *hdr;
	unsigned char *img;
	int height;
	int compos;
	int i, j;
	int ival;
	int idx;
	float *data;
	float fval;   
	unsigned char *c;
	
	fp = fopen(filename, "wb");
	if (!fp) exit(-1);
	hdr = fopen(hdrfile, "rb");
	if (!hdr) exit(-1);

	height = width;

	data = (float *)malloc(sizeof(float) * width * height * 4);
	if (!data) {
		printf("muda muda muda!\n");
		exit(-1);
	}

	c = (unsigned char *)&fval;

	for (i = 0; i < width; i++) {
		for (j = 0; j < width; j++) {
			fread(&fval, sizeof(float), 1, hdr);
			if (swapflag) swap(&fval);
			if (isnan(fval)) fval = 0.0f;
			if (fval < 0.0) fval = 0.0f;
			printf("fval = %f\n", fval);
			if (flipflag) {
				data[4 * ((width - i - 1) * width + j) + 0] = fval;
			} else {
				data[4 * (i * width + j) + 0] = fval;
			}
			fread(&fval, sizeof(float), 1, hdr);
			if (swapflag) swap(&fval);
			if (isnan(fval)) fval = 0.0f;
			if (fval < 0.0) fval = 0.0f;
			if (flipflag) {
				data[4 * ((width - i - 1) * width + j) + 1] = fval;
			} else {
				data[4 * (i * width + j) + 1] = fval;
			}
			fread(&fval, sizeof(float), 1, hdr);
			if (swapflag) swap(&fval);
			if (isnan(fval)) fval = 0.0f;
			if (fval < 0.0) fval = 0.0f;
			if (flipflag) {
				data[4 * ((width - i - 1) * width + j) + 2] = fval;
			} else {
				data[4 * (i * width + j) + 2] = fval;
			}
			data[4 * (i * width + j) + 3] = 1.0f;
		}
	}	

	/* output is saved in little endian format. */

	ival = width;
	endconv((void *)&ival);
	fwrite(&ival, sizeof(int), 1, fp);

	ival = height;
	endconv((void *)&ival);
	fwrite(&ival, sizeof(int), 1, fp);

	for (i = 0; i < width * height * 4; i++) {
		fval = data[i];
		endconv((void *)&fval);
		fwrite(&fval, sizeof(float), 1, fp);
	}

	fclose(hdr);
	fclose(fp);

}

int
main(int argc, char **argv)
{
	if (argc < 4) {
		printf("usage: hdr2tex outfile.tex infile.float width [swap] [flip]\n");
	}

	if (argc > 4) {
		if (strcmp(argv[4], "swap") == 0) {
			swapflag = 1;
		}
		if (strcmp(argv[4], "flip") == 0) {
			flipflag = 1;
		}
	}

	if (argc > 5) {
		if (strcmp(argv[5], "swap") == 0) {
			swapflag = 1;
		}
		if (strcmp(argv[5], "flip") == 0) {
			flipflag = 1;
		}
	}

	printf("swap = %d, flip = %d\n", swapflag, flipflag);

	write_texture(argv[1], argv[2], atoi(argv[3]));
}
