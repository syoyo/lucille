/*
 * jpeg -> .hdr converter
 *
 * gcc jpg2hdr.c jpeg.c rgbe.c -ljpeg
 *
 */
#include <stdio.h>
#include <string.h>

#include "jpeg.h"

#define CIE_rf 0.265074126
#define CIE_gf 0.670114631
#define CIE_bf 0.064811243
#define INTENSITY(v) (CIE_rf * (v)[0] + CIE_gf  * (v)[1] + CIE_bf * (v)[2])

void
write_array(const char *filename, const char *jpegfile)
{
	FILE *fp;
	unsigned char *img;
	int width;
	int height;
	int compos;
	int i, j;
	int ival;
	int idx;
	float *data;
	float fval;   
	float intensity;
	
	fp = fopen(filename, "w");
	if (!fp) exit(-1);

	img = jpeg_load(jpegfile, &width, &height, &compos);
	if (img == NULL) {
		printf("can't load jpeg file.\n");
		exit(-1);
	}

	data = (float *)malloc(sizeof(float) * width * height * 3);
	if (!data) {
		printf("muda muda muda!\n");
		exit(-1);
	}

	for (i = 0; i < width * height; i++) {
		idx = 3 * i;
	
		switch (compos) {
			case 1:
				data[idx + 0] = (float)(*img++) / (256.0f);
				data[idx + 1] = data[idx + 0];
				data[idx + 2] = data[idx + 1];
				break;

			case 2:
				data[idx + 0] = (float)(*img++) / (256.0f);
				data[idx + 1] = (float)(*img++) / (256.0f);
				data[idx + 2] = data[idx + 1];
				break;

			case 3:
				data[idx + 0] = (float)(*img++) / (256.0f);
				data[idx + 1] = (float)(*img++) / (256.0f);
				data[idx + 2] = (float)(*img++) / (256.0f);
				break;

		}
	}

	for (j = height - 1; j >= 0; j--) {
		for (i = 0; i < width; i++) {
			intensity = INTENSITY(&data[3 * (j * width + i)]);
			fprintf(fp, "%f ", intensity);
		}
		fprintf(fp, "\n");
	}

	fclose(fp);

}

int
main(int argc, char **argv)
{
	if (argc < 3) {
		printf("usage: jpg2hdr outfile.hdr infile.jpg\n");
	}

	write_array(argv[1], argv[2]);
}
