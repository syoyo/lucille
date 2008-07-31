/*
 * jpeg -> .hdr converter
 *
 * gcc jpg2hdr.c jpeg.c rgbe.c -ljpeg
 *
 */
#include <stdio.h>
#include <string.h>

#include "jpeg.h"
#include "rgbe.h"

static void endconv(void *data)
{
#ifdef BIG_ENDIAN
	char tmp[4];
	tmp[0] = ((char *)data)[0];
	tmp[1] = ((char *)data)[1];
	tmp[2] = ((char *)data)[2];
	tmp[3] = ((char *)data)[3];

	((char *)data)[0] = tmp[3];
	((char *)data)[1] = tmp[2];
	((char *)data)[2] = tmp[1];
	((char *)data)[3] = tmp[0];
#endif
}

void
write_texture(const char *filename, const char *jpegfile)
{
	FILE *fp;
	unsigned char *img;
	int width;
	int height;
	int compos;
	int i;
	int ival;
	int idx;
	float *data;
	float fval;   
	
	fp = fopen(filename, "wb");
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

	//printf("w = %d, h = %d, c = %d\n", width, height, compos);

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

	RGBE_WriteHeader(fp, width, height, NULL);
	RGBE_WritePixels_RLE(fp, data, width, height);

}

int
main(int argc, char **argv)
{
	if (argc < 3) {
		printf("usage: jpg2hdr outfile.hdr infile.jpg\n");
	}

	write_texture(argv[1], argv[2]);
}
