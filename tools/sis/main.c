#include <stdio.h>
#include <stdlib.h>

#include "sis.h"

float *
read_hdr(const char *filename, int width, int height)
{
	int i;
	float *data = NULL;
	FILE *fp;

	fp = fopen(filename, "rb");
	if (!fp) {
		printf("can't open %s\n", filename);
	}

	data = malloc(sizeof(float) * width * height * 3);
	if (!data) exit(-1);
	
	fread(data, sizeof(float), width * height * 3, fp);

	fclose(fp);

	return data;
}

int
main(int argc, char **argv)
{
	float *data;
	pixelinfo_t *pixels; 
	int width;
	int nsamples = 300;

	if (argc < 3) {
		printf("usage: sisgen image.float width [number of samples]\n");
		printf("  Input HDRI must be Angular Map format.\n");
		printf("  Default number of samples = 300.\n");
		printf(" \n");
		printf("  Example usage.\n");
		printf(" \n");
		printf("  sisgen rnl.float 900\n");
		printf("  sisgen stpeters.float 1500 128\n");
		exit(-1);
	}

	if (argc > 3) {
		nsamples = atoi(argv[3]);
	}

	//const char *filename = "stpeters_probe750.float";
	//int width = 750;

	width = atoi(argv[2]);

	data = read_hdr(argv[1], width, width);

	pixels = ri_sis(data, nsamples, width, width);

	/* pixels is used only for debugging. */
	//output_hdr(pixels, 6, width, width);
}
