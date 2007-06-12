/*
 * $Id$
 *
 * HDRI(raw float format) to OpenEXR converter.
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <OpenEXR/ImfCRgbaFile.h>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "rgbe.h"

void
write_openexr(const char *filename, const char *hdrfile)
{
	FILE *hdrfp;
	int ret;
	int width, height;
	int i;
	float *img;
	rgbe_header_info info;
	ImfRgba *rgba;
	ImfOutputFile *fp;
	ImfHeader *header;
	
	hdrfp = fopen(hdrfile, "rb");
	if (!hdrfp) exit(-1);

	ret = RGBE_ReadHeader(hdrfp, &width, &height, &info);
	if (ret != RGBE_RETURN_SUCCESS) {
		printf("-1\n");
		exit(-1);
	}

	img = (float *)malloc(sizeof(float) * width * height * 3);

	ret = RGBE_ReadPixels_RLE(hdrfp, img, width, height);
	if (ret != RGBE_RETURN_SUCCESS) {
		printf("-1\n");
		exit(-1);
	}

	header = ImfNewHeader();
	ImfHeaderSetDisplayWindow(header, 0, 0, width - 1, height - 1);
	ImfHeaderSetDataWindow(header, 0, 0, width - 1, height - 1);
	ImfHeaderSetScreenWindowWidth(header, width);
	
	fp = ImfOpenOutputFile(filename, header, IMF_WRITE_RGBA);
	if (!fp) {
		printf("-1\n");
		exit(-1);
	}

	ImfDeleteHeader(header);

	rgba = (ImfRgba *)malloc(width * height * sizeof(ImfRgba));
	for (i = 0; i < width * height; i++) {
		ImfFloatToHalf(img[3 * i + 0], &rgba[i].r);
		ImfFloatToHalf(img[3 * i + 1], &rgba[i].g);
		ImfFloatToHalf(img[3 * i + 2], &rgba[i].b);
		ImfFloatToHalf(          1.0f, &rgba[i].a);
	}

	ImfOutputSetFrameBuffer(fp, rgba, 1, width);
        ImfOutputWritePixels(fp, height);

        ImfCloseOutputFile(fp);
	fclose(hdrfp);

	free(img);
	free(rgba);
}

int
main(int argc, char **argv)
{
	if (argc < 3) {
		printf("usage: hdr2exr outfile.exr infile.hdr\n");
		exit(-1);
	}

	write_openexr(argv[1], argv[2]);
}
