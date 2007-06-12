/*
 * $Id: sis.c,v 1.2 2004/08/25 13:04:40 syoyo Exp $
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <string.h>
#include <math.h>

//#include "memory.h"
#include "sis.h"

#ifndef M_PI
#define M_PI 3.1415926535
#endif


static double sinc(double x);
static void calc_solidangle(pixelinfo_t *pixels, int width, int height);
static void mean(double *meanval, pixelinfo_t *pixels, int npixels);
static void sd(double *sdval, pixelinfo_t *pixels, int npixels,
	       double meanval);
static void gamma_4pi(double *val, pixelinfo_t *pixels,
		      int npixels, double domega0);
static int  inunitsphere(int i, int j, int width, int height);
static pixelinfo_t *initpixelinfo(float *texels, int width, int height);
static void assign_level(pixelinfo_t *pixels, int npixels, double sd,
			 int maxlevel);
static void layerize(pixelinfo_t **levels, int maxlevel,
		     pixelinfo_t *src, int npixels);
static void output_hdr(pixelinfo_t *pixels, int level, int width, int height);
static void output_cc(pixelinfo_t *pixels, int level, int width, int height);

pixelinfo_t *
ri_sis(float *texels, int ngensamples, int width, int height)
{
	int    i;
	int    maxlevel = 6;
	double m;
	double s;
	double v;
	double domega0;
	pixelinfo_t *info;
	pixelinfo_t **layer;
	pixelinfo_t *gensamples;
	FILE *fp;

	fp = fopen("gensamples.dat", "w");
	if (!fp) exit(-1);

	printf("start sis\n");
		
	gensamples = (pixelinfo_t *)malloc(sizeof(pixelinfo_t) * ngensamples);
	if (!gensamples) {
		printf("muda muda muda. gensamples = NULL\n");
		exit(-1);
	}

	layer = (pixelinfo_t **)malloc(sizeof(pixelinfo_t *) * maxlevel);
	if (!layer) {
		printf("muda muda muda. layer = NULL\n");
		exit(-1);
	}

	for (i = 0; i < maxlevel; i++) {
		layer[i] = (pixelinfo_t *)malloc(sizeof(pixelinfo_t) *
						 width * height); 
	}

	info = initpixelinfo(texels, width, height);
	mean(&m, info, width * height);
	sd  (&s, info, width * height, m);
	assign_level(info, width * height, s, maxlevel);

	layerize(layer, maxlevel, info, width * height);

	//output_hdr(layer[1], 1, width, height);

	/* detect connected components each layer */
	for (i = 0; i < maxlevel; i++) {
		connected_components(layer[i], width, height);
		//output_cc(layer[i], i, width, height);
	}

	domega0 = 0.01;
	gamma_4pi(&v, info, width * height, domega0);

	generate_sample(gensamples, ngensamples, v,
			layer, maxlevel, width, height);

	/* Output samples */

	fprintf(fp, "%d\n", ngensamples);
	fprintf(fp, "%d %d\n", width, height);
		
	for (i = 0; i < ngensamples; i++) {
		fprintf(fp, "%d %d %f %f %f\n", gensamples[i].x,
					      gensamples[i].y,
					      gensamples[i].c[0],
					      gensamples[i].c[1],
					      gensamples[i].c[2]);
	}

	fclose(fp);

	return info;
}

/* --- private functions --- */

/* importance metric function */
double
impfunc(float intensity, float domega)
{
	float w;
	float mindomega = 0.01;

	domega = (domega > mindomega) ? mindomega : domega;

	/* w = domega^{1/4} */
	w = sqrt(domega);
	w = sqrt(w);

	return intensity * w;
}

void
mean(double *meanval, pixelinfo_t *pixels, int npixels)
{
	int i, j;
	int n;

	*meanval = 0.0;

	n = 0;
	for (i = 0; i < npixels; i++) {
		if (!pixels[i].valid)  continue;

		(*meanval) += pixels[i].intensity;
			
		n++;
	}

	printf("sum = %f\n", (*meanval));
	printf("n = %d\n", n); 

	(*meanval) /= (double)n; 

	printf("mean = %f\n", (*meanval));
}

/* standard deviation */
void
sd(double *sdval, pixelinfo_t *pixels, int npixels, double meanval)
{
	int i;
	int n;
	double sum;	
	double x;	

	sum = 0.0;

	n = 0;
	for (i = 0; i < npixels; i++) {
		if (!pixels[i].valid) continue;

		x = pixels[i].intensity - meanval;	

		sum += x * x;

		n++;
	}

	printf("sum = %f\n", sum);
	printf("n = %d\n", n);
	sum /= (double)n;
	printf("sum/n = %f\n", sum);

	(*sdval) = sqrt(sum);
	printf("sd = %f\n", sqrt(sum));

	printf("sd = %f\n", (*sdval));
}

void
gamma_4pi(double *val, pixelinfo_t *pixels, int npixels, double domega0)
{
	int i;
	double net;

	net = 0.0;
	/* net illumination in the texture map */
	for (i = 0; i < npixels; i++) {
		if (!pixels[i].valid) continue;

		net += pixels[i].intensity;				
	}

	/* Gamma_{4 Pi} = L * dOmega_{0}^{1/4} */
	(*val) = impfunc(net, domega0);

	printf("gamma_4pi = %f\n", (*val));
}

void
calc_solidangle(pixelinfo_t *pixels, int width, int height)
{
	int i, j;
	double u, v, r;
	double theta, phi;

	for (j = 0; j < height; j++) {
		for (i = 0; i < width; i++) {
			if (!pixels[j * width + i].valid) continue;

			v = (width / 2.0 - i) / (width / 2.0);
			u = (j - height / 2.0) / (height / 2.0);
			r = sqrt(u * u + v * v);
			if (r > 1.0) {
				printf("???\n");
				continue;
			}

			theta = M_PI * r;
			phi   = atan2(v, u);

			pixels[j * width + i].domega = (2 * M_PI / width) *
						       (2 * M_PI / height) *
						       sinc(theta);
		}
	}
}

double
sinc(double x)
{
	if (fabs(x) < 1.0e-6) return 1.0;

	return sin(x) / x;
}

int
inunitsphere(int i, int j, int width, int height)
{
	double u, v;
	double r;

	v = (height / 2.0 - j) / (height / 2.0);
	u = (i - width / 2.0) / (width/ 2.0);

	r = sqrt(u * u + v * v);

	if (r > 1.0) return 0;

	return 1;
}

pixelinfo_t *
initpixelinfo(float *texels, int width, int height)
{
	int i, j;
	int ninvalid;
	int offset;
	pixelinfo_t *info = NULL;
	
	info = (pixelinfo_t *)malloc(sizeof(pixelinfo_t) * width * height);
	if (!info) exit(-1);

	ninvalid = 0;

	

	for (j = 0; j < height; j++) {
		for (i = 0; i < width; i++) {
			offset = j * width + i;

			if (inunitsphere(i, j, width, height)) {

				info[offset].valid = 1;

				info[offset].c[0] = texels[3*offset+0];
				info[offset].c[1] = texels[3*offset+1];
				info[offset].c[2] = texels[3*offset+2];
		
				info[offset].x = i;
				info[offset].y = j;

				/* RGB -> YCbCr(use Y only) */
				info[offset].intensity = 
					0.2989 * texels[3*offset+0] +
					0.5866 * texels[3*offset+1] +
					0.1145 * texels[3*offset+2];
				
			} else {
				/* this pixels is not used */ 
				info[offset].valid = 0;
				ninvalid++;
			}
			
		}
	}

	calc_solidangle(info, width, height);

	printf("ninvalid = %d\n", ninvalid);
	return info;
}

void
assign_level(pixelinfo_t *pixels, int npixels, double sd, int maxlevel)
{
	int i, j;
	int nassigned;
	double threshold;

	for (i = 0; i < npixels; i++) {
		pixels[i].level = -1;	
	}

	for (j = 0; j < maxlevel; j++) {
		threshold = j * sd;
		printf("assigning level %d: threshold = %f\n", j, threshold);
		nassigned = 0;
		for (i = 0; i < npixels; i++) {
			if (!pixels[i].valid) continue;

			if (pixels[i].intensity >= threshold) {
				pixels[i].level = j;
				nassigned++;
			}	
		}

		printf("assigned %d pixels of %d\n", nassigned, npixels); 
	} 
}

/* levels must be malloced */
void
layerize(pixelinfo_t **levels, int maxlevel, pixelinfo_t *src, int npixels)
{
	int i, j;
	int count;
	pixelinfo_t *dst;

	for (j = 0; j < maxlevel; j++) {
		dst = levels[j];
		count = 0;	
		for (i = 0; i < npixels; i++) {
			if (src[i].level >= j) { 
				memcpy(&(dst[i]),
				       &(src[i]),
					sizeof(pixelinfo_t));
				dst[i].level = j;
			} else {
				dst[i].level = -1;
				dst[i].valid = 0;
				count++;
			}
		}

		printf("number of invalid pixels = %d\n", count);
	}
}

void
output_hdr(pixelinfo_t *pixels, int level, int width, int height)
{
	int i, j;
	char buf[255];
	float col[3];
	FILE *fp;

	sprintf(buf, "level%d.float", level);
	fp = fopen(buf, "wb");
	if (!fp) {
		printf("can't open %s\n", buf);
		exit(-1);
	}

	for (j = 0; j < width * height; j++) {
		if (pixels[j].valid) {
			col[0] = pixels[j].c[0];			
			col[1] = pixels[j].c[1];			
			col[2] = pixels[j].c[2];

			fwrite(col, sizeof(float), 3, fp);
		} else {
			col[0] = col[1] = col[2] = 0.0;

			fwrite(col, sizeof(float), 3, fp);
		}
	}

	fclose(fp);
}

void
output_cc(pixelinfo_t *pixels, int level, int width, int height)
{
	int i;
	int c;
	int maxlabel;
	char buf[255];
	FILE *fp;

	sprintf(buf, "cc%d.ppm", level);
	fp = fopen(buf, "w");
	if (!fp) return;

	fprintf(fp, "P3\n");
	fprintf(fp, "%d %d\n", width, height);
	fprintf(fp, "255\n");

	maxlabel = 0;
	for (i = 0; i < width * height; i++) {
		if (!pixels[i].valid) continue;

		if (maxlabel < pixels[i].label) maxlabel = pixels[i].label;
	}


	maxlabel++;

	fprintf(stderr, "maxlabel = %d\n", maxlabel);

	if (maxlabel == 0) exit(-1);

	for (i = 0; i < width * height; i++) {
		if (!pixels[i].valid) {
			fprintf(fp, "%d %d %d\n", 0, 0, 255);
		} else {
			c = (int)(((pixels[i].label + 1) / (double)maxlabel) * 255.0);
			if (c < 0) c = 0;
			if (c > 255) c = 255;
			fprintf(fp, "%d %d %d\n", c, c, c);
		}
	}

	fclose(fp);
}
