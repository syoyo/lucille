/*
 * $Id: sis.c,v 1.1.1.1 2004/01/06 13:57:13 syoyo Exp $
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <string.h>
#include <math.h>

#include "memory.h"
#include "sis.h"

#ifndef M_PI
#define M_PI 3.1415926535
#endif

static double sinc(double x);
static void calc_solidangle(double *domegas, int width, int height);
static void mean(double meanval[3], float *texels, int width, int height);
static void sd(double sdval[3],
	       float *textels, int width, int height, double meanval[3]);
static void gamma_4pi(double val[3],
		      float *texels, int width, int height, double domega0);
static int  inunitsphere(int i, int j, int width, int height);

void
ri_sis_setup(ri_sis_t *sis)
{
	double m[3];
	double s[3];
	double v[3];
	double domega0;
	double *domegas;

	printf("start sis\n");

	domegas = (double *)malloc(sizeof(double) * sis->texture->width
						  * sis->texture->height);
	if (!domegas) return;

	mean(m, sis->texture->data, sis->texture->width, sis->texture->height);
	sd  (s, sis->texture->data, sis->texture->width, sis->texture->height,
		m);

	domega0 = 0.01;
	gamma_4pi(v, sis->texture->data, sis->texture->width, sis->texture->height, domega0);

	//exit(-1);
}

/* --- private functions --- */
static void
mean(double meanval[3], float *texels, int width, int height)
{
	int i, j;
	int n;

	meanval[0] = meanval[1] = meanval[2] = 0.0;

	n = 0;
	for (j = 0; j < height; j++) {
		for (i = 0; i < width; i++) {
			if (!inunitsphere(i, j, width, height)) {
				continue;
			}

			meanval[0] += texels[4 * (j * width + i) + 0];
			meanval[1] += texels[4 * (j * width + i) + 1];
			meanval[2] += texels[4 * (j * width + i) + 2];
			
			n++;
		}
	}

	meanval[0] /= (double)n; 
	meanval[1] /= (double)n; 
	meanval[2] /= (double)n; 

	printf("mean = (%f, %f, %f)\n", meanval[0],
					meanval[1],
					meanval[2]);
}

/* standard deviation */
static void
sd(double sdval[3], float *texels, int width, int height, double meanval[3])
{
	int i;
	int n;
	double sum[3];	
	double x[3];	

	n = width * height;

	sum[0] = sum[1] = sum[2] = 0.0;

	for (i = 0; i < n; i++) {
		x[0] = texels[4 * i + 0] - meanval[0];	
		x[1] = texels[4 * i + 1] - meanval[1];	
		x[2] = texels[4 * i + 2] - meanval[2];	

		sum[0] += x[0] * x[0];
		sum[1] += x[1] * x[1];
		sum[2] += x[2] * x[2];
	}

	sum[0] /= (double)n;
	sum[1] /= (double)n;
	sum[2] /= (double)n;

	sdval[0] = sqrt(sum[0]);
	sdval[1] = sqrt(sum[1]);
	sdval[2] = sqrt(sum[2]);

	printf("sd = (%f, %f, %f)\n", sdval[0], sdval[1], sdval[2]);
}

static void
gamma_4pi(double val[3], float *texels, int width, int height, double domega0)
{
	int i;
	int n;
	double net[3];

	n = width * height;
	net[0] = net[1] = net[2] = 0.0;
	/* net illumination in the texture map */
	for (i = 0; i < n; i++) {
		net[0] += texels[4 * i + 0];				
		net[1] += texels[4 * i + 1];				
		net[2] += texels[4 * i + 2];				
	}

	/* Gamma_{4 Pi} = L * dOmega_{0}^{1/4} */
	domega0 = sqrt(domega0);
	domega0 = sqrt(domega0);
	val[0] = net[0] * domega0;
	val[1] = net[1] * domega0;
	val[2] = net[2] * domega0;

	printf("gamma_4pi = (%f, %f, %f)\n", val[0], val[1], val[2]);

}

static void
calc_solidangle(double *domegas, int width, int height)
{
	int i, j;
	double u, v, r;
	double theta, phi;

	for (j = 0; j < height; j++) {
		for (i = 0; i < width; i++) {
			v = (width / 2.0 - i) / (width / 2.0);
			u = (j - height / 2.0) / (height / 2.0);
			r = sqrt(u * u + v * v);
			if (r > 1.0) {
				domegas[j * width + i] = -1.0;
				continue;
			}

			theta = M_PI * r;
			phi   = atan2(v, u);

			domegas[j * width + i] = (2 * M_PI / width) *
						 (2 * M_PI / height) *
						 sinc(theta);
		}
	}
}

static double
sinc(double x)
{
	if (fabs(x) < 1.0e-6) return 1.0;

	return sin(x) / x;
}

static int
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
