/*
 * $Id: multipolelight.c,v 1.1.1.1 2004/01/06 13:57:09 syoyo Exp $
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <string.h>
#include <math.h>

#include "random.h"
#include "multipolelight.h"

#ifndef M_PI
#define M_PI 3.14159265358979
#endif

void
ri_multipolelight_rectangular(ri_vector_t  outirrad[3],
			      ri_vector_t  inirrad,
			      ri_vector_t  v,
			      double       l,
			      double       w)
{
	/* calculate second-order expansion for the light vector
	 * due to a uniform rectangle its center is origin and size is [w x l].
	 */ 	

	int    i;
	double x, y, z;
	double k;
	double c1, cx, cy;
	double r;
	double M1[3], Mx[3], My[3];	/* each for RGB. */

	k = 1.0 / (4.0 * M_PI);
	r = ri_vector_length(v);
	x = v.e[0]; y = v.e[1]; z = v.e[2];

	c1 = 6.0 * (pow(r, 4) + x * x * l * l + y * y * w * w)
	         - (l * l + w * w) * r * r; 
	c1 = c1 / (6.0 * pow(r, 8));
	printf("c1 = %f\n", c1);

	cx = x * l * l;
	cx = cx / (3.0 * pow(r, 6));
	printf("cx = %f\n", cx);

	cy = y * w * w;
	cy = cy / (3.0 * pow(r, 6));
	printf("cy = %f\n", cy);

	for (i = 0; i < 3; i++) {
		M1[i] = inirrad.e[i] * c1;
		Mx[i] = inirrad.e[i] * cx;
		My[i] = inirrad.e[i] * cy;
	}

	for (i = 0; i < 3; i++) {
		outirrad[i].e[0] = k * z * (x * M1[i] - Mx[i]);
		outirrad[i].e[1] = k * z * (y * M1[i] - My[i]);
		outirrad[i].e[2] = k * z * (z * M1[i]);

		printf("outirrad[%d] = (%f, %f, %f)\n", i, outirrad[i].e[0],
							   outirrad[i].e[1],
							   outirrad[i].e[2]);
	}
}

void
ri_exact_rectangular(ri_vector_t  outirrad[3],
		     ri_vector_t  inirrad,
		     ri_vector_t  v,
		     double       l,
		     double       w)
{
	int nsamples = 10000;
	int i, j, idx;	
	double s, t;
	double len;
	double cos1;
	double coeff[3];
	double ds;
	double cosv[4];
	ri_vector_t v0, v1, v2, v3;
	ri_vector_t t0, t1, t2;
	ri_vector_t rs;
	ri_vector_t n;
	ri_vector_t vr;
	ri_vector_t tmp;
	ri_vector_t normal[4];
	ri_vector_t p[4];
	v0.e[0] = -l * 0.5; v0.e[1] = -w * 0.5; v0.e[2] = 0.0;
	v1.e[0] = -l * 0.5; v1.e[1] =  w * 0.5; v1.e[2] = 0.0;
	v2.e[0] =  l * 0.5; v2.e[1] =  w * 0.5; v2.e[2] = 0.0;
	v3.e[0] =  l * 0.5; v3.e[1] = -w * 0.5; v3.e[2] = 0.0;
	n.e[0] = 0.0; n.e[1] = 0.0; n.e[2] = 1.0;

	ri_vector_zero(&outirrad[0]);
	ri_vector_zero(&outirrad[1]);
	ri_vector_zero(&outirrad[2]);

#if 0
	ri_vector_sub(&(p[0]), v0, v);
	ri_vector_sub(&(p[1]), v1, v);
	ri_vector_sub(&(p[2]), v2, v);
	ri_vector_sub(&(p[3]), v3, v);
	ri_vector_normalize(&(p[0]));
	ri_vector_normalize(&(p[1]));
	ri_vector_normalize(&(p[2]));
	ri_vector_normalize(&(p[3]));

	cosv[0] = ri_vector_dot3(p[0], p[1]);
	cosv[1] = ri_vector_dot3(p[1], p[2]);
	cosv[2] = ri_vector_dot3(p[2], p[3]);
	cosv[3] = ri_vector_dot3(p[3], p[0]);
	cosv[0] = acos(cosv[0]);
	cosv[1] = acos(cosv[1]);
	cosv[2] = acos(cosv[2]);
	cosv[3] = acos(cosv[3]);
	printf("cos[0] = %f\n", cosv[0]);
	printf("cos[1] = %f\n", cosv[1]);
	printf("cos[2] = %f\n", cosv[2]);
	printf("cos[3] = %f\n", cosv[3]);
	

	ri_vector_cross3(&(normal[0]), p[1], p[0]);
	ri_vector_cross3(&(normal[1]), p[2], p[1]);
	ri_vector_cross3(&(normal[2]), p[3], p[2]);
	ri_vector_cross3(&(normal[3]), p[0], p[3]);
	ri_vector_normalize(&(normal[0]));
	ri_vector_normalize(&(normal[1]));
	ri_vector_normalize(&(normal[2]));
	ri_vector_normalize(&(normal[3]));

	coeff[0] = inirrad.e[0] / (2.0 * M_PI * l * w);
	coeff[1] = inirrad.e[1] / (2.0 * M_PI * l * w);
	coeff[2] = inirrad.e[2] / (2.0 * M_PI * l * w);

	for (i = 0; i < 4; i++) {
		ri_vector_copy(&tmp, normal[i]);
		//ri_vector_copy(&tmp, n);
		ri_vector_scale(&tmp, cosv[i]);

		ri_vector_add(&(outirrad[0]), outirrad[0], tmp); 	
		ri_vector_add(&(outirrad[1]), outirrad[1], tmp); 	
		ri_vector_add(&(outirrad[2]), outirrad[2], tmp); 	
	}	

	ri_vector_scale(&(outirrad[0]), coeff[0]);
	ri_vector_scale(&(outirrad[1]), coeff[1]);
	ri_vector_scale(&(outirrad[2]), coeff[2]);
#else
	ds = (l * w) / (double)nsamples;

	for (i = 0; i < nsamples; i++) {
		idx = (int)(randomMT() * 2);
		if (idx == 0) {
			ri_vector_copy(&t0, v0);
			ri_vector_copy(&t1, v1);
			ri_vector_copy(&t2, v2);
		} else {
			ri_vector_copy(&t0, v0);
			ri_vector_copy(&t1, v2);
			ri_vector_copy(&t2, v3);
		}

		s = sqrt(randomMT());
		t = randomMT();

		rs.e[0] = t0.e[0]*(1.0-s) + t1.e[0]*(s-s*t) + t2.e[0]*s*t;
		rs.e[1] = t0.e[1]*(1.0-s) + t1.e[1]*(s-s*t) + t2.e[1]*s*t;
		rs.e[2] = t0.e[2]*(1.0-s) + t1.e[2]*(s-s*t) + t2.e[2]*s*t;

		ri_vector_sub(&vr, v, rs);
		len = ri_vector_length(vr);

		ri_vector_normalize(&vr);

		cos1 = ri_vector_dot3(n, vr);

		for (j = 0; j < 3; j++) {
			coeff[j] = ds * (inirrad.e[j] * cos1) / (4.0 * M_PI * len * len);
			ri_vector_copy(&tmp, vr);
			ri_vector_scale(&tmp, coeff[j]);
			ri_vector_add(&(outirrad[j]), outirrad[j], tmp);
		}
	}

#endif
	for (j = 0; j < 3; j++) {
		printf("outirrad[%d] = (%f, %f, %f)\n", j, outirrad[j].e[0],
							   outirrad[j].e[1],
							   outirrad[j].e[2]);
	}
}
