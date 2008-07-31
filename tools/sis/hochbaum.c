#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "random.h"
#include "sis.h"


static double dist(pixelinfo_t p0, pixelinfo_t p1);

double
dist(pixelinfo_t p0, pixelinfo_t p1)
{
	double dx, dy;

	dx = (double)(p0.x - p1.x);
	dy = (double)(p0.y - p1.y);

	return dx * dx + dy * dy;
}

void
hochbaum_symoys(pixelinfo_t *x, int nx, pixelinfo_t *y, int ny, int k)
{
	int i, j, p, q, l;
	int randid;
	int argmax_id, argmin_id;
	double len;
	double minlen = 1.0e+16;
	double maxlen = -1.0;;
	double argmaxy = -1.0;
	double argminx = 1.0e+16;
	pixelinfo_t *s;
	
	/* choose an arbitrary point in y. */
	randid = (int)(ny * randomMT());
	memcpy(&(x[nx]), &(y[randid]), sizeof(pixelinfo_t));

	s = (pixelinfo_t *)malloc(sizeof(pixelinfo_t) * k);

	for (i = 1; i < k; i++) {

		//argmaxy = dist(y[0], x[0]);
		//argmax_id = 0; 
		argmaxy = -1.0;
		for (p = 0; p < ny; p++) {
			minlen = 1.0e+16;
			for (q = 0; q < nx + i; q++) {
				/* find min */
				len = dist(y[p], x[q]);
				if (minlen > len) minlen = len;
			}

			if (argmaxy < minlen) {
				argmaxy = minlen;
				argmax_id = p;
			}
		}

		//printf("k = %d, argmax = %f, argmax_id = %d\n",
		//	i, argmaxy, argmax_id); 
		memcpy(&(x[nx + i]), &(y[argmax_id]), sizeof(pixelinfo_t));

		//printf("x[k] = (%f, %f)\n", x[nx + i].x, x[nx + i].y);


		fprintf(stderr, "generated %d points\n", i);
	}

#if 0
	for (j = 0; j < k; j++) {
		argminx = 1.0e+16;
		//argmin_id = 0;
		for (i = 0; i < ny; i++) {
			for (l = 0; l < k; l++) {
				len = dist(y[i], x[l]);
				//printf("len(y[%d], x[%d]) = %f\n",
				//	i, l, len);
				if (argminx > len) {
					argminx   = len;
					argmin_id = l;
				} 
			}
		}

		//printf("k = %d, argmin = %f, argmin_id = %d\n",
		//	j, argminx, argmin_id); 

		copy_pixel(&(s[j]), &(x[argmin_id]));
	}

	/* append S_{j} to X */
	for (i = 0; i < k; i++) {
		copy_pixel(&(x[nx + i]), &(s[j]));
	}
#endif
}
