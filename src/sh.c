#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <math.h>
#include <stdlib.h>
#include <stdio.h>

#ifndef M_PI
#define M_PI 3.1415926535
#endif

/* normalization factor */
static int    factorial(int n);
static int    doublefactorial(int n);
static double K(int l, int m);
static double legendreP(int l, int m, double x);

void
shY(float sh[25], float x, float y, float z)
{
	/* A is normalization factor: A = sqrt((2 * l + 1 / (4 * PI))) */

	float c[25];

	c[ 0] = 0.282095;

	c[ 1] = 0.488603;
	c[ 2] = 0.488603;
	c[ 3] = 0.488603;

	c[ 4] = 1.092548;
	c[ 5] = 1.092548;
	c[ 6] = 0.315392;
	c[ 7] = 1.092548;
	c[ 8] = 0.546274;

	c[ 9] = 0.590044;			// Y{3, -3}
	c[10] = 2.89061;			// Y{3, -2}
	c[11] = 0.457046;			// Y{3, -1}
	c[12] = 0.373176;			// Y{3,  0}
	c[13] = 0.457046;			// Y{3,  1}
	c[14] = 1.44531;			// Y{3,  2}
	c[15] = 0.590044;			// Y{3,  3}

	c[16] = 2.50334;			// Y{4, -4}
	c[17] = 1.77013;			// Y{4, -3}
	c[18] = 0.946175;			// Y{4, -2}
	c[19] = 0.669047;			// Y{4, -1}
	c[20] = 0.105786;			// Y{4,  0}
	c[21] = 0.669047;			// Y{4,  1}
	c[22] = 0.473087;			// Y{4,  2}
	c[23] = 1.77013;			// Y{4,  3}
	c[24] = 0.625836;			// Y{4,  4}

	/* Y_{0,0} */
	sh[ 0] = c[ 0];

	/* Y_{1,-1} Y_{1,0}, Y_{1,1} */
	sh[ 1] = c[ 1] * y;
	sh[ 2] = c[ 2] * z;
	sh[ 3] = c[ 3] * x;

	/* Y_{2, -2} Y_{2,-1}, Y_{2,1} */
	sh[ 4] = c[ 4] * x * y;
	sh[ 5] = c[ 5] * y * z;
	sh[ 7] = c[ 7] * x * z;

	/* Y_{2,0} */
	sh[ 6] = c[ 6] * (3.0 * z * z - 1.0);

	/* Y_{2,2} */
	sh[ 8] = c[ 8] * (x * x - y * y);

	/* Y_{3, -3} = A * sqrt(5/8) * (3 * x^2 * y - y^3)		*/
	sh[ 9] = c[ 9] * (3.0 * x * x * y - y * y * y); 

	/* Y_{3, -2} = A * sqrt(15) * x * y * z 			*/
	sh[10] = c[10] * x * y * z;

	/* Y_{3, -1} = A * sqrt(3/8) * y * (5 * z^2 - 1)		*/
	sh[11] = c[11] * y * (5.0 * z * z - 1.0);

	/* Y_{3,  0} = A * (1/2) * (5 * z^3 - 3 *z)			*/
	sh[12] = c[12] * (5.0 * z * z * z - 3 * z);

	/* Y_{3,  1} = A * sqrt(3/8) * x * (5 * z^2 - 1)		*/
	sh[13] = c[13] * x * (5.0 * z * z  - 1.0);

	/* Y_{3,  2} = A * sqrt(15/4) * z *(x^2 - y^2)			*/
	sh[14] = c[14] * z * (x * x - y * y);

	/* Y_{3,  3} = A * sqrt(5/8) * (x^3 - 3 * x * y^2)		*/
	sh[15] = c[15] * (x * x * x - 3.0 * x * y * y);

	/* Y_{4, -4} = A * sqrt(35/4) * (x^3 * y - x * y^3)		*/
	sh[16] = c[16] * (x * x * x * y - x * y * y * y);

	/* Y_{4, -3} = A * sqrt(35/8) * z * (3 * x^2 * y - y^3)		*/
	sh[17] = c[17] * z * (3.0 * x * x * y - y * y * y);

	/* Y_{4, -2} = A * sqrt(5/4) * x * y * (7 * z^2 - 1)		*/
	sh[18] = c[18] * x * y * (7.0 * z * z - 1.0);

	/* Y_{4, -1} = A * sqrt(5/8) * (7 * y * z^3 - 3 * y * z)	*/
	sh[19] = c[19] * (7.0 * y * z * z * z - 3.0 * y * z);

	/* Y_{4,  0} = A * (1/8) * (35 * z^4- 30 * z^2 + 3)		*/
	sh[20] = c[20] * (35.0 * z * z * z * z - 30.0 * z * z + 3.0);

	/* Y_{4,  1} = A * sqrt(5/8) * (7 * x * z^3 - 3 * x * z)	*/
	sh[21] = c[21] * (7.0 * x * z * z * z - 3.0 * x * z);

	/* Y_{4,  2} = A * sqrt(5/16) * (x^2 - y^2) * (7 * z^2 - 1)	*/
	sh[22] = c[22] * (x * x - y * y) * (7.0 * z * z - 1.0);

	/* Y_{4,  3} = A * sqrt(35/8) * z * (x^3 - 3 * x * y^2)		*/
	sh[23] = c[23] * z * (x * x * x - 3.0 * x * y * y);

	/* Y_{4,  4} = A * sqrt(35/64) * (x^4 - 6 * x^2 * y^2 + y^4)	*/
	sh[24] = c[24] * (x * x * x * x - 6.0 * x * x * y * y + y * y * y * y);
}

static int
factorial(int n)
{
	/* n! */

	int ret;

	if (n == 0) return 1;

	ret = n;
	
	while (--n > 0) {
		ret *= n;
	}

	return ret;
}

static int
doublefactorial(int n)
{
	/* n!! */	
	int ret;

	if (n == 0 || n == -1) return 1;

	ret = n;
	
	while ((n -= 2) > 0) {
		ret *= n;
	}

	return ret;
}

static double
K(int l, int m)
{
	double c1, c2;

	c1 = (double)(2 * l + 1) / (4.0 * M_PI);
	c2 = (double)factorial(l - m) / (double)factorial(l + m);

	return sqrt(c1 * c2);

}

/*
 * this scheme does not work if order l is higher...
 */
static double
y(int l, int m, double theta, double phi)
{
	double ret;

	if (m == 0) {
		ret = K(l, 0) * legendreP(l, 0, cos(theta));
	} else if (m > 0) {
		ret = sqrt(2.0) * K(l, m) * cos(m * phi) * legendreP(l, m, cos(theta));
	} else {	/* m < 0 */
		ret = sqrt(2.0) * K(l, -m) * sin(-m * phi) * legendreP(l, -m, cos(theta));
	}

	return ret;
}

static double
legendreP(int l, int m, double x)
{
	double ret;

	if (l == m) {
		ret = pow(-1, m) * doublefactorial(2 * m - 1) * pow(sqrt(1.0 - x * x), m);	
	} else if (l == m + 1) {
		ret = x * (2.0 * m + 1.0) * legendreP(m, m, x);	
	} else {
		ret = x * (2.0 * l - 1.0) * legendreP(l - 1, m, x) -
			  (l + m - 1.0) * legendreP(l - 2, m, x);	
		ret /= (double)(l - m);
	}

	return ret;
}

#if 0
int
main(int argc, char **argv)
{
	if (argc < 5) exit(-1);

	printf("y(%d, %d, %f, %f)  = %f\n", atoi(argv[1]), atoi(argv[2]), atof(argv[3]), atof(argv[4]), y(atoi(argv[1]), atoi(argv[2]), atof(argv[3]), atof(argv[4])));
}

#endif
