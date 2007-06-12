/*
 * 2D Z curve(Lebesgue curve) order scanning routine.
 *
 * $Id: zorder2d.c,v 1.1 2004/10/22 16:58:02 syoyo Exp $
 */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#if defined(LINUX) || defined(__APPLE__)
#include <sys/time.h>
#endif

unsigned int g_z_order = 0;			/* level */
unsigned int g_z_s = 0;
unsigned int g_z_quadrant = 0;
unsigned int g_z_offset = 0;
unsigned int g_z_maxs = 0;

unsigned int g_z_table[4][2] = {{0, 0}, {1, 0}, {0, 1}, {1, 1}};

/* expanded table for bit-interleaving.
 * each N'th bit moves (2N-1)'th bit. ( N > 1 ).
 * 1'th bit stays there.
 *
 * 00000000   -> 00000000_00000000
 * 00000001   -> 00000000_00000001
 * 00000010   -> 00000000_00000100
 * 00000011   -> 00000000_00000101
 * 00000100   -> 00000000_00010000
 * 00000101   -> 00000000_00010001
 * 00000110   -> 00000000_00010100
 * ...
 * 11111111   -> 01010101_01010101
 *
 */
unsigned short g_z_tbl_expand[256];


void
compute_expand_table()
{
	unsigned int   i, j;
	unsigned int   bit;
	unsigned int   v;
	unsigned int   ret;
	unsigned int   mask = 0x1;
	int            shift;

	for (i = 0; i < 256; i++) {
		v = i;
		shift = 0;
		ret = 0;
		for (j = 0; j < 8; j++) {
			/* extract (j+1)'th bit */
			bit = (v >> j) & mask;

			ret += bit << shift;
			shift += 2;
		}

		g_z_tbl_expand[i] = (unsigned short)ret;
	}
}

/* 2D Z curve code. */
void
zorder_xy_from_s(unsigned int s, int n, unsigned int *xp, unsigned int *yp)
{
	unsigned int i;
	unsigned int bit;
	unsigned int mask = 0x0000003;
	unsigned int t;
	unsigned int scale = 1;
	
	(*xp) = 0; (*yp) = 0;
	t = s;

	/* unsinged int(32bit) can represent 2^16 pattern of 2D Z curve. */
	for (i = 0; i < g_z_order; i++) {
		bit = (t & mask);
		
		(*xp) += scale * (g_z_table[bit][0]);
		(*yp) += scale * (g_z_table[bit][1]);

		t = t >> 2;
		scale = scale << 1;
	}
}

/*
 * Function: zorder_setup
 *
 *     Setups variables required for Z curve order scan rendering.
 *
 * Parameters:
 *
 *     wpix        - the number of pixels in image width.
 *     hpix        - the number of pixels in image height.
 *     bucket_size - the size of bucket in pixels.
 *
 * Returns:
 *
 *     None.
 */
void
zorder_setup(unsigned int wpix, unsigned int hpix, unsigned int bucket_size)
{
	unsigned int i;
	unsigned int pow2n;
	unsigned int maxpixlen;

	if (wpix > hpix) {
		maxpixlen = wpix;
	} else {
		maxpixlen = hpix;
	} 

	maxpixlen /= bucket_size * 2;

	if (maxpixlen == 0) {
		g_z_order = 0;
		return;
	}

	/* find nearest 2^n value against maxpixlen.
	 * I think there exists more sofisticated way for this porpose ...
	 */
	pow2n = 1;
	for (i = 1; i < maxpixlen; i++) {
		if (maxpixlen <= pow2n) break;
		pow2n *= 2;
	}
	
	g_z_order = i;
	g_z_maxs = (unsigned int) pow(4, i - 1);
	g_z_offset = pow2n - 1;

}

/*
 * Function: zorder_get_nextlocation
 *
 *     Returns next location in Z curve order.
 *
 *
 * Parameters:
 *
 *     *xp - next location of x.
 *     *yp - next location of y.
 *
 * Returns:
 *
 *     0 if traversed all Z curve, 1 if not.
 */ 
int
zorder_get_nextlocation(unsigned int *xp, unsigned int *yp)
{
	if (g_z_s >= g_z_maxs) return 0;
	//if (g_z_quadrant >= 4) return 0;

	zorder_xy_from_s(g_z_s, g_z_order, xp, yp);

	switch (g_z_quadrant) {
		case 0:		/* upper-right	*/
		*xp += g_z_offset + 1;
		*yp += g_z_offset + 1;
		break;

		case 1:		/* upper-left	*/
		*xp = g_z_offset - (*xp);
		*yp += g_z_offset + 1;
		break;

		case 2:		/* bottom-left	*/
		*xp = g_z_offset - (*xp);
		*yp = g_z_offset - (*yp);
		break;

		case 3:		/* bottom-right	*/
		*xp += g_z_offset + 1;
		*yp = g_z_offset - (*yp);
		break;
	}
	
#if 0
	g_z_s++;
	if (g_z_s >= g_z_maxs) {
		g_z_s = 0;
		g_z_quadrant++;
	}
#else
	g_z_quadrant++;

	if (g_z_quadrant >= 4) {
		g_z_quadrant = 0;
		g_z_s++;
	}
#endif

	return 1;
}

#ifdef TEST

void
test1()
{
	unsigned int x, y;

	while (1) {

		if (!zorder_get_nextlocation(&x, &y)) {
			break;
		} 
	}
}

#define ZINDEX(x, y) (g_z_tbl_expand[(x)] | (g_z_tbl_expand[(y)] << 1))

void
test2()
{
	unsigned int i, j;
	unsigned int idx;

	for (i = 0; i < 8; i++) {
		for (j = 0; j < 8; j++) {
			idx = ZINDEX(j, i);

			printf("(%d, %d) => index = %d\n", j, i, idx); 
		}
	}
}

void 
main()
{
	double elapsed;
	int done = 0;

	struct timeval start, end;


	zorder_setup(1024, 1024, 32);

	compute_expand_table();

	gettimeofday(&start, NULL);

	test1();

	gettimeofday(&end, NULL);

	elapsed = (double)(end.tv_sec - start.tv_sec) + 
                  (double)(end.tv_usec - start.tv_usec) / (double)1.0e6;

	printf("test1: elapsed = %f\n", elapsed);

	gettimeofday(&start, NULL);

	test2();

	gettimeofday(&end, NULL);

	elapsed = (double)(end.tv_sec - start.tv_sec) + 
                  (double)(end.tv_usec - start.tv_usec) / (double)1.0e6;

	printf("test2: elapsed = %f\n", elapsed);

}
#endif
