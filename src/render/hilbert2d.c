/*
 * 2D Hilbert curve order scanning routine.
 *
 * $Id: hilbert2d.c,v 1.2 2004/10/20 14:12:02 syoyo Exp $
 */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

unsigned int g_order = 0;
unsigned int g_s = 0;
unsigned int g_quadrant = 0;
unsigned int g_offset = 0;
unsigned int g_maxs = 0;

/* 2D hilbert code. from Hacker's Delight. */
void
hil_xy_from_s(unsigned int s, int n, unsigned int *xp, unsigned int *yp)
{
	unsigned int comp, swap, cs, t, sr;

	s = s | (0x55555555 << 2 * n);
	sr = (s >> 1) & 0x55555555;
	cs = ((s & 0x55555555) + sr) ^ 0x55555555;

	cs = cs ^ (cs >> 2);	
	cs = cs ^ (cs >> 4);	
	cs = cs ^ (cs >> 8);	
	cs = cs ^ (cs >> 16);	
	swap = cs & 0x55555555;
	comp = (cs >> 1) & 0x55555555;

	t = (s & swap) ^ comp;
	s = s ^ sr ^ t ^ (t << 1);
	s = s & ((1 << 2 * n) - 1);

	t = (s ^ (s >> 1)) & 0x22222222; s = s ^ t ^ (t << 1);
	t = (s ^ (s >> 2)) & 0x0C0C0C0C; s = s ^ t ^ (t << 2);
	t = (s ^ (s >> 4)) & 0x00F000F0; s = s ^ t ^ (t << 4);
	t = (s ^ (s >> 8)) & 0x0000FF00; s = s ^ t ^ (t << 8);

	*yp = s >> 16;
	*xp = s & 0xFFFF;
}

/*
 * Function: hil_setup
 *
 *     Setups variables required for hilbert order scan rendering.
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
hil_setup(unsigned int wpix, unsigned int hpix, unsigned int bucket_size)
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
		g_order = 0;
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
	
	g_order = i;
	g_maxs = (unsigned int) pow(4, i - 1);
	g_offset = pow2n - 1;

}

/*
 * Function: hil_get_nextlocation
 *
 *     Returns next location in hilbert curve order.
 *
 *
 * Parameters:
 *
 *     *xp - next location of x.
 *     *yp - next location of y.
 *
 * Returns:
 *
 *     0 if traversed all hilbert curve, 1 if not.
 */ 
int
hil_get_nextlocation(unsigned int *xp, unsigned int *yp)
{
	if (g_s >= g_maxs) return 0;
	//if (g_quadrant >= 4) return 0;

	hil_xy_from_s(g_s, g_order, xp, yp);

	switch (g_quadrant) {
		case 0:		/* upper-right	*/
		*xp += g_offset + 1;
		*yp += g_offset + 1;
		break;

		case 1:		/* upper-left	*/
		*xp = g_offset - (*xp);
		*yp += g_offset + 1;
		break;

		case 2:		/* bottom-left	*/
		*xp = g_offset - (*xp);
		*yp = g_offset - (*yp);
		break;

		case 3:		/* bottom-right	*/
		*xp += g_offset + 1;
		*yp = g_offset - (*yp);
		break;
	}
	
#if 0
	g_s++;
	if (g_s >= g_maxs) {
		g_s = 0;
		g_quadrant++;
	}
#else
	g_quadrant++;

	if (g_quadrant >= 4) {
		g_quadrant = 0;
		g_s++;
	}
#endif

	return 1;
}

#ifdef TEST
void 
main()
{
	unsigned int s;
	unsigned int x, y;
	int done = 0;

	hil_setup(128, 128, 8);

	while (1) {

		if (!hil_get_nextlocation(&x, &y)) {
			break;
		} 

	}
}
#endif
