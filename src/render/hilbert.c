/*
 * copyright J.K.Lawder 2002 
 *
 * Functions for calculating the Hilbert Curve sequence number of a point
 * and the inverse operation. Applies to the '32nd order' Hilbert Curve.
 *
 * For further information see :
 * Calculation of Mappings Between One and n-dimensional Values Using the
 * Hilbert Space-filling Curve. Research Report BBKCS-00-01
 * http://www.dcs.bbk.ac.uk/~jkl/publications.html
 *
 * last changed 17.1.2002
 */

#include <stdio.h>
#include <stdlib.h>

#include "hilbert.h"

#if 0		/* moved to hilbert.h */
/*
 * 'DIM' is the number of dimensions in space through which the
 * Hilbert Curve passes.
 * Don't use this implementation with values for DIM of > 31! 
 * Also, make sure you use a 32 bit compiler!
 */
#define	DIM 3

typedef unsigned int U_int;

/*
 * An Hcode holds the Hilbert Curve sequence number of a point as an array
 * of unsigned ints. The least significant bit of hcode[0] is the least
 * significant bit of the sequence number.
 */
typedef struct {
	U_int	hcode[DIM];
}Hcode;

typedef Hcode Point;

#endif

/*
 * g_mask can be omitted...
 * g_mask[x] can be replaced by (1 << DIM - 1 - x)
 */
const U_int g_mask[] = {4, 2, 1};

#if 0	/* moved to hilbert.h */

/*
 * retained for historical reasons: the number of bits in an attribute value:
 * effectively the order of a curve
 */
#define		NUMBITS			32

/*
 * the number of bits in a word used to store an hcode (or in an element of
 * an array that's used)
 */
#define		WORDBITS		32

#endif


/*============================================================================*/
/*                            H_encode					      */
/*============================================================================*/
/*
 * given the coordinates of a point, it finds the sequence number of the point
 * on the Hilbert Curve
 */
Hcode H_encode(Point p)
{
	U_int	mask = (U_int)1 << WORDBITS - 1, element, temp1, temp2,
		A, W = 0, S, tS, T, tT, J, P = 0, xJ;
	Hcode	h = {0};
	int	i = NUMBITS * DIM - DIM, j;

	for (j = A = 0; j < DIM; j++)
		if (p.hcode[j] & mask)
			A |= g_mask[j];

	S = tS = A;

	P |= S & g_mask[0];
	for (j = 1; j < DIM; j++)
		if( S & g_mask[j] ^ (P >> 1) & g_mask[j])
			P |= g_mask[j];

	/* add in DIM bits to hcode */
	element = i / WORDBITS;
	if (i % WORDBITS > WORDBITS - DIM)
	{
		h.hcode[element] |= P << i % WORDBITS;
		h.hcode[element + 1] |= P >> WORDBITS - i % WORDBITS;
	}
	else
		h.hcode[element] |= P << i - element * WORDBITS;

	J = DIM;
	for (j = 1; j < DIM; j++)
		if ((P >> j & 1) == (P & 1))
			continue;
		else
			break;
	if (j != DIM)
		J -= j;
	xJ = J - 1;

	if (P < 3)
		T = 0;
	else
		if (P % 2)
			T = (P - 1) ^ (P - 1) / 2;
		else
			T = (P - 2) ^ (P - 2) / 2;
	tT = T;

	for (i -= DIM, mask >>= 1; i >=0; i -= DIM, mask >>= 1)
	{
		for (j = A = 0; j < DIM; j++)
			if (p.hcode[j] & mask)
				A |= g_mask[j];

		W ^= tT;
		tS = A ^ W;
		if (xJ % DIM != 0)
		{
			temp1 = tS << xJ % DIM;
			temp2 = tS >> DIM - xJ % DIM;
			S = temp1 | temp2;
			S &= ((U_int)1 << DIM) - 1;
		}
		else
			S = tS;

		P = S & g_mask[0];
		for (j = 1; j < DIM; j++)
			if( S & g_mask[j] ^ (P >> 1) & g_mask[j])
				P |= g_mask[j];

		/* add in DIM bits to hcode */
		element = i / WORDBITS;
		if (i % WORDBITS > WORDBITS - DIM)
		{
			h.hcode[element] |= P << i % WORDBITS;
			h.hcode[element + 1] |= P >> WORDBITS - i % WORDBITS;
		}
		else
			h.hcode[element] |= P << i - element * WORDBITS;

		if (i > 0)
		{
			if (P < 3)
				T = 0;
			else
				if (P % 2)
					T = (P - 1) ^ (P - 1) / 2;
				else
					T = (P - 2) ^ (P - 2) / 2;

			if (xJ % DIM != 0)
			{
				temp1 = T >> xJ % DIM;
				temp2 = T << DIM - xJ % DIM;
				tT = temp1 | temp2;
				tT &= ((U_int)1 << DIM) - 1;
			}
			else
				tT = T;

			J = DIM;
			for (j = 1; j < DIM; j++)
				if ((P >> j & 1) == (P & 1))
					continue;
				else
					break;
			if (j != DIM)
				J -= j;

			xJ += J - 1;
		/*	J %= DIM;*/
		}
	}
	return h;
}

/*============================================================================*/
/*                            H_decode					      */
/*============================================================================*/
/*
 * given the sequence number of a point, it finds the coordinates of the point
 * on the Hilbert Curve
 */
Point H_decode (Hcode H)
{
	U_int	mask = (U_int)1 << WORDBITS - 1, element, temp1, temp2,
		A, W = 0, S, tS, T, tT, J, P = 0, xJ;
	Point	pt = {0};
	int	i = NUMBITS * DIM - DIM, j;


	/*--- P ---*/
	element = i / WORDBITS;
	P = H.hcode[element];
	if (i % WORDBITS > WORDBITS - DIM)
	{
		temp1 = H.hcode[element + 1];
		P >>= i % WORDBITS;
		temp1 <<= WORDBITS - i % WORDBITS;
		P |= temp1;
	}
	else
		P >>= i % WORDBITS;	/* P is a DIM bit hcode */

	/* the & masks out spurious highbit values */
	#if DIM < WORDBITS
		P &= (1 << DIM) -1;
	#endif

	/*--- xJ ---*/
	J = DIM;
	for (j = 1; j < DIM; j++)
		if ((P >> j & 1) == (P & 1))
			continue;
		else
			break;
	if (j != DIM)
		J -= j;
	xJ = J - 1;

	/*--- S, tS, A ---*/
	A = S = tS = P ^ P / 2;


	/*--- T ---*/
	if (P < 3)
		T = 0;
	else
		if (P % 2)
			T = (P - 1) ^ (P - 1) / 2;
		else
			T = (P - 2) ^ (P - 2) / 2;

	/*--- tT ---*/
	tT = T;

	/*--- distrib bits to coords ---*/
	for (j = DIM - 1; P > 0; P >>=1, j--)
		if (P & 1)
			pt.hcode[j] |= mask;


	for (i -= DIM, mask >>= 1; i >=0; i -= DIM, mask >>= 1)
	{
		/*--- P ---*/
		element = i / WORDBITS;
		P = H.hcode[element];
		if (i % WORDBITS > WORDBITS - DIM)
		{
			temp1 = H.hcode[element + 1];
			P >>= i % WORDBITS;
			temp1 <<= WORDBITS - i % WORDBITS;
			P |= temp1;
		}
		else
			P >>= i % WORDBITS;	/* P is a DIM bit hcode */

		/* the & masks out spurious highbit values */
		#if DIM < WORDBITS
			P &= (1 << DIM) -1;
		#endif

		/*--- S ---*/
		S = P ^ P / 2;

		/*--- tS ---*/
		if (xJ % DIM != 0)
		{
			temp1 = S >> xJ % DIM;
			temp2 = S << DIM - xJ % DIM;
			tS = temp1 | temp2;
			tS &= ((U_int)1 << DIM) - 1;
		}
		else
			tS = S;

		/*--- W ---*/
		W ^= tT;

		/*--- A ---*/
		A = W ^ tS;

		/*--- distrib bits to coords ---*/
		for (j = DIM - 1; A > 0; A >>=1, j--)
			if (A & 1)
				pt.hcode[j] |= mask;

		if (i > 0)
		{
			/*--- T ---*/
			if (P < 3)
				T = 0;
			else
				if (P % 2)
					T = (P - 1) ^ (P - 1) / 2;
				else
					T = (P - 2) ^ (P - 2) / 2;

			/*--- tT ---*/
			if (xJ % DIM != 0)
			{
				temp1 = T >> xJ % DIM;
				temp2 = T << DIM - xJ % DIM;
				tT = temp1 | temp2;
				tT &= ((U_int)1 << DIM) - 1;
			}
			else
				tT = T;

			/*--- xJ ---*/
			J = DIM;
			for (j = 1; j < DIM; j++)
				if ((P >> j & 1) == (P & 1))
					continue;
				else
					break;
			if (j != DIM)
				J -= j;
			xJ += J - 1;
		}
	}
	return pt;
}
