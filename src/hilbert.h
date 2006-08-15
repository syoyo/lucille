#ifndef HILBERT_H
#define HILBERT_H

#ifdef __cplusplus
extern "C" {
#endif

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

#if 0
/*
 * g_mask can be omitted...
 * g_mask[x] can be replaced by (1 << DIM - 1 - x)
 */
const U_int g_mask[] = {4, 2, 1};
#endif

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

extern Hcode H_encode(Point p);
extern Point H_decode (Hcode H);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif
