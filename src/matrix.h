/*
 * 4x4 matrix calculation routine.
 *
 * $Id: matrix.h,v 1.1.1.1 2004/01/06 13:57:09 syoyo Exp $
 */

#ifndef MATRIX_H
#define MATRIX_H

#include "ri.h"

/*
 * Matrix is defined in row-major order.
 *
 * e.x.
 * (1.0, 2.0, 4.0) translatiton matrix is like this.
 *
 * {{1.0, 0.0, 0.0, 0.0}
 *  {0.0, 1.0, 0.0, 0.0}
 *  {0.0, 0.0, 1.0, 0.0}
 *  {1.0, 2.0, 4.0, 1.0}}
 */

/*
 * We use union for easy use of AltiVec's vector type and traditional array
 * of float type.
 * In this way, member-wise access to a "vector float" type is enabled.
 *
 *
 *   Note: sizeof(ri_matrix_t) is always 64 byte.
 */
typedef union {
#ifdef WITH_ALTIVEC
	vector float    v[4];	/* AltiVec's vector float type */
#endif
	RtMatrix        e;	/* elements of traditional form */
} ri_matrix_t;

#ifdef __cplusplus
extern "C" {
#endif

void ri_matrix_identity	(      ri_matrix_t *dst);
void ri_matrix_mul	(      ri_matrix_t *dst,
 				 const ri_matrix_t *a,
 				 const ri_matrix_t *b);
void ri_matrix_translate	(      ri_matrix_t *dst,
					       RtFloat x, RtFloat y, RtFloat z);
/* \todo implement this */
void ri_matrix_rotate	(      ri_matrix_t * dst,
 				       RtFloat angle,
 				       RtFloat axisx,
 				       RtFloat axisy,
 				       RtFloat axisz);
void ri_matrix_scale	(      ri_matrix_t *dst,
					       RtFloat sx, RtFloat sy, RtFloat sz);
/* Z axis perspective transformation */
void ri_matrix_perspective(      ri_matrix_t *dst, RtFloat d);
void ri_matrix_transpose	(      ri_matrix_t *dst);
void ri_matrix_inverse	(      ri_matrix_t *dst);
void ri_matrix_print	(const ri_matrix_t *mat);
/* create ri_matrix_t type vector from RtVector */
void ri_matrix_set	(      ri_matrix_t *dst,
 				       RtMatrix src);
void ri_matrix_copy	(      ri_matrix_t *dst,
 				 const ri_matrix_t *src);

ri_matrix_t *ri_matrix_new       ();
void         ri_matrix_free      (ri_matrix_t *mat);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif
