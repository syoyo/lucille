/*
 * quatarnion utility.
 * 
 * $Id: quaternion.h,v 1.3 2004/02/08 16:38:48 syoyo Exp $
 */

#ifndef QUATERNION_H
#define QUATERNION_H

#include "ri.h"
#include "vector.h"
#include "matrix.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _ri_quat_t
{
	ri_float_t x, y, z, w;
} ri_quat_t;

extern void ri_quat_set(ri_quat_t         *quat,
		 	ri_float_t         theta,
			const ri_vector_t *axis);

extern void ri_quat_mat(ri_matrix_t     *mat,
			const ri_quat_t  quat);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif

