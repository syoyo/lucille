/*
 * $Id: quaternion.c,v 1.2 2004/02/08 16:38:48 syoyo Exp $
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <math.h>
#include <stdio.h>

#include "vector.h"
#include "quaternion.h"

void
ri_quat_set(ri_quat_t *quat, float theta, const ri_vector_t *axis)
{
	const double deg2rad = 3.14159265 / 180.0;

	ri_vector_t nvec;

	float s = (float)sin(-deg2rad * theta / 2.0);
	float c = (float)cos(-deg2rad * theta / 2.0);

	ri_vector_copy(&nvec, axis);
	ri_vector_normalize(&nvec);

	quat->x = nvec.e[0] * s;
	quat->y = nvec.e[1] * s;
	quat->z = nvec.e[2] * s;
	quat->w = c;
}

void
ri_quat_mat(ri_matrix_t *mat, const ri_quat_t quat)
{
	float norm;
	float s;
	float xs, ys, zs, wx, wy, wz, xx, xy, xz, yy, yz, zz;
	
	norm = quat.x * quat.x + quat.y * quat.y
	     + quat.z * quat.z + quat.w * quat.w; 

	s = (norm > 0.0) ? 2.0 / norm : 0.0;

	xs = quat.x *  s; ys = quat.y *  s; zs = quat.z *  s;
	wx = quat.w * xs; wy = quat.w * ys; wz = quat.w * zs;
	xx = quat.x * xs; xy = quat.x * ys; xz = quat.x * zs;
	yy = quat.y * ys; yz = quat.y * zs; zz = quat.z * zs;

	mat->e[0][0] = 1.0 - (yy + zz);
	mat->e[0][1] = xy - wz;
	mat->e[0][2] = xz + wy;
	mat->e[0][3] = 0.0;

	mat->e[1][0] = xy + wz;
	mat->e[1][1] = 1.0 - (xx + zz);
	mat->e[1][2] = yz - wx;
	mat->e[1][3] = 0.0;
	
	mat->e[2][0] = xz - wy;
	mat->e[2][1] = yz + wx;
	mat->e[2][2] = 1.0 - (xx + yy);
	mat->e[2][3] = 0.0;
	
	mat->e[3][0] = 0.0;
	mat->e[3][1] = 0.0;
	mat->e[3][2] = 0.0;
	mat->e[3][3] = 1.0;
}

