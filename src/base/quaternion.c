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
ri_quat_set(ri_quat_t *quat, ri_float_t theta, const ri_vector_t axis)
{
    const ri_float_t deg2rad = (ri_float_t)M_PI / 180.0;

    ri_vector_t nvec;

    ri_float_t s = (ri_float_t)sin(-deg2rad * theta / 2.0);
    ri_float_t c = (ri_float_t)cos(-deg2rad * theta / 2.0);

    ri_vector_copy(nvec, axis);
    ri_vector_normalize(nvec);

    quat->x = nvec[0] * s;
    quat->y = nvec[1] * s;
    quat->z = nvec[2] * s;
    quat->w = c;
}

void
ri_quat_mat(ri_matrix_t *mat, const ri_quat_t quat)
{
    ri_float_t norm;
    ri_float_t s;
    ri_float_t xs, ys, zs, wx, wy, wz, xx, xy, xz, yy, yz, zz;
    
    norm = quat.x * quat.x + quat.y * quat.y
         + quat.z * quat.z + quat.w * quat.w; 

    s = (norm > 0.0) ? 2.0 / norm : 0.0;

    xs = quat.x *  s; ys = quat.y *  s; zs = quat.z *  s;
    wx = quat.w * xs; wy = quat.w * ys; wz = quat.w * zs;
    xx = quat.x * xs; xy = quat.x * ys; xz = quat.x * zs;
    yy = quat.y * ys; yz = quat.y * zs; zz = quat.z * zs;

    mat->f[0][0] = 1.0 - (yy + zz);
    mat->f[0][1] = xy - wz;
    mat->f[0][2] = xz + wy;
    mat->f[0][3] = 0.0;

    mat->f[1][0] = xy + wz;
    mat->f[1][1] = 1.0 - (xx + zz);
    mat->f[1][2] = yz - wx;
    mat->f[1][3] = 0.0;
    
    mat->f[2][0] = xz - wy;
    mat->f[2][1] = yz + wx;
    mat->f[2][2] = 1.0 - (xx + yy);
    mat->f[2][3] = 0.0;
    
    mat->f[3][0] = 0.0;
    mat->f[3][1] = 0.0;
    mat->f[3][2] = 0.0;
    mat->f[3][3] = 1.0;
}

