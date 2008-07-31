/*
 * $Id: matrix.c,v 1.3 2004/02/12 05:17:33 syoyo Exp $
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "matrix.h"
#include "quaternion.h"
#include "memory.h"
#include "vector.h"

/* ---------------------------------------------------------------------------
 *
 * Public functions
 *
 * ------------------------------------------------------------------------ */


void
ri_matrix_identity(
    ri_matrix_t *dst)
{
    register int i, j;

    for (j = 0; j < 4; j++) {
        for (i = 0; i < 4; i++) {
            if (j == i) dst->f[j][i] = 1.0f;
            else        dst->f[j][i] = 0.0f;
        }
    }
} 
            
void
ri_matrix_mul(
    ri_matrix_t       *dst,
    const ri_matrix_t *a,
    const ri_matrix_t *b)
{
    register int i, j, k;

    for (j = 0; j < 4; j++) {
        for (i = 0; i < 4; i++) {
            dst->f[j][i] = 0.0f;
            for (k = 0; k < 4; k++) {
                dst->f[j][i] += a->f[j][k] * b->f[k][i];
            }
        }
    }
}

void
ri_matrix_translate(
    ri_matrix_t *dst,
    RtFloat      x,
    RtFloat      y,
    RtFloat      z)
{
    ri_matrix_t trans;
    ri_matrix_t tmp;

    /*
     * RenderMan states matrix is row major.
     *
     *     | 1 0 0 x | ^T
     *     | 0 1 0 y |
     * T = | 0 0 1 z |
     *     | 0 0 0 1 |    
     */
    ri_matrix_identity(&trans);
    trans.f[3][0] = x;    
    trans.f[3][1] = y;    
    trans.f[3][2] = z;

    ri_matrix_copy(&tmp, dst);
    ri_matrix_mul(dst, &trans, &tmp);

}

void
ri_matrix_rotate(
    ri_matrix_t *dst,
    RtFloat      angle,
    RtFloat      axisx,
    RtFloat      axisy,
    RtFloat      axisz)
{
    RtVector    v;
    ri_vector_t axis;
    ri_quat_t   quat;
    ri_matrix_t tmp;
    ri_matrix_t rotmat;

    v[0] = axisx; v[1] = axisy; v[2] = axisz;
    
    ri_vector_set_from_rman(axis, v);

    ri_quat_set(&quat, angle, axis);

    ri_quat_mat(&rotmat, quat);

    /* dst = dst R     */
    ri_matrix_copy(&tmp, dst);
    ri_matrix_mul(dst, &rotmat, &tmp);
    
#if 0
    const float deg2rad = 3.141592 / 180.0;
    float c, s, t;
    ri_matrix_t rotmat;
    ri_matrix_t tmp;

    
    c = cos(angle * deg2rad);
    s = sin(angle * deg2rad);
    t = 1 - c;

    /*                                                  
     * RenderMan states matrix is row major.
     *
     *     | t x^2 +  c  txy   - sz  txz   + sy       0| ^T
     * R = | txy   + sz  t y^2 +  c  tyz   - sx       0|
     *     | txz   - sy  tyz   + sx  t z^2 +  c       0|
     *     |          0           0           0       1|
     */
    rotmat.f[0][0] = t * axisx * axisx + c;
    rotmat.f[0][1] = t * axisx * axisy + s * axisz;
    rotmat.f[0][2] = t * axisx * axisz - s * axisy;
    rotmat.f[0][3] = 0.0;

    rotmat.f[1][0] = t * axisx * axisy - s * axisz;
    rotmat.f[1][1] = t * axisy * axisy + c;
    rotmat.f[1][2] = t * axisy * axisy - s * axisx;
    rotmat.f[1][3] = 0.0;

    rotmat.f[2][0] = t * axisx * axisz + s * axisy;
    rotmat.f[2][1] = t * axisy * axisz - s * axisx;
    rotmat.f[2][2] = t * axisz * axisz + c;
    rotmat.f[2][3] = 0.0;

    rotmat.f[3][0] = 0.0;
    rotmat.f[3][1] = 0.0;
    rotmat.f[3][2] = 0.0;
    rotmat.f[3][3] = 1.0;

    /* dst = dst R     */
    ri_matrix_copy(&tmp, dst);
    ri_matrix_mul(dst, &rotmat, &tmp);
#endif
}

void
ri_matrix_scale(
    ri_matrix_t *dst,
    RtFloat      sx,
    RtFloat      sy,
    RtFloat      sz)
{
    ri_matrix_t scale;
    ri_matrix_t tmp;

    ri_matrix_identity(&scale);

    scale.f[0][0] = sx;
    scale.f[1][1] = sy;
    scale.f[2][2] = sz;

    ri_matrix_copy(&tmp, dst);
    ri_matrix_mul(dst, &scale, &tmp);

}

void 
ri_matrix_perspective(
    ri_matrix_t *dst,
    RtFloat      d)
{
    int i;
    RtFloat f;

    /* too small */
    if (fabs(d) < RI_EPSILON) return;

    f = 1.0f / d;

    for (i = 0; i < 4; i++) {
        dst->f[i][3] += dst->f[i][2] * f;
        dst->f[i][2] = 0.0f;
    }
}

void
ri_matrix_transpose(
    ri_matrix_t *dst)
{
    register int i, j;
    ri_matrix_t tmp;

    for (j = 0; j < 4; j++) {
        for (i = 0; i < 4; i++) {
            tmp.f[i][j] = dst->f[j][i];
        }
    }

    for (j = 0; j < 4; j++) {
        for (i = 0; i < 4; i++) {
            dst->f[j][i] = tmp.f[j][i];
        }
    }
}

void
ri_matrix_print(
    const ri_matrix_t *mat)
{
    int i, j;

    for (j = 0; j < 4; j++) {
        for (i = 0; i < 4; i++) {
            printf("%f ", mat->f[j][i]);
        }
        printf("\n");
    }
    printf("\n");
}

void
ri_matrix_set(
    ri_matrix_t *dst,
    RtMatrix     src)
{
    int i, j;

    for (j = 0; j < 4; j++) {
        for (i = 0; i < 4; i++) {
            dst->f[j][i] = src[j][i];
        }
    }
}

void
ri_matrix_copy(
    ri_matrix_t       *dst,
    const ri_matrix_t *src)
{
    int i, j;

    for (j = 0; j < 4; j++) {
        for (i = 0; i < 4; i++) {
            dst->f[j][i] = src->f[j][i];
        }
    }
}

void 
ri_matrix_inverse(
    ri_matrix_t *dst)
{
    /*
     * codes from intel web
     * cramer's rule version
     */
    int i, j;
    RtFloat tmp[12];    /* tmp array for pairs */
    RtFloat tsrc[16];    /* array of transpose source matrix */
    RtFloat det;        /* determinant */

    /* transpose matrix */
    for (i = 0; i < 4; i++) {
        tsrc[i     ] = dst->f[i][0];
        tsrc[i + 4 ] = dst->f[i][1];
        tsrc[i + 8 ] = dst->f[i][2];
        tsrc[i + 12] = dst->f[i][3];
    }    

    /* calculate pair for first 8 elements(cofactors) */
    tmp[0]  = tsrc[10] * tsrc[15];
    tmp[1]  = tsrc[11] * tsrc[14];
    tmp[2]  = tsrc[9]  * tsrc[15];
    tmp[3]  = tsrc[11] * tsrc[13];
    tmp[4]  = tsrc[9]  * tsrc[14];
    tmp[5]  = tsrc[10] * tsrc[13];
    tmp[6]  = tsrc[8]  * tsrc[15];
    tmp[7]  = tsrc[11] * tsrc[12];
    tmp[8]  = tsrc[8]  * tsrc[14];
    tmp[9]  = tsrc[10] * tsrc[12];
    tmp[10] = tsrc[8]  * tsrc[13];
    tmp[11] = tsrc[9]  * tsrc[12];

    /* calculate first 8 elements(cofactors) */
    dst->f[0][0]  = tmp[0] * tsrc[5] + tmp[3] * tsrc[6] + tmp[4] * tsrc[7];
    dst->f[0][0] -= tmp[1] * tsrc[5] + tmp[2] * tsrc[6] + tmp[5] * tsrc[7];
    dst->f[0][1]  = tmp[1] * tsrc[4] + tmp[6] * tsrc[6] + tmp[9] * tsrc[7];
    dst->f[0][1] -= tmp[0] * tsrc[4] + tmp[7] * tsrc[6] + tmp[8] * tsrc[7];
    dst->f[0][2]  = tmp[2] * tsrc[4] + tmp[7] * tsrc[5] + tmp[10] * tsrc[7];
    dst->f[0][2] -= tmp[3] * tsrc[4] + tmp[6] * tsrc[5] + tmp[11] * tsrc[7];
    dst->f[0][3]  = tmp[5] * tsrc[4] + tmp[8] * tsrc[5] + tmp[11] * tsrc[6];
    dst->f[0][3] -= tmp[4] * tsrc[4] + tmp[9] * tsrc[5] + tmp[10] * tsrc[6];
    dst->f[1][0]  = tmp[1] * tsrc[1] + tmp[2] * tsrc[2] + tmp[5] * tsrc[3];
    dst->f[1][0] -= tmp[0] * tsrc[1] + tmp[3] * tsrc[2] + tmp[4] * tsrc[3];
    dst->f[1][1]  = tmp[0] * tsrc[0] + tmp[7] * tsrc[2] + tmp[8] * tsrc[3];
    dst->f[1][1] -= tmp[1] * tsrc[0] + tmp[6] * tsrc[2] + tmp[9] * tsrc[3];
    dst->f[1][2]  = tmp[3] * tsrc[0] + tmp[6] * tsrc[1] + tmp[11] * tsrc[3];
    dst->f[1][2] -= tmp[2] * tsrc[0] + tmp[7] * tsrc[1] + tmp[10] * tsrc[3];
    dst->f[1][3]  = tmp[4] * tsrc[0] + tmp[9] * tsrc[1] + tmp[10] * tsrc[2];
    dst->f[1][3] -= tmp[5] * tsrc[0] + tmp[8] * tsrc[1] + tmp[11] * tsrc[2];

    /* calculate pairs for second 8 elements(cofactors) */
    tmp[0]  = tsrc[2] * tsrc[7];
    tmp[1]  = tsrc[3] * tsrc[6];
    tmp[2]  = tsrc[1] * tsrc[7];
    tmp[3]  = tsrc[3] * tsrc[5];
    tmp[4]  = tsrc[1] * tsrc[6];
    tmp[5]  = tsrc[2] * tsrc[5];
    tmp[6]  = tsrc[0] * tsrc[7];
    tmp[7]  = tsrc[3] * tsrc[4];
    tmp[8]  = tsrc[0] * tsrc[6];
    tmp[9]  = tsrc[2] * tsrc[4];
    tmp[10] = tsrc[0] * tsrc[5];
    tmp[11] = tsrc[1] * tsrc[4];

    /* calculate second 8 elements(cofactors) */
    dst->f[2][0]  = tmp[0] * tsrc[13] + tmp[3] * tsrc[14] + tmp[4] * tsrc[15];
    dst->f[2][0] -= tmp[1] * tsrc[13] + tmp[2] * tsrc[14] + tmp[5] * tsrc[15];
    dst->f[2][1]  = tmp[1] * tsrc[12] + tmp[6] * tsrc[14] + tmp[9] * tsrc[15];
    dst->f[2][1] -= tmp[0] * tsrc[12] + tmp[7] * tsrc[14] + tmp[8] * tsrc[15];
    dst->f[2][2] = tmp[2] * tsrc[12] + tmp[7] * tsrc[13] + tmp[10] * tsrc[15];
    dst->f[2][2]-= tmp[3] * tsrc[12] + tmp[6] * tsrc[13] + tmp[11] * tsrc[15];
    dst->f[2][3] = tmp[5] * tsrc[12] + tmp[8] * tsrc[13] + tmp[11] * tsrc[14];
    dst->f[2][3]-= tmp[4] * tsrc[12] + tmp[9] * tsrc[13] + tmp[10] * tsrc[14];
    dst->f[3][0] = tmp[2] * tsrc[10] + tmp[5] * tsrc[11] + tmp[1] * tsrc[9];
    dst->f[3][0]-= tmp[4] * tsrc[11] + tmp[0] * tsrc[9] + tmp[3] * tsrc[10];
    dst->f[3][1] = tmp[8] * tsrc[11] + tmp[0] * tsrc[8] + tmp[7] * tsrc[10];
    dst->f[3][1]-= tmp[6] * tsrc[10] + tmp[9] * tsrc[11] + tmp[1] * tsrc[8];
    dst->f[3][2] = tmp[6] * tsrc[9] + tmp[11] * tsrc[11] + tmp[3] * tsrc[8];
    dst->f[3][2]-= tmp[10] * tsrc[11] + tmp[2] * tsrc[8] + tmp[7] * tsrc[9];
    dst->f[3][3] = tmp[10] * tsrc[10] + tmp[4] * tsrc[8] + tmp[9] * tsrc[9];
    dst->f[3][3]-= tmp[8] * tsrc[9] + tmp[11] * tsrc[0] + tmp[5] * tsrc[8];

    /* calculate determinant */
    det = tsrc[0] * dst->f[0][0] + tsrc[1] * dst->f[0][1]
        + tsrc[2] * dst->f[0][2] + tsrc[3] * dst->f[0][3];

    /* calculate matrix inverse */
    det = 1.0f / det;

    for (j = 0; j < 4; j++) {
        for (i = 0; i < 4; i++) {
            dst->f[j][i] *= det;
        }
    }
}

ri_matrix_t *
ri_matrix_new()
{
    ri_matrix_t *p = NULL;

    p = (ri_matrix_t *)ri_mem_alloc(sizeof(ri_matrix_t));

    ri_matrix_identity(p);

    return p;
}

void
ri_matrix_free(ri_matrix_t *mat)
{
    ri_mem_free(mat);

}
