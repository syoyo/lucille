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


/*
 * Routines used only in matrix.c.
 */
static void adjoint(ri_matrix_t *out, ri_matrix_t *in);
static RtFloat det3x3(RtFloat a1, RtFloat a2, RtFloat a3,
		      RtFloat b1, RtFloat b2, RtFloat b3,
		      RtFloat c1, RtFloat c2, RtFloat c3);
static RtFloat det2x2(RtFloat a, RtFloat b, RtFloat c, RtFloat d);
 
#if 0
void ludcmp(ri_matrix_t *dst, int *index, RtFloat *d);
void lubksb(ri_matrix_t *dst, int *index, RtFloat *d);

/*
 * ludcmp() code is from Numerical recipie in C.
 */
void
ludcmp(ri_matrix_t *dst, RtInt *indx, RtFloat *d)
{
	const int n = 4;
	RtInt i, imax, j, k;
	RtFloat big, dum, sum, temp;
	RtFloat	v[4];

	*d = 1.0;

	for (i = 1; i <= n; i++) {
		big = 0.0f;
		for (j = 1; j <= n; j++) {
			if ((temp = fabs(dst->f[i][j])) > big) big = temp;
		}

		if (big == 0.0f) printf("singular matrix");

		v[i] = 1.0f / big;
	}

	for (j = 1; j <= n; j++) {
		for (i = 1; i <= n; i++) {
			sum = dst->f[i][j];
			for (k = 1; k < i; k++) sum -= dst->f[i][k] * dst->f[k][j];
			dst->f[i][j] = sum;
		}

		big = 0.0f;

		for (i = j; i <= n; i++) {
			sum = dst->f[i][j];
			for (k = 1; k < j; k++) {
				sum -= dst->f[i][k] * dst->f[k][j];
			}
			
			dst->f[i][j] = sum;

			if ( (dum = v[i] * fabs(sum)) >= big) {
				big = dum;
				imax = i;
			}
		}

		if (j != imax) {
			for (k = 1; k <= n; k++) {
				dum = dst->f[imax][k];
				dst->f[imax][k] = dst->f[j][k];
				dst->f[j][k] = dum;
			}

			*d = -(*d);
			v[imax] = v[j];
		}

		indx[j] = imax;
		if (dst->f[j][i] == 0.0f) dst->f[j][i] = RI_EPSILON;
		if (j != n) {
			dum = 1.0f / dst->f[j][i];
			for (i = j + 1; i <= n; i++) dst->f[i][j] *= dum;
		}
	}
}

void
lubksb(ri_matrix_t *dst, RtInt *indx, RtFloat b[])
{
	const int n = 4; 
	RtInt i, ii = 0, ip, j;
	RtFloat sum;

	for (i = 1; i <= n; i++) {
		ip = indx[i];
		sum = b[ip];
		b[ip] = b[i];
		if (ii) {
			for (j = ii; j <= i - 1; j++) sum -= dst->f[i][j] * b[j];
		} else if (sum) ii = i;
		b[i] = sum;
	}
	
	for (i = n; i >= 1; i--) {
		sum = b[i];
		for (j = i + 1; j <= n; j++) sum -= dst->f[i][j] * b[j];
		b[i] = sum / dst->f[i][i];
	}
}
#endif

/*
 * Codes from Graphics Gems
 */
void adjoint(ri_matrix_t *out, ri_matrix_t *in)
{
	RtFloat a1, a2, a3, a4, b1, b2, b3, b4;
	RtFloat c1, c2, c3, c4, d1, d2, d3, d4;
	
	a1 = in->f[0][0]; b1 = in->f[0][1];
	c1 = in->f[0][2]; d1 = in->f[0][3];

	a2 = in->f[1][0]; b2 = in->f[1][1];
	c2 = in->f[1][2]; d2 = in->f[1][3];

	a3 = in->f[2][0]; b3 = in->f[2][1];
	c3 = in->f[2][2]; d3 = in->f[2][3];

	a4 = in->f[3][0]; b4 = in->f[3][1];
	c4 = in->f[3][2]; d4 = in->f[3][3];

	out->f[0][0] =  det3x3(b2, b3, b4, c2, c3, c4, d2, d3, d4);
	out->f[1][0] = -det3x3(a2, a3, a4, c2, c3, c4, d2, d3, d4);
	out->f[2][0] =  det3x3(a2, a3, a4, b2, b3, b4, d2, d3, d4);
	out->f[3][0] = -det3x3(a2, a3, a4, b2, b3, b4, c2, c3, c4);

	out->f[0][1] = -det3x3(b1, b3, b4, c1, c3, c4, d1, d3, d4);
	out->f[1][1] =  det3x3(a1, a3, a4, c1, c3, c4, d1, d3, d4);
	out->f[2][1] = -det3x3(a1, a3, a4, b1, b3, b4, d1, d3, d4);
	out->f[3][1] =  det3x3(a1, a3, a4, b1, b3, b4, c1, c3, c4);

	out->f[0][2] =  det3x3(b1, b2, b4, c1, c2, c4, d1, d2, d4);
	out->f[1][2] = -det3x3(a1, a2, a4, c1, c2, c4, d1, d2, d4);
	out->f[2][2] =  det3x3(a1, a2, a4, b1, b2, b4, d1, d2, d4);
	out->f[3][2] = -det3x3(a1, a2, a4, b1, b2, b4, c1, c2, c4);

	out->f[0][3] = -det3x3(b1, b2, b3, c1, c2, c3, d1, d2, d3);
	out->f[1][3] =  det3x3(a1, a2, a3, c1, c2, c3, d1, d2, d3);
	out->f[2][3] = -det3x3(a1, a2, a3, b1, b2, b3, d1, d2, d3);
	out->f[3][3] =  det3x3(a1, a2, a3, b1, b2, b3, c1, c2, c3);
}

static RtFloat
det3x3(RtFloat a1, RtFloat a2, RtFloat a3,
       RtFloat b1, RtFloat b2, RtFloat b3,
       RtFloat c1, RtFloat c2, RtFloat c3)
{
	RtFloat ans;

	ans =  a1 * det2x2(b2, b3, c2, c3)
	     - b1 * det2x2(a2, a3, c2, c3)
	     + c1 * det2x2(a2, a3, b2, b3);

	return ans;
}

static RtFloat
det2x2(RtFloat a, RtFloat b, RtFloat c, RtFloat d)
{
	RtFloat ans;

	ans = a * d - b * c;

	return ans;
}
	
void
ri_matrix_identity(ri_matrix_t *dst)
{
	register int i, j;

	for (j = 0; j < 4; j++) {
		for (i = 0; i < 4; i++) {
			if (j == i) dst->f[j][i] = 1.0f;
			else	    dst->f[j][i] = 0.0f;
		}
	}
} 
			
void
ri_matrix_mul(ri_matrix_t *dst, const ri_matrix_t *a, const ri_matrix_t *b)
{
	/* TODO: implement AltiVec version. */
#ifdef WITH_ALTIVEC
	register int i, j;
	ri_matrix_t tb;
	ri_vector_t av, bv;

	ri_matrix_copy(&tb, b); 
	ri_matrix_transpose(&tb);

	for (j = 0; j < 4; j++) {
		av.v = a->v[j];
		for (i = 0; i < 4; i++) {
			bv.v = tb.v[i];
			dst->f[j][i] = ri_vector_dot4(&av, &bv);
		}
	}
#else
	register int i, j, k;

	for (j = 0; j < 4; j++) {
		for (i = 0; i < 4; i++) {
			dst->f[j][i] = 0.0f;
			for (k = 0; k < 4; k++) {
				dst->f[j][i] += a->f[j][k] * b->f[k][i];
			}
		}
	}
#endif
}

void
ri_matrix_translate(ri_matrix_t *dst, RtFloat x, RtFloat y, RtFloat z)
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
ri_matrix_rotate(ri_matrix_t *dst, RtFloat angle, RtFloat axisx, RtFloat axisy, RtFloat axisz)
{
	RtVector v;
	ri_vector_t axis;
	ri_quat_t   quat;
	ri_matrix_t tmp;
	ri_matrix_t rotmat;

	v[0] = axisx; v[1] = axisy; v[2] = axisz;
	
	ri_vector_set_rman(&axis, v);

	ri_quat_set(&quat, angle, &axis);

	ri_quat_mat(&rotmat, quat);

	/* dst = dst R 	*/
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

	/* dst = dst R 	*/
	ri_matrix_copy(&tmp, dst);
	ri_matrix_mul(dst, &rotmat, &tmp);
#endif
}

void
ri_matrix_scale(ri_matrix_t *dst, RtFloat sx, RtFloat sy, RtFloat sz)
{
	/* TODO: implement AltiVec version. */
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
ri_matrix_perspective(ri_matrix_t *dst, RtFloat d)
{
	int i;
	RtFloat f;

	// too small
	if (fabs(d) < RI_EPSILON) return;

	f = 1.0f / d;

	for (i = 0; i < 4; i++) {
		dst->f[i][3] += dst->f[i][2] * f;
		dst->f[i][2] = 0.0f;
	}
}

void
ri_matrix_transpose(ri_matrix_t *dst)
{
#ifdef WITH_ALTIVEC
	/*
	 * The code is descended from Apple Sample Code.
	 *
	 * http://developer.apple.com/hardware/ve/index.html
	 */

	vector float v0, v1, v2, v3;

	v0 = vec_mergeh(dst->v[0], dst->v[2]);
	v1 = vec_mergeh(dst->v[1], dst->v[3]);
	v2 = vec_mergel(dst->v[0], dst->v[2]);
	v3 = vec_mergel(dst->v[1], dst->v[3]);

	dst->v[0] = vec_mergeh(v0, v1);
	dst->v[1] = vec_mergel(v0, v1);
	dst->v[2] = vec_mergeh(v2, v3);
	dst->v[3] = vec_mergel(v2, v3);
#else
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
#endif
}

void
ri_matrix_print(const ri_matrix_t *mat)
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
ri_matrix_set(ri_matrix_t *dst, RtMatrix src)
{
	int i, j;

	for (j = 0; j < 4; j++) {
		for (i = 0; i < 4; i++) {
			dst->f[j][i] = src[j][i];
		}
	}
}

void
ri_matrix_copy(ri_matrix_t *dst, const ri_matrix_t *src)
{
#ifdef WITH_ALTIVEC
	int i;

	for (i = 0; i < 4; i++)
		dst->v[i] = src->v[i];
#else
	int i, j;

	for (j = 0; j < 4; j++) {
		for (i = 0; i < 4; i++) {
			dst->f[j][i] = src->f[j][i];
		}
	}
#endif
}

void 
ri_matrix_inverse(ri_matrix_t *dst)
{
	/*
	 * codes from intel web
	 * cramer's rule version
	 */
	int i, j;
	RtFloat tmp[12];	/* tmp array for pairs */
	RtFloat tsrc[16];	/* array of transpose source matrix */
	RtFloat det;		/* determinant */

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
