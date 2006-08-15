#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>

#include "clipper.h"

// -------------------------------------------
// Clipping Engine
// -------------------------------------------

// -------------------------------------------
// compute intersection point
// ---------------------------------------muda
void intersect(vertex_t *i, vertex_t *s, vertex_t *p, plane_t *b)
{
	int j;

	ri_vector_t v;
	double vdotn;
	double sdotn;
	double d;

	double t;


	// calc direction
	ri_vector_sub(&v, &p->pos, &s->pos);	

	vdotn = v.e[0] * b->normal.e[0]
	      + v.e[1] * b->normal.e[1]
	      + v.e[2] * b->normal.e[2];
	if (fabs(vdotn) < 0.00001) {
		fprintf(stderr, "[clipper] warning: vdotn is too small!.\n");
		fprintf(stderr, "          clipping plane and line edge may be nearly parallel.\n");
		printf("  vdotn = %f\n", vdotn);

		t = 0.0;

	} else { 

		// d = plane equation
		d = -(  b->pos.e[0] * b->normal.e[0]
		      + b->pos.e[1] * b->normal.e[1]
		      + b->pos.e[2] * b->normal.e[2]);

		sdotn = s->pos.e[0] * s->normal.e[0]
		      + s->pos.e[1] * s->normal.e[1]
		      + s->pos.e[2] * s->normal.e[2];
		
		// t = ray parameter of intersection point.
		t = -(sdotn + d) / vdotn;

	}

	assert(t >= 0.0);
	assert(t <= 1.0);

	for (j = 0; j < 3; j++) {
		i->pos.e[j] = s->pos.e[j] + t * v.e[j];

		// Interpolate vertex parameters

		i->normal.e[j] = (1.0 - t) * s->normal.e[j] + t * p->normal.e[j];
		i->tangent.e[j] = (1.0 - t) * s->tangent.e[j] + t * p->tangent.e[j];
		i->binormal.e[j] = (1.0 - t) * s->binormal.e[j] + t * p->binormal.e[j];
		i->color.e[j] = (1.0 - t) * s->color.e[j] + t * p->color.e[j];
		i->opacity.e[j] = (1.0 - t) * s->opacity.e[j] + t * p->opacity.e[j];
	}

	i->u = (1.0 - t) * s->u + t * p->u;
	i->v = (1.0 - t) * s->v + t * p->v;
}

// ------------------------------------------------------------------
// check whether the vertex is inside or not against plane boundary
// --------------------------------------------------------------muda
int inside(vertex_t *p, plane_t *b)
{
	ri_vector_t pb;
	float d;

	ri_vector_sub(&pb, &p->pos, &b->pos);

	// if ((p - b.pos) dot b.normal) >= 0, then p is inside
	d = ri_vector_dot3(&pb, &b->normal);

	if (d >= 0) return 1;
	
	return 0;		// not inside
}

// ------------------------------------------------------------------
// add vertex to output.
// --------------------------------------------------------------muda
void output(vertex_t *p, int *outLen, vertex_t *out)
{
	out[(*outLen)] = *p;

	// increment index
	(*outLen)++;
}

/*
 *                       clip plne 
 *                           |
 *                (inner) -  |-> + (inner) 
 *                           |
 *          s                |                  p
 *          *----------------*----------------->*
 *                           i(clipped vertex) 
 *                           |
 *                           |
 *                           |
 */
void clip(vertex_t *in, int inLen,
	  vertex_t *outInner, int *outInnerLen,
	  vertex_t *outOuter, int *outOuterLen,
	  plane_t *clipPlane)
{
	int j;
	vertex_t *s, *p;
	vertex_t newv;

	*outInnerLen = 0;
	*outOuterLen = 0;

	s = &in[inLen - 1];	// start with last vertex.

	for (j = 0; j < inLen; j++) {
		p = &in[j];	// current vertex

		if (inside(p, clipPlane)) {
			if (inside(s, clipPlane)) {

				// add to inner
				output(p, outInnerLen, outInner);

			} else {
				intersect(&newv, s, p, clipPlane);

				// add to inner
				output(&newv, outInnerLen, outInner);
				output(p, outInnerLen, outInner);

				// add to outer
				output(&newv, outOuterLen, outOuter);
			}
		} else {
			if (inside(s, clipPlane)) {
				intersect(&newv, s, p, clipPlane);

				// add to inner
				output(&newv, outInnerLen, outInner);

				// add to outer
				output(&newv, outOuterLen, outOuter);
				output(p, outOuterLen, outOuter);
			} else {

				// add to outer
				output(p, outOuterLen, outOuter);
			}
		}

		s = p;
	}
}

