#ifndef CLIPPER_H
#define CLIPPER_H

#include "vector.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _vertex_t
{
	ri_vector_t pos;
	ri_vector_t normal;
	ri_vector_t tangent;
	ri_vector_t binormal;

	ri_vector_t color;	// vertex color
	ri_vector_t opacity;	// vertex color opacity

	float u, v;		// texture UV coord.
} vertex_t;

typedef struct _plane_t
{
	ri_vector_t pos;
	ri_vector_t normal;
} plane_t;


/*
 * input triangles is splitted against clip plane,
 * then clipped triangles are distributed *outInner and *outOuter
 * according to its position in the space.
 */
extern void clip(vertex_t *in, int inLen, /* input */
		 vertex_t *outInner, int *outInnerLen,
		 vertex_t *outOuter, int *outOuterLen,
		 plane_t *clipPlane);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif
