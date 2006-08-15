/*
 * Quadric handling routine
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "vector.h"
#include "apitable.h"
#include "attribute.h"
#include "geom.h"
#include "log.h"
#include "matrix.h"
#include "memory.h"
#include "render.h"

void
ri_api_sphere(RtFloat radius, RtFloat zmin, RtFloat zmax, RtFloat tmax,
	      RtInt n, RtToken tokens[], RtPointer params[])
{
	const int       ndiv = 16;	/* TODO: Adaptive tessalation. */
	const double    deg2rad = 3.14159265 / 180.0;
	int             u, v;	
	int             offset;
	unsigned int    idx;
	unsigned int    npoints;
	unsigned int   *indices;
	unsigned int    nindices;
	double          ua, va;	
	double          phimin, phimax;
	ri_matrix_t    *m;
	ri_matrix_t     itm;
	ri_vector_t     pos;
	ri_vector_t    *positions;
	ri_vector_t    *normals;
	ri_vector_t    *tangents;
	ri_vector_t    *binormals;
	ri_geom_t      *geom;
	ri_attribute_t *attr;

	(void)n;
	(void)tokens;
	(void)params;

	assert(ndiv >= 3);

	/*
	 * Generate ndiv * ndiv tesserated triangle sphere.
	 * TODO: Adaptive tesseration.
	 * TODO: Twi-Sided.
	 */ 

	geom = ri_geom_new();
	npoints = ndiv * (ndiv - 1) + 2; /* +2 for north and south pole. */

	positions = (ri_vector_t *)ri_mem_alloc(sizeof(ri_vector_t) * npoints);
	normals = (ri_vector_t *)ri_mem_alloc(sizeof(ri_vector_t) * npoints);
	tangents = (ri_vector_t *)ri_mem_alloc(sizeof(ri_vector_t) * npoints);
	binormals = (ri_vector_t *)ri_mem_alloc(sizeof(ri_vector_t) * npoints);

	nindices = ndiv * 3 * 2 + ndiv * (ndiv - 2) * 3 * 2;
	indices = (unsigned int *)ri_mem_alloc(sizeof(unsigned int) * nindices);

	m = (ri_matrix_t *)ri_stack_get(ri_render_get()->context->trans_stack);

	ri_matrix_copy(&itm, m);
	itm.e[0][3] = 0.0;
	itm.e[1][3] = 0.0;
	itm.e[2][3] = 0.0;
	itm.e[3][0] = 0.0;
	itm.e[3][1] = 0.0;
	itm.e[3][2] = 0.0;
	itm.e[3][3] = 1.0;

	ri_matrix_inverse(&itm);
	ri_matrix_transpose(&itm);

	if (zmin > -radius) {
		phimin = asin(zmin / radius);
	} else {
		phimin = -90.0 * deg2rad;
	}

	if (zmax < radius) {
		phimax = asin(zmax / radius);
	} else {
		phimax = 90.0 * deg2rad;
	}

	/*
	 * Generate vertices.
	 */

	/* bottom(south pole) vertex */
	pos.e[0] =  0.0;
	pos.e[1] =  0.0;
	pos.e[2] = -radius;
	pos.e[3] =  1.0;
	ri_vector_transform(&positions[0], &pos, m);
	ri_vector_transform(&normals[0], &pos, &itm);
	ri_vector_normalize(&normals[0]);
	pos.e[0] = -1.0;
	pos.e[1] =  0.0;
	pos.e[2] =  0.0;
	pos.e[3] =  1.0;
	ri_vector_transform(&tangents[0], &pos, &itm);
	ri_vector_normalize(&tangents[0]);
	pos.e[0] =  0.0;
	pos.e[1] =  1.0;
	pos.e[2] =  0.0;
	pos.e[3] =  1.0;
	ri_vector_transform(&binormals[0], &pos, &itm);
	ri_vector_normalize(&binormals[0]);

	for (v = 0; v < ndiv - 1; v++) { 
		va = phimin + ((phimax - phimin) * (double)(v + 1)) /
		     (double)ndiv;
		for (u = 0; u < ndiv; u++) { 
			ua = deg2rad * tmax * (double)u / (double)ndiv;

			idx = v * ndiv + u + 1;

			pos.e[0] = radius * cos(ua) * cos(va);
			pos.e[1] = radius * sin(ua) * cos(va);
			pos.e[2] = radius * sin(va);
			pos.e[3] = 1.0;
			ri_vector_transform(&positions[idx], &pos, m);
			ri_vector_transform(&normals[idx], &pos, &itm);
			ri_vector_normalize(&normals[idx]);

			pos.e[0] = -sin(ua) * cos(va);
			pos.e[1] =  cos(ua) * cos(va);
			pos.e[2] =  0.0;
			pos.e[3] =  1.0;
			ri_vector_transform(&tangents[idx], &pos, &itm);
			ri_vector_normalize(&tangents[idx]);

			pos.e[0] = -cos(ua) * sin(va);
			pos.e[1] = -sin(ua) * cos(va);
			pos.e[2] =  cos(va);
			pos.e[3] =  1.0;
			ri_vector_transform(&binormals[idx], &pos, &itm);
			ri_vector_normalize(&binormals[idx]);
		}
	}

	/* top(north pole) vertex. */
	pos.e[0] = 0.0;
	pos.e[1] = 0.0;
	pos.e[2] = radius;
	pos.e[3] = 1.0;
	ri_vector_transform(&positions[ndiv * (ndiv - 1) + 1], &pos, m);
	ri_vector_transform(&normals[ndiv * (ndiv - 1) + 1], &pos, &itm);
	ri_vector_normalize(&normals[ndiv * (ndiv - 1) + 1]);
	pos.e[0] = -1.0;
	pos.e[1] =  0.0;
	pos.e[2] =  0.0;
	pos.e[3] =  1.0;
	ri_vector_transform(&tangents[ndiv * (ndiv - 1) + 1], &pos, &itm);
	ri_vector_normalize(&tangents[ndiv * (ndiv - 1) + 1]);
	pos.e[0] =  0.0;
	pos.e[1] = -1.0;
	pos.e[2] =  0.0;
	pos.e[3] =  1.0;
	ri_vector_transform(&binormals[ndiv * (ndiv - 1) + 1], &pos, &itm);
	ri_vector_normalize(&binormals[ndiv * (ndiv - 1) + 1]);

	/*
	 * Generate indices.
	 */

	/* make indices for bottom band. */
	for (u = 0; u < ndiv; u++) { 
		indices[3 * u + 0] = 0;
		if (u == ndiv - 1) {
			indices[3 * u + 1] = 1;
		} else {
			indices[3 * u + 1] = u + 2;
		}
		indices[3 * u + 2] = u + 1;
	}

	/* make indices for inner band. */
	for (v = 1; v < ndiv - 1; v++) { 
		for (u = 0; u < ndiv; u++) { 
			offset = ndiv * 3 + 6 * ((v-1) * ndiv + u);
			if (u == ndiv - 1) {
				indices[offset + 0] = (v-1) * ndiv + u + 1;
				indices[offset + 1] = v * ndiv + 1;
				indices[offset + 2] = v * ndiv + u + 1;

				indices[offset + 3] = (v-1) * ndiv + 1;
				indices[offset + 4] = v * ndiv + 1;
				indices[offset + 5] = (v-1) * ndiv + u + 1;
			} else {
				indices[offset + 0] = (v-1) * ndiv + u + 1;
				indices[offset + 1] = v * ndiv + u + 2;
				indices[offset + 2] = v * ndiv + u + 1;

				indices[offset + 3] = (v-1) * ndiv + u + 2;
				indices[offset + 4] = v * ndiv + u + 2;
				indices[offset + 5] = (v-1) * ndiv + u + 1;
			}
		}
	}

	/* make indices for top band. */
	for (u = 0; u < ndiv; u++) { 
		offset = ndiv + (ndiv * (ndiv - 2) * 2) + u;
		indices[3 * offset + 0] = ndiv * (ndiv - 2) + u + 1;
		if (u == ndiv - 1) {
			indices[3 * offset + 1] = ndiv * (ndiv - 2) + 1;
		} else {
			indices[3 * offset + 1] = ndiv * (ndiv - 2) + u + 2;
		}
		indices[3 * offset + 2] = ndiv * (ndiv - 1) + 1;
	}

	ri_geom_add_positions(geom, npoints,  positions);
	ri_geom_add_normals  (geom, npoints,  normals);
	ri_geom_add_tangents (geom, npoints,  tangents);
	ri_geom_add_binormals(geom, npoints,  binormals);
	ri_geom_add_indices  (geom, nindices, indices);

	/* hack */
	geom->kd = 0.75;
	geom->ks = 0.0;

	attr = (ri_attribute_t *)ri_stack_get(ri_render_get()->context->attr_stack);

	if (attr->surface) {
		geom->shadername = strdup(attr->surface);
	}

 	if (attr->shader) {
		geom->shader     = ri_shader_dup(attr->shader);
	}
	
	if (attr->material) {
		if (!geom->material) geom->material = ri_material_new();
		ri_material_copy(geom->material, attr->material);
	}

	geom->two_side = 0;

	ri_render_add_geom(ri_render_get(), geom);

	ri_mem_free(positions);
	ri_mem_free(normals);
	ri_mem_free(tangents);
	ri_mem_free(binormals);
	ri_mem_free(indices);
}


