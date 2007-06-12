/*
 * $Id: light.c,v 1.6 2004/06/13 06:44:51 syoyo Exp $
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <string.h>

#include "vector.h"
#include "memory.h"
#include "log.h"
#include "light.h"
#include "apitable.h"
#include "render.h"
#include "random.h"
#include "reflection.h"
#include "qmc.h"
#include "option.h"
#include "sunsky.h"

static void calc_normal(ri_vector_t *n,
			const ri_vector_t v0,
			const ri_vector_t v1,
			const ri_vector_t v2);

ri_light_t *
ri_light_new()
{
	int          rh;
	ri_vector_t  v;
	ri_matrix_t *m = NULL;
	ri_matrix_t  om;
	ri_matrix_t  c2w;		/* camera to world */
	ri_matrix_t  o2c;		/* object to world */
	ri_matrix_t  orientation;
	RtPoint      from;

	ri_light_t  *light = NULL;

	light = (ri_light_t *)ri_mem_alloc(sizeof(ri_light_t));

	if (strcmp(ri_render_get()->context->option->orientation,
		   RI_RH) == 0) {
		rh = 1;	
	} else {
		rh = 0;
	}

	ri_matrix_identity(&orientation);
	if (rh) {
		orientation.f[2][2] = -orientation.f[2][2];
	}

	/* Camera to world */
	ri_matrix_copy(&c2w, &(ri_render_get()->context->world_to_camera));
	ri_matrix_inverse(&c2w);

	/* get transformation matrix */
	m = (ri_matrix_t *)ri_stack_get(ri_render_get()->context->trans_stack);

	/* om = orientation . modelview */
	ri_matrix_mul(&om, m, &orientation);

	/* Object to camera */
	ri_matrix_mul(&o2c, &c2w, &om); 

	from[0] =  0.0;
	from[1] =  0.0;
	from[2] = -1.0;

	ri_vector_set_rman(&v, from);

	/*
	 * set default value
	 */
	ri_vector_transform(&(light->pos), &v, &o2c);

	light->col.f[0] = 1.0;
	light->col.f[1] = 1.0;
	light->col.f[2] = 1.0;
	light->col.f[3] = 1.0;

	light->intensity = 1.0;

	//light->directional    = 0;
	light->type           = LIGHTTYPE_NONE;
	light->direction.f[0] = 0.0;
	light->direction.f[1] = 0.0;
	light->direction.f[2] = 1.0;
	light->texture = NULL;
	light->iblsampler = IBL_SAMPLING_COSWEIGHT;

	light->sisfile = NULL;

	light->geom = NULL;

	return light;
}

void
ri_light_free(ri_light_t *light)
{
	ri_mem_free(light->sisfile);
	ri_geom_delete(light->geom);
	ri_mem_free(light);
}

void
ri_light_attach_geom(ri_light_t *light, ri_geom_t *geom)
{
	light->geom = geom;
}

void
ri_light_sample_pos_and_normal(
	ri_light_t  *light,
	ri_vector_t *pos,
	ri_vector_t *normal)
{
	int i;
	double s, t;
	ri_vector_t v0, v1, v2;

	if (light->geom == NULL) return;

	/* pick random triangle index */
	i = (int)(randomMT() * (light->geom->nindices / 3));

	v0 = light->geom->positions[light->geom->indices[3 * i + 0]];
	v1 = light->geom->positions[light->geom->indices[3 * i + 1]];
	v2 = light->geom->positions[light->geom->indices[3 * i + 2]];

	s = sqrt(randomMT());
	t = randomMT();

	/*
	 * pos = v0(1.0 - s) + v1(s - t * s) + v2 * s * t
	 */
	pos->f[0] = (float)(v0.f[0]*(1.0-s) + v1.f[0]*(s-t*s) + v2.f[0]*s*t);
	pos->f[1] = (float)(v0.f[1]*(1.0-s) + v1.f[1]*(s-t*s) + v2.f[1]*s*t);
	pos->f[2] = (float)(v0.f[2]*(1.0-s) + v1.f[2]*(s-t*s) + v2.f[2]*s*t);

	if (light->type == LIGHTTYPE_DIRECTIONAL) {
		ri_vector_copy(normal, &light->direction);
	} else {	
		calc_normal(normal, v0, v1, v2);
	}
}

void
ri_light_sample_pos_and_normal_qmc(ri_light_t *light,
		                   ri_vector_t *pos,
		                   ri_vector_t *normal,
				   int d, int i, int **perm)
{
	int j;
	double s, t;
	double r;
	ri_vector_t v0, v1, v2;

	if (light->geom == NULL) return;

	/* Assign larger d for sampling triangle index and
	 * smaller d for sampling a position on the triangle,
	 * which may improve sampling uniformity.
	 */

	/* Pick triangle index to sample. */
	r = generalized_scrambled_halton(i, 0, d + 2, perm);
	j = (int)(r * (light->geom->nindices / 3));

	v0 = light->geom->positions[light->geom->indices[3 * j + 0]];
	v1 = light->geom->positions[light->geom->indices[3 * j + 1]];
	v2 = light->geom->positions[light->geom->indices[3 * j + 2]];

	r = generalized_scrambled_halton(i, 0, d, perm);
	s = sqrt(r);
	t = generalized_scrambled_halton(i, 0, d + 1, perm);

	/*
	 * pos = v0(1.0 - s) + v1(s - t * s) + v2 * s * t
	 */
	pos->f[0] = (float)(v0.f[0]*(1.0-s) + v1.f[0]*(s-t*s) + v2.f[0]*s*t);
	pos->f[1] = (float)(v0.f[1]*(1.0-s) + v1.f[1]*(s-t*s) + v2.f[1]*s*t);
	pos->f[2] = (float)(v0.f[2]*(1.0-s) + v1.f[2]*(s-t*s) + v2.f[2]*s*t);

	if (light->type == LIGHTTYPE_DIRECTIONAL) {
		ri_vector_copy(normal, &light->direction);
	} else {	
		calc_normal(normal, v0, v1, v2);
	}
}


void
ri_light_sample_pos_and_dir(ri_light_t *light,
			    ri_vector_t *pos, ri_vector_t *dir)
{
	ri_vector_t n;

	if (light->geom == NULL) return;

	ri_light_sample_pos_and_normal(light, pos, &n);

	ri_random_vector_cosweight(dir, &n);
}

void
ri_light_sample_pos_and_dir_qmc(
	ri_light_t *light,
	ri_vector_t *pos,
	ri_vector_t *dir,
	int d,
	int i,
	int **perm)
{
	ri_vector_t n;

	if (light->geom == NULL) return;

	ri_light_sample_pos_and_normal_qmc(light, pos, &n, d, i, perm);

	ri_random_vector_cosweight(dir, &n);
}



/* --- private functions --- */

static void
calc_normal(ri_vector_t *n,
	    const ri_vector_t v0, const ri_vector_t v1, const ri_vector_t v2)
{
	ri_vector_t v01, v02;

	ri_vector_sub(&v01, &v1, &v0);
	ri_vector_sub(&v02, &v2, &v0);
	ri_vector_cross3(n, &v01, &v02);
	ri_vector_normalize3(n);
}

