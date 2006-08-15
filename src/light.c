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

static void calc_normal(ri_vector_t *n,
			const ri_vector_t *v0,
			const ri_vector_t *v1,
			const ri_vector_t *v2);

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
		orientation.e[2][2] = -orientation.e[2][2];
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

	ri_vector_set(&v, from);

	/*
	 * set default value
	 */
	ri_vector_transform(&(light->pos), &v, &o2c);

	light->col.e[0] = 1.0;
	light->col.e[1] = 1.0;
	light->col.e[2] = 1.0;
	light->col.e[3] = 1.0;

	light->intensity = 1.0;

	light->directional    = 0;
	light->direction.e[0] = 0.0;
	light->direction.e[1] = 0.0;
	light->direction.e[2] = 1.0;
	light->texture = NULL;
	light->ibl = 0;
	light->iblsampler = IBL_SAMPLING_COSWEIGHT;

	light->sisfile = NULL;

	light->geom = NULL;
	light->domelight = 0;

	return light;
}

void
ri_light_free(ri_light_t *light)
{
	ri_mem_free(light->sisfile);
	ri_geom_free(light->geom);
	ri_mem_free(light);
}

void
ri_light_attach_geom(ri_light_t *light, ri_geom_t *geom)
{
	light->geom = geom;
}

void
ri_light_sample_pos_and_normal(ri_light_t *light,
		               ri_vector_t *pos,
		               ri_vector_t *normal)
{
	int i;
	double s, t;
	ri_vector_t *v0, *v1, *v2;

	if (light->geom == NULL) return;

	/* pick random triangle index */
	i = (int)(randomMT() * (light->geom->nindices / 3));

	v0 = &(light->geom->positions[light->geom->indices[3 * i + 0]]);
	v1 = &(light->geom->positions[light->geom->indices[3 * i + 1]]);
	v2 = &(light->geom->positions[light->geom->indices[3 * i + 2]]);

	s = sqrt(randomMT());
	t = randomMT();

	/*
	 * pos = v0(1.0 - s) + v1(s - t * s) + v2 * s * t
	 */
	pos->e[0] = (float)(v0->e[0]*(1.0-s) + v1->e[0]*(s-t*s) + v2->e[0]*s*t);
	pos->e[1] = (float)(v0->e[1]*(1.0-s) + v1->e[1]*(s-t*s) + v2->e[1]*s*t);
	pos->e[2] = (float)(v0->e[2]*(1.0-s) + v1->e[2]*(s-t*s) + v2->e[2]*s*t);

	if (light->directional) {
		ri_vector_copy(normal, &(light->direction));
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
	ri_vector_t *v0, *v1, *v2;

	if (light->geom == NULL) return;

	/* Assign larger d for sampling triangle index and
	 * smaller d for sampling a position on the triangle,
	 * which may improve sampling uniformity.
	 */

	/* Pick triangle index to sample. */
	r = generalized_scrambled_halton(i, 0, d + 2, perm);
	j = (int)(r * (light->geom->nindices / 3));

	v0 = &(light->geom->positions[light->geom->indices[3 * j + 0]]);
	v1 = &(light->geom->positions[light->geom->indices[3 * j + 1]]);
	v2 = &(light->geom->positions[light->geom->indices[3 * j + 2]]);

	r = generalized_scrambled_halton(i, 0, d, perm);
	s = sqrt(r);
	t = generalized_scrambled_halton(i, 0, d + 1, perm);

	/*
	 * pos = v0(1.0 - s) + v1(s - t * s) + v2 * s * t
	 */
	pos->e[0] = (float)(v0->e[0]*(1.0-s) + v1->e[0]*(s-t*s) + v2->e[0]*s*t);
	pos->e[1] = (float)(v0->e[1]*(1.0-s) + v1->e[1]*(s-t*s) + v2->e[1]*s*t);
	pos->e[2] = (float)(v0->e[2]*(1.0-s) + v1->e[2]*(s-t*s) + v2->e[2]*s*t);

	if (light->directional) {
		ri_vector_copy(normal, &(light->direction));
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
ri_light_sample_pos_and_dir_qmc(ri_light_t *light,
			        ri_vector_t *pos, ri_vector_t *dir,
				int d, int i, int **perm)
{
	ri_vector_t n;

	if (light->geom == NULL) return;

	ri_light_sample_pos_and_normal_qmc(light, pos, &n, d, i, perm);

	ri_random_vector_cosweight(dir, &n);
}


RtLightHandle
ri_api_light_source(RtToken name, RtInt n,
		    RtToken tokens[], RtPointer params[])
{
	int i;
	int rh;
	ri_light_t  *light;
	ri_vector_t  v;
	ri_matrix_t *m;
	ri_matrix_t  om;
	ri_matrix_t  c2w;
	ri_matrix_t  o2c;
	ri_matrix_t  orientation;
	RtPoint *from;
	RtFloat *intensity;
	RtColor *lightcol;

	light = ri_light_new();

	if (strcmp(name, "domelight") == 0) {
		light->domelight = 1;
	}

	if (n != 0) {
		if (strcmp(ri_render_get()->context->option->orientation,
			   RI_RH) == 0) {
			rh = 1;	
		} else {
			rh = 0;
		}

		ri_matrix_identity(&orientation);
		if (rh) {
			orientation.e[2][2] = -orientation.e[2][2];
		}

		/* get transformation matrix */
		m = (ri_matrix_t *)ri_stack_get(ri_render_get()->context->trans_stack);

		/* om = orientation . modelview */
		ri_matrix_mul(&om, m, &orientation);

		/* Camera to world */
		ri_matrix_copy(&c2w,
			       &(ri_render_get()->context->world_to_camera));
		ri_matrix_inverse(&c2w);

		/* Object to camera */
		ri_matrix_mul(&o2c, &c2w, &om); 

		for (i = 0; i < n; i++) {
			if (strcmp(tokens[i], "from") == 0) {

				from = (RtPoint *)params[i];
				ri_vector_set(&v, *from);
				ri_vector_transform(&(light->pos), &v, &o2c);

			} else if (strcmp(tokens[i], "intensity") == 0) {

				intensity = (RtFloat *)params[i];
				light->intensity = *intensity;

			} else if (strcmp(tokens[i], "lightcolor") == 0) {

				lightcol = (RtColor *)params[i];
				
				ri_vector_set(&(light->col), *lightcol);

			}
		}
	}

	ri_list_append(ri_render_get()->lightlist, light);

	return NULL; 
}

RtLightHandle
ri_api_area_light_source(RtToken name,
		         RtInt n, RtToken tokens[], RtPointer params[])
{
	int          i;
	ri_light_t  *light;
	RtFloat       *valp;
	RtToken       *tokp;
	float          scale = 1.0;

	(void)name;

	light = ri_light_new();

	for (i = 0; i < n; i++) {
		if (strcmp(tokens[i], "direction") == 0) {
			valp = (RtFloat *)params[i];
			light->direction.e[0] =	(float)(*valp++);
			light->direction.e[1] =	(float)(*valp++);
			light->direction.e[2] =	(float)(*valp);
			light->direction.e[3] =	1.0;

			ri_vector_normalize(&(light->direction));
			printf("dir = (%f, %f, %f)\n", light->direction.e[0],
			                               light->direction.e[1],
			                               light->direction.e[2]);
	
			light->directional = 1;
		} else if (strcmp(tokens[i], "ibl") == 0) {
			tokp = (RtToken *)params[i];
			light->texture = ri_texture_load(*tokp);
			//printf("ibl = [ %s ] \n", *tokp);
			light->ibl = 1;
		} else if (strcmp(tokens[i], "iblscale") == 0) {
			valp = (RtFloat *)params[i];
			scale = *valp;
			//printf("iblscale = [ %f ] \n", scale);
		} else if (strcmp(tokens[i], "sisfile") == 0) {
			tokp = (RtToken *)params[i];
			light->sisfile = strdup((*tokp));
			//printf("sisfile = [ %s ] \n", *tokp);
			light->iblsampler = IBL_SAMPLING_STRUCTURED;
		} else if (strcmp(tokens[i], "eihdrifile") == 0) {
			tokp = (RtToken *)params[i];
			light->eihdrifile = strdup((*tokp));
			//printf("sisfile = [ %s ] \n", *tokp);
			light->iblsampler = IBL_SAMPLING_STRUCTURED;
		} else if (strcmp(tokens[i], "sampling") == 0) {
			tokp = (RtToken *)params[i];
			if (strcmp(*tokp, "cosweight") == 0) {
				light->iblsampler = IBL_SAMPLING_COSWEIGHT;
			} else if (strcmp(*tokp, "importance") == 0) {
				light->iblsampler = IBL_SAMPLING_IMPORTANCE;
			} else if (strcmp(*tokp, "stratified") == 0) {
				light->iblsampler = IBL_SAMPLING_STRATIFIED;
			} else if (strcmp(*tokp, "structured") == 0) {
				printf("structured\n");
				light->iblsampler = IBL_SAMPLING_STRUCTURED;
			} else if (strcmp(*tokp, "bruteforce") == 0) {
				printf("bruteforce\n");
				light->iblsampler = IBL_SAMPLING_BRUTEFORCE;
			}

			//printf("sampling = [ %s ] \n", *tokp);
		}
	}

	if (light->ibl) {
		ri_texture_scale(light->texture, scale);
	}

	ri_list_append(ri_render_get()->lightlist, light);

	ri_render_get()->context->arealight_block = 1;


	return NULL; 
}

/* --- private functions --- */

static void
calc_normal(ri_vector_t *n,
	    const ri_vector_t *v0, const ri_vector_t *v1, const ri_vector_t *v2)
{
	ri_vector_t v01, v02;

#if 0
	printf("v0 = (%f, %f, %f), v1 = (%f, %f, %f), v2 = (%f, %f, %f)\n",
		v0.e[0], v0.e[1], v0.e[2],
		v1.e[0], v1.e[1], v1.e[2],
		v2.e[0], v2.e[1], v2.e[2]);
#endif

	ri_vector_sub(&v01, v1, v0);
	ri_vector_sub(&v02, v2, v0);
	ri_vector_cross3(n, &v01, &v02);
	ri_vector_normalize(n);
}

