/*
 * $Id: shading.c,v 1.13 2004/06/13 06:44:51 syoyo Exp $
 *
 * shader.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <assert.h>

#include "vector.h"
#include "geom.h"
#include "ibl.h"
#include "log.h"
#include "material.h"
#include "memory.h"
#include "qmc.h"
#include "random.h"
#include "raytrace.h"
#include "reflection.h"
#include "shader.h"
#include "shading.h"
#include "timer.h"
#include "thread.h"

#ifndef M_PI
#define M_PI 3.141592
#endif

//static int ncachepoints = 0;

static void shader_shading(ri_vector_t *radiance,
			   const ri_vector_t *eye,
			   const ri_ray_t *ray,
			   ri_intersection_state_t *state);

void
ri_shade(ri_vector_t *radiance,
	 const ri_vector_t *eye,
	 const ri_ray_t *ray,
	 ri_intersection_state_t *state)
{
#if 0	// TODO
	if (state->geom->shader) {
		shader_shading(radiance, eye, ray, state);
	} else {
		/* Fixed shading pipeline. */
		fixed_shading(radiance, eye, ray, state);
	}
#endif
}

void
ri_shade_statistics()
{
	//printf("number of irradiance cache points = %d\n", ncachepoints);

}

#if 0
void ri_shade_indirect(ri_vector_t *power,	
	 	       ri_photonmap_t *photonmap,
	 	       ri_octree_t *irradcachetree,
	  	       const ri_ray_t *v,
	  	       int nsample,
		       ri_float_t gather_radius,
		       int gather_num,
		       int fixicache, int forcecalc)
{
	indirect_lighting(power, photonmap, irradcachetree, v,
		  	  nsample, gather_radius, gather_num,
		  	  fixicache, forcecalc);

}
#endif

/* --- private functions --- */


static void
shader_shading(ri_vector_t *radiance,
	       const ri_vector_t *eye,
	       const ri_ray_t *ray,
	       ri_intersection_state_t *state)
{
	ri_shader_t *shader;
	ri_output_t out;
	ri_status_t status;
	ri_vector_t lightpos;
	ri_vector_t Idir;

	shader = state->geom->shader;

	if (shader) {
	
		ri_timer_start(ri_render_get()->context->timer,
			       "Surface shader execution");	
		
		lightpos.f[0] =  1.0;
		lightpos.f[1] =  0.5;
		lightpos.f[2] =  1.0;
		lightpos.f[3] =  1.0;

		ri_vector_copy(&Idir, eye);
		ri_vector_normalize3(&Idir);

		status.thread_num = ray->thread_num;
		status.ray_depth  = 0;

		/* Setup predefined surface shader variables. */
		ri_vector_copy(&(status.input.Cs), &state->color);
		ri_vector_copy(&(status.input.P),  &state->P); 
		ri_vector_copy(&(status.input.N),  &state->Ng);
		if (state->inside) {	/* flip normal */
			ri_vector_neg(&(status.input.N));
		}
		ri_vector_copy(&(status.input.dPdu), &state->tangent);
		ri_vector_copy(&(status.input.dPdv), &state->binormal);
		ri_vector_copy(&(status.input.E),     eye);
		ri_vector_copy(&(status.input.I),    &Idir);
		ri_vector_copy(&(status.input.L),    &lightpos);
		status.input.s = state->u;
		status.input.t = state->v;

#if 0
		status.input.Os.f[0] = state->opacity;
		status.input.Os.f[1] = state->opacity;
		status.input.Os.f[2] = state->opacity;
		status.input.Os.f[3] = state->opacity;
#endif

		ri_vector_zero(&(out.Ci));
		ri_vector_zero(&(out.Oi));

		shader->shaderproc(&out, &status, shader->param);	

		ri_timer_end(ri_render_get()->context->timer,
			     "Surface shader execution");	

		ri_vector_copy(radiance, &out.Ci);
	} else {
		ri_vector_zero(radiance);
	}
}
