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

static ri_texture_t *create_sunsky_image(const ri_sunsky_t *sunsky);
static ri_sunsky_t  *setup_sunsky(RtInt n, RtToken tokens[], RtPointer params[]);

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
		light->type = LIGHTTYPE_DOME;
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
			orientation.f[2][2] = -orientation.f[2][2];
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
				ri_vector_set_rman(&v, *from);
				ri_vector_transform(&(light->pos), &v, &o2c);

			} else if (strcmp(tokens[i], "intensity") == 0) {

				intensity = (RtFloat *)params[i];
				light->intensity = *intensity;

			} else if (strcmp(tokens[i], "lightcolor") == 0) {

				lightcol = (RtColor *)params[i];
				
				ri_vector_set_rman(&(light->col), *lightcol);

			}
		}
	}

	ri_list_append(ri_render_get()->scene->light_list, light);

	return NULL; 
}

RtLightHandle
ri_api_area_light_source(RtToken name,
		         RtInt n, RtToken tokens[], RtPointer params[])
{
	int          i;
	ri_light_t  *light, *sunlight;
	ri_sunsky_t *sunsky;
	RtFloat       *valp;
	RtToken       *tokp;
	float          scale = 1.0;
	float          rgb[3];

	(void)name;

	light = ri_light_new();

	if (strcmp(name, "sunsky") == 0) {

		sunsky = setup_sunsky(n, tokens, params);
		light->sunsky = sunsky;	
		light->texture = create_sunsky_image(light->sunsky);
		light->type = LIGHTTYPE_SUNSKY;

		for (i = 0; i < n; i++) {
			if (strcmp(tokens[i], "sampling") == 0) {
				tokp = (RtToken *)params[i];

				if (strcmp(*tokp, "cosweight") == 0) {
					light->iblsampler = IBL_SAMPLING_COSWEIGHT;
				} else if (strcmp(*tokp, "importance") == 0) {
					light->iblsampler = IBL_SAMPLING_IMPORTANCE;
				} else if (strcmp(*tokp, "stratified") == 0) {
					light->iblsampler = IBL_SAMPLING_STRATIFIED;
				} else if (strcmp(*tokp, "structured") == 0) {
					light->iblsampler = IBL_SAMPLING_STRUCTURED;
				} else if (strcmp(*tokp, "bruteforce") == 0) {
					light->iblsampler = IBL_SAMPLING_BRUTEFORCE;
				}
			}
		}

		/* Set sunsky map to background map */
		ri_render_get()->background_map = light->texture;

		/*
		 * Create sunlight(as directional light)
		 */
		sunlight = ri_light_new();
		sunlight->type = LIGHTTYPE_DIRECTIONAL;
		sunlight->direction.f[0] = sunsky->sun_dir[0];
		sunlight->direction.f[1] = sunsky->sun_dir[2];	/* swap y and z */
		sunlight->direction.f[2] = sunsky->sun_dir[1];

		printf("sundir = %f, %f, %f\n",
			sunsky->sun_dir[0],
			sunsky->sun_dir[1],
			sunsky->sun_dir[2]);
		
		ri_sunsky_get_sunlight_rgb(rgb, sunsky);
		sunlight->col.f[0] = rgb[0];
		sunlight->col.f[1] = rgb[1];
		sunlight->col.f[2] = rgb[2];

		ri_list_append(ri_render_get()->scene->light_list, sunlight);

		ri_log(LOG_INFO, "Added sunsky");

	} else {

		for (i = 0; i < n; i++) {
			if (strcmp(tokens[i], "direction") == 0) {
				valp = (RtFloat *)params[i];
				light->direction.f[0] =	(float)(*valp++);
				light->direction.f[1] =	(float)(*valp++);
				light->direction.f[2] =	(float)(*valp);
				light->direction.f[3] =	1.0;

				ri_vector_normalize3(&(light->direction));
				printf("dir = (%f, %f, %f)\n", light->direction.f[0],
							       light->direction.f[1],
							       light->direction.f[2]);
		
				light->type = LIGHTTYPE_DIRECTIONAL;
			} else if (strcmp(tokens[i], "ibl") == 0) {
				tokp = (RtToken *)params[i];
				light->texture = ri_texture_load(*tokp);
				//printf("ibl = [ %s ] \n", *tokp);
				light->type = LIGHTTYPE_IBL;
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

		if (light->type == LIGHTTYPE_IBL) {
			ri_texture_scale(light->texture, scale);
		}
	}

	ri_list_append(ri_render_get()->scene->light_list, light);

	ri_render_get()->context->arealight_block = 1;

	return NULL; 
}

/* --- private functions --- */

/*
 * Create the sunsky object from parameters.
 */
static ri_sunsky_t *
setup_sunsky(RtInt n, RtToken tokens[], RtPointer params[])
{
	int i;
	ri_sunsky_t *sunsky;

	/*
	 * Default parameter.
	 *
	 *   Jan 20, 10:30
	 *   Tokyo, Japan.
	 * 
	 *   julian day        = 20( Jan 20 )
	 *   standard meridian = 135( = timezone 9 )
	 *   time of day       = 10.5 (10:30)
	 *   latitude          = 35.39
	 *   longitude         = 139.44
	 *
	 */ 
	float    time_of_day = 10.5;
	float    turbidity = 2.0;
	float    latitude  = 35.39; // in degree
	float    longitude = 139.44; //  in degree
	int      julian_day = 20;
	float    standard_meridian = 135.0 / 15;        // in degree. sm = timezone * 15. sm may be computed by rint(longitude/15) ?

	sunsky = ri_sunsky_new();

	for (i = 0; i < n; i++) {
		if (strcmp(tokens[i], "latitude") == 0) {
			latitude = *(RtFloat *)params[i];
		} else if (strcmp(tokens[i], "longitude") == 0) {
			longitude = *(RtFloat *)params[i];
		} else if (strcmp(tokens[i], "standard_meridian") == 0) {
			standard_meridian = *(RtFloat *)params[i];
		} else if (strcmp(tokens[i], "julian_day") == 0) {
			julian_day = *(RtInt *)params[i];
		} else if (strcmp(tokens[i], "time_of_day") == 0) {
			time_of_day = *(RtFloat *)params[i];
		} else if (strcmp(tokens[i], "turbidity") == 0) {
			turbidity = *(RtFloat *)params[i];
		}
	}

	ri_log(LOG_INFO, "[sunsky] latitude  = %f", latitude);
	ri_log(LOG_INFO, "[sunsky] longitude = %f", longitude);
	ri_log(LOG_INFO, "[sunsky] sm        = %f", standard_meridian);
	ri_log(LOG_INFO, "[sunsky] day       = %d", julian_day);
	ri_log(LOG_INFO, "[sunsky] timeofday = %f", time_of_day);
	ri_log(LOG_INFO, "[sunsky] turbidity = %f", turbidity);

	ri_sunsky_init(sunsky, latitude, longitude, standard_meridian,
		julian_day, time_of_day, turbidity, 0);


	return sunsky;
}

/*
 * TODO: move this function into texture.c or sunsky.c
 */
static ri_texture_t *
create_sunsky_image(const ri_sunsky_t *sunsky)
{
	const int size = 512;

	ri_texture_t *texture;
	int i, j;
	float u, v, r;
	float theta, phi;
	float rgb[3];
	float dir[3];

	/*
	 * Precompute sunsky image(in angular map)
	 */
	texture = ri_mem_alloc(sizeof(ri_texture_t));
	texture->width  = size;
	texture->height = size;
	texture->data = ri_mem_alloc(sizeof(float) * size * size * 4);

	for (j = 0; j < size; j++) {
		for (i = 0; i < size; i++) {
			u = (size / 2.0 - i) / (size / 2.0);
			v = (j - size / 2.0) / (size / 2.0);
			r = sqrt(u * u + v * v);

			if (r > 1.0) {
				texture->data[4 * (j * size + i) + 0] = 0.0;
				texture->data[4 * (j * size + i) + 1] = 0.0;
				texture->data[4 * (j * size + i) + 2] = 0.0;
			} else {
				theta = atan2(v, u);
				phi   = M_PI * r;

				/* quote from www.debevec.org/Probes,
				 *   The unit vector pointing in the correspon-
				 *   ding direction is obtained by rotating 
				 *   (0,0,-1) by phi degrees around the y (up)
				 *   axis and then theta degrees around the 
				 *   -z (forward) axis.
				 * quote end.
				 *
				 * Thus, the meaning of theta and phi is
				 * exchanged in the angular map case,
				 * in contrast to usual spherical mapping case.
				 */
				dir[0] = -sin(phi) * cos(theta);
				dir[1] = -sin(phi) * sin(theta);
				dir[2] = -cos(phi);

				ri_sunsky_get_sky_rgb(rgb, sunsky, dir);

				// Question: Should I consider differential solid angle?
				texture->data[4 * (j * size + i) + 0] = rgb[0];
				texture->data[4 * (j * size + i) + 1] = rgb[1];
				texture->data[4 * (j * size + i) + 2] = rgb[2];

			}
		}
	}

	return texture;

}
