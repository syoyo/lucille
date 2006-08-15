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
#include "bssrdf.h"
#include "geom.h"
#include "ibl.h"
#include "irradcache.h"
#include "log.h"
#include "material.h"
#include "memory.h"
#include "octree.h"
#include "photonmap.h"
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

static int ncachepoints = 0;

void direct_lighting(ri_vector_t *power,
		     ri_light_t *light,
		     const ri_ray_t *ray,
		     ri_surface_info_t *surfinfo,
		     const ri_vector_t *eye,
		     int nsample);

void indirect_lighting(ri_vector_t *power,	
		 	      ri_photonmap_t *photonmap,
		 	      ri_octree_t *irradcachetree,
		  	      const ri_ray_t *v,
		  	      int nsample,
			      float gather_radius,
			      int gather_num,
			      int fixicache, int forcecalc);

void caustics_lighting(ri_vector_t *power,
			      ri_photonmap_t *photonmap,
			      const ri_ray_t *v,
		  	      float gather_radius,
			      int gather_num);

static void finalgather(ri_vector_t *power, ri_hemisphere_t *hemi,
			int level, int nsamples,
			const ri_ray_t *v,
			ri_photonmap_t *photonmap,
			float gather_radius,
			int gather_num);
static void hemisample_ibl(ri_vector_t *power, ri_hemisphere_t *hemi,
	       		   int nsamples, const ri_ray_t *v, ri_light_t *light);

static void fixed_shading(ri_vector_t *radiance,
			  const ri_vector_t *eye,
			  ri_light_t *light,
			  const ri_ray_t *ray,
			  ri_surface_info_t *surfinfo);

static void shader_shading(ri_vector_t *radiance,
			   const ri_vector_t *eye,
			   ri_light_t *light,
			   const ri_ray_t *ray,
			   ri_surface_info_t *surfinfo);

#if 0
static void sis_ibl(ri_vector_t *power, ri_hemisphere_t *hemi,
	       	    ri_ray_t v, ri_light_t *light);
#endif

//static float sinc(float x);

void
ri_shade(ri_vector_t *radiance,
	 const ri_vector_t *eye,
	 ri_light_t *light,
	 const ri_ray_t *ray,
	 ri_surface_info_t *surfinfo)
{
	if (surfinfo->geom->shader) {
		shader_shading(radiance, eye, light, ray, surfinfo);
	} else {
		/* Fixed shading pipeline. */
		fixed_shading(radiance, eye, light, ray, surfinfo);
	}
}

void
ri_shade_statistics()
{
	//printf("number of irradiance cache points = %d\n", ncachepoints);

}

void ri_shade_indirect(ri_vector_t *power,	
	 	       ri_photonmap_t *photonmap,
	 	       ri_octree_t *irradcachetree,
	  	       const ri_ray_t *v,
	  	       int nsample,
		       float gather_radius,
		       int gather_num,
		       int fixicache, int forcecalc)
{
	indirect_lighting(power, photonmap, irradcachetree, v,
		  	  nsample, gather_radius, gather_num,
		  	  fixicache, forcecalc);

}

/* --- private functions --- */

void
direct_lighting(ri_vector_t *power, ri_light_t *light, const ri_ray_t *ray,
		ri_surface_info_t *surfinfo, const ri_vector_t *eye, int nsample)
{
	int         i;
	int         hit, lighthit;
	ri_vector_t lightpos;
	ri_vector_t lightnormal;
	ri_vector_t lightvec;
	ri_vector_t rad;
	ri_ray_t    r;
	float       dot;
	float       t;
	double      dpower[3];
	double      brdf;
	ri_surface_info_t sinfo, tmpinfo;
	int         count;
	int         use_photnmap = 0;
	ri_photonmap_option_t *pmapopt = NULL;
	static int              first = 1;
	static ri_hemisphere_t *hemi = NULL;

	if (first) {
		hemi = (ri_hemisphere_t *)ri_mem_alloc(sizeof(ri_hemisphere_t));
		first = 0;
	}

	dpower[0] = dpower[1] = dpower[2] = 0.0;
	ri_vector_copy(&(hemi->basis[0]), &surfinfo->tangent);
	ri_vector_copy(&(hemi->basis[1]), &surfinfo->binormal);
	ri_vector_copy(&(hemi->basis[2]), &surfinfo->normal);

	//ri_vector_copy(power, surfinfo->normal);
	//return;

	if (use_photnmap) {
		dot = -ri_vector_dot3(eye, &surfinfo->normal);
		if (dot <= 0.0) {
			ri_vector_zero(power);
			return;
		}

		ri_timer_start(ri_render_get()->context->timer,
			       "Photonmap | Radiance Estimate");

		ri_photonmap_estimate_irradiance(
			ri_render_get()->global_photonmap,
			power,
			&surfinfo->pos,
		        &surfinfo->normal,
		 	pmapopt->max_gather_radius,
			pmapopt->max_gather_photons);
		ri_timer_end(ri_render_get()->context->timer,
			     "Photonmap | Radiance Estimate");

		// assume diffuse
		ri_vector_scale(power, dot);
		return;
	}

	if (light->ibl) {	/* image based lighting */
		count = 0;

		switch (light->iblsampler) {
			case IBL_SAMPLING_COSWEIGHT:
				ri_ibl_sample_cosweight(&rad,
							&surfinfo->normal,
							nsample,
							ray,
							&surfinfo->pos,
							eye,
							light);
				break;

			case IBL_SAMPLING_IMPORTANCE:
				ri_ibl_sample_importance(&rad,
							 hemi,
							 nsample,
							 &surfinfo->pos,
							 eye,
							 light);
				break;
			case IBL_SAMPLING_STRATIFIED:
				ri_ibl_sample_stratified(&rad,
							 hemi,
							 nsample,
							 &surfinfo->pos,
							 eye,
							 light);
				break;
			case IBL_SAMPLING_STRUCTURED:
				ri_ibl_sample_structured(&rad,
							 hemi,
							 nsample,
							 &surfinfo->pos,
							 eye,
							 light);
				break;
			case IBL_SAMPLING_BRUTEFORCE:
				ri_ibl_sample_bruteforce(&rad,
							hemi,
							nsample,
							&surfinfo->pos,
							eye,
							light);
				break;
			default:
				printf("invalid IBL sampling method\n");
				return;
		}

		dpower[0] = rad.e[0];
		dpower[1] = rad.e[1];
		dpower[2] = rad.e[2];

	} else if (light->geom) {	/* area light	*/

		count = 0;
		for (i = 0; i < nsample; i++) {
			if (light->directional) {
				ri_vector_copy(&lightvec, &light->direction);
				ri_vector_neg(&lightvec);
			} else {
				ri_light_sample_pos_and_normal(light,
							       &lightpos,
							       &lightnormal);
				ri_vector_sub(&lightvec,
					      &lightpos, &surfinfo->pos);
				ri_vector_normalize(&lightvec);
			}

			ri_vector_copy(&(r.org), &surfinfo->pos);
			ri_vector_copy(&(r.dir), &lightvec);
			r.thread_num = ray->thread_num;

			/* occlusion test */
			hit = ri_raytrace(ri_render_get(), &r, &sinfo);
			if (hit) {

				t = r.isectt;

				/* trace against area light geometry */
				lighthit = ri_raytrace_geom(light->geom,
							    &r,
							    &tmpinfo);			

				if (lighthit) {
					if (r.isectt > t ||
					    r.isectt < 0.0) {
						/* there is a occluder
						 * between surface and area
						 * light.
						 */

					} else {
						/* currently, diffuse lighting only */
						dot = ri_vector_dot3(&r.dir,
								     &surfinfo->normal);
						if (dot > 0.0) {
							dpower[0] += light->intensity * light->col.e[0] * dot;
							dpower[1] += light->intensity * light->col.e[1] * dot;
							dpower[2] += light->intensity * light->col.e[2] * dot;
						}
					}
					
				}
			} else {
				/* currently, diffuse lighting only */
				dot = ri_vector_dot3(&r.dir, &surfinfo->normal);
				if (dot > 0.0) {
					dpower[0] += light->intensity * light->col.e[0] * dot;
					dpower[1] += light->intensity * light->col.e[1] * dot;
					dpower[2] += light->intensity * light->col.e[2] * dot;
				}
			}
		}

		dpower[0] /= (double)nsample;
		dpower[1] /= (double)nsample;
		dpower[2] /= (double)nsample;

		//printf("dpower = %f, %f, %f\n", dpower[0], dpower[1], dpower[2]);

	} else if (light->domelight) {		/* dome light	*/
		ri_domelight_sample(&rad,
				    hemi,
				    nsample,
				    ray,
				    &surfinfo->pos,
				    eye,
				    light);

		dpower[0] = rad.e[0] / M_PI;
		dpower[1] = rad.e[1] / M_PI;
		dpower[2] = rad.e[2] / M_PI;

	} else {		/* point light	*/

		//ri_vector_sub(&lightvec, &light->pos, &surfinfo->pos);
		ri_vector_copy(&lightvec, &light->pos);
		ri_vector_normalize(&lightvec);

		ri_vector_copy(&(r.org), &surfinfo->pos);
		ri_vector_copy(&(r.dir), &lightvec);

		/* occlusion test */
		hit = ri_raytrace(ri_render_get(), &r, &sinfo);
		if (!hit) {
			/* currently, diffuse lighting only */
			dot = ri_vector_dot3(&r.dir, &surfinfo->normal);
			if (dot > 0.0) {
#if 0
				brdf = ri_brdf_ward_anisotropic(
						eye,
						lightvec,
						surfinfo->normal,
						surfinfo->tangent,
						surfinfo->binormal,
						0.1, 0.33, 0.05, 0.16);

#endif
				brdf = 1.0;
				dpower[0] += light->intensity * light->col.e[0] * brdf * dot;
				dpower[1] += light->intensity * light->col.e[1] * brdf * dot;
				dpower[2] += light->intensity * light->col.e[2] * brdf * dot;
			}
		}

	}

	power->e[0] = (float)dpower[0];
	power->e[1] = (float)dpower[1];
	power->e[2] = (float)dpower[2];

}

void
indirect_lighting(ri_vector_t *power,	/* irradiance power */	
		  ri_photonmap_t *photonmap,
		  ri_octree_t *irradcachetree,
		  const ri_ray_t *v,
		  int nsample, float gather_radius, int gather_num,
		  int fixicache, int forcecalc)
{
	int nfound;
	int level;
	double dpower[3];
	double weight;
	double ri;
	float  dot;
	ri_vector_t irrad;
	ri_ray_t ray;
	ri_irradcache_t val;
	ri_option_t *opt;
	ri_photonmap_option_t *pmapopt;
	static int              first = 1;
	static ri_hemisphere_t *hemi = NULL;

	if (first) {
		hemi = (ri_hemisphere_t *)ri_mem_alloc(sizeof(ri_hemisphere_t));
		first = 0;
	}

	dpower[0] = dpower[1] = dpower[2] = 0.0;

	ri_vector_copy(&(ray.org), &v->org);
	ray.thread_num = v->thread_num;
	assert(ray.thread_num >= 0 && ray.thread_num < RI_MAX_THREADS);

	nfound = 0;

	ri_vector_zero(&irrad);

	opt = ri_render_get()->context->option;
	pmapopt = ri_photonmap_get_option();

	if (opt->enable_irradcache && !forcecalc) {

		/* First, check irradiance cache. */
		weight = ri_irradcache_find(irradcachetree, &v->org, &v->dir,
					    &irrad, &nfound);

		if (nfound) {
			/* cache found. Reuse cached irradiance. */
			ri_vector_scale(&irrad, 1.0f / (float)weight);
			ri_vector_copy(power, &irrad);
			return;
		}

		if (fixicache) {
			ri_log(LOG_WARN, "no icache found.");
			ri_vector_zero(power);
			return;
		}

	}

	/* primary final gathering */
	level = 1;
	finalgather(power, hemi, 1, nsample, v,
		    photonmap, gather_radius, gather_num);

#if 0
	ri = hemi->rtotal;
#else
	ri = hemi->rmean;
#endif

	if (opt->enable_irradcache) {
#if 1
		if (ri < 1.0e-6) {
			/* purely dark region.
			 * no sample ray hits anything.
			 * but irradiance cache requires dark color cache point.
			 */  
			fprintf(stderr, "dark point.\n");
			ri = opt->irradcache_max_radius;
		} else if (ri > 0.0) {
			//ri = (double)count / ri;
			//ri = (double)(hemi->ntheta * hemi->nphi) / ri;
			//printf("org ri = %f\n", ri);
		}
		ri_irradcache_calc_gradient(&(val.tg), &(val.rg), hemi);
		/* reduce radius if gradient is large */
		dot = ri_vector_dot3(&val.tg, &val.tg);
		if (dot * ri * ri > 1.0) {
			//fprintf(stderr, "\nreduce radius. at (%f, %f, %f). tg = (%f, %f, %f), ri = %f \n", v.org.e[0], v.org.e[1], v.org.e[2], val.tg.e[0], val.tg.e[1], val.tg.e[2], ri);
			ri = 1.0 / sqrt(dot);

			fprintf(stderr, "reduced radius. ri = %f \n", ri);
		}

		if (ri < 0.01) {
			printf("ri < 0.01\n");
			ri = 0.01;
			if (dot * ri * ri > 1.0) { /* cap gradient */
				//fprintf(stderr, "cap gradient. dot = %f\n", dot);
				dot = 1.0 / ri / sqrt(dot);

				ri_vector_scale(&(val.tg), dot);
			}

		}
		

		//if (ri < 0.01) ri = 0.01;
		if (ri > opt->irradcache_max_radius) {
			printf("ri = %f\n", ri);
			ri = opt->irradcache_max_radius;
		}
#endif

		ri_vector_copy(&(val.p), &v->org);
		ri_vector_copy(&(val.n), &v->dir);
		ri_vector_copy(&(val.e), power);
		val.r = (float)ri;

		if (!ri_irradcache_insert(irradcachetree, &val)) {
			// for test
			//power->e[0] = 100; power->e[1] = power->e[2] = 0.0;
		}
		power->e[0] = 100; power->e[1] = power->e[2] = 0.0;
		ncachepoints++;
	}	
}

void
caustics_lighting(ri_vector_t *power,	/* radiance */	
		  ri_photonmap_t *photonmap,
		  const ri_ray_t *v,
		  float gather_radius, int gather_num)
{
	/* directly visualize caustics photonmap using density estimation */

	ri_timer_start(ri_render_get()->context->timer,
		       "Photonmap | Caustics Radiance Estimate");
	ri_photonmap_estimate_irradiance(photonmap,
					 power,
					 &v->org, &v->dir,
					 gather_radius,
					 gather_num);
	ri_timer_end(ri_render_get()->context->timer,
		     "Photonmap | Caustics Radiance Estimate");


}

#if 0
void
generate_cache(ri_vector_t *rad, const ri_ray_t *ray, ri_light_t *light)
{
	int hit, lighthit;
	ri_surface_info_t surfinfo;
	ri_ray_t surfray;
	ri_photonmap_option_t *pmapopt;
	ri_option_t *opt;
	int nofixicache = 0;
	static ri_ray_t prev_hit;
	static int first = 1;
	int forcecalc = 0;

	if (first) {
		ri_vector_zero(&(prev_hit.org));
		ri_vector_zero(&(prev_hit.dir));
		first = 0;
	}


	/* First, check if hit light geometry */
	lighthit = ri_raytrace_geom(light->geom,
				    ray,
				    &surfinfo);			

	if (lighthit) return;

	hit = ri_raytrace(ri_render_get(), ray, &surfinfo);

	if (!hit) return;
	
	ri_vector_copy(&(surfray.dir), &surfinfo.normal);
	ri_vector_copy(&(surfray.org), &surfinfo.pos);

	pmapopt = ri_photonmap_get_option();
	opt = ri_render_get()->context->option;

	/* if geometry is not smooth(in screen space),
	 * force calculate irradiance cache. */
	//ri_vector_sub(&p, surfray.org, prev_hit.org);
	//l = ri_vector_length(p);
	//dot = ri_vector_dot3(surfray.dir, prev_hit.dir);
	//if (dot < 0.9) forcecalc = 1;

	indirect_lighting(rad,
			  ri_render_get()->global_photonmap,
			  ri_render_get()->irradiance_cache,
			  &surfray,
			  opt->nfinalgather_rays,
			  pmapopt->max_gather_radius,
			  pmapopt->max_gather_photons,
			  nofixicache,
			  forcecalc);

	ri_vector_copy(&(prev_hit.org), &surfray.org);
	ri_vector_copy(&(prev_hit.dir), &surfray.dir);
	
}
#endif

/* Final gathering for indirect lighting using global photonmap. */
void
finalgather(ri_vector_t *power, ri_hemisphere_t *hemi,
	    int level, int nsamples, const ri_ray_t *v,
	    ri_photonmap_t *photonmap, float gather_radius, int gather_num)
{
	int i, j, k;
	int hit;
	int nfound;
	int count = 0;
	float dot;
	double u[2];
	double dpower[3];
	double theta, phi;
	ri_vector_t irrad;
	ri_vector_t dir;
	ri_ray_t ray;
	ri_ray_t secondray;
	ri_surface_info_t surfinfo;
	ri_option_t *opt;
	ri_photonmap_option_t *pmapopt;
	ri_timer_t *timer;
	double     *samples = NULL;
	static int              first = 1;
	static ri_hemisphere_t *secondhemi = NULL;

	if (first) {
		secondhemi = (ri_hemisphere_t *)
				ri_mem_alloc(sizeof(ri_hemisphere_t));
		first = 0;
	}

	dpower[0] = dpower[1] = dpower[2] = 0.0;

	ri_vector_copy(&(ray.org), &v->org);
	ray.thread_num = v->thread_num;

	nfound = 0;

	ri_vector_zero(&irrad);

	opt = ri_render_get()->context->option;
	pmapopt = ri_photonmap_get_option();
	timer = ri_render_get()->context->timer;

	ri_ortho_basis(hemi->basis, &v->dir);

	if (opt->use_qmc) {	/* quasi-Monte Carlo sampling. */
                samples = (double *)ri_mem_alloc(sizeof(double) * nsamples * 2);

                for (i = 0; i < nsamples; i++) {
                        samples[2 * i + 0] = generalized_scrambled_hammersley(
                                                i, 0, nsamples, 1,
                                                ri_render_get()->perm_table);
                        samples[2 * i + 1] = generalized_scrambled_hammersley(
                                                i, 0, nsamples, 2,
                                                ri_render_get()->perm_table);

                        u[0] = generalized_scrambled_halton(
                                                v->i, 0, 3,
                                                ri_render_get()->perm_table);
                        u[1] = generalized_scrambled_halton(
                                                v->i, 0, 4,
                                                ri_render_get()->perm_table);

                        samples[2 * i + 0] = mod_1(u[0] + samples[2 * i + 0]);
                        samples[2 * i + 1] = mod_1(u[1] + samples[2 * i + 1]);
                }

		for (i = 0; i < nsamples; i++) {
			theta = sqrt(samples[2 * i + 0]);
			phi = 2.0 * M_PI * samples[2 * i + 1];

			dir.e[0] = (float)(cos(phi) * theta);
			dir.e[1] = (float)(sin(phi) * theta);
			dir.e[2] = (float)(sqrt(1.0 - theta * theta));

			for (k = 0; k < 3; k++) {
				ray.dir.e[k] =
					dir.e[0] * hemi->basis[0].e[k]
				      + dir.e[1] * hemi->basis[1].e[k]
				      + dir.e[2] * hemi->basis[2].e[k];
			}

			ri_vector_normalize(&(ray.dir));
			hit = ri_raytrace(ri_render_get(),
					  &ray, &surfinfo);

			if (hit) {

				/* if final gather ray hits very
				 * closely, do secondary final
				 * gathering.
				 */
				if (ray.isectt < 2.0 && level < 2) {
					ri_vector_copy(&secondray.org,
						       &surfinfo.pos);
					ri_vector_copy(&secondray.dir,
						       &surfinfo.normal);
					secondray.thread_num = ray.thread_num;
					assert(secondray.thread_num < RI_MAX_THREADS);

					finalgather(&irrad,
						     secondhemi,
						     level + 1,
						     nsamples,
						    &secondray,
						     photonmap,
						     gather_radius, 
						     gather_num);
				} else {
					ri_timer_start(
						timer,
					        "Photonmap | Final gather radiance estimate");

					if (pmapopt->precompute_irradiance) {
						ri_photonmap_estimate_precomputed_irradiance(
							photonmap,
							&irrad,
							&surfinfo.pos,
							&surfinfo.normal);
					} else {
						ri_photonmap_estimate_irradiance(
							photonmap,
							 &irrad,
							 &surfinfo.pos,
							 &surfinfo.normal,
							 gather_radius,
							 gather_num);
					}
					ri_timer_end(
						timer,
						"Photonmap | Final gather radiance estimate");
				}


				/* convert irradiance to radiance
				 * (diffuse reflection) */

				/* P=Irrad*diffuse_color*cos(-Rd, N)*/
				ri_vector_neg(&(ray.dir));
				dot = ri_vector_dot3(&ray.dir,
						     &surfinfo.normal);
				if (dot > 0.0) {
					ri_vector_mul(&irrad,
						      &irrad,
						      &surfinfo.color);
					ri_vector_scale(&irrad, dot);
					dpower[0] += irrad.e[0];
					dpower[1] += irrad.e[1];
					dpower[2] += irrad.e[2];
					count++;

					/* r = 1.0 / distance */
					//hemi->samples[i][j].r =
					//	1.0 / ray.isectt;

					//if (hemi->rmean > ray.isectt) {
					//	hemi->rmean = ray.isectt;
					//}
				} else {
					ri_vector_zero(&irrad);
					//hemi->samples[i][j].r = 0.0;
				}
					
			
				//ri_vector_copy(&(hemi->samples[i][j].L),
				//	       irrad);

			} else {
				//hemi->samples[i][j].r = 0.0;
				//ri_vector_zero(&(hemi->samples[i][j].L));
			}

			/* radius of irradiance cache value. */ 
			//hemi->rtotal += hemi->samples[i][j].r;
			hemi->rtotal = 0.0;
		}

		ri_mem_free(samples);

		power->e[0] = (float)(dpower[0] / (double)(nsamples));
		power->e[1] = (float)(dpower[1] / (double)(nsamples));
		power->e[2] = (float)(dpower[2] / (double)(nsamples));

	} else {		/* Monte Carlo sampling. */

		/* theta * phi = total samples.
		 * phi = 3 * theta.
		 */
		hemi->ntheta = (int)(nsamples / 3.0);
		hemi->ntheta = (int)sqrt((double)hemi->ntheta);
		if (hemi->ntheta < 1) hemi->ntheta = 1;
		if (hemi->ntheta > MAX_HEMISAMPLE) {
			hemi->ntheta = MAX_HEMISAMPLE;
		}
		hemi->nphi   = 3 * hemi->ntheta;

		/* calculate irradiance. */
		hemi->rtotal = 0.0;
		hemi->rmean = 1.0e+6;
		count = 0;

		for (j = 0; j < (int)hemi->nphi; j++) {
			for (i = 0; i < (int)hemi->ntheta; i++) {
				theta = sqrt(((double)i + randomMT()) /
					(double)hemi->ntheta);
				phi = 2.0 * M_PI * ((double)j + randomMT()) /
				      (double)hemi->nphi;		
				dir.e[0] = (float)(cos(phi) * theta);
				dir.e[1] = (float)(sin(phi) * theta);
				dir.e[2] = (float)(sqrt(1.0 - theta * theta));

				for (k = 0; k < 3; k++) {
					ray.dir.e[k] =
						dir.e[0] * hemi->basis[0].e[k]
					      + dir.e[1] * hemi->basis[1].e[k]
					      + dir.e[2] * hemi->basis[2].e[k];
				}

				ri_vector_normalize(&(ray.dir));

				hit = ri_raytrace(ri_render_get(),
						  &ray, &surfinfo);

				if (hit) {

					/* if final gather ray hits very
					 * closely, do secondary final
					 * gathering.
					 */
					if (ray.isectt < 2.0 && level < 2) {
						ri_vector_copy(&secondray.org,
							       &surfinfo.pos);
						ri_vector_copy(&secondray.dir,
							       &surfinfo.normal);

						secondray.thread_num =
							ray.thread_num;

						finalgather(&irrad,
							     secondhemi,
							     level + 1,
							     nsamples,
							     &secondray,
							     photonmap,
							     gather_radius, 
							     gather_num);
					} else {
						ri_timer_start(
							timer,
						        "Photonmap | Final gather radiance estimate");

						if (pmapopt->precompute_irradiance) {
							ri_photonmap_estimate_precomputed_irradiance(
								photonmap,
								&irrad,
								&surfinfo.pos,
								&surfinfo.normal);
						} else {
							ri_photonmap_estimate_irradiance(
								photonmap,
								 &irrad,
								 &surfinfo.pos,
								 &surfinfo.normal,
								 gather_radius,
								 gather_num);
						}
						ri_timer_end(
							timer,
							"Photonmap | Final gather radiance estimate");
					}


					/* convert irradiance to radiance
					 * (diffuse reflection) */

					/* P=Irrad*diffuse_color*cos(-Rd, N)*/
					ri_vector_neg(&(ray.dir));
					dot = ri_vector_dot3(&ray.dir,
							     &surfinfo.normal);
					if (dot > 0.0) {
						ri_vector_mul(&irrad,
							      &irrad,
							      &surfinfo.color);
						ri_vector_scale(&irrad, dot);
						dpower[0] += irrad.e[0];
						dpower[1] += irrad.e[1];
						dpower[2] += irrad.e[2];
						count++;

						/* r = 1.0 / distance */
						if (ray.isectt < 1.0-4) {
							hemi->samples[i][j].r =
								1.0;
						} else {
							hemi->samples[i][j].r =
								1.0 / ray.isectt;
						}

						if (hemi->rmean > ray.isectt) {
							hemi->rmean = ray.isectt;
						}
					} else {
						ri_vector_zero(&irrad);
						hemi->samples[i][j].r = 0.0;
					}
						
				
					ri_vector_copy(&(hemi->samples[i][j].L),
						       &irrad);

				} else {
					hemi->samples[i][j].r = 0.0;
					ri_vector_zero(&(hemi->samples[i][j].L));
				}

				/* radius of irradiance cache value. */ 
				hemi->rtotal += hemi->samples[i][j].r;
			}
		}

		hemi->rtotal = (double)count / hemi->rtotal;
		power->e[0] = (float)(dpower[0] / (double)(hemi->ntheta * hemi->nphi));
		power->e[1] = (float)(dpower[1] / (double)(hemi->ntheta * hemi->nphi));
		power->e[2] = (float)(dpower[2] / (double)(hemi->ntheta * hemi->nphi));

		if (hemi->rmean < 1.0e-6) hemi->rmean = 1.0e-6;
		if (hemi->rmean > 1.0e+6) hemi->rmean = 1.0e+6;
	}
}

void
hemisample_ibl(ri_vector_t *power, ri_hemisphere_t *hemi,
	       int nsamples, const ri_ray_t *v, ri_light_t *light)
{
	int i, j, k;
	int hit;
	int count;
	double dpower[3];
	double theta, phi;
	ri_vector_t rad;
	ri_vector_t dir;
	ri_ray_t ray;
	ri_surface_info_t surfinfo;
	static int first = 1;
	FILE *fp = NULL;

	if (first) {
		fp = fopen("hemi.dat", "w");
		if (!fp) exit(-1);

		fprintf(fp, "%f %f %f\n", v->dir.e[0], v->dir.e[1], v->dir.e[2]);
	}

	dpower[0] = dpower[1] = dpower[2] = 0.0;

	ri_vector_copy(&(ray.org), &v->org);

	ri_vector_zero(&rad);

	/* theta * phi = total samples.
	 * phi = 3 * theta.
	 */
	hemi->ntheta = (int)(nsamples / 3.0);
	hemi->ntheta = (int)sqrt((double)hemi->ntheta);
	if (hemi->ntheta < 1) hemi->ntheta = 1;
	if (hemi->ntheta > MAX_HEMISAMPLE) hemi->ntheta = MAX_HEMISAMPLE;
	hemi->nphi   = 3 * hemi->ntheta;

	/* calculate irradiance. */
	hemi->rtotal = 0.0;
	count = 0;

	if (first) {
		
		fprintf(fp, "%d\n", hemi->ntheta * hemi->nphi);
	}

	ri_ortho_basis(hemi->basis, &v->dir);

	for (j = 0; j < (int)hemi->nphi; j++) {
		for (i = 0; i < (int)hemi->ntheta; i++) {
			theta = sqrt(((double)i + randomMT()) / hemi->ntheta);
			phi = 2.0 * M_PI * ((double)j + randomMT()) / hemi->nphi;		
			dir.e[0] = (float)(cos(phi) * theta);
			dir.e[1] = (float)(sin(phi) * theta);
			dir.e[2] = (float)(sqrt(1.0 - theta * theta));

			for (k = 0; k < 3; k++) {
				ray.dir.e[k]  = dir.e[0] * hemi->basis[0].e[k]
					      + dir.e[1] * hemi->basis[1].e[k]
					      + dir.e[2] * hemi->basis[2].e[k];
			}

			ri_vector_normalize(&(ray.dir));
			if (first) {
				fprintf(fp, "%f %f %f\n",
					ray.dir.e[0], ray.dir.e[1], ray.dir.e[2]);
			}

			hit = ri_raytrace(ri_render_get(), &ray, &surfinfo);

			if (!hit) {
				ri_texture_ibl_fetch(&rad,
						     light->texture,
						     &ray.dir);

	
				dpower[0] += rad.e[0];
				dpower[1] += rad.e[1];
				dpower[2] += rad.e[2];
			}

			/* radius of irradiance cache value. */ 
			hemi->rtotal += hemi->samples[i][j].r;
		}
	}


	power->e[0] = (float)(dpower[0] / (double)(hemi->ntheta * hemi->nphi));
	power->e[1] = (float)(dpower[1] / (double)(hemi->ntheta * hemi->nphi));
	power->e[2] = (float)(dpower[2] / (double)(hemi->ntheta * hemi->nphi));

	if (first) {
		fclose(fp);
		first = 0;
	}
}

static void
fixed_shading(ri_vector_t *radiance,
	      const ri_vector_t *eye,
	      ri_light_t *light,
	      const ri_ray_t *ray,
	      ri_surface_info_t *surfinfo)
{
	int fixicache = 1;
	int forcecalc = 0;
	ri_ray_t surfray;
	ri_vector_t neye;
	ri_vector_t indirectrad;
	ri_vector_t directrad;
	ri_vector_t causrad;
	ri_option_t *opt;
	ri_octree_t *irradcache;
	ri_vector_t texcol;
	ri_photonmap_t *gmap;
	ri_photonmap_option_t *pmapopt;
	
	opt     = ri_render_get()->context->option;
	pmapopt = ri_photonmap_get_option();

	irradcache = ri_render_get()->irradiance_cache;
	gmap = ri_render_get()->global_photonmap;

	ri_ray_copy(&surfray, ray);
	ri_vector_copy(&(surfray.org), &surfinfo->pos);
	ri_vector_copy(&(surfray.dir), &surfinfo->normal);

	ri_vector_zero(&directrad);
	ri_vector_zero(&indirectrad);
	ri_vector_zero(&causrad);

	if (surfinfo->geom->shadername &&
	    strcmp(surfinfo->geom->shadername, "bssrdf") == 0) {

		ri_vector_copy(&neye, eye);
		ri_vector_neg(&neye);

		ri_vector_zero(&directrad);
		if (opt->enable_direct_lighting) {
			direct_lighting(&directrad,
					light,
					ray, surfinfo, &neye,
					opt->narealight_rays);
		}

		ri_vector_add(radiance, &directrad, &indirectrad);
		//ri_vector_copy(result, &indirectrad);

	} else {

		ri_vector_zero(&indirectrad);
		if (opt->enable_indirect_lighting) {
			indirect_lighting(&indirectrad,
					  gmap,
					  irradcache,
					  &surfray,
					  opt->nfinalgather_rays,
					  pmapopt->max_gather_radius,
					  pmapopt->max_gather_photons,
					  fixicache,
					  forcecalc);
		}

		ri_vector_zero(&causrad);
		if (opt->enable_caustics_lighting) {
			caustics_lighting(&causrad,
					  ri_render_get()->caustics_photonmap,
					  &surfray,
					  pmapopt->max_gather_radius,
					  pmapopt->max_gather_photons);
		}

		ri_vector_zero(&directrad);
		if (opt->enable_direct_lighting) {
			direct_lighting(&directrad,
					light,
					ray, surfinfo, &neye,
					opt->narealight_rays);
		}

		ri_vector_add(radiance, &indirectrad, &causrad);
		ri_vector_add(radiance, radiance, &directrad);
	}
	
	if (surfinfo->geom->material &&
	    surfinfo->geom->material->texture) {
		ri_texture_fetch(&texcol,
				 surfinfo->geom->material->texture,
				 surfinfo->u, surfinfo->v);
		ri_vector_mul(radiance, radiance, &texcol);

	} else {
		ri_vector_mul(radiance, radiance, &surfinfo->color);
	}
}

static void
shader_shading(ri_vector_t *radiance,
	       const ri_vector_t *eye,
	       ri_light_t *light,
	       const ri_ray_t *ray,
	       ri_surface_info_t *surfinfo)
{
	ri_shader_t *shader;
	ri_output_t out;
	ri_status_t status;
	ri_vector_t lightpos;
	ri_vector_t Idir;

	shader = surfinfo->geom->shader;

	if (shader) {
	
		ri_timer_start(ri_render_get()->context->timer,
			       "Surface shader execution");	
		
		lightpos.e[0] =  1.0;
		lightpos.e[1] =  0.5;
		lightpos.e[2] =  1.0;
		lightpos.e[3] =  1.0;

		ri_vector_copy(&Idir, eye);
		ri_vector_normalize(&Idir);

		status.thread_num = ray->thread_num;
		status.ray_depth  = 0;

		/* Setup predefined surface shader variables. */
		ri_vector_copy(&(status.input.Cs), &surfinfo->color);
		ri_vector_copy(&(status.input.P), &surfinfo->pos); 
		ri_vector_copy(&(status.input.N), &surfinfo->normal);
		if (surfinfo->inside) {	/* flip normal */
			ri_vector_neg(&(status.input.N));
		}
		ri_vector_copy(&(status.input.dPdu), &surfinfo->tangent);
		ri_vector_copy(&(status.input.dPdv), &surfinfo->binormal);
		ri_vector_copy(&(status.input.E), eye);
		ri_vector_copy(&(status.input.I), &Idir);
		ri_vector_copy(&(status.input.L), &lightpos);
		status.input.s = surfinfo->u;
		status.input.t = surfinfo->v;

		status.input.Os.e[0] = surfinfo->opacity;
		status.input.Os.e[1] = surfinfo->opacity;
		status.input.Os.e[2] = surfinfo->opacity;
		status.input.Os.e[3] = surfinfo->opacity;

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


#if 0
void
sis_ibl(ri_vector_t *power, ri_hemisphere_t *hemi,
     	ri_ray_t v, ri_light_t *light)
{
	static int first = 1;
	static float *dirs;
	static float *intensitys;
	static int    width, height;
	static int    nsamples;    
	int    i, k;
	int    hit;
	int    x, y;
	float  dot;
	float  uparam, vparam, r;
	float  theta, phi;
	float  dpower[3];
	float  domega;
	ri_ray_t ray;
	ri_surface_info_t surfinfo;
	FILE *fp;

	if (first) {
		fp = fopen(light->sisfile, "r");
		if (!fp) {
			printf("can't open file [ %s ]\n", light->sisfile);
			exit(-1);
		}

		fscanf(fp, "%d", &nsamples);
		printf("sis: nsamples = %d\n", nsamples);

		fscanf(fp, "%d %d", &width, &height);
		printf("sis: width x height = %d x %d\n", width, height);

		dirs = (float *)ri_mem_alloc(sizeof(float) * nsamples * 3);
		intensitys = (float *)ri_mem_alloc(sizeof(float) * nsamples * 3);

		for (i = 0; i < nsamples; i++) {

	
			fscanf(fp, "%d %d %f %f %f", &x, &y,
						&(intensitys[3 * i + 0]),
						&(intensitys[3 * i + 1]),
						&(intensitys[3 * i + 2]));

			uparam = (width / 2.0 - x) / (width / 2.0);
			vparam = (height / 2.0 - y) / (height / 2.0);
			r      = sqrt(uparam * uparam + vparam * vparam);

			theta = M_PI * r;
			phi   = atan2(vparam, uparam);

			dirs[3 * i + 0] = sin(theta) * cos(phi);
			dirs[3 * i + 1] = sin(theta) * sin(phi);
			dirs[3 * i + 2] = cos(theta);
		}

		fclose(fp);

		first = 0;
	}


	dpower[0] = dpower[1] = dpower[2] = 0.0;

	ri_ortho_basis(hemi->basis, v.dir);

	ri_vector_copy(&(ray.org), v.org);

	for (i = 0; i < nsamples; i++) {
#if 0
		for (k = 0; k < 3; k++) {
			ray.dir.e[k]  = dirs[3 * i + 0] * hemi->basis[0].e[k]
				      + dirs[3 * i + 1] * hemi->basis[1].e[k]
				      + dirs[3 * i + 2] * hemi->basis[2].e[k];
		}
#endif
		ray.dir.e[0] = dirs[3 * i + 0];
		ray.dir.e[1] = dirs[3 * i + 1];
		ray.dir.e[2] = dirs[3 * i + 2];
		
		ri_vector_normalize(&(ray.dir));

		dot = ri_vector_dot3(v.dir, ray.dir);
		if (dot <= 0.0) continue;

		hit = ri_raytrace(ri_render_get(), &ray, &surfinfo);
		if (!hit) {
#if 0
			theta = acos(ray.dir.e[2]);

			domega = (2.0 * M_PI / (float)width) *
				 (2.0 * M_PI / (float)height) *
				 sinc(theta);

			dpower[0] += intensitys[3 * i + 0] * dot * domega;
			dpower[1] += intensitys[3 * i + 1] * dot * domega;
			dpower[2] += intensitys[3 * i + 2] * dot * domega;
#endif
			dpower[0] += intensitys[3 * i + 0] * dot;
			dpower[1] += intensitys[3 * i + 1] * dot;
			dpower[2] += intensitys[3 * i + 2] * dot;

		}
	}

	power->e[0] = dpower[0] / (double)nsamples;
	power->e[1] = dpower[1] / (double)nsamples;
	power->e[2] = dpower[2] / (double)nsamples;
}
#endif
