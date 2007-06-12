/*
 * Image-Based Lighting routine.
 *
 * Copyright Syoyo Fujita
 *
 * $Id: ibl.c,v 1.13 2004/08/15 05:19:39 syoyo Exp $
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>

#include "vector.h"
#include "ibl.h"
#include "reflection.h"
#include "random.h"
#include "brdf.h"
#include "qmc.h"

#ifndef M_PI
#define M_PI 3.1415926535
#endif

#ifndef INV_M_PI
#define INV_M_PI 0.31831f	/* 1 / M_PI */
#endif

typedef struct _histgram_t
{
	//int x, y;
	ri_vector_t dir;
	ri_vector_t color;
	ri_float_t sumintensity;
} histgram_t;

static int inunitsphere(int x, int y, int w, int h);
static void xy_to_vec(ri_vector_t *vec, int x, int y, int w, int h);
static ri_float_t rgb_to_intensity(ri_float_t r, ri_float_t g, ri_float_t b);
static ri_float_t sinc(ri_float_t x);
static void gen_qmc_samples(ri_float_t *samples,
			    int nsamples, int instancenum, int **perm);

/*
 * Cosine weighted sampling.
 * Generate sampling direction with PDF(Probability Density Function)
 * cos(theta) / PI on hemisphere.
 * i.e. more samples along normal direction, less samples along perpendicular
 * to normal vector.
 */
void
ri_ibl_sample_cosweight(
	ri_vector_t       *power,
	ri_vector_t       *normal,
	int                nsamples,
	const ri_ray_t    *inray,
	const ri_vector_t *pos,
	const ri_vector_t *eye,
	const ri_light_t  *light)
{
	int i, j, k;
	int hit;
	int count;
	int ntheta, nphi;
	int tid = inray->thread_num;
	ri_float_t dpower[3];
	ri_float_t theta, phi;
	ri_float_t brdf;
	ri_float_t *samples;
	ri_vector_t rad;
	ri_vector_t dir;
	ri_vector_t basis[3];
	ri_ray_t r;
	ri_intersection_state_t state;
	ri_option_t *opt;
	
#if 0
	ri_float_t glossness = 15.0;
	ri_float_t pdf;
#endif

	(void)eye;

	opt = ri_render_get()->context->option;

	dpower[0] = dpower[1] = dpower[2] = 0.0;

	ri_ray_copy(&r, inray);

	ri_vector_copy(&r.org, pos);

	/* slightly move the shading point towards the surface normal */
	r.org.f[0] += normal->f[0] * 0.0001;
	r.org.f[1] += normal->f[1] * 0.0001;
	r.org.f[2] += normal->f[2] * 0.0001;

	r.thread_num = inray->thread_num;

	ri_vector_zero(&rad);

	//hemi->rtotal = 0.0;
	count = 0;

	ri_ortho_basis(basis, normal);

	if (opt->use_qmc) {	/* quasi-Monte Carlo sampling */

		samples = (ri_float_t *)ri_mem_alloc(sizeof(ri_float_t) * nsamples * 2);

		gen_qmc_samples(samples,
				nsamples, inray->i, ri_render_get()->perm_table);

		for (i = 0; i < nsamples; i++) {
			theta = sqrt(samples[2 * i + 0]);
			phi = 2.0 * M_PI * samples[2 * i + 1];		
			dir.f[0] = cos(phi) * theta;
			dir.f[1] = sin(phi) * theta;
			dir.f[2] = sqrt(1.0 - theta * theta);

			for (k = 0; k < 3; k++) {
				r.dir.f[k]=dir.f[0]*basis[0].f[k]
					  +dir.f[1]*basis[1].f[k]
					  +dir.f[2]*basis[2].f[k];
			}

			ri_vector_normalize3(&(r.dir));

			hit = ri_raytrace(ri_render_get(),
					  &r, &state);

			if (!hit) {
				ri_texture_ibl_fetch(&rad,
						     light->texture,
						     &r.dir);

				/* lambert */
				brdf = INV_M_PI;	/* 1.0 / M_PI; */

				dpower[0] += rad.f[0] * brdf;
				dpower[1] += rad.f[1] * brdf;
				dpower[2] += rad.f[2] * brdf;
			}

			/* radius of irradiance cache value. */ 
			//hemi->rtotal += hemi->samples[i][j].r;
		}

		power->f[0] = dpower[0] / (ri_float_t)nsamples;
		power->f[1] = dpower[1] / (ri_float_t)nsamples;
		power->f[2] = dpower[2] / (ri_float_t)nsamples;

		ri_mem_free(samples);

	} else {	/* Monte Carlo sampling */
		/* theta * phi = total samples.
		 * phi = 3 * theta.
		 */
		ntheta = nsamples / 3.0;
		ntheta = (int)sqrt((ri_float_t)ntheta);
		if (ntheta < 1) {
			ntheta = 1;
		}
		if (ntheta > MAX_HEMISAMPLE) {
			ntheta = MAX_HEMISAMPLE;
		}
		nphi   = 3 * ntheta;

		for (j = 0; j < (int)nphi; j++) {
			for (i = 0; i < (int)ntheta; i++) {

#if 1
				theta = sqrt(((ri_float_t)i + randomMT2(tid)) /
					ntheta);
				phi = 2.0 * M_PI * ((ri_float_t)j + randomMT2(tid)) /
				      nphi;		
				dir.f[0] = cos(phi) * theta;
				dir.f[1] = sin(phi) * theta;
				dir.f[2] = sqrt(1.0 - theta * theta);

				for (k = 0; k < 3; k++) {
					r.dir.f[k]=dir.f[0]*basis[0].f[k]
						  +dir.f[1]*basis[1].f[k]
						  +dir.f[2]*basis[2].f[k];
				}
#else
				ri_sample_modified_phong(&(r.dir), &pdf,
					&inray->dir, normal,
					randomMT(), randomMT(), glossness);
#endif

				ri_vector_normalize3(&(r.dir));

				hit = ri_raytrace(ri_render_get(),
						  &r, &state);

				if (!hit) {
					ri_texture_ibl_fetch(&rad,
							     light->texture,
							     &r.dir);

					/* lambert */
					brdf = 1.0 / M_PI;
					
					/* phong */
					//brdf = pdf / M_PI;

					dpower[0] += rad.f[0] * brdf;
					dpower[1] += rad.f[1] * brdf;
					dpower[2] += rad.f[2] * brdf;
				}

				/* radius of irradiance cache value. */ 
				//hemi->rtotal += hemi->samples[i][j].r;
			}
		}

		power->f[0] = M_PI * dpower[0] /
			      (ri_float_t)(ntheta * nphi);
		power->f[1] = M_PI * dpower[1] /
			      (ri_float_t)(ntheta * nphi);
		power->f[2] = M_PI * dpower[2] /
			      (ri_float_t)(ntheta * nphi);
	}
}

/*
 * Importance sampling.
 * Generate sampling direction according to intensity histgram.
 * Higher intensity has higher probability, lower intensity has lower
 * probability.
 */
void
ri_ibl_sample_importance(
	ri_vector_t       *power,
	ri_hemisphere_t   *hemi,
	int                nsamples,
	const ri_vector_t *pos,
	const ri_vector_t *eye,
	const ri_light_t  *light)
{
	static histgram_t *hist = NULL;
	static int         npixels;
	int                hit;
	int                i, j;
	int                index;
	int                w, h;
	ri_float_t              r, g, b;
	ri_float_t              dot;
	ri_float_t             sumintensity;
	ri_float_t             maxintensity;
	ri_float_t             rnd;
	ri_float_t             perterb[3];
	ri_float_t             dpower[3];
	ri_float_t             brdf;
	ri_ray_t           ray;
	ri_intersection_state_t  state;

	static ri_vector_t *sampledirs = NULL;
	static ri_vector_t *samplecols = NULL;

	(void)eye;

	if (!light->texture) {
		printf("there is no ibl texture.\n");
		exit(-1);
	}

	/* assume IBL texture is Angular Map */

	w = light->texture->width;
	h = light->texture->height;
	//printf("wxh = %d, %d\n", w, h);

	//if (hist == NULL) {
	if (sampledirs == NULL) {
		/* calculate histgram of intensity of map */
		npixels = 0;
		hist = (histgram_t *)ri_mem_alloc(sizeof(histgram_t) *
						  w * h);
		
		sumintensity = 0.0;

		for (j = 0; j < h; j++) {
			for (i = 0; i < w; i++) {

				if (!inunitsphere(i, j, w, h)) continue;

				index = j * w + i;

				xy_to_vec(&(hist[npixels].dir), i, j, w, h);

				r = light->texture->data[4 * index + 0];
				g = light->texture->data[4 * index + 1];
				b = light->texture->data[4 * index + 2];
#if 0
				if (r <= 0.0 || isnan(r)) r = 0.0;
				if (g <= 0.0 || isnan(g)) g = 0.0;
				if (b <= 0.0 || isnan(b)) b = 0.0;
#else
				if (r <= 0.0) r = 0.0;
				if (g <= 0.0) g = 0.0;
				if (b <= 0.0) b = 0.0;
#endif

				hist[npixels].color.f[0] = r;
				hist[npixels].color.f[1] = g;
				hist[npixels].color.f[2] = b;
				hist[npixels].color.f[3] = 1.0;

				sumintensity += rgb_to_intensity(r, g, b);

				hist[npixels].sumintensity = sumintensity;
				
				npixels++;
			}
		}
		printf("IBL:importance: npixels = %d\n", npixels);
		printf("IBL:importance: sunintencity = %f\n", sumintensity);

		maxintensity = sumintensity;

		sampledirs = (ri_vector_t *)ri_mem_alloc(sizeof(ri_vector_t) *
							 nsamples);
		samplecols = (ri_vector_t *)ri_mem_alloc(sizeof(ri_vector_t) *
							 nsamples);

		for (j = 0; j < nsamples; j++) {
			/* generate random number in [0, maxintensity) */
			rnd = randomMT() * maxintensity;

			for (i = 0; i < npixels; i++) {
				if (rnd < hist[i].sumintensity) break;
			}

			if (i == npixels) {
				printf("??? i == npixels\n");
			}

			ri_vector_copy(&(sampledirs[j]), &hist[i].dir);
			ri_vector_copy(&(samplecols[j]), &hist[i].color);

		}

	}

	ri_vector_copy(&(ray.org), pos);
	maxintensity = hist[npixels - 1].sumintensity;

	dpower[0] = dpower[1] = dpower[2] = 0.0;

	for (i = 0; i < nsamples; i++) {

#if 0
		/* generate random number in [0, maxintensity) */
		rnd = randomMT() * maxintensity;

		for (j = 0; j < npixels; j++) {
			if (rnd < hist[j].sumintensity) break;
		}

		if (j == npixels) {
			printf("??? j == npixels\n");
		}


		dot = ri_vector_dot3(v.dir, hist[j].dir);
		if (dot <= 0.0) continue;

		ri_vector_copy(&(ray.dir), hist[j].dir);
	
		hit = ri_raytrace(ri_render_get(), &ray, &surfinfo);
		if (!hit) {	/* no occluders towards ray direction */
			dpower[0] += hist[j].color.f[0] * dot;
			dpower[1] += hist[j].color.f[1] * dot;
			dpower[2] += hist[j].color.f[2] * dot;
		}
#endif

		ri_vector_copy(&(ray.dir), &sampledirs[i]);
		//ri_vector_normalize(&(ray.dir));

		/* slightly perterb samle direction to soften shadow
		 * boundary.
		 */
		perterb[0] = 0.02 * randomMT() - 0.01;
		perterb[1] = 0.02 * randomMT() - 0.01;
		perterb[2] = 0.02 * randomMT() - 0.01;
		
		ray.dir.f[0] += perterb[0];
		ray.dir.f[1] += perterb[1];
		ray.dir.f[2] += perterb[2];
		ri_vector_normalize3(&(ray.dir));

		dot = ri_vector_dot3(&hemi->basis[2], &ray.dir);
		if (dot <= 0.0) continue;
	
		hit = ri_raytrace(ri_render_get(), &ray, &state);
		if (!hit) {	/* no occluders towards ray direction */
			//brdf = ri_brdf_modified_phong(eye, ray.dir, v.dir, 2.3);
#if 0
			brdf = ri_brdf_ward_anisotropic(eye, ray.dir, 
							hemi->basis[2],
							hemi->basis[0],
							hemi->basis[1],
							//0.67, 0.07, 0.092, 0.092);
							//0.15, 0.19, 0.088, 0.13);
							//0.1, 0.21, 0.04, 0.09);
							//0.1, 0.33, 0.05, 0.16);
							0.0, 1.0, 0.5, 0.15);
#endif
#if 0
			brdf = ri_brdf_ashikhmin_shirley(eye, ray.dir,
							 hemi->basis[2],
							 hemi->basis[0],
							 hemi->basis[1],
							 0.3, 0.7,
							 10, 1000);
			brdf = ri_brdf_phong(eye, ray.dir,
					     hemi->basis[2],
					     0.3, 0.7, 3.3);
#endif
			brdf = 1.0 / M_PI;
			dpower[0] += samplecols[i].f[0] * brdf * dot;
			dpower[1] += samplecols[i].f[1] * brdf * dot;
			dpower[2] += samplecols[i].f[2] * brdf * dot;
#if 0
			dpower[0] += samplecols[i].f[0] * dot;
			dpower[1] += samplecols[i].f[1] * dot;
			dpower[2] += samplecols[i].f[2] * dot;
#endif
		}
	}

	power->f[0] = (2.0 * M_PI) * dpower[0] / (ri_float_t)(nsamples);
	power->f[1] = (2.0 * M_PI) * dpower[1] / (ri_float_t)(nsamples);
	power->f[2] = (2.0 * M_PI) * dpower[2] / (ri_float_t)(nsamples);

	//printf("pwr = %f, %f, %f\n", power->f[0], power->f[1], power->f[2]);
}


/* Stratified importance sampling.
 * Same as above importance sampling, but random number is generated with
 * stratification.
 */
void
ri_ibl_sample_stratified(
	ri_vector_t       *power,
	ri_hemisphere_t   *hemi,
	int                nsamples,
	const ri_vector_t *pos,
	const ri_vector_t *eye,
	const ri_light_t  *light)
{
	static histgram_t *hist = NULL;
	static int         npixels;
	int                hit;
	int                i, j;
	int                index;
	int                w, h;
	ri_float_t              r, g, b;
	ri_float_t              dot;
	ri_float_t             sumintensity;
	ri_float_t             maxintensity;
	ri_float_t             rnd;
	ri_float_t             perterb[3];
	ri_float_t             dpower[3];
	ri_float_t             brdf;
	ri_float_t             theta;
	ri_ray_t           ray;
	ri_intersection_state_t  state;
	ri_vector_t        rad;

	static ri_vector_t *sampledirs = NULL;
	static ri_vector_t *samplecols = NULL;

	(void)eye;

	if (!light->texture) {
		printf("there is no ibl texture.\n");
		exit(-1);
	}

	/* assume IBL texture is Angular Map */

	w = light->texture->width;
	h = light->texture->height;
	//printf("wxh = %d, %d\n", w, h);

	//if (hist == NULL) {
	if (sampledirs == NULL) {
		/* calculate histgram of intensity of map */
		npixels = 0;
		hist = (histgram_t *)ri_mem_alloc(sizeof(histgram_t) *
						  w * h);
		
		sumintensity = 0.0;

		for (j = 0; j < h; j++) {
			for (i = 0; i < w; i++) {

				if (!inunitsphere(i, j, w, h)) continue;

				index = j * w + i;

				xy_to_vec(&(hist[npixels].dir), i, j, w, h);

				r = light->texture->data[4 * index + 0];
				g = light->texture->data[4 * index + 1];
				b = light->texture->data[4 * index + 2];
#if 0
				if (r <= 0.0 || isnan(r)) r = 0.0;
				if (g <= 0.0 || isnan(g)) g = 0.0;
				if (b <= 0.0 || isnan(b)) b = 0.0;
#else
				if (r <= 0.0) r = 0.0;
				if (g <= 0.0) g = 0.0;
				if (b <= 0.0) b = 0.0;
#endif

				hist[npixels].color.f[0] = r;
				hist[npixels].color.f[1] = g;
				hist[npixels].color.f[2] = b;
				hist[npixels].color.f[3] = 1.0;

				sumintensity += rgb_to_intensity(r, g, b);

				hist[npixels].sumintensity = sumintensity;
				
				npixels++;
			}
		}
		printf("IBL:stratified: npixels = %d\n", npixels);
		printf("IBL:stratified: sunintencity = %f\n", sumintensity);

		maxintensity = sumintensity;

		sampledirs = (ri_vector_t *)ri_mem_alloc(sizeof(ri_vector_t) *
							 nsamples);
		samplecols = (ri_vector_t *)ri_mem_alloc(sizeof(ri_vector_t) *
							 nsamples);

		for (j = 0; j < nsamples; j++) {
			/* generate random number in [0, maxintensity) */
			rnd = (((ri_float_t)j + randomMT()) / (ri_float_t)nsamples) * maxintensity;

			for (i = 0; i < npixels; i++) {
				if (rnd < hist[i].sumintensity) break;
			}

			if (i == npixels) {
				printf("??? i == npixels\n");
			}

			ri_vector_copy(&(sampledirs[j]), &hist[i].dir);
			ri_vector_copy(&(samplecols[j]), &hist[i].color);

		}

	}

	ri_vector_copy(&(ray.org), pos);
	maxintensity = hist[npixels - 1].sumintensity;

	dpower[0] = dpower[1] = dpower[2] = 0.0;

	for (i = 0; i < nsamples; i++) {

#if 0
		/* generate random number in [0, maxintensity) */
		rnd = randomMT() * maxintensity;

		for (j = 0; j < npixels; j++) {
			if (rnd < hist[j].sumintensity) break;
		}

		if (j == npixels) {
			printf("??? j == npixels\n");
		}


		dot = ri_vector_dot3(v.dir, hist[j].dir);
		if (dot <= 0.0) continue;

		ri_vector_copy(&(ray.dir), hist[j].dir);
	
		hit = ri_raytrace(ri_render_get(), &ray, &surfinfo);
		if (!hit) {	/* no occluders towards ray direction */
			dpower[0] += hist[j].color.f[0] * dot;
			dpower[1] += hist[j].color.f[1] * dot;
			dpower[2] += hist[j].color.f[2] * dot;
		}
#endif

		ri_vector_copy(&(ray.dir), &sampledirs[i]);

		/* slightly perterb samle direction to soften shadow
		 * boundary.
		 */
		perterb[0] = 0.02 * randomMT() - 0.01;
		perterb[1] = 0.02 * randomMT() - 0.01;
		perterb[2] = 0.02 * randomMT() - 0.01;
		
		ray.dir.f[0] += perterb[0];
		ray.dir.f[1] += perterb[1];
		ray.dir.f[2] += perterb[2];
		ri_vector_normalize3(&(ray.dir));

		dot = ri_vector_dot3(&hemi->basis[2], &ray.dir);
		if (dot <= 0.0) continue;
	
		hit = ri_raytrace(ri_render_get(), &ray, &state);
		if (!hit) {	/* no occluders towards ray direction */
			//brdf = ri_brdf_modified_phong(eye, ray.dir, v.dir, 2.3);
#if 0
			brdf = ri_brdf_ward_anisotropic(eye, ray.dir, 
							hemi->basis[2],
							hemi->basis[0],
							hemi->basis[1],
							//0.67, 0.07, 0.092, 0.092);
							//0.15, 0.19, 0.088, 0.13);
							//0.1, 0.21, 0.04, 0.09);
							//0.1, 0.33, 0.05, 0.16);
							0.0, 1.0, 0.5, 0.15);
			brdf = ri_brdf_ashikhmin_shirley(eye, ray.dir,
							 hemi->basis[2],
							 hemi->basis[0],
							 hemi->basis[1],
							 0.3, 0.7,
							 10, 1000);
#endif
			ri_texture_ibl_fetch(&rad,
					     light->texture,
					     &ray.dir);
			theta = acos(sampledirs[i].f[2]);
			//theta = 0.0;
			dot = 1.0;
			brdf = 1 / M_PI;
#if 0
			dpower[0] += samplecols[i].f[0] * brdf * dot * sinc(theta);
			dpower[1] += samplecols[i].f[1] * brdf * dot * sinc(theta);
			dpower[2] += samplecols[i].f[2] * brdf * dot * sinc(theta);
#endif
			/* It is needed to multiply a intensity by solid angle
			 *
			 * D(omega) = sin(theta) * D(theta) * D(phi) 
			 *
			 * We assume HDRI is given by Angular Map and
			 * its solid angle are caclulated by
			 *
			 * 2 * PI   2 * PI   sin(theta)
			 * ------ * ------ * --------- 
			 * width    width      theta
			 *
			 * It is derived by ramamoorthi's prefilter.c
			 *
			 * http://graphics.stanford.edu/papers/envmap/
			 * prefilter.c
			 *
			 * I can't figure out why...
			 */
			dpower[0] += rad.f[0] * brdf * dot * sinc(theta);
			dpower[1] += rad.f[1] * brdf * dot * sinc(theta);
			dpower[2] += rad.f[2] * brdf * dot * sinc(theta);
		}
	}

	power->f[0] = (4.0 * M_PI * M_PI) * dpower[0] / (ri_float_t)nsamples;
	power->f[1] = (4.0 * M_PI * M_PI) * dpower[1] / (ri_float_t)nsamples;
	power->f[2] = (4.0 * M_PI * M_PI) * dpower[2] / (ri_float_t)nsamples;
}

/* Structured importance sampling.
 * see:
 * "Stractured Importance Sampling of Environment Maps"
 * SIGGRAPH 2003.
 *
 * A implemantaion of sampling point generation with SIS are in
 * tools/sis
 * 
 */
void
ri_ibl_sample_structured(
	ri_vector_t       *power,
	ri_hemisphere_t   *hemi,
	int                nsamples,
	const ri_vector_t *pos,
	const ri_vector_t *eye,
	const ri_light_t  *light)
{
	static int first = 1;
	//static ri_float_t *dirs;
	static ri_vector_t *dirs;
	static ri_float_t *intensitys;
	static int    width, height;
	static int    nusedsamples;    
	int    i;
	int    hit;
	//int    x, y;
	ri_float_t  dot;
	//ri_float_t  uparam, vparam, r;
	ri_float_t  theta;
        //ri_float_t  phi;
	ri_float_t  dpower[3];
	//ri_float_t  domega;
	ri_float_t  area;
	int    tmp1, tmp2;
	ri_float_t brdf;
	ri_ray_t ray;
	ri_intersection_state_t state;
	FILE *fp;

	(void)nsamples;
	(void)eye;

	if (first) {
		fp = fopen(light->sisfile, "r");
		if (!fp) {
			printf("sis:can't open file [ %s ]\n", light->sisfile);
			exit(-1);
		}

		fscanf(fp, "%d", &nusedsamples);
		printf("sis: nsamples = %d\n", nusedsamples);

		fscanf(fp, "%d %d", &width, &height);
		printf("sis: width x height = %d x %d\n", width, height);

		dirs = (ri_vector_t *)ri_mem_alloc(sizeof(ri_vector_t) * nusedsamples);
		intensitys = (ri_float_t *)ri_mem_alloc(sizeof(ri_float_t) * nusedsamples * 3);

		for (i = 0; i < nusedsamples; i++) {

	
#if 0
			fscanf(fp, "%d %d %f %f %f", &x, &y,
						&(intensitys[3 * i + 0]),
						&(intensitys[3 * i + 1]),
						&(intensitys[3 * i + 2]));

			xy_to_vec(&(dirs[i]), x, y, width, height);
#endif

#ifdef ENABLE_DOUBLE_PRECISION
			fscanf(fp, "%lf, %lf, %lf, %lf, %lf, %lf, %lf, %d, %d, \n",
#else
			fscanf(fp, "%f, %f, %f, %f, %f, %f, %f, %d, %d, \n",
#endif
				&(dirs[i].f[0]),
				&(dirs[i].f[1]),
				&(dirs[i].f[2]),
				&(intensitys[3 * i + 0]),
				&(intensitys[3 * i + 1]),
				&(intensitys[3 * i + 2]),
				&area,
				&tmp1,		/* npix in the stratum */
				&tmp2);		/* creation level */

			intensitys[3 * i + 0] *= area;
			intensitys[3 * i + 1] *= area;
			intensitys[3 * i + 2] *= area;
		}

		fclose(fp);

		first = 0;
	}


	dpower[0] = dpower[1] = dpower[2] = 0.0;


	for (i = 0; i < nusedsamples; i++) {
		ri_vector_copy(&(ray.dir), &dirs[i]);
		ri_vector_normalize3(&(ray.dir));

		dot = ri_vector_dot3(&hemi->basis[2], &ray.dir);
		if (dot <= 0.0) continue;

		// slightly moves the ray towards its direction
		// for solving numerical problem.
		ri_vector_copy(&(ray.org), pos);

		ray.org.f[0] += ray.dir.f[0] * 0.0001;
		ray.org.f[1] += ray.dir.f[1] * 0.0001;
		ray.org.f[2] += ray.dir.f[2] * 0.0001;

		hit = ri_raytrace(ri_render_get(), &ray, &state);
		if (!hit) {
			theta = acos(ray.dir.f[2]);

			//domega = ((2.0 * M_PI) * (2.0 * M_PI) / (ri_float_t)nusedsamples) * sinc(theta);

			brdf = 1 / M_PI;
#if 0
			dpower[0] += intensitys[3 * i + 0] * brdf * dot * domega;
			dpower[1] += intensitys[3 * i + 1] * brdf * dot * domega;
			dpower[2] += intensitys[3 * i + 2] * brdf * dot * domega;
#else
			dpower[0] += intensitys[3 * i + 0] * brdf * dot;
			dpower[1] += intensitys[3 * i + 1] * brdf * dot;
			dpower[2] += intensitys[3 * i + 2] * brdf * dot;

#endif

		}
	}

	power->f[0] = (ri_float_t)dpower[0];
	power->f[1] = (ri_float_t)dpower[1];
	power->f[2] = (ri_float_t)dpower[2];
}

/* EIHDRI sampling.
 * see:
 *
 * Thomas Kollig, Alexander Keller,
 * "Efficient Illumination by High Dynamic Range Images"
 * 14th Eurographics Symposium on Rendering, 2003.
 *
 * In this function, simply do integration using sampling points which is
 * generated by another generator program.
 * 
 */
void
ri_ibl_sample_eihdri(
	ri_vector_t       *power,
	ri_hemisphere_t   *hemi,
	int                nsamples,
	const ri_vector_t *pos,
	const ri_vector_t *eye,
	const ri_light_t  *light)
{
	static int first = 1;
	//static ri_float_t *dirs;
	static ri_vector_t *dirs;
	static ri_float_t *intensitys;
	//static int    width, height;
	static int    nusedsamples;
	static int    nlayers;
	int    i;
	int    hit;
	//int    x, y;
	ri_float_t  dot;
	//ri_float_t  uparam, vparam, r;
	ri_float_t  theta;
        ri_float_t  phi;
	ri_float_t  maxangle;
	ri_float_t  dpower[3];
	//ri_float_t  domega;
	ri_float_t brdf;
	ri_ray_t ray;
	ri_intersection_state_t state;
	FILE *fp;

	(void)nsamples;
	(void)eye;

	if (first) {
		fp = fopen(light->eihdrifile, "r");
		if (!fp) {
			printf("sis:can't open file [ %s ]\n", light->sisfile);
			exit(-1);
		}

		fscanf(fp, "%d %d", &nusedsamples, &nlayers);
		printf("sis: nsamples = %d\n", nusedsamples);

		dirs = (ri_vector_t *)ri_mem_alloc(
					sizeof(ri_vector_t) * nusedsamples);
		intensitys = (ri_float_t *)ri_mem_alloc(
					sizeof(ri_float_t) * nusedsamples * 3);

		for (i = 0; i < nusedsamples; i++) {
#ifdef ENABLE_DOUBLE_PRECISION
			fscanf(fp, "%lf %lf %lf %lf %lf %lf %lf %lf %lf\n",
#else
			fscanf(fp, "%f %f %f %f %f %f %f %f %f\n",
#endif
				&(dirs[i].f[0]),
				&(dirs[i].f[1]),
				&(dirs[i].f[2]),
				&theta,				/* not used */
				&phi,				/* not used */
				&maxangle,			/* not used */
				&(intensitys[3 * i + 0]),
				&(intensitys[3 * i + 1]),
				&(intensitys[3 * i + 2]));
		}

		fclose(fp);

		first = 0;
	}

	dpower[0] = dpower[1] = dpower[2] = 0.0;

	ri_vector_copy(&(ray.org), pos);

	for (i = 0; i < nusedsamples; i++) {
		ri_vector_copy(&(ray.dir), &dirs[i]);
		ri_vector_normalize3(&(ray.dir));

		dot = ri_vector_dot3(&hemi->basis[2], &ray.dir);
		if (dot <= 0.0) continue;

		hit = ri_raytrace(ri_render_get(), &ray, &state);
		if (!hit) {
			theta = acos(ray.dir.f[2]);

			brdf = 1 / M_PI;
			dpower[0] += intensitys[3 * i + 0] * brdf * dot;
			dpower[1] += intensitys[3 * i + 1] * brdf * dot;
			dpower[2] += intensitys[3 * i + 2] * brdf * dot;
		}
	}

	power->f[0] = M_PI * dpower[0];
	power->f[1] = M_PI * dpower[1];
	power->f[2] = M_PI * dpower[2];
}

void
ri_domelight_sample(
	ri_vector_t       *power,
	ri_hemisphere_t   *hemi,
	int                nsamples,
	const ri_ray_t    *inray,
	const ri_vector_t *pos,
	const ri_vector_t *eye,
	const ri_light_t  *light)
{
	int i, j, k;
	int hit;
	//int count;
	ri_float_t dpower[3];
	ri_float_t theta, phi;
	ri_float_t brdf;
	ri_float_t  u, v;
	ri_float_t *samples;
	//ri_float_t *samplepoints;
	ri_vector_t rad;
	ri_vector_t dir;
	ri_ray_t r;
	ri_intersection_state_t state;
	ri_option_t *opt;

	opt = ri_render_get()->context->option;

	(void)eye;

	dpower[0] = dpower[1] = dpower[2] = 0.0;
	ri_ray_copy(&r, inray);
	ri_vector_copy(&(r.org), pos);
	ri_vector_zero(&rad);

	ri_ortho_basis(hemi->basis, &hemi->basis[2]);

	if (opt->use_qmc) {	/* quasi-Monte Carlo sampling */

		samples = (ri_float_t *)ri_mem_alloc(sizeof(ri_float_t) * nsamples * 2);



#if 0 
		u = randomMT();
		v = randomMT();
#endif

		for (i = 0; i < nsamples; i++) {
			samples[2 * i + 0] = generalized_scrambled_hammersley(
						i, 0, nsamples, 1,
						ri_render_get()->perm_table);
			samples[2 * i + 1] = generalized_scrambled_hammersley(
						i, 0, nsamples, 2,
						ri_render_get()->perm_table);

#if 1
			u = generalized_scrambled_halton(
						inray->i, 0, inray->d,
						ri_render_get()->perm_table);
			v = generalized_scrambled_halton(
						inray->i, 0, inray->d+1,
						ri_render_get()->perm_table);

#endif

			samples[2 * i + 0] = mod_1(u + samples[2 * i + 0]);
			samples[2 * i + 1] = mod_1(u + samples[2 * i + 1]);
		}

		for (i = 0; i < nsamples; i++) {
			theta = sqrt(samples[2 * i + 0]);
			phi = 2.0 * M_PI * samples[2 * i + 1];		
			dir.f[0] = cos(phi) * theta;
			dir.f[1] = sin(phi) * theta;
			dir.f[2] = sqrt(1.0 - theta * theta);

			for (k = 0; k < 3; k++) {
				r.dir.f[k]=dir.f[0]*hemi->basis[0].f[k]
					  +dir.f[1]*hemi->basis[1].f[k]
					  +dir.f[2]*hemi->basis[2].f[k];
			}

			ri_vector_normalize3(&(r.dir));

			hit = ri_raytrace(ri_render_get(),
					  &r, &state);

			if (!hit) {
				ri_vector_copy(&rad, &light->col);
				ri_vector_scale(&rad, &rad, (ri_float_t)light->intensity);

				/* lambert */
				brdf = (ri_float_t)1.0 / M_PI;

				dpower[0] += rad.f[0] * brdf;
				dpower[1] += rad.f[1] * brdf;
				dpower[2] += rad.f[2] * brdf;
			}
		}

		power->f[0] = M_PI * dpower[0] / (ri_float_t)nsamples;
		power->f[1] = M_PI * dpower[1] / (ri_float_t)nsamples;
		power->f[2] = M_PI * dpower[2] / (ri_float_t)nsamples;

		ri_mem_free(samples);

	} else { /* Monte Carlo sampling, */

		/* theta * phi = total samples.
		 * phi = 3 * theta.
		 */
		hemi->ntheta = nsamples / 3.0;
		hemi->ntheta = (int)sqrt((ri_float_t)hemi->ntheta);
		if (hemi->ntheta < 1) hemi->ntheta = 1;
		if (hemi->ntheta > MAX_HEMISAMPLE) {
			hemi->ntheta = MAX_HEMISAMPLE;
		}
		hemi->nphi   = 3 * hemi->ntheta;


		for (j = 0; j < (int)hemi->nphi; j++) {
			for (i = 0; i < (int)hemi->ntheta; i++) {
				theta = sqrt(((ri_float_t)i + randomMT()) /
					hemi->ntheta);
				phi = 2.0 * M_PI * ((ri_float_t)j + randomMT()) /
				      hemi->nphi;		
				dir.f[0] = cos(phi) * theta;
				dir.f[1] = sin(phi) * theta;
				dir.f[2] = sqrt(1.0 - theta * theta);

				for (k = 0; k < 3; k++) {
					r.dir.f[k]=dir.f[0]*hemi->basis[0].f[k]
						  +dir.f[1]*hemi->basis[1].f[k]
						  +dir.f[2]*hemi->basis[2].f[k];
				}

				ri_vector_normalize3(&(r.dir));

				hit = ri_raytrace(ri_render_get(),
						  &r, &state);

				if (!hit) {
					ri_vector_copy(&rad, &light->col);
					ri_vector_scale(&rad, &rad, light->intensity);

					/* lambert */
					brdf = 1.0 / M_PI;

					dpower[0] += rad.f[0] * brdf;
					dpower[1] += rad.f[1] * brdf;
					dpower[2] += rad.f[2] * brdf;
				}
			}
		}

		power->f[0] = M_PI * dpower[0] / (ri_float_t)(nsamples);
		power->f[1] = M_PI * dpower[1] / (ri_float_t)(nsamples);
		power->f[2] = M_PI * dpower[2] / (ri_float_t)(nsamples);
	}
}

/*
 * perform exact hemisphere integration.
 */
void
ri_ibl_sample_bruteforce(
	ri_vector_t       *power,
	ri_hemisphere_t   *hemi,
	int                nsamples,
	const ri_vector_t *pos,
	const ri_vector_t *eye,
	const ri_light_t  *light)
{
	const ri_float_t scaling = 1.0;
	int i, j;
	int hit;
	int width;
	ri_float_t invdist;
	ri_float_t dpower[3];
	ri_float_t theta, phi;
	ri_float_t brdf;
	ri_float_t u,v,r,x,y,z,domega ;
	ri_float_t dot;
	ri_vector_t rad;
	ri_vector_t dist;
	ri_ray_t ray;
	ri_intersection_state_t state;

	(void)eye;
	(void)nsamples;

	if ((light->type != LIGHTTYPE_IBL) && !light->texture) {
		ri_vector_zero(power);
		return;
	}

	dpower[0] = dpower[1] = dpower[2] = 0.0;

	ri_vector_copy(&(ray.org), pos);

	ri_vector_zero(&rad);

	/* we assume input IBL texture is angular map. */
	if (light->texture->width != light->texture->height) {
		printf("texture width != texture height.\n");
		ri_vector_zero(power);
		return;
	}

	width = light->texture->width;

	/* codes from Ravi Ramamoorthi's prefilter.c */

	ri_ortho_basis(hemi->basis, &hemi->basis[2]);

	for (i = 0 ; i < width ; i++) {
		for (j = 0 ; j < width ; j++) {

			/* We now find the cartesian components
			 *for the point (i,j) */

			/* v ranges from -1 to 1 */
			v = ((ri_float_t)width/2.0 - (ri_float_t)i)/((ri_float_t)width/2.0);
			/* u ranges from -1 to 1 */
			u = ((ri_float_t)j-(ri_float_t)width/2.0)/((ri_float_t)width/2.0);
			r = sqrt(u*u+v*v) ;               /* The "radius" */
			/* Consider only circle with r<1 */
			if (r > 1.0) continue ;

			/* theta parameter of (i,j) */
			theta = M_PI*r ;
			/* phi parameter */
			phi = atan2(v,u) ;

			/* Cartesian components */
			x = sin(theta)*cos(phi) ;
			y = sin(theta)*sin(phi) ;
			z = cos(theta) ;

			ray.dir.f[0]  = x;
			ray.dir.f[1]  = y;
			ray.dir.f[2]  = z;

			dot = ri_vector_dot3(&ray.dir, &hemi->basis[2]);
			if (dot < 0.0) continue;

		        /* Computation of the solid angle.  This follows from
		         * some elementary calculus converting
		         * sin(theta) d theta d phi into coordinates in terms
		         * of r.  This calculation should be redone 
		         * if the form of the input changes
		         */

		        domega = (2.0*M_PI/width)*(2.0*M_PI/width)*sinc(theta) ;
		        //domega = (2.0*M_PI/width)*(2.0*M_PI/width)*sinc(acos(y)) ;


			hit = ri_raytrace(ri_render_get(), &ray, &state);

			if (!hit) {
				ri_texture_ibl_fetch(&rad,
						     light->texture,
						     &ray.dir);

				ri_vector_scale(&ray.dir, &ray.dir, scaling);
				ri_vector_scale(&ray.org, &ray.dir, scaling);
				ri_vector_sub(&dist, &ray.dir, &ray.org);
				invdist = ri_vector_length3(&dist);

				if (invdist != 0.0) {
					invdist = 1.0 / (invdist * invdist);
				}
				//invdist = 1.0;

				/* lambert */
				brdf = 1.0 / M_PI;

				dpower[0] += rad.f[0] * brdf * dot * domega * invdist;
				dpower[1] += rad.f[1] * brdf * dot * domega * invdist;
				dpower[2] += rad.f[2] * brdf * dot * domega * invdist;
			}
		}
	}

	power->f[0] = dpower[0];
	power->f[1] = dpower[1];
	power->f[2] = dpower[2];
}


/* --- private functions --- */

static int
inunitsphere(int x, int y, int w, int h)
{
	ri_float_t u, v;
	ri_float_t r;

	v = (y - h / 2.0) / (h / 2.0);
	u = (x - w / 2.0) / (w / 2.0);

	r = sqrt(u * u + v * v);

	if (r > 1.0) return 0;

	return 1;
}

static void
xy_to_vec(ri_vector_t *vec, int x, int y, int w, int h)
{
	ri_float_t u, v;
	ri_float_t r;
	ri_float_t theta, phi;

	v = ((ri_float_t)y - (ri_float_t)h / 2.0) / ((ri_float_t)h / 2.0);
	//v = ((ri_float_t)h / 2.0 - (ri_float_t)y) / ((ri_float_t)h / 2.0);
	u = ((ri_float_t)x - (ri_float_t)w / 2.0) / ((ri_float_t)w / 2.0);

	r = sqrt(u * u + v * v);

	theta = M_PI * r;
	phi   = atan2(v, u);

	vec->f[0] = (ri_float_t)(sin(theta) * cos(phi));
	vec->f[1] = (ri_float_t)(sin(theta) * sin(phi));
	vec->f[2] = (ri_float_t)(cos(theta));
}

/* RGB -> YCbCr(use Y only) */
static ri_float_t
rgb_to_intensity(ri_float_t r, ri_float_t g, ri_float_t b)
{
	return (0.2989 * r + 0.5866 * g + 0.1145 * b);
}

static ri_float_t
sinc(ri_float_t x)
{
	if (fabs(x) < 1.0e-6) return 1.0;
	
	return sin(x) / x;
}

/*
 * For details, see:
 * Alexander Keller,
 * "Strictly deterministic sampling methods in computer graphics"
 * (mental images technical report, 2001)
 *       in "Monte Carlo Ray Tracing", SIGGRAPH'2003 Course #44.
 */
static void
gen_qmc_samples(ri_float_t *samples, int nsamples, int instancenum, int **perm)
{
#if 1
	int    i;
	ri_float_t u[2];

	for (i = 0; i < nsamples; i++) {
		//samples[2 * i + 0] = generalized_scrambled_hammersley(
		//			i, instancenum, nsamples, 1,
		//			perm);
		//samples[2 * i + 1] = generalized_scrambled_hammersley(
		//			i, instancenum, nsamples, 2,
		//			perm);
		samples[2 * i + 0] = generalized_scrambled_halton(
					i, instancenum, 1,
					perm);
		samples[2 * i + 1] = generalized_scrambled_halton(
					i, instancenum, 2,
					perm);

#if 0
		u[0] = generalized_scrambled_halton(
					i, instancenum, 3,
					perm);
		u[1] = generalized_scrambled_halton(
					i, instancenum, 4,
					perm);
#endif
		u[0] = 0.0; u[1] = 0.0;

		samples[2 * i + 0] = mod_1(u[0] + samples[2 * i + 0]);
		samples[2 * i + 1] = mod_1(u[1] + samples[2 * i + 1]);
	}

#else
	(void)nsamples;
	(void)instancenum;
	(void)perm;

	fibonacci_lattice_2D(samples, 10);
#endif
}
