/*
 * $Id: bssrdf.c,v 1.5 2004/04/16 13:46:45 syoyo Exp $
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <math.h>

#include "array.h"
#include "memory.h"
#include "log.h"
#include "render.h"
#include "random.h"
#include "bssrdf.h"
#include "raytrace.h"
#include "photonmap.h"
#include "octree.h"
#include "irradcache.h"
#include "reflection.h"

#ifndef M_PI
#define M_PI 3.1415926535
#endif

#define MAX_LEVEL 8

/* data used for SIGGRAPH2002's method. */
typedef struct _sig2002_node_data_t
{
	ri_vector_t irrad;	/* total irradiance.			  */
	ri_vector_t p;		/* average location of the sample points. */
	double      area;	/* total area.				  */
	ri_array_t  *idarray;	/* list of point's ID included its node.  */

	// for our method;
	int         code;	/* octree code.				*/
	int         voxel[3];
	ri_vector_t center;	/* center of cell position		*/
	ri_vector_t localirrad;
	ri_vector_t farirrad;
	ri_vector_t accumlocal;
	int debug;
} sig2002_node_data_t;

/* data array used for our method. */
static sig2002_node_data_t *fmmcell[MAX_LEVEL];


static void direct_lighting(ri_vector_t *power,		/* light power */	
			    ri_light_t *light,
			    const ri_ray_t *v, int nsample);
static void indirect_lighting(ri_vector_t *power,	/* irradiance power */	
			      ri_photonmap_t *photonmap,
			      ri_octree_t *irradcachetree,
			      const ri_ray_t *v,
			      int nsample, float gather_radius, int gather_num,
			      int fixcache);
static void calc_bbox(const ri_vector_t *positions, unsigned int n, 
		      ri_vector_t *min, ri_vector_t *max);
static double calc_area(const ri_vector_t *v0,
			const ri_vector_t *v1,
			const ri_vector_t *v2);
static void calc_normal(ri_vector_t *n,
			const ri_vector_t *v0,
			const ri_vector_t *v1,
			const ri_vector_t *v2);
static void lerp_normal(ri_vector_t *n,
			const ri_vector_t *n0,
			const ri_vector_t *n1,
			const ri_vector_t *n2,
			float u, float v);
static void calc_irradiance(ri_vector_t *irrad,
			    const ri_vector_t *p, const ri_vector_t *n,
			    ri_light_t *light, ri_photonmap_t *photonmap);

#if 0
static void build_fmmtree(ri_bssrdf_t *bssrdf);

static void build_fmmtree_recursive(ri_octree_t *tree,
				    ri_bssrdf_t *bssrdf);
#endif
static void subdiv_fmmtree_recursive(ri_octree_t *tree,
				     ri_bssrdf_t *bssrdf);
static void subdiv_octree(ri_octree_t *octree);
static int  within_node(ri_octree_t *octree, const ri_vector_t *pi);
//static double bssrdf_rd();
static double f_dr();
static double f_t(const ri_vector_t *w, const ri_vector_t *n);

static void build_sig2002tree(ri_bssrdf_t *bssrdf);
static void subdiv_sig2002tree_recursive(ri_octree_t *tree,
				        ri_bssrdf_t *bssrdf);
static void build_sig2002tree_recursive(ri_octree_t *tree,
				        ri_bssrdf_t *bssrdf);

static double delta_w(const ri_vector_t *x, double a, const ri_vector_t *p);

/* potential function of diffuse BSSRDF */
//static double potfunc();
static double potfunc_sig2002(const ri_vector_t *xi, const ri_vector_t *xo);
static void   eval_sig2002_traverse(ri_octree_t *tree,
				    ri_bssrdf_t *bssrdf,
				    ri_vector_t *m, const ri_vector_t *x);
#if 0
static void   dump_htree(ri_octree_t *tree);
static void   dump_fmmtree(ri_octree_t *tree);
static void   dump_fmmtree_rec(FILE *fp, ri_octree_t *tree);
static void   calc_farpower(ri_bssrdf_t *bssrdf, int level);
static void   gather_farpower(ri_bssrdf_t *bssrdf, int level, int x, int y, int z,
			      int xl, int xr, int yl, int yr, int zl, int zr);
static void   gather_nearpower(ri_bssrdf_t *bssrdf, int level, int x, int y, int z);
#endif
static void   gather_nearcell(ri_bssrdf_t *bssrdf, int level, int x, int y, int z, ri_vector_t *m, const ri_vector_t *xpos);
static void   eval_fmm_traverse(ri_octree_t *tree,
				ri_bssrdf_t *bssrdf,
				ri_vector_t *m, const ri_vector_t *xpos);

//static void   calc_localpower(ri_octree_t *tree, ri_bssrdf_t *bssrdf);
static void   calc_localpower_traverse(ri_octree_t *tree, ri_bssrdf_t *bssrdf);
static void   calc_xyz(int cell[3], int level,
		       const ri_vector_t *p, const ri_vector_t *center,
		       double width);
static void   write_cache(ri_bssrdf_t *bssrdf);
static int    read_cache(ri_bssrdf_t *bssrdf);
static void   calc_hemibasis(ri_hemisphere_t *hemi, const ri_vector_t *normal);


ri_bssrdf_t *
ri_bssrdf_new()
{
	ri_bssrdf_t *p = NULL;

	p = (ri_bssrdf_t *)ri_mem_alloc(sizeof(ri_bssrdf_t));

	/*
	 * set default value
	 */
	p->nsamples    = 0;
	p->positions   = NULL;
	p->normals     = NULL;
	p->irradiances = NULL;
	p->fmmtree     = NULL;
	p->max_treelevel = ri_render_get()->context->option->bssrdf_tree_level;
	p->area          = 0.0;

	p->htree       = NULL;

	return p;
}

void
ri_bssrdf_free(ri_bssrdf_t *bssrdf)
{
	ri_octree_free(bssrdf->fmmtree);
	ri_mem_free(bssrdf->positions);
	ri_mem_free(bssrdf->normals);
	ri_mem_free(bssrdf->irradiances);
	ri_mem_free(bssrdf);
}

void
ri_bssrdf_generate_samples(ri_bssrdf_t *bssrdf,
			   ri_geom_t *geom,
			   ri_light_t *light,
			   ri_photonmap_t *photonmap)
{
	//const int ngen = 10000;
	//FILE *fp;
	ri_array_t *density;
	unsigned int i, j;
	unsigned int idx;
	unsigned int i0, i1, i2;
	double r, e;
	double s, t;
	double area;
	double areasum;
	int    *counts;
	int     gencache;
	ri_vector_t *v0, *v1, *v2;
	ri_vector_t *n0, *n1, *n2;
	ri_vector_t pos;

	if (geom == NULL) return;

	gencache = 0;
	if (read_cache(bssrdf)) {
		/* build FMM tree. */
		//build_fmmtree(bssrdf);
		build_sig2002tree(bssrdf);
		return;		
	} else {
		gencache = 1;
	}

	density = ri_array_new(sizeof(double));
	
	areasum = 0.0;

	counts = (int *)ri_mem_alloc(sizeof(int) * (geom->nindices / 3));

	for (i = 0; i < geom->nindices / 3; i++) {
		counts[i] = 0;
	}

	for (i = 0; i < geom->nindices / 3; i++) {
		v0 = &(geom->positions[geom->indices[i * 3 + 0]]);
		v1 = &(geom->positions[geom->indices[i * 3 + 1]]);
		v2 = &(geom->positions[geom->indices[i * 3 + 2]]);

		area = calc_area(v0, v1, v2);
		areasum += area;

		ri_array_insert(density, i, (void *)&areasum);

	}

	
	bssrdf->nsamples = ri_render_get()->context->option->bssrdf_nsamples;
	bssrdf->positions = (ri_vector_t *)ri_mem_alloc(sizeof(ri_vector_t) *
							bssrdf->nsamples);
	bssrdf->normals = (ri_vector_t *)ri_mem_alloc(sizeof(ri_vector_t) *
						      bssrdf->nsamples);
	bssrdf->irradiances = (ri_vector_t *)ri_mem_alloc(sizeof(ri_vector_t) *
							  bssrdf->nsamples);
	bssrdf->area = ri_geom_area(geom);

	/* generate random points onto surface of the polygon model */
	for (i = 0; i < bssrdf->nsamples; i++) {
		/* pick random triangle index weighted by area */
		r = randomMT() * areasum;

		idx = density->nelems - 1;
		for (j = 0; j < density->nelems; j++) {
			e = *(double *)ri_array_at(density, j);
			if (r < e) {
				idx = j;
				break;
			}
		}

		counts[idx]++;

		i0 = idx * 3 + 0;
		i1 = idx * 3 + 1;
		i2 = idx * 3 + 2;

		v0 = &(geom->positions[geom->indices[i0]]);
		v1 = &(geom->positions[geom->indices[i1]]);
		v2 = &(geom->positions[geom->indices[i2]]);

		/* generate random points on the triangle. */

		s = sqrt(randomMT());
		t = randomMT();

		/*
		 * pos = v0(1.0 - s) + v1(s - t * s) + v2 * s * t
		 */
		pos.e[0] = v0->e[0]*(1.0-s) + v1->e[0]*(s-t*s) + v2->e[0]*s*t;
		pos.e[1] = v0->e[1]*(1.0-s) + v1->e[1]*(s-t*s) + v2->e[1]*s*t;
		pos.e[2] = v0->e[2]*(1.0-s) + v1->e[2]*(s-t*s) + v2->e[2]*s*t;
		pos.e[3] = 1.0f;

		ri_vector_copy(&(bssrdf->positions[i]), &pos);

		if (geom->normals) {
			n0 = &(geom->normals[i0]);
			n1 = &(geom->normals[i1]);
			n2 = &(geom->normals[i2]);

			lerp_normal(&(bssrdf->normals[i]), n0, n1, n2, s, t);
		} else {
			calc_normal(&(bssrdf->normals[i]), v0, v1, v2);
		}

		/* calculate irradiance */
		calc_irradiance(&(bssrdf->irradiances[i]),
				&(bssrdf->positions[i]),
				&(bssrdf->normals[i]),
				light,
				photonmap);

		if ((i % 1000) == 0) {
			printf("generated %d samples.\n", i);
		}
	}


	if (gencache) {
		write_cache(bssrdf);
	}
	
	
	/* build FMM tree. */
	//build_fmmtree(bssrdf);

	/* build tree */
	build_sig2002tree(bssrdf);

	//dump_fmmtree(bssrdf->fmmtree);
	
	ri_mem_free(counts);

}
	
/* --- private functions --- */

static double
calc_area(const ri_vector_t *v0, const ri_vector_t *v1, const ri_vector_t *v2)
{
	double area;
	ri_vector_t v01, v02;
	ri_vector_t cross;

	ri_vector_sub(&v01, v1, v0);
	ri_vector_sub(&v02, v2, v0);
	ri_vector_cross3(&cross, &v01, &v02);

	area = ri_vector_length(&cross) * 0.5;

	return area;
}

static void
direct_lighting(ri_vector_t *power, ri_light_t *light,
		const ri_ray_t *v, int nsample)
{
	int         i;
	int         hit, lighthit;
	ri_vector_t lightpos;
	ri_vector_t lightnormal;
	ri_vector_t lightvec;
	ri_ray_t    ray;
	float       dot;
	float       t;
	double      dpower[3];
	ri_surface_info_t surfinfo, tmpinfo;
	int         count;
	
	dpower[0] = dpower[1] = dpower[2] = 0.0;

	if (light->geom) {	/* area light	*/

		count = 0;
		for (i = 0; i < nsample; i++) {
			ri_light_sample_pos_and_normal(light,
						       &lightpos,
						       &lightnormal);
			ri_vector_sub(&lightvec, &lightpos, &(v->org));
			ri_vector_normalize(&lightvec);

			ri_vector_copy(&(ray.org), &(v->org));
			ri_vector_copy(&(ray.dir), &lightvec);

			/* occlusion test */
			hit = ri_raytrace(ri_render_get(), &ray, &surfinfo);
			if (hit) {

				t = ray.isectt;

				/* trace against area light geometry */
				lighthit = ri_raytrace_geom(light->geom,
							    &ray,
							    &tmpinfo);			

				if (lighthit) {
					if (ray.isectt > t ||
					    ray.isectt < 0.0) {
						/* there is occluder
						 * between surface and area
						 * light.
						 */

					} else {
						/* currently, diffuse lighting only */
						dot = ri_vector_dot3(&(ray.dir),
								     &(v->dir));
						if (dot > 0.0) {
							dpower[0] += light->intensity * light->col.e[0] * dot;
							dpower[1] += light->intensity * light->col.e[1] * dot;
							dpower[2] += light->intensity * light->col.e[2] * dot;
						}
					}
					
				}
			} else {
				/* currently, diffuse lighting only */
				dot = ri_vector_dot3(&(ray.dir), &(v->dir));
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

	} else {		/* point light	*/

		ri_vector_sub(&lightvec, &(light->pos), &(v->org));
		ri_vector_normalize(&lightvec);

		ri_vector_copy(&(ray.org), &(v->org));
		ri_vector_copy(&(ray.dir), &lightvec);

		/* occlusion test */
		hit = ri_raytrace(ri_render_get(), &ray, &surfinfo);
		if (hit) {
			/* currently, diffuse lighting only */
			dot = ri_vector_dot3(&ray.dir, &(v->dir));
			if (dot > 0.0) {
				dpower[0] += light->intensity * light->col.e[0] * dot;
				dpower[1] += light->intensity * light->col.e[1] * dot;
				dpower[2] += light->intensity * light->col.e[2] * dot;
			}

		}
	}

	power->e[0] = dpower[0];
	power->e[1] = dpower[1];
	power->e[2] = dpower[2];

}

static void
indirect_lighting(ri_vector_t *power,	/* irradiance power */	
		  ri_photonmap_t *photonmap,
		  ri_octree_t *irradcachetree,
		  const ri_ray_t *v,
		  int nsample, float gather_radius, int gather_num,
		  int fixicache)
{
	int i, j, k;
	int hit;
	int nfound;
	int count;
	float dot;
	double dpower[3];
	double weight;
	double theta, phi;
	double ri;
	ri_vector_t irrad;
	ri_vector_t dir;
	ri_ray_t ray;
	ri_surface_info_t surfinfo;
	ri_irradcache_t val;
	ri_hemisphere_t hemi;
	ri_option_t *opt;
	

	dpower[0] = dpower[1] = dpower[2] = 0.0;

	ri_vector_copy(&(ray.org), &(v->org));

	nfound = 0;

	ri_vector_zero(&irrad);

	opt = ri_render_get()->context->option;

	if (opt->enable_irradcache) {

		/* First, check irradiance cache. */
		weight = ri_irradcache_find(irradcachetree,
					    &(v->org), &(v->dir),
					    &irrad, &nfound);

		if (nfound) {
			/* cache found. Reuse cached irradiance. */
			ri_vector_scale(&irrad, 1.0f / weight);
			ri_vector_copy(power, &irrad);
			return;
		}

		if (fixicache) {
			//ri_log("warning", "no icache found.");
			ri_vector_zero(power);
			return;
		}

	}

	/* theta * phi = total samples.
	 * phi = 3 * theta.
	 */
	hemi.ntheta = nsample / 3.0;
	hemi.ntheta = (int)sqrt((double)hemi.ntheta);
	if (hemi.ntheta < 1) hemi.ntheta = 1;
	if (hemi.ntheta > MAX_HEMISAMPLE) hemi.ntheta = MAX_HEMISAMPLE;
	hemi.nphi   = 3 * hemi.ntheta;

	/* calculate irradiance. */
	ri = 0.0;
	count = 0;

	calc_hemibasis(&hemi, &(v->dir));

	for (j = 0; j < (int)hemi.nphi; j++) {
		for (i = 0; i < (int)hemi.ntheta; i++) {
			theta = sqrt(((double)i + randomMT()) / hemi.ntheta);
			phi = 2.0 * M_PI * ((double)j + randomMT()) / hemi.nphi;		
			dir.e[0] = cos(phi) * theta;
			dir.e[1] = sin(phi) * theta;
			dir.e[2] = sqrt(1.0 - theta * theta);

			for (k = 0; k < 3; k++) {
				ray.dir.e[k]  = dir.e[0] * hemi.basis[0].e[k]
					      + dir.e[1] * hemi.basis[1].e[k]
					      + dir.e[2] * hemi.basis[2].e[k];
			}
	
			ri_vector_normalize(&(ray.dir));
			hit = ri_raytrace(ri_render_get(), &ray, &surfinfo);

			if (hit) {
				ri_photonmap_estimate_irradiance(photonmap,
								 &irrad,
								 &surfinfo.pos,
								 &surfinfo.normal,
								 gather_radius,
								 gather_num);

				/* convert irradiance to radiance(diffuse reflection) */

				/* P = Irrad * diffuse_color * cos(-Rd, N) */
				ri_vector_neg(&(ray.dir));
				dot = ri_vector_dot3(&ray.dir, &surfinfo.normal);
				if (dot > 0.0) {
					ri_vector_mul(&irrad,
						      &irrad, &surfinfo.color);
					ri_vector_scale(&irrad, dot);
					dpower[0] += irrad.e[0];
					dpower[1] += irrad.e[1];
					dpower[2] += irrad.e[2];
					count++;

					/* r = 1.0 / distance */
					hemi.samples[i][j].r = 1.0 / ray.isectt;
				} else {
					ri_vector_zero(&irrad);
					hemi.samples[i][j].r = 0.0;
				}
					
			
				ri_vector_copy(&(hemi.samples[i][j].L), &irrad);

			} else {
				hemi.samples[i][j].r = 0.0;
				ri_vector_zero(&(hemi.samples[i][j].L));
			}

			/* radius of irradiance cache value. */ 
			ri += hemi.samples[i][j].r;
		}
	}

	power->e[0] = dpower[0] / (double)(hemi.ntheta * hemi.nphi);
	power->e[1] = dpower[1] / (double)(hemi.ntheta * hemi.nphi);
	power->e[2] = dpower[2] / (double)(hemi.ntheta * hemi.nphi);

	if (opt->enable_irradcache) {
		if (ri < 1.0e-6) {
			/* purely dark region.
			 * no sample ray hits anything.
			 * but irradiance cache requires dark color cache point.
			 */  
			fprintf(stderr, "dark point.\n");
			ri = 25.0;
		} else if (ri > 0.0) {
			ri = (double)count / ri;
			//ri = (double)(hemi.ntheta * hemi.nphi) / ri;
		}
		ri_irradcache_calc_gradient(&(val.tg), &(val.rg), &hemi);
		/* reduce radius if gradient is large */
		dot = ri_vector_dot3(&val.tg, &val.tg);
		if (dot * ri * ri > 1.0) {
			ri = 1.0 / sqrt(dot);
		}

		if (ri < 0.01) {
			ri = 0.01;
			if (dot * ri * ri > 1.0) { /* cap gradient */
				dot = 1.0 / ri / sqrt(dot);

				//ri_vector_scale(&(val.tg), dot);
			}

		}
		

		if (ri > 25.0)  ri = 25.0;

		ri_vector_copy(&(val.p), &(v->org));
		ri_vector_copy(&(val.n), &(v->dir));
		ri_vector_copy(&(val.e), power);
		val.r = ri;

		if (!ri_irradcache_insert(irradcachetree, &val)) {
			// for test
			//power->e[0] = 100; power->e[1] = power->e[2] = 0.0;
		}
	}	
}

static void
calc_normal(ri_vector_t *n,
	    const ri_vector_t *v0, const ri_vector_t *v1, const ri_vector_t *v2)
{
	ri_vector_t v01, v02;

	ri_vector_sub(&v01, v1, v0);
	ri_vector_sub(&v02, v2, v0);
	ri_vector_cross3(n, &v01, &v02);
	ri_vector_normalize(n);
}

static void
lerp_normal(ri_vector_t *n,
	    const ri_vector_t *n0, const ri_vector_t *n1, const ri_vector_t *n2,
	    float u, float v)
{
	ri_vector_t ns0, ns1, ns2;

	/* n = (1 - u - v) n0 + u n1 + v n2 */

	ri_vector_copy(&ns0, n0);
	ri_vector_scale(&ns0, (1.0 - u - v));
	ri_vector_copy(&ns1, n1);
	ri_vector_scale(&ns1, u);
	ri_vector_copy(&ns2, n2);
	ri_vector_scale(&ns2, v);

	ri_vector_add(n, &ns0, &ns1);
	ri_vector_add(n, n, &ns2);
}

static void
calc_irradiance(ri_vector_t *irrad,
		const ri_vector_t *p, const ri_vector_t *n, 
		ri_light_t *light, ri_photonmap_t *photonmap)
{
	int fixicache = 0;
	ri_ray_t surfray;
	ri_vector_t indirectrad, directrad;
	ri_option_t *opt;
	ri_photonmap_option_t *pmapopt;
	ri_octree_t *irradcache;

	opt     = ri_render_get()->context->option;
	pmapopt = ri_photonmap_get_option();

	irradcache = ri_render_get()->irradiance_cache;

	ri_vector_copy(&(surfray.org), p);
	ri_vector_copy(&(surfray.dir), n);

	indirect_lighting(&indirectrad,
			  photonmap,
			  irradcache,
			  &surfray,
			  opt->nfinalgather_rays,
			  pmapopt->max_gather_radius,
			  pmapopt->max_gather_photons,
			  fixicache);

	direct_lighting(&directrad,
			light,
			&surfray,
			opt->narealight_rays);

	ri_vector_add(irrad, &directrad, &indirectrad);
	//ri_vector_copy(irrad, indirectrad);
	
#if 0
	irrad->e[0] *= surfinfo->color.e[0];
	irrad->e[1] *= surfinfo->color.e[1];
	irrad->e[2] *= surfinfo->color.e[2];
#endif

}

static void
calc_bbox(const ri_vector_t *positions, unsigned int n, 
	 ri_vector_t *min, ri_vector_t *max)
{
	int i;

	assert(n > 1);
	assert(positions != NULL);
	assert(min       != NULL);
	assert(max       != NULL);

	min->e[0] = positions[0].e[0];
	min->e[1] = positions[0].e[1];
	min->e[2] = positions[0].e[2];

	max->e[0] = positions[0].e[0];
	max->e[1] = positions[0].e[1];
	max->e[2] = positions[0].e[2];
	
	for (i = 1; i < (int)n; i++) {
		if (positions[i].e[0] < min->e[0]) min->e[0]=positions[i].e[0]; 
		if (positions[i].e[1] < min->e[1]) min->e[1]=positions[i].e[1]; 
		if (positions[i].e[2] < min->e[2]) min->e[2]=positions[i].e[2]; 

		if (positions[i].e[0] > max->e[0]) max->e[0]=positions[i].e[0]; 
		if (positions[i].e[1] > max->e[1]) max->e[1]=positions[i].e[1]; 
		if (positions[i].e[2] > max->e[2]) max->e[2]=positions[i].e[2]; 
	}

	printf("min = (%f, %f, %f)\n", min->e[0], min->e[1], min->e[2]);
	printf("max = (%f, %f, %f)\n", max->e[0], max->e[1], max->e[2]);

}

#if 0
static void
build_fmmtree(ri_bssrdf_t *bssrdf)
{
	unsigned int i, j;
	int size;
	ri_vector_t min, max;
	ri_vector_t center;
	double width;
	double len[3];
	double val;
	sig2002_node_data_t *nodedata;
	

	/* initialize data */
	for (i = 0; i <= (unsigned int)bssrdf->max_treelevel; i++) {
		size = pow(8, i);
		fmmcell[i] = (sig2002_node_data_t *)
				ri_mem_alloc(sizeof(sig2002_node_data_t) * size);
		for (j = 0; j < (unsigned int)size; j++) {
			fmmcell[i][j].idarray = ri_array_new(sizeof(unsigned int));
			fmmcell[i][j].debug = 0;
		}
	}

	//return;

	calc_bbox(bssrdf->positions, bssrdf->nsamples, &min, &max);

	bssrdf->fmmtree = ri_octree_new();

	/* build uniform width bounding box */
	center.e[0] = 1.1 * (min.e[0] + max.e[0]) * 0.5;
	center.e[1] = 1.1 * (min.e[1] + max.e[1]) * 0.5;
	center.e[2] = 1.1 * (min.e[2] + max.e[2]) * 0.5;

	len[0] = max.e[0] - min.e[0];
	len[1] = max.e[1] - min.e[1];
	len[2] = max.e[2] - min.e[2];

	val = len[0];
	if (val < len[1]) val = len[1];
	if (val < len[2]) val = len[2];

	width = val * 0.5;

	printf("fmm root center = (%f, %f, %f)\n", center.e[0], center.e[1], center.e[2]);

	/*
	 * level zero.
	 * all samples are included.
	 */
	bssrdf->fmmtree->width = width;
	ri_vector_copy(&(bssrdf->fmmtree->center), &center);
	bssrdf->fmmtree->depth = 0;

	nodedata = &fmmcell[0][0];

	for (i = 0; i < bssrdf->nsamples; i++) {
		ri_array_insert(nodedata->idarray, i, (void *)&i);
	}

	ri_vector_copy(&(nodedata->center), &center);
	ri_vector_zero(&(nodedata->accumlocal));

	bssrdf->fmmtree->data = (void *)nodedata;


	subdiv_fmmtree_recursive(bssrdf->fmmtree, bssrdf);
	build_fmmtree_recursive(bssrdf->fmmtree, bssrdf);

	assert(bssrdf->fmmtree != NULL);

	calc_farpower(bssrdf, bssrdf->max_treelevel);
	calc_localpower(bssrdf->fmmtree, bssrdf);

	assert(bssrdf->fmmtree != NULL);
	
	
}
#endif

static void
subdiv_fmmtree_recursive(ri_octree_t *tree, ri_bssrdf_t *bssrdf)
{
	//ri_vector_t pos;
	//ri_vector_t irrad;
	unsigned int i, id;
	int cell;
	int size;
	int voxel[3];
	int code;
	double w;
	double f_dt;
	double area_p;
	unsigned int npoints;
	sig2002_node_data_t *nodedata;
	sig2002_node_data_t *childnode;

	assert(tree != NULL);
	assert(bssrdf != NULL);

	/* If this node is leaf node, terminate. */
	if (tree->leaf) return;

	nodedata = (sig2002_node_data_t *)tree->data;
	npoints = nodedata->idarray->nelems;

	/* calculate total area, total irradiance, and average position. */
	/* we assume each sample's area is equal. */
	nodedata->area = (bssrdf->area * (double)npoints)
				      / (double)bssrdf->nsamples;
	ri_vector_zero(&(nodedata->irrad));
	ri_vector_copy(&(nodedata->center), &tree->center);
		
	/* If node has maximun level, node is leaf. */
	/* note:
	 * there are nodes which contains no samples.
	 */
	if (tree->depth > bssrdf->max_treelevel - 1) {
		tree->leaf = 1;

		nodedata->area = 0.0;
		ri_vector_zero(&(nodedata->irrad));
		ri_vector_zero(&(nodedata->p));

		npoints = nodedata->idarray->nelems;

		ri_vector_zero(&(nodedata->localirrad));
		f_dt = 1.0 - f_dr();

		area_p = bssrdf->area / (double)bssrdf->nsamples;
		for (i = 0; i < npoints; i++) {
			id = *(unsigned int *)ri_array_at(nodedata->idarray, i);
			w = potfunc_sig2002(&tree->center,
					    &(bssrdf->positions[id]));

			nodedata->localirrad.e[0] += f_dt * w * bssrdf->irradiances[id].e[0] * area_p;
			nodedata->localirrad.e[1] += f_dt * w * bssrdf->irradiances[id].e[1] * area_p;
			nodedata->localirrad.e[2] += f_dt * w * bssrdf->irradiances[id].e[2] * area_p;
		}

		nodedata->area = (bssrdf->area * (double)npoints) / (double)bssrdf->nsamples;

		return;
	}

	subdiv_octree(tree);

	for (i = 0; i < 8; i++) {
		// generate code.
		size = pow(2, tree->child[i]->depth);

		if (tree->depth == 0) {	/* root of tree. */
			tree->child[i]->code = i;
		} else {
			code = pow(8, tree->child[i]->depth) * i	
			     + tree->code;

			tree->child[i]->code = code;
		}

		//childnode = (sig2002_node_data_t *)
		//		ri_mem_alloc(sizeof(sig2002_node_data_t));
		calc_xyz(voxel, tree->child[i]->depth,
			 &(tree->child[i]->center), &(bssrdf->fmmtree->center), 			 bssrdf->fmmtree->width);

		childnode = &fmmcell[tree->child[i]->depth][voxel[2] * size * size + voxel[1] * size + voxel[0]];
		childnode->voxel[0] = voxel[0];
		childnode->voxel[1] = voxel[1];
		childnode->voxel[2] = voxel[2];
		assert(!childnode->debug);

		childnode->debug = 1;

		tree->child[i]->data = (void *)childnode;

	}

	for (i = 0; i < npoints; i++) {
		id = *(unsigned int *)ri_array_at(nodedata->idarray, i);
	
		cell = within_node(tree, &(bssrdf->positions[id]));

		childnode = (sig2002_node_data_t *)
				tree->child[cell]->data;

		ri_array_insert(childnode->idarray,
				childnode->idarray->nelems,
				(void *)&id);
	}

	for (i = 0; i < 8; i++) {
		subdiv_fmmtree_recursive(tree->child[i], bssrdf);
	}
}

#if 0
static void
build_fmmtree_recursive(ri_octree_t *tree, ri_bssrdf_t *bssrdf)
{
	ri_vector_t pos[8];
	//ri_vector_t irrad[8];
	unsigned int i;
	//int cell;
	//unsigned int npoints;
	sig2002_node_data_t *nodedata;
	sig2002_node_data_t *childnode;
	double w;
	double f_dt;

	assert(tree != NULL);
	assert(bssrdf != NULL);

	/* If this node is leaf node, terminate. */
	//if (tree->leaf) return;

	/* first, goto node which has empty or leaf child. */
	for (i = 0; i < 8; i++) {
		if (tree->child[i] && !tree->child[i]->leaf)
			build_sig2002tree_recursive(tree->child[i], bssrdf);
	}

	nodedata = (sig2002_node_data_t *)tree->data;

	nodedata->area = 0.0;
	ri_vector_zero(&(nodedata->localirrad));
	ri_vector_zero(&(nodedata->p));

	f_dt = 1.0 - f_dr();

	for (i = 0; i < 8; i++) {
		childnode = (sig2002_node_data_t *)tree->child[i]->data;


		ri_vector_zero(&pos[i]);

		w = potfunc_sig2002(&(tree->center), &(tree->child[i]->center));

		nodedata->localirrad.e[0] += f_dt * w * childnode->localirrad.e[0] * childnode->area;
		nodedata->localirrad.e[0] += f_dt * w * childnode->localirrad.e[0] * childnode->area;
		nodedata->localirrad.e[0] += f_dt * w * childnode->localirrad.e[0] * childnode->area;
		nodedata->area += childnode->area;
	}

}
#endif

static void
build_sig2002tree(ri_bssrdf_t *bssrdf)
{
	unsigned int i;
	ri_vector_t min, max;
	ri_vector_t center;
	double width;
	double val;
	//ri_array_t *idarray;
	sig2002_node_data_t *nodedata;

	calc_bbox(bssrdf->positions, bssrdf->nsamples, &min, &max);

	bssrdf->htree = ri_octree_new();

	/* build uniform bounding box */
	val = min.e[0];
	if (val > min.e[1]) val = min.e[1];
	if (val > min.e[2]) val = min.e[2];
	min.e[0] = min.e[1] = min.e[2] = val;

	val = max.e[0];
	if (val < max.e[1]) val = max.e[1];
	if (val < max.e[2]) val = max.e[2];
	max.e[0] = max.e[1] = max.e[2] = val;

	width = (max.e[0] - min.e[0]) * 0.5;
	center.e[0] = center.e[1] = center.e[2] = min.e[0] + width;
	center.e[3] = 1.0f;

	printf("max= %f, min = %f\n", max.e[0], min.e[0]);
	printf("width = %f\n", width);
	printf("center = (%f, %f, %f)\n", center.e[0], center.e[1], center.e[2]);
	/*
	 * level zero.
	 * node includes all samples.
	 */
	bssrdf->htree->width = width;
	ri_vector_copy(&(bssrdf->htree->center), &center);
	bssrdf->htree->depth = 0;

	nodedata = (sig2002_node_data_t *)
			ri_mem_alloc(sizeof(sig2002_node_data_t));
	nodedata->idarray = ri_array_new(sizeof(unsigned int));

	for (i = 0; i < bssrdf->nsamples; i++) {
		ri_array_insert(nodedata->idarray, i, (void *)&i);
	}


	bssrdf->htree->data = nodedata;
	
	subdiv_sig2002tree_recursive(bssrdf->htree, bssrdf);
	build_sig2002tree_recursive(bssrdf->htree, bssrdf);

	printf("builded sig2002 tree\n");
}

static void
subdiv_sig2002tree_recursive(ri_octree_t *tree, ri_bssrdf_t *bssrdf)
{
	ri_vector_t pos;
	//ri_vector_t irrad;
	unsigned int i, id;
	int cell;
	unsigned int npoints;
	sig2002_node_data_t *nodedata;
	sig2002_node_data_t *childnode;

	assert(tree != NULL);
	assert(bssrdf != NULL);

	/* If this node is leaf node, terminate. */
	if (tree->leaf) return;

	nodedata = (sig2002_node_data_t *)tree->data;
	npoints = nodedata->idarray->nelems;

	/* calculate total area, total irradiance, and average position. */
	/* we assume each sample's area is equal. */
	nodedata->area = (bssrdf->area * (double)npoints)
				      / (double)bssrdf->nsamples;
	ri_vector_zero(&(nodedata->irrad));
	ri_vector_zero(&(nodedata->p));
		
	/* If samples are less than 8, node is leaf. */
	//if (npoints < 8 || tree->depth > 2) {
	if (npoints < 8) {
		tree->leaf = 1;
		return;
	}

	subdiv_octree(tree);

	for (i = 0; i < 8; i++) {
		childnode = (sig2002_node_data_t *)
				ri_mem_alloc(sizeof(sig2002_node_data_t));
		childnode->idarray =
			ri_array_new(sizeof(unsigned int));
		tree->child[i]->data = (void *)childnode;
	}

	for (i = 0; i < npoints; i++) {
		id = *(unsigned int *)ri_array_at(nodedata->idarray, i);
		pos = bssrdf->positions[id];
	
		cell = within_node(tree, &pos);

		childnode = (sig2002_node_data_t *)
				tree->child[cell]->data;

		ri_array_insert(childnode->idarray,
				childnode->idarray->nelems,
				(void *)&id);
	}

	for (i = 0; i < 8; i++) {
		subdiv_sig2002tree_recursive(tree->child[i], bssrdf);
	}
}

static void
build_sig2002tree_recursive(ri_octree_t *tree, ri_bssrdf_t *bssrdf)
{
	ri_vector_t pos[8];
	ri_vector_t irrad[8];
	unsigned int i, j, k, id;
	//int cell;
	unsigned int npoints;
	sig2002_node_data_t *nodedata;
	sig2002_node_data_t *childnode;
	double w;

	assert(tree != NULL);
	assert(bssrdf != NULL);

	/* If this node is leaf node, terminate. */
	//if (tree->leaf) return;

	/* first, goto node which has empty or leaf child. */
	for (i = 0; i < 8; i++) {
		if (tree->child[i] && !tree->child[i]->leaf)
			build_sig2002tree_recursive(tree->child[i], bssrdf);
	}

	nodedata = (sig2002_node_data_t *)tree->data;

	nodedata->area = 0.0;
	ri_vector_zero(&(nodedata->irrad));
	ri_vector_zero(&(nodedata->p));

	for (i = 0; i < 8; i++) {
		childnode = (sig2002_node_data_t *)tree->child[i]->data;

		ri_vector_zero(&irrad[i]);
		ri_vector_zero(&pos[i]);

		if (tree->child[i]->leaf) {
			for (j = 0; j < childnode->idarray->nelems; j++) {
				id = *(unsigned int *)ri_array_at(childnode->idarray, j);
				ri_vector_add(&irrad[i],
					      &irrad[i],
					      &(bssrdf->irradiances[id]));
			}

			for (j = 0; j < childnode->idarray->nelems; j++) {
				id = *(unsigned int *)ri_array_at(childnode->idarray, j);
				w = 0.0;
				for (k = 0; k < 3; k++) {
					if (irrad[i].e[k] > 1.0e-6) {
						w += bssrdf->irradiances[id].e[k] / irrad[i].e[k];
					}
				} 

				w /= 3.0;
				// hack
				//w = 1.0 / (double)nodedata->idarray->nelems;

				pos[i].e[0] += w * bssrdf->positions[id].e[0];
				pos[i].e[1] += w * bssrdf->positions[id].e[1];
				pos[i].e[2] += w * bssrdf->positions[id].e[2];
			}

			npoints = childnode->idarray->nelems;	
			nodedata->area +=
				(bssrdf->area * (double)npoints)
					      / (double)bssrdf->nsamples;
		} else {
			if (childnode->idarray->nelems == 0) {
				//printf("empty node.\n");
			} else {
				//printf("node voxel.\n");
				ri_vector_copy(&irrad[i], &(childnode->irrad));
				ri_vector_copy(&pos[i], &(childnode->p));
				nodedata->area += childnode->area;
			}
		}
	}

	//printf("level %d:\n", tree->depth);
	for (i = 0; i < 8; i++) {
		childnode = (sig2002_node_data_t *)tree->child[i]->data;
		//printf("child[%d]\n", childnode->idarray->nelems);
		//printf("irrad[%d] = (%f, %f, %f)\n", i, irrad[i].e[0], irrad[i].e[1], irrad[i].e[2]);
		ri_vector_add(&(nodedata->irrad),
			      &(nodedata->irrad), &(irrad[i]));
	}

	for (i = 0; i < 8; i++) {
		w = 0.0;
		for (k = 0; k < 3; k++) {
			if (nodedata->irrad.e[k] > 1.0e-6) {
				w += irrad[i].e[k] / nodedata->irrad.e[k];
			}
		}

		w /= 3.0;

		// hack
		//w = 1.0 / 8.0;

		nodedata->p.e[0] += w * pos[i].e[0];
		nodedata->p.e[1] += w * pos[i].e[1];
		nodedata->p.e[2] += w * pos[i].e[2];
	}
}

static void
subdiv_octree(ri_octree_t *octree)
{
	int i;
	double width;
	ri_vector_t center;

	width = 0.5 * octree->width;

	for (i = 0; i < 8; i++) {
		ri_vector_copy(&center, &(octree->center));
		switch(i) {
			case 0:
				center.e[0] += width;	
				center.e[1] += width;	
				center.e[2] += width;	
				break;
			case 1:
				center.e[0] += width;	
				center.e[1] += width;	
				center.e[2] -= width;	
				break;
			case 2:
				center.e[0] += width;	
				center.e[1] -= width;	
				center.e[2] += width;	
				break;
			case 3:
				center.e[0] += width;	
				center.e[1] -= width;	
				center.e[2] -= width;	
				break;
			case 4:
				center.e[0] -= width;	
				center.e[1] += width;	
				center.e[2] += width;	
				break;
			case 5:
				center.e[0] -= width;	
				center.e[1] += width;	
				center.e[2] -= width;	
				break;
			case 6:
				center.e[0] -= width;	
				center.e[1] -= width;	
				center.e[2] += width;	
				break;
			case 7:
				center.e[0] -= width;	
				center.e[1] -= width;	
				center.e[2] -= width;	
				break;
			default:
				break;
		}
	
		octree->child[i] = ri_octree_new();
		octree->child[i]->width = width;
		octree->child[i]->depth = octree->depth + 1;
		ri_vector_copy(&(octree->child[i]->center), &center);
		
	}
}

static int
within_node(ri_octree_t *octree, const ri_vector_t *pi)
{
	if (pi->e[0] > octree->center.e[0]) {
		if (pi->e[1] > octree->center.e[1]) {
			if (pi->e[2] > octree->center.e[2]) {
				return 0;
			} else {
				return 1;
			}
		} else {
			if (pi->e[2] > octree->center.e[2]) {
				return 2;
			} else {
				return 3;
			}
		}
	} else {
		if (pi->e[1] > octree->center.e[1]) {
			if (pi->e[2] > octree->center.e[2]) {
				return 4;
			} else {
				return 5;
			}
		} else {
			if (pi->e[2] > octree->center.e[2]) {
				return 6;
			} else {
				return 7;
			}
		}
	}
}

#if 0
static double
bssrdf_rd()
{
	const double g = 0.0;	/* mean cosine				*/
	const double s_s;	/* scattering coefficient		*/
	const double s_a;	/* absorption coefficient		*/

	double a;		/* boundary condition for mismatched
				   interface				*/
	double s_tr;		/* effective transport extinction coeff	*/
	double s_s1;		/* reduced scattering coefficient	*/
	double s_t1;		/* reduced extinction coefficient	*/
	double lu;		/* mean free path 			*/
	double zr;		/* dipole postion			*/
	double zv;		/*					*/
	double dr;		/* distant to the real light source	*/
	double dv;		/* distant to the virtual source	*/
	double r;		/* distant from xo to illumination	*/
	double albedo1;	
	double c1, c2;

	double rd;		/* diffuse fluence			*/

	a = (1.0 + f_dr()) / (1.0 - f_dr());

	s_s1 = (1.0 - g) * s_s;
	s_t1 = s_s1 + s_a;

	albedo1 = s_s1 / s_t1;

	lu   = 1.0 / s_t1;	

	zr = lu;
	zv = lu * (1.0 + (4.0 / 3.0) * a);

	s_tr = sqrt(3.0 * s_a * s_t1);

	//ri_vector_sub(&dist, xo, xi);
	//r = ri_vector_length(dist);

	dr = sqrt(r * r + zr * zr);
	dv = sqrt(r * r + zv * zv);

	c1 = zr * (s_tr + 1.0 / dr);
	c2 = zv * (s_tr + 1.0 / dv);

	rd = (albedo1 / (4.0 * M_PI))
	   * ((c1 * exp(-(s_tr * dr)) / (dr * dr)) +
	      (c2 * exp(-(s_tr * dv)) / (dv * dv)));

	return rd;
}

static void
bssrdf_sample_exitance(ri_vector_t *m, const ri_vector_t *p)
{
	double f_dt;
	//double area;
	//double rd;
	//ri_vector_t irrad;
	
	f_dt = 1.0 - f_dr();
	(void)m;
	(void)p;

#if 0
	// not implemented yet
	for (;;) {
		area = ;
		ri_vector_copy(&irrad, ;
		rd = bssrdf_rd();

		m->e[0] += f_dr * rd * irrad.e[0] * area;
		m->e[1] += f_dr * rd * irrad.e[1] * area;
		m->e[2] += f_dr * rd * irrad.e[2] * area;
	}	
#endif

}

static void
bssrdf_convert_exitance_to_radiance(ri_vector_t *r,
				    const ri_vector_t *m,
				    const ri_vector_t *p,
				    const ri_vector_t *w){
	double f_t;		/* Fresnel transmittance		*/

	f_t = 1.0;
	(void)p;
	(void)w;

	r->e[0] = (f_t / f_dr()) * (m->e[0] / M_PI);
	r->e[1] = (f_t / f_dr()) * (m->e[1] / M_PI);
	r->e[2] = (f_t / f_dr()) * (m->e[2] / M_PI);
}
#endif

static double
f_dr()
{
	const double eta = 1.3;	/* relative index of refraction	*/
	//double f_dr;		/* diffuse Fresnel term			*/
	double val;

	val = -1.440 / (eta * eta) + 0.710 / eta + 0.668 + 0.0636 * eta;

	return val;
}

static double
f_t(const ri_vector_t *w, const ri_vector_t *n)
{
	double dot;
	double fo = 0.04;	
	double tmp;
	double fr;

	dot = ri_vector_dot3(w, n);

	/* use Schlick's approximation */
	tmp = (1.0 - dot) * (1.0 - dot);
	tmp *= tmp;
	tmp *= (1.0 - dot);

	fr = fo + (1.0 - fo) * tmp;

	return fabs(1 - fr);
	//return fr;
}

static double
delta_w(const ri_vector_t *x, double a, const ri_vector_t *p)
{
	ri_vector_t xp;
	double      len;
	double      w = 1.0e+6;

	ri_vector_sub(&xp, x, p);
	len = ri_vector_length(&xp);

	assert(len != 0.0);

	//w = a / (len * len);
	if (len > 1.0e-6) {
		w = a / (len * len);
	}

	return w;
}

static double
potfunc_sig2002(const ri_vector_t *xi, const ri_vector_t *xo)
{
	//const double g = 0.0;	/* mean cosine				*/
	//const double s_s = 0.70;/* scattering coefficient		*/
	//const double s_a = 0.0020;/* absorption coefficient		*/
	double s_a;

	double a;		/* boundary condition for mismatched
				   interface				*/
	double s_tr;		/* effective transport extinction coeff	*/
	double s_s1;		/* reduced scattering coefficient	*/
	double s_t1;		/* reduced extinction coefficient	*/
	double lu;		/* mean free path 			*/
	double zr;		/* dipole postion			*/
	double zv;		/*					*/
	double dr;		/* distant to the real light source	*/
	double dv;		/* distant to the virtual source	*/
	double r;		/* distant from xo to illumination	*/
	double albedo1;	
	double c1, c2;
	double ecoeff;

	double weight;

	ri_vector_t dist;


	a = (1.0 + f_dr()) / (1.0 - f_dr());

	//s_s1 = (1.0 - g) * s_s;
	s_s1 = ri_render_get()->context->option->bssrdf_scatter;
	s_a  = ri_render_get()->context->option->bssrdf_absorb;
	s_t1 = s_s1 + s_a;

	albedo1 = s_s1 / s_t1;

	lu   = 1.0 / s_t1;	

	zr = lu;
	zv = lu * (1.0 + 4.0 / (3.0 * a));

	s_tr = sqrt(3.0 * s_a * s_t1);

	ri_vector_sub(&dist, xo, xi);
	r = ri_vector_length(&dist);

	//if (r <= 1.0e-6) return 0.0;

	// hack
	r *= ri_render_get()->context->option->bssrdf_scale;

	dr = sqrt(r * r + zr * zr);
	dv = sqrt(r * r + zv * zv);

	c1 = zr * (s_tr + 1.0 / dr);
	c2 = zv * (s_tr + 1.0 / dv);

	ecoeff = exp(-(s_tr * dr));
	//weight  = c1 * (ecoeff / (dr * dr));
	weight  = c1 * (ecoeff / (s_t1 * dr * dr));

	ecoeff = exp(-(s_tr * dv));
	//weight  += c2 * (ecoeff / (dv * dv));
	weight  += c2 * (ecoeff / (s_t1 * dv * dv));

	//weight  = c1 * exp(-(s_tr * dr)) / (dr * dr);
	//weight += c2 * exp(-(s_tr * dv)) / (dv * dv);
	weight *= (1.0 / (4.0 * M_PI));

	return weight;
}

void
ri_bssrdf_eval_bruteforce(ri_vector_t *rad, ri_bssrdf_t *bssrdf,
			  const ri_vector_t *eye,
			  const ri_vector_t *xo, const ri_vector_t *no)
{
	unsigned int i;
	double w;
	double area_p;	
	ri_vector_t mo;		/* radiance exitance at xo*/
	//ri_vector_t dist;
	//double len;

	double f_dt;

	f_dt = 1.0 - f_dr();
	ri_vector_zero(&mo);
	
	/* we assume area at each sample location is constant. */
	area_p = bssrdf->area / (double)bssrdf->nsamples; 

	//area_p = 1.0; 
	for (i = 0; i < bssrdf->nsamples; i++) {
		w = potfunc_sig2002(xo, &(bssrdf->positions[i]));
		
		mo.e[0] += f_dt * w * bssrdf->irradiances[i].e[0] * area_p;
		mo.e[1] += f_dt * w * bssrdf->irradiances[i].e[1] * area_p;
		mo.e[2] += f_dt * w * bssrdf->irradiances[i].e[2] * area_p;
		//mo.e[0] += w * bssrdf->irradiances[i].e[0] * area_p;
		//mo.e[1] += w * bssrdf->irradiances[i].e[1] * area_p;
		//mo.e[2] += w * bssrdf->irradiances[i].e[2] * area_p;
	}

	//ri_vector_scale(&mo, 4.0);

	rad->e[0] = (f_t(eye, no) / f_dr()) * (mo.e[0] / M_PI);
	rad->e[1] = (f_t(eye, no) / f_dr()) * (mo.e[1] / M_PI);
	rad->e[2] = (f_t(eye, no) / f_dr()) * (mo.e[2] / M_PI);
}

void
ri_bssrdf_eval_sig2002(ri_vector_t *rad, ri_bssrdf_t *bssrdf,
		       const ri_vector_t *eye,
		       const ri_vector_t *xo, const ri_vector_t *no)
{
	//unsigned int i;
	//double w;
	double area_p;	
	ri_vector_t mo;		/* radiance exitance at xo*/

	//double f_dt;

	ri_vector_zero(&mo);
	
	eval_sig2002_traverse(bssrdf->htree, bssrdf, &mo, xo);

	rad->e[0] = (f_t(eye, no) / f_dr()) * (mo.e[0] / M_PI);
	rad->e[1] = (f_t(eye, no) / f_dr()) * (mo.e[1] / M_PI);
	rad->e[2] = (f_t(eye, no) / f_dr()) * (mo.e[2] / M_PI);

	area_p = bssrdf->area / (double)bssrdf->nsamples;

	rad->e[0] /= area_p;
	rad->e[1] /= area_p;
	rad->e[2] /= area_p;
}

void
ri_bssrdf_eval_fmm(ri_vector_t *rad, ri_bssrdf_t *bssrdf,
		   const ri_vector_t *eye,
		   const ri_vector_t *xo, const ri_vector_t *no)
{
	//unsigned int i;
	//double w;
	double area_p;	
	ri_vector_t mo;		/* radiance exitance at xo*/

	//double f_dt;

	ri_vector_zero(&mo);
	
	eval_fmm_traverse(bssrdf->fmmtree, bssrdf, &mo, xo);


	rad->e[0] = (f_t(eye, no) / f_dr()) * (mo.e[0] / M_PI);
	rad->e[1] = (f_t(eye, no) / f_dr()) * (mo.e[1] / M_PI);
	rad->e[2] = (f_t(eye, no) / f_dr()) * (mo.e[2] / M_PI);


	area_p = bssrdf->area / (double)bssrdf->nsamples;

	rad->e[0] /= area_p;
	rad->e[1] /= area_p;
	rad->e[2] /= area_p;

}

void
ri_bssrdf_test()
{
	FILE *fp;
	ri_vector_t x, xi;
	double w;
	int i;
	int nsamples = 10000;
	double tick;
	double maxdist = 10.0;
	double dist;
	double scatter, absorb;
	double lu;		/* mean free path 			*/

	scatter = ri_render_get()->context->option->bssrdf_scatter;
	absorb = ri_render_get()->context->option->bssrdf_absorb;
	lu = 1.0 / (scatter + absorb);

	printf("lu = %f\n", lu);

	fp = fopen("bssrdf.dat", "w");
	if (!fp) return;

	ri_vector_zero(&x);
	ri_vector_zero(&xi);

	tick = maxdist / (double)nsamples;
	for (i = 0; i < nsamples; i++) {
		dist = (double)i * tick; 	

		xi.e[0] = dist;
		
		w = potfunc_sig2002(&x, &xi);
		w *= 4.0 * M_PI;

		fprintf(fp, "%f %f\n", dist, w);
	}

	fclose(fp);
}


static void
eval_sig2002_traverse(ri_octree_t *tree, ri_bssrdf_t *bssrdf, ri_vector_t *m,
		      const ri_vector_t *x)
{
	unsigned int i;
	unsigned int id;
	double w;
	double f_dt;
	double area_p;
	double delta;
	//ri_vector_t dist;
	sig2002_node_data_t *nodedata;

	f_dt = 1.0 - f_dr();
	
	nodedata = (sig2002_node_data_t *)tree->data;

	if (nodedata->idarray->nelems < 1) return;


	if (tree->leaf) {
		area_p = bssrdf->area / (double)bssrdf->nsamples;

		/* directly compute. */
		//printf("direct.\n");
		for (i = 0; i < nodedata->idarray->nelems; i++) {
			id = *(unsigned int *)ri_array_at(nodedata->idarray, i);

			w = potfunc_sig2002(x, &(bssrdf->positions[id]));
			
			m->e[0] += f_dt * w * bssrdf->irradiances[id].e[0] * area_p;
			m->e[1] += f_dt * w * bssrdf->irradiances[id].e[1] * area_p;
			m->e[2] += f_dt * w * bssrdf->irradiances[id].e[2] * area_p;
		}

	} else {

		delta = delta_w(x, nodedata->area, &(nodedata->p));

		//printf("delta = %f\n", delta);

		if (delta < 0.1) {
			w = potfunc_sig2002(x, &(nodedata->p));

			m->e[0] += f_dt * w * nodedata->irrad.e[0] * nodedata->area;
			m->e[1] += f_dt * w * nodedata->irrad.e[1] * nodedata->area;
			m->e[2] += f_dt * w * nodedata->irrad.e[2] * nodedata->area;
		}


		for (i = 0; i < 8; i++) {
			eval_sig2002_traverse(tree->child[i], bssrdf, m, x);
		}

	}
}

void
ri_bssrdf_show_points(ri_bssrdf_t *bssrdf, ri_display_drv_t *drv)
{
	(void)bssrdf;
	(void)drv;		

}

#if 0
static void
dump_htree(ri_octree_t *tree)
{
	FILE *fp;
	int i;
	sig2002_node_data_t *nodedata;
	ri_vector_t          irrad;
	ri_vector_t          pos;
	ri_vector_t          tcenter;
	double               area;
	double               width;

	fp = fopen("bssrdf_htree.dat", "w");
	if (!fp) return;

	// top level
	ri_vector_copy(&tcenter, &(tree->center));
	width = tree->width;
	nodedata = (sig2002_node_data_t *)tree->data;
	ri_vector_copy(&irrad, &(nodedata->irrad));
	ri_vector_copy(&pos, &(nodedata->p));
	area = nodedata->area;

	fprintf(fp, "level 0:\n");
	fprintf(fp, "  center: (%f, %f, %f)\n", tcenter.e[0], tcenter.e[1], tcenter.e[2]);
	fprintf(fp, "  width: %f\n", width);
	fprintf(fp, "  irrad: (%f, %f, %f)\n", irrad.e[0], irrad.e[1], irrad.e[2]);
	fprintf(fp, "  pos  : (%f, %f, %f)\n", pos.e[0], pos.e[1], pos.e[2]);
	fprintf(fp, "  area : %f\n", area);
	fprintf(fp, "  area : %f\n", area);

	// level 1
	for (i = 0; i < 8; i++) {
		ri_vector_copy(&tcenter, &(tree->child[i]->center));
		width = tree->child[i]->width;
		nodedata = (sig2002_node_data_t *)tree->child[i]->data;

		
		ri_vector_copy(&irrad, &(nodedata->irrad));
		ri_vector_copy(&pos, &(nodedata->p));
		area = nodedata->area;

		fprintf(fp, "level 1 child %d:\n", i);
		fprintf(fp, "  center: (%f, %f, %f)\n", tcenter.e[0], tcenter.e[1], tcenter.e[2]);
		fprintf(fp, "  width: %f\n", width);
		fprintf(fp, "  irrad: (%f, %f, %f)\n", irrad.e[0], irrad.e[1], irrad.e[2]);
		fprintf(fp, "  pos  : (%f, %f, %f)\n", pos.e[0], pos.e[1], pos.e[2]);
		fprintf(fp, "  area : %f\n", area);

	}

	fclose(fp);
}


static void
dump_fmmtree(ri_octree_t *tree)
{
	FILE *fp;
	int i;
	sig2002_node_data_t *nodedata;
	ri_vector_t          irrad;
	ri_vector_t          pos;
	ri_vector_t          tcenter;
	ri_vector_t          localirrad;
	double               area;
	double               width;

	fp = fopen("bssrdf_fmmtree.dat", "w");
	if (!fp) return;

	// top level
	ri_vector_copy(&tcenter, &(tree->center));
	width = tree->width;
	nodedata = (sig2002_node_data_t *)tree->data;
	ri_vector_copy(&irrad, &(nodedata->irrad));
	ri_vector_copy(&localirrad, &(nodedata->localirrad));
	ri_vector_copy(&pos, &(nodedata->p));
	area = nodedata->area;

	fprintf(fp, "level 0:\n");
	fprintf(fp, "  center: (%f, %f, %f)\n", tcenter.e[0], tcenter.e[1], tcenter.e[2]);
	fprintf(fp, "  width: %f\n", width);
	fprintf(fp, "  irrad: (%f, %f, %f)\n", irrad.e[0], irrad.e[1], irrad.e[2]);
	fprintf(fp, "  pos  : (%f, %f, %f)\n", pos.e[0], pos.e[1], pos.e[2]);
	fprintf(fp, "  local : (%f, %f, %f)\n", localirrad.e[0], localirrad.e[1], localirrad.e[2]);
	fprintf(fp, "  area : %f\n", area);

	// level 1
	for (i = 0; i < 8; i++) {
		dump_fmmtree_rec(fp, tree->child[i]);
	}

	fclose(fp);
}

static void
dump_fmmtree_rec(FILE *fp, ri_octree_t *tree)
{
	int i;
	sig2002_node_data_t *nodedata;
	ri_vector_t          irrad;
	ri_vector_t          pos;
	ri_vector_t          tcenter;
	ri_vector_t          localirrad;
	double               area;
	double               width;

	// top level
	ri_vector_copy(&tcenter, &(tree->center));
	width = tree->width;
	nodedata = (sig2002_node_data_t *)tree->data;
	ri_vector_copy(&irrad, &(nodedata->irrad));
	ri_vector_copy(&localirrad, &(nodedata->localirrad));
	ri_vector_copy(&pos, &(nodedata->p));
	area = nodedata->area;

	fprintf(fp, "level %d:\n", tree->depth);
	fprintf(fp, "  center: (%f, %f, %f)\n", tcenter.e[0], tcenter.e[1], tcenter.e[2]);
	fprintf(fp, "  width: %f\n", width);
	fprintf(fp, "  irrad: (%f, %f, %f)\n", irrad.e[0], irrad.e[1], irrad.e[2]);
	fprintf(fp, "  pos  : (%f, %f, %f)\n", pos.e[0], pos.e[1], pos.e[2]);
	fprintf(fp, "  local : (%f, %f, %f)\n", localirrad.e[0], localirrad.e[1], localirrad.e[2]);
	fprintf(fp, "  area : %f\n", area);

	if (tree->leaf) return;

	for (i = 0; i < 8; i++) {
		dump_fmmtree_rec(fp, tree->child[i]);
	}
}

static void
calc_farpower(ri_bssrdf_t *bssrdf, int level)
{
	int x, y, z;
	int il, ir, jl, jr, kl, kr;
	int size;

	size = pow(2, level);

	for (z = 0; z < size; z++) {
		if (z % 2 ==0) {
			kl = z - 2;
			kr = z + 4;
		} else {
			kl = z - 3;
			kr = z + 3;
		}

		for (y = 0; y < size; y++) {
			if (y % 2 ==0) {
				jl = y - 2;
				jr = y + 4;
			} else {
				jl = y - 3;
				jr = y + 3;
			}
			for (x = 0; x < size; x++) {
				if (x % 2 ==0) {
					il = x - 2;
					ir = x + 4;
				} else {
					il = x - 3;
					ir = x + 3;
				}

				gather_farpower(bssrdf, level, x, y, z,
						il, ir, jl, jr, kl, kr);

			}
		}
	}

	printf("computed far power.\n");

}


static void
gather_farpower(ri_bssrdf_t *bssrdf, int level, int x, int y, int z, 
	        int xl, int xr, int yl, int yr, int zl, int zr)
{
	int i, j, k;
	int size;
	double width;
	//ri_vector_t center;
	//ri_vector_t irrad;
	double w;
	double f_dt;
	sig2002_node_data_t *nodedata;
	sig2002_node_data_t *fardata;

	size = pow(2, level);

	nodedata = &fmmcell[level][z * size * size + y * size + x];

	width = bssrdf->fmmtree->width;
	// cell width at level
	width /= (double)size;
	
	f_dt = 1.0 - f_dr();

	ri_vector_zero(&(nodedata->farirrad));

	for (k = zl; k < zr; k++) {
		if (k < 0 || k >= size) continue;

		for (j = yl; j < yr; j++) {
			if (j < 0 || j >= size) continue;

			for (i = xl; i < xr; i++) {
				if (i < 0 || i >= size) continue;

				if (i == x && j == y && k == z) {
					// cell is myself
					continue;
				}			

				if (abs(i - x) <= 1 &&
				    abs(j - y) <= 1 &&
				    abs(k - z) <= 1) {
					// cell is near
					continue;
				}

				// far cell
				fardata = &fmmcell[level][k * size * size + j * size + i];

				w = potfunc_sig2002(&(nodedata->center),
						    &(fardata->center));
				nodedata->farirrad.e[0] += f_dt * w * fardata->localirrad.e[0] * fardata->area;
				nodedata->farirrad.e[1] += f_dt * w * fardata->localirrad.e[1] * fardata->area;
				nodedata->farirrad.e[2] += f_dt * w * fardata->localirrad.e[2] * fardata->area;

			}
		}
	}

#if 0
	printf("far power at [%d][%d][%d] = (%f, %f, %f)\n", x, y, z, 
		nodedata->farirrad.e[0],
		nodedata->farirrad.e[1],
		nodedata->farirrad.e[2]);
#endif

}

static void
gather_nearpower(ri_bssrdf_t *bssrdf, int level, int x, int y, int z)
{
	int i, j, k;
	int size;

	(void)bssrdf;

	size = pow(2, level);

	for (k = z - 1; k <= z + 1; k++) {
		if (k < 0 || k >= size) continue;
		for (j = y - 1; j <= y + 1; j++) {
			if (j < 0 || j >= size) continue;
			for (i = x - 1; i <= x + 1; x++) {
				if (x < 0 || x >= size) continue;

				// gather power;
			}
		}
	}


}
#endif

static void
eval_fmm_traverse(ri_octree_t *tree, ri_bssrdf_t *bssrdf, ri_vector_t *m,
		  const ri_vector_t *xpos)
{
	int x, y, z;
	int cell;
	unsigned int i;
	unsigned int id;
	double w;
	double f_dt;
	double area_p;
	//double delta;
	//ri_vector_t dist;
	sig2002_node_data_t *nodedata;
	sig2002_node_data_t *childdata;

	f_dt = 1.0 - f_dr();
	
	nodedata = (sig2002_node_data_t *)tree->data;

	if (nodedata->idarray->nelems < 1) return;

	area_p = bssrdf->area / (double)bssrdf->nsamples;

	if (tree->leaf) {
		/* directly compute. */
		for (i = 0; i < nodedata->idarray->nelems; i++) {
			id = *(unsigned int *)ri_array_at(nodedata->idarray, i);

			w = potfunc_sig2002(xpos, &(bssrdf->positions[id]));
			
			m->e[0] += f_dt * w * bssrdf->irradiances[id].e[0] * area_p;
			m->e[1] += f_dt * w * bssrdf->irradiances[id].e[1] * area_p;
			m->e[2] += f_dt * w * bssrdf->irradiances[id].e[2] * area_p;
		}

		x = nodedata->voxel[0];
		y = nodedata->voxel[1];
		z = nodedata->voxel[2];

		gather_nearcell(bssrdf, tree->depth, x, y, z, m, xpos);

#if 0
		// far power
		w = potfunc_sig2002(xpos, nodedata->center);
		m->e[0] += f_dt * nodedata->farirrad.e[0] * area_p;
		m->e[1] += f_dt * nodedata->farirrad.e[1] * area_p;
		m->e[2] += f_dt * nodedata->farirrad.e[2] * area_p;

#endif
		// local power
#if 0
		w = potfunc_sig2002(xpos, nodedata->center);
		m->e[0] += f_dt * w * nodedata->accumlocal.e[0] * nodedata->area;
		m->e[1] += f_dt * w * nodedata->accumlocal.e[1] * nodedata->area;
		m->e[2] += f_dt * w * nodedata->accumlocal.e[2] * nodedata->area;

#endif
		m->e[0] += nodedata->accumlocal.e[0];
		m->e[1] += nodedata->accumlocal.e[1];
		m->e[2] += nodedata->accumlocal.e[2];
#if 0
		w = potfunc_sig2002(xpos, nodedata->center);
		m->e[0] += f_dt * w * nodedata->localirrad.e[0] * nodedata->area;
		m->e[1] += f_dt * w * nodedata->localirrad.e[1] * nodedata->area;
		m->e[2] += f_dt * w * nodedata->localirrad.e[2] * nodedata->area;

#endif

#if 0
		printf("accum local = (%f, %f, %f)\n", nodedata->accumlocal.e[0],
			nodedata->accumlocal.e[1],
			nodedata->accumlocal.e[2]);

#endif

	} else {

		// distribute local power
		for (i = 0; i < 8; i++) {
			childdata = (sig2002_node_data_t *)tree->child[i]->data;

			w = potfunc_sig2002(&(tree->child[i]->center),
					    &(nodedata->center));
			childdata->accumlocal.e[0] = nodedata->accumlocal.e[0] + f_dt * w * nodedata->localirrad.e[0] * area_p;
			childdata->accumlocal.e[1] = nodedata->accumlocal.e[1] + f_dt * w * nodedata->localirrad.e[1] * area_p;
			childdata->accumlocal.e[2] = nodedata->accumlocal.e[2] + f_dt * w * nodedata->localirrad.e[2] * area_p;
#if 0
			childdata->accumlocal.e[0] = nodedata->accumlocal.e[0] + f_dt * w * nodedata->localirrad.e[0] * nodedata->area;
			childdata->accumlocal.e[1] = nodedata->accumlocal.e[1] + f_dt * w * nodedata->localirrad.e[1] * nodedata->area;
			childdata->accumlocal.e[2] = nodedata->accumlocal.e[2] + f_dt * w * nodedata->localirrad.e[2] * nodedata->area;

#endif
 
		}

		cell = within_node(tree, xpos);

		eval_fmm_traverse(tree->child[cell], bssrdf, m, xpos);

	}
}

#if 0
static void
calc_localpower(ri_octree_t *tree, ri_bssrdf_t *bssrdf)
{
	unsigned int i;
	unsigned int id;
	unsigned int npoints;
	double w;
	double f_dt;
	double area_p;
	sig2002_node_data_t *nodedata;
	ri_vector_t irrad;

	f_dt = 1.0 - f_dr();
	
	/* we assume area at each sample location is constant. */
	area_p = bssrdf->area / (double)bssrdf->nsamples; 

	// root cell.
	ri_vector_zero(&irrad);
	nodedata = (sig2002_node_data_t *)tree->data;

	npoints = nodedata->idarray->nelems;

	for (i = 0; i < npoints; i++) {
		id = *(unsigned int *)ri_array_at(nodedata->idarray, i);

		w = potfunc_sig2002(&(tree->center), &(bssrdf->positions[id]));
	
		irrad.e[0] += f_dt * w * bssrdf->irradiances[id].e[0] * area_p;
		irrad.e[1] += f_dt * w * bssrdf->irradiances[id].e[1] * area_p;
		irrad.e[2] += f_dt * w * bssrdf->irradiances[id].e[2] * area_p;
	}

	ri_vector_copy(&(nodedata->localirrad), &irrad);

	for (i = 0; i < 8; i++) {
		calc_localpower_traverse(tree->child[i], bssrdf);
	}
}
#endif
static void
calc_localpower_traverse(ri_octree_t *tree, ri_bssrdf_t *bssrdf)
{

	unsigned int i;
	unsigned int id;
	unsigned int npoints;
	double w;
	double f_dt;
	double area_p;
	sig2002_node_data_t *nodedata;
	ri_vector_t irrad;


	f_dt = 1.0 - f_dr();
	
	/* we assume area at each sample location is constant. */
	area_p = bssrdf->area / (double)bssrdf->nsamples; 

	// root cell.
	ri_vector_zero(&irrad);
	nodedata = (sig2002_node_data_t *)tree->data;

	npoints = nodedata->idarray->nelems;

	for (i = 0; i < npoints; i++) {
		id = *(unsigned int *)ri_array_at(nodedata->idarray, i);

		w = potfunc_sig2002(&(tree->center), &(bssrdf->positions[id]));
	
		irrad.e[0] += f_dt * w * bssrdf->irradiances[id].e[0] * area_p;
		irrad.e[1] += f_dt * w * bssrdf->irradiances[id].e[1] * area_p;
		irrad.e[2] += f_dt * w * bssrdf->irradiances[id].e[2] * area_p;
	}

	ri_vector_copy(&(nodedata->localirrad), &irrad);

	if (tree->leaf) return;

	for (i = 0; i < 8; i++) {
		calc_localpower_traverse(tree->child[i], bssrdf);
	}
}

static void
gather_nearcell(ri_bssrdf_t *bssrdf, int level, int x, int y, int z, ri_vector_t *m, const ri_vector_t *xpos)
{
	int i, j, k, l;
	unsigned int id;
	int size;
	int npoints;
	double w;
	double f_dt;
	double area_p;

	sig2002_node_data_t *nodedata;
	
	size = pow(2, level);

	f_dt = 1.0 - f_dr();

	area_p = bssrdf->area / (double)bssrdf->nsamples;


	for (k = z - 1; k <= z + 1; k++) {
		if (k < 0 || k >= size) continue;
		for (j = y - 1; j <= y + 1; j++) {
			if (j < 0 || j >= size) continue;
			for (i = x - 1; i <= x + 1; i++) {
				if (i < 0 || i >= size) continue;

				if (i == x && j == y && k == z) continue;

				nodedata = &fmmcell[level][k * size * size + j * size + i];
				npoints = nodedata->idarray->nelems;

				for (l = 0; l < npoints; l++) {
					id = *(unsigned int *)ri_array_at(nodedata->idarray, l);

					w = potfunc_sig2002(
						xpos,
						&(bssrdf->positions[id]));
					
					m->e[0] += f_dt * w * bssrdf->irradiances[id].e[0] * area_p;
					m->e[1] += f_dt * w * bssrdf->irradiances[id].e[1] * area_p;
					m->e[2] += f_dt * w * bssrdf->irradiances[id].e[2] * area_p;

				}
			}
		}
	}

}

static void
calc_xyz(int cell[3], int level,
	 const ri_vector_t *p, const ri_vector_t *center, double width)
{
	int ndiv;

	double min[3];

	min[0] = center->e[0] - width;
	min[1] = center->e[1] - width;
	min[2] = center->e[2] - width;

	ndiv = (int)pow(2, level);

	cell[0] = (int)(((p->e[0] - min[0]) / (width * 2.0)) * (double)ndiv); 
	cell[1] = (int)(((p->e[1] - min[1]) / (width * 2.0)) * (double)ndiv); 
	cell[2] = (int)(((p->e[2] - min[2]) / (width * 2.0)) * (double)ndiv); 

}

static void
write_cache(ri_bssrdf_t *bssrdf)
{
	FILE *fp;
	char *filename;

	unsigned int i;

	filename = ri_render_get()->context->option->bssrdf_cache_file;

	fp = fopen(filename, "w");
	if (!fp) {
		printf("can't open file [ %s ] to write cache.\n", filename);
		return;
	}

	fprintf(fp, "%u\n", bssrdf->nsamples);
	fprintf(fp, "%f\n", bssrdf->area);

	for (i = 0; i < bssrdf->nsamples; i++) {
		fprintf(fp, "%f %f %f ", bssrdf->irradiances[i].e[0],
					 bssrdf->irradiances[i].e[1],
					 bssrdf->irradiances[i].e[2]);
		fprintf(fp, "%f %f %f ", bssrdf->positions[i].e[0],
					 bssrdf->positions[i].e[1],
					 bssrdf->positions[i].e[2]);
		fprintf(fp, "%f %f %f ", bssrdf->normals[i].e[0],
					 bssrdf->normals[i].e[1],
					 bssrdf->normals[i].e[2]);

	} 

	fclose(fp);
}

static int
read_cache(ri_bssrdf_t *bssrdf)
{
	FILE *fp;
	char *filename;

	unsigned int i;

	filename = ri_render_get()->context->option->bssrdf_cache_file;

	fp = fopen(filename, "r");
	if (!fp) {
		printf("can't open file [ %s ] to read cache.\n", filename);
		return 0;
	}

	fscanf(fp, "%u\n", &(bssrdf->nsamples));
	fscanf(fp, "%lf\n", &(bssrdf->area));

	bssrdf->positions = (ri_vector_t *)ri_mem_alloc(sizeof(ri_vector_t) *
							bssrdf->nsamples);
	bssrdf->normals = (ri_vector_t *)ri_mem_alloc(sizeof(ri_vector_t) *
						      bssrdf->nsamples);
	bssrdf->irradiances = (ri_vector_t *)ri_mem_alloc(sizeof(ri_vector_t) *
							  bssrdf->nsamples);

	for (i = 0; i < bssrdf->nsamples; i++) {
		fscanf(fp, "%f %f %f ", &(bssrdf->irradiances[i].e[0]),
					 &(bssrdf->irradiances[i].e[1]),
					 &(bssrdf->irradiances[i].e[2]));
		fscanf(fp, "%f %f %f ", &(bssrdf->positions[i].e[0]),
					 &(bssrdf->positions[i].e[1]),
					 &(bssrdf->positions[i].e[2]));
		fscanf(fp, "%f %f %f ", &(bssrdf->normals[i].e[0]),
					 &(bssrdf->normals[i].e[1]),
					 &(bssrdf->normals[i].e[2]));
	} 

	fclose(fp);

	return 1;
}

static void
calc_hemibasis(ri_hemisphere_t *hemi, const ri_vector_t *normal)
{
	int i;
	ri_vector_copy(&(hemi->basis[2]), normal);
	ri_vector_zero(&(hemi->basis[1]));

	for (i = 0; i < 3; i++) {
		if (hemi->basis[2].e[i] < 0.6 && hemi->basis[2].e[i] > -0.6)
			break;
	}

	if (i >= 3) i = 0;
	hemi->basis[1].e[i] = 1.0;

	ri_vector_cross3(&hemi->basis[0], &hemi->basis[1], &hemi->basis[2]);
	ri_vector_normalize(&hemi->basis[0]);
	ri_vector_cross3(&hemi->basis[1], &hemi->basis[2], &hemi->basis[0]);
}
