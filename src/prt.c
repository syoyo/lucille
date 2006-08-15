#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <assert.h>

#include "vector.h"
#include "log.h"
#include "memory.h"
#include "render.h"
#include "raytrace.h"
#include "random.h"
#include "irradcache.h"
#include "parallel.h"
#include "sh.h"
#include "reflection.h"
#include "htm.h"
#include "qmc.h"

#define NSAMPLES 8
#define MAXWIDTH 2000

#define MAXGEOM 1000

#ifndef M_PI
#define M_PI 3.1415926535
#endif

/*
 * method. assume object is diffuse.
 * for each vertex
 *   0. M = 0;
 *   1. generate many random directions s on unit sphere
 *   2. calculate visibility v(s).
 *      If the ray with direction s intersects object itself, v(s) = 0,
 *      else v(s) = 1.
 *   3. M += (Ps / PI) v(s) max((N . s), 0) y(s).
 *
 */

/* SH coeff of vertex */
typedef struct _shcoeff_t
{
	int           vtxid;
	float         y[25][25];
} shcoeff_t;

typedef struct _vtxinfo_t
{
	int                 vtxid;
	float               y[25][25];
	ri_htm_occlusion_t *occlusion;
} vtxinfo_t;

typedef struct _prt_sh_sample_t
{
	double theta, phi;
	double v[3];
	double y[25];
} prt_sh_sample_t;

typedef struct _prt_sh_samplesphere_t
{
	int ntheta, nphi;
	prt_sh_sample_t *samples;
} prt_sh_samplesphere_t;

typedef struct _prt_sample_t
{
	int    occlusion;
	//double m[25][25];
	float  m[25][25];
} prt_sample_t;

typedef struct _prt_hemisample_t
{
	int ntheta, nphi;
	prt_sample_t *samples;
} prt_hemisample_t;

/* this is hack. not elegant... */
static shcoeff_t *prevlist[MAXGEOM];
static shcoeff_t *currlist[MAXGEOM];
static shcoeff_t *accumlist[MAXGEOM];
static ri_geom_t *geomtable[MAXGEOM];
static int        find_id_by_geom(ri_geom_t *geom);
static int        ngeom;
static int        g_samples;
static prt_sh_sample_t *g_shsamples;

static void endconv(void *data);
static void   sample_geom(ri_geom_t *geom, int objid);
#if 0
static void   init_sh_samplesphere(prt_sh_samplesphere_t *sphere,
				   int ntheta, int nphi);
#endif
static void   init_sh_samples(prt_sh_sample_t *shsamples,
			      ri_vector_t *points, int npoints);
static void   sample_sphere(vtxinfo_t *info, ri_htm_t *htm,
			    prt_sh_sample_t *shsamples,
			    const ri_vector_t *p, const ri_vector_t *n);
static void   sample_sphere_glossy(vtxinfo_t *info, ri_htm_t *htm,
				   prt_sh_sample_t *shsamples, 
				   const ri_vector_t *p, const ri_vector_t *n);
/* calculate spherical harmonics Yn(0 <= n <= 25) */
static void   shfunc(double sh[25], double x, double y, double z);
static int    nearest_vertex(ri_geom_t *geom, unsigned int index,
			     float u, float v);
static void   g_kern(float g[25], float kd, float ks, float glossness);
static float  g_convolve(float m[25][25], float y[25], int j,
			 float kd, float ks, float glossness);
static void   alpha_coeff(float alpha[25]);
static float  modified_phong(float r[3], float v[3], float n[3],
			     float kd, float ks, float glossness);
static void   calc_gloss_kern_coeff(float gloss_kern_coeff[25],
				    float kd, float ks, float glossness);
static void   refvec(float r[3], float in[3], float n[3]);
static void   interreflection_pass(int npass, vtxinfo_t *infos,
				   ri_geom_t *geom,
				   ri_htm_t *htm,
				   int glossy);
static void   interreflection_diffuse(shcoeff_t *shcoeffs,
				      shcoeff_t *prevcoeffs,
				      vtxinfo_t *infos,
				      int vtxindex,
				      double kd,
				      ri_geom_t *geom,
				      ri_htm_t *htm);
static void   interreflection_diffuse_hierarchical(shcoeff_t *shcoeffs,
						   shcoeff_t *prevcoeffs,
						   vtxinfo_t *infos,
						   int vtxindex,
						   int nodeindex,
						   double kd,
				   		   ri_geom_t *geom,
						   ri_htm_t *htm);
static void   interreflection_glossy(shcoeff_t *shcoeffs,
				     shcoeff_t *prevcoeffs,
				     vtxinfo_t *infos,
				     int vtxindex,
				     double kd,
				     double ks,
				     double glossness,
				     ri_geom_t *geom,
				     ri_htm_t *htm);
static void   interreflection_glossy_hierarchical(shcoeff_t *shcoeffs,
						  shcoeff_t *prevcoeffs,
						  vtxinfo_t *infos,
						  int vtxindex,
						  int nodeindex,
						  double kd, 
						  double ks, 
						  double glossness, 
				   		  ri_geom_t *geom,
						  ri_htm_t *htm);
//static void   write_data(vtxinfo_t *infos, int nvertices, int glossy, int objid);
static void   write_data(shcoeff_t *infos, int nvertices, int glossy, int objid);
static void   interreflection_pass_nohtm(ri_geom_t *geom);
static void   interreflection_diffuse_nohtm(shcoeff_t *shcoeffs,
			int vtxindex,
			double kd,
			ri_geom_t *geom);
#if 0
static int    nocclusionpoints(ri_htm_occlusion_t *occlusion, ri_htm_t *htm);
#endif

void
ri_prt_sample()
{
	int i, j, k;
	int ninterrefpass = 3;
	int do_interreflection;
	int glossy;
	
	ri_geom_t   *geom = NULL;
	ri_list_t   *geomitr;
	ri_htm_t    *htm;
	ri_option_t *opt;

	opt = ri_render_get()->context->option;

	htm = ri_htm_build(opt->prt_samplinglevel);

	g_shsamples = (prt_sh_sample_t *)ri_mem_alloc(
				sizeof(prt_sh_sample_t) * htm->npoints);
	init_sh_samples(g_shsamples, htm->points, htm->npoints);
	g_samples = htm->npoints;
	glossy    = opt->prt_is_glossy;

	ri_htm_free(htm);

	printf("--- start prt sampling ---\n");

	
	ngeom = 0;
	for (geomitr = ri_list_first(ri_render_get()->geomlist);
	     geomitr != NULL;
	     geomitr = ri_list_next(geomitr)) {
		geom = (ri_geom_t *)(geomitr->data);
		if (!geom) continue;

		geomtable[ngeom] = (ri_geom_t *)(geomitr->data);
		ngeom++;
	}

	/* shadow pass */
	for (i = 0; i < ngeom; i++) {
		sample_geom(geomtable[i], i);
	}

	do_interreflection = opt->prt_do_interreflection;

	/* interrefction pass */
	if (do_interreflection) {
		printf("--- interreflection pass ---\n");
		for (j = 0; j < ninterrefpass; j++) {
			printf("--- pass: %d of %d\n", j, ninterrefpass);
			for (i = 0; i < ngeom; i++) {
				interreflection_pass_nohtm(geomtable[i]);
			}

			for (i = 0; i < ngeom; i++) {
				for (k = 0; k < (int)(geomtable[i]->npositions); k++) {

					memcpy(&(prevlist[i][k].y[0][0]),
					       &(currlist[i][k].y[0][0]),
					       sizeof(float) * 25 * 25);
				}
			}
		}

		for (i = 0; i < ngeom; i++) {
			for (k = 0; k < (int)(geomtable[i]->npositions); k++) {

				memcpy(&(currlist[i][k].y[0][0]),
				       &(accumlist[i][k].y[0][0]),
				       sizeof(float) * 25 * 25);
			}
		}
	}

	/* write out prt data */
	for (i = 0; i < ngeom; i++) {
		write_data(currlist[i], geomtable[i]->npositions, glossy, i);
		ri_mem_free(currlist[i]);
		ri_mem_free(accumlist[i]);
		ri_mem_free(prevlist[i]);
	}
}

/* --- private functions */

void
sample_geom(ri_geom_t *geom, int objid)
{
	int i, j, k;
	int start, span, nelems;
	int glossy = 0;
	int samplinglevel;
	int nvertices;
	int vtxid;
	int do_interreflection;
	//int ninterreflectionpass = 3;
	ri_vector_t p, n;
	prt_sh_sample_t *shsamples;
	shcoeff_t *localcoeff;
	shcoeff_t *globalcoeff;

	ri_htm_occlusion_node_t *localocclist;
	ri_htm_occlusion_node_t *globalocclist;

	vtxinfo_t  vtx;
	vtxinfo_t *vtxcoeffs;		/* vertex coeffs in current pass */
	
	ri_htm_t  *htm;
	
	ri_option_t *opt;

	opt = ri_render_get()->context->option;
	samplinglevel = opt->prt_samplinglevel;
	do_interreflection = opt->prt_do_interreflection;
	glossy = opt->prt_is_glossy;

	htm = ri_htm_build(samplinglevel);

	shsamples = (prt_sh_sample_t *)ri_mem_alloc(sizeof(prt_sh_sample_t) *
						    htm->npoints);
	/* precompute SH value Y(x, y, z). */
	init_sh_samples(shsamples, htm->points, htm->npoints);
	
	nvertices = (int)geom->npositions;

	nelems = (int)(ceil((double)geom->npositions / ri_parallel_ntasks()));

	printf("nelems = %d\n", nelems);

	printf("allocating memory ---\n");

	vtx.occlusion = ri_htm_occlusion_new(samplinglevel);

	localcoeff = (shcoeff_t *)ri_mem_alloc(sizeof(shcoeff_t) * nelems);
	globalcoeff = (shcoeff_t *)ri_mem_alloc(sizeof(shcoeff_t) *
					     nelems * ri_parallel_ntasks());
	/* flat occlusion node array */
	localocclist = (ri_htm_occlusion_node_t *)ri_mem_alloc(
				sizeof(ri_htm_occlusion_node_t) * htm->nnodes *
				nelems);
	globalocclist = (ri_htm_occlusion_node_t *)ri_mem_alloc(
				sizeof(ri_htm_occlusion_node_t) * htm->nnodes *
				nelems * ri_parallel_ntasks());

	vtxcoeffs = (vtxinfo_t *)ri_mem_alloc(sizeof(vtxinfo_t) * nvertices);
	for (i = 0; i < nvertices; i++) {
		vtxcoeffs[i].occlusion = ri_htm_occlusion_new(samplinglevel);
	}

	for (i = 0; i < nelems; i++) {
		localcoeff[i].vtxid = -1;
	}

	for (i = 0; i < nelems * ri_parallel_ntasks(); i++) {
		globalcoeff[i].vtxid = -1;
	}

	printf("allocated memory ---\n");

	span = nelems;
	start = span * ri_parallel_taskid();
	if (start + span > (int)geom->npositions) {
		span = (int)geom->npositions - start;
	}
	 

	printf("nvert = %d\n", geom->npositions);
	printf("nfaces = %d\n", geom->nindices / 3);
	printf("rank[%d]: start = %d, span = %d\n", ri_parallel_taskid(),
						    start, span);

	/* foreach vertex */
	for (i = 0; i < span; i++) {
		p = geom->positions[start + i];
		n = geom->normals[start + i];

		if (ri_parallel_taskid() == 0) {
			printf("sampling vertex[%d] of span[%d]\n", start + i, start + span);
		}

		ri_htm_occlusion_clear(vtx.occlusion);

		if (glossy) {
			sample_sphere_glossy(&vtx,
					     htm,
					     shsamples,
					     &geom->positions[start + i],
					     &geom->normals[start + i]);
		} else {
			sample_sphere(&vtx,
				      htm,
				      shsamples,
				      &geom->positions[start + i],
				      &geom->normals[start + i]);
		}

		localcoeff[i].vtxid = start + i;

		if (glossy) {
			for (j = 0; j < 25; j++) {
				for (k = 0; k < 25; k++) {
					localcoeff[i].y[j][k] = vtx.y[j][k];
				}
			}
		} else {
			for (k = 0; k < 25; k++) {
				localcoeff[i].y[0][k] = vtx.y[0][k];
			}
		}

		ri_mem_copy(&localocclist[i * htm->nnodes],
			    vtx.occlusion->nodes,
			    sizeof(ri_htm_occlusion_node_t) * htm->nnodes);
	}

	ri_parallel_gather(localcoeff,
			   globalcoeff,
			   sizeof(shcoeff_t) * nelems);
	ri_parallel_gather(localocclist,
			   globalocclist,
			   sizeof(ri_htm_occlusion_node_t) *
			   nelems * htm->nnodes);

	if (ri_parallel_taskid() == 0) {
		for (i = 0; i < ri_parallel_ntasks() * nelems; i++) {
			
			vtxid = globalcoeff[i].vtxid;
			if (vtxid == -1) continue;

			assert(vtxid < ri_parallel_ntasks() * nelems);

			/* copy SH coeff */
			ri_mem_copy(&(vtxcoeffs[vtxid].y[0][0]),
				    &(globalcoeff[i].y[0][0]),
				    sizeof(float) * 25 * 25);

			/* copy occlusion */
			ri_mem_copy(vtxcoeffs[vtxid].occlusion->nodes,
				    &(globalocclist[i * htm->nnodes]),
				    sizeof(ri_htm_occlusion_node_t) *
				    htm->nnodes);
		}
	}

	ri_htm_occlusion_free(vtx.occlusion);
	ri_mem_free(localcoeff);
	ri_mem_free(globalcoeff);

	ri_mem_free(localocclist);
	ri_mem_free(globalocclist);

	if (ri_parallel_taskid() == 0) {

		//prevlist[i] = vtxcoeffs;
		currlist[objid] = (shcoeff_t *)ri_mem_alloc(sizeof(shcoeff_t) * geom->npositions);
		prevlist[objid] = (shcoeff_t *)ri_mem_alloc(sizeof(shcoeff_t) * geom->npositions);
		accumlist[objid] = (shcoeff_t *)ri_mem_alloc(sizeof(shcoeff_t) * geom->npositions);
		for (i = 0; i < (int)geom->npositions; i++) {
			prevlist[objid][i].vtxid = vtxcoeffs[i].vtxid;
			for (j = 0; j < 25; j++) {
				for (k = 0; k < 25; k++) {
					prevlist[objid][i].y[j][k] = vtxcoeffs[i].y[j][k];
					currlist[objid][i].y[j][k] = vtxcoeffs[i].y[j][k];
					accumlist[objid][i].y[j][k] = vtxcoeffs[i].y[j][k];
				}
			}
		}
	}

	for (i = 0; i < (int)geom->npositions; i++) {
		ri_htm_occlusion_free(vtxcoeffs[i].occlusion);
	}
	ri_mem_free(vtxcoeffs);
	ri_htm_free(htm);

	ri_parallel_barrier();
}

static void endconv(void *data)
{
#ifdef BIG_ENDIAN
	char tmp[4];
	tmp[0] = ((char *)data)[0];
	tmp[1] = ((char *)data)[1];
	tmp[2] = ((char *)data)[2];
	tmp[3] = ((char *)data)[3];

	((char *)data)[0] = tmp[3];
	((char *)data)[1] = tmp[2];
	((char *)data)[2] = tmp[1];
	((char *)data)[3] = tmp[0];
#endif
}

static void
sample_sphere(vtxinfo_t *info, ri_htm_t *htm, prt_sh_sample_t *shsamples,
	      const ri_vector_t *p, const ri_vector_t *n)
{
	int i, j, l;
	int hit;
	int id;
	int doscale = ri_render_get()->context->option->prt_do_distscale;
	float weight;
	float dot;
	float dist;
	float kd = 1.0;			/* diffuse reflectance */
	float diffuse;
	float scale = ri_render_get()->context->option->prt_scale;
	ri_ray_t ray;
	ri_vector_t *sample;
	ri_vector_t path;
	ri_vector_t vspherepos;		/* virtual sphere position */
	ri_surface_info_t surfinfo;

	diffuse = kd / M_PI;

	ri_vector_copy(&(ray.org), p);

	for (i = 0; i < 25; i++) {
		for (l = 0; l < 25; l++) {
			info->y[i][l] = 0.0;
		}
	}

	for (i = 0; i < htm->npoints; i++) {
		htm->sampled[i] = 0;
	}

	for (i = 1; i <= htm->nnodes; i++) {
		if (htm->nodes[i].child[0]) continue;	/* non-leaf node */

		for (j = 0; j < htm->nodes[i].npoints; j++) {
			id = htm->nodes[i].p[j];
			assert(id < htm->npoints);

			if (htm->sampled[id]) {
				/* this sampling point is already used */
				continue;
			}

			sample = &(htm->points[id]);

			ri_vector_copy(&(ray.dir), sample);

			dot = ri_vector_dot3(&(ray.dir), n);

			if (dot <= 0.0) continue;	/* under hemisphere */

			ri_vector_normalize(&(ray.dir));
			hit = ri_raytrace(ri_render_get(), &ray, &surfinfo);

			if (!hit) {
				if (doscale) {
					ri_vector_copy(&vspherepos, &(ray.dir));
					ri_vector_scale(&vspherepos, scale);

					ri_vector_sub(&path,
						      &vspherepos, &(ray.org));
					dist = ri_vector_length(&path);

					if (dist <= 0.0) dist = 0.01;

					/* attenuate with inverse square. */
					dist = 1.0 / (dist * dist);

				} else {
					dist = 1.0;
				}

				for (l = 0; l < 25; l++) {
					info->y[0][l] +=
						diffuse * 
						shsamples[id].y[l] *
						dot * dist;
				}
			} else {
				/* there is a occluder towards ray direction.
				 * turn on occlusion bit.
				 */
				info->occlusion->nodes[i].occlusions[j] = 1;

				/* We can store polygon index of hitted surface
				 * to prevent re-raytracing same direction in
				 * the subsequent interreflection pass.
				 * But it is very memory costly.
				 * For large(30k or more) scene,
				 * it's memory consuption becomes about 1GB...
				 * so we don't use index caching.
				 */ 
#if 0
				info->occlusion->nodes[i].indices[j] =
						nearest_vertex(geom,
							       surfinfo.index,
							       surfinfo.u,
							       surfinfo.v);
#endif
			}

			htm->sampled[id] = 1;
		}	
	}

	weight = (4.0 * M_PI) / (double)htm->npoints;

	for (l = 0; l < 25; l++) {
		info->y[0][l] *= weight;
	}

	/* build occlusion hierarchy */
	ri_htm_occlusion_build_hierarchy(info->occlusion, htm);
}

static void
sample_sphere_glossy(vtxinfo_t *info, ri_htm_t *htm, prt_sh_sample_t *shsamples,
		     const ri_vector_t *p, const ri_vector_t *n)
{
	int i, j, k, l;
	int hit;
	int id;
	int doscale = ri_render_get()->context->option->prt_do_distscale;
	float dot;
	float dist;
	float scale = ri_render_get()->context->option->prt_scale;
	double weight;
	ri_vector_t *sample;
	ri_vector_t path;
	ri_vector_t vspherepos;		/* virtual sphere position */
	ri_ray_t ray;
	ri_surface_info_t surfinfo;

	ri_vector_copy(&(ray.org), p);

	for (k = 0; k < 25; k++) {
		for (l = 0; l < 25; l++) {
			info->y[k][l] = 0.0;
		}
	}

	for (i = 0; i < htm->npoints; i++) {
		htm->sampled[i] = 0;
	}

	for (i = 1; i <= htm->nnodes; i++) {
		if (htm->nodes[i].child[0]) continue;	/* non-leaf node */

		for (j = 0; j < htm->nodes[i].npoints; j++) {
			id = htm->nodes[i].p[j];
			assert(id < htm->npoints);

			if (htm->sampled[id]) {
				/* this sampling point is already used */
				continue;
			}

			sample = &(htm->points[id]);

			ri_vector_copy(&(ray.dir), sample);

			dot = ri_vector_dot3(&ray.dir, n);

			if (dot <= 0.0) continue;	/* under hemisphere */

			//ri_vector_normalize(&(ray.dir));
			hit = ri_raytrace(ri_render_get(), &ray, &surfinfo);

			if (!hit) {
				if (doscale) {
					ri_vector_copy(&vspherepos, &ray.dir);
					ri_vector_scale(&vspherepos, scale);

					ri_vector_sub(&path,
						      &vspherepos, &ray.org);
					dist = ri_vector_length(&path);
					
					if (dist <= 0.0) dist = 0.01;

					/* attenuate with inverse square. */
					dist = 1.0 / (dist * dist);
				} else {
					dist = 1.0;
				}

				for (k = 0; k < 25; k++) {
					for (l = 0; l < 25; l++) {
						info->y[k][l] +=
							shsamples[id].y[l] *
							shsamples[id].y[k] *
							dist;
					}
				}
			} else {
				/* there is a occluder towards ray direction.
				 * turn on occlusion bit.
				 */
				info->occlusion->nodes[i].occlusions[j] = 1;
#if 0
				info->occlusion->nodes[i].indices[j] =
						nearest_vertex(geom,
							       surfinfo.index,
							       surfinfo.u,
							       surfinfo.v);
#endif
			}

			htm->sampled[id] = 1;
		}	
	}

	weight = (4.0 * M_PI) / (double)htm->npoints;

	for (k = 0; k < 25; k++) {
		for (l = 0; l < 25; l++) {
			info->y[k][l] *= weight;
		}
	}

	/* build occlusion hierarchy */
	ri_htm_occlusion_build_hierarchy(info->occlusion, htm);
}


static void
shfunc(double sh[25], double x, double y, double z)
{
	/* A is normalization factor: A = sqrt((2 * l + 10 / (4 * PI))) */

	double c[25];

	c[ 0] = 0.282095;
	c[ 1] = 0.488603;
	c[ 2] = 0.488603;
	c[ 3] = 0.488603;
	c[ 4] = 1.092548;
	c[ 5] = 1.092548;
	c[ 6] = 0.315392;
	c[ 7] = 1.092548;
	c[ 8] = 0.546274;

	c[ 9] = 0.590044;			// Y{3, -3}
	c[10] = 2.89061;			// Y{3,  2}
	c[11] = 0.457046;			// Y{3, -1}
	c[12] = 0.373176;			// Y{3,  0}
	c[13] = 0.457046;			// Y{3,  1}
	c[14] = 1.44531;			// Y{3, -2}
	c[15] = 0.590044;			// Y{3,  3}

	c[16] = 2.50334;			// Y{4, -4}
	c[17] = 1.77013;			// Y{4, -3}
	c[18] = 0.946175;			// Y{4, -2}
	c[19] = 0.669047;			// Y{4, -1}
	c[20] = 0.105786;			// Y{4,  0}
	c[21] = 0.669047;			// Y{4, -1}
	c[22] = 0.473087;			// Y{4,  2}
	c[23] = 1.77013;			// Y{4,  3}
	c[24] = 0.625836;			// Y{4,  4}

	/* Y_{0,0} */
	sh[ 0] = c[ 0];

	/* Y_{1,-1} Y_{1,0}, Y_{1,1} */
	sh[ 1] = c[ 1] * y;
	sh[ 2] = c[ 2] * z;
	sh[ 3] = c[ 3] * x;

	/* Y_{2, -2} Y_{2,-1}, Y_{2,1} */
	sh[ 4] = c[ 4] * x * y;
	sh[ 5] = c[ 5] * y * z;
	sh[ 7] = c[ 7] * x * z;

	/* Y_{2,0} */
	sh[ 6] = c[ 6] * (3.0 * z * z - 1.0);

	/* Y_{2,2} */
	sh[ 8] = c[ 8] * (x * x - y * y);

	/* Y_{3, -3} = A * sqrt(5/8) * (3 * x^2 * y - y^3)		*/
	sh[ 9] = c[ 9] * (3.0 * x * x * y - y * y * y); 

	/* Y_{3, -2} = A * sqrt(15) * x * y * z 			*/
	sh[10] = c[10] * x * y * z;

	/* Y_{3, -1} = A * sqrt(3/8) * y * (5 * z^2 - 1)		*/
	sh[11] = c[11] * y * (5.0 * z * z - 1.0);

	/* Y_{3,  0} = A * (1/2) * (5 * z^3 - 3 *z)			*/
	sh[12] = c[12] * (5.0 * z * z * z - 3 * z);

	/* Y_{3,  1} = A * sqrt(3/8) * x * (5 * z^2 - 1)		*/
	sh[13] = c[13] * x * (5.0 * z * z  - 1.0);

	/* Y_{3,  2} = A * sqrt(15/4) * z *(x^2 - y^2)			*/
	sh[14] = c[14] * z * (x * x - y * y);

	/* Y_{3,  3} = A * sqrt(5/8) * (x^3 - 3 * x * y^2)		*/
	sh[15] = c[15] * (x * x * x - 3.0 * x * y * y);

	/* Y_{4, -4} = A * sqrt(35/4) * (x^3 * y - x * y^3)		*/
	sh[16] = c[16] * (x * x * x * y - x * y * y * y);

	/* Y_{4, -3} = A * sqrt(35/8) * z * (3 * x^2 * y - y^3)		*/
	sh[17] = c[17] * z * (3.0 * x * x * y - y * y * y);

	/* Y_{4, -2} = A * sqrt(5/4) * x * y * (7 * z^2 - 1)		*/
	sh[18] = c[18] * x * y * (7.0 * z * z - 1.0);

	/* Y_{4, -1} = A * sqrt(5/8) * (7 * y * z^3 - 3 * y * z)	*/
	sh[19] = c[19] * (7.0 * y * z * z * z - 3.0 * y * z);

	/* Y_{4,  0} = A * (1/8) * (35 * z^4- 30 * z^2 + 3)		*/
	sh[20] = c[20] * (35.0 * z * z * z * z - 30.0 * z * z + 3.0);

	/* Y_{4,  1} = A * sqrt(5/8) * (7 * x * z^3 - 3 * x * z)	*/
	sh[21] = c[21] * (7.0 * x * z * z * z - 3.0 * x * z);

	/* Y_{4,  2} = A * sqrt(5/16) * (x^2 - y^2) * (7 * z^2 - 1)	*/
	sh[22] = c[22] * (x * x - y * y) * (7.0 * z * z - 1.0);

	/* Y_{4,  3} = A * sqrt(35/8) * z * (x^3 - 3 * x * y^2)		*/
	sh[23] = c[23] * z * (x * x * x - 3.0 * x * y * y);

	/* Y_{4,  4} = A * sqrt(35/64) * (x^4 - 6 * x^2 * y^2 + y^4)	*/
	sh[24] = c[24] * (x * x * x * x - 6.0 * x * x * y * y + y * y * y * y);
}

#if 0
static double
g(ri_vector_t s, ri_vector_t n, double glossness)
{
	float dot;
	ri_vector_t r;
	ri_reflect(&r, s, n);

	dot = ri_vector_dot3(r, s);
	if (dot < 0.0) return 0.0;

	return pow(dot, glossness);
}
#endif

#if 0
static void
init_sh_samplesphere(prt_sh_samplesphere_t *sphere, int ntheta, int nphi)
{
	int i, j;
	double x, y;
	double vx, vy, vz;
	double theta, phi;

	sphere->samples = (prt_sh_sample_t *)
				ri_mem_alloc(sizeof(prt_sh_sample_t) *
					     ntheta * nphi);

	sphere->ntheta = ntheta;
	sphere->nphi   = nphi;
	
	for (i = 0; i < nphi; i++) {
		for (j = 0; j < ntheta; j++) {
			x = (j + randomMT()) / (double)(ntheta * nphi);
			y = (i + randomMT()) / (double)(ntheta * nphi);

			theta = 2.0 * acos(sqrt(1.0 - x)); /* [0, 180] */
			phi   = 2.0 * M_PI * y;		   /* [0, 360] */


			sphere->samples[i * ntheta + j].theta = theta;
			sphere->samples[i * ntheta + j].phi   = phi;

			vx = sin(theta) * cos(phi);
			vy = sin(theta) * sin(phi);
			vz = cos(theta);

			sphere->samples[i * ntheta + j].v[0] = vx;
			sphere->samples[i * ntheta + j].v[1] = vy;
			sphere->samples[i * ntheta + j].v[2] = vz;
			
			shfunc(sphere->samples[i * ntheta + j].y,
			       vx, vy, vz);
		}
	}
}
#endif

static void
init_sh_samples(prt_sh_sample_t *shsamples, ri_vector_t *points, int npoints)
{
	int i;
	double vx, vy, vz;

	for (i = 0; i < npoints; i++) {
		vx = points[i].e[0];
		vy = points[i].e[1];
		vz = points[i].e[2];
		shsamples[i].v[0] = vx;
		shsamples[i].v[1] = vy;
		shsamples[i].v[2] = vz;
			
		shfunc(shsamples[i].y, vx, vy, vz);
	}

}

static int
nearest_vertex(ri_geom_t *geom, unsigned int index,
	       float u, float v)
{
	/*
	 * Find the nearest triangle vertex from the point given by 
	 * p = (1 - u - v) v0 + u v1 + v v2
	 */ 
	unsigned int i0, i1, i2;
	unsigned int nearest;

	if (index + 2 >= geom->nindices) {
		printf("geom->nindices = %d, index = %d\n",
		       geom->nindices, index); 
	}

	i0 = geom->indices[index + 0];
	i1 = geom->indices[index + 1];
	i2 = geom->indices[index + 2];

	/* todo: I don't know below is correct method... */
	if (u + v < 0.5) {
		nearest = i0;
	} else if (u > v) {
		nearest = i1;
	} else {
		nearest = i2;
	}

	return nearest;
}

static void
interreflection_pass(int npass, vtxinfo_t *infos, ri_geom_t *geom,
		     ri_htm_t *htm, int glossy)
{
	unsigned int i;
	int          j, k, l;
	double       kd;	/* diffuse reflectance			*/
	double       ks;	/* specular reflectance			*/
	double       glossness;	
	double totalpower;
	double     weight;
	shcoeff_t *curr;	/* SH coeffs in the current iteration	*/
	shcoeff_t *prev;	/* SH coeffs in the previous iteration	*/
	shcoeff_t *accum;	/* accumlated SH coeff			*/

	weight = (4.0 * M_PI) / (double)htm->npoints;

	curr = (shcoeff_t *)ri_mem_alloc(sizeof(shcoeff_t) * geom->npositions);
	for (i = 0; i < geom->npositions; i++) {
		for (j = 0; j < 25; j++) {
			for (k = 0; k < 25; k++) {
				curr[i].y[j][k] = 0.0;
			}
		}
	}

#if 0
	prev = (shcoeff_t *)ri_mem_alloc(sizeof(shcoeff_t) * geom->npositions);
	for (i = 0; i < geom->npositions; i++) {
		for (j = 0; j < 25; j++) {
			for (k = 0; k < 25; k++) {
				prev[i].y[j][k] = infos[i].y[j][k];
			}
		}
	}
#endif
		
	/*
	 * Ma = M0 + M1 + M2 + ... + Mn
	 *
	 * M0 is the SH coefficient in the shadow pass.
	 * M1 ... Mn is the SH coefficient in the i'th interreflection pass.
	 */
	accum = (shcoeff_t *)ri_mem_alloc(sizeof(shcoeff_t) * geom->npositions);
	
	/* first accumlate the SH coefficient in the shadow pass. */
	for (i = 0; i < geom->npositions; i++) {
		for (j = 0; j < 25; j++) {
			for (k = 0; k < 25; k++) {
				accum[i].y[j][k] = infos[i].y[j][k];
				//accum[i].y[j][k] = 0.0;
			}
		}
	}

	for (l = 0; l < npass; l++) {
		totalpower = 0.0;
		if (glossy) {
			printf("--- pass %d of %d: glossy interreflection ---\n", l+1, npass);
		} else {
			printf("--- pass %d of %d: diffuse interreflection ---\n", l+1, npass);
		}
		for (i = 0; i < geom->npositions; i++) {
			printf("  pass %d of %d: sampling vtx[%d] / %d \n",
			       l+1, npass, i, geom->npositions);

			if (glossy) {
				/* change this parameter as you like. */
				kd = 0.0;
				ks = 1.0;
				glossness = 4.0;

				interreflection_glossy(curr,
						       prev,
						       infos,
						       i,
						       kd,
						       ks,
						       glossness,
						       geom,
						       htm);
			} else {
				/* change this parameter as you like. */
				kd = 1.0;

				interreflection_diffuse(curr,
							prev,
							infos,
							i,
							kd,
							geom,
							htm);
			}
		}

		for (i = 0; i < geom->npositions; i++) {
			for (j = 0; j < 25; j++) {
				for (k = 0; k < 25; k++) {
					curr[i].y[j][k] *= weight;

					accum[i].y[j][k] += curr[i].y[j][k];
					prev[i].y[j][k] = curr[i].y[j][k];
				}
			}
		}
	}
	
	/* write back accumlated SH coefficient into infos */
	for (i = 0; i < geom->npositions; i++) {
		for (j = 0; j < 25; j++) {
			for (k = 0; k < 25; k++) {
				infos[i].y[j][k] = accum[i].y[j][k];
			}
		}
	}

	ri_mem_free(curr);
	ri_mem_free(prev);
	ri_mem_free(accum);

}

static void
interreflection_pass_nohtm(ri_geom_t *geom)
{
	unsigned int i;
	int          j, k;
	int          id;
	double       kd;	/* diffuse reflectance			*/
	//double       ks;	/* specular reflectance			*/
	//double       glossness;	
	double     weight;
	shcoeff_t *curr;	/* SH coeffs in the current iteration	*/
	shcoeff_t *prev;	/* SH coeffs in the previous iteration	*/
	shcoeff_t *accum;	/* accumlated SH coeff			*/

	weight = (4.0 * M_PI) / (double)g_samples;

	id = find_id_by_geom(geom);
	//curr = (shcoeff_t *)ri_mem_alloc(sizeof(shcoeff_t) * geom->npositions);
	curr = currlist[id];
	prev = prevlist[id];
	accum = accumlist[id];

	for (i = 0; i < geom->npositions; i++) {
		for (j = 0; j < 25; j++) {
			for (k = 0; k < 25; k++) {
				curr[i].y[j][k] = 0.0;
			}
		}
	}

	for (i = 0; i < geom->npositions; i++) {
		printf(" sampling vtx[%d] / %d \n", i, geom->npositions);

		/* change this parameter as you like. */
		kd = 1.0;

		interreflection_diffuse_nohtm(curr,
					i,
					kd,
					geom);
	}

	for (i = 0; i < geom->npositions; i++) {
		for (j = 0; j < 25; j++) {
			for (k = 0; k < 25; k++) {
				curr[i].y[j][k] *= weight;

				accum[i].y[j][k] += curr[i].y[j][k];
			}
		}
	}
}

static void
interreflection_diffuse(shcoeff_t *shcoeffs,
			shcoeff_t *prevcoeffs,
			vtxinfo_t *infos,
			int vtxindex,
			double kd,
			ri_geom_t *geom,
			ri_htm_t *htm)
{
	int i;

	for (i = 0; i < htm->npoints; i++ ) {
		htm->sampled[i] = 0;
	}

	/* Gather diffuse interreflection hierarchically using 
	 * hierarchical occlusion infomation.
	 */

	for (i = 1; i <= 8; i++) {
		interreflection_diffuse_hierarchical(shcoeffs,
						     prevcoeffs,
						     infos,
						     vtxindex,
						     i,
						     kd,
						     geom,
						     htm);
	}

}

static void
interreflection_diffuse_nohtm(shcoeff_t *shcoeffs,
			int vtxindex,
			double kd,
			ri_geom_t *geom)
{
	int                      i, l;
	int                      hit;
	unsigned int             qid = 0;
	double                   diffuse;
	double                   dot;
	ri_vector_t             *normal;
	ri_ray_t                 ray;
	ri_geom_t               *hitgeom;
	int                      geomid;
	ri_surface_info_t        surfinfo;

	diffuse = kd / M_PI;	/* diffuse factor */

	for (i = 0; i < g_samples; i++) {

		/*     b                           b-1
		 * (Mp)  += (Pp/Pi)(1 - Vp(sd))(Mq)   Hn(sd)
		 *     i                           i
		 *                 ~~~~~~~~~~~~
		 *                 this line is always 1
		 *
		 * sd is the ray direction.
		 * Mp is the SH coefficient vector of this point on
		 * the surface.
		 * Mq is the SH coefficient vector of the point on
		 * the surface.
		 * visible towards the direction sd.
		 * Pp is the diffuse reflectance at this point.
		 * superscript b is the bounce pass iteration.
		 * subscript i is the i'th SH coefficient.
		 * Vp(sd) is 1 if not occluded in sd direction or 0
		 * if occluded(we cull directions which are not
		 * occluded, so 1 - Vp(sd) is always 1. 
		 */

		/* Find vertex position q towards the direction sd. */
		ray.dir.e[0] = g_shsamples[i].v[0];
		ray.dir.e[1] = g_shsamples[i].v[1];
		ray.dir.e[2] = g_shsamples[i].v[2];

		normal = &(geom->normals[vtxindex]);
		dot = ri_vector_dot3(normal, &ray.dir);

		if (dot <= 0.0) {
			// under the hemisphere
			continue;
		}

		ri_vector_copy(&(ray.org), &geom->positions[vtxindex]);
		
		hit = ri_raytrace(ri_render_get(), &ray, &surfinfo);
		/* should be intersect some occluder */
		if (!hit) {
			continue;
		}

		hitgeom = surfinfo.geom;
		geomid  = find_id_by_geom(hitgeom);
		if (geomid < 0 || geomid >= ngeom) {
			printf("geomid < 0 || geomid >= ngeom");
			exit(-1);
		}
		//hitgeom = geom;

		qid = nearest_vertex(hitgeom, surfinfo.index,
				     surfinfo.u, surfinfo.v);

		for (l = 0; l < 25; l++) {
			shcoeffs[vtxindex].y[0][l] +=
				diffuse * prevlist[geomid][qid].y[0][l] * dot;
		}
	}
}

static void
interreflection_diffuse_hierarchical(shcoeff_t *shcoeffs,
				     shcoeff_t *prevcoeffs,
				     vtxinfo_t *infos,
				     int vtxindex,
				     int nodeindex,
				     double kd,
				     ri_geom_t *geom,
				     ri_htm_t *htm)
{
	int                      i, l;
	int                      hit;
	unsigned int             pid;
	unsigned int             qid = 0;
	double                   diffuse;
	double                   dot;
	ri_vector_t             *normal;
	ri_htm_occlusion_node_t *occnode;
	ri_ray_t                 ray;
	ri_geom_t               *hitgeom;
	ri_surface_info_t        surfinfo;

	occnode = &(infos[vtxindex].occlusion->nodes[nodeindex]);

	if (!occnode->nodeocclusion) {
		/* no occlusion in this node hierarchy */
		return;
	}

	if (!htm->nodes[nodeindex].child[0]) {	/* leaf node */

		diffuse = kd / M_PI;	/* diffuse factor */

		for (i = 0; i < htm->nodes[nodeindex].npoints; i++) {
			if (!occnode->occlusions[i]) {
				/* i'th sampling direction in this node 
				 * does not hit any occluder.
				 */
				continue;
			}

			if (htm->sampled[nodeindex]) {
				/* this sampling direction is already
				 * used before by another node.
				 */
				continue;
			}

			/*     b                           b-1
			 * (Mp)  += (Pp/Pi)(1 - Vp(sd))(Mq)   Hn(sd)
			 *     i                           i
			 *                 ~~~~~~~~~~~~
			 *                 this line is always 1
			 *
			 * sd is the ray direction.
			 * Mp is the SH coefficient vector of this point on
			 * the surface.
			 * Mq is the SH coefficient vector of the point on
			 * the surface.
			 * visible towards the direction sd.
			 * Pp is the diffuse reflectance at this point.
			 * superscript b is the bounce pass iteration.
			 * subscript i is the i'th SH coefficient.
			 * Vp(sd) is 1 if not occluded in sd direction or 0
			 * if occluded(we cull directions which are not
			 * occluded, so 1 - Vp(sd) is always 1. 
			 */
		
			/* Find vertex position q towards the direction sd. */
			pid = htm->nodes[nodeindex].p[i];
			ri_vector_copy(&(ray.dir), &htm->points[pid]);
			ri_vector_copy(&(ray.org), &geom->positions[vtxindex]);
			
			hit = ri_raytrace(ri_render_get(), &ray, &surfinfo);
			/* should be intersect some occluder */
			if (!hit) {
				printf("??? ray should be intersect");
				continue;
			}

			normal = &(geom->normals[vtxindex]);
			dot = ri_vector_dot3(normal, &ray.dir);

			if (dot <= 0.0) {
				printf("??? dot should not be <=0. dot = %f\n",
									dot);
			}

			//hitgeom = surfinfo.material->geom;
			hitgeom = geom;

			qid = nearest_vertex(hitgeom, surfinfo.index,
					     surfinfo.u, surfinfo.v);

			for (l = 0; l < 25; l++) {
				shcoeffs[vtxindex].y[0][l] +=
					diffuse * prevcoeffs[qid].y[0][l] * dot;
			}

			
			/* mark this sampling direction used */
			htm->sampled[nodeindex] = 1;	 
		}
	}

	/* non-leaf node */

	/* traverse children */
	for (i = 0; i < 4; i++) {
		interreflection_diffuse_hierarchical(
					shcoeffs,
					prevcoeffs,
					infos,
					vtxindex,
					htm->nodes[nodeindex].child[i],
					kd,
					geom,
					htm);
	}
}

static void
interreflection_glossy(shcoeff_t *shcoeffs,
		       shcoeff_t *prevcoeffs,
		       vtxinfo_t *infos,
		       int vtxindex,
		       double kd,
		       double ks,
		       double glossness,
		       ri_geom_t *geom,
		       ri_htm_t *htm)
{
	int i;

	for (i = 0; i < htm->npoints; i++ ) {
		htm->sampled[i] = 0;
	}

	/* Gather glossy interreflection hierarchically using 
	 * hierarchical occlusion infomation.
	 */

	for (i = 1; i <= 8; i++) {
		interreflection_glossy_hierarchical(shcoeffs,
						    prevcoeffs,
						    infos,
						    vtxindex,
						    i,
						    kd,
						    ks,
						    glossness,
						    geom,
						    htm);
	}

}

static void
interreflection_glossy_hierarchical(shcoeff_t *shcoeffs,
				    shcoeff_t *prevcoeffs,
				    vtxinfo_t *infos,
				    int vtxindex,
				    int nodeindex,
				    double kd,
				    double ks,
				    double glossness,
				    ri_geom_t *geom,
				    ri_htm_t *htm)
{
	int                      i, j, k;
	int                      hit;
	unsigned int             pid;
	unsigned int             qid;
	ri_vector_t              ref;
	ri_vector_t             *normal;
	float                    conv;
	double                   shy[25];
	float                    yr[25];
	float                    ys[25];

	ri_htm_occlusion_node_t *occnode;
	ri_ray_t                 ray;
	ri_surface_info_t        surfinfo;

	occnode = &(infos[vtxindex].occlusion->nodes[nodeindex]);

	if (!occnode->nodeocclusion) {
		/* no occlusion in this node hierarchy */
		return;
	}

	if (!htm->nodes[nodeindex].child[0]) {	/* leaf node */

		for (i = 0; i < htm->nodes[nodeindex].npoints; i++) {
			if (!occnode->occlusions[i]) {
				/* i'th sampling direction in this node 
				 * does not hit any occluder.
				 */
				continue;
			}

			if (htm->sampled[nodeindex]) {
				/* this sampling direction is already
				 * used before by another node.
				 */
				continue;
			}

			/*     b                
			 * (Mp)  += (1 - Vp(sd))
			 *     jk               
			 *          ~~~~~~~~~~~~
			 *           this line is always 1
			 *
			 *                   *     b-1
			 *       *  (sum a  G  (Mq)   y (ref(-sd, Nq)) y (sd)
			 *            l   l  l     lk   l               j 
			 *
			 * sd is the ray direction.
			 * Mp is the SH coefficient matrix of this point on
			 * the surface.
			 * Mq is the SH coefficient matrix of the point on
			 * the surface visible towards the direction sd.
			 * superscript b is the bounce pass iteration.
			 * subscript i is the i'th SH coefficient.
			 * ak is the convolution constant.
			 * G* is the SH projected glossy BRDF.
			 * Vp(sd) is 1 if not occluded in sd direction or 0
			 * if occluded(we cull directions which are not
			 * occluded, so 1 - Vp(sd) is always 1. 
			 */
		
			/* Find vertex position q towards the direction sd. */
			pid = htm->nodes[nodeindex].p[i];
			ri_vector_copy(&(ray.dir), &htm->points[pid]);
			ri_vector_copy(&(ray.org), &geom->positions[vtxindex]);
			
			hit = ri_raytrace(ri_render_get(), &ray, &surfinfo);
			/* should be intersect some occluder */
			if (!hit) {
				printf("??? ray should be intersect");
				continue;
			}

			/* Y(sd) */
			shfunc(shy, ray.dir.e[0], ray.dir.e[1], ray.dir.e[2]);
			for (j = 0; j < 25; j++) {
				ys[j] = (float)shy[j];
			}

			qid = nearest_vertex(geom, surfinfo.index,
					     surfinfo.u, surfinfo.v);
			normal = &(geom->normals[vtxindex]);

			/* ref = reflect(sd, N) */
			//ri_vector_negate(&(ray.dir));
			ri_reflect(&ref, &ray.dir, normal);

			/* Y(ref) */
			shfunc(shy, ref.e[0], ref.e[1], ref.e[2]);
			for (j = 0; j < 25; j++) {
				yr[j] = (float)shy[j];
			}

			for (j = 0; j < 25; j++) {
				for (k = 0; k < 25; k++) {
					conv = g_convolve(prevcoeffs[qid].y,
							  yr,
							  k,
							  kd, ks, glossness);
							  
					shcoeffs[vtxindex].y[j][k] +=
						conv * ys[j];
				}
			}

			
			/* mark this sampling direction used */
			htm->sampled[nodeindex] = 1;	 
		}
	}

	/* non-leaf node */

	/* traverse children */
	for (i = 0; i < 4; i++) {
		interreflection_glossy_hierarchical(
					shcoeffs,
					prevcoeffs,
					infos,
					vtxindex,
					htm->nodes[nodeindex].child[i],
					kd,
					ks,
					glossness,
					geom,
					htm);
	}
}

static void
//write_data(vtxinfo_t *infos, int nvertices, int glossy, int objid)
write_data(shcoeff_t *infos, int nvertices, int glossy, int objid)
{
	FILE *fp;

	int  i, j, k;
	char type;
	int   ival;
	float fval;
	char  fname[1024];

	sprintf(fname, "prt%d.dat", objid);

	fp = fopen(fname, "wb");
	if (!fp) {
		fprintf(stderr, "can't open file [ %s ]\n", fname);
		exit(-1);
	}
 
	if (glossy) {
		type = 'g';	/* glossy */
	} else {
		type = 'd';	/* diffuse */
		printf("diffuse\n");
	}
	fwrite(&type, sizeof(char), 1, fp);


	ival = nvertices;
	endconv(&ival);
	fwrite(&ival, sizeof(int), 1, fp);

	for (i = 0; i < nvertices; i++) {
		/* write out vertex number */
		printf("\nvtx = %d\n", i);
		ival = i;
		endconv(&ival);
		fwrite(&ival, sizeof(int), 1, fp);
		
		if (glossy) {
			for (j = 0; j < 25; j++) {
				for (k = 0; k < 25; k++) {
					fval = infos[i].y[j][k];
					endconv(&fval);
					fwrite(&fval, sizeof(float), 1, fp);
				}
			}
		} else { 
			for (j = 0; j < 25; j++) {
				fval = infos[i].y[0][j];
				endconv(&fval);
				fwrite(&fval, sizeof(float), 1, fp);

				printf("%f ", infos[i].y[0][j]);
			}
		}
	}

	fclose(fp);
}

#if 0
static int
nocclusionpoints(ri_htm_occlusion_t *occlusion, ri_htm_t *htm)
{
	int i, j;
	int nocc = 0;

	for (i = 1; i <= htm->nnodes; i++) {
		if (htm->nodes[i].child[0]) continue;

		for (j = 0; j < htm->nodes[i].npoints; j++) {
			if (occlusion->nodes[i].occlusions[j]) {
				nocc++;
			}
		}
	}

	return nocc;
}
#endif

static void
g_kern(float g[25], float kd, float ks, float glossness)
{
	static int initialized = 0;
	static float gloss_kernel_coeff[25];
	

	if (!initialized) {
		calc_gloss_kern_coeff(gloss_kernel_coeff,
				      ks, kd, glossness);

		initialized = 1;
	}

	g[0] = gloss_kernel_coeff[0];

	g[1] = g[2] = g[3] = gloss_kernel_coeff[2];

	g[4] = g[5] = g[6] = g[7] = g[8] = gloss_kernel_coeff[6];

	g[9] = g[10] = g[11] = g[12] = g[13] = g[14] = g[15] = 
		gloss_kernel_coeff[12];

	g[16] = g[17] = g[18] = g[19] = g[20] = g[21] = g[22] = g[23] = g[24] =
		gloss_kernel_coeff[20];
}

static void
calc_gloss_kern_coeff(float gloss_kern_coeff[25],
		      float kd, float ks, float glossness)
{
	/* Project glossy BRDF(we use Modified Phong BRDF)
	 * with spherical harmonics.
	 * There may analytic integration formular exits, but
	 * I don't figure out it. so I use numerical integration
	 * (quasi-Monte Carlo integration) of glossy BRDF. */
	
	const int nsamples = 10000;
	int i, k;
	float dot;
	float w;
	float r[3];
	float n[3];
	float v[3];
	float brdf;
	double *samples;
	double shy[25];

	for (k = 0; k < 25; k++) {
		gloss_kern_coeff[k] = 0.0;
	}

	/* normal = (0, 0, 1) */
	n[0] = 0.0; n[1] = 0.0; n[2] = 1.0;

	samples = (double *)malloc(sizeof(double) * nsamples * 3);

	/* Generate uniformly distributed sampling points using
	 * Hammersley points on sphere.
	 */ 
	hammersley2_sphere(samples, nsamples);

	for (i = 0; i < nsamples; i++) {
		v[0] = samples[3 * i + 0];	
		v[1] = samples[3 * i + 1];	
		v[2] = samples[3 * i + 2];	
		
		/* in practice, we don't need to calculate x and y.
		 * so coefficient is non-zero only m = 0.
		 */

		/* dot product of v and n(=(0, 0, 1)) */
		dot = v[2];

		if (dot <= 0.0) continue;	/* under the hemisphere */

		brdf = modified_phong(r, v, n, kd, ks, glossness);

		shfunc(shy, 0.0, 0.0, v[2]);

		for (k = 0; k < 25; k++) {
			gloss_kern_coeff[k] += brdf * dot * shy[k];
		}
		
	}

	w = (4.0 * M_PI) / (float)nsamples;

	for (k = 0; k < 25; k++) {
		gloss_kern_coeff[k] *= w;
		printf("g[%d] = %f\n", k, gloss_kern_coeff[k]);
	}

	free(samples);
}

static void
refvec(float r[3], float in[3], float n[3])
{
	float dot;
	float len;

	/*
	 * r = 2n(in . n) - in
	 */

	dot = in[0] * n[0] + in[1] * n[1] + in[2] * in[2];
	r[0] = 2.0 * dot * n[0] - in[0];
	r[1] = 2.0 * dot * n[1] - in[1];
	r[2] = 2.0 * dot * n[2] - in[2];

	len = sqrt(r[0] * r[0] + r[1] * r[1] + r[2] * r[2]);
	if (len != 0.0) len = 1.0 / len;

	r[0] *= len;
	r[1] *= len;
	r[2] *= len;
}

static float
modified_phong(float r[3], float v[3], float n[3],
	       float kd, float ks, float glossness)
{
	float brdf;
	float r_dot_v;
	
	/* Modified Phong BRDF */

	refvec(r, v, n);

	r_dot_v = r[0] * v[0] + r[1] * v[1] + r[2] * r[2];
	if (r_dot_v <= 0.0) r_dot_v = 0.0;

	brdf = kd / M_PI +
	       ks * ((glossness + 2.0) / (2.0 * M_PI)) *
	       pow(r_dot_v, glossness);

	return brdf;
}

static void
alpha_coeff(float alpha[25])
{
	int i, j;
	static float k[5];
	static int initialized = 0;

	if (!initialized) {
		for (i = 0; i < 5; i++) {
			k[i] = sqrt((4.0 * M_PI) / (double)(2 * i + 1));
			printf("a[%d] = %f\n", i, k[i]);
		}
		
		initialized = 1;
	}

	for (i = 0; i < 5; i++) {
		for (j = i; j < (i + 1) * (i + 1); j++) {
			alpha[j] = k[i];
		}
	}
}

static float
g_convolve(float m[25][25], float y[25], int j,
	   float kd, float ks, float glossness)
{
	int k;
	float a[25];
	float g[25];
	float sum;

	/*    25          *       
	 * = sum a_{k} * G_{k} * M_{k,j} * y_{k}
	 *   k=1  
	 */

	sum = 0.0;

	alpha_coeff(a);
	g_kern(g, kd, ks, glossness);

	for (k = 0; k < 25; k++) {
		sum += a[k] * g[k] * m[k][j] * y[k];
	}

	return sum;
}

static int
find_id_by_geom(ri_geom_t *geom)
{
	int i;
	
	for (i = 0; i < ngeom; i++) {
		if (geom == geomtable[i]) {
			return i;
		}
	}

	exit(-1);
}
