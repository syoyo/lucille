/*
 * Photon mapping implementation
 *
 * $Id: photonmap.c,v 1.7 2004/06/13 06:44:51 syoyo Exp $
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>

#include "vector.h"
#include "geom.h"
#include "log.h"
#include "memory.h"
#include "photonmap.h"
#include "random.h"
#include "polygon.h"
#include "light.h"
#include "raytrace.h"
#include "reflection.h"
#include "timer.h"
#include "rgbe.h"
#include "parallel.h"
#include "qmc.h"


/*
 * - Implementation Note. -
 *
 * For details of photong mapping, see
 *
 * Henrik Wann Jensen,
 * "Realistic Image Synthesis Using Photon Mapping".
 *
 * For details of quasi-Monte Carlo photon tracing, see
 *
 * Alexander Keller,
 * "Strictly deterministic sampling methods in computer graphics"
 * (mental images technical report, 2001)
 *       in "Monte Carlo Ray Tracing", SIGGRAPH'2003 Course #44.
 *
 */


#define DUMP_RAYPATH 1

#ifndef RI_PI
#define RI_PI 3.141592
#endif

#ifndef fabsf
#define fabsf(x) (float)fabs((double)(x))
#endif

#define NPHOTONBUF 1024

/* Photon record buffer for parallel photon shooting using MPI. */
typedef struct _photonbuf_t
{
	ri_photon_t photons[NPHOTONBUF];
	char        ptypes[NPHOTONBUF];
	int         nphotons;
} photonbuf_t;

static ri_photonmap_option_t *g_photonmap_option = NULL;

static void median_split_intl   (ri_photon_t **p,
			         const int start,
			         const int end,
			         const int median,
			         const int axis);

static void balance_segment_intl(ri_photonmap_t *map,
				 ri_photon_t **pbal,
			 	 ri_photon_t **porg,
				 const int index,
				 const int start,
				 const int end);

static void trace_photon(ri_ray_t *ray, ri_vector_t *energy, ri_surface_info_t *surfinfo, ri_vector_t *pdir, int *hit, const int maxdepth);

static void vector_to_polar(const ri_vector_t *vec, unsigned char *phi, unsigned char *theta);
static void polar_to_vector(ri_vector_t *vec, unsigned char phi, unsigned char theta);
static double gaussian_filter(float dist, float radius);
//static double cone_filter(float dist, float radius);
static double calc_area(ri_geom_t *geom);
static void dump_raypath_init();
static void dump_raypath_close();
static void dump_raypath_start(const ri_vector_t *p);
static void dump_raypath_track(const ri_vector_t *p);
static void dump_raypath_end(char type);
static void save_photomap(const char *filename, ri_photonmap_t *pmap);

static FILE *raypathfp;
static int   ndepth;
static ri_vector_t raypath[1024];

/*
 * max heap util(from MegaPOV & RenderPark)
 */
typedef struct _nearest_photons_t
{
	ri_photon_t **nearest_photons;
	float        *dist2;			/* squared distance array  */
	int           nfound;			/* number of found photons */
	int           nmax;			/* heap maximum size	   */
	float         maxdist;         
	ri_vector_t   query_pos;
	short         gotheap;			/* heap is build?	   */
} nearest_photons_t;

static nearest_photons_t *nearest_photons_new    (int maxsize, float maxradius);
static void               nearest_photons_free   (nearest_photons_t *np);
static void               nearest_photons_insert (nearest_photons_t *np, ri_photon_t *pindex, float dist2);
static void               nearest_photons_replace(nearest_photons_t *np, ri_photon_t *pindex, float dist2);
static void               nearest_photons_fixup  (nearest_photons_t *np);
static void               nearest_photons_fixdown(nearest_photons_t *np);

static void locate_photons(ri_photonmap_t *map, nearest_photons_t *np, int index);
static void locate_photons_jensen(ri_photonmap_t *map, nearest_photons_t *np, int index);
static void locate_photons_jensen_iterative(ri_photonmap_t *map,
					    nearest_photons_t *np);
static void locate_nearest_photon_with_normal(ri_photonmap_t  *map,
					      ri_photon_t    **nearest,
					      const ri_vector_t *pos,
					      const ri_vector_t *normal,
					      float           *dist2,
					      int index);

static void precompute_irradiance(ri_photonmap_t *map);
static void update_np(nearest_photons_t *np, ri_photon_t *p);

/*
 * Function: ri_photon_new
 *
 *     Creates new photon data.
 *
 * Parameters:
 *
 *     None:
 *
 * Returns:
 *
 *     Pointer to newly created photon data.
 */
ri_photon_t *
ri_photon_new()
{
	ri_photon_t *p = NULL;

	p = (ri_photon_t *)ri_mem_alloc(sizeof(ri_photon_t));

	p->pos.e[0] = p->pos.e[1] = p->pos.e[2] = 0;
	p->phi = 0; p->theta = 0;
	p->plane = 0;
	p->power[0] = p->power[1] = p->power[2] = 0;
	p->irrad[0] = p->irrad[1] = p->irrad[2] = p->irrad[3] = 0;
	p->nphi = 0; p->ntheta = 0;


	return p;
}

/*
 * Function: ri_photon_free
 *
 *     Frees the memory of photon data.
 *
 * Parameters:
 *
 *     *photon - a photon data to be freed.
 *
 * Returns:
 *
 *     None.
 */
void
ri_photon_free(ri_photon_t *photon)
{
	ri_mem_free(photon);
}

/*
 * Function: ri_photonmap_new
 *
 *     Creates new photon map data. photon map contains many of photons.
 *
 * Parameters:
 *
 *     None:
 *
 * Returns:
 *
 *     Pointer to newly created photon map data.
 */
ri_photonmap_t *
ri_photonmap_new()
{
	ri_photonmap_t *p = NULL;

	p = (ri_photonmap_t *)ri_mem_alloc(sizeof(ri_photonmap_t));

	p->tmp_array   = ri_array_new(sizeof(ri_photon_t));
	p->photons     = NULL;
	p->nphotons    = 0;
	p->balanced    = 0;
	p->bbox_min[0] = p->bbox_min[1] = p->bbox_min[2] = 1e8f;
	p->bbox_max[0] = p->bbox_max[1] = p->bbox_max[2] = -1e8f;

	return p;
}

void
ri_photonmap_free(ri_photonmap_t *map)
{
	if (map == NULL) return;

	ri_mem_free(map->tmp_array);
	ri_mem_free(map->photons);
	ri_mem_free(map);
}

void
ri_photonmap_add_photon(ri_photonmap_t *map, ri_photon_t *photon)
{
	int i;

	/* photon array is start with 1 */
	map->nphotons++;
	ri_array_insert(map->tmp_array, map->nphotons, photon);

	/* bounding box update */
	for (i = 0; i < 3; i++) {
		if (photon->pos.e[i] < map->bbox_min[i]) {
			map->bbox_min[i] = photon->pos.e[i];
		}

		if (photon->pos.e[i] > map->bbox_max[i]) {
			map->bbox_max[i] = photon->pos.e[i];
		}
	}
}

void
ri_photonmap_balance(ri_photonmap_t *map)
{
	ri_photon_t **ptmp1, **ptmp2;	/* temporary array */
	ri_photon_t   foo_photon;
	unsigned int           i;
	unsigned int           d, j, foo;

	ri_log_and_return_if(map->balanced);
	ri_log_and_return_if(map->nphotons <= 1);

	map->balanced = 1;

	map->photons = (ri_photon_t *)ri_mem_alloc(sizeof(ri_photon_t) *
						      (map->nphotons + 1));
	for (i = 1; i <= map->nphotons; i++) {
		map->photons[i] = *(ri_photon_t *)ri_array_at(map->tmp_array, i);
	}
	
	/* [0] is not used. */
	map->photons[0].pos.e[0] = 0.0;
	map->photons[0].pos.e[1] = 0.0;
	map->photons[0].pos.e[2] = 0.0;

	ri_mem_free(map->tmp_array);
	map->tmp_array = NULL;
	
	ptmp1 = (ri_photon_t **)ri_mem_alloc(sizeof(ri_photon_t *) *
						(map->nphotons + 1));
	ptmp2 = (ri_photon_t **)ri_mem_alloc(sizeof(ri_photon_t *) *
						(map->nphotons + 1));

	for (i = 0; i <= map->nphotons; i++) {
		ptmp2[i] = &(map->photons[i]);
	}

	balance_segment_intl(map, ptmp1, ptmp2, 1, 1, map->nphotons);

	ri_mem_free(ptmp2);

	/* reorganize balanced kd-tree(make a heap) */
	j = 1; foo = 1;
	foo_photon = map->photons[j];

	for (i = 1; i <= map->nphotons; i++) {
		d = ptmp1[j] - map->photons;
		ptmp1[j] = NULL;

		if (d != foo) {
			map->photons[j] = map->photons[d];
		} else {
			map->photons[j] = foo_photon;

			if (i < map->nphotons) {
				for (; foo <= map->nphotons; foo++) {
					if (ptmp1[foo] != NULL) break;
				}

				foo_photon = map->photons[foo];
				j          = foo;
			}

			continue;
		}

		j = d;
	}

	ri_mem_free(ptmp1);
}

void
ri_photonmap_emit_photons(ri_photonmap_t *cmap, ri_photonmap_t *gmap,
			  ri_light_t *light)
{
	unsigned int        i;
	unsigned int        stored_photons;
	int                 j, k;
	int                 hit;
	int                 maxdepth;
	ri_vector_t         energy;
	ri_vector_t         photondir;
	ri_surface_info_t   surfinfo;
	ri_photon_t       **photons;
	ri_ray_t            ray;
	float               wattscale;
	double              area;
	unsigned char       theta, phi;
	ri_photonmap_option_t *opt;
	char               *ptypes;		/* 'G' or 'C' */

	// for parallel photon shooting
	int                 np;
	int                 myid;
	int                 nloops;
	int                 currphoton;
	photonbuf_t         localphotonbuf;
	photonbuf_t        *globalphotonbuf;
	ri_photon_t        *photon, *srcphoton;
	char               *ptype;
	char                msg[1024];
	
	
	opt = ri_photonmap_get_option();
	
	photons = (ri_photon_t **)ri_mem_alloc(sizeof(ri_photon_t *) *
					       opt->nphotons);

	ptypes   = (char *)ri_mem_alloc(sizeof(char) * opt->nphotons);

	for (i = 0; i < opt->nphotons; i++) {
		photons[i] = ri_photon_new();
		ptypes[i] = '\0';
	}

	np = ri_parallel_ntasks();
	myid = ri_parallel_taskid();

	globalphotonbuf = (photonbuf_t *)ri_mem_alloc(sizeof(photonbuf_t) * np);

	nloops = (int)(ceil((double)opt->nphotons / (np * NPHOTONBUF)));

#if DEBUG
	if (myid == 0) {
		printf("[ %s ] emitting %d photons.\n", __FILE__, opt->nphotons);
	}
#endif

	maxdepth = ri_render_get()->context->option->max_ray_depth;

	area = calc_area(light->geom);

#ifdef DUMP_RAYPATH
	if (myid == 0) dump_raypath_init();
#endif

	stored_photons = 0;

	// This actually generates different random number on each PE?
	//seedMT((unsigned long)(4357 - myid));

	ri_parallel_barrier();
	ri_timer_start(ri_render_get()->context->timer, "Photonmap | Emit photon");


	for (i = 0; i < (unsigned int)nloops; i++) {

		localphotonbuf.nphotons = 0;
		
		for (j = 0; j < NPHOTONBUF; j++) {
			if (NPHOTONBUF * i + j >= opt->nphotons) {
				// no more photons to shoot.
				printf("no more photon of rank(%d): photon num = %d\n", myid,
					NPHOTONBUF * i + j);
				break;
			}

			ri_light_sample_pos_and_dir(light,
						    &(ray.org), &(ray.dir));

			energy.e[0] = opt->wattage.e[0] * (float)area;
			energy.e[1] = opt->wattage.e[1] * (float)area;
			energy.e[2] = opt->wattage.e[2] * (float)area;

			hit = 0;
			ray.nbound_diffuse = 0;
			ray.nbound_specular = 0;

			/* QMC: Ray's instance number. */
			ray.i = j + i * nloops + myid * (NPHOTONBUF * nloops);

			/* Dimension 1 - 4 are already assigned for
			 * light source sampling.
			 */
			ray.d = 5;	
#ifdef DUMP_RAYPATH
			if (myid == 0) dump_raypath_start(&ray.org);
#endif

			ray.prev_hit = 'L';		/* Light	*/
			trace_photon(&ray, &energy, &surfinfo, &photondir,
				     &hit, maxdepth);

#ifdef DUMP_RAYPATH
			if (myid == 0) dump_raypath_end(ray.prev_hit);
#endif

			if (hit) {
				vector_to_polar(&photondir, &phi, &theta);

				currphoton = localphotonbuf.nphotons;

				photon = &(localphotonbuf.photons[currphoton]);
				photon->phi   = phi;
				photon->theta = theta;
				ri_vector_copy(&photon->pos, &surfinfo.pos);
				photon->power[0] = energy.e[0];
				photon->power[1] = energy.e[1];
				photon->power[2] = energy.e[2];

				vector_to_polar(&surfinfo.normal, 
						&(photon->nphi),
						&(photon->ntheta));

				ptype = &(localphotonbuf.ptypes[currphoton]);

				if (ray.prev_hit == 'S' &&
				    ray.nbound_diffuse == 0) {
					*ptype = 'C';	/* caustics photon */
				} else {
					*ptype = 'G';	/* global photon */
				}
					
				localphotonbuf.nphotons++;
			} // if hit
		} // j = NPHOTONBUF loop

		// syncronize
		ri_parallel_barrier();

		ri_parallel_gather(&localphotonbuf,
				   globalphotonbuf,
				   sizeof(photonbuf_t));

		if (myid != 0)  continue;

		for (j = 0; j < np; j++) {
			for (k = 0; k < globalphotonbuf[j].nphotons; k++) {
				if (stored_photons >= opt->nphotons)
					break;

				photon = photons[stored_photons];
				srcphoton = &(globalphotonbuf[j].photons[k]);

				// copy photon info.
				ri_mem_copy(photon,
					    srcphoton,
					    sizeof(ri_photon_t));

				ptypes[stored_photons] =
					globalphotonbuf[j].ptypes[k];

				stored_photons++;
			}
		}

		printf("Emitted %d photons.\r", stored_photons);
		fflush(stdout);

	}

	printf("\n");

#ifdef DUMP_RAYPATH
	if (myid == 0) dump_raypath_close();
#endif

	ri_parallel_barrier();
	ri_timer_end(ri_render_get()->context->timer, "Photonmap | Emit photon");


	if (myid == 0) {
		wattscale = 1.0 / opt->nphotons;
		sprintf(msg, "watt scale = %f", wattscale);
		ri_log(LOG_INFO, msg);

		for (i = 0; i < stored_photons; i++) {
			photons[i]->power[0] *= wattscale;
			photons[i]->power[1] *= wattscale;
			photons[i]->power[2] *= wattscale;

			if (ptypes[i] == 'C') {
				ri_photonmap_add_photon(cmap, (void *)photons[i]);
			} else {
				ri_photonmap_add_photon(gmap, (void *)photons[i]);
			}
		}

		for (i = 0; i < opt->nphotons; i++) {
			ri_photon_free(photons[i]);
		}
		ri_mem_free(photons);

		/* optimize photonmap */
		ri_photonmap_balance(gmap);
		ri_photonmap_balance(cmap);


		if (opt->precompute_irradiance) {
			printf("precompute raidnace\n");
			precompute_irradiance(gmap);
			precompute_irradiance(cmap);
		}

		save_photomap("global_photonmap.dat", gmap);
		save_photomap("caustics_photonmap.dat", cmap);

#if DEBUG
		printf("[ %s ] emitted %d photons.\n", __FILE__, stored_photons);
		printf("[ %s ] %d global photons.\n", __FILE__, (int)gmap->nphotons);
		printf("[ %s ] %d caustics photons.\n", __FILE__, (int)cmap->nphotons);
		printf("[ %s ] emit photon time| %f\n", __FILE__, ri_timer_elapsed(ri_render_get()->context->timer, "Emit photon"));
		printf("[ %s ] bbox_min = %f, %f, %f\n", __FILE__, gmap->bbox_min[0], gmap->bbox_min[1], gmap->bbox_min[2]);
		printf("[ %s ] bbox_max = %f, %f, %f\n", __FILE__, gmap->bbox_max[0], gmap->bbox_max[1], gmap->bbox_max[2]);
		printf("[ %s ] wattage = %f, %f, %f\n", __FILE__, opt->wattage.e[0], opt->wattage.e[1], opt->wattage.e[2]);
		printf("[ %s ] max_gather_radius = %f\n", __FILE__, opt->max_gather_radius);
		printf("[ %s ] max_gather_photons = %u\n", __FILE__, opt->max_gather_photons);

#endif

	}

	// syncronize photonmap data.
	ri_parallel_barrier();

	printf("rank(%d): syncronizing photonmap...\n", myid);
	ri_parallel_bcast(cmap, sizeof(ri_photonmap_t));
	ri_parallel_bcast(gmap, sizeof(ri_photonmap_t));

	if (myid != 0) {
		cmap->tmp_array = NULL;
		gmap->tmp_array = NULL;

		printf("rank(%d): cmap->nphotons = %d\n", myid, (int)cmap->nphotons);
		cmap->photons = (ri_photon_t *)
					ri_mem_alloc(sizeof(ri_photon_t) *
						     cmap->nphotons);
		printf("rank(%d): gmap->nphotons = %d\n", myid, (int)gmap->nphotons);
		gmap->photons = (ri_photon_t *)
					ri_mem_alloc(sizeof(ri_photon_t) *
						     gmap->nphotons);
	}

	ri_parallel_bcast(cmap->photons,
			  sizeof(ri_photon_t) * cmap->nphotons);
	ri_parallel_bcast(gmap->photons,
			  sizeof(ri_photon_t) * gmap->nphotons);

	ri_parallel_barrier();

	printf("rank(%d): syncronized photonmap\n", myid);
}

ri_photonmap_option_t *
ri_photonmap_get_option()
{
	if (!g_photonmap_option) {
		g_photonmap_option = 
			(ri_photonmap_option_t *)
			ri_mem_alloc(sizeof(ri_photonmap_option_t));

		g_photonmap_option->nphotons = 10000;
		g_photonmap_option->wattage.e[0] = 1000;
		g_photonmap_option->wattage.e[1] = 1000;
		g_photonmap_option->wattage.e[2] = 1000;
		g_photonmap_option->max_gather_radius = 2.5;
		g_photonmap_option->max_gather_photons = 20;
		g_photonmap_option->precompute_irradiance = 0;
	}

	return g_photonmap_option;
}


void
ri_photonmap_estimate_irradiance(ri_photonmap_t *map, ri_vector_t *irrad,
				 const ri_vector_t *pos, const ri_vector_t *normal,
				 float radius, int maxphoton)
{

	int i;
	nearest_photons_t *np;
	ri_vector_t        pdir;
	ri_vector_t        ppos;
	float              inva;
	double             w;
	double             rr;
	float              dot;
	float              ellipfactor;

	if (map->nphotons < 1) return;
	np = nearest_photons_new(maxphoton, radius);

	ri_vector_copy(&(np->query_pos), pos);
	rr = radius * radius;
	np->dist2[0] = rr;

	//locate_photons_jensen(map, np, 1);
	locate_photons_jensen_iterative(map, np);

	irrad->e[0] = 0.0;
	irrad->e[1] = 0.0;
	irrad->e[2] = 0.0;

	if (np->nfound) {	
		for (i = 1 ; i <= np->nfound; i++) {
			polar_to_vector(&pdir, np->nearest_photons[i]->phi,
					       np->nearest_photons[i]->theta);

			dot = ri_vector_dot3(&pdir, normal);
			if (dot > 0.0) {
				/* To prevent "false photons" at sharp
				 * corners, use an ellipsoid insterd of
				 * a sphere. */

				ri_vector_sub(&ppos, &np->nearest_photons[i]->pos, pos);
				dot = ri_vector_dot3(normal, &ppos);
				ellipfactor = fabsf(dot);
				if (ellipfactor > radius * 0.1) {
					continue;
				}

				
				w = gaussian_filter(np->dist2[i], rr);
				//w = cone_filter(np->dist2[i], rr);
				//w = 1.0;
				//printf("w = %f\n", w);
				irrad->e[0] += np->nearest_photons[i]->power[0] * w;
				irrad->e[1] += np->nearest_photons[i]->power[1] * w;
				irrad->e[2] += np->nearest_photons[i]->power[2] * w;
			}
		}

		inva = 1.0 / (RI_PI * rr);

		ri_vector_scale(irrad, inva);

	}

	nearest_photons_free(np);
}

void
ri_photonmap_estimate_precomputed_irradiance(ri_photonmap_t *map,
					     ri_vector_t *irrad,
				 	     const ri_vector_t *pos,
					     const ri_vector_t *normal)
{
	float dist2;
	ri_photon_t *nearest;
	ri_photonmap_option_t *pmapopt;

	if (map->nphotons < 1) return;

	pmapopt = ri_photonmap_get_option();

	dist2 = pmapopt->max_gather_radius * pmapopt->max_gather_radius;

	nearest = NULL;
	locate_nearest_photon_with_normal(map, &nearest, pos, normal, &dist2, 1);

	if (nearest) {	
		rgbe2float(&(irrad->e[0]), &(irrad->e[1]), &(irrad->e[2]),
			   nearest->irrad);
	} else {
		ri_vector_zero(irrad);
	}
}

/* --- private functions --- */
static void
trace_photon(ri_ray_t *ray, ri_vector_t *energy, ri_surface_info_t *surfinfo, ri_vector_t *pdir, int *hit, const int maxdepth)
{
	int         rayhit;
	int         depth;
	//float       eta = 1.0 / 1.52;		/* glass */
	float       eta, ior_air = 1.0f, ior_glass = 1.4;
	double      d, s, t, r;
	int       **perm;
	
	depth = ray->nbound_diffuse + ray->nbound_specular;
	if (depth >= maxdepth) return;
	
	rayhit = ri_raytrace(ri_render_get(), ray, surfinfo);

	/* incoming photon direction = -(ray direction) */
	ri_vector_copy(pdir, &ray->dir);
	ri_vector_neg(pdir);

	/* Preomputed permutation table for QMC. */
	perm = ri_render_get()->perm_table;

	if (rayhit) {

#ifdef DUMP_RAYPATH
		dump_raypath_track(&surfinfo->pos);
#endif

		(*hit) = 1;

		d = ri_vector_ave(&surfinfo->geom->material->kd);
		s = ri_vector_ave(&surfinfo->geom->material->ks);
		t = ri_vector_ave(&surfinfo->geom->material->kt);

		r = randomMT();	/* Monte Carlo */
		//r = generalized_scrambled_halton(ray->i, 0, ray->d, perm);
		//(ray->d)++;

		if (d + s + t > 1.0) {
			ri_log(LOG_WARN, "d+s+t > 1.0");
		}

		/*
		 * russian roulette.
		 * we assume (d + s) <= 1.0
		 */
		if (r < d) {
			/* diffuse reflection */

			/* If previous hit is specular,
			 * store photon here.
			 */ 
			if (ray->prev_hit == 'S') {
				ray->nbound_specular = 100;
				return;
			}
				

#if 0
			/* Generate random direction around the normal. */
			ri_qmc_vector_cosweight(&ray->dir,
						&surfinfo->normal,
						ray->d,
						ray->i,
						perm);
			ray->d += 2;
#else
			ri_random_vector_cosweight(&ray->dir, &surfinfo->normal);
#endif
			
			ri_vector_copy(&(ray->org), &surfinfo->pos);

			energy->e[0] *= surfinfo->color.e[0] / d;
			energy->e[1] *= surfinfo->color.e[1] / d;
			energy->e[2] *= surfinfo->color.e[2] / d;

			ray->nbound_diffuse++;
			ray->prev_hit = 'D';

			trace_photon(ray, energy, surfinfo, pdir, hit, maxdepth);
		} else if (r < d + s){
			/* specular reflection */
			//ri_vector_copy(&negin, ray->dir);
			//ri_vector_neg(&negin);

			ri_reflect(&(ray->dir),
				   &ray->dir,
				   &surfinfo->normal);
	
			ri_vector_copy(&(ray->org), &surfinfo->pos);

			ray->nbound_specular++;
			ray->prev_hit = 'S';

			trace_photon(ray, energy, surfinfo, pdir, hit, maxdepth);
		} else if (r < d + s + t){
			/* specular refraction */
			//ri_vector_copy(&negin, ray->dir);
			//ri_vector_neg(&negin);

			if (ray->nbound_diffuse) {
				ray->prev_hit = 'N';
				(*hit) = 0;
				ray->nbound_specular = 100;
				return;
			}

			if (surfinfo->inside) {
				/* ray exits from inside of geometry */
				eta = ior_glass / ior_air;
			} else {
				/* ray enters to inside of geometry */
				eta = ior_air / ior_glass;
			}

			ri_refract(&(ray->dir),
				   &ray->dir,
				   &surfinfo->normal,
				   eta);

			ri_vector_normalize(&(ray->dir));
			ri_vector_copy(&(ray->org), &surfinfo->pos);

			ray->org.e[0] += ray->dir.e[0] * 0.01;
			ray->org.e[1] += ray->dir.e[1] * 0.01;
			ray->org.e[2] += ray->dir.e[2] * 0.01;

			ray->nbound_specular++;
			ray->prev_hit = 'S';

			trace_photon(ray, energy, surfinfo, pdir, hit, maxdepth);
		} else {
			/* photon will be stored */
		}
	} else {
		ray->prev_hit = 'N';
	}
}

static void
median_split_intl(ri_photon_t **p,
		  const int start,
		  const int end,
		  const int median,
		  const int axis)
{
	int   i, j;
	int   left, right;
	float v;
	ri_photon_t *tmp;	/* for swapping */
	

	left  = start;
	right = end;

	while (right > left) {
		v = p[right]->pos.e[axis];
		i = left - 1;
		j = right;

		for (;;) {
			while (p[++i]->pos.e[axis] < v) ;
			while (p[--j]->pos.e[axis] > v && j > left) ;

			if (i >= j) break;

			/* swap */
			tmp = p[i]; p[i] = p[j]; p[j] = tmp;
		}

		/* swap */
		tmp = p[i]; p[i] = p[right]; p[right] = tmp;
	
		if (i >= median) right = i - 1;
		if (i <= median) left  = i + 1;
	}
}
	
static void
balance_segment_intl(ri_photonmap_t *map,
		     ri_photon_t **pbal,
		     ri_photon_t **porg,
		     const int index,
		     const int start,
		     const int end)
{
	int   median;
	int   axis;
	float tmp;

	/*
	 * compute new median
	 */
	median = 1;

	while ((4 * median) <= (end - start + 1)) {
		median += median;
	}

	if ((3 * median) <= (end - start + 1)) {
		median += median;
		median += start - 1;
	} else {
		median = end - median + 1;
	}

	/*
	 * find axis to split along
	 */
	axis = 2;

	if ((map->bbox_max[0] - map->bbox_min[0]) >
	    (map->bbox_max[1] - map->bbox_min[1]) &&
	    (map->bbox_max[0] - map->bbox_min[0]) >
	    (map->bbox_max[2] - map->bbox_min[2])) {
		axis = 0;
	} else if ((map->bbox_max[1] - map->bbox_min[1]) >
		   (map->bbox_max[2] - map->bbox_min[2])) {
		axis = 1;
	}

	/*
	 * partition photon block around the median
	 */
	median_split_intl(porg, start, end, median, axis);

	pbal[index] = porg[median];
	pbal[index]->plane = axis;

	/*
	 * recursively balance the left and right block
	 */ 
	if (median > start) {
		/* balance left segment */
		if (start < median - 1) {
			tmp = map->bbox_max[axis];
			map->bbox_max[axis] = pbal[index]->pos.e[axis];

			balance_segment_intl(map, pbal, porg,
					     2 * index, start, median - 1);

			map->bbox_max[axis] = tmp;
		} else {
			pbal[2 * index] = porg[start];
		}
	}

	if (median < end) {
		/* balance right segment */
		if (median + 1 < end) {
			tmp = map->bbox_min[axis];

			map->bbox_min[axis] = pbal[index]->pos.e[axis];

			balance_segment_intl(map, pbal, porg,
					     2 * index + 1, median + 1, end);

			map->bbox_min[axis] = tmp;
		} else {
			pbal[2 * index + 1] = porg[end];
		}
	}
}

static void
locate_photons(ri_photonmap_t *map, nearest_photons_t *np, int index)
{
	ri_photon_t *curr;
	ri_vector_t tmp;
	float dist;
	unsigned short plane;

	curr  =	&(map->photons[index]);
	plane = curr->plane;

	if ((unsigned int)(2 * index + 1) < map->nphotons) {

		/* signed distance to splitting plane. */
		dist  =  np->query_pos.e[plane] - curr->pos.e[plane];

		if (dist > 0.0) {
			/* search right subtree first.  */
			locate_photons(map, np, 2 * index + 1);

			if (dist * dist < np->dist2[0]) {
				/* check left subtree */
				locate_photons(map, np, 2 * index);
			}
		} else {
			/* search left subtree first */
			locate_photons(map, np, 2 * index);
			
			if (dist * dist < np->dist2[0]) {
				/* check right subtree */
				locate_photons(map, np, 2 * index + 1);
			}
		}
	}

	/* compute squared distance from photon to position x. */
	ri_vector_sub(&tmp, &curr->pos, &np->query_pos);
	dist = ri_vector_dot3(&tmp, &tmp);

	if (dist < np->dist2[0]) {
		if (np->nfound < np->nmax) {	/* heap is not full */
			nearest_photons_insert(np, curr, dist);
		} else {
			nearest_photons_replace(np, curr, dist);
		}	
	}		
}

static void
locate_photons_jensen(ri_photonmap_t *map, nearest_photons_t *np, int index)
{
	ri_photon_t *curr;
	ri_vector_t tmp;
	float dist;
	unsigned short plane;

	int half_found;
	int j, k;
	int parent;
	float dst2;
	ri_photon_t *phot;


	curr  =	&(map->photons[index]);
	plane = curr->plane;

	if (index <= (int)(map->nphotons / 2)) {

		/* signed distance to splitting plane. */
		dist  = np->query_pos.e[plane] - curr->pos.e[plane];

		if (dist > 0.0) {
			/* search right subtree first.  */
			if (2 * index + 1 <= (int)map->nphotons) {
				locate_photons_jensen(map, np, 2 * index + 1);
			}

			if (dist * dist < np->dist2[0]) {
				/* check left subtree */
				locate_photons_jensen(map, np, 2 * index);
			}
		} else {
			/* search left subtree first */
			locate_photons_jensen(map, np, 2 * index);
			
			if (dist * dist < np->dist2[0]) {
				/* check right subtree */
				if (2 * index + 1 <= (int)map->nphotons) {
					locate_photons_jensen(map, np, 2 * index + 1);
				}
			}
		}
	}

	/* compute squared distance from photon to position x. */
	ri_vector_sub(&tmp, &curr->pos, &np->query_pos);
	dist = ri_vector_dot3(&tmp, &tmp);

	if (dist < np->dist2[0]) {
		if (np->nfound < np->nmax) {	/* heap is not full */
			np->nfound++;
			np->dist2[np->nfound] = dist;
			np->nearest_photons[np->nfound] = curr;
		} else {
			if (np->gotheap == 0) {
				/* build heap */
				half_found = (np->nfound) >> 1;
			
				for (k = half_found; k >= 1; k--) {
					parent = k;
					phot   = np->nearest_photons[k];
					dst2   = np->dist2[k];

					while (parent <= half_found) {
						j = parent + parent;
						if (j < np->nfound &&
						    np->dist2[j] < np->dist2[j+1]) {
							j++;
						}

						if (dst2 >= np->dist2[j]) break;

						np->dist2[parent] = np->dist2[j];
						np->nearest_photons[parent] = np->nearest_photons[j];
						parent = j;
					}

					np->dist2[parent] = dst2;
					np->nearest_photons[parent] = phot;
				}

				np->gotheap = 1;
			}

			parent = 1;
			j      = 2;

			while (j <= np->nfound) {
				if (j < np->nfound &&
				    np->dist2[j] < np->dist2[j+1]) {
					j++;
				}

				if (dist > np->dist2[j]) break;

				np->dist2[parent]           = np->dist2[j];
				np->nearest_photons[parent] = np->nearest_photons[j];
				parent = j;
				j += j;
			}

			np->nearest_photons[parent] = curr;
			np->dist2[parent]           = dist;
		
			np->dist2[0]                = np->dist2[1];
		}	
	}		
}

static void
vector_to_polar(const ri_vector_t *vec, unsigned char *phi, unsigned char *theta)
{

	int thetaval;
	int phival;

	thetaval = (int)(acos(vec->e[2]) * (256.0 / RI_PI));
	
	if (thetaval > 255) (*theta) = 255;
	else                (*theta) = (unsigned char)thetaval;

	phival = (int)(atan2(vec->e[1], vec->e[0]) * (256.0 / (2.0 * RI_PI)));

	if (phival > 255)    (*phi) = 255;
	else if (phival < 0) (*phi) = (unsigned char)(phival + 256);
	else                 (*phi) = (unsigned char)phival;
}

static void
polar_to_vector(ri_vector_t *vec, unsigned char phi, unsigned char theta)
{
	int          i;
	double       angle;
	static int   tblinit = 0;
	static float costhetatbl[256];
	static float sinthetatbl[256];
	static float cosphitbl[256];
	static float sinphitbl[256];

	if (!tblinit) {
		for (i = 0; i < 256; i++) {
			angle = (double)(i) * (1.0 / 256.0) * RI_PI;
			costhetatbl[i] = cos(angle);
			sinthetatbl[i] = sin(angle);
			cosphitbl[i]   = cos(2.0 * angle);
			sinphitbl[i]   = sin(2.0 * angle);
		}

		tblinit = 1;
	}

	vec->e[0] = sinthetatbl[theta] * cosphitbl[phi];
	vec->e[1] = sinthetatbl[theta] * sinphitbl[phi];
	vec->e[2] = costhetatbl[theta];
}


static nearest_photons_t *
nearest_photons_new(int maxsize, float maxradius)
{
	nearest_photons_t *p;

	p = (nearest_photons_t *)ri_mem_alloc(sizeof(nearest_photons_t));

	if (maxsize < 1) {
		ri_log(LOG_WARN, "maxsize < 1");
		maxsize = 1;
	}

	p->nearest_photons = (ri_photon_t **)ri_mem_alloc(
					sizeof(ri_photon_t *) * (maxsize+1));
	p->dist2           = (float *)ri_mem_alloc(sizeof(float) * (maxsize+1));

	p->nmax            = maxsize;
	p->nfound          = 0;
	p->query_pos.e[0]  = 0.0;
	p->query_pos.e[1]  = 0.0;
	p->query_pos.e[2]  = 0.0;
	p->gotheap         = 0;

	p->dist2[0]        = maxradius * maxradius;

	return p;
}

static void
nearest_photons_free(nearest_photons_t *np)
{
	ri_log_and_return_if(np == NULL);

	ri_mem_free(np->nearest_photons);
	ri_mem_free(np->dist2);
	ri_mem_free(np);
}

static void
nearest_photons_insert(nearest_photons_t *np, ri_photon_t *pindex, float dist2)
{
	ri_log_and_return_if(np->nfound >= np->nmax);

	np->nearest_photons[np->nfound] = pindex;
	np->dist2[np->nfound]           = dist2;
	
	nearest_photons_fixup(np);

	np->nfound++;

	if (np->nfound >= np->nmax) {
		np->maxdist = np->dist2[0];
	}
}

static void
nearest_photons_replace(nearest_photons_t *np, ri_photon_t *pindex, float dist2)
{
	np->nearest_photons[0] = pindex;
	np->dist2[0]           = dist2;

	nearest_photons_fixdown(np);

	/* max = top of heap */
	np->maxdist = np->dist2[0];
}

static void
nearest_photons_fixup(nearest_photons_t *np)
{
	int          son, parent;
	float        d;
	ri_photon_t *p;

	son = np->nfound;
	parent = (son - 1) >> 1;

	while ((son > 0) &&
	       np->dist2[parent] < np->dist2[son]) {
		d = np->dist2[parent];
		p = np->nearest_photons[parent];

		np->dist2[parent]       = np->dist2[son];
		np->nearest_photons[parent] = np->nearest_photons[son];

		np->dist2[son]       = d;
		np->nearest_photons[son] = p;

		son = parent;
		parent = (son - 1) >> 1;	
	}	
}

static void
nearest_photons_fixdown(nearest_photons_t *np)
{
	int          son, parent;
	int          max;
	float        d;
	ri_photon_t *p;

	max = np->nfound;

	parent = 0;
	son    = 1;

	while (son < max) {
		if (np->dist2[son] <= np->dist2[parent]) {
			if ((++son >= max) ||
			    np->dist2[son] <= np->dist2[parent]) {
				return;
			}
		} else {
			if ((son + 1 < max) &&
			    np->dist2[son + 1] > np->dist2[son]) {
				son++;
			}

		}

		/* swap */
		d = np->dist2[parent];
		p = np->nearest_photons[parent];

		np->dist2[parent]       = np->dist2[son];
		np->nearest_photons[parent] = np->nearest_photons[son];

		np->dist2[son]       = d;
		np->nearest_photons[son] = p;

		parent = son;
		son    = (parent << 1) + 1;	
	}
}

static double
gaussian_filter(float dist2, float radius2)
{
	double alpha = 0.918;
	double beta  = 1.953;
	double denom, numer;
	double k, w;

	k = dist2 / (2.0 * radius2);
	denom = 1.0 - exp(-beta);
	numer = 1.0 - exp(-beta * k);

	w = alpha * (1.0 - numer / denom);

	return w; 	

}

#if 0
static double
cone_filter(float dist2, float radius2)
{
	const double k = 1.0;
	double w;
	double d, r;
	double dr;

	/*
	 * w = max(0, 1 - d/(k r)) / (1 - 2 / (3 k))
	 */

	assert(r != 0.0);

	d = sqrt(dist2);
	r = sqrt(radius2);

	dr = d / (k * r);
	if (dr < 0.0) dr = 0.0;
	
	w = dr / (1.0 - (2.0 / (3.0 * k)));

	return w; 	

}
#endif

static double
calc_area(ri_geom_t *geom)
{
	unsigned int i;
	unsigned int i0, i1, i2;
	double area;
	ri_vector_t *v0, *v1, *v2;
	ri_vector_t v01, v02;
	ri_vector_t cross;

	area = 0.0;
	for (i = 0; i < geom->nindices / 3; i++) {
		i0 = geom->indices[3 * i + 0];	
		i1 = geom->indices[3 * i + 1];	
		i2 = geom->indices[3 * i + 2];	

		v0 = &(geom->positions[i0]);
		v1 = &(geom->positions[i1]);
		v2 = &(geom->positions[i2]);

		ri_vector_sub(&v01, v1, v0);
		ri_vector_sub(&v02, v2, v0);
		ri_vector_cross3(&cross, &v01, &v02);

		area += ri_vector_length(&cross) * 0.5;
	}

	//printf("geom area = %f\n", area);

	return area;
}

static void
save_photomap(const char *filename, ri_photonmap_t *pmap)
{
	FILE *fp;
	unsigned int i;
	double *bmin, *bmax;
	
	if (pmap->nphotons < 2) return;

	fp = fopen(filename, "w");
	if (!fp) return;

	fprintf(fp, "%d\n", (int)pmap->nphotons);

	/* write scene bounding box. */
	bmin = &(ri_render_get()->bmin[0]);
	bmax = &(ri_render_get()->bmax[0]);
	fprintf(fp, "%f %f %f\n", bmin[0], bmin[1], bmin[2]);
	fprintf(fp, "%f %f %f\n", bmax[0], bmax[1], bmax[2]);

	for (i = 0; i < pmap->nphotons; i++) {
		fprintf(fp, "%f %f %f %f %f %f\n",
				pmap->photons[i].pos.e[0],
				pmap->photons[i].pos.e[1],
				pmap->photons[i].pos.e[2],
				pmap->photons[i].power[0],
				pmap->photons[i].power[1],
				pmap->photons[i].power[2]);
	}

	fclose(fp);
}

static void
dump_raypath_init()
{
	raypathfp = fopen("raypath.dat", "w");
	if (!raypathfp) return;	

	
}

static void
dump_raypath_close()
{

	if (raypathfp) fclose(raypathfp);
}

static void
dump_raypath_start(const ri_vector_t *p)
{
	ndepth = 0;
	ri_vector_copy(&(raypath[ndepth]), p);
	ndepth++;
}

static void
dump_raypath_track(const ri_vector_t *p)
{
	ri_vector_copy(&(raypath[ndepth]), p);
	ndepth++;
}

static void
dump_raypath_end(char type)
{
	int i;
	fprintf(raypathfp, "%d", ndepth);
	fprintf(raypathfp, " %c", type);

	for (i = 0; i < ndepth; i++) {
		fprintf(raypathfp, " %f %f %f", raypath[i].e[0],
					        raypath[i].e[1],
					        raypath[i].e[2]);
	}
	fprintf(raypathfp, "\n");
}


static void
precompute_irradiance(ri_photonmap_t *map)
{
	/* precompute irradiance at each photon position. */

	unsigned int i, j;
	nearest_photons_t *np;
	ri_vector_t  ppos;
	ri_vector_t  irrad;
	ri_photonmap_option_t *pmapopt;
	ri_vector_t        pdir, normal;
	float              inva;
	float              maxradius;
	double             rr;
	int                maxphotons;
	float              dot;
	float              ellipfactor;

	pmapopt = ri_photonmap_get_option();

	if (map->nphotons < 1) return;

	maxphotons = pmapopt->max_gather_photons;
	maxradius  = pmapopt->max_gather_radius;

	np = nearest_photons_new(maxphotons, maxradius);


	ri_timer_start(ri_render_get()->context->timer,
		       "Photonmap | Precompute Irradiance");

	printf("map->nphotons = %lu\n", map->nphotons);

	for (i = 0; i < map->nphotons; i++) {
		ri_vector_copy(&(np->query_pos),
			       &map->photons[i].pos);
		rr = maxradius * maxradius;
		np->dist2[0] = rr;

		np->nfound          = 0;
		np->gotheap         = 0;

		locate_photons_jensen(map, np, 1);
		//locate_photons_jensen_iterative(map, np);

		ri_vector_zero(&irrad);

		if (np->nfound < 4) {
			float2rgbe(map->photons[i].irrad,
				   irrad.e[0], irrad.e[1], irrad.e[2]);
			continue;
		}

		polar_to_vector(&pdir,
				map->photons[i].nphi,
				map->photons[i].ntheta);

		for (j = 1 ; j <= (unsigned int)np->nfound; j++) {
			polar_to_vector(&normal,
					np->nearest_photons[j]->nphi,
					np->nearest_photons[j]->ntheta);
					
			dot = ri_vector_dot3(&pdir, &normal);
			if (dot > 0.0) {
				/* To prevent "false photons" at sharp
				 * corners, use an ellipsoid insterd of
				 * a sphere. */

				ri_vector_sub(&ppos,
					      &np->nearest_photons[j]->pos,
					      &np->query_pos);
				dot = ri_vector_dot3(&normal, &ppos);
				ellipfactor = fabsf(dot);
				if (ellipfactor > maxradius * 0.25) {
					continue;
				}

				
				irrad.e[0] += np->nearest_photons[j]->power[0];
				irrad.e[1] += np->nearest_photons[j]->power[1];
				irrad.e[2] += np->nearest_photons[j]->power[2];
			}
		}

		// one RI_PI for lambert, another for area dA
		inva = 1.0 / (RI_PI * RI_PI * rr);

		ri_vector_scale(&irrad, inva);

		float2rgbe(map->photons[i].irrad,
			   irrad.e[0], irrad.e[1], irrad.e[2]);
		
	}

	nearest_photons_free(np);

	ri_timer_end(ri_render_get()->context->timer,
		     "Photonmap | Precompute Irradiance");
}

static void
locate_nearest_photon_with_normal(ri_photonmap_t     *map,
			          ri_photon_t       **nearest,
				  const ri_vector_t  *pos,
				  const ri_vector_t  *normal,
				  float              *dist2,
				  int                 index)
{
	ri_photon_t *curr;
	ri_vector_t tmp;
	ri_vector_t pnormal;
	float dist;
	unsigned short plane;

	curr  =	&(map->photons[index]);
	plane = curr->plane;

	if (index <= (int)(map->nphotons / 2)) {

		/* signed distance to splitting plane. */
		dist  = pos->e[plane] - curr->pos.e[plane];

		if (dist > 0.0) {
			/* search right subtree first.  */
			if (2 * index + 1 <= (int)map->nphotons) {
				locate_nearest_photon_with_normal(
						map,
						nearest,
						pos,
						normal,
						dist2,
						2 * index + 1);
			}

			if (dist * dist < (*dist2)) {
				/* check left subtree */
				locate_nearest_photon_with_normal(
						map,
						nearest,
						pos,
						normal,
						dist2,
						2 * index);
			}
		} else {
			/* search left subtree first */
			locate_nearest_photon_with_normal(
					map,
					nearest,
					pos,
					normal,
					dist2,
					2 * index);
			
			if (dist * dist < (*dist2)) {
				/* check right subtree */
				if (2 * index + 1 <= (int)map->nphotons) {
					locate_nearest_photon_with_normal(
							map,
							nearest,
							pos,
							normal,
							dist2,
							2 * index + 1);
				}
			}
		}
	}

	/* compute squared distance from photon to position x. */
	ri_vector_sub(&tmp, &curr->pos, pos);
	dist = ri_vector_dot3(&tmp, &tmp);

	if (dist < (*dist2)) {
		polar_to_vector(&pnormal, curr->phi, curr->theta);
		if (ri_vector_dot3(&pnormal, normal) > 0.0) {
			(*dist2) = dist;
			(*nearest) = curr;
		}
	}		
}

/* iterative version */
#define ARRAYSIZE 24
static void
locate_photons_jensen_iterative(ri_photonmap_t *map, nearest_photons_t *np)
{

	int i = 1;
	int level = 0;
	int nhalf;
	int camefrom;
	unsigned short plane;
	float dist;
	ri_photon_t *curr;
	int chosen[ARRAYSIZE];		/* up to 16677216 photons */
	float dist_2[ARRAYSIZE];

	nhalf = map->nphotons >> 1;	/* nphotons / 2		*/

	while (1) {
		/* move down through the subtrees containing query_pos ultil
		 * a leaf is reached
		 */
		while (i < nhalf) {
			curr  =	&(map->photons[i]);
			plane = curr->plane;

			dist          = np->query_pos.e[plane] -
					curr->pos.e[plane];
			dist_2[level] = dist * dist;

			i = i << 1;	/* i = 2 * i		*/
		
			if (dist > 0.0) i++;	/* choose left/right child */
	
			chosen[level++] = i;
		}

		/* check this leaf photon, add it if it is among the nearest
		 * so far.
		 */
		update_np(np, &(map->photons[i]));

		/* move up in tree until we reach a photon where we need to
		 * check that photon and the other subtree.
		 */
		do {
			camefrom = i;
			i = i >> 1;	/* i = i / 2 */
			--level;
			if (i <= 0) return;	/* we passed root. */
		} while (dist_2[level] >= np->dist2[0] ||
			 camefrom != chosen[level]);

		/* check this leaf photon, add it if it is among the nearest
		 * so far.
		 */
		update_np(np, &(map->photons[i]));

		/* step into the other subtree */
		i = chosen[level++] ^ 1;
	}
}

static void
update_np(nearest_photons_t *np, ri_photon_t *p)
{
	int          half_found;
	int          j, k;
	int          parent;
	float        dst2;
	double       dist;
	ri_photon_t *phot;
	ri_vector_t  tmp;

	/* compute squared distance from photon to position x. */
	ri_vector_sub(&tmp, &p->pos, &np->query_pos);
	dist = ri_vector_dot3(&tmp, &tmp);

	if (dist < np->dist2[0]) {
		if (np->nfound < np->nmax) {	/* heap is not full */
			np->nfound++;
			np->dist2[np->nfound] = dist;
			np->nearest_photons[np->nfound] = p;
		} else {
			if (np->gotheap == 0) {
				/* build heap */
				half_found = (np->nfound) >> 1;
			
				for (k = half_found; k >= 1; k--) {
					parent = k;
					phot   = np->nearest_photons[k];
					dst2   = np->dist2[k];

					while (parent <= half_found) {
						j = parent + parent;
						if (j < np->nfound &&
						    np->dist2[j] < np->dist2[j+1]) {
							j++;
						}

						if (dst2 >= np->dist2[j]) break;

						np->dist2[parent] = np->dist2[j];
						np->nearest_photons[parent] = np->nearest_photons[j];
						parent = j;
					}

					np->dist2[parent] = dst2;
					np->nearest_photons[parent] = phot;
				}

				np->gotheap = 1;
			}

			parent = 1;
			j      = 2;

			while (j <= np->nfound) {
				if (j < np->nfound &&
				    np->dist2[j] < np->dist2[j+1]) {
					j++;
				}

				if (dist > np->dist2[j]) break;

				np->dist2[parent]           = np->dist2[j];
				np->nearest_photons[parent] = np->nearest_photons[j];
				parent = j;
				j += j;
			}

			np->nearest_photons[parent] = p;
			np->dist2[parent]           = dist;
		
			np->dist2[0]                = np->dist2[1];
		}	
	}		
}
