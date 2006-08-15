/*
 * Metropolis Light Transport implementation.
 * 
 * Syoyo Fujita
 * 
 * $Id: metropolis.c,v 1.5 2004/08/15 05:19:39 syoyo Exp $
 *
 * TODO: o Sampling caustics path.
 *       o Bi-directional path tracing for underlying path sampling method.
 *       o Integrate with quasi-Monte Carlo method.
 */

/* File: metropolis.c
 *
 * This is a imprementation of Metropolis Light Transport based on
 * Szirmay-Kalos's simple and robust mutation strategy.
 *
 * Reference:
 *
 * - Eric Veach and Leonidas J. Guibas,
 *   "Metropolis Light Transport",
 *   SIGGRAPH '97 Proceedings, pp. 65-76, 1997.
 *
 * - Eric Veach,
 *   "Robust Monte Carlo Methods for Light Transport Simulation",
 *   Ph.D. dissertation, Standord University, 1997.
 *
 * - Csaba Kelemen, Laszlo Szirmay-Kalos, Gyoergy Antal and Ferenc Csonka,
 *   "A Simple and Robust Mutation Strategy for the Metropolis Light Transport
 *    Algorithm",
 *   Computer Graphics Forum(EUROGRAPHICS 2002), Vol 21, no.2, pp. 531-540,
 *   2002.
 *
 * - Laszlo Szirmay-Kalos ans Gyoergy Antal,
 *   "Metropolis sampling in random walk global illumination algorithms",
 *   Graphics Programming Methods, pp. 249-259, 2003.
 *
 */

/*
 * QMC:
 * List of allocated dimensions used for MLT
 *
 * 0   - screen position(x)
 * 1   - mutation for screen position(x)
 * 2   - screen position(y)
 * 3   - mutation for screen position(y)
 * ...
 *
 * TODO...
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
#include "raytrace.h"
#include "render.h"
#include "light.h"
#include "geom.h"
#include "random.h"
#include "reflection.h"
#include "stack.h"
#include "qmc.h"

#ifndef M_PI
#define M_PI 3.141592
#endif

#define MAX_PATH_VERTICES 10		/* max ray depth		*/
#define MAX_FOLLOWED_VERTS 5		/* cut the recursion depth if needed */
#define MAX_STACKSIZE 1024

/* CIE color definitions */
#define CIE_rf 0.265074126
#define CIE_gf 0.670114631
#define CIE_bf 0.064811243

#define CIEcol(v) (CIE_rf * (v)[0] + CIE_gf * (v)[1] + CIE_bf * (v)[2])

typedef struct _primary_coord_t
{
	unsigned long modify_time;	/* the last modification time	*/
	double        value;		/* the random value		*/
} primary_coord_t;

/* Vertex in the ray path */
typedef struct _path_node_t
{
	ri_surface_info_t hit;		/* information of the intersection
					 * point. */
	ri_vector_t       indir;	/* imcoming direction */
	ri_vector_t       outdir;	/* outgoing direction */
	double            G;		/* geometric coefficients for the
					 * energy transport. */
	double            pdf;		/* generation prob. of this vertex. */
	double            bsdf[3];	/* BSDF at surface point. */
	unsigned char     type;		/* vertex type. D, S, or T. */ 
} path_node_t;

/* ray path.
 * the first vertex is always the eye.
 * and the last vertex is always the light source.
 */
typedef struct _path_t
{
	path_node_t verts[MAX_PATH_VERTICES];	/* path vertices	*/
	long        nverts;			/* the number of the vertices */
	long        px, py;			/* pixel position of this path
						 * contributes */
	double      radiance[3];		/* radiance of this path */
	double      I;				/* importance of this path */
	double      pdf;			/* the generation prob. of							 * the full path. */
	long        nusedcoords;		/* the number of the random
						 * coordinates associated
						 * with this path */
	int         large_step;			/* this path is large step
						 * or not */
} path_t;


static int             use_qmc = 0;
static primary_coord_t u[MAX_PATH_VERTICES * 3];
static primary_coord_t ustack[MAX_STACKSIZE];
static int             ustack_depth = 0;
static unsigned long   time;			/* current time		*/
static unsigned long   large_step_time;		/* the last time when large
						 * step was performed   */

static path_t          new_path;		/* tentative path	*/
static path_t          old_path;		/* current path		*/
static path_node_t     eye_node;		/* reused all paths	*/

static unsigned int    qmc_instance[MAX_PATH_VERTICES * 3 * 2];

ri_matrix_t        c2w;				/* camera to world	*/

int                gpixwidth;
int                gpixheight;

static double mutate_value  (int i, double value);
static double primary_sample(int i);
static double sample_value  (int i);

static void   contribute_path(const ri_display_drv_t *ddrv,
			      const path_t *path, double weight);

static int    generate_new_path();
static int    pixel_sample();
static int    surface_sample();
static void   eye_point_sample();
static int    light_sample();
static void   trace_eye_path();

static int    sample_outdir(const ri_material_t *material,
			    const ri_vector_t *in, const ri_vector_t *normal,
			    double rand1, double rand2, double rand3,
			    ri_vector_t *out, double *pdf,	/* output */
			    unsigned char *type); 		/* output */
static void   sample_cosweight(const ri_vector_t *normal, double r1, double r2,
		               ri_vector_t *outdir, double *pdf); /* output */

static void   brdf(double f[3],
		   const ri_surface_info_t *surfinfo,
		   const ri_vector_t *indir,
		   const ri_vector_t *outdir,
		   const ri_vector_t *normal);

static void ustack_push (primary_coord_t  coord);
static void ustack_top  (primary_coord_t *coord);
static void ustack_pop  ();
static int  ustack_empty();

static void get_camera(ri_vector_t *pos, ri_vector_t *dir, ri_matrix_t *c2w);

static unsigned char clamp(float f);

void
ri_transport_mlt(const ri_display_drv_t *ddrv)
{
	int           uid;
	int           path_index;
	int           progress = 0;
	unsigned long i;
	unsigned long nseed_paths = 3000;
	unsigned long M;			/* total number of samples */
	double        sumI;
	double        a;
	double        b;
	double        r;
	double        lstep;
	double        w_new, w_old;
	double        accum_importance;
	double        p_large = 0.2;
	unsigned long naccepted_mutations = 0;
	unsigned long nrejected_mutations = 0;
	unsigned long nunlinked = 0;
	path_t *seed_paths = NULL;
	ri_option_t *opt;
	ri_camera_t *camera;

	ri_vector_t in, out, normal;
	
	in.e[0] = 1.0f; in.e[1] = 0.0f; in.e[2] = 0.0f;
	normal.e[0] = -1.0f; normal.e[1] = 0.0f; normal.e[2] = 0.0f;
	ri_refract(&out, &in, &normal, 1.0f / 1.4f);
	printf("out = "); ri_vector_print(&out);

	/* Initialize */

	opt = ri_render_get()->context->option;

	camera = ri_render_get()->context->option->camera;
	gpixwidth  = camera->horizontal_resolution;
	gpixheight = camera->vertical_resolution;
	
	M = (unsigned long)opt->mlt_nsamples;
	printf("mlt nsamples = %lu\n", M);

	time = large_step_time = 0;

	eye_point_sample();

	for (i = 0; i < 3 * MAX_PATH_VERTICES; i++) {
		// TODO: for QMC,
		// initializing u[i].value with zero is correnct?
		u[i].value       = randomMT();
		u[i].modify_time = 0;

	}

	for (i = 0; i < 2 * 3 * MAX_PATH_VERTICES; i++) {
		qmc_instance[i] = 0;
	}

	/* ------ 1st phase: ------
	 * a number of independent samples are generated to estimate
	 * the overall brightness of the image and to select seeds for the
	 * Metropolis phase.
	 */

	seed_paths = ri_mem_alloc(sizeof(path_t) * nseed_paths);
	for (i = 0; i < nseed_paths; i++) {
		seed_paths[i].nverts = 0;
		seed_paths[i].I      = 0.0;
	}

	sumI = 0.0;
	new_path.large_step = 1;
	
	for (i = 0; i < nseed_paths; i++) {
		generate_new_path();
		time++;

		while(!ustack_empty()) {
			ustack_pop();
		}

		assert(new_path.I >= 0.0);

		sumI += new_path.I;

		ri_mem_copy(&seed_paths[i], &new_path, sizeof(path_t));
	}

	/* b = the average luminance of the image */
	printf("[info] MLT: sumI = %f\n", sumI);
	b = sumI / (double)nseed_paths;	

	printf("[info] MLT: b = %f\n", b);

	/* select a seed path according to the importance of the path. */
	path_index = 0;
	accum_importance = 0;
	r = randomMT() * sumI;

	for (i = 0; i < nseed_paths; i++) {
		accum_importance += seed_paths[i].I;
		if (accum_importance >= r) {
			path_index = i;
			break;
		}
	}

	printf("[info] MLT: seed path index = %d\n", path_index); 

	/* ------ 2nd phase: ------
	 * Metropolis phase
	 */
	ri_mem_copy(&old_path, &seed_paths[path_index], sizeof(path_t));

	for (i = 0; i < M; i++) {
		new_path.large_step = (randomMT() < p_large) ? 1 : 0;	
		
		if (!generate_new_path()) {
			nunlinked++;
		} else {
#if 0
			printf("gen'ed path nvert = %d\n", new_path.nverts - 1);
			if (new_path.verts[new_path.nverts - 2].type == 'T') {
				printf("caustics path\n");
				printf("I = %f\n", new_path.I);
				printf("nverts = %d\n", new_path.nverts);

			}
#endif
		}

		if (old_path.I < 1.0e-15) {
			a = 0.0;
		} else {
			a = new_path.I / old_path.I;
		}
		
		if (a > 1.0) a = 1.0;

		lstep = (new_path.large_step) ? 1.0 : 0.0;
		
		w_new = (a + lstep) / (new_path.I / b + p_large) / M;
		w_old = (1.0 - a) / (old_path.I / b + p_large) / M;

		contribute_path(ddrv, &new_path, w_new);
		contribute_path(ddrv, &old_path, w_old);

		if (randomMT() < a) {	/* accept */

			naccepted_mutations++;

			if (new_path.large_step) large_step_time = time;
			
			time++;

			while (!ustack_empty()) {
				ustack_pop();
			}

			ri_mem_copy(&old_path, &new_path, sizeof(path_t));

		} else {		/* reject. restore primary sample u */

			nrejected_mutations++;

			uid = new_path.nusedcoords - 1;

			while (!ustack_empty()) {
				ustack_top(&u[uid--]);
				ustack_pop();
			}
		}

		if (progress < (int)(i * 100 / M)) {
			progress++;
			printf("rendered %d%%\n", progress); 
		}
	}

	printf("naccepted = %lu\n", naccepted_mutations); 
	printf("nrejected = %lu\n", nrejected_mutations); 
	printf("nunlinked = %lu\n", nunlinked); 
}

/* Generate one in-between vertex of the path. */
static int 
surface_sample()
{
	unsigned char     type;
	int               hit;
	double            r[3];
	double            pdf_dir;
	double            dist;
	double            cosa, cosb;
	ri_vector_t       v;
	ri_ray_t          ray;
	ri_surface_info_t surfinfo;

	path_node_t *prev_node = NULL;
	path_node_t *this_node = NULL;
	path_node_t *new_node  = NULL;

	prev_node = &new_path.verts[new_path.nverts - 2];
	this_node = &new_path.verts[new_path.nverts - 1];
	new_node  = &new_path.verts[new_path.nverts    ];

	r[0] = primary_sample(new_path.nusedcoords++);
	r[1] = primary_sample(new_path.nusedcoords++);
	r[2] = primary_sample(new_path.nusedcoords++);

	sample_outdir(this_node->hit.geom->material,
		      &this_node->indir, &this_node->hit.normal,
		      r[0], r[1], r[2], &new_node->indir, &pdf_dir, &type);
	ri_vector_copy(&this_node->outdir, &new_node->indir);

	if (pdf_dir < 1.0e-6f) {
		return 0;
	}

	if (type == 'T') {
		//ri_vector_print(&this_node->hit.geom->material->kt);
	}

	ri_vector_copy(&ray.org, &this_node->hit.pos);
	ri_vector_copy(&ray.dir, &new_node->indir);

	hit = ri_raytrace(ri_render_get(), &ray, &surfinfo);
	if (!hit) return 0;
	
	ri_mem_copy(&new_node->hit, &surfinfo, sizeof(ri_surface_info_t));
#if 0
	printf("surf: type = %c\n", type);
	printf("surf: vert = %d\n", new_path.nverts - 1);
	printf("surf: new_node->kd = ");
	ri_vector_print(&surfinfo.geom->material->kd);
	printf("surf: new_node->kt = ");
	ri_vector_print(&surfinfo.geom->material->kt);
	printf("surf: this_node->kd = ");
	ri_vector_print(&this_node->hit.geom->material->kd);
	printf("surf: this_node->kt = ");
	ri_vector_print(&this_node->hit.geom->material->kt);
#endif
	brdf(this_node->bsdf,
	     &this_node->hit,
	     &this_node->indir, &new_node->indir, &this_node->hit.normal);
	if (this_node->bsdf[0] < 1.0e-6) {
		printf("???: surface brdf\n");
	}

	this_node->type = type;
	if (this_node->type == 'T' && this_node->hit.geom->material->kt.e[0] < 1.0e-3) {
		printf("???\n");
		printf("kt = ");
		ri_vector_print(&this_node->hit.geom->material->kt);

	}

	ri_vector_sub(&v, &new_node->hit.pos, &this_node->hit.pos);
	dist = ri_vector_length(&v);
	cosa = ri_vector_dot3(&this_node->hit.normal, &new_node->indir);
	cosb = -1.0 * ri_vector_dot3(&new_node->hit.normal, &new_node->indir);
	if (cosa < 0.0) cosa = -cosa;
	if (cosb < 0.0) cosb = -cosb;
	new_node->pdf = pdf_dir * cosb / (dist * dist);
	new_node->G   = cosa * cosb / (dist * dist);

	return 1;
}

static void
trace_eye_path()
{
	if (new_path.nverts + 1 >= MAX_FOLLOWED_VERTS) {
		/* End of the recursion. */
		return;
	}

	if (!surface_sample()) {
		/* Russian Roulette has ended the path. */
		return;
	}

	new_path.nverts++;
	trace_eye_path();
}

/* Generate new path */
static int
generate_new_path()
{
	int i;

	//printf("new path\n");

	new_path.radiance[0] = 0.0;
	new_path.radiance[1] = 0.0;
	new_path.radiance[2] = 0.0;
	new_path.I = 0.0;

	/* 1. eye point is sampled. reuse eye_node. */
	memcpy(&(new_path.verts[0]), &eye_node, sizeof(path_node_t));
	new_path.nverts = 1;

	/* 2. sample points on pixel plane. */
	if (!pixel_sample()) return 0;
	new_path.nverts = 2;

	/* 3. trace path */
	trace_eye_path();

	/* 4. sample light source. */
	if (!light_sample()) return 0;
	new_path.nverts++;

	/* 5. evaluate radiance and pdf */
	new_path.radiance[0] = 1.0;
	new_path.radiance[1] = 1.0;
	new_path.radiance[2] = 1.0;
	new_path.pdf = 1.0;

	for (i = 1; i < new_path.nverts; i++) {
		/* radiance *= v[i].bsdf * v[i].G */	
		new_path.radiance[0] *= new_path.verts[i].bsdf[0] *
					new_path.verts[i].G;
		new_path.radiance[1] *= new_path.verts[i].bsdf[1] *
					new_path.verts[i].G;
		new_path.radiance[2] *= new_path.verts[i].bsdf[2] *
					new_path.verts[i].G;
		new_path.pdf *= new_path.verts[i].pdf;

#if 0
		if (new_path.verts[new_path.nverts - 2].type == 'T') {
			printf("[%d]new_path.verts.pdf = %f\n", i, new_path.verts[i].pdf);
			printf("[%d]new_path.verts.G = %f\n", i, new_path.verts[i].G);
			printf("[%d]new_path.verts.type = %c\n", i, new_path.verts[i].type);
			printf("[%d]new_path.verts.bsdf = %f, %f, %f\n", i,
					new_path.verts[i].bsdf[0],
					new_path.verts[i].bsdf[1],
					new_path.verts[i].bsdf[2]);
			printf("[%d]new_path.radiance = %f, %f, %f\n", i,
				new_path.radiance[0],
				new_path.radiance[1],
				new_path.radiance[2]);
		}
#endif
	}

	//assert(new_path.pdf >= 1.0e-6);
	if (new_path.pdf < 0.0f) {
#if 0
		printf("new_path.pdf = %f\n", new_path.pdf);
		for (i = 1; i < new_path.nverts; i++) {
			printf("[%d]new_path.verts.pdf = %f\n",
				i, new_path.verts[i].pdf);
		}
#endif
	}
	assert(new_path.pdf >= -1.0e-6);
#if 0
	printf("radiance = %f, %f, %f\n",
			new_path.radiance[0],
			new_path.radiance[1],
			new_path.radiance[2]);
#endif
	if (new_path.pdf < 1.0e-15) {
		//printf("too small pdf: %f\n", new_path.pdf);
		new_path.I = 0.0;
		return 0;
	} else {
		new_path.I = CIEcol(new_path.radiance) / new_path.pdf;
		//printf("new_path.I = %f\n", new_path.I);
	}
	
	return 1;
}

/* Sample a random point on the pixel image plane. */
static int
pixel_sample()
{
	double p[2];
	double cos_patch, cos_pixel;
	float  fov;
	float  flen;
	float  dist_pixel2;	/* distance^2 from camera to pixel plane. */
	float  dist_patch2;	/* distance^2 from hit point to camera. */
	float  pixh, pixv;
	int    hit;
	int    width, height;

	ri_vector_t        dir;
	ri_vector_t        v;
	ri_camera_t       *camera;
	ri_ray_t           ray;
	ri_surface_info_t  surfinfo;

	path_node_t *pixel_node = NULL;

	pixel_node = &(new_path.verts[1]);
	new_path.nusedcoords = 2;

	camera = ri_render_get()->context->option->camera;
	width  = camera->horizontal_resolution;
	height = camera->vertical_resolution;
	fov    = camera->fov;
	flen   = 1.0f / (float)tan((fov * M_PI / 180.0) * 0.5);
	pixh = 2.0f * (float)tan(fov * M_PI / 180.0) / (float)width;
	pixv = 2.0f * (float)tan(fov * M_PI / 180.0) / (float)height;

	//printf("pixh,v = %f, %f\n", pixh, pixv);

	p[0] = primary_sample(0);
	p[0] = (p[0] - 0.5) * width;
	p[1] = (primary_sample(1) - 0.5) * height;

	new_path.px = (long)(p[0] + width / 2.0);
	new_path.py = (long)(p[1] + height / 2.0);

	//printf("path.px, py = %lu, %lu\n", new_path.px, new_path.py);

	if (new_path.px < 0       ||
	    new_path.px >= width  ||
	    new_path.py < 0       ||
	    new_path.py >= height) {
		/* Outsize of screen */
		printf("out of screen\n");
		printf("(x, y) = (%lu, %lu\n", new_path.px, new_path.py);
		return 0;
	}

	dir.e[0] = p[0] / (width / 2.0);
	dir.e[1] = p[1] / (height / 2.0);
	dir.e[2] = flen;
	dir.e[3] = 1.0f;

	dist_pixel2 = ri_vector_length(&dir);
	dist_pixel2 *= dist_pixel2;

	ri_vector_copy(&ray.org, &eye_node.hit.pos);

	ri_vector_transform(&v, &dir, &c2w);	/* camera to world*/
	ri_vector_sub(&ray.dir, &v, &eye_node.hit.pos);
	ri_vector_normalize(&ray.dir);

	hit = ri_raytrace(ri_render_get(), &ray, &surfinfo);
	if (!hit) {
		/* the ray intersects nothing in the scene. */
		//printf("the ray intersects nothing\n");
		return 0;
	} else {
		//printf("hits\n");
	}

	ri_mem_copy(&pixel_node->hit, &surfinfo, sizeof(ri_surface_info_t));
	ri_vector_copy(&pixel_node->indir, &ray.dir);

	/* cosPatch = -In . N */
	cos_patch = -1.0 * ri_vector_dot3(&pixel_node->hit.normal,
					  &pixel_node->indir);

	/* cosPixel = Camera.Z . In */
	cos_pixel = ri_vector_dot3(&eye_node.hit.normal, &pixel_node->indir);

	if (cos_pixel <= 0.0 || cos_patch <= 0.0) {
		return 0;
	}

	/* (hit.P - Camera.P)^2 */
	ri_vector_sub(&v, &pixel_node->hit.pos, &eye_node.hit.pos);
	dist_patch2 = ri_vector_length(&v);
	dist_patch2 *= dist_patch2;

	/* scene area measure */
	pixel_node->G = cos_pixel * cos_patch / dist_patch2;

	/* p1 * dxdy = 1.0 / area_screen */
	pixel_node->pdf  =  1.0 / ((pixh*(float)width) * (pixv*(float)height));
	/* p2 * dw = p1 / (cosPixel / distPixel2) */
	pixel_node->pdf *= dist_pixel2 / cos_pixel;
	/* p3 * dxdy = p2 * (cosPatch / distPatch2) */
	pixel_node->pdf *= cos_patch / dist_patch2;

	new_path.nverts = 2;
	
	return 1;
}

/* Generate first vertex(eye position) of the path */
static void
eye_point_sample()
{
	ri_vector_t cam_pos;	/* camera position in world coord */
	ri_vector_t cam_dir;	/* camera direction in world coord */

	get_camera(&cam_pos, &cam_dir, &c2w);

	ri_vector_copy(&(eye_node.hit.pos), &cam_pos);
	ri_vector_copy(&(eye_node.hit.normal), &cam_dir);
	eye_node.pdf = 1.0;	/* differential eye area cancels out with
				 * conputing the flux. */
	eye_node.G   = 1.0;
	eye_node.bsdf[0] = 1.0;
	eye_node.bsdf[1] = 1.0;
	eye_node.bsdf[2] = 1.0;
}

/* Generate last vertex(light position) of the path */
static int
light_sample()
{
	//int         i;
	int         hit;
	float       cos1, cos2;
	//float       diffuse;
	float       nearestt;
	ri_light_t *light;
	ri_vector_t light_pos;
	ri_vector_t light_normal;
	//ri_vector_t light_dir;		
	ri_ray_t    ray;
	ri_surface_info_t surfinfo;

	path_node_t *light_node = NULL;
	path_node_t *last_node  = NULL;

	if (new_path.verts[new_path.nverts - 2].type == 'T') {
		/* caustics path */
		new_path.nverts = new_path.nverts - 1;
		//printf("light_sample: caustics path. nvert = %d\n", new_path.nverts - 1);
	}
	light_node = &(new_path.verts[new_path.nverts    ]);
	last_node  = &(new_path.verts[new_path.nverts - 1]);

	/* Only consider one area light source */
	light = (ri_light_t *)ri_list_first(ri_render_get()->lightlist)->data;
	if (!light->geom) {
		return 0;	/* not an area light source. */
	}

	if (last_node->type == 'T' ||		/* caustics or mirror */
	    last_node->type == 'S') {
		//printf("spec: vertnum = %d\n", new_path.nverts - 1);
		/* Sampling direction is already computed. */
		ri_vector_copy(&ray.dir, &(last_node->outdir));
		//ri_vector_neg(&ray.dir);
	} else {
		ri_light_sample_pos_and_normal(light, &light_pos, &light_normal);

		ri_vector_sub(&ray.dir, &light_pos, &(last_node->hit.pos));
		ri_vector_normalize(&ray.dir);
	}

	ri_vector_copy(&ray.org, &(last_node->hit.pos));

	/* shoot a ray from the last node.
	 * Check visibility between light and surface.
	 */
	/* 1. Is a ray hits light source? */
	hit = ri_raytrace_geom(light->geom, &ray, &surfinfo);
	if (!hit) {
		return 0;
	}

	nearestt = ray.isectt;
	ri_vector_copy(&light_normal, &surfinfo.normal);

	/* 2. is there a occluder between light source and last node? */
	hit = ri_raytrace(ri_render_get(), &ray, &surfinfo);
	if (hit) {
		if (ray.isectt < nearestt) {
			/* there is a occluder */
			return 0;
		}
	}

	//printf("hit light\n");
	//ri_vector_copy(&(light_node->hit.pos), &surfinfo.pos);
	//ri_vector_copy(&(light_node->hit.normal), &surfinfo.normal);
#if 0
	// point light
	//ri_vector_sub(&light_dir, &(last_node->hit.pos), &light_pos);
	//ri_vector_normalize(&light_dir);
	//ri_vector_copy(&ray.org, &light_pos);
	ri_vector_copy(&ray.dir, &light_dir);
#endif

	/* cos1 = ray.dir . last->node->hit.normal */
	cos1 = ri_vector_dot3(&ray.dir, &(last_node->hit.normal));
	if (last_node->type == 'T') cos1 = -cos1;
	if (cos1 <= 0.0) {
		/* light source sees the back face. */	
		//printf("cos1 < 0.0\n");
		return 0;
	}

	/* cos2 = -ray.dir . light.normal */
	cos2 = -ri_vector_dot3(&ray.dir, &light_normal);
	if (cos2 <= 0.0) {
#if 0
		printf("ray.dir = ");
		ri_vector_print(&ray.dir);
		printf("normal = ");
		ri_vector_print(&light_normal);
		printf("cos2 = %f\n", cos2);
#endif
	}
	assert(cos2 >= 0.0);

	light_node->pdf = 1.0;	// TODO: fix me!
	light_node->G = cos1 * cos2;	// TODO: divide by distance?
	light_node->bsdf[0] = 1.0;
	light_node->bsdf[1] = 1.0;
	light_node->bsdf[2] = 1.0;
		
	//printf("last_node->type = %c\n", last_node->type);
	//printf("last_node->nvert = %d\n", new_path.nverts - 1);
	brdf(last_node->bsdf,
	     &last_node->hit,
	     &last_node->indir, &ray.dir, &last_node->hit.normal);

	if (last_node->bsdf[0] < 1.0e-6) {
#if 0
		if (last_node->type == 'T') {
			printf("last_node is caustics path:\n");
			printf("kt = ");
			ri_vector_print(&last_node->hit.geom->material->kt);
		}
		for (i = 1; i < new_path.nverts; i++) {
			printf("[%d]new_path.verts.kt = ", i);
			ri_vector_print(&new_path.verts[i].hit.geom->material->kt);
			printf("[%d]new_path.verts.pdf = %f\n", i, new_path.verts[i].pdf);
			printf("[%d]new_path.verts.G = %f\n", i, new_path.verts[i].G);
			printf("[%d]new_path.verts.type = %c\n", i, new_path.verts[i].type);
			printf("[%d]new_path.verts.bsdf = %f, %f, %f\n", i,
					new_path.verts[i].bsdf[0],
					new_path.verts[i].bsdf[1],
					new_path.verts[i].bsdf[2]);
			printf("[%d]new_path.radiance = %f, %f, %f\n", i,
					new_path.radiance[0],
					new_path.radiance[1],
					new_path.radiance[2]);
		}

#endif
	} 

	return 1;
}

static int
sample_outdir(const ri_material_t *material,	/* surface material */
	      const ri_vector_t *in, const ri_vector_t *normal,
	      double rand1, double rand2, double rand3,
	      ri_vector_t *out, double *pdf,  /* output */
	      unsigned char *type)
{
	const double kd = 0.5;		/* diffuse reflectance. */
	int   til;			/* Total internal reflection */

	if (ri_vector_ave(&material->kt) > 1.0e-3) {
		/* refraction */

		til = ri_refract(out, in, normal, 1.0f / 1.4f);

		/* TODO: Is this right calculation of PDF? */
		if (til) {	/* total internal reflection */
			*pdf = ri_vector_dot3(out, normal);	// cos(theta)	
			*type = 'S';
		} else {
			*pdf = -ri_vector_dot3(out, normal);	// cos(theta)	
			//*pdf = 1.0f;
			*type = 'T';
		}

		assert(*pdf >= 0.0);
		//printf("pdf = %f\n", *pdf);
		return 1;
	}

	if (ri_vector_ave(&material->ks) > 1.0e-3) {
		/* mirror reflection */
		ri_reflect(out, in, normal);
		*pdf = ri_vector_dot3(out, normal);	// cos(theta)	
		assert(*pdf >= 0.0f);
		*type = 'S';
		return 1;
	}

	/* diffuse reflection */

	*pdf = 0.0;
	*type = 'D';

	/* Russian Roulette. */
	if (rand1 > kd) {
		return 0;		/* absourbed */
	}

	sample_cosweight(normal, rand2, rand3, out, pdf);

	*pdf = kd * (*pdf);

	return 1;
}

static void
sample_cosweight(const ri_vector_t *normal, double r1, double r2,
		 ri_vector_t *outdir, double *pdf)
{
	int   i;
	float cost, sint, phi;
	ri_vector_t v;
	ri_vector_t basis[3];

	ri_ortho_basis(basis, normal);

	cost = (float)sqrt(r1);
	sint = (float)sqrt(1.0 - r1);
	phi = 2.0 * M_PI * r2;

	v.e[0] = (float)cos(phi) * sint;
	v.e[1] = (float)sin(phi) * sint;
	v.e[2] = (float)cost;

	for (i = 0; i < 3; i++) {
		outdir->e[i] = v.e[0] * basis[0].e[i]
			     + v.e[1] * basis[1].e[i]
			     + v.e[2] * basis[2].e[i];
	}
	outdir->e[3] = 1.0;
				
	*pdf = cost / M_PI;
}

static void
brdf(double f[3],
     const ri_surface_info_t *surfinfo,
     const ri_vector_t *indir,
     const ri_vector_t *outdir,
     const ri_vector_t *normal)
{
	ri_vector_t diff;
	ri_vector_t refvec;
	const ri_vector_t *color;
	ri_material_t *material;

	material = surfinfo->geom->material;
	color = &surfinfo->color;

	if (ri_vector_ave(&material->kt) > 1.0e-3) {
		// refraction

		ri_refract(&refvec, indir, normal, 1.0f / 1.4f);
		ri_vector_sub(&diff, &refvec, outdir);
		if (ri_vector_length(&diff) < 1.0e-4) {
			//printf("len << 1.0e-4\n");
			f[0] = 1.0f * color->e[0];
			f[1] = 1.0f * color->e[1];
			f[2] = 1.0f * color->e[2];
		} else {
#if 1
			printf("len > 1.0e-4\n");
			printf("in = "); ri_vector_print(indir);
			printf("normal = "); ri_vector_print(normal);
			printf("refvec = "); ri_vector_print(&refvec);
			printf("outdir = "); ri_vector_print(outdir);
#endif
	
			f[0] = 0.0f;
			f[1] = 0.0f;
			f[2] = 0.0f;
		}

		return;
	}

	if (ri_vector_dot3(normal, outdir) < 0.0) {
		f[0] = 0.0;
		f[1] = 0.0;
		f[2] = 0.0;
	} else {
		if (ri_vector_ave(&material->ks) > 1.0e-3) {
			// mirror reflection

			ri_reflect(&refvec, indir, normal);
			ri_vector_sub(&diff, &refvec, outdir);
			if (ri_vector_length(&diff) < 1.0e-4) {
				f[0] = 1.0f * color->e[0];
				f[1] = 1.0f * color->e[1];
				f[2] = 1.0f * color->e[2];
			} else {
				f[0] = 0.0f;
				f[1] = 0.0f;
				f[2] = 0.0f;
			}

			return;
		}

		/* diffuse reflection only. */
		f[0] = material->kd.e[0] * color->e[0] / M_PI;
		f[1] = material->kd.e[1] * color->e[1] / M_PI;
		f[2] = material->kd.e[2] * color->e[2] / M_PI;
	}
}

/* Mulate a coordinate randomly in [0,1] */
double mutate_value(int i, double value)
{
	const double max_mutation_dist = 0.2;
	double s;

	if (use_qmc && (i == 0 || i == 1)) {
		/* switch to use QMC */
		s = generalized_scrambled_halton(
				qmc_instance[2 * i + 1],
				0,
				2 * i + 1,
				ri_render_get()->perm_table);
		qmc_instance[2 * i + 1]++;
	} else {
		/* use MC */

		s = randomMT();
	}

	value += 2.0 * max_mutation_dist * (s - 0.5);

	if (value > 1.0) value -= 1.0;
	if (value < 0.0) value += 1.0;

	return value;
}

/* Lazy evaluation of a coordinate in the primary sample space.
 * Returns the i'th coordinate of the priary sample.
 */
static double
primary_sample(int i)
{
	/* TODO: use low-discrepancy sequence(QMC) instead of
	 * random number(MC).
	 */

	if (u[i].modify_time < time) {
		if (new_path.large_step) {	/* large step */
			ustack_push(u[i]);

			u[i].value = sample_value(i);
			u[i].modify_time = time; // modified now;
		} else {

			if (u[i].modify_time < large_step_time) {
				u[i].value = sample_value(i);
				u[i].modify_time = large_step_time;

			}

			while (u[i].modify_time < time - 1) { /* lazy eval */
				u[i].value = mutate_value(i, u[i].value);
				u[i].modify_time++;
			}

			ustack_push(u[i]);
			u[i].value       = mutate_value(i, u[i].value);
			u[i].modify_time = time;
		}
	}

	return u[i].value;

}

/* Sample u value using MC or QMC */
static double
sample_value(int i)
{
	double val;

	if (use_qmc && (i == 0 || i == 1)) {
		/* switch to use QMC */
		val = generalized_scrambled_halton(
				qmc_instance[2 * i], /* i */
				0,		   /* offset */
				2 * i,		   /* dim */
			 	ri_render_get()->perm_table);
		qmc_instance[2 * i]++;
	} else {
		/* use MC */
		val = randomMT();
	}

	return val;
}

/* Update the pixel of the image affected by the path. */
static void
contribute_path(const ri_display_drv_t *ddrv, const path_t *path, double weight)
{
	double      Fstar[3];		/* F* */
	double      col[3];
	float       L;
	float       fcol[3];
	unsigned char bytecol[3];
	
	/* F* = L / pdf */
	if (path->pdf < 1.0e-6) {
		return;
	}

	Fstar[0] = path->radiance[0] / path->pdf;
	Fstar[1] = path->radiance[1] / path->pdf;
	Fstar[2] = path->radiance[2] / path->pdf;

	L = CIEcol(Fstar);
	if (L < 1.0e-6f) return;

	/* col = weight * (Fstar / L) * path->I */
	col[0] = weight * (Fstar[0] / L) * path->I;
	col[1] = weight * (Fstar[1] / L) * path->I;
	col[2] = weight * (Fstar[2] / L) * path->I;

	// hack
	fcol[0] = (float)(col[0] * 30000.0);
	fcol[1] = (float)(col[1] * 30000.0);
	fcol[2] = (float)(col[2] * 30000.0);

	//printf("px,py = %lu, %lu\n", path->px, path->py);
	//printf("col = %f, %f, %f\n", fcol[0], fcol[1], fcol[2]);

	if (strcmp(ri_render_get()->context->option->display->display_type,
		   "framebuffer") == 0) {
		bytecol[0] = clamp(fcol[0] * 1000.0);
		bytecol[1] = clamp(fcol[1] * 1000.0);
		bytecol[2] = clamp(fcol[2] * 1000.0);

		ddrv->write(path->px, gpixheight - path->py - 1, bytecol);

	} else {
		ddrv->write(path->px, gpixheight - path->py - 1, fcol);
	}
	
}

static void
ustack_push(primary_coord_t coord)
{
	if (ustack_depth < 0) ustack_depth = 0;
	if (ustack_depth >= 3 * MAX_PATH_VERTICES) return;

	memcpy(&(ustack[ustack_depth]), &coord, sizeof(primary_coord_t));

	ustack_depth++;
}

static void
ustack_pop()
{
	if (ustack_depth < 0) return;

	ustack_depth--;
	//if (ustack_depth < 0) ustack_depth = 0;
}

static void
ustack_top(primary_coord_t *coord)
{
	if (ustack_depth < 0) return;

	if (coord != NULL) {
		memcpy(coord, &(ustack[ustack_depth]), sizeof(primary_coord_t));
	}
}

static int
ustack_empty()
{
	if (ustack_depth < 1)  return 1;

	return 0;
}

static void
get_camera(ri_vector_t *pos, ri_vector_t *dir, ri_matrix_t *c2w)
{
	ri_matrix_t        orientation;
	ri_matrix_t        m;
	ri_vector_t        v;

	ri_matrix_identity(&orientation);
	if (strcmp(ri_render_get()->context->option->orientation, RI_RH) == 0) {
		orientation.e[2][2] = - orientation.e[2][2];
	}

	/* compute camera to world matrix 
	 *
	 * c2w = orientation . (world_to_camera)^{-1}
	 *
	 */
	ri_matrix_copy(&m, &(ri_render_get()->context->world_to_camera));
	ri_matrix_inverse(&m);
	ri_matrix_mul(c2w, &m, &orientation);

	/* camera position in world coordinate. */
	ri_vector_set4(&v, 0.0, 0.0, 0.0, 1.0);
	ri_vector_transform(pos, &v, c2w);

	/* camera direction in world coordinate. */
	ri_vector_set4(&v, 0.0, 0.0, 1.0, 1.0);		/* +z */
	ri_vector_transform(dir, &v, c2w);
	ri_vector_sub(dir, dir, pos);
	ri_vector_normalize(dir);
}

static unsigned char
clamp(float f)
{
	int ival;

	ival = (int)(f * 255.5f);
	if (ival < 0) ival = 0;
	if (ival > 255) ival = 255;

	return (unsigned char)ival;
}
