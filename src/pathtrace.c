/*
 * Simple Path Tracing implementation.
 *
 * Syoyo Fujita
 * 
 * Limitation:
 *
 *   o No QMC sampling.
 *   o No parallel rendering and multi threading.
 *   o HDRI IBL light only(distant environment map).
 *   o Perspective camera only(orthogonal camera is not supported).
 *
 *
 * TODO:
 *
 *   o QMC
 *   o MPI Parallel rendering and pthread multi threading.
 *   o Integrate the code to transport.c.
 *   o Area light source support.
 *
 *
 * 8 tab stop, 8 indent.
 *
 * $Id: pathtrace.c,v 1.2 2004/06/13 06:44:51 syoyo Exp $
 * 
 */

/* File: pathrrace.c
 *
 * Simple path tracing code.
 *
 * Reference:
 *
 * - James T. Kajiya,
 *   "The rendering equation",
 *   SIGGRAPH '86 Proceedings, pp. 143-150, 1986.
 *
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
//#include "qmc.h"

#ifndef M_PI
#define M_PI 3.141592
#endif

#define floateq(a, b) ((float)fabs((a) - (b)) < 1.0e-6f) ? 1 : 0

#define MAX_PATH_VERTICES 10		/* max ray depth		*/

typedef struct _pathnode_t
{
	ri_vector_t       indir;	/* incident direction		*/
	ri_surface_info_t surfinfo;	/* surface information of path node */
	int               interior;	/* ray is in volume of object. */
	//double            pdf;		/* path's PDF */
	double            G[3];		/* path's geometry factor */
	int               depth;
} pathnode_t;

ri_light_t        *light;
ri_matrix_t        c2w;			/* camera to world	           */
ri_vector_t        cam_pos;		/* camera position in world coord  */
ri_vector_t        cam_dir;		/* camera direction in world coord */
int                pixwidth;		/* screen pixel width              */
int                pixheight;		/* screen pixel height             */

/* trace a ray from camera through pixel (x, y) */
static void   trace_pixel(ri_vector_t *radiance,	/* output */
			  int x, int y);

/* get a ray direction from camera, which through pixel (x, y).  */
static int    sample_pixel(ri_vector_t *dir, int x, int y);

/* recursively trace a ray path */
static int    trace_path(pathnode_t *node);	/* path's current node. */

/* sample light source. */
static int    light_sample(ri_vector_t *radiance,	/* output           */
			   const ri_vector_t *pos,	/* path's pos       */
			   const ri_vector_t *dir);	/* sample direction */

/* determine whether to continue tracing or not */
static int russian_roulette(const ri_material_t *material);

/* sample reflection type according to surface material */
static unsigned char sample_reflection_type(const ri_material_t *material);

/* sample outgoing ray direction according to surface material and BRDF. */ 
static int    sample_outdir(ri_vector_t *out,			/* output */
			    unsigned char *type,	
			    int interior,
			    const ri_material_t *material,
			    const ri_vector_t *in, const ri_vector_t *normal);

/* cosine weighted sampling of hemisphere(for Lambert surface reflection). */
static void   sample_cosweight(ri_vector_t *outdir,             /* output */
			       const ri_vector_t *normal);

/* evaluate BRDF */
static void   brdf(double f[3],				/* output: BRDF */
		   unsigned char type,			/* reflection type */
		   const ri_surface_info_t *surfinfo,	/* surface property */
		   const ri_vector_t *indir,		/* incident direction */
		   const ri_vector_t *outdir,		/* outgoing direction */
		   const ri_vector_t *normal);		/* surface normal */

/* get camera information from scene data */
static void get_camera(ri_vector_t *pos, ri_vector_t *dir, ri_matrix_t *c2w);

/*
 * path trace across whole screen image.
 */
void
ri_transport_pathtrace(const ri_display_drv_t *ddrv)
{
	int i;
	int x, y;
	int nsamples;			/* nsamples per pixel */
	int ntotalpixels;
	int nfinishedpixels;
	double dcol[3];			/* tempolary color buffer */
	float  fcol[3];
	ri_option_t *opt;		/* rendering options */

	ri_vector_t radiance;

	/* Initialize */
	opt = ri_render_get()->context->option;
	pixwidth  = opt->camera->horizontal_resolution;
	pixheight = opt->camera->vertical_resolution;
	nsamples = opt->pt_nsamples;

	ntotalpixels = pixwidth * pixheight * nsamples;
	nfinishedpixels = 0;

	light = (ri_light_t *)(ri_list_first(ri_render_get()->lightlist)->data);

	get_camera(&cam_pos, &cam_dir, &c2w);

	/* for each pixel, trace nsamples rays. */
	for (x = 0; x < pixwidth; x++) {
		for (y = pixheight - 1; y >= 0; y--) {
			dcol[0] = 0.0; dcol[1] = 0.0; dcol[2] = 0.0;

			for (i = 0; i < nsamples; i++) {
				trace_pixel(&radiance, x, y);
				// add_color(x, y, radiance);
				dcol[0] += (double)radiance.e[0];
				dcol[1] += (double)radiance.e[1];
				dcol[2] += (double)radiance.e[2];

			}

			nfinishedpixels += nsamples;

#if 0
			printf("%f %% finished\r",
				(float)(nfinishedpixels * 100.0 /
					ntotalpixels));
			fflush(stdout);
#endif
	
			fcol[0] = (float)(dcol[0] / (double)nsamples);
			fcol[1] = (float)(dcol[1] / (double)nsamples);
			fcol[2] = (float)(dcol[2] / (double)nsamples);
			ddrv->write(x, pixheight - 1 - y, &fcol[0]);
		}
	}
}

static void
trace_pixel(ri_vector_t *radiance, int x, int y)
{
	unsigned char     type;
	double            bsdf[3];
	ri_ray_t          ray;
	ri_surface_info_t surfinfo;
	pathnode_t        node;
	ri_vector_t       Le;
 
	/* first check a ray hits scene object through pixel (x, y) */
	ri_vector_copy(&ray.org, &cam_pos);
	sample_pixel(&ray.dir, x, y);

	ray.thread_num = 0;

	if (!ri_raytrace(ri_render_get(), &ray, &surfinfo)) {
		/* hits background */
		ri_texture_ibl_fetch(radiance, light->texture, &ray.dir);
		return;
	}

	node.depth = 2;
	node.G[0] = 1.0; node.G[1] = 1.0; node.G[2] = 1.0;
	ri_mem_copy(&node.surfinfo, &surfinfo, sizeof(ri_surface_info_t));
	ri_vector_copy(&node.indir, &ray.dir);
	// assume that the camera is not located in the transparent object
	node.interior = 0;

	trace_path(&node);

	/* connect the path to the IBL light source */
	type = sample_reflection_type(node.surfinfo.geom->material);

	//if (node.interior) printf("???\n");

	sample_outdir(&ray.dir, &type,
		      node.interior,
		      node.surfinfo.geom->material,
		      &node.indir, &node.surfinfo.normal);

	brdf(bsdf,
	     type, &node.surfinfo,
	     &node.indir, &ray.dir, &node.surfinfo.normal);
	node.G[0] *= bsdf[0];
	node.G[1] *= bsdf[1];
	node.G[2] *= bsdf[2];

	ri_vector_copy(&ray.org, &node.surfinfo.pos);

	light_sample(&Le, &ray.org, &ray.dir);

	radiance->e[0] = Le.e[0] * node.G[0];
	radiance->e[1] = Le.e[1] * node.G[1];
	radiance->e[2] = Le.e[2] * node.G[2];
}

static int
trace_path(pathnode_t *path)
{
	unsigned char     type;		/* type of reflection */
	int               hit;
	int               prev_interior;
	double            bsdf[3];
	ri_vector_t       outdir;
	ri_ray_t          ray;
	ri_surface_info_t next_surfinfo;

	ri_material_t *material;

	if (path->depth >= MAX_PATH_VERTICES) {
		return 0;
	}

	material = path->surfinfo.geom->material;

	if (!russian_roulette(material)) {
		return 0;
	}

	type = sample_reflection_type(material);

	prev_interior = path->interior;

	if (path->interior && !floateq(material->ior, 1.0f)) {
		/* ray exits from a transparent object */
		path->interior = 0;
	}

	sample_outdir(&outdir, &type,
		      path->interior, material,
		      &path->indir, &path->surfinfo.normal);

	if (type == 'T' && prev_interior) {
		/* ray enters into a transparent object */
		path->interior = 1;
	}


	/* find next surface point where ray hits */
	ri_vector_copy(&ray.org, &path->surfinfo.pos);
	ri_vector_copy(&ray.dir, &outdir);

	ray.thread_num = 0;

	hit = ri_raytrace(ri_render_get(), &ray, &next_surfinfo);
	if (!hit) {
		return 0;	
	}

	brdf(bsdf,
	     type, &path->surfinfo,
	     &path->indir, &outdir, &path->surfinfo.normal);
	path->G[0] *= bsdf[0];
	path->G[1] *= bsdf[1];
	path->G[2] *= bsdf[2];

	path->depth++;

	ri_vector_copy(&path->indir, &outdir);
	//ri_vector_neg(&path->indir);
	ri_mem_copy(&path->surfinfo, &next_surfinfo, sizeof(ri_surface_info_t));
	
	return trace_path(path);

}

/* Sample a random point on the subpixel (x, y). */
static int
sample_pixel(ri_vector_t *dir, int x, int y)
{
	double p[2];			/* random sampling point */
	float  fov;
	float  flen;			/* focal length */

	ri_vector_t        v;
	ri_camera_t       *camera;

	camera = ri_render_get()->context->option->camera;
	fov    = camera->fov;
	flen   = 1.0f / (float)tan((fov * M_PI / 180.0) * 0.5);

	p[0] = (randomMT() + x);
	p[1] = (randomMT() + y);

	dir->e[0] = (2.0 * p[0] - (double)pixwidth ) / (double)pixwidth;
	dir->e[0] *= (camera->screen_window[1] - camera->screen_window[0]) / 2;
	dir->e[1] = (2.0 * p[1] - (double)pixheight) / (double)pixheight;
	dir->e[1] *= (camera->screen_window[3] - camera->screen_window[2]) / 2;
	dir->e[2] = flen;
	dir->e[3] = 1.0f;
	//assert(dir->e[0] >= -1.0);
	//assert(dir->e[0] <=  1.0);
	//assert(dir->e[1] >= -1.0);
	//assert(dir->e[1] <=  1.0);

	ri_vector_transform(&v, dir, &c2w);	/* camera to world */
	/* dir = v - camera.pos */
	ri_vector_sub(dir, &v, &cam_pos);
	ri_vector_normalize(dir);

	return 1;
}

static int
light_sample(ri_vector_t *radiance,
	     const ri_vector_t *pos, const ri_vector_t *dir)
{
	int               hit;
	ri_ray_t          ray;
	ri_surface_info_t surfinfo;

	/* visibility check */
	ri_vector_copy(&ray.org, pos);
	ri_vector_copy(&ray.dir, dir);

	ray.thread_num = 0;
	
	hit = ri_raytrace(ri_render_get(), &ray, &surfinfo);
	if (hit) {
		/* a ray doesn't hit background environment */
		ri_vector_zero(radiance);
		return 0;
	}

	/* sample enviroment map in the direction of ray.dir */
	ri_texture_ibl_fetch(radiance, light->texture, &ray.dir);

	return 1;
}

/*
 * russian roulette to choose whether continue tracing or not according to
 * surface material.
 * return 1 for continue, return 0 not.
 *
 */
static int
russian_roulette(const ri_material_t *material)
{
	double d, s, t;
	double r;

	r = randomMT();

	d = ri_vector_ave(&material->kd);
	s = ri_vector_ave(&material->ks);
	t = ri_vector_ave(&material->kt);
	assert(d + s + t <= 1.0);

	if (r > d + s + t) {	/* reject */
		return 0;
	}

	return 1;

}

static unsigned char
sample_reflection_type(const ri_material_t *material)
{
	unsigned char type;
	double d, s, t;
	double r;

	d = ri_vector_ave(&material->kd);
	s = ri_vector_ave(&material->ks);
	t = ri_vector_ave(&material->kt);
	assert(d + s + t <= 1.0);

	r = randomMT() * (d + s + t);


	/* randomly choose reflection type */

	if (r < d) {			/* diffuse */
		type = 'D';
	} else if (r < d + s) {		/* specular reflection */
		type = 'S';
	} else {  			/* specular refraction */
		type = 'T';
	}

	return type;
}

static int
sample_outdir(ri_vector_t *out,			/* output */
	      unsigned char *type,
	      int interior,
	      const ri_material_t *material,	/* surface material */
	      const ri_vector_t *in, const ri_vector_t *normal)
{
	//double d, s, t;
	//double r;
	float eta;

	int   til;			/* Total internal reflection */

	switch (*type) {
	case 'D':
		sample_cosweight(out, normal);
		break;
	case 'S':
		ri_reflect(out, in, normal);
		break;
	case 'T':
		//printf("T\n");

		if (interior) {
			eta = material->ior / 1.0f;
		} else {
			eta = 1.0f / material->ior;
		}

		til = ri_refract(out, in, normal, eta);

		if (til) {	/* total internal reflection */
			*type = 'S';
		} else {
			*type = 'T';
		}
		break;
	default:
		/* ??? */
		break;
	}

	return 1;
}

static void
sample_cosweight(ri_vector_t *outdir, const ri_vector_t *normal)
{
	int   i;
	double r[2];
	double cost, sint, phi;
	ri_vector_t v;
	ri_vector_t basis[3];

	ri_ortho_basis(basis, normal);

	r[0] = randomMT();
	r[1] = randomMT();

	cost = sqrt(r[0]);
	sint = sqrt(1.0 - r[0]);
	phi = 2.0 * M_PI * r[1];

	v.e[0] = (float)(cos(phi) * sint);
	v.e[1] = (float)(sin(phi) * sint);
	v.e[2] = (float)cost;

	for (i = 0; i < 3; i++) {
		outdir->e[i] = v.e[0] * basis[0].e[i]
			     + v.e[1] * basis[1].e[i]
			     + v.e[2] * basis[2].e[i];
	}
	outdir->e[3] = 1.0;
}

static void
brdf(double f[3],
     unsigned char type,
     const ri_surface_info_t *surfinfo,
     const ri_vector_t *indir,
     const ri_vector_t *outdir,
     const ri_vector_t *normal)
{
	ri_material_t *material;
	const ri_vector_t   *color;

	material = surfinfo->geom->material;
	color = &surfinfo->color;

	if (type == 'D') {
		f[0] = material->kd.e[0] * color->e[0] / M_PI;
		f[1] = material->kd.e[1] * color->e[1] / M_PI;
		f[2] = material->kd.e[2] * color->e[2] / M_PI;
	} else if (type == 'S') {
		f[0] = material->ks.e[0] * color->e[0];
		f[1] = material->ks.e[1] * color->e[1];
		f[2] = material->ks.e[2] * color->e[2];
	} else {		/* 'T' */
		f[0] = material->kt.e[0] * color->e[0];
		f[1] = material->kt.e[1] * color->e[1];
		f[2] = material->kt.e[2] * color->e[2];
	}
}

static void
get_camera(ri_vector_t *pos, ri_vector_t *dir, ri_matrix_t *c2w)
{
	ri_matrix_t        orientation;
	ri_matrix_t        m;
	ri_vector_t        v;

	/* if world coordinates is right-handed,
	 * convert it to left-handed(RenderMan default).
	 */
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
