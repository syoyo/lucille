/* Imprementation of irradiance gradient cacing.
 *
 * $Id: irradcache.c,v 1.4 2004/06/13 06:44:51 syoyo Exp $
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "memory.h"
#include "log.h"
#include "irradcache.h"
#include "array.h"
#include "octree.h"
#include "octree_frisken.h"
#include "render.h"

/* CIE color definitions */
#define CIE_rf 0.265074126
#define CIE_gf 0.670114631
#define CIE_bf 0.064811243

#define CIEcol(v) (CIE_rf * (v).e[0] + CIE_gf * (v).e[1] + CIE_bf * (v).e[2])


#ifndef M_PI
#define M_PI 3.1415926535
#endif

#define DEG2RAD M_PI / 180.0

static double error(const ri_vector_t *P, const ri_vector_t *Pi,
                    const ri_vector_t *N, const ri_vector_t *Ni,
		    double Ri, float proj_area);

static float weight(const ri_vector_t *p, const ri_vector_t *pi,
		    const ri_vector_t *n, const ri_vector_t *ni, float r);
static float exclude(const ri_vector_t *p, const ri_vector_t *pi,
		     const ri_vector_t *n, const ri_vector_t *ni);
static int   within_node(ri_octree_t *octree, const ri_vector_t *pi);
static int   within_node2(otCell *octree, const ri_vector_t *pi);
static int   inside(const ri_vector_t *center, const ri_vector_t *pos,
		    float width);
static void  rot_grad(ri_vector_t *grad, const ri_hemisphere_t *hemi);
static void  trans_grad(ri_vector_t *grad, const ri_hemisphere_t *hemi);
static void  irradcache_dump_trav(ri_octree_t *octree, FILE *fp);
static int   count_nirradcache(ri_octree_t *octree);
static void  count_nirradcache_trav(ri_octree_t *octree, int *nfound);
static int   nearly_equal(const ri_vector_t *v1, const ri_vector_t *v2);

static void subdiv_octree(otCell *cell);

int
ri_irradcache_insert(ri_octree_t *octree, ri_irradcache_t *val)
{
	//const float tolerance = 0.1;
	double      tolerance;
	int         index;
	float       width;
	ri_vector_t center;
	ri_irradcache_t *newirrad = NULL;

	if (octree == NULL) {
		ri_log(LOG_WARN, "octree == NULL");
		return 0;
	}

	tolerance = ri_render_get()->context->option->irradcache_insert_tolerance;

	/* if radius is very small(which implies illumination value is
	 * nearly 0), reject value.  */
	//if (val->r < 1.0e-3) return 0;

	if (octree->depth != 0 &&		/* not root node */
	    octree->width < 2 * val->r * tolerance) {
		if (!octree->vals) {
			octree->vals = ri_array_new(sizeof(ri_irradcache_t));
		}

		newirrad = (ri_irradcache_t *)
			   ri_mem_alloc(sizeof(ri_irradcache_t));
		ri_mem_copy(newirrad, val, sizeof(ri_irradcache_t));
		ri_array_insert(octree->vals,
				octree->vals->nelems,
				(void *)newirrad);


#if 0
		printf("irrad = ");
		ri_vector_print(val->e);
		printf("depth = %d, p = (%f, %f, %f), center = (%f, %f, %f), width = %f\n", octree->depth, val->p.e[0], val->p.e[1], val->p.e[2], octree->center.e[0], octree->center.e[1], octree->center.e[2], octree->width);
		printf("r * tole = %f, r = %f\n", val->r * tolerance, val->r);
#endif
	} else {
		index = within_node(octree, &val->p);

		if (octree->child[index] == NULL) {
			width = 0.5 * octree->width;
			ri_vector_copy(&center, &octree->center);

			switch(index) {
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

			octree->child[index] = ri_octree_new();
			octree->child[index]->depth = octree->depth + 1;
			octree->child[index]->width = width;
			ri_vector_copy(&(octree->child[index]->center),
				       &center);
		}

		ri_irradcache_insert(octree->child[index], val);
	}

	return 1;

}

int
ri_irradcache_insert2(otCell *octree, ri_irradcache_t *val)
{
	//const float tolerance = 0.1;
	double      tolerance;
	int         index;
	float       cell_width;
	ri_irradcache_t *newirrad = NULL;
	int         nextlevel;

	if (octree == NULL) {
		ri_log(LOG_WARN, "octree == NULL");
		return 0;
	}

	tolerance = ri_render_get()->context->option->irradcache_insert_tolerance;

	/* if radius is very small(which implies illumination value is
	 * nearly 0), reject value.  */
	//if (val->r < 1.0e-3) return 0;

	cell_width = (float)(1 << octree->level) * ri_render_get()->bmaxwidth;

	if (octree->level != OT_ROOT_LEVEL &&		/* not a root node */
	    cell_width < 2 * val->r * tolerance) {
		/* store irradiance sample */
		if (!octree->data) {
			octree->data = ri_array_new(sizeof(ri_irradcache_t));
		}

		newirrad = (ri_irradcache_t *)
			   ri_mem_alloc(sizeof(ri_irradcache_t));
		ri_mem_copy(newirrad, val, sizeof(ri_irradcache_t));
		ri_array_insert(octree->data,
				((ri_array_t *)octree->data)->nelems,
				(void *)newirrad);

	} else {
		index = within_node2(octree, &val->p);

		nextlevel = octree->level - 1;	/* next level */

		if (!octree->children) {
			subdiv_octree(octree);
		}

		if (!octree->children[index].children) {
			subdiv_octree(&(octree->children[index]));
		}

		//ri_irradcache_insert(octree->children[index], val);
	}

	return 1;

}

double
ri_irradcache_find(ri_octree_t  *octree,
		   const ri_vector_t *pos,
		   const ri_vector_t *normal,
		   ri_vector_t  *irradiance,
		   unsigned int *nfound)
{
	double tolerance;
	int i;
	//double d;
	double wi, di;
	double ws;
	//ri_vector_t      cross;
	//ri_vector_t      trans;
	ri_irradcache_t *ic;

	tolerance = ri_render_get()->context->option->irradcache_find_tolerance;

	ws = 0.0;

	if (octree->depth != 0 &&		/* not root node */
	    octree->vals) {
		for (i = 0; i < (int)octree->vals->nelems; i++) {
			ic = (ri_irradcache_t *)ri_array_at(octree->vals, i);

			if (nearly_equal(pos, &(ic->p))) {
				/* Query position is same as previously
				 * calculated irradiance cache value position,
				 * so, use its value.
				 */

				ri_vector_copy(irradiance, &ic->e);
				//irradiance->e[0] = 100.0;
				//irradiance->e[1] = 0.0;
				//irradiance->e[2] = 0.0;
				(*nfound) = 10000;
				return 1.0;
			}

			/* If vector (ic->p - pos) and
			 * normal vector (ic->n + normal) are perpendicular,
			 * exclude() returns slightly minus value.
			 */
			di = exclude(pos, &ic->p, normal, &ic->n);

			if (di < -1.0e-3) continue;

			wi = weight(pos, &ic->p, normal, &ic->n, ic->r);

			if (wi > 1.0 / tolerance) {
#if 0
				/* irradiance gradients version */
				d = 1.0;	/* 0'th order */
				
				/* gradient due to translation.
				 * (P - Pi) . Tgrad(Ei)
				 */
				ri_vector_sub(&trans, pos, ic->p);
				d += ri_vector_dot3(trans, ic->tg);

				/* gradient due to rotation.
				 * (Ni x N) . Rgrad(Ei)
				 */
				ri_vector_cross3(&cross, ic->n, normal);
				d += ri_vector_dot3(cross, ic->rg);

				/* E = w * Ei(1.0 + TransGrad + RotGrad) */
				irradiance->e[0] += wi * d * ic->e.e[0];
				irradiance->e[1] += wi * d * ic->e.e[1];
				irradiance->e[2] += wi * d * ic->e.e[2];
#else
				irradiance->e[0] += wi * ic->e.e[0];
				irradiance->e[1] += wi * ic->e.e[1];
				irradiance->e[2] += wi * ic->e.e[2];
#endif

				ws += wi;
				(*nfound)++;
			}
		}
	}

	for (i = 0; i < 8; i++) {
		if (octree->child[i] &&
		    inside(&(octree->child[i]->center), pos, octree->width)) {
			ws += ri_irradcache_find(octree->child[i],
						 pos, normal,
						 irradiance,
						 nfound);
			if (*nfound == 10000) return ws;
		}

	}

	return ws;
}

#if 0
double
ri_irradcache_find2(otCell            *octree,
		    const ri_vector_t *pos,
		    const ri_vector_t *normal,
		    ri_vector_t       *irradiance,
		    unsigned int      *nfound)
{
	double tolerance;
	int i;
	double wi, di;
	double ws;
	ri_irradcache_t *ic;

	tolerance = ri_render_get()->context->option->irradcache_find_tolerance;

	ws = 0.0;

	if (octree->depth != 0 &&		/* not root node */
	    octree->vals) {
		for (i = 0; i < (int)octree->vals->nelems; i++) {
			ic = (ri_irradcache_t *)ri_array_at(octree->vals, i);

			if (nearly_equal(pos, &(ic->p))) {
				/* Query position is same as previously
				 * calculated irradiance cache value position,
				 * so, use its value.
				 */

				ri_vector_copy(irradiance, &ic->e);
				irradiance->e[0] = 100.0;
				irradiance->e[1] = 0.0;
				irradiance->e[2] = 0.0;
				(*nfound) = 10000;
				return 1.0;
			}

			/* If vector (ic->p - pos) and
			 * normal vector (ic->n + normal) are perpendicular,
			 * exclude() returns slightly minus value.
			 */
			di = exclude(pos, &ic->p, normal, &ic->n);

			if (di < -1.0e-3) continue;

			wi = weight(pos, &ic->p, normal, &ic->n, ic->r);

			if (wi > 1.0 / tolerance) {
//#if 0
				/* irradiance gradients version */
				d = 1.0;	/* 0'th order */
				
				/* gradient due to translation.
				 * (P - Pi) . Tgrad(Ei)
				 */
				ri_vector_sub(&trans, pos, ic->p);
				d += ri_vector_dot3(trans, ic->tg);

				/* gradient due to rotation.
				 * (Ni x N) . Rgrad(Ei)
				 */
				ri_vector_cross3(&cross, ic->n, normal);
				d += ri_vector_dot3(cross, ic->rg);

				/* E = w * Ei(1.0 + TransGrad + RotGrad) */
				irradiance->e[0] += wi * d * ic->e.e[0];
				irradiance->e[1] += wi * d * ic->e.e[1];
				irradiance->e[2] += wi * d * ic->e.e[2];
//#else
				irradiance->e[0] += wi * ic->e.e[0];
				irradiance->e[1] += wi * ic->e.e[1];
				irradiance->e[2] += wi * ic->e.e[2];
//#endif

				ws += wi;
				(*nfound)++;
			}
		}
	}

	for (i = 0; i < 8; i++) {
		if (octree->child[i] &&
		    inside(&(octree->child[i]->center), pos, octree->width)) {
			ws += ri_irradcache_find(octree->child[i],
						 pos, normal,
						 irradiance,
						 nfound);
			if (*nfound == 10000) return ws;
		}

	}

	return ws;
}
#endif

void
ri_irradcache_calc_gradient(ri_vector_t *tg, ri_vector_t *rg,
			    const ri_hemisphere_t *hemi)
{
	trans_grad(tg, hemi);
	rot_grad(rg, hemi);
}

void
ri_irradcache_dump(ri_octree_t  *octree)
{
	FILE *fp;
	int  ncache;
	ri_option_t *opt;

	opt = ri_render_get()->context->option;
	
	if (opt->irradcache_file) {
		fp = fopen(opt->irradcache_file, "w");
	} else {
		fp = fopen("irradcache.dat", "w");
	}

	if (!fp) return;

	ncache = count_nirradcache(octree);
	fprintf(fp, "%d\n", ncache);
	
	irradcache_dump_trav(octree, fp);

	if (opt->irradcache_file) {
		fp = fopen(opt->irradcache_file, "w");
		printf("irradiance cache saved to [ %s ]\n", opt->irradcache_file);
	} else {
		printf("irradiance cache saved to [ %s ]\n", "irradcache.dat");
	}

	fclose(fp);
}

/* --- private functions --- */
static void
subdiv_octree(otCell *cell)
{
	int i;
	int nlevel = cell->level - 1;	/* next level */

	cell->children = (otCell *)ri_mem_alloc(sizeof(otCell) * 8);

	for (i = 0; i < 8; i++) {
		cell->children[i].children  = NULL;
		cell->children[i].data      = NULL;
		cell->children[i].parent    = cell;
		cell->children[i].level     = nlevel;
	}

	cell->children[0].xLocCode = cell->xLocCode;
	cell->children[0].yLocCode = cell->yLocCode;
	cell->children[0].zLocCode = cell->zLocCode;

	cell->children[1].xLocCode = cell->xLocCode | (1 << nlevel);
	cell->children[1].yLocCode = cell->yLocCode;
	cell->children[1].zLocCode = cell->zLocCode;

	cell->children[2].xLocCode = cell->xLocCode;
	cell->children[2].yLocCode = cell->yLocCode | (1 << nlevel);
	cell->children[2].zLocCode = cell->zLocCode;

	cell->children[3].xLocCode = cell->xLocCode | (1 << nlevel);
	cell->children[3].yLocCode = cell->yLocCode | (1 << nlevel);
	cell->children[3].zLocCode = cell->zLocCode;

	cell->children[4].xLocCode = cell->xLocCode;
	cell->children[4].yLocCode = cell->yLocCode;
	cell->children[4].zLocCode = cell->zLocCode | (1 << nlevel);

	cell->children[5].xLocCode = cell->xLocCode | (1 << nlevel);
	cell->children[5].yLocCode = cell->yLocCode;
	cell->children[5].zLocCode = cell->zLocCode | (1 << nlevel);

	cell->children[6].xLocCode = cell->xLocCode;
	cell->children[6].yLocCode = cell->yLocCode | (1 << nlevel);
	cell->children[6].zLocCode = cell->zLocCode | (1 << nlevel);

	cell->children[7].xLocCode = cell->xLocCode | (1 << nlevel);
	cell->children[7].yLocCode = cell->yLocCode | (1 << nlevel);
	cell->children[7].zLocCode = cell->zLocCode | (1 << nlevel);
}

static void
irradcache_dump_trav(ri_octree_t *octree, FILE *fp)
{
	unsigned int i;
	ri_irradcache_t *ic;

	if (octree->vals) {
		for (i = 0; i < octree->vals->nelems; i++) {
			ic = (ri_irradcache_t *)ri_array_at(octree->vals, i);

			fprintf(fp, "%f %f %f ",
				ic->p.e[0], ic->p.e[1], ic->p.e[2]);
			fprintf(fp, "%f %f %f ",
				ic->n.e[0], ic->n.e[1], ic->n.e[2]);
			fprintf(fp, "%f %f %f ",
				ic->e.e[0], ic->e.e[1], ic->e.e[2]);
			fprintf(fp, "%f %f %f ",
				ic->tg.e[0], ic->tg.e[1], ic->tg.e[2]);
			fprintf(fp, "%f %f %f ",
				ic->rg.e[0], ic->rg.e[1], ic->rg.e[2]);
			fprintf(fp, "%f\n",
				ic->r);
		}
	}

	for (i = 0; i < 8; i++) {
		if (octree->child[i]) {
			irradcache_dump_trav(octree->child[i], fp);
		}
	}
}

/* Imporved error metric function presented in
 *
 * "An Approximate Global Illumination System for Computer Generated Films"
 * Eric Tabellion and Arnauld Lamorlette, SIGGRAPH 2004.
 *
 * It's functionality is evidenved in Shrek 2.
 */
static double
error(const ri_vector_t *P, const ri_vector_t *Pi,
      const ri_vector_t *N, const ri_vector_t *Ni,
      double Ri, const float proj_area)
{
	const double k = 1.0;			// TODO: to be controlled
	const double alpha = 10.0 * DEG2RAD;	// TODO: to be controlled 

	float dotn;

	double dist_p;
	double minR, maxR;
	double maxE;
	double R_plus, R_minus;
	double Epi, Eni;
	double sqrOneMinusCosA;

	ri_vector_t PPi;

	/* R+ and R- is respectively 10 and 1.5 times the square root of
	 * the projected pixel area.
	 */
	R_plus  = 10.0 * proj_area;
	R_minus =  1.5 * proj_area;

	/* max(min(Ri/2, R+), R-) */
	minR = (0.5 * Ri) < R_plus ? 0.5 * Ri : R_plus;
	maxR = minR > R_minus ? minR : R_minus;

	ri_vector_sub(&PPi, P, Pi);
	dist_p = (double)ri_vector_length(&PPi);

	Epi = dist_p / maxR;

	/* sqrt(1 - cos(A)) */
	sqrOneMinusCosA = sqrt(1.0 - cos(alpha));

	dotn = ri_vector_dot3(N, Ni);

	Eni = sqrt(1.0 - (double)dotn) / sqrOneMinusCosA;

	maxE = Epi > Eni ? Epi : Eni;

	return k * maxE;
}

static float
weight(const ri_vector_t *p, const ri_vector_t *pi,
       const ri_vector_t *n, const ri_vector_t *ni, float r)
{
	float w;
	ri_vector_t v;
	float l;
	float ndot;

	/* l = |P - Pi| */
	ri_vector_sub(&v, p, pi);
	l = ri_vector_length(&v);
	
	/* ndot = N . Ni */
	ndot = ri_vector_dot3(n, ni);
	if (ndot < 0.0) {
		//printf("ndot < 0.0\n");
		return 0.0;
	}
	
	/*                  1.0
	 * w = ------------------------------------
	 *     |P - Pi| / Ri + sqrt(1.0 - (N . Ni))
	 */

	if (l < 1.0e-6) {
		fprintf(stderr, "l = 0.0\n");
		return 0.0;
	}

	if (ndot >= 1.0) {
		w = 1.0 / (l / r);
	} else {
		w = 1.0 / (l / r + sqrt(1.0 - ndot));
	}

	return w;
}

static float
weight2(const ri_vector_t *p, const ri_vector_t *pi,
        const ri_vector_t *n, const ri_vector_t *ni, float r)
{
	float w;

	//w = 1.0 - error(p, pi, n, ni, r);
	w = 1.0;

	return w;
}


static float
exclude(const ri_vector_t *p, const ri_vector_t *pi,
	const ri_vector_t *n, const ri_vector_t *ni)
{
	float dot;
	ri_vector_t pp;
	ri_vector_t nn;

	/* a test to reject illuminance value that is "in front" of p */

	/*           
	 * calculate ((P - Pi) . (N + Ni))
	 *           
	 */

	ri_vector_sub(&pp, p, pi);
	ri_vector_add(&nn, n, ni);

	dot = ri_vector_dot3(&pp, &nn); 

	return dot;
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

static int
within_node2(otCell *octree, const ri_vector_t *pi)
{
#if 0
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
#endif
	return 0;
}

static int
inside(const ri_vector_t *center, const ri_vector_t *pos, float width)
{
	if (center->e[0] - width <= pos->e[0] &&
	    center->e[0] + width >= pos->e[0] &&
	    center->e[1] - width <= pos->e[1] &&
	    center->e[1] + width >= pos->e[1] &&
	    center->e[2] - width <= pos->e[2] &&
	    center->e[2] + width >= pos->e[2]) {
		return 1;
	}

	return 0; 
}

static void
rot_grad(ri_vector_t *grad, const ri_hemisphere_t *hemi)
{
	unsigned int j, k;
	double mag = 0.0;
	double phi, xd, yd;

	xd = yd = 0.0;
	for (k = 0; k < hemi->nphi; k++) {
		for (j = 0; j < hemi->ntheta; j++) {
			mag += CIEcol(hemi->samples[j][k].L) /
			       sqrt(hemi->ntheta / (j + 0.5) - 1.0);
		}

		phi = 2.0 * M_PI * (k + 0.5) / hemi->nphi + M_PI / 2.0;
		xd += mag * cos(phi);
		yd += mag * sin(phi);
	}

	for (j = 0; j < 3; j++) {
		grad->e[j] = (xd * hemi->basis[0].e[j] +
			      yd * hemi->basis[1].e[j]) /
			     (hemi->ntheta * hemi->nphi);
	}

}

static void
trans_grad(ri_vector_t *grad, const ri_hemisphere_t *hemi)
{
	unsigned int j, k;
	double nextsine, lastsine, b, d;
	double mag0, mag1;
	double phi, cosp, sinp, xd, yd;
	
	xd = yd = 0.0;

	for (k = 0; k < hemi->nphi; k++) {
		mag0 = mag1 = 0.0;
		lastsine = 0.0;

		for (j = 0; j < hemi->ntheta; j++) {
			b = CIEcol(hemi->samples[j][k].L);

			if (j > 0) {
				/* min(distance[j-1][k], distance[j][k] */
				d = hemi->samples[j - 1][k].r;
				if (d < hemi->samples[j][k].r) {
					d = hemi->samples[j][k].r;
				}

				/* sin(t) * cos(t)^2 */
				d *= lastsine * (1.0 - (double)j / hemi->ntheta);
				mag0 += d * (b - CIEcol(hemi->samples[j-1][k].L));
			}

			nextsine = sqrt((double)(j + 1) / hemi->ntheta);
			
			if (k > 0) {
				/* min(distance[j][k-1], distance[j][k] */
				d = hemi->samples[j][k-1].r;
				if (d < hemi->samples[j][k].r) {
					d = hemi->samples[j][k].r;
				}

				mag1 += d * (nextsine - lastsine) *
					(b - CIEcol(hemi->samples[j][k-1].L));
			} else {
				/* min(distance[j][NP], distance[j][k] */
				d = hemi->samples[j][hemi->nphi - 1].r;
				if (d < hemi->samples[j][k].r) {
					d = hemi->samples[j][k].r;
				}

				mag1 += d * (nextsine - lastsine) *
					(b - CIEcol(hemi->samples[j][hemi->nphi - 1].L));
				
			}

			lastsine = nextsine;
		}

		mag0 *= 2.0 * M_PI / hemi->nphi;
		phi   = 2.0 * M_PI * (double)j / hemi->nphi;
		cosp  = cos(phi); sinp = sin(phi);
		xd   += mag0 * cosp - mag1 * sinp;
		yd   += mag0 * sinp + mag1 * cosp;
	}

	for (j = 0; j < 3; j++) {
		grad->e[j] = (xd * hemi->basis[0].e[j] +
			      yd * hemi->basis[1].e[j]) / M_PI;
	}
	
}

static int 
count_nirradcache(ri_octree_t *octree)
{
	int nfound;

	nfound = 0;

	count_nirradcache_trav(octree, &nfound);

	return nfound;
}

static void
count_nirradcache_trav(ri_octree_t *octree, int *nfound)
{
	unsigned int i;

	if (octree->vals) {
		(*nfound) += (int)octree->vals->nelems;
	}

	for (i = 0; i < 8; i++) {
		if (octree->child[i]) {
			count_nirradcache_trav(octree->child[i], nfound);
		}
	}
}

static int
nearly_equal(const ri_vector_t *v1, const ri_vector_t *v2)
{
	if ((fabs(fabs(v1->e[0]) - fabs(v2->e[0])) < 1.0e-6) &&
	    (fabs(fabs(v1->e[1]) - fabs(v2->e[1])) < 1.0e-6) &&
	    (fabs(fabs(v1->e[2]) - fabs(v2->e[2])) < 1.0e-6)) {
		return 1;
	}

	return 0;
}

void
ri_irradcache_load(ri_octree_t  *octree, const char *cachefile)
{
	FILE *fp;
	int  i;
	int  npoints;
	ri_irradcache_t val;

	ri_log_and_return_if(octree == NULL);

	fp = fopen(cachefile, "r");
	if (!fp) return;

	fscanf(fp, "%d\n", &npoints);
	
	for (i = 0; i < npoints; i++) {
		fscanf(fp, "%f %f %f ", &(val.p.e[0]), &(val.p.e[1]), &(val.p.e[2]));
		fscanf(fp, "%f %f %f ", &(val.n.e[0]), &(val.n.e[1]), &(val.n.e[2]));
		fscanf(fp, "%f %f %f ", &(val.e.e[0]), &(val.e.e[1]), &(val.e.e[2]));
		fscanf(fp, "%f %f %f ", &(val.tg.e[0]), &(val.tg.e[1]), &(val.tg.e[2]));
		fscanf(fp, "%f %f %f ", &(val.rg.e[0]), &(val.rg.e[1]), &(val.rg.e[2]));
		fscanf(fp, "%f\n", &(val.r));

		printf("p = (%f, %f, %f)\n", val.p.e[0], val.p.e[1], val.p.e[2]);

		ri_irradcache_insert(octree, &val);
	}

	fclose(fp);

	return;
}
