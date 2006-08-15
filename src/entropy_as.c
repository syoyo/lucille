/*
 * File: entropy_as.c
 *
 * This code is an implementation of
 *
 * - Jaume Rigau , Miquel Feixas and Mateu Sbert,
 *   "Entropy-based Adaptive Sampling",
 *    Graphics Interface 2003.
 *
 * $Id: ibl.h,v 1.5 2004/06/13 06:44:51 syoyo Exp $
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>

#include "entropy_as.h"
#include "random.h"
#include "qmc.h"

#define LOCAL_DEBUG

#define MAXDEPTH 5			/* max tree depth 		*/
#define NR 4				/* the number of regions of an n-node */

typedef struct _node_t
{
	double q;			/* probability of the node	*/
	double c[3];			/* RGB color			*/
	double c_ave[3];		/* average color int the n-node	*/
	struct _node_t *children[NR];	/* leaf tree			*/
} node_t;

static node_t *alloc_node();
static double pixel_channel_entropy(int Ns,
static double pixel_channel_contrast(int Ns,

static node_t *
alloc_node()
{
	int i;

	node_t *newnode;

	newnode = (node_t *)malloc(sizeof(node_t));
	for (i = 0; i < NS; i++) {
		newnode->children[i] = NULL;
	}

	return newnode;
}

int
entropy_as(float *rgb, int npixels, int Ns)
{
	int i = 0;
	node_t *tree[MAXDEPTH];

	/* root tree(= whole image plane) */
	tree[0] = alloc_node();
	tree[0]->q = 1.0;

	/* calculates average color int the image */
	tree[0]->c_ave[0] = 0.0;
	tree[0]->c_ave[1] = 0.0;
	tree[0]->c_ave[2] = 0.0;
	for (i = 0; i < npixels; i++) {
		tree[0]->c_ave[0] += rgb[3 * i + 0];
		tree[0]->c_ave[1] += rgb[3 * i + 1];
		tree[0]->c_ave[2] += rgb[3 * i + 2];
	}

#ifdef LOCAL_DEBUG
	fprintf(stderr, "root: c_ave = (%f, %f, %f)\n",
		tree[0]->c_ave[0], tree[0]->c_ave[1], tree[0]->c_ave[2]);
#endif

}

static double
calc_q(node_t *node, int n)
{
	int i;

	double q;

	for (i = 0; i < NS; i++) {
		q = pow(NR, n);

	}
}

/*
 * Function: pixel_channel_entropy
 *
 *     Evaluates the equation 6 in the paper.
 *
 * Returns:
 *
 *     H^{c}
 */
static double
pixel_channel_entropy(double *c, int Ns)
{
	int    i;
	double c_denom;
	double p;
	double Hc;

	c_denom = 0.0;
	for (i = 0; i < Ns; i++) {
		c_denom += c[i];	
	}

	assert(c_denom > 0.0);

	/* evaluates pixel channel entropy. */
	Hc = 0.0;
	for (i = 0; i < Ns; i++) {
		p = c[i] / c_denom;
		Hc += -(p * log(p));
	}

#ifdef LOCAL_DEBUG
	fprintf(stderr, "H^{c} = %lf\n", Hc);
#endif

	return H;
}

/*
 * Function: pixel_channel_contrast
 *
 *     Evaluates the equation 7 in the paper.
 *
 * Returns:
 *
 *     C^{c}
 */
static double
pixel_channel_contrast(double Hc, int Ns)
{
	double Cc;

	Cc = 1.0 - Hc / log(Ns);

	return Cc;
}

/*
 * Function: pixel_color_contrast
 *
 *     Evaluates the equation 8 in the paper.
 *     We assume Nc = 3 (i.e. RGB color).
 *
 * Parameters:
 *
 *     *r    - the vector of red component with length Ns.
 *     *g    - the vector of green component with length Ns.
 *     *b    - the vector of blue component with length Ns.
 *     *Cc   - the vector of pixel channel contrast with length 3.
 *     Ns    - the number of samples.
 *
 * Returns:
 *
 *     Evaluated pixel color contrast.
 *
 */
static double
pixel_color_contrast(double *pcc,
		     double *r, double *g, double *b, double *Cc, int Ns)
{
	/* RGB perceptual coefficients */
	const double w[3] = {0.213, 0.715, 0.072};

	int    i;
	double c_ave[3];
	doub
	double Cc;
	double weight;
	double pcc;

	weight = w[0] + w[1] + w[2];

	c_ave[0] = c_ave[1] = c_ave[2] = 0.0;
	for (i = 0; i < Ns; i++) {
		c_ave[0] += r[i];	
		c_ave[1] += g[i];	
		c_ave[2] += b[i];	
	}

	pcc = 0.0;
	for (i = 0; i < 3; i++) {
		pcc += (w[i] * Cc[i] * c_ave[i]);
	}
	pcc /= weight;

	return Cc;
}

/*
 * Function: pixel_geometric_entropy
 *
 *     Evaluates the equation 9 in the paper.
 *
 * Returns:
 *
 *     H^{g}
 */
static double
pixel_geometric_entropy(double *cosT, double *d, int Ns)
{
	int i;
	double p_denom;
	double p;
	double Hg;

	p_denom = 0.0;
	for (i = 0; i < Ns; i++) {
		p_denom += cosT[i] / (d[i] * d[i]);		
	}

	assert(p_denom > 0.0);

	
	Hg = 0.0
	for (i = 0; i < Ns; i++) {
		p = (cosT[i] / (d[i] * d[i])) / p_denom;

		Hg += -(p * log(p));
	}

	return Hg;
}

/*
 * Function: pixel_geometric_contrast
 *
 *     Evaluates the equation 10 in the paper.
 *
 * Returns:
 *
 *     C^{g}
 */
static double
pixel_geometric_contrast(double Hg, int Ns)
{
	double Cg;

	Cg = 1.0 - Hg / log(Ns);

	return Cg;
}

