#include <stdio.h>
#include <stdlib.h>

#include "sis.h"
#include "hochbaum.h"

/* connected components data structure. */
typedef struct _cc_t
{
	pixelinfo_t  *pixels;		/* array of pixels */
	int           npixels;

	pixelinfo_t  *assignedsamples;	/* array of pixels assigned to 
					 * sample point(includes parent). */

	int           nassignedsamples;	/* number of assigned points 
					 * excluding assigned points which 
					 * are in parent region
					 */
	int           ntotalsamples;	/* number of samples to assign
					 * (parent + this level). */

	int          *parents;		/* list of index to connected 
					 * components in +1 level up layer 
					 * which are included this connected
					 * component in current level.  */
	int           nparents;
} cc_t; 

typedef struct _level_t
{
	int   level;
	cc_t *ccs;			/* list of connected components. */
	int   nccs;
} level_t;

static void gather_pixels_in_ccs(level_t *out, pixelinfo_t *src, int width, int height);

static void assign_samples(level_t *level, level_t *parentlevel, int nsamples,
			   double totalimportance, int npixels,
			   int *assignedsamples);

static void find_parent_cc(level_t *level, pixelinfo_t *parentlayer, int width, int height);

static void add_to_unique_list(int *list, int *nelems, int val);
static void gather_parentsamples(pixelinfo_t *out, cc_t *cc,
				 level_t *parentlevel, int maxsize,
				 int *ntotal);
static void gather_samples(pixelinfo_t *out, level_t *level, int maxsize);

void
generate_sample(pixelinfo_t *samples, int nsamples, double totalimportance,
		pixelinfo_t **layers, int nlayers, int width, int height)
{
	int i;
	int assignedsamples, totalassignedsamples;
	level_t *levels;

	//samples = (pixelinfo_t *)malloc(sizeof(pixelinfo_t) * nsamples);
	levels = (level_t *)malloc(sizeof(level_t) * nlayers);

	/* first, build pixel list in each connected components C_{j} 
	 * for each layer.
	 */
	for (i = 0; i < nlayers; i++){
		printf("--- layer %d: gather pixels ---\n", i); 
		levels[i].level = i;
		gather_pixels_in_ccs(&(levels[i]), layers[i], width, height);
		printf("----------------\n"); 
	}

	/* build connected component hierarchy. */
	for (i = nlayers - 2; i >= 0; i--){
		printf("--- layer %d: find parent ---\n", i); 
		find_parent_cc(&(levels[i]), layers[i + 1], width, height);
		printf("----------------\n"); 
	}

	assignedsamples = 0;
	totalassignedsamples = 0;
	printf("--- assign samples stage ---\n"); 

	
	printf("--- level %d: assign samples ---\n", nlayers - 1); 
	assign_samples(&(levels[nlayers - 1]), NULL, nsamples, totalimportance,
		       width * height, &assignedsamples);
	totalassignedsamples += assignedsamples;
		
	for (i = nlayers - 2; i >= 0; i--){
		printf("--- level %d: assign samples ---\n", i); 
		assign_samples(&(levels[i]), &(levels[i + 1]),
			       nsamples, totalimportance, width * height,
			       &assignedsamples);
		totalassignedsamples += assignedsamples;
		printf("----------------\n"); 
		printf("number of assigned samples = %d\n", assignedsamples);
	}	
	
	printf("number of total assigned samples = %d\n", totalassignedsamples);

	gather_samples(samples, &(levels[0]), nsamples);
}

void
gather_pixels_in_ccs(level_t *out, pixelinfo_t *src, int width, int height)
{
	int i, j;
	int nccs;	/* number of connected components in src. */
	int npixels;

	/* count number of connected components in src. */
	nccs = 0;
	for (i = 0; i < width * height; i++) {
		if (!src[i].valid) continue;

		if (nccs < src[i].label) nccs = src[i].label;
	}

	printf("n ccs = %d\n", nccs);

	out->ccs = (cc_t *)malloc(sizeof(cc_t) * nccs);
	if (!out->ccs) {
		printf("muda muda muda. out->ccs == NULL\n");
		exit(-1);
	}

	out->nccs = nccs;

	for (j = 0; j < nccs; j++) {
		out->ccs[j].parents = NULL;
		out->ccs[j].nparents = 0;
	
		/* count number of pixels in connected component C_{j}. */
		npixels = 0;
		for (i = 0; i < width * height; i++) {
			if (!src[i].valid) continue;

			/* label starts from 1. */
			if (src[i].label == (j + 1)) npixels++;
		}

		printf("C[%d]: npixels = %d\n", j, npixels);
		if (npixels == 0) {
			printf("??? 0 pixel C[%d]\n", j);
		}

		out->ccs[j].pixels =
			(pixelinfo_t *)malloc(sizeof(pixelinfo_t) * npixels);
		out->ccs[j].npixels = npixels;

		/* gather pixels in connected component C_{j}. */
		npixels = 0;
		for (i = 0; i < width * height; i++) {
			if (!src[i].valid) continue;

			/* label starts from 1. */
			if (src[i].label == (j + 1)) {
				memcpy(&(out->ccs[j].pixels[npixels]),
				       &(src[i]),
					sizeof(pixelinfo_t));
				npixels++;
			}
		}

		if (out->ccs[j].npixels != npixels) {
			printf("??? out = %d, npixels = %d\n", out->ccs[j].npixels, npixels);
		}

	}
}

void
find_parent_cc(level_t *level, pixelinfo_t *parentlayer, int width, int height)
{
	int  i;
	int  j;
	int  x, y;
	int  id;
	int  nparents;
	int  count;
	int *parentlist = NULL;

	/* for each connected component, find connnected components
	 * in parent(+1 level up) layer which are included in currnt level.
	 */ 
	printf("  nccs = %d\n", level->nccs);

	for (j = 0; j < level->nccs; j++) {
		nparents = 0; 
		printf("npixels = %d\n", level->ccs[j].npixels);
		parentlist = (int *)malloc(sizeof(int) * level->ccs[j].npixels);
		if (!parentlist) {
			printf("muda muda muda. parentlist=NULL\n");
			exit(-1);
		}

		count = 0;
		for (i = 0; i < level->ccs[j].npixels; i++) {
			x = level->ccs[j].pixels[i].x;
			y = level->ccs[j].pixels[i].y;


			/* If pixel position (x, y) in the parent layer is
			 * valid for the parent layer(i.e. the pixel is part
			 * of connected component), that pixel in the parent
			 * layer must be included in the current connected
			 * component which contains pixel positon (x, y).
			 */
 
			if (parentlayer[y * width + x].valid) {
				id = parentlayer[y * width + x].label;
				add_to_unique_list(parentlist, &nparents, id);
				count++;

#if 0
				if (level->nccs == 1) {
					printf("pixel[%d](x,y) = (%d, %d)\n", i, x, y);
				}

				if (level->nccs == 1) {
					printf("id = %d\n", id);
				}
#endif


			}			
		}

		level->ccs[j].parents = (int *)malloc(sizeof(int) * nparents);
		memcpy(level->ccs[j].parents,
		       parentlist,
		       sizeof(int) * nparents);
		level->ccs[j].nparents = nparents;

		printf("C[%d]: valid count = %d\n", j, count);
		printf("C[%d]: nparents = %d\n", j, nparents);

		free(parentlist);
		parentlist = NULL;
	}
}

#if 0
	double sumintensity, sumdomega;

		/* calculate importance of C_{j} and number of samples
		 * assigned to C_{j}
		 */
		sumintensity = 0.0;
		sumdomega    = 0.0;
		for (i = 0; i < out->ccs[j].npixels; i++) {
			sumintensity += out->ccs[j].pixels[i].intensity;
			sumdomegao   += out->ccs[j].pixels[i].domega;
		}

		/* N_{j} = N * (Gammma_{j} / Gamma_{4 Pi}) */
#endif

void
add_to_unique_list(int *list, int *nelems, int val)
{
	int i;
	int unique = 1;

	for (i = 0; i < (*nelems); i++) {
		if (list[i] == val) {
			unique = 0;
			break;
		}
	}

	if (unique) {
		list[(*nelems)] = val;
		(*nelems)++;
	}
}

void
assign_samples(level_t *level, level_t *parentlevel, int nsamples,
	       double totalimportance, int npixels, int *assignedsamples)
{
	int i, j;
	int nassignsamples;
	int totalsamples;
	int nassigned;
	int nnewassign;
	double importance;
	double sumintensity, sumdomega;
	double weight;
	level_t *curr;

	for (j = 0; j < level->nccs; j++) {
		/* calculate importance metric of C_{j}. */

		level->ccs[j].ntotalsamples = 0;
		level->ccs[j].nassignedsamples = 0;
		level->ccs[j].assignedsamples = NULL;
			
		sumintensity = 0.0;
		sumdomega = 0.0;
		for (i = 0; i < level->ccs[j].npixels; i++) {
			sumintensity += level->ccs[j].pixels[i].intensity;
			sumdomega    += level->ccs[j].pixels[i].domega;
		}

		importance = impfunc(sumintensity, sumdomega);
		nassignsamples = (int)((double)nsamples *
				       (importance / totalimportance));

		printf("sumintensity = %g, 4pi sumdomega = %g\n", sumintensity, sumdomega);
		printf("importance = %g, 4pi importance = %g\n", importance,
								 totalimportance);

		if (nassignsamples > nsamples) {
			printf("??? nassignsamples = %d, nsamples = %d\n", 
				nassignsamples, nsamples);
			nassignsamples = nsamples;
		}

		printf("nassignsamples = %d\n", nassignsamples);

		if (nassignsamples < 1) {
			printf("C[%d]: nassignsamples = 0", j);
			continue;
		}

		level->ccs[j].assignedsamples = 
			(pixelinfo_t *)malloc(sizeof(pixelinfo_t) *
					      nassignsamples);
		level->ccs[j].nassignedsamples = nassignsamples;
	
		nassigned = 0;

		if (parentlevel != NULL) {

			/* fill assigned points from parent. */
			gather_parentsamples(level->ccs[j].assignedsamples,
					     &(level->ccs[j]), 
					     parentlevel,
					     nassignsamples,
					     &nassigned);
		}

		/* assign new sample points. */
		nnewassign = nassignsamples - nassigned;

		if (nnewassign < 1) {
			printf("??? C[%d]: nnewassign = %d, nassignsamples= %d, nassigned = %d\n",
			       j, nnewassign, nassignsamples, nassigned);
			continue;
		}

		printf("nnewassgin = %d\n", nnewassign);
			
		hochbaum_symoys(level->ccs[j].assignedsamples,
				nassigned,
				level->ccs[j].pixels,
				level->ccs[j].npixels,
				nnewassign);

		/* weight pixel intensity by
		 * (nnewassign / nsamples) * (area / npixels)
		 * to be valid in Monte Carlo quadrature.
		 */
		weight  = (double)nnewassign / (double)nsamples;
		weight *= ((double)level->ccs[j].npixels / (double)npixels);

		for (i = 0; i < nnewassign; i++) {
			level->ccs[j].assignedsamples[nassigned + i].c[0] *=
				weight;
			level->ccs[j].assignedsamples[nassigned + i].c[1] *=
				weight;
			level->ccs[j].assignedsamples[nassigned + i].c[2] *=
				weight;
		}
	}


}

/* gather assigned sample points from parent's connected components. */
void
gather_parentsamples(pixelinfo_t *out, cc_t *cc,
		     level_t *parentlevel, int maxsize, int *ntotal)
{
	int i, j;
	int index;
	int parentcc;
	int nsamples;

	index = 0;
	for (i = 0; i < cc->nparents; i++) {
		/* connected components ID starts from 1. */
		parentcc = cc->parents[i];
		parentcc--;

		nsamples = parentlevel->ccs[parentcc].nassignedsamples;

		if (nsamples > 0) {
			if (index + nsamples > maxsize - 1) {
				/* this will not occur... */
				nsamples = maxsize - index - 1;
			}

			memcpy(&(out[index]),
			       parentlevel->ccs[parentcc].assignedsamples,
			       sizeof(pixelinfo_t) * nsamples);

			index += nsamples;

			if (index > maxsize - 1) break;
		}
	}

	(*ntotal) = index;

}

void
gather_samples(pixelinfo_t *out, level_t *level, int maxsize)
{
	int i, j;
	int index;
	int parentcc;
	int nsamples;

	index = 0;
	for (j = 0; j < level->nccs; j++) {
		nsamples = level->ccs[j].nassignedsamples;
		if (nsamples < 1) continue;

		if (index + nsamples > maxsize){
			/* this branch will not occur... */
			nsamples = maxsize - index;
		}

		memcpy(&(out[index]),
		       level->ccs[j].assignedsamples,
		       sizeof(pixelinfo_t) * nsamples);

		index += nsamples;

		if (index > maxsize - 1) break;
	}
}
