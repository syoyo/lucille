#ifndef SAMPLER_H
#define SAMPLER_H

#include "sis.h"

extern void generate_samples(pixelinfo_t *samples, int nsamples,
		             double totalimportance,
			     pixelinfo_t **layers, int nlayers
			     int width, int height);
	
#endif
