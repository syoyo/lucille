#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <assert.h>

#include "spectrum.h"
#include "memory.h"

static float
lerp(float a, float b, float t)
{
	return a * (1.0 - t) + b * t;
}

ri_spectrum_t *
ri_spectrum_new()
{
	int nsamples;
	ri_spectrum_t *s;

	nsamples = (SPECTRUM_END - SPECTRUM_START) / SPECTRUM_INTERVAL + 1;
	
	s = (ri_spectrum_t *)ri_mem_alloc(sizeof(ri_spectrum_t));
	memset(s, 0, sizeof(ri_spectrum_t));

	s->start_wavelength = SPECTRUM_START;
	s->end_wavelength   = SPECTRUM_END;
	s->nsamples         = nsamples;

	s->samples = (float *)malloc(sizeof(float) * nsamples);
	memset(s->samples, 0, sizeof(float) * nsamples);

	return s;
}

void
ri_spectrum_free(ri_spectrum_t *s)
{
	if (s->samples) {
		ri_mem_free(s->samples);
	}

	if (s) {
		ri_mem_free(s);
	} 
}

float
ri_spectrum_get_value(const ri_spectrum_t *s, int lambda)
{
	int l0, l1;
	float t;
	float t_frac;
	float  s0, s1;
	int    t_int;

	if (s->start_wavelength > lambda) {
		lambda = s->start_wavelength;
	}

	if (s->end_wavelength < lambda) {
		lambda = s->end_wavelength;
	}

	t = (lambda - s->start_wavelength) / (float)SPECTRUM_INTERVAL;

	t_int  = (int)floor(t);
	t_frac = t - (float)t_int; 

	l0 = t_int; l1 = t_int + 1;
	if (l0 > s->nsamples - 1) l0 = s->nsamples - 1;
	if (l1 > s->nsamples - 1) l1 = s->nsamples - 1;

	/* linear interpolation */
	s0 = s->samples[l0];
	s1 = s->samples[l1];

	return lerp(s0, s1, t_frac);
}

void
ri_spectrum_set_value(ri_spectrum_t *s, int lambda, float value)
{
	int l;

	if (s->start_wavelength > lambda) {
		lambda = s->start_wavelength;
	}

	if (s->end_wavelength < lambda) {
		lambda = s->end_wavelength;
	}

	l = (lambda - s->start_wavelength) / SPECTRUM_INTERVAL;
	if (l > s->nsamples - 1) l = s->nsamples - 1;

	s->samples[l] = value;
}

void
ri_spectrum_regular_curve(ri_spectrum_t *s,	/* assume s is already initialized */
		       float *samples,
		       int start_wavelength,
		       int end_wavelength,
		       int nsamples)
{
	int i;
	int step;

	step = (end_wavelength - start_wavelength) / nsamples;
	for (i = 0; i < nsamples; i++) { 
		ri_spectrum_set_value(s, start_wavelength + step * i, samples[i]);
	}
}

void
ri_spectrum_irregular_curve(ri_spectrum_t *s,	/* assume s is already initialized */
		         float *amplitudes,
		         float *wavelengths,
			 int nsamples)
{
	int i, j;
	int l, l0, l1;
	int step;
	//float amp_interpolated;
	float t_frac;

	assert(nsamples > 1);

	step = SPECTRUM_INTERVAL;

	l = ((wavelengths[0] - s->start_wavelength) / SPECTRUM_INTERVAL) * SPECTRUM_INTERVAL;
	if (l > s->start_wavelength) {

		for (i = s->start_wavelength; i < l; i += step) { 
			ri_spectrum_set_value(s, i, amplitudes[0]);
		}

	}

	for (i = 0; i < nsamples - 1; i++) {
		l0 = ((wavelengths[i] - s->start_wavelength) / SPECTRUM_INTERVAL) * SPECTRUM_INTERVAL;
		l1 = ((wavelengths[i + 1] - s->start_wavelength) / SPECTRUM_INTERVAL) * SPECTRUM_INTERVAL;

		for (j = l0; j < l1; j += step) {
			t_frac = (j - l0) / (float)(l1 - l0);

			ri_spectrum_set_value(s, j, lerp(amplitudes[i],
						      amplitudes[i + 1],
						      t_frac));

		}
	}

	l = ((s->end_wavelength - wavelengths[nsamples - 1]) / SPECTRUM_INTERVAL) * SPECTRUM_INTERVAL;

	if (l < s->end_wavelength) {
		for (i = l; i < s->end_wavelength; i += step) { 
			ri_spectrum_set_value(s, i, amplitudes[nsamples - 1]);
		}
	}
}

void
ri_spectrum_zero(ri_spectrum_t *s)
{
	int i;
		
	for (i = 0; i < s->nsamples; i++) {
		s->samples[i] = 0.0;
	}
}
