#ifndef SPECTRUM_H
#define SPECTRUM_H

#ifdef __cplusplus
extern "C" {
#endif

/* The values are fixed for now */
#define SPECTRUM_START    380
#define SPECTRUM_END      780
#define SPECTRUM_INTERVAL  10

typedef struct _ri_spectrum_t
{
	int    start_wavelength;
	int    end_wavelength;
	int    nsamples;

	float *samples;

} ri_spectrum_t;

extern ri_spectrum_t *ri_spectrum_new();
extern void        ri_spectrum_free(ri_spectrum_t *s);
extern float       ri_spectrum_get_value(const ri_spectrum_t *s, int lambda);
extern void        ri_spectrum_set_value(ri_spectrum_t *s, int lambda, float value);
extern void        ri_spectrum_regular_curve(ri_spectrum_t *s, float *samples,
					  int start_wavelength,
					  int end_wavelength,
					  int nsamples);
// Fill with all spectrum with zero.
extern void        ri_spectrum_zero(ri_spectrum_t *s);

#if 0	/* not yet implemented */
/* Generate spectum from irregulary sampled data.
 * Assume that values in wavelengths array are sorted in advance.
 */
extern void        ri_spectrum_irregular_curve(ri_spectrum_t *s,
					    float *amplitudes,
					    float *wavelengths,
					    int nsamples);
#endif

#ifdef __cplusplus
}	/* extern "C" */
#endif


#endif

