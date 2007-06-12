#ifndef SUNSKY_H
#define SUNSKY_H

#ifdef __cplusplus
extern "C" {
#endif

#define RI_IN
#define RI_OUT
#define RI_INOUT

#include "spectrum.h"

/*
 * Skylight algorithm from
 *
 * A.J. Preethan, Peter Shirley and Brian Smits,
 * "A Practical Analytic Model for Daylight"
 * SIGGRAPH 1999, pp 91-100.
 *
 */

typedef struct _ri_sunsky_t
{

	float sun_theta, sun_phi;	/* Solar position */
	float sun_dir[3];		/* Solar position in Cartesian coord. */
	float turbidity;

	float V;

	// Sun's spectrum radiance.
	ri_spectrum_t *sun_spectrum;

	// Sun's RGB color
	float       sun_rgb[3];

	//
	// Some coefficients...
	//

	float perez_x[5];
	float perez_y[5];
	float perez_Y[5];

	float zenith_x;
	float zenith_y;
	float zenith_Y;

} ri_sunsky_t;

// Allocate memory for a sunsky structure.
ri_sunsky_t *ri_sunsky_new();

// Initialize the sunsky.
extern void
ri_sunsky_init(
	RI_OUT ri_sunsky_t *sunsky,
	RI_IN float latitude, float longitude, /* site potion (in degree) */
	RI_IN float sm,                        /* standard meridian(in degree) */
	RI_IN int jd,                          /* julian day(1-365) */
	RI_IN float tod,                       /* Time of day(in hours) */
	RI_IN float turb,                      /* turbidity */
	RI_IN int effects);

// Returns the spectral radiance of the sky in the direction `v'.
extern void ri_sunsky_get_sky_spectrum(
	RI_OUT ri_spectrum_t *sky_spectrum,
	RI_IN  const ri_sunsky_t *sunsky,
	RI_IN  const float v[3]);

// Returns the RGB of the sky in the direction v.
extern void ri_sunsky_get_sky_rgb(
	RI_OUT float rgb[3],
	RI_IN  const ri_sunsky_t *sunsky,
	RI_IN  const float v[3]);

// Returns the RGB of the sun light in given time and location.
extern void ri_sunsky_get_sunlight_rgb(
	RI_OUT float rgb[3],
	RI_IN  const ri_sunsky_t *sunsky);

#ifdef __cplusplus
}	/* extern "C" */
#endif


#endif
