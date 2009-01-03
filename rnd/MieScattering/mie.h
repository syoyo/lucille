/*
 *
 *                   lucille | Global Illumination Renderer
 *
 *         Copyright 2003-2203 Syoyo Fujita (syoyo@lucillerender.org)
 *
 *
 */

/*
 * Scattering based on Lorenz-Mie theory
 */

#ifndef LUCILLE_MIE_H
#define LUCILLE_MIE_H

#ifdef __cplusplus
extern "C" {
#endif

#define MIE_THETA_RESOLUTION 1024

typedef struct _ri_mie_phase_function_t
{
    double phase[MIE_THETA_RESOLUTION];         /* p(theta)                 */

    double wavelength;                          /* [nm]                     */
    double particle_size;                       /* [nm]                     */

} ri_mie_phase_function_t;

/* Compute phase function for a given wavelength and particle size.  */ 
extern void ri_mie_compute_phase_function_milk(
    ri_mie_phase_function_t *phase,
    double                   wavelength,
    double                   particle_size,
    double                   fat_content);      /* [wt.-%] [0, 10]          */

#ifdef __cplusplus
}
#endif

#endif  /* LUCILLE_MIE_H */
