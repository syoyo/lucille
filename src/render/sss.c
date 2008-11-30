/*
 *   lucille | Global Illumination renderer
 *
 *             written by Syoyo Fujita.
 *
 */

/*
 * Sub-surface scattering effect based on dipole approximation.
 * 
 * References:
 *
 *  - Henrik Wann Jensen, Stephen R. Marschner, Marc Levoy and Pat Hanrahan,
 *    A Practical Model for Subsurface Light Transport,
 *    SIGGRAPH 2001.
 *
 *  - Henrik Wann Jensen and Juan Buhler,
 *    A Rapid Hierarchial Rendering Technique for Translucent Materials,
 *    SIGGRAPH 2002.
 *    
 *  - Christpher Hey,
 *    Implementing a skin BSSRDF(or several...)
 *    ILM 2005.
 *    (Implementation note in RenderMan shader)
 *
 */

#include "intersection_state.h"
#include "reflection.h"
#include "random.h"
#include "sss.h"

/* ----------------------------------------------------------------------------
 *
 * Private function definitions
 *
 * ------------------------------------------------------------------------- */

static int
single_scattering_contribution(
    ri_sss_t                *sss,
    ri_intersection_state_t *isect);

/* ----------------------------------------------------------------------------
 *
 * Private functions
 *
 * ------------------------------------------------------------------------- */

/* ----------------------------------------------------------------------------
 *
 * Public functions
 *
 * ------------------------------------------------------------------------- */

int
ri_sss_sample(
    ri_sss_t *sss,
    ri_intersection_state_t *isect)
{
    (void)sss;
    (void)isect;

    /*
     *
     * S(x_i, w_i, x_o, w_o) = Sd(x_i, w_i, x_o, w_o) + S1(x_i, w_i, x_o, w_o)
     *
     * where
     *
     *   Sd  = Multiple scattering term
     *   S1  = Single scattering term
     *   w_i = Incident light direction
     *   x_i = The position where incident light enteres
     *   w_o = Outgoing light direction
     *   x_o = The position where outgoing light exits
     */

    return 0;
}

/*
 * Calculate contribution from single scattering.
 */
int
ri_sss_single_scattering(
    ri_sss_t                *sss,
    ri_intersection_state_t *isect)
{
    (void)sss;

    /*
     *
     * Computing refractive direction T_o is easy, but computing refractive
     * direction T_i(s -> x_i) is difficult.
     * 
     * Assume L(light) is placed at distant and the ray towards L does not 
     * reflect(s' -> L)
     *
     * E                                L
     * -_ w_o   n_o          n_i     _-
     *   -_     ^            ^     _- 
     *     -_   |x_o      x_i|   _-  w_i
     *       -_ |            | _-
     * ---------o------------o---------------------------
     *           \         _-/
     *        To  \      _- /
     *             \   _-  / Ti
     *              o_-   /
     *           s'  \   /
     *                \ /
     *                 o s
     *
     *  s' = x_o + To * s'_o
     *
     *  where
     *
     *           -log([0, 1))
     *    s'_o = --------------
     *               sigma_t
     */

    vec     To;
    vec     s_o;
    vec     P;
    vec     I;
    vec     N;
    double  sigma_t;
    double  s_dist;
    double  eta;

    //vcpy(I, isect->I);
    vcpy(N, isect->Ns);
    eta = 1.4;  // FIXME

    /*
     * 1. Compute refraction vector To
     */
    ri_refract(To, I, N, 1.0 / eta);
    vnormalize(To);
 

    /*
     * 2. Compute scattering point s_o
     */
    s_dist = -log(randomMT()) / sigma_t;

    s_o[0] = P[0] + s_dist * To[0];
    s_o[1] = P[1] + s_dist * To[1];
    s_o[2] = P[2] + s_dist * To[2];

    /*
     * 3. Shoot the ray towards the light.
     */

    return 0;
}

#if 0


    double Fdr;
    double eta;

    /*         1.440   0.710
     * Fdr = - ----- + ----- + 0.668 + 0.0636 eta
     *         eta^2    eta  
     */
    Fdr = -1.440 / (eta * eta) + 0.710 / eta + 0.668 + 0.0636 * eta;

#endif
    
