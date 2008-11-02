#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "IBLSampler.h"

/*
 * - Divide hemisphere into 16 sub-regions(4 sub-regions for each octant).
 * - Trace beam from the sub-region.
 * - calc visibility map and IBL map.
 * - calc final contribution from IBL with visibiity.
 */

#define RESOLUTION  (32)
#define SDIV        (2)

void
sample_ibl(
    const ri_texture_t              *iblmap,
    const ri_intersection_state_t   *isect)
{

    int i;
    int o;
    int sx, sy;
    
    double theta_step = (0.5 * M_PI) / SDIV;
    double phi_step   = (2.0 * M_PI) / (4.0 * SDIV);

    double theta[4], phi[4];                // left, top, right, bottom

    for (o = 0; o < 4; o++) {               // octants(upper hemisphere)

        for (sy = 0; sy < SDIV; sy++) {        // sub-regions
            for (sx = 0; sx < SDIV; sx++) {

                theta[0] = (sy + 0) * theta_step;
                theta[1] = (sy + 0) * theta_step;
                theta[2] = (sy + 1) * theta_step;
                theta[3] = (sy + 1) * theta_step;

                phi[0]   = o * (M_PI * 0.5) + (sx + 0) * phi_step;
                phi[1]   = o * (M_PI * 0.5) + (sx + 1) * phi_step;
                phi[2]   = o * (M_PI * 0.5) + (sx + 0) * phi_step;
                phi[3]   = o * (M_PI * 0.5) + (sx + 1) * phi_step;

                for (i = 0; i < 4; i++) {
                    printf("[%d] = (%f, %f)\n", i, theta[i], phi[i]);
                }
            }
        }

    }
}
