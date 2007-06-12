#include <math.h>

#include "vector.h"

static void
map_unitdisk( ri_float_t p[2] )
{
        const ri_float_t    pi_4 = 3.1415926535 / 4.0;
        ri_float_t          phi, r;
        ri_float_t          a = 2.0 * p[0] - 1.0;
        ri_float_t          b = 2.0 * p[1] - 1.0;

        if ( a > -b ) {
                if ( a > b ) {
                        r = a;
                        phi = pi_4 * ( b / a );
                } else {
                        r = b;
                        phi = pi_4 * ( 2 - a / b );
                }
        } else {
                if ( a < b ) {
                        r = -a;
                        phi = pi_4 * ( 4 + b / a );
                } else {
                        r = -b;
                        if ( b != 0.0 ) {
                                phi = pi_4 * ( 6 - a / b );
                        } else {
                                phi = 0.0;
                        }
                }
        }

        p[0] = r * cos( phi );
        p[1] = r * sin( phi );
}
