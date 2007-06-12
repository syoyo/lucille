#include "tonemap.h"

static ri_float_t
exposure( ri_float_t val, ri_float_t gain, ri_float_t gamma )
{
#ifdef DEBUG
        if ( gamma == 0.0 ) {
                ri_log( LOG_WARN, "gamma = 0.0" );
                return 0.0;
        }
#endif
        if ( gamma == 0.0 ) {
                return 0.0;
	}

        return pow( ( gain * val ), 1.0 / gamma );
}

void
ri_tonemap_apply( const ri_display_t *disp, ri_vector_t * result )
{
        ri_display_t   *disp;
        ri_float_t      val;

        val = exposure( result->f[0], disp->gain, disp->gamma );
        if ( val < 0.0 )
                val = 0.0;
        if ( val > 1.0 )
                val = 1.0;
        result->f[0] = ( RtFloat ) val;

        val = exposure( result->f[1], disp->gain, disp->gamma );
        if ( val < 0.0 )
                val = 0.0;
        if ( val > 1.0 )
                val = 1.0;
        result->f[1] = ( RtFloat ) val;

        val = exposure( result->f[2], disp->gain, disp->gamma );
        if ( val < 0.0 )
                val = 0.0;
        if ( val > 1.0 )
                val = 1.0;
        result->f[2] = ( RtFloat ) val;
}

