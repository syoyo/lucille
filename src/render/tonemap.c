#include "tonemap.h"

static float
exposure( float val, float gain, float gamma )
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
ri_tonemap_apply( const ri_display_t *disp, float result[3] )
{
    float      val;

    val = exposure( result[0], disp->gain, disp->gamma );
    if ( val < 0.0 ) val = 0.0;
    if ( val > 1.0 ) val = 1.0;
    result[0] = ( RtFloat ) val;

    val = exposure( result[1], disp->gain, disp->gamma );
    if ( val < 0.0 ) val = 0.0;
    if ( val > 1.0 ) val = 1.0;
    result[1] = ( RtFloat ) val;

    val = exposure( result[2], disp->gain, disp->gamma );
    if ( val < 0.0 ) val = 0.0;
    if ( val > 1.0 ) val = 1.0;
    result[2] = ( RtFloat ) val;
}

