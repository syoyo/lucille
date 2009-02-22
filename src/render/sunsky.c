
/*
   File: sunsky.c
    Implements sky simulation.

*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "specrend.h"
#include "spectrum.h"
#include "sunsky.h"
#include "memory.h"

#include "sunsky.dat"

//#define LOCAL_DEBUG

#define deg2rad(x) ((x) * M_PI / 180.0)

float
angle_between( float v_theta, float v_phi, float s_theta, float s_phi )
{
    float           cospi;

    cospi = sin( v_theta ) * sin( s_theta ) * cos( s_phi - v_phi )
        + cos( v_theta ) * cos( s_theta );

    if ( cospi > 1.0 )
            return 0.0;
    if ( cospi < -1.0 )
            return M_PI;

    return acos( cospi );
}

void
init_sun_theta_phi( float *sun_theta, float *sun_phi,   /* output */
                    float latitude, float longitude,
                    float time_of_day,
                    int julian_day, float standard_meridian )
{
    float           solar_time;
    float           solar_declination;
    float           solar_altitude;
    float           solar_azimuth;
    float           opp, adj;

    solar_time = time_of_day
        + ( 0.170 * sin( 4.0 * M_PI * ( julian_day - 80 ) / 373 )
            - 0.129 * sin( 2.0 * M_PI * ( julian_day - 8 ) / 355 ) )
        + 12.0 * ( deg2rad( standard_meridian ) -
                   deg2rad( longitude ) ) / M_PI;

    solar_declination =
        ( 0.4093 * sin( 2.0 * M_PI * ( julian_day - 81 ) / 368 ) );

    solar_altitude =
        asin( sin( deg2rad( latitude ) ) * sin( solar_declination ) -
              cos( deg2rad( latitude ) ) * cos( solar_declination ) *
              cos( M_PI * solar_time / 12 ) );


    opp = -cos( solar_declination ) * sin( M_PI * solar_time / 12 );
    adj = -( cos( deg2rad( latitude ) ) * sin( solar_declination ) +
             sin( deg2rad( latitude ) ) * cos( solar_declination ) *
             cos( M_PI * solar_time / 12 ) );

    solar_azimuth = atan2( opp, adj );

    ( *sun_phi ) = -solar_azimuth;
    ( *sun_theta ) = M_PI / 2.0 - solar_altitude;
}

void
compute_attenuated_sunlight( ri_spectrum_t * sun_spectrum, float theta,
                             float turbidity )
{
    int             i;

    const float     alpha = 1.3;
    const float     lozone = 0.35;
    const float     w = 2.0;

    float           beta;
    float           tauR, tauA, tauO, tauG, tauWA;
    float           m;
    float           lambda;
    float           amp;

    //float data[41];       // (780 - 380) / 10 + 1

    beta = 0.04608365822050 * turbidity - 0.04586025928522;

    // Relative optical mass
    m = 1.0 / ( cos( theta ) +
                0.15 * pow( 93.885 - theta / M_PI * 180.0, -1.253 ) );
#ifdef LOCAL_DEBUG
    printf( "cos(theta) = %f\n", cos( theta ) );
    printf( "pow = %f\n",
                pow( 93.885 - theta / M_PI * 180.0, -1.253 ) );
    printf( "val = %f\n", 93.885 - theta / M_PI * 180.0 );
    printf( "M_PI = %f\n", M_PI );
    printf( "m = %f\n", m );
#endif

    for ( i = 0, lambda = 380; lambda < 781; i++, lambda += 10 ) {

            // Rayleigh scattering
            tauR = exp( -m * 0.008735 * pow( lambda / 1000, -4.08 ) );

            // Aerosol (water + dust) attenuation
            tauA = exp( -m * beta * pow( lambda / 1000, -alpha ) );

            // Attenuation due to ozone absorption
            tauO = exp( -m * k_oAmplitudes[i] * lozone );

            // Attenuation due to mixed gases absorption
            tauG =
                exp( -1.41 * k_gAmplitudes[i] * m /
                     pow( 1.0 + 118.93 * k_gAmplitudes[i] * m,
                          0.45 ) );

            // Attenuation due to water vapor absorption
            tauWA = exp( -0.2385 * k_waAmplitudes[i] * w * m /
                         pow( 1.0 + 20.07 * k_waAmplitudes[i] * w * m,
                              0.45 ) );

            // 100.0 comes from solAmplitudes begin in wrong units.
            amp =
                100.0 * solAmplitudes[i] * tauR * tauA * tauO * tauG *
                tauWA;
            ri_spectrum_set_value( sun_spectrum, ( int ) lambda, amp );
    }
}



float
PerezFunction( const float *lam, float theta, float gamma, float lvz,
               float sun_theta )
{
    float           den;
    float           num;

    den = ( ( 1.0 + lam[0] * exp( lam[1] ) ) *
            ( 1.0 + lam[2] * exp( lam[3] * sun_theta ) +
              lam[4] * cos( sun_theta ) * cos( sun_theta ) ) );

    num = ( ( 1.0 + lam[0] * exp( lam[1] / cos( theta ) ) ) *
            ( 1.0 + lam[2] * exp( lam[3] * gamma ) +
              lam[4] * cos( gamma ) * cos( gamma ) ) );

    return lvz * num / den;
}

/*
 * Function: ri_sunsky_new
 *
 *   Creates a sunsky object.
 *
 * Parameters:
 *
 *   None
 *
 * Returns:
 *
 *   The sunsky object.
 */
ri_sunsky_t    *
ri_sunsky_new(  )
{
        ri_sunsky_t    *s;

        s = ( ri_sunsky_t * ) ri_mem_alloc( sizeof( ri_sunsky_t ) );
        s->sun_spectrum = ri_spectrum_new(  );

        return s;
}

void
ri_sunsky_init( ri_sunsky_t * sunsky, float latitude, float longitude,  /* site potion (in degree) */
                float sm,       /* standard meridian(in degree) */
                int jd,         /* julian day(1-365) */
                float tod,      /* Time of day(in hours) */
                float turb,     /* turbidity */
                int effects )
{
    float           sun_theta, sun_phi;
    float           theta2, theta3;
    float           T, T2;
    float           chi;
    float           zenith_Y;
    float           zenith_x, zenith_y;
    float           perez_Y[5];
    float           perez_x[5], perez_y[5];
    float           x, y, z, r, g, b;

    ( void ) effects;

    struct colourSystem *cs = &CIEsystem;

    sunsky->V = 4.0;        // Junge's exponent;

    sm = sm * 15.0;         // = standard meridian = timezone * 15

    init_sun_theta_phi( &sun_theta, &sun_phi,
                        latitude, longitude, tod, jd, sm );

    sunsky->sun_theta = sun_theta;
    sunsky->sun_phi = sun_phi;

	printf("theta = %f, phi = %f\n", sun_theta, sun_phi);

    sunsky->sun_dir[0] = cos( sun_phi ) * sin( sun_theta );
    sunsky->sun_dir[1] = sin( sun_phi ) * sin( sun_theta );
    sunsky->sun_dir[2] = cos( sun_theta );

    //
    // Compute Sun's spectral color.
    //

    if ( sun_theta < 0.5 * M_PI ) { // avobe the horizon
            compute_attenuated_sunlight( sunsky->sun_spectrum,
                                         sun_theta, turb );
    } else {                // under the horizon
            ri_spectrum_zero( sunsky->sun_spectrum );
    }

    spectrum_to_xyz( sunsky->sun_spectrum->samples, &x, &y, &z );
    xyz_to_rgb( cs, x, y, z, &r, &g, &b );

    sunsky->sun_rgb[0] = r;
    sunsky->sun_rgb[1] = g;
    sunsky->sun_rgb[2] = b;

#ifdef LOCAL_DEBUG
    printf( "xyz = %f, %f, %f, rgb = %f, %f, %f\n", x, y, z, r, g, b );
#endif

    theta2 = sun_theta * sun_theta;
    theta3 = theta2 * sun_theta;

    T = turb;
    T2 = turb * turb;

    chi = ( 4.0 / 9.0 - T / 120.0 ) * ( M_PI - 2.0 * sun_theta );
    zenith_Y =
        ( 4.0453 * T - 4.9710 ) * tan( chi ) - 0.2155 * T + 2.4192;
    zenith_Y *= 1000;       /* conversion from kcd/m^2 to cd/m^2 */

    zenith_x =
        ( 0.00165 * theta3 - 0.00374 * theta2 + 0.00208 * sun_theta +
          0 ) * T2 + ( -0.02902 * theta3 + 0.06377 * theta2 -
                       0.03202 * sun_theta + 0.00394 ) * T +
        ( 0.11693 * theta3 - 0.21196 * theta2 + 0.06052 * sun_theta +
          0.25885 );

    zenith_y =
        ( 0.00275 * theta3 - 0.00610 * theta2 + 0.00316 * sun_theta +
          0 ) * T2 + ( -0.04212 * theta3 + 0.08970 * theta2 -
                       0.04153 * sun_theta + 0.00515 ) * T +
        ( 0.15346 * theta3 - 0.26756 * theta2 + 0.06669 * sun_theta +
          0.26688 );

    perez_Y[0] = 0.17872 * T - 1.46303;
    perez_Y[1] = -0.35540 * T + 0.42749;
    perez_Y[2] = -0.02266 * T + 5.32505;
    perez_Y[3] = 0.12064 * T - 2.57705;
    perez_Y[4] = -0.06696 * T + 0.37027;

    perez_x[0] = -0.01925 * T - 0.25922;
    perez_x[1] = -0.06651 * T + 0.00081;
    perez_x[2] = -0.00041 * T + 0.21247;
    perez_x[3] = -0.06409 * T - 0.89887;
    perez_x[4] = -0.00325 * T + 0.04517;

    perez_y[0] = -0.01669 * T - 0.26078;
    perez_y[1] = -0.09495 * T + 0.00921;
    perez_y[2] = -0.00792 * T + 0.21023;
    perez_y[3] = -0.04405 * T - 1.65369;
    perez_y[4] = -0.01092 * T + 0.05291;

    sunsky->zenith_x = zenith_x;
    sunsky->zenith_y = zenith_y;
    sunsky->zenith_Y = zenith_Y;

    sunsky->perez_Y[0] = perez_Y[0];
    sunsky->perez_Y[1] = perez_Y[1];
    sunsky->perez_Y[2] = perez_Y[2];
    sunsky->perez_Y[3] = perez_Y[3];
    sunsky->perez_Y[4] = perez_Y[4];

    sunsky->perez_x[0] = perez_x[0];
    sunsky->perez_x[1] = perez_x[1];
    sunsky->perez_x[2] = perez_x[2];
    sunsky->perez_x[3] = perez_x[3];
    sunsky->perez_x[4] = perez_x[4];

    sunsky->perez_y[0] = perez_y[0];
    sunsky->perez_y[1] = perez_y[1];
    sunsky->perez_y[2] = perez_y[2];
    sunsky->perez_y[3] = perez_y[3];
    sunsky->perez_y[4] = perez_y[4];
}

void
chromaticity_to_spectrum( ri_spectrum_t * s, float x, float y )
{
    int             i;
    float           M1, M2;

    M1 = ( -1.3515 - 1.7703 * x + 5.9114 * y )
        / ( 0.0241 + 0.2562 * x - 0.7341 * y );
    M2 = ( 0.03 - 31.4424 * x + 30.0717 * y )
        / ( 0.0241 + 0.2562 * x - 0.7341 * y );

    // Assume that spectrum range is 380nm - 780nm by 10nm
    for ( i = 0; i < s->nsamples; i++ ) {
            s->samples[i] =
                S0Amplitudes[i] + M1 * S1Amplitudes[i] +
                M2 * S2Amplitudes[i];
    }
}

// Returns the spectral radiance of the sky in the direction v.
void
ri_sunsky_get_sky_spectrum( ri_spectrum_t * sky_spectrum,
                            const ri_sunsky_t * sunsky, const float v[3] )
{
    int             i;

    float           theta, phi;
    float           vlen;
    float           gamma;
    float           x, y, Y;
    float           lx, ly, lz;
    float           t[3];

    //t[0] = v[0]; t[1] = v[1]; t[2] = v[2];
    t[0] = v[0];
    t[1] = v[2];
    t[2] = v[1];
    if ( t[2] < 0.0 ) {
            // Under the horizon
            ri_spectrum_zero( sky_spectrum );
            return;
            t[2] = 0.0005;
    }

    if ( t[2] < 0.001 ) {
            t[2] = 0.001;
            vlen = sqrt( t[0] * t[0] + t[1] * t[1] + t[2] * t[2] );
            t[0] /= vlen;
            t[1] /= vlen;
            t[2] /= vlen;
    }

    theta = acos( t[2] );
    if ( fabs( theta ) < 1.0e-6 ) {
            phi = 0.0;
    } else {
            phi = atan2( t[1], t[0] );
    }

    // The anble between viewing direction and Sun's direction
    gamma =
        angle_between( theta, phi, sunsky->sun_theta,
                       sunsky->sun_phi );

    x = PerezFunction( sunsky->perez_x, theta, gamma, sunsky->zenith_x,
                       sunsky->sun_theta );
    y = PerezFunction( sunsky->perez_y, theta, gamma, sunsky->zenith_y,
                       sunsky->sun_theta );
    Y = PerezFunction( sunsky->perez_Y, theta, gamma, sunsky->zenith_Y,
                       sunsky->sun_theta );

    chromaticity_to_spectrum( sky_spectrum, x, y );
    spectrum_to_xyz( sky_spectrum->samples, &lx, &ly, &lz );

    if ( fabs( ly ) < 1.0e-48 ) {
            ly = 1.0;
    }
    // 380nm - 780nm by 10nm (nsamples = 41);
    for ( i = 0; i < sky_spectrum->nsamples; i++ ) {
            sky_spectrum->samples[i] =
                Y * sky_spectrum->samples[i] / ly;
    }

}

// Returns the RGB of the sky in the direction v.
void
ri_sunsky_get_sky_rgb( float rgb[3], const ri_sunsky_t * sunsky,
                       const float v[3] )
{
    static int      first = 1;
    static ri_spectrum_t *s = NULL;

    float           x, y, z;
    struct colourSystem *cs = &CIEsystem;


    if ( first ) {
            s = ri_spectrum_new(  );
            first = 0;
    }

    ri_sunsky_get_sky_spectrum( s, sunsky, v );

    spectrum_to_xyz( s->samples, &x, &y, &z );
    xyz_to_rgb( cs, x, y, z, &rgb[0], &rgb[1], &rgb[2] );
}

void
ri_sunsky_get_sunlight_rgb(float rgb[3], const ri_sunsky_t *sunsky)
{
    static int      first = 1;
    static ri_spectrum_t *s = NULL;

    float           x, y, z;
    struct colourSystem *cs = &CIEsystem;

    if ( first ) {
            s = ri_spectrum_new(  );
            first = 0;
    }

    compute_attenuated_sunlight( s, sunsky->sun_theta, sunsky->turbidity );

    spectrum_to_xyz( s->samples, &x, &y, &z );
    xyz_to_rgb( cs, x, y, z, &rgb[0], &rgb[1], &rgb[2] );
}
