/*
 *   lucille | Global Illumination renderer
 *
 *             written by Syoyo Fujita.
 *
 */

/*
 * $Id: camera.c,v 1.3 2004/06/13 06:44:51 syoyo Exp $
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "memory.h"
#include "log.h"
#include "camera.h"
#include "render.h"
#include "random.h"

static int
rootdeg2(
    ri_float_t root[2],        /* [out] */
    ri_float_t a,
    ri_float_t b )
{
        /* solve x^2 + a x + b = 0 */
        ri_float_t          d;

        d = a * a - 4.0 * b;
        if ( d >= 0.0 ) {
                d = sqrt( d );
                root[0] = 0.5 * ( -a - d );
                root[1] = 0.5 * ( -a + d );
                return 2;
        } else {
                printf( "a = %f, b = %f\n", a, b );
                printf( "d = %f\n", d );
                return 0;
        }
}

static void
dof(
    ri_vector_t from,        /* [out] */
    ri_vector_t to,        /* [out] */
    ri_float_t  fstop,
    ri_float_t  flen,
    ri_float_t  fdist,
    ri_float_t  imgflen )
{
        int                 nroots;
        ri_float_t          t;
        ri_float_t          aperture;
        ri_float_t          dtheta, dr;
        ri_float_t          dx, dy;
        ri_float_t          lensdist;
        ri_float_t          root[2] = {0.0, 0.0};

        aperture = flen / ( 2.0 * fstop );

        t = ( fdist - from[2] ) / ( to[2] - from[2] );
        dtheta = randomMT(  ) * 2.0 * M_PI;
        dr = sqrt( -log( randomMT(  ) ) / 5.0 ) * aperture;

        to[0] = t * ( to[0] - from[0] ) + from[0];
        to[1] = t * ( to[1] - from[1] ) + from[1];
        to[2] = fdist;

        dx = cos( dtheta ) * dr;
        dy = sin( dtheta ) * dr;

        nroots =
            rootdeg2( root, -( fdist - imgflen ), ( fdist - imgflen ) * flen );
        if ( nroots == 0 ) {
                printf( "fdist = %f\n", fdist );
                printf( "imgflen = %f\n", imgflen );
                printf( "flen = %f\n", flen );
                //printf("???\n");
        }
        assert( nroots != 0 );

        lensdist = ( root[0] < root[1] ) ? root[0] : root[1];
        from[0] += 100.0 * dx;
        from[1] += 100.0 * dy;
        from[2] = lensdist;
}

ri_camera_t *
ri_camera_new()
{
    ri_matrix_t  mat;
    ri_camera_t *camera = NULL;

    camera = ( ri_camera_t * )ri_mem_alloc( sizeof( ri_camera_t ) );

    /*
     * Set default value.
     */
    camera->horizontal_resolution = 640;
    camera->vertical_resolution   = 480;
    camera->pixel_aspect_ratio    = 1.0f;

    camera->crop_window[0] = 0.0f;
    camera->crop_window[1] = 1.0f;
    camera->crop_window[2] = 0.0f;
    camera->crop_window[3] = 1.0f;

    camera->frame_aspect_ratio = 4.0f / 3.0f;

    camera->screen_window[0] = -camera->frame_aspect_ratio;
    camera->screen_window[1] =  camera->frame_aspect_ratio;
    camera->screen_window[2] = -1.0f;
    camera->screen_window[3] =  1.0f;

    camera->camera_projection = RI_ORTHOGRAPHIC;

    ri_matrix_identity( &mat );
    camera->world_to_camera   = mat;

    camera->nearclip          = ( float )RI_EPSILON;
    camera->farclip           = ( float )RI_INFINITY;

    camera->fstop             = ( float )RI_INFINITY;
    camera->focal_length      = 0.0f;
    camera->focal_distance    = 0.0f;

    camera->shutter_open      = 0.0f;
    camera->shutter_close     = 0.0f;

    camera->fov               = 90.0f;

    camera->use_glcamera      = 0;
    ri_vector_setzero( camera->cam_pos );
    ri_vector_setzero( camera->cam_at );
    ri_vector_setzero( camera->cam_up );

    return camera;
}

void
ri_camera_free(
    ri_camera_t *camera )
{
    ri_mem_free( camera );
}

void
ri_camera_get_glmatrix( ri_matrix_t * m, const ri_camera_t * camera )
{
        ri_matrix_t     tmp, orientation;
        ri_vector_t     dir, up, right;

        ri_vector_sub( dir, camera->cam_at, camera->cam_pos );
        ri_vector_cross( right, dir, camera->cam_up );
        ri_vector_cross( up, right, dir );

        ri_vector_normalize( dir );
        ri_vector_normalize( right );
        ri_vector_normalize( up );

        /* build orientation matrix */
        ri_matrix_identity( &orientation );
        orientation.f[2][2] = -orientation.f[2][2];

        ri_matrix_identity( &tmp );
        tmp.f[0][0] = right[0];
        tmp.f[0][1] = right[1];
        tmp.f[0][2] = right[2];
        tmp.f[0][3] = 0.0;
        tmp.f[1][0] = up[0];
        tmp.f[1][1] = up[1];
        tmp.f[1][2] = up[2];
        tmp.f[1][3] = 0.0;
        tmp.f[2][0] = dir[0];
        tmp.f[2][1] = dir[1];
        tmp.f[2][2] = dir[2];
        tmp.f[2][3] = 0.0;
        ri_matrix_mul( m, &tmp, &orientation );
        ri_matrix_translate( m,
                             camera->cam_pos[0],
                             camera->cam_pos[1],
                             camera->cam_pos[2] );
        ri_matrix_inverse( m );
}

void
ri_camera_setup(ri_camera_t *camera)
{
    ri_matrix_t orientation;
    ri_matrix_t m;

    camera->flength = 1.0 / tan( ( camera->fov * 3.141592 / 180.0 ) * 0.5 );

        /*
     * build orientation matrix
     */
        ri_matrix_identity( &orientation );
        if ( strcmp(ri_render_get()->context->option->orientation,
            RI_RH ) == 0 ) {
                orientation.f[2][2] = -orientation.f[2][2];
        }

        /*
     * compute camera to world matrix
     */
        if ( camera->use_glcamera ) {
                ri_camera_get_glmatrix( &m, camera );
        } else {
                ri_matrix_copy( &m,
                                &( ri_render_get(  )->context->
                                   world_to_camera ) );
                ri_matrix_inverse( &m );
        }
        ri_matrix_mul( &camera->camera_to_world, &m, &orientation );

}

void
ri_camera_get_pos_and_dir(
    ri_vector_t        pos,  /* [out] */
    ri_vector_t        dir,  /* [out] */
    const ri_camera_t *camera,
    ri_float_t         x,
    ri_float_t         y)
{
    ri_vector_t         v;
    ri_float_t          flength = (ri_float_t)0.0;
    int                 ortho;

    ri_float_t          w;
    ri_float_t          h;

    const ri_matrix_t  *c2w;

    w = camera->horizontal_resolution;
    h = camera->vertical_resolution;

    v[0] = ( 2.0f * x - w ) / w;
    v[1] = ( 2.0f * y - h ) / h;
    v[2] = flength;
    v[3] = 1.0;

    ortho = ( camera->camera_projection == RI_ORTHOGRAPHIC ) ? 1 : 0;
    c2w   = &camera->camera_to_world;

    /*
     * perturb ray for DOF effect.
     */
    if ( camera->focal_length > 0.0 ) {

        /* TODO: fix this */

        if ( ortho ) {  /* orthographic camera */

            pos[0] = v[0];
            pos[1] = v[1];
            pos[2] = 0.0;
            pos[3] = 1.0;

        } else {        /* perspective camera */

            ri_vector_setzero( pos );
            pos[3] = 1.0;

        }

        dof( pos, v,
             camera->fstop,
             camera->focal_length,
             camera->focal_distance, flength );

        ri_vector_transform( dir, v, c2w );
        ri_vector_copy( v, pos );
        ri_vector_transform( pos, v, c2w );

    } else {

        ri_vector_transform( dir, v, c2w );

        if ( ortho ) {  /* orthographic camera */

            v[0] = v[0];
            v[1] = v[1];
            v[2] = 0.0;
            v[3] = 1.0;

        } else {        /* perspective camera */

            ri_vector_setzero( v );
            v[3] = 1.0;

        }

        ri_vector_transform( pos, v, c2w );
    }
}

/* -----------------------------------------------------------------------------
 *
 * RenderMan Interface implemenataion
 *
 * -------------------------------------------------------------------------- */

void
ri_api_format(
    RtInt xres, RtInt yres, RtFloat aspect )
{
    if (xres < 0) {
        ri_log( LOG_WARN, "Format: xres < 0" );
        xres = 640;     /* set to default */
    }

    if (yres < 0) {
        ri_log( LOG_WARN, "Format: yres < 0" );
        xres = 480;     /* set to default */
    }

    if (aspect < 0) {
        ri_log( LOG_WARN, "Format: aspect < 0" );
        aspect = 1.0;   /* set to default */
    }

    ri_render_get()->context->option->camera->horizontal_resolution = xres;
    ri_render_get()->context->option->camera->vertical_resolution   = yres;
    ri_render_get()->context->option->camera->pixel_aspect_ratio  = aspect;
}

void
ri_api_frame_aspect_ratio(
    RtFloat aspect )
{
    ri_render_get()->context->option->camera->frame_aspect_ratio = aspect;
}

void
ri_api_screen_window(
    RtFloat left, RtFloat right, RtFloat bot, RtFloat top )
{
    ri_render_get()->context->option->camera->screen_window[0] = left;
    ri_render_get()->context->option->camera->screen_window[1] = right;
    ri_render_get()->context->option->camera->screen_window[2] = bot;
    ri_render_get()->context->option->camera->screen_window[3] = top;
}

void
ri_api_crop_window(
    RtFloat xmin, RtFloat xmax, RtFloat ymin, RtFloat ymax )
{
    ri_log( LOG_WARN, "RiCropWindo is not yet implemented" );

    ri_render_get()->context->option->camera->crop_window[0] = xmin;
    ri_render_get()->context->option->camera->crop_window[1] = xmax;
    ri_render_get()->context->option->camera->crop_window[2] = ymin;
    ri_render_get()->context->option->camera->crop_window[3] = ymax;
}

void
ri_api_projection(
    RtToken name, RtInt n, RtToken tokens[], RtPointer params[] )
{
    int          i;
    ri_camera_t *camera;

    ri_log_and_return_if( strcmp( name, RI_ORTHOGRAPHIC ) != 0 &&
                 strcmp( name, RI_PERSPECTIVE )  != 0 );

    camera = ri_render_get()->context->option->camera;

    for (i = 0; i < n; i++) {
        if (strcmp( tokens[i], "fov" ) == 0) {
            camera->fov = *( RtFloat * )params[i];
            camera->camera_projection = RI_PERSPECTIVE;
        }
    }
}

void
ri_api_clipping(
    RtFloat hither, RtFloat yon )
{
    ri_log( LOG_WARN, "RiClipping is not yet implemented" );

    ri_render_get()->context->option->camera->nearclip = hither;
    ri_render_get()->context->option->camera->farclip  = yon;

}

void
ri_api_clipping_plane(
    RtFloat x, RtFloat y, RtFloat z,
    RtFloat nx, RtFloat ny, RtFloat nz )
{
    ri_log( LOG_WARN, "RiClippingPlane is not yet implemented" );

    ( void )x;
    ( void )y;
    ( void )z;
    ( void )nx;
    ( void )ny;
    ( void )nz;
}

void
ri_api_depth_of_field(
    RtFloat fstop, RtFloat focallength,
    RtFloat focaldistance )
{
    ri_camera_t *camera = ri_render_get()->context->option->camera;

    camera->fstop          = fstop;
    camera->focal_length   = focallength;
    camera->focal_distance = focaldistance;
}

void
ri_api_shutter(
    RtFloat min, RtFloat max )
{
    ri_log( LOG_WARN, "RiShutter is not yet implemented" );

    ri_render_get()->context->option->camera->shutter_open  = min;
    ri_render_get()->context->option->camera->shutter_close = max;
}


void
ri_api_perspective(
    RtFloat fov )
{
    ri_render_get()->context->option->camera->fov = fov;

    ri_render_get()->context->option->camera->camera_projection =
        RI_PERSPECTIVE;
}
