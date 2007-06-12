/*
 *   lucille | Global Illumination renderer
 *
 *             written by Syoyo Fujita.
 *
 */

/*
 * RenderMan camera model
 *
 * $Id: camera.h,v 1.1.1.1 2004/01/06 13:57:08 syoyo Exp $
 */

#ifndef LUCILLE_CAMERA_H
#define LUCILLE_CAMERA_H

#include "ri.h"
#include "vector.h"
#include "matrix.h"

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Struct: ri_camera_t
 *
 *     RenderMan camera object
 */
typedef struct _ri_camera_t {
	/* The horizonttal resolution in the output image. */
	RtInt       horizontal_resolution;

	/* The vertical resolution in the output image. */
	RtInt       vertical_resolution;

	/* The ratio of the width to the height of a single pixel. */
	RtFloat     pixel_aspect_ratio;

	/* The region of the raster that is actually rendered. */
	RtFloat     crop_window[4];

	/* The aspect ratio of the desired image. */
	RtFloat     frame_aspect_ratio;

	/*
	 * The screen coordinates(coorinates after the projection) of the area
	 * to be rendererd.
	 */
	RtFloat     screen_window[4];

	/* The camera to screen projection. */
	RtToken     camera_projection;

	/* World to camera transformation. */
	ri_matrix_t world_to_camera;

	/* Camera to world transformation. */
	ri_matrix_t camera_to_world;

	/* The postion of th near and far clipping planes. */
	RtFloat     nearclip, farclip;

	/* Additional planes that clip geometry from the scene. */
	/* GList *plane_list; */

	/*
	 * Parameters controlling depth of field.
	 */
	RtFloat     fstop;
	RtFloat     focal_length, focal_distance;

	/* The times when the shutter opens and closes. */
	RtFloat     shutter_open, shutter_close;

	/* field of view */
	RtFloat     fov;

	/* Uset OpenGL style camera setting */
	int         use_glcamera;
	ri_vector_t cam_pos;
	ri_vector_t cam_at;
	ri_vector_t cam_up;


	ri_float_t  flength;

} ri_camera_t;

extern ri_camera_t *	ri_camera_new();
extern void		ri_camera_free( ri_camera_t *camera );

extern void		ri_camera_get_glmatrix( ri_matrix_t *m,     /* [out] */
					        const ri_camera_t *camera );

extern void ri_camera_get_pos_and_dir(
	ri_vector_t       *pos,  /* [out] */
	ri_vector_t       *dir,  /* [out] */
	const ri_camera_t *camera,
	ri_float_t         x,
	ri_float_t         y);

#ifdef __cplusplus
}       /* extern "C" */
#endif

#endif  /* LUCILLE_CAMERA_H */
