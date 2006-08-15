/*
 * $Id: camera.c,v 1.3 2004/06/13 06:44:51 syoyo Exp $
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "log.h"
#include "camera.h"
#include "render.h"

ri_camera_t *
ri_camera_new()
{
	ri_matrix_t mat;
	ri_camera_t *camera = NULL;

	camera = (ri_camera_t *)ri_mem_alloc(sizeof(ri_camera_t));

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

	ri_matrix_identity(&mat);
	camera->world_to_camera   = mat;

	camera->nearclip          = (float)RI_EPSILON;
	camera->farclip           = (float)RI_INFINITY;

	camera->fstop             = (float)RI_INFINITY;
	camera->focal_length      = 0.0f;
	camera->focal_distance    = 0.0f;

	camera->shutter_open      = 0.0f;
	camera->shutter_close     = 0.0f;

	camera->fov               = 90.0f;

	camera->use_glcamera      = 0;
	ri_vector_zero(&camera->cam_pos);
	ri_vector_zero(&camera->cam_at);
	ri_vector_zero(&camera->cam_up);

	return camera;
}

void
ri_camera_free(ri_camera_t *camera)
{
	ri_mem_free(camera);
}

void
ri_api_format(RtInt xres, RtInt yres, RtFloat aspect)
{
	if (xres < 0) {
		ri_log(LOG_WARN, "Format: xres < 0");
		xres = 640;	/* set to default */
	}

	if (yres < 0) {
		ri_log(LOG_WARN, "Format: yres < 0");
		xres = 480;	/* set to default */
	}

	if (aspect < 0) {
		ri_log(LOG_WARN, "Format: aspect < 0");
		aspect = 1.0;	/* set to default */
	}

	ri_render_get()->context->option->camera->horizontal_resolution = xres;
	ri_render_get()->context->option->camera->vertical_resolution   = yres;
	ri_render_get()->context->option->camera->pixel_aspect_ratio  = aspect;
}

void
ri_api_frame_aspect_ratio(RtFloat aspect)
{
	ri_render_get()->context->option->camera->frame_aspect_ratio = aspect;
}

void
ri_api_screen_window(RtFloat left, RtFloat right, RtFloat bot, RtFloat top)
{
	ri_render_get()->context->option->camera->screen_window[0] = left;
	ri_render_get()->context->option->camera->screen_window[1] = right;
	ri_render_get()->context->option->camera->screen_window[2] = bot;
	ri_render_get()->context->option->camera->screen_window[3] = top;
}

void
ri_api_crop_window(RtFloat xmin, RtFloat xmax, RtFloat ymin, RtFloat ymax)
{
	ri_log(LOG_WARN, "RiCropWindo is not yet implemented");

	ri_render_get()->context->option->camera->crop_window[0] = xmin;
	ri_render_get()->context->option->camera->crop_window[1] = xmax;
	ri_render_get()->context->option->camera->crop_window[2] = ymin;
	ri_render_get()->context->option->camera->crop_window[3] = ymax;
}

void
ri_api_projection(RtToken name, RtInt n, RtToken tokens[], RtPointer params[])
{
	int i;
	ri_camera_t *camera;

	ri_log_and_return_if(strcmp(name, RI_ORTHOGRAPHIC) != 0 &&
			     strcmp(name, RI_PERSPECTIVE)  != 0);

	camera = ri_render_get()->context->option->camera;

	for (i = 0; i < n; i++) {
		if (strcmp(tokens[i], "fov") == 0) {
			camera->fov = *(RtFloat *)params[i];			
			camera->camera_projection = RI_PERSPECTIVE;
		}
	}
}

void
ri_api_clipping(RtFloat hither, RtFloat yon)
{
	ri_log(LOG_WARN, "RiClipping is not yet implemented");

	ri_render_get()->context->option->camera->nearclip = hither;
	ri_render_get()->context->option->camera->farclip  = yon;

}

void
ri_api_clipping_plane(RtFloat x, RtFloat y, RtFloat z,
                      RtFloat nx, RtFloat ny, RtFloat nz)
{
	ri_log(LOG_WARN, "RiClippingPlane is not yet implemented");
	(void)x;
	(void)y;
	(void)z;
	(void)nx;
	(void)ny;
	(void)nz;
}

void
ri_api_depth_of_field(RtFloat fstop, RtFloat focallength,
		      RtFloat focaldistance)
{
	ri_camera_t *camera = ri_render_get()->context->option->camera;

	camera->fstop          = fstop;	
	camera->focal_length   = focallength;	
	camera->focal_distance = focaldistance;	
}

void
ri_api_shutter(RtFloat min, RtFloat max)
{
	ri_log(LOG_WARN, "RiShutter is not yet implemented");

	ri_render_get()->context->option->camera->shutter_open  = min;	
	ri_render_get()->context->option->camera->shutter_close = max;	
}


void
ri_api_perspective(RtFloat fov)
{
	ri_render_get()->context->option->camera->fov = fov;

	/* use "perspective" projection */
	ri_render_get()->context->option->camera->camera_projection =
							RI_PERSPECTIVE;
}
