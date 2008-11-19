/*
 * $Id: context.c,v 1.3 2004/04/16 13:46:45 syoyo Exp $
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "matrix.h"
#include "apitable.h"
#include "attribute.h"
#include "context.h"
#include "log.h"
#include "render.h"

ri_context_t *
ri_context_new()
{
	ri_context_t   *ctx   = NULL;
	ri_matrix_t    *ident = NULL;
	ri_attribute_t *attr  = NULL;

	ctx = (ri_context_t *)ri_mem_alloc(sizeof(ri_context_t));

	ctx->option          = ri_option_new();
	ctx->trans_stack     = ri_stack_new();
	ctx->attr_stack      = ri_stack_new();
	ctx->timer           = ri_timer_new();
	ctx->declares        = ri_hash_new();
	ctx->world_block     = 0;
	ctx->arealight_block = 0;

	ctx->world_begin_cb  = NULL;
	ctx->world_end_cb    = NULL;
	ctx->render_end_cb   = NULL;

	/* add identity matrix to matrxix_stack */
	ident = ri_matrix_new();
	ri_matrix_identity(ident);
	ri_stack_push(ctx->trans_stack, (void *)ident);

	/* add default attribute state */
	attr = ri_attribute_new();
	ri_stack_push(ctx->attr_stack, (void *)attr);

	ri_matrix_identity(&ctx->world_to_camera);

	return ctx;
}

void
ri_context_free(ri_context_t *ctx)
{
	ri_matrix_t    *matptr;
	ri_attribute_t *attrptr;

	ri_option_free(ctx->option);

	matptr = (ri_matrix_t *)ri_stack_get(ctx->trans_stack);
	while (matptr != NULL) {
		ri_matrix_free(matptr);
		ri_stack_pop(ctx->trans_stack);
		matptr = (ri_matrix_t *)ri_stack_get(ctx->trans_stack);
	}
	ri_stack_free(ctx->trans_stack);

	attrptr = (ri_attribute_t *)ri_stack_get(ctx->attr_stack);
	while (attrptr != NULL) {
		ri_attribute_free(attrptr);
		ri_stack_pop(ctx->attr_stack);
		attrptr = (ri_attribute_t *)ri_stack_get(ctx->attr_stack);
	}
	ri_stack_free(ctx->attr_stack);

	ri_mem_free(ctx);
}

void
ri_api_transform_begin()
{
	ri_matrix_t *newmat = NULL;
	ri_matrix_t *mat = NULL;

	newmat = (ri_matrix_t *)ri_mem_alloc(sizeof(ri_matrix_t));

	mat = (ri_matrix_t *)ri_stack_get(
				ri_render_get()->context->trans_stack);

	ri_log_and_return_if(mat == NULL);

	ri_matrix_copy(newmat, mat);

	ri_stack_push(ri_render_get()->context->trans_stack, (void *)newmat);
}

void
ri_api_transform_end()
{
	ri_matrix_t *mat = NULL;

	mat = (ri_matrix_t *)ri_stack_get(
				ri_render_get()->context->trans_stack);

	ri_log_and_return_if(mat == NULL);

	ri_matrix_free(mat);

	ri_stack_pop(ri_render_get()->context->trans_stack);
}

void
ri_api_begin(RtToken name)
{
	if (name != RI_NULL) {
		ri_log(LOG_WARN, "RiBegin: Only RI_NULL is supported");
	}

	ri_render_init();

	return;
}

void
ri_api_end(void)
{
	ri_render_free();

	return;
}

void
ri_api_world_begin()
{
	const ri_matrix_t *m;
	      ri_matrix_t *ident;
	
	ri_render_get()->context->world_block++;

	/* copy current transformation matrix to world_to_camera */
	m = (ri_matrix_t *)ri_stack_get(ri_render_get()->context->trans_stack);
	if (m != NULL) {
		ri_matrix_copy(&(ri_render_get()->context->world_to_camera), m);
	}

	/* push identity matrtix to current transformation stack */
	ident = ri_matrix_new();
	ri_matrix_identity(ident);

	ri_stack_push(ri_render_get()->context->trans_stack, (void *)ident);

	if (ri_render_get()->context->world_begin_cb) {
		ri_render_get()->context->world_begin_cb();
	}
}

void
ri_api_world_end()
{
	ri_log_and_return_if(ri_render_get()->context->world_block == 0);
	ri_render_get()->context->world_block--;

	/* ri_timer_start() is called in lsh/main.c */
	ri_timer_end(ri_render_get()->context->timer, "RIB parsing");

	/*
	 * begin rendering!
	 */
	ri_render_frame();

	/*
	 * Call user defined callback
	 */
	if (ri_render_get()->context->world_end_cb) {
		ri_render_get()->context->world_end_cb();
	}
}

void
ri_api_orientation(RtToken orientation)
{
	ri_option_t *opt;

	opt = ri_render_get()->context->option;

	if (strcmp(orientation, RI_LH) == 0) {
		opt->orientation = RI_LH;
	} else if (strcmp(orientation, RI_RH) == 0) {
		opt->orientation = RI_RH;
	}
}


void
ri_api_exposure(RtFloat gain, RtFloat gamma)
{
	ri_display_t *disp;

    /* Get current display driver */
	disp = ri_option_get_curr_display(ri_render_get()->context->option);

	disp->gain  = gain;
	disp->gamma = gamma;
}

void
ri_api_pixel_samples(RtFloat xsamples, RtFloat ysamples)
{
	ri_display_t *disp;

	disp = ri_option_get_curr_display(ri_render_get()->context->option);

	if (xsamples < 1.0) xsamples = 1.0;
	if (ysamples < 1.0) ysamples = 1.0;
	disp->sampling_rates[0] = xsamples;
	disp->sampling_rates[1] = ysamples;
}

