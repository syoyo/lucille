/*
 * Renderer internal context.
 * The context includes graphics state, geometry data, etc.
 *
 * $Id: context.h,v 1.2 2004/01/30 04:43:57 syoyo Exp $
 */

#ifndef CONTEXT_H
#define CONTEXT_H

#include "list.h"
#include "option.h"
#include "matrix.h"
#include "stack.h"
#include "timer.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _ri_context_t
{
	ri_option_t     *option;
	ri_stack_t	*trans_stack;		/* transformation stack */
	ri_stack_t	*attr_stack;		/* attribute stack */
	unsigned int     world_block;
	unsigned int     arealight_block;              
	ri_matrix_t      world_to_camera;	/*  World to camera
						    transformation matrix */
	ri_timer_t      *timer;
	ri_hash_t       *declares;

	/* backdoors */
	void            (*world_begin_cb)(void);
	void            (*world_end_cb)(void);
	void            (*render_end_cb)(void);
} ri_context_t;

extern ri_context_t *ri_context_new ();
extern void	     ri_context_free(ri_context_t *ctx);

#ifdef __cplusplus
}	/* extern "C" */
#endif


#endif
