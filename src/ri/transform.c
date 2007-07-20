/*
 *   lucille | Global Illumination render
 *
 *             written by Syoyo.
 *
 */

/*
 * RI Transformation API implementation
 *
 * $Id$
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "matrix.h"
#include "quaternion.h"
#include "memory.h"
#include "vector.h"
#include "list.h"
#include "render.h"
#include "stack.h"


void
ri_api_identity(void)
{
	ri_matrix_t  *m;

	m = (ri_matrix_t *)ri_stack_get(ri_render_get()->context->trans_stack);

	ri_matrix_identity(m);
}

void
ri_api_transform(RtMatrix transform)
{
	ri_matrix_t *m;

	// pointer to topmost transformation matrix stack
	m = (ri_matrix_t *)ri_stack_get(ri_render_get()->context->trans_stack);

	// m = transform
	ri_matrix_set(m, transform); 
}

void
ri_api_concat_transform(RtMatrix transform)
{
	ri_matrix_t *m;
	ri_matrix_t  tmp, trans;

	// pointer to topmost transformation matrix stack
	m = (ri_matrix_t *)ri_stack_get(ri_render_get()->context->trans_stack);

	// m = m transform
	ri_matrix_copy(&tmp, m); 
	ri_matrix_set(&trans, transform); 
	ri_matrix_mul(m, &trans, &tmp); 
}

void
ri_api_translate(RtFloat dx, RtFloat dy, RtFloat dz)
{
	ri_matrix_t *m;

	// pointer to topmost transformation matrix stack
	m = (ri_matrix_t *)ri_stack_get(ri_render_get()->context->trans_stack);

	ri_matrix_translate(m, dx, dy, dz);
}

void
ri_api_rotate(RtFloat angle, RtFloat dx, RtFloat dy, RtFloat dz)
{
	ri_matrix_t *m;

	// pointer to topmost transformation matrix stack
	m = (ri_matrix_t *)ri_stack_get(ri_render_get()->context->trans_stack);

	ri_matrix_rotate(m, angle, dx, dy, dz);
}

void
ri_api_scale(RtFloat sx, RtFloat sy, RtFloat sz)
{
	ri_matrix_t *m;

	// pointer to topmost transformation matrix stack
	m = (ri_matrix_t *)ri_stack_get(ri_render_get()->context->trans_stack);

	ri_matrix_scale(m, sx, sy, sz);

}
