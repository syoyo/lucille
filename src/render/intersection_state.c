/*
 *   lucille | Global Illumination render
 *
 *             written by Syoyo.
 *
 */

#include "intersection_state.h"
#include "memory.h"

/*
 * Function: ri_intersection_state_new
 *
 *    Allocates memory for ri_intersection_state_t object.
 *
 * Parameters:
 *
 *    None
 *
 * Returns:
 *
 *    Pointer of created ri_intersection_state_t object.
 */
ri_intersection_state_t *
ri_intersection_state_new()
{
	ri_intersection_state_t *state;

	state = ( ri_intersection_state_t * )
		ri_mem_alloc( sizeof( ri_intersection_state_t ) );

	return state;
}

/*
 * Function: ri_intersection_state_delete
 *
 *    Frees memory for ri_intersection_state_t object.
 *
 * Parameters:
 *
 *    state - Pointer to ri_intersection_state_t object to be freed
 *
 * Returns:
 *
 *    None.
 */
void
ri_intersection_state_delete(
	ri_intersection_state_t *state )
{
	ri_mem_free( state );
}
