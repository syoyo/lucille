/*
 *   lucille | Global Illumination render
 *
 *             written by Syoyo.
 *
 */

#include "vector.h"
#include "geometric.h"
#include "reflection.h"
#include "intersection_state.h"
#include "memory.h"

/* ----------------------------------------------------------------------------
 *
 * Static definitions
 *
 * ------------------------------------------------------------------------- */

static void lerp_uv(
    ri_float_t *newu,           /* [out]    */
    ri_float_t *newv,           /* [out]    */
    const ri_float_t *uv0,
    const ri_float_t *uv1,
    const ri_float_t *uv2,
    ri_float_t u,
    ri_float_t v );


/* ----------------------------------------------------------------------------
 *
 * Public functions
 *
 * ------------------------------------------------------------------------- */

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
 * Function: ri_intersection_state_free
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
ri_intersection_state_free(
	ri_intersection_state_t *state )
{
	ri_mem_free( state );
}

/*
 * Function: ri_intersection_state_build
 *
 *    Fills information of interection state.
 *
 * Parameters:
 *
 *    state_inout - Pointer to ri_intersection_state_t object to be filled &
 *                  updated.
 *    eye         - rayorg
 *    dir         - raydir
 *
 * Returns:
 *
 *    None.
 */
void
ri_intersection_state_build(
    ri_intersection_state_t *state_inout,    /* [inout] */
    const ri_vector_t        eye,
    const ri_vector_t        dir)
{
    ri_vector_t tmpbasis[3];
    ri_vector_t defcol;
    uint32_t    i0, i1, i2;
    ri_vector_t v0;
    ri_vector_t v1;
    ri_vector_t v2;
    ri_vector_t n0;
    ri_vector_t n1;
    ri_vector_t n2;
    ri_vector_t c0;
    ri_vector_t c1;
    ri_vector_t c2;

    ri_vector_t isectpoint;

    ri_float_t   t;
    ri_float_t   u;
    ri_float_t   v;
    ri_geom_t   *geom;
    uint32_t     index;

    t     = state_inout->t;
    u     = state_inout->u;
    v     = state_inout->v;
    geom  = state_inout->geom;
    index = state_inout->index;

    isectpoint[0] = eye[0] + dir[0] * t;
    isectpoint[1] = eye[1] + dir[1] * t;
    isectpoint[2] = eye[2] + dir[2] * t;
    isectpoint[3] = 0.0;

    ri_vector_copy( state_inout->P, isectpoint );

    i0 = geom->indices[index + 0];
    i1 = geom->indices[index + 1];
    i2 = geom->indices[index + 2];

    ri_vector_copy(v0, geom->positions[i0]);
    ri_vector_copy(v1, geom->positions[i1]);
    ri_vector_copy(v2, geom->positions[i2]);

    if (geom->normals) {
        ri_vector_copy(n0, geom->normals[i0]);
        ri_vector_copy(n1, geom->normals[i1]);
        ri_vector_copy(n2, geom->normals[i2]);

        ri_lerp_vector( state_inout->Ng, n0, n1, n2, u, v );

        if (geom->tangents && geom->binormals) {

            ri_vector_copy(n0, geom->tangents[i0]);
            ri_vector_copy(n1, geom->tangents[i1]);
            ri_vector_copy(n2, geom->tangents[i2]);

            ri_lerp_vector( state_inout->tangent, n0, n1, n2, u, v );

            ri_vector_copy(n0, geom->binormals[i0]);
            ri_vector_copy(n1, geom->binormals[i1]);
            ri_vector_copy(n2, geom->binormals[i2]);

            ri_lerp_vector( state_inout->binormal, n0, n1, n2, u, v );
        } else {

            ri_ortho_basis( tmpbasis, state_inout->Ng );
            ri_vector_copy( state_inout->tangent, tmpbasis[0] );
            ri_vector_copy( state_inout->binormal, tmpbasis[1] );
        }

    } else {

        ri_normal_of_triangle( state_inout->Ng, v0, v1, v2 );
        ri_ortho_basis( tmpbasis, state_inout->Ng );
        ri_vector_copy( state_inout->tangent, tmpbasis[0] );
        ri_vector_copy( state_inout->binormal, tmpbasis[1] );
    }

    if (geom->colors) {
        ri_vector_copy(c0, geom->colors[i0]);
        ri_vector_copy(c1, geom->colors[i1]);
        ri_vector_copy(c2, geom->colors[i2]);

        ri_lerp_vector( state_inout->color, c0, c1, c2, u, v );
    } else {

        ri_vector_set1( defcol, 1.0 );
        ri_vector_copy( state_inout->color, defcol );
    }

#if 0
    if (geom->opacities)
        state->opacity = geom->opacities[i0].f[0];
    else
        state->opacity = 1.0;
#endif

    if (geom->texcoords) {
        lerp_uv( &state_inout->stqr[0], &state_inout->stqr[1],
            (const ri_float_t *)&geom->texcoords[2 * i0],
            (const ri_float_t *)&geom->texcoords[2 * i1],
            (const ri_float_t *)&geom->texcoords[2 * i2],
            u, v );
    } else {
        state_inout->stqr[0] = 0.0;
        state_inout->stqr[1] = 0.0;
    }

    if (geom->two_side) {
        if (index < geom->nindices / 2) {
            state_inout->inside = 0;
        } else {
            /* surface is inside of geometry */
            state_inout->inside = 1;
        }
    } else {
        state_inout->inside = 0;
    }

    state_inout->geom  = (ri_geom_t *)geom;
    state_inout->index = index;
    //state->kd    = geom->kd;
    //state->ks    = geom->ks;
}


/* ----------------------------------------------------------------------------
 *
 * Private functions
 *
 * ------------------------------------------------------------------------- */

void
lerp_uv(
    ri_float_t *newu,
    ri_float_t *newv,
    const ri_float_t *uv0,
    const ri_float_t *uv1,
    const ri_float_t *uv2,
    ri_float_t u,
    ri_float_t v )
{
    /* n = (1 - u - v) n0 + u n1 + v n2 */

    *newu = ( 1 - u - v ) * uv0[0] + u * uv1[0] + v * uv2[0];
    *newv = ( 1 - u - v ) * uv0[1] + u * uv1[1] + v * uv2[1];
}
