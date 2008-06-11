/*
 *   lucille | Global Illumination renderer
 *
 *             written by Syoyo Fujita.
 *
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "vector.h"
#include "list.h"
#include "log.h"
#include "raytrace.h"
#include "polygon.h"
#include "util.h"
#include "accel.h"
#include "render.h"
#include "memory.h"
#include "reflection.h"
#include "thread.h"
#include "ugrid.h"

#define EPS 1.0e-6
#define EPSILON 1.0e-6
#define EPSILON2 1.0e-6
#define DIREPS 1.0e-6

static int inside(       const ri_vector_t        pos,
                         ri_float_t               bmin[3],
                         ri_float_t               bmax[3] );

static void build_state( ri_intersection_state_t *state_out,    /* [out] */
                         const ri_vector_t        eye,
                         const ri_vector_t        dir,
                         const ri_float_t         t,
                         const ri_geom_t         *geom,
                         uint32_t                 index,
                         ri_float_t               u,
                         ri_float_t               v );

static uint32_t triangle_intersect(
                         const ri_vector_t        orig,
                         const ri_vector_t        dir,
                         const ri_vector_t        v0,
                         const ri_vector_t        v1,
                         const ri_vector_t        v2,
                         ri_float_t               curr_t,
                         uint32_t                 tid,
                         ri_float_t              *newt, 
                         ri_float_t              *newu, 
                         ri_float_t              *newv, 
                         uint32_t                *newtid );

// TODO: remove
// static void build_z_table();

void
ri_raytrace_setup()
{
    // build_z_table();
}

int
ri_raytrace(
    ri_render_t             *render,
    ri_ray_t                *ray,
    ri_intersection_state_t *state_out )
{
    int                     hit = 0;
    int                     hashit = 0;
    ri_intersection_state_t state;

    ray->t = 0.0;

    render->stat.nrays++;

    state.inside = 0;


    assert(render->scene);
    assert(render->scene->accel);
    assert(render->scene->accel->intersect);
    hit = render->scene->accel->intersect(  render->scene->accel->data,
                                            ray,
                                           &state );

    if (hit) {
        hashit  = 1;

        memcpy( state_out, &state, sizeof( ri_intersection_state_t ) );

    }


    return hashit;
}

void
ri_raytrace_statistics()
{
    uint64_t ngridtravs;
    uint64_t ntesttris;
    uint64_t nrays;

    ri_float_t             elapsed = 0.0;

    ngridtravs   = ri_render_get()->stat.ngridtravs;
    ntesttris    = ri_render_get()->stat.ntesttris;
    nrays        = ri_render_get()->stat.nrays;

    elapsed = ri_timer_elapsed( ri_render_get()->context->timer,
                   "Render frame" );

    printf( "\n" );
    printf( "/= Raytracing statistics =================="
        "====================================\n" );
    printf( "| %-48s:  %20llu\n", "Total rays", nrays );
    printf( "| %-48s:  %20llu\n", "Total grid cell traversals", ngridtravs );
    printf( "| %-48s:  %20llu\n", "Total triangle tests", ntesttris );
    //printf("total mailboxing hits:  %20llu\n", nmailboxhits);
    printf( "| %-48s:  %20.6f\n", "The number of tests per ray",
           ( double )ntesttris / ( double )nrays );
    printf( "| %-48s:  %20.6f\n", "The number of travs per ray",
           ( double )ngridtravs / ( double )nrays );
    printf( "| %-48s:  %20.6f\n", "Rays/sec", ( double )nrays /
           ( double )elapsed );
    printf(
        "\\------------------------------------------------------------------------------\n" );
    fflush( stdout );
}


/*
 * --- private functions ---
 */

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

int
intersect_with_ugrid(
    ri_ugrid_t              *ugrid,
    const ri_vector_t        eye,
    const ri_vector_t        dir,
    ri_intersection_state_t *state_out )
{
    int                     x, y, z;
    int                     hit;
    ri_float_t              rayt = ( ri_float_t )RI_INFINITY;
    ri_float_t              maxt = ( ri_float_t )RI_INFINITY;
    ri_float_t              tvals[2];
    ri_float_t              raypos[3];
    ri_float_t              nextx, nexty, nextz;
    ri_float_t              deltax, deltay, deltaz;
    int                     stepx, stepy, stepz;
    int                     outx, outy, outz;
    ri_intersection_state_t state;
    ri_vector_t             isectpoint;
    ri_float_t              nearest = ( ri_float_t )RI_INFINITY;

    if (dir[0] == 0.0 && dir[1] == 0.0 && dir[2] == 0.0) return 0;

    if (inside( eye, ugrid->bboxmin, ugrid->bboxmax ) )
        rayt = 0.0;
    else {
        if (!intersect_ray_bbox( eye, dir,
                    ugrid->bboxmin, ugrid->bboxmax, tvals ) )
            return 0;
        rayt = tvals[0];
        maxt = tvals[1];
    }

    raypos[0] = eye[0] + dir[0] * rayt;
    raypos[1] = eye[1] + dir[1] * rayt;
    raypos[2] = eye[2] + dir[2] * rayt;

    x = ( int )( ( raypos[0] - ugrid->bboxmin[0] ) * ugrid->invwidth[0] );
    if (x == ugrid->voxels[0]) x--;

    if (fabs( dir[0] ) < DIREPS) {
        nextx  = RI_INFINITY;
        deltax = 0.0;
        stepx  = 0;
        outx   = -1;
    } else if (dir[0] > 0) {
        nextx = rayt +
            ( ( ( x + 1 ) * ugrid->width[0] + ugrid->bboxmin[0] ) -
             raypos[0] ) / dir[0];

        deltax = ugrid->width[0] / dir[0];
        stepx = 1;
        outx = ugrid->voxels[0];
    } else {
        nextx = rayt +
            ( ( x * ugrid->width[0] + ugrid->bboxmin[0] ) -
             raypos[0] ) / dir[0];

        deltax = -ugrid->width[0] / dir[0];
        stepx = -1;
        outx = -1;
    }

    y = ( int )( ( raypos[1] - ugrid->bboxmin[1] ) * ugrid->invwidth[1] );
    if (y == ugrid->voxels[1]) y--;

    if (fabs( dir[1] ) < DIREPS) {
        nexty  = RI_INFINITY;
        deltay = 0.0;
        stepy  = 0;
        outy   = -1;
    } else if (dir[1] > 0) {
        nexty = rayt +
            ( ( ( y + 1 ) * ugrid->width[1] + ugrid->bboxmin[1] ) -
             raypos[1] ) / dir[1];

        deltay = ugrid->width[1] / dir[1];
        stepy = 1;
        outy = ugrid->voxels[1];
    } else {
        nexty = rayt +
            ( ( y * ugrid->width[1] + ugrid->bboxmin[1] ) -
             raypos[1] ) / dir[1];

        deltay = -ugrid->width[1] / dir[1];
        stepy = -1;
        outy = -1;
    }

    z = ( int )( ( raypos[2] - ugrid->bboxmin[2] ) * ugrid->invwidth[2] );
    if (z == ugrid->voxels[2]) z--;

    if (fabs( dir[2] ) < DIREPS) {
        nextz  = RI_INFINITY;
        deltaz = 0.0;
        stepz  = 0;
        outz   = -1;
    } else if (dir[2] < 0) {
        nextz = rayt +
            ( ( z * ugrid->width[2] + ugrid->bboxmin[2] ) -
             raypos[2] ) / dir[2];

        deltaz = -ugrid->width[2] / dir[2];
        stepz = -1;
        outz = -1;
    } else {
        nextz = rayt +
            ( ( ( z + 1 ) * ugrid->width[2] + ugrid->bboxmin[2] ) -
             raypos[2] ) / dir[2];

        deltaz = ugrid->width[2] / dir[2];
        stepz = 1;
        outz = ugrid->voxels[2];
    }

    hit = 0;

    if (x < 0) x = 0;
    if (x > ugrid->voxels[0] - 1) x = ugrid->voxels[0] - 1;
    if (y < 0) y = 0;
    if (y > ugrid->voxels[1] - 1) y = ugrid->voxels[1] - 1;
    if (z < 0) z = 0;
    if (z > ugrid->voxels[2] - 1) z = ugrid->voxels[2] - 1;

    state.inside = 0;

    while (1) {
        //if (g_profile) g_ngridtravcells++;
        //ri_render_get()->stat.ngridtravs++;

        if (ugrid->cell[z][y][x]) {
            if (intersect_foreach_trilist( ugrid->cell[z][y][x],
                              eye, dir,
                              &state ) ) {
                if (state.t > EPS && state.t <= nearest) {

                    hit = 1;

                    nearest = state.t;

                    ri_mem_copy( state_out, &state,
                            sizeof(
                                ri_intersection_state_t ) );

                    /* if intersection point is in this
                     * voxel, terminate traversal.
                     */
                    ri_vector_copy( isectpoint, dir );
                    ri_vector_scale( isectpoint, isectpoint, state.t );
                    ri_vector_add( isectpoint, isectpoint, eye );

                    if (inside_voxel( isectpoint, x, y, z,
                             ugrid ) )
                        break;
                }
            }
        }

        /* 3D DDA */
        if ( ( nextx < nexty ) && ( nextx < nextz ) ) {
            if (maxt < nextx) break;
            x += stepx;
            if (x == outx) break;
            nextx += deltax;
        } else if ( ( nextz < nexty ) ) {
            if (maxt < nextz) break;
            z += stepz;
            if (z == outz) break;
            nextz += deltaz;
        } else {
            if (maxt < nexty) break;
            y += stepy;
            if (y == outy) break;
            nexty += deltay;
        }
    }

    state_out->t = nearest;

    return hit;

}

int
inside( 
    const ri_vector_t pos,
    ri_float_t        bmin[3],
    ri_float_t        bmax[3] )
{
    if (pos[0] < bmin[0]) return 0;
    if (pos[1] < bmin[1]) return 0;
    if (pos[2] < bmin[2]) return 0;

    if (pos[0] > bmax[0]) return 0;
    if (pos[1] > bmax[1]) return 0;
    if (pos[2] > bmax[2]) return 0;

    return 1;
}

int
intersect_ray_bbox( 
    const ri_vector_t  eye,
    const ri_vector_t  dir,
    const ri_float_t   bmin[3],
    const ri_float_t   bmax[3], 
    ri_float_t         isectt[2])        /* [out] */
{
    ri_float_t t0, t1;
    ri_float_t invraydir;
    ri_float_t neart, fart;
    ri_float_t tmp;

    t0 = 0.0; t1 = RI_INFINITY;

    if (dir[0] == 0.0 && dir[1] == 0.0 && dir[2] == 0.0) return 0;

    if (dir[0] != 0.0) {
        invraydir = 1.0 / dir[0];

        neart = ( bmin[0] - eye[0] ) * invraydir;
        fart  = ( bmax[0] - eye[0] ) * invraydir;
        if (neart > fart) {
            tmp = neart; neart = fart; fart = tmp;
        }

        t0 = neart > t0 ? neart : t0;
        t1 = fart  < t1 ? fart  : t1;
        if (t0 > t1) return 0;
    }

    if (dir[1] != 0.0) {
        invraydir = 1.0 / dir[1];

        neart = ( bmin[1] - eye[1] ) * invraydir;
        fart  = ( bmax[1] - eye[1] ) * invraydir;
        if (neart > fart) {
            tmp = neart; neart = fart; fart = tmp;
        }

        t0 = neart > t0 ? neart : t0;
        t1 = fart  < t1 ? fart  : t1;
        if (t0 > t1) return 0;
    }

    if (dir[2] != 0.0) {
        invraydir = 1.0 / dir[2];

        neart = ( bmin[2] - eye[2] ) * invraydir;
        fart  = ( bmax[2] - eye[2] ) * invraydir;
        if (neart > fart) {
            tmp = neart; neart = fart; fart = tmp;
        }

        t0 = neart > t0 ? neart : t0;
        t1 = fart  < t1 ? fart  : t1;
        if (t0 > t1) return 0;
    }

    isectt[0] = t0;
    isectt[1] = t1;

    return 1;
}

int
intersect_foreach_trilist(
    ri_tri_list_t           *list,
    const ri_vector_t        eye,
    const ri_vector_t        dir,
    ri_intersection_state_t *state_out )
{
    int                     i;
    uint32_t                hit = 0;
    uint32_t                hashit = 0;
    uint32_t                tid;
    uint32_t                i0, i1, i2;
    ri_vector_t             v0, v1, v2;
    ri_float_t              u, v;
    ri_geom_t     *RESTRICT geom;
    ri_geom_t     *RESTRICT hitgeom = NULL;
    ri_tri_info_t *RESTRICT triinfo;
    ri_float_t    t = 0.0;

    t = state_out->t;

    for (i = 0; i < list->ntris; i++) {

        triinfo = &list->tris[i];

        geom = triinfo->geom;

#if 0
        if (triinfo->index >= geom->nindices) {
            printf( "triinfo->index = %d\n", triinfo->index );
            exit( -1 );
        }
#endif

        i0 = geom->indices[triinfo->index + 0];
        i1 = geom->indices[triinfo->index + 1];
        i2 = geom->indices[triinfo->index + 2];
        ri_vector_copy(v0, geom->positions[i0]);
        ri_vector_copy(v1, geom->positions[i1]);
        ri_vector_copy(v2, geom->positions[i2]);

        hit |= triangle_intersect( eye, dir, v0, v1, v2, t,
                     triinfo->index, &t, &u, &v, &tid );

        ri_render_get()->stat.ntesttris++;

        if (hashit) {
            hitgeom = geom;
        }
    }

    if (hashit) {
        build_state(state_out, eye, dir, t, hitgeom, tid, u, v);
    }

    return hashit;
}


int
inside_voxel(
    const ri_vector_t  pos,
    int                x,
    int                y,
    int                z,
    const ri_ugrid_t  *grid )
{
    ri_float_t bmin[3];
    ri_float_t bmax[3];

    bmin[0] = x * grid->width[0] + grid->bboxmin[0];
    bmin[1] = y * grid->width[1] + grid->bboxmin[1];
    bmin[2] = z * grid->width[2] + grid->bboxmin[2];

    bmax[0] = ( x + 1 ) * grid->width[0] + grid->bboxmin[0];
    bmax[1] = ( y + 1 ) * grid->width[1] + grid->bboxmin[1];
    bmax[2] = ( z + 1 ) * grid->width[2] + grid->bboxmin[2];

    if (inside( pos, bmin, bmax ) ) return 1;

    return 0;
}

void
build_state(
    ri_intersection_state_t *state_out,    /* [out] */
    const ri_vector_t        eye,
    const ri_vector_t        dir,
    const ri_float_t         t,
    const ri_geom_t         *geom,
    uint32_t                 index,
    ri_float_t               u,
    ri_float_t               v )
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

    ri_vector_copy( isectpoint, dir );
    ri_vector_scale( isectpoint, isectpoint, t );
    ri_vector_add( isectpoint, isectpoint, eye );

    ri_vector_copy( state_out->P, isectpoint );

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

        ri_lerp_vector( state_out->Ng, n0, n1, n2, u, v );

        if (geom->tangents && geom->binormals) {

            ri_vector_copy(n0, geom->tangents[i0]);
            ri_vector_copy(n1, geom->tangents[i1]);
            ri_vector_copy(n2, geom->tangents[i2]);

            ri_lerp_vector( state_out->tangent, n0, n1, n2, u, v );

            ri_vector_copy(n0, geom->binormals[i0]);
            ri_vector_copy(n1, geom->binormals[i1]);
            ri_vector_copy(n2, geom->binormals[i2]);

            ri_lerp_vector( state_out->binormal, n0, n1, n2, u, v );
        } else {

            ri_ortho_basis( tmpbasis, state_out->Ng );
            ri_vector_copy( state_out->tangent, tmpbasis[0] );
            ri_vector_copy( state_out->binormal, tmpbasis[1] );
        }

    } else {

        ri_normal_of_triangle( state_out->Ng, v0, v1, v2 );
        ri_ortho_basis( tmpbasis, state_out->Ng );
        ri_vector_copy( state_out->tangent, tmpbasis[0] );
        ri_vector_copy( state_out->binormal, tmpbasis[1] );
    }

    if (geom->colors) {
        ri_vector_copy(c0, geom->colors[i0]);
        ri_vector_copy(c1, geom->colors[i1]);
        ri_vector_copy(c2, geom->colors[i2]);

        ri_lerp_vector( state_out->color, c0, c1, c2, u, v );
    } else {

        ri_vector_set1( defcol, 1.0 );
        ri_vector_copy( state_out->color, defcol );
    }

#if 0
    if (geom->opacities)
        state->opacity = geom->opacities[i0].f[0];
    else
        state->opacity = 1.0;
#endif

    if (geom->texcoords) {
        lerp_uv( &state_out->u, &state_out->v,
            (const ri_float_t *)&geom->texcoords[2 * i0],
            (const ri_float_t *)&geom->texcoords[2 * i1],
            (const ri_float_t *)&geom->texcoords[2 * i2],
            u, v );
    } else {
        state_out->u = 0.0;
        state_out->v = 0.0;
    }

    if (geom->two_side) {
        if (index < geom->nindices / 2) {
            state_out->inside = 0;
        } else {
            /* surface is inside of geometry */
            state_out->inside = 1;
        }
    } else {
        state_out->inside = 0;
    }

    state_out->geom  = (ri_geom_t *)geom;
    state_out->index = index;
    //state->kd    = geom->kd;
    //state->ks    = geom->ks;
}

#if 0
static void
build_z_table()
{
    uint32_t i, j;
    uint32_t bit;
    uint32_t v;
    uint32_t ret;
    uint32_t mask = 0x1;
    int          shift;

    for (i = 0; i < 256; i++) {
        v = i;
        shift = 0;
        ret = 0;
        for (j = 0; j < 8; j++) {
            /* extract (j+1)'th bit */
            bit = ( v >> j ) & mask;

            ret += bit << shift;
            shift += 3;
        }

        g_z_table[i] = ret;
    }
}
#endif

/*
 * codes from
 * "Practical Analysis of Optimized Ray-Triangle Intersection"
 * Tomas Moeller
 * http://www.ce.chalmers.se/staff/tomasm/raytri/
 *
 * : we use opti1 version(divide at end).
 *   this version seems faster on x86 CPU.
 */
uint32_t
triangle_intersect(
    const ri_vector_t  orig,
    const ri_vector_t  dir,
    const ri_vector_t  v0,
    const ri_vector_t  v1,
    const ri_vector_t  v2,
    ri_float_t         curr_t,
    uint32_t           tid,
    ri_float_t        *newt, 
    ri_float_t        *newu, 
    ri_float_t        *newv, 
    uint32_t          *newtid )
{
    /* ray-triangle intersection test with back face culling test */

    ri_vector_t edge1, edge2, tvec, pvec, qvec;
    ri_float_t  det, inv_det;
    ri_float_t  u, v, t;

    ri_vector_sub( edge1, v1, v0 );
    ri_vector_sub( edge2, v2, v0 );

    /* begin calculation determinant */
    ri_vector_cross( pvec, dir, edge2 );

    /* if determinant is near zero, ray lies in plane of triangle */
    det = ri_vector_dot( edge1, pvec );

    if (det > EPSILON) {

        ri_vector_sub( tvec, orig, v0 );

        u = ri_vector_dot( tvec, pvec );
        if (u < 0.0 || u > det) return 0;

        ri_vector_cross( qvec, tvec, edge1 );

        v = ri_vector_dot( dir, qvec );
        if (v < 0.0 || u + v > det) return 0;
    } else
        /* ray is parallel to the plane of the triangle or
         * the ray hits back-faced triangle. */
        return 0;

    inv_det = 1.0 / det;

    t = ri_vector_dot( edge2, qvec );

    if (t < curr_t && t > EPS) {

        ( *newt )   = t * inv_det;
        ( *newu )   = u * inv_det;
        ( *newv )   = v * inv_det;
        ( *newtid ) = tid;

    }

    return 1;       /* hit! */
}

