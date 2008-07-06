/*
 *   lucille | Global Illumination renderer
 *
 *             written by Syoyo Fujita.
 *
 */

/*
 * Implements beam tracing algorithm.
 *
 * $Id$
 *
 *  inner node:
 *    - Do packet traversal
 *
 *  leaf node :
 *    - Project triangles in the leaf against domiant axis of the
 *      beam.
 *    
 *    - Clip triangles and beam in 2D domain. 
 *
 *
 *
 * References:
 *
 *   - A Real-time Beam Tracer with Application to Exact Soft Shadows
 *     Ryan Overbeck, Ravi Ramamoorthi and William R. Mark
 *     EuroGraphics Symposium on Rendering, 2007.
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <assert.h>

#include "beam.h"
#include "bvh.h"

typedef ri_float_t point2d_t[2];

typedef struct _plane2d_t
{
    point2d_t p;      /* position */
    point2d_t n;      /* normal   */
} plane2d_t;



/* ----------------------------------------------------------------------------
 *
 * Private function definitions
 *
 * ------------------------------------------------------------------------- */

static ri_float_t safeinv( ri_float_t a, ri_float_t eps, ri_float_t val );

/*
 * Clipping routine.
 */

static ri_float_t intersect(
          point2d_t    i_out,           /* [out]    */
    const point2d_t    s,
    const point2d_t    p,
    const plane2d_t   *b);

static int inside(
          point2d_t    p,
    const plane2d_t   *b);

static void clip(
          point2d_t   *outer_vlist_out,       /* [out]    */
          int         *outer_len_out,         /* [out]    */
          point2d_t   *inner_vlist_out,       /* [out]    */
          int         *inner_len_out,         /* [out]    */
    const point2d_t   *vlist_in,
          int          len_in,
    const plane2d_t   *clip_plane);



/* ----------------------------------------------------------------------------
 *
 * Private functions
 *
 * ------------------------------------------------------------------------- */

#define vdot2d( a, b ) ((a)[0] * (b)[0] + (a)[1] * (b)[1])

/*
 *   1.0 / a       if fabs(a) > eps
 *   val           otherwise
 */
ri_float_t 
safeinv( ri_float_t a, ri_float_t eps, ri_float_t val )
{
    if ( fabs(a) > eps ) {
        return 1.0 / a;
    } else {
        return val;
    }
}

ri_float_t
intersect(
          point2d_t    i_out,       /* [out]    */
    const point2d_t    s,
    const point2d_t    p,
    const plane2d_t   *b)
{
    point2d_t   v;
    ri_float_t  vdotn;
    ri_float_t  sdotn;
    ri_float_t  d;

    ri_float_t  t;


    /*
     * v = direction    
     */
    v[0] = p[0] - s[0];
    v[1] = p[1] - s[1];

    vdotn = vdot2d( v, b->n );
    if (fabs(vdotn) < RI_EPS) {
        printf("Error: vdotn is too small!\n");
        vdotn = 1.0;
    } 

    /*
     * d = plane equation(= -(p . n) )
     */
    d = -( vdot2d( b->p, b->n ) );

    sdotn = vdot2d( s, b->n );
    
    /*
     * t = ray parameter of intersection point.
     */
    t = -(sdotn + d) / vdotn;
    assert(t >= 0.0);

    i_out[0] = s[0] + t * v[0];
    i_out[1] = s[1] + t * v[1];

    return t;
}

/*
 * Check whether the vertex is inside or not against plane boundary
 * return 1 if the vertex is inside, 0 if not.
 */
int
inside(
          point2d_t  p,
    const plane2d_t *b)
{
    point2d_t   pb;
    ri_float_t  d;

    pb[0] = p[0] - b->p[0];
    pb[1] = p[1] - b->p[1];

    // if ((p - b.pos) dot b.normal) >= 0, then p is inside
    d = vdot2d( pb, b->n );

    if (d >= 0) return 1;
    
    return 0;
}

/*
 * Add the vertex to output.
 */
void
output(
    point2d_t   *list_out,          /* [out]    */
    int         *len_inout,         /* [inout]  */
    point2d_t    p)
{
    int idx;

    idx = (*len_inout);

    vcpy( list_out[idx], p );

    (*len_inout)++;
}


/*
 * Clipper
 */
void
clip(
          point2d_t   *outer_vlist_out,       /* [out]    */
          int         *outer_len_inout,       /* [out]    */
          point2d_t   *inner_vlist_out,       /* [out]    */
          int         *inner_len_inout,       /* [out]    */
    const point2d_t   *vlist_in,
          int          len_in,
    const plane2d_t   *clip_plane)
{

    int                j;
    ri_float_t         t;
    point2d_t         *s_ptr, *p_ptr;
    point2d_t          newv;

    /* start with last vertex.    */
    s_ptr = (point2d_t *)&vlist_in[len_in - 1];    

    for (j = 0; j < len_in; j++) {

        p_ptr = (point2d_t *)&vlist_in[j];    /* current vertex    */

        printf("p ptx = %f, %f\n", (*p_ptr)[0], (*p_ptr)[1]);

        if (inside( (*p_ptr), clip_plane )) {

            if (inside( (*s_ptr), clip_plane )) {

                output(inner_vlist_out, inner_len_inout, (*p_ptr) );

                /* No outer */

            } else {
    
                t = intersect(newv, (*s_ptr), (*p_ptr), clip_plane);

                printf("t = %f, new = %f, %f, p = %f, %f\n",
                    t, newv[0], newv[1], (*p_ptr)[0], (*p_ptr)[1]);

                if (t < 1.0) {  /* If t == 1.0, don't output newv */
                    printf("IO, output new\n");
                    output(inner_vlist_out, inner_len_inout, newv);
                }
                output(inner_vlist_out, inner_len_inout, (*p_ptr));

                output(outer_vlist_out, outer_len_inout, newv);
            
            }

        } else {

            if (inside( (*s_ptr), clip_plane) ) {

                t = intersect(newv, (*s_ptr), (*p_ptr), clip_plane);

                printf("t = %f, new = %f, %f, p = %f, %f\n",
                    t, newv[0], newv[1], (*p_ptr)[0], (*p_ptr)[1]);

                output(outer_vlist_out, outer_len_inout, newv);
                output(outer_vlist_out, outer_len_inout, (*p_ptr));

                if (t > 0.0) {  /* If t == 0, don't output newv */
                    printf("OI, output new\n");
                    output(inner_vlist_out, inner_len_inout, newv);
                }

            } else {

                output(outer_vlist_out, outer_len_inout, (*p_ptr) );

            }

        }

        s_ptr = p_ptr;
    }
}

/*
 * Function: ri_bem_set
 *
 *   Setups a beam structure.
 *
 *
 * Parameters:
 *
 *   beam   - Pointer to the beam to be set up. 
 *   org    - Origin of beam.
 *   dir[4] - Corner rays of beam. 
 *
 *
 * Returns:
 *
 *   0 if OK, otherwise if err
 */
int
ri_beam_set(
    ri_beam_t   *beam,      /* [inout]  */
    ri_vector_t  org,
    ri_vector_t  dir[4])
{
    int        i, j;
    int        mask;
    int        dominant_axis;
    int        zeros;
    ri_float_t maxval;  

    beam->d     = 1024.0;
    beam->t_max = RI_INFINITY;

    /*
     * Check if beam's directions lie in same quadrant.
     */
    for (i = 0; i < 3; i++) {

        zeros = 0;
        mask  = 0;

        for (j = 0; j < 4; j++) {
            if (fabs(dir[j][i]) < RI_EPS) {
                zeros++;
            } else {
                mask  += (dir[j][i] <  0.0) ? 1 : -1;
            }
        }

        if ( (mask != -(4 - zeros)) && (mask != (4 - zeros)) ) {

            /* FIXME:
             * split beam so that subdivided beam has same sign.
             */
            fprintf(stderr, "TODO: Beam's dir does not have same sign.\n");

            for (j = 0; j < 4; j++) {
                fprintf(stderr, "      dir[%d] = %f, %f, %f\n", j,
                    dir[j][0], dir[j][1], dir[j][2]);
            }

            return -1;

        }

    }

    vcpy( beam->org,    org    );

    /*
     * Find dominant plane. Use dir[0]
     */
    maxval        = dir[0][0];
    dominant_axis = 0;
    if ( (maxval < dir[0][1]) ) {
        maxval        = dir[0][0];
        dominant_axis = 1;
    }
    if ( (maxval < dir[0][2]) ) {
        maxval        = dir[0][2];
        dominant_axis = 2;
    }
         
    beam->dominant_axis = dominant_axis;

    /*
     * Precompute sign of direction.
     * We know all 4 directions has same sign, thus dir[0] is used to
     * get sign of direction for each axis.
     */
    beam->dirsign[0] = (dir[0][0] < 0.0) ? 1 : 0;
    beam->dirsign[1] = (dir[0][1] < 0.0) ? 1 : 0;
    beam->dirsign[2] = (dir[0][2] < 0.0) ? 1 : 0;


    /*
     * Project beam dir onto axis-alied plane.
     */
    {

        ri_vector_t normals[3] = {
            { 1.0, 0.0, 0.0 }, { 0.0, 1.0, 0.0 }, { 0.0, 0.0, 1.0 } };
        ri_vector_t normal;
        ri_float_t  t;
        ri_float_t  k;

        vcpy( normal, normals[beam->dominant_axis] );
        
        if (beam->dirsign[beam->dominant_axis]) {
            vneg( normal );
        }


        for (i = 0; i < 4; i++) {

            t = vdot( dir[i], normal );
            
            if (fabs(t) > RI_EPS) {
                k = beam->d / t;
            } else {
                k = 1.0;
            }

            beam->dir[i][0] = k * dir[i][0];
            beam->dir[i][1] = k * dir[i][1];
            beam->dir[i][2] = k * dir[i][2];
            
        }

    }


    /*
     * Precompute inverse of direction
     */
    for (i = 0; i < 4; i++) {
        beam->invdir[i][0] = safeinv( beam->dir[i][0], RI_EPS, RI_FLT_MAX );
        beam->invdir[i][1] = safeinv( beam->dir[i][1], RI_EPS, RI_FLT_MAX );
        beam->invdir[i][2] = safeinv( beam->dir[i][2], RI_EPS, RI_FLT_MAX );
    }

    /*
     * Precompute normal plane of beam frustum
     */
    vcross( beam->normal[0], beam->dir[1], beam->dir[0] );
    vcross( beam->normal[1], beam->dir[2], beam->dir[1] );
    vcross( beam->normal[2], beam->dir[3], beam->dir[2] );
    vcross( beam->normal[3], beam->dir[0], beam->dir[3] );

    return 0;   /* OK   */
}

static void
create_subbeam(
          ri_beam_t *subbeam,
          point2d_t *p,
          int        is_tetrahedron,
          int        idx0,
          int        idx1,
          int        idx2,
          int        idx3,
    const ri_beam_t *parent)
{

    int axis[3][2] = { {1, 2}, {2, 0}, {0, 1} }; 
    int a0, a1;

    memcpy(subbeam, parent, sizeof(ri_beam_t));

    a0 = axis[parent->dominant_axis][0];
    a1 = axis[parent->dominant_axis][1];

    subbeam->dir[0][a0] = p[idx0][0];
    subbeam->dir[0][a1] = p[idx0][1];
    subbeam->dir[1][a0] = p[idx1][0];
    subbeam->dir[1][a1] = p[idx1][1];
    subbeam->dir[2][a0] = p[idx2][0];
    subbeam->dir[2][a1] = p[idx2][1];
    subbeam->dir[3][a0] = p[idx3][0];
    subbeam->dir[3][a1] = p[idx3][1];

    subbeam->is_tetrahedron = is_tetrahedron;
}


void ri_beam_clip_by_triangle2d(
          ri_beam_t       *outer_out,     /* [out]        */
          ri_beam_t       *inner_out,     /* [out]        */
          int             *nouter_out,    /* [out]        */
          int             *ninner_out,    /* [out]        */
          ri_triangle2d_t *triangle2d,    /* triangle     */
    const ri_beam_t       *beam)
{

    /*
     *
     * Clip beam's poygon by triangle's edge(3 edges).
     *
     * outer poygons : clipped polygon outside of triangle.
     * inner poygons : clipped polygon inside  of triangle.
     *
     * +------------+
     * | beam's vtx | ---- [ clip ] ----->  outer polygons
     * +------------+         |
     *                     [ clip ] ----->  outer polygons
     *                        |
     *                     [ clip ] ----->  outer polygons
     *                        |                             
     *                     inner polygns
     *
     */

    point2d_t outer_polygon[3][8];
    point2d_t inner_polygon[2][16];     /* double-buffering */
    point2d_t *input_p, *inner_p;

    int       i;
    int       idx = 0;
    int       outer_len[3];
    int       inner_len;
    int       len;

    plane2d_t plane[3];

    int axis[3][2] = { {1, 2}, {2, 0}, {0, 1} }; 

    outer_len[0] = 0;
    outer_len[1] = 0;
    outer_len[2] = 0;

    if (beam->is_tetrahedron) {
        len = 3;
    } else {
        len = 4;
    }

    /*
     * beam.dir is positioned at axis-aligned plane, so we don't need 
     * projection.
     */
    input_p = &inner_polygon[0][0];
    for (i = 0; i < len; i++) {
        input_p[i][0] = beam->dir[i][axis[beam->dominant_axis][0]];
        input_p[i][1] = beam->dir[i][axis[beam->dominant_axis][1]];
    }
    

    /*
     * Make clip planes
     */
    plane[0].n[0]   =  (triangle2d->v[1][1] - triangle2d->v[0][1]);
    plane[0].n[1]   = -(triangle2d->v[1][0] - triangle2d->v[0][0]);
    plane[0].p[0]   =  triangle2d->v[0][0];
    plane[0].p[1]   =  triangle2d->v[0][1];

    plane[1].n[0]   =  (triangle2d->v[2][1] - triangle2d->v[1][1]);
    plane[1].n[1]   = -(triangle2d->v[2][0] - triangle2d->v[1][0]);
    plane[1].p[0]   =  triangle2d->v[1][0];
    plane[1].p[1]   =  triangle2d->v[1][1];

    plane[2].n[0]   =  (triangle2d->v[0][1] - triangle2d->v[2][1]);
    plane[2].n[1]   = -(triangle2d->v[0][0] - triangle2d->v[2][0]);
    plane[2].p[0]   =  triangle2d->v[2][0];
    plane[2].p[1]   =  triangle2d->v[2][1];

    idx = 1;

    for (i = 0; i < 3; i++) {

        inner_p      = &inner_polygon[idx][0];   /* output buf   */
        inner_len    = 0;

        outer_len[i] = 0;

        clip( &outer_polygon[i][0], &outer_len[i],
               inner_p, &inner_len,
              (const point2d_t *)input_p, len,
              (const plane2d_t *)&plane[i] );

#if 0
        printf("outer = %d vtx, inner = %d vtx\n", outer_len, inner_len ); 

        // DEBUG
        printf("-- plane pos    %f, %f\n", plane[i].p[0], plane[i].p[1]);
        printf("-- plane normal %f, %f\n", plane[i].n[0], plane[i].n[1]);

        for (j = 0; j < inner_len; j++) {
            printf("[%d] inner[%d] = %f, %f\n", i, j, inner_p[j][0], inner_p[j][1]);
        }

        for (j = 0; j < outer_len; j++) {
            printf("[%d] outer[%d] = %f, %f\n", i, j, outer_p[j][0], outer_p[j][1]);
        }
#endif
        
        /* Inner polygon are further subdivided by the next clip plane */
        input_p    = inner_p;
        len        = inner_len;

        if (inner_len == 0) {
            /* Early exit: beam's vertices and the triangle are separated. */
            break;
        }

        idx = idx ^ 1;      /* flip */
    }

#if 0
    // DEBUG
    for (i = 0; i < inner_len; i++) {
        printf("inner[%d] = %f, %f\n", i, inner_p[i][0], inner_p[i][1]);
    }

    for (i = 0; i < outer_len; i++) {
        printf("outer[%d] = %f, %f\n", i, outer_p[i][0], outer_p[i][1]);
    }
#endif

    /*
     * subdivide outer beam.
     */
    {
        point2d_t pts[4];

        int n = 0;

        for (i = 0; i < 3; i++) {
            
            if (outer_len[i] == 0) continue;

            /*
             * TODO: Employ sophisticated polygon subdivision.
             */
            assert( outer_len[i] < 6 );

            if (outer_len[i] == 5) {

                /* 0, 1, 2 */
                pts[0][0] = outer_polygon[i][0][0];
                pts[0][1] = outer_polygon[i][0][1];

                create_subbeam(  outer_out + n,
                                &outer_polygon[i][0],
                                 1,
                                 0, 1, 2, 2,
                                 beam );
                n++;
                

                /* 2, 3, 4, 0 */
                create_subbeam(  outer_out + n,
                                &outer_polygon[i][0],
                                 0,
                                 2, 3, 4, 0,
                                 beam );
                n++;

            } else if (outer_len[i] == 4) {

                create_subbeam(  outer_out + n,
                                &outer_polygon[i][0],
                                 0,
                                 0, 1, 2, 3, 
                                 beam );
                n++;

            } else {

                create_subbeam(  outer_out + n,
                                &outer_polygon[i][0],
                                 1,
                                 0, 1, 2, 2, 
                                 beam );
                n++;

            }

        }

        (*nouter_out) = n;
    }

    /*
     * subdivide inner beam.
     */
    {
        int n = 0;
        int m;

        if (inner_len > 4) {

            m = inner_len - 3;

            /* create triangle fan */
            for (i = 0; i < m; i++) {

                /* 0, 2i + 1, 2i + 2 */
                create_subbeam(  inner_out + n,
                                 inner_p,
                                 1,
                                 0, 2 * i + 1, 2 * i + 2, 0,
                                 beam );
                n++;
                
            }

        } else if ( inner_len == 4) {

            create_subbeam(  inner_out + n,
                             inner_p,
                             0,
                             0, 1, 2, 3, 
                             beam );
            n++;

        } else if ( inner_len == 3) {

            create_subbeam(  inner_out + n,
                             inner_p,
                             1,
                             0, 1, 2, 2, 
                             beam );
            n++;

        } else {

            /* inner_len = 0 */

        }

        (*ninner_out) = n;

    }

}
