/*
 *
 *                   lucille | Global Illumination Renderer
 *
 *         Copyright 2003-2203 Syoyo Fujita (syoyo@lucillerender.org)
 *
 *
 */

/*
 * Copyright 2003-2203 Syoyo Fujita.  All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the names of the authors nor the names of their contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS
 * OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
 * OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
 * OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
 * EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 */

/* ---------------------------------------------------------------------------
 *
 * Raytracing curve geometry.
 *
 * References
 *
 * [1] Koji Nakamaru and Yoshio Ono
 *     RAY TRACING FOR CURVES PRIMITIVE
 *     WSCG 2002.
 *
 * [2] Tomoyuki Nishita, Thomas W. Sederberg, and Masanori Kakimoto.
 *     Ray tracing trimmed rational surface patches.
 *     Computer Graphics (SIGGRAPH ’90 Proceedings), 24(4):33-345, August 1990.
 *
 * ------------------------------------------------------------------------ */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <assert.h>

#include "curve.h"

#define vdot(a, b) ((a).x * (b).x + (a).y * (b).y + (a).z * (b).z)

static float vlength(point_t p)
{
    return sqrt(p.x * p.x + p.y * p.y + p.z * p.z);
}

static void vnormalize(point_t *p)
{
    float d;
    float d2 = p->x * p->x + p->y * p->y + p->z * p->z;

    if (d2 > 1.0e-6f) {
        d = sqrt(d2);
        p->x /= d; 
        p->y /= d; 
        p->z /= d; 
    } 

}

static ri_curve_stat_t g_stat;


/*
 * Project the curve onto a 2D coordinate system through an orthographic
 * projection along the ray.
 *
 * |   1   0   0  0 | |  lz/d  -lx*ly/d   lx   0 |
 * |   0   1   0  0 | |     0         d   ly   0 |
 * |   0   0   1  0 | | -lx/d  -ly*lz/d   lz   0 |
 * | -ox -oy -oz  1 | |     0         0    0   1 |
 *
 * where
 *
 *   (lx, ly, lz) is the normalized ray direction
 *   (ox, oy, oz) is the ray origin
 *   d = sqrt(lx^2 + lz^2)
 *
 * The rotation matrix should be replaced with the one that rotates +- pi/2
 * around the x-axis if d is zero, where the rotation direction depends on
 * the sign of ly.
 */

/*
 * Bezier curve evaluator for cubic Bezier curve.
 */
void
ri_bezier_curve_eval3(
    point_t           *p,
    ri_bezier_curve_t  c,
    float              t)
{
    float w[4];

    w[0] =        (1.0f - t) * (1.0f - t) * (1.0f - t); 
    w[1] = 3.0f * (1.0f - t) * (1.0f - t) *         t; 
    w[2] = 3.0f * (1.0f - t) *         t  *         t; 
    w[3] =                t  *         t  *         t; 

    p->x = c.P[0].x * w[0] + c.P[1].x * w[1] + c.P[2].x * w[2] + c.P[3].x * w[3];
    p->y = c.P[0].y * w[0] + c.P[1].y * w[1] + c.P[2].y * w[2] + c.P[3].y * w[3];
    p->z = c.P[0].z * w[0] + c.P[1].z * w[1] + c.P[2].z * w[2] + c.P[3].z * w[3];
}

/*
 * Calculate derivative of cubic Bezier curve.
 */
void
ri_bezier_curve_deriv3(
    point_t           *p,
    ri_bezier_curve_t  c,
    float              t)
{
    float   w[3];
    point_t Q[3];

    Q[0].x = c.P[1].x - c.P[0].x;
    Q[0].y = c.P[1].y - c.P[0].y;
    Q[0].z = c.P[1].z - c.P[0].z;
    Q[1].x = c.P[2].x - c.P[1].x;
    Q[1].y = c.P[2].y - c.P[1].y;
    Q[1].z = c.P[2].z - c.P[1].z;
    Q[2].x = c.P[3].x - c.P[2].x;
    Q[2].y = c.P[3].y - c.P[2].y;
    Q[2].z = c.P[3].z - c.P[2].z;

    // w = n * B
    w[0] = 2.0f *        (1.0f - t) * (1.0f - t); 
    w[1] = 2.0f * 2.0f * (1.0f - t) *         t; 
    w[2] = 2.0f *                t  *         t; 

    p->x = Q[0].x * w[0] + Q[1].x * w[1] + Q[2].x * w[2];
    p->y = Q[0].y * w[0] + Q[1].y * w[1] + Q[2].y * w[2];
    p->z = Q[0].z * w[0] + Q[1].z * w[1] + Q[2].z * w[2];
}

/* Distance from O to point onto Bezier curve = v param.
 * Distance = 
 *
 * d = |dP x (P - O)| / |dP|
 *
 * where P is the point on the Bezier curve and dP is the derivative
 * at P.
 */
float
calculate_distance_to_bezier_curve(
    point_t  P,
    point_t dP)
{

    point_t r;
    float   r_norm;
    float   dP_norm;

    r.x = dP.y * P.z - P.y * dP.z;
    r.y = dP.z * P.x - P.z * dP.x;
    r.z = dP.x * P.y - P.x * dP.y;

    r_norm = vlength(r);
    
    dP_norm = vlength(dP);
    if (dP_norm > 1.0e-6f) {
        return r_norm / dP_norm;     
    } else {
        return r_norm;
    }
}

int
ri_bezier_curve_converge(
    ri_curve_intersect_t *isect,
    ri_bezier_curve_t     curve,
    int                   depth,
    float                 v0,
    float                 vn)
{
    int         n;

    const float width = 16.0f;
    const float eps = 1.0e-6f;
    
    float       w;
    float       v;
    float       dist;

    point_t     dP0;
    point_t     dPn;
    point_t     dP;
    point_t     dir;
    point_t     p;

    bbox_t      bbox;

    bbox = curve.bbox;
    n    = curve.ncontrol_points;

    g_stat.nsubdivisions++;

    /* Check if the bounding box overlaps the (ray) square centered at O. */
    if ( (bbox.bmin.z >= isect->t)  || (bbox.bmax.z <= eps) ||
         (bbox.bmin.x >= width) || (bbox.bmax.x <= -width) ||
         (bbox.bmin.y >= width) || (bbox.bmax.y <= -width) ) {

        return 0;

    } else if (depth == 0) {
    
        /* The maximum recursion depth is reached. */

        /* Calculate tangent vectors for P0 and Pn.
         * For Bezier curve, this is easily derived as P[i] - P[i-1].
         */

        dP0.x = curve.P[1].x - curve.P[0].x;
        dP0.y = curve.P[1].y - curve.P[0].y;
        dP0.z = curve.P[1].z - curve.P[0].z;
    
        dPn.x = curve.P[n-1].x - curve.P[n-2].x;
        dPn.y = curve.P[n-1].y - curve.P[n-2].y;
        dPn.z = curve.P[n-1].z - curve.P[n-2].z;

        /* Pn - P0  */ 
        dir.x = curve.P[n-1].x - curve.P[0].x;
        dir.y = curve.P[n-1].y - curve.P[0].y;
        dir.z = curve.P[n-1].z - curve.P[0].z;

        /* dPi, where i is in 0 and n, are reversed if necessary to avoid
         * inflection problem.
         */
        if (vdot(dir, dP0) < 0.0f) {
            dP0.x = -dP0.x;
            dP0.y = -dP0.y;
            dP0.z = -dP0.z;
        }

        if (vdot(dP0, curve.P[0]) >= 0.0f) {
            // printf("dP0 . P[0] >= 0.0f\n"); 
            return 0;
        }

    
        if (vdot(dir, dPn) < 0.0f) {
            dPn.x = -dPn.x;
            dPn.y = -dPn.y;
            dPn.z = -dPn.z;
        }

        if (vdot(dPn, curve.P[n-1]) < 0.0f) {
            // printf("dPn . P[n] < 0.0f\n"); 
            return 0;
        }

        /* Compute w on the line segment    */

        w = dir.x * dir.x + dir.y * dir.y;
        if (fabs(w) < 1.0e-6f) {
            // printf("small w\n"); 
            return 0;
        }

        w = -(curve.P[0].x * dir.x + curve.P[0].y * dir.y) / w;
        if (w < 0.0f) w = 0.0f;
        if (w > 1.0f) w = 1.0f;

        /* Compute v on the curve segment   */
        v = v0 * (1.0f - w) + vn * w;

        /* Compare x-y distances            */
        ri_bezier_curve_eval3(&p, curve, v);

        if (p.x * p.x + p.y * p.y >= (width * width) ||
            p.z <= eps || p.z > isect->t) {
            return 0;
        }

        /* Found a new intersection         */
        isect->t = p.z;
        isect->u = v;

        /* Calculating V coord may require derivative of Bezier curve. */
        ri_bezier_curve_deriv3(
            &dP, curve, v);

        /* d = |dP x (P - O)| / |dP| */
        dist = calculate_distance_to_bezier_curve(p, dP);
        dist = dist / width;
        if (dist > 1.0f) dist = 1.0f;
        if (dist < 0.0f) dist = 0.0f;

        if ( (dP.x * p.y - dP.y * p.x) < 0.0f ) {
            isect->v = 0.5f - 0.5f * dist;
        } else {
            isect->v = 0.5f + 0.5f * dist;
        }

        return 1;

    } else {

        depth--;

        float vm = (v0 + vn) * 0.5f;

        ri_bezier_curve_t cl, cr;
        ri_bezier_curve_split(&cl, &cr, curve);

        return (ri_bezier_curve_converge(isect, cl, depth, v0, vm) ||
                ri_bezier_curve_converge(isect, cr, depth, vm, vn));

    }
}

void
ri_bezier_curve_compute_bbox(
    ri_bezier_curve_t *curve)
{
    int i;
    int n;

    n = curve->ncontrol_points;

    curve->bbox.bmin.x = curve->P[0].x;
    curve->bbox.bmin.y = curve->P[0].y;
    curve->bbox.bmin.z = curve->P[0].z;
    curve->bbox.bmax.x = curve->P[0].x;
    curve->bbox.bmax.y = curve->P[0].y;
    curve->bbox.bmax.z = curve->P[0].z;

    for (i = 1; i < n; i++) {

        if (curve->bbox.bmin.x > curve->P[i].x) curve->bbox.bmin.x = curve->P[i].x;
        if (curve->bbox.bmin.y > curve->P[i].y) curve->bbox.bmin.y = curve->P[i].y;
        if (curve->bbox.bmin.z > curve->P[i].z) curve->bbox.bmin.z = curve->P[i].z;
        
        if (curve->bbox.bmax.x < curve->P[i].x) curve->bbox.bmax.x = curve->P[i].x;
        if (curve->bbox.bmax.y < curve->P[i].y) curve->bbox.bmax.y = curve->P[i].y;
        if (curve->bbox.bmax.z < curve->P[i].z) curve->bbox.bmax.z = curve->P[i].z;

    }
}

/*
 * Split bezier curve 'curve' into two smaller bezier curves 'cl' and 'cr'.
 */
void
ri_bezier_curve_split(
    ri_bezier_curve_t *cl,
    ri_bezier_curve_t *cr,
    ri_bezier_curve_t  curve)
{
    /* Use de Casteljau's Algorithm to split bezier curve into two smaller
     * bezier curves.
     */

    float t = 0.5f;
    point_t p;
    point_t m;

    ri_bezier_curve_eval3(&p, curve, t);

    m.x = t * curve.P[2].x + (1.0f - t) * curve.P[1].x;
    m.y = t * curve.P[2].y + (1.0f - t) * curve.P[1].y;
    m.z = t * curve.P[2].z + (1.0f - t) * curve.P[1].z;
    
    cl->P[0]   = curve.P[0];
    cl->P[3]   = p;

    cr->P[0]   = p;
    cr->P[3]   = curve.P[3];

    /* Conpute new control points */

    cl->P[1].x = t * curve.P[1].x + (1.0f - t) * curve.P[0].x;
    cl->P[1].y = t * curve.P[1].y + (1.0f - t) * curve.P[0].y;
    cl->P[1].z = t * curve.P[1].z + (1.0f - t) * curve.P[0].z;
    cl->P[2].x = t * m.x + (1.0f - t) * cl->P[1].x;
    cl->P[2].y = t * m.y + (1.0f - t) * cl->P[1].y;
    cl->P[2].z = t * m.z + (1.0f - t) * cl->P[1].z;

    cr->P[2].x = t * curve.P[3].x + (1.0f - t) * curve.P[2].x;
    cr->P[2].y = t * curve.P[3].y + (1.0f - t) * curve.P[2].y;
    cr->P[2].z = t * curve.P[3].z + (1.0f - t) * curve.P[2].z;
    cr->P[1].x = t * cr->P[2].x   + (1.0f - t) * m.x;
    cr->P[1].y = t * cr->P[2].y   + (1.0f - t) * m.y;
    cr->P[1].z = t * cr->P[2].z   + (1.0f - t) * m.z;

    cl->ncontrol_points = curve.ncontrol_points;
    cr->ncontrol_points = curve.ncontrol_points;

    ri_bezier_curve_compute_bbox(cl);
    ri_bezier_curve_compute_bbox(cr);

    return;    
}

/*
 * 1/20th of ribbon width for eps is a resonable value.
 */
int
ri_bezier_curve_calculate_max_recursion_depth(
    ri_bezier_curve_t curve,
    float             eps)
{
    int i;
    int n = curve.ncontrol_points;
    int r0;

    float L0 = -1.0f;
    float x_val, y_val;
    float max_val;

    assert(n > 3);

    for (i = 0; i < n - 2; i++) {
        x_val = fabs(curve.P[i].x - 2.0f * curve.P[i+1].x + curve.P[i+2].x);
        y_val = fabs(curve.P[i].y - 2.0f * curve.P[i+1].y + curve.P[i+2].y);
        max_val = x_val > y_val ? x_val : y_val;
        if (L0 < max_val) L0 = max_val;
    }

    r0 = (int)(log(sqrt(2.0) * n * (n-1) * L0 / (8.0 * eps)) / log(4.0));

    return r0;
}

int
ri_bezier_curve_intersect(
    ri_curve_intersect_t *isect_out,
    ri_bezier_curve_t     curve,
    int                   depth)
{
    g_stat.nrays++;

    int ret;

    isect_out->t = 1.0e+30f;
    ret = ri_bezier_curve_converge(isect_out, curve, depth, 0.0f, 1.0f);

    return ret;
}    

void
ri_curve_clear_stat()
{
    memset(&g_stat, 0, sizeof(ri_curve_stat_t));
}

void
ri_curve_show_stat()
{
    double average_subdivisions;

    average_subdivisions = g_stat.nsubdivisions / (double)g_stat.nrays;
    printf("======= Statistics for curve rendering ======================\n");
    printf("  nrays              = %llu\n", g_stat.nrays);
    printf("  subdivisions / ray = %f\n", average_subdivisions);
    printf("=============================================================\n");
}


/*
 * Notes on Bezier curve.
 *
 * Bezier curve X(u) is defined as
 *
 *   X(u) = sum_{i=0}^{n} Bn,i(u) Pi
 *
 * where Bn,i(u) is defined as
 *
 *   Bn,i(u) = (n! / (i! (n-i)!)) u^i (1 - u)^(n-i)
 *
 * and Pi are control points.
 *
 * Derivative of Bezier curve X'(u) is defined as
 *
 *   X'(u) = sum_{i=0}^{n-1} B_{n-1, i}(u) * n * (P_{i+1} - Pi)
 */
