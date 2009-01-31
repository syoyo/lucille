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
#include <math.h>
#include <assert.h>

#include "curve.h"

#define vdot(a, b) ((a).x * (b).x + (a).y * (b).y + (a).z * (b).z)

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

int
ri_curve_converge(
    ri_bezier_curve_t curve,
    int               depth,
    float            *t,
    float             v0,
    float             vn)
{
    int         n;

    const float width = 4.0f;
    const float eps = 1.0e-6f;
    
    float       w;
    float       v;

    point_t     dP0;
    point_t     dPn;
    point_t     dir;
    point_t     p;

    bbox_t      bbox;

    bbox = curve.bbox;
    n    = curve.ncontrol_points;

    /* Check if the bounding box overlaps the (ray) square centered at O. */
    if ( (bbox.bmin.z >= (*t))  || (bbox.bmax.z <= eps) ||
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
            p.z <= eps || p.z > (*t)) {
            return 0;
        }

        /* Found a new intersection         */
        (*t) = p.z;
        
        return 1;

    } else {

        depth--;

        float vm = (v0 + vn) * 0.5f;

        ri_bezier_curve_t cl, cr;
        ri_bezier_curve_split(&cl, &cr, curve);

        return (ri_curve_converge(cl, depth, t, v0, vm) ||
                ri_curve_converge(cr, depth, t, vm, vn));

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

