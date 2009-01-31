#ifndef LUCILLE_CURVE_H
#define LUCILLE_CURVE_H

#ifdef __cplusplus
extern "C" {
#endif

#define CURVE_MAX_CONTROL_POINTS 4

typedef struct _point_t
{
    float   x, y, z, w;
} point_t;

typedef struct _bbox_t
{
    point_t bmin;
    point_t bmax;
} bbox_t;

typedef struct _ri_bezier_curve_t
{
    bbox_t  bbox;

    point_t P[4 * CURVE_MAX_CONTROL_POINTS];            /* control points   */
    int     ncontrol_points;

} ri_bezier_curve_t;

/* Return 1 if the intersection point was found, 0 if not. */
extern int ri_curve_converge(
    ri_bezier_curve_t curve,
    int               depth,
    float            *t,
    float             v0,
    float             vn);

extern void ri_bezier_curve_eval3(
    point_t           *p,
    ri_bezier_curve_t  c,
    float              t);

extern void ri_bezier_curve_split(
    ri_bezier_curve_t *cl,
    ri_bezier_curve_t *cr,
    ri_bezier_curve_t  curve);

extern int  ri_bezier_curve_calculate_max_recursion_depth(
    ri_bezier_curve_t curve,
    float             eps);

extern void ri_bezier_curve_compute_bbox(
    ri_bezier_curve_t *curve);

#ifdef __cplusplus
}
#endif

#endif  /* LUCILLE_CURVE_H */
