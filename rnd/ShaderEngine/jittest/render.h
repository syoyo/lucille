#ifndef RENDER_H
#define RENDER_H

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _vec
{
    float x;
    float y;
    float z;
    float w;
} vec;

typedef struct _sphere_t
{
    vec   center;
    float radius;
} sphere_t;

typedef struct _plane_t
{
    vec  p;
    vec  n;

} plane_t;

typedef struct _isect_t
{
    vec   p;
    vec   n;
    float t;
} isect_t;


typedef struct _ray_t
{
    vec    org;
    vec    dir;
} ray_t;

extern sphere_t scene_spheres[3];
//extern plane_t  plane;

extern void init_render_scene();
extern int ray_sphere_intersect(isect_t *isect, const ray_t *ray, const sphere_t *sphere);
extern int ray_plane_intersect(isect_t *isect, const ray_t *ray, const plane_t *plane);

#ifdef __cplusplus
}
#endif

#endif  /* RENDER_H */
