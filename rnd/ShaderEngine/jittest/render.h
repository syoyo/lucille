#ifndef RENDER_H
#define RENDER_H

typedef struct _sphere_t
{
    float center[3];
    float radius;
} sphere_t;

typedef struct _isect_t
{
    float p[4];
    float n[4];
    float t;
} isect_t;

int sphere_isect(isect_t *isect, const sphere_t *sphere, float *org, float *dir);

#endif  /* RENDER_H */
