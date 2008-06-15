#ifndef CONTROLLER_H
#define CONTROLLER_H

#include "glm.h"

#include "scene.h"

extern GLMmodel *gobj;
extern ri_scene_t *gscene;

extern int scene_add_obj( ri_scene_t     *scene,     // [inout]
                          const GLMmodel *model);

extern void render( float *image,
                    int    width,
                    int    height,
                    vec    eye,
                    vec    lookat,
                    vec    up );

#endif
