#ifndef CONTROLLER_H
#define CONTROLLER_H

#include "glm.h"

#include "scene.h"

GLMmodel *gobj;
ri_scene_t *gscene;

extern int scene_add_obj( ri_scene_t     *scene,     // [inout]
                           const GLMmodel *model);

#endif
