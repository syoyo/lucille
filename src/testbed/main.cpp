#include <stdio.h>
#include <stdlib.h>

#include "fluid_gui.h"
#include "glm.h"
#include "controller.h"

#include "bvh.h"
#include "render.h"


GLMmodel *
loadObj(const char *name)
{
    GLMmodel *model = NULL;

    printf("[obj] Loading obj ...\n"); fflush(stdout);

    model = glmReadOBJ((char *)name);

    if (!model) {
        printf("[obj] couldn't open [ %s ] \n", name);
        return NULL;
    }

    glmFacetNormals(model);
    glmVertexNormals(model, 90.0);

    printf("[obj] Load obj OK!\n");
    printf("[obj] ntriangles = %d\n", model->numtriangles);

    return model;
}

void
init(const char *objname)
{
    //
    // Parse .obj and convert geometry data for lucille internal geom format.
    //
    gobj = loadObj(objname);
    if (gobj == NULL) {
        printf("failed to open: %s\n", objname);
        exit(1);
    }

    ri_render_init();

    gscene = ri_scene_new();
    scene_add_obj( gscene, gobj );

    //
    // Bind acclerator with BVH acceleration structure.
    //
    ri_accel_t *accel = ri_accel_new();
    ri_accel_bind( accel, RI_ACCEL_BVH );

    ri_scene_set_accel( gscene, accel );
    ri_scene_build_accel( gscene );

    guiGLView->setObj(gobj);

    //
    // For visualization of BVH
    // 
    guiGLView->bvhVisualizer->setBVH( (ri_bvh_t *)gscene->accel->data );

}

int
main(int argc, char **argv)
{
    float scale[3];
    float maxscale;

    const char *defname = "cornellbox.obj";
    const char *filename;

    filename = defname;
    if (argc > 1) {
        // printf("testbed <model.obj>\n");
        // exit(1);
        filename = argv[1];
    }


    Fl_Double_Window *mainWindow  = make_window();
    Fl_Double_Window *paramWindow = make_param_window();
    setup_param_gui();

    paramWindow->position( mainWindow->x() + mainWindow->w() + 15,
                           mainWindow->y() );

    init(filename);

    glmDimensions( gobj, scale );
    maxscale = scale[0];
    if (scale[0] < scale[1]) {
        maxscale = scale[1];
    }
    if (maxscale < scale[2]) {
        maxscale = scale[2];
    }

    guiGLView->viewOrg[2] = -2.0 * maxscale;
    guiGLView->sceneScale = -2.0 * maxscale;

    paramWindow->show();
    mainWindow->show();


    return Fl::run();
}
