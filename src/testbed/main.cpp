#include <stdio.h>
#include <stdlib.h>

#include "fluid_gui.h"
#include "glm.h"
#include "controller.h"

#include "bvh.h"
#include "render.h"
#include "texture.h"
#include "IBLSampler.h"
#include "image_saver.h"

// set from conf file.
char iblmapname[1024];
char objname[1024];

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
    glmVertexNormals(model, 80.0);

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

void
load_conf()
{
    const char *confname = "conf.dat";

    FILE *fp;

    fp = fopen(confname, "r");
    if (!fp) {
        printf("can't open file [ %s ]\n", confname);
        exit(1);
    }

    fscanf(fp, "%s\n", objname);
    fscanf(fp, "%s\n", iblmapname);
    fscanf(fp, "%d\n", &giblsamples);
    fscanf(fp, "%d\n", &gnsubsamples);

    printf("CONF: objname     = %s\n", objname);
    printf("CONF: iblname     = %s\n", iblmapname);
    printf("CONF: iblsamples  = %d\n", giblsamples);
    printf("CONF: nsubsamples = %d\n", gnsubsamples);

    fclose(fp);

}

void
test_sat()
{
    ri_texture_t  tex;
    ri_sat_t     *sat;
    int s = 4;
    int i, j;

    tex.data = (float *)malloc(sizeof(float) * s * s * 3);
    tex.width     = s;
    tex.height    = s;

    for (i = 0; i < 3 * s * s; i++) {
        tex.data[i] = 1.0f;
    }

    sat = ri_texture_make_sat(&tex);

    for (j = 0; j < s; j++) {
        for (i = 0; i < s; i++) {
            printf("[%d][%d] = %f\n", j, i, sat->data[3 * (j * s + i)]);
        }
    }
}

int
main(int argc, char **argv)
{
    float scale[3];
    float maxscale;

    const char *filename;

    test_sat();
    load_conf();

    //ri_intersection_state_t *state;
    //sample_ibl(iblmap, state);

    // exit(0);

    giblmap = ri_texture_load(iblmapname);
    if (giblmap == NULL) {
        printf("can't load map [ %s ] \n", iblmapname);
        exit(0);
    } 

    glatlongmap = ri_texture_make_longlat_from_angularmap(
        giblmap, 128, 128);

    filename = objname;
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

    guiGLView->viewOrg[2] = 1.0 * maxscale;
    guiGLView->saveCurrentViewAsDefaultView();

    guiGLView->sceneScale = 1.0 * maxscale;

    paramWindow->show();
    mainWindow->show();


    return Fl::run();
}
