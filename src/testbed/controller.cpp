#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "render.h"
#include "scene.h"
#include "geom.h"
#include "bvh.h"

#include <png.h>

#include <FL/Fl.H>
#include "fluid_gui.h"
#include "glm.h"
#include "defines.h"
#include "simplerender.h"

GLMmodel   *gobj;
ri_scene_t *gscene;
int         gvisualizeMode = VISUALIZE_IMAGE;
ri_texture_t *giblmap;
ri_texture_t *glatlongmap;
int           giblsamples;
int           gnsubsamples;

int              gdebugpixel;
int              gdebugpixel_x;
int              gdebugpixel_y;

int              gprogressiveMode = 0;

Fl_Menu_Item visualizeMenu[] = {
    { "Image"      , 0, 0, (void *)VISUALIZE_IMAGE          , 0, 0, 0, 0, 0},
    { "# of travs" , 0, 0, (void *)VISUALIZE_NUM_TRAVERSALS , 0, 0, 0, 0, 0},
    { "# of isects", 0, 0, (void *)VISUALIZE_NUM_ISECTS     , 0, 0, 0, 0, 0},
    { 0            , 0, 0, 0                                , 0, 0, 0, 0, 0}
};

void
setup_param_gui()
{
    paramVisualizeChoice->menu( visualizeMenu );

}

//
// GUI callbacks
//
void
visualize_choice_cb(Fl_Choice *w, void *arg)
{
    (void)arg;

    // switch ( (int)(w->mvalue()->user_data()) ) {
    // case VISUALIZE_IMAGE:
    // case VISUALIZE_NUM_TRAVERSALS:
    // case VISUALIZE_NUM_ISECTS:
    // }
  
    gvisualizeMode = (int)(w->mvalue()->user_data()); 

}

static void
savePNGFile(
    const char *pngfilename,
    unsigned char *img,
    int width,
    int height)
{
	int i;

	FILE *fp;
	png_structp png_ptr;
	png_infop info_ptr;
	//const char *filename;
	png_bytep *rowp;

	fp = fopen(pngfilename, "wb");
	if (!fp) {
		fprintf(stderr, "Cannot save the file.\n");
		return;
	}

	rowp = (png_bytep *)malloc(sizeof(png_bytep *) * height);
	assert(rowp);

	for (i = 0; i < height; i++) {
		rowp[i] = (png_bytep)&img[3 * ((height - i - 1) * width)];
		
	}

	png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING,
					  NULL, NULL, NULL);
	assert(png_ptr);
	info_ptr = png_create_info_struct(png_ptr);
	assert(info_ptr);

	png_init_io(png_ptr, fp);
	png_set_IHDR(png_ptr, info_ptr, width, height, 8, PNG_COLOR_TYPE_RGB,
		     PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_DEFAULT,
		     PNG_FILTER_TYPE_DEFAULT);
	png_write_info(png_ptr, info_ptr);
	png_write_image(png_ptr, rowp);
	png_write_end(png_ptr, info_ptr);
	png_destroy_write_struct(&png_ptr, &info_ptr);

	free(rowp);

	fclose(fp);

	printf("Image saved to [ %s ].\n", pngfilename );
}

void save_image_png_cb(Fl_Menu_*w, void *arg)
{
    (void)w;
    (void)arg;

    savePNGFile( "output.png",
                 guiGLView->image,
                 guiGLView->imageWidth,
                 guiGLView->imageHeight );

}


void get_grpname(char *dst, char *str)
{
    int len;

    // skip whitespace
    while ( ((*str) == '\t') || ((*str) == ' ') ) {
        str++;
    }

    len = strlen(str);

    if (str[len - 1] == '\n') { // LF
        str[len - 1] = '\0';
    } else if (str[len - 1] == '\r') {  // CR
        str[len - 1] = '\0';
    } else if ((len > 2) && str[len - 2] == '\r') { // CR + LF
        str[len - 2] = '\0';
    }

    strcpy(dst, str);
}

ri_geom_t *
parse_geom(
    const GLMgroup *grp,
    const GLMmodel *model)
{
    /*
     * do right hand -> left hand conversion
     * 1. z = -z
     * 2. index = reverse
     * 3. normal = -normal
     */
    unsigned int i;
    unsigned int tid;
    unsigned int id[3];

    float        v[3];

    ri_geom_t   *geom;


    ri_vector_t    *vertices;
    ri_vector_t    *normals;
    unsigned int   *indices;

    geom     = ri_geom_new();
    vertices = (ri_vector_t *)ri_mem_alloc(sizeof(ri_vector_t) * grp->numtriangles * 3);
    normals = (ri_vector_t *)ri_mem_alloc(sizeof(ri_vector_t) * grp->numtriangles * 3);
    indices  = (unsigned int *)ri_mem_alloc(sizeof(unsigned int ) * grp->numtriangles * 3);

    for (i = 0; i < grp->numtriangles; i++) {
        tid = grp->triangles[i];

        id[0] = model->triangles[tid].vindices[0];
        id[1] = model->triangles[tid].vindices[1];
        id[2] = model->triangles[tid].vindices[2];

        indices[ 3 * i + 0 ] = 3 * i + 0;
        indices[ 3 * i + 1 ] = 3 * i + 1;
        indices[ 3 * i + 2 ] = 3 * i + 2;

        v[0] = model->vertices[3 * id[0] + 0];
        v[1] = model->vertices[3 * id[0] + 1];
        v[2] = model->vertices[3 * id[0] + 2];

        vertices[3 * i + 0][0] = v[0];
        vertices[3 * i + 0][1] = v[1];
        vertices[3 * i + 0][2] = v[2];

        v[0] = model->vertices[3 * id[1] + 0];
        v[1] = model->vertices[3 * id[1] + 1];
        v[2] = model->vertices[3 * id[1] + 2];

        vertices[3 * i + 1][0] = v[0];
        vertices[3 * i + 1][1] = v[1];
        vertices[3 * i + 1][2] = v[2];

        v[0] = model->vertices[3 * id[2] + 0];
        v[1] = model->vertices[3 * id[2] + 1];
        v[2] = model->vertices[3 * id[2] + 2];

        vertices[3 * i + 2][0] = v[0];
        vertices[3 * i + 2][1] = v[1];
        vertices[3 * i + 2][2] = v[2];

    }

    ri_geom_add_positions( geom, grp->numtriangles * 3, vertices );
    ri_geom_add_indices( geom, grp->numtriangles * 3, indices );

    if (model->numnormals) {

        //assert(model->numvertices == model->numnormals);

        for (i = 0; i < grp->numtriangles; i++) {
            tid = grp->triangles[i];

            id[0] = model->triangles[tid].nindices[0];
            id[1] = model->triangles[tid].nindices[1];
            id[2] = model->triangles[tid].nindices[2];

            indices[ 3 * i + 0 ] = 3 * i + 0;
            indices[ 3 * i + 1 ] = 3 * i + 1;
            indices[ 3 * i + 2 ] = 3 * i + 2;

            v[0] = model->normals[3 * id[0] + 0];
            v[1] = model->normals[3 * id[0] + 1];
            v[2] = model->normals[3 * id[0] + 2];

            normals[3 * i + 0][0] = v[0];
            normals[3 * i + 0][1] = v[1];
            normals[3 * i + 0][2] = v[2];

            v[0] = model->normals[3 * id[1] + 0];
            v[1] = model->normals[3 * id[1] + 1];
            v[2] = model->normals[3 * id[1] + 2];

            normals[3 * i + 1][0] = v[0];
            normals[3 * i + 1][1] = v[1];
            normals[3 * i + 1][2] = v[2];

            v[0] = model->normals[3 * id[2] + 0];
            v[1] = model->normals[3 * id[2] + 1];
            v[2] = model->normals[3 * id[2] + 2];

            normals[3 * i + 2][0] = v[0];
            normals[3 * i + 2][1] = v[1];
            normals[3 * i + 2][2] = v[2];

        }

        ri_geom_add_normals( geom, grp->numtriangles * 3, normals );
    }

    // ri_mem_free( vertices );
    // ri_mem_free( indices );

    return geom;

}

int
scene_add_obj(
    ri_scene_t     *scene,     // [inout]
    const GLMmodel *model)
{

    const GLMgroup *grp;
    char buf[1024];

    int i = 0;  // triangle index

    // max_ntriangles = calc_ntriangles(model);
    // scene->triangles  = (triangle4_t *)malloc(sizeof(triangle4_t) * max_ntriangles);
    // scene->ntriangles = max_ntriangles;

    grp = model->groups;
    
    ri_geom_t *geom;

    i = 0;
    while (grp != NULL) {
        if (!grp) break;
        if (grp->numtriangles == 0) {
            grp = grp->next;
            continue;
        }

        get_grpname(buf, grp->name);
        printf("[scene] Adding group: %s\n", buf);

        geom = parse_geom(grp, model);

        ri_scene_add_geom( scene, geom );

        grp = grp->next;
    }
    
    return 0;
}

void
render(
    float *image,
    int    width,
    int    height,
    vec    eye,
    vec    lookat,
    vec    up)
{

    int beam_size = 64;

    simple_render_progressive( (ri_bvh_t *)gscene->accel->data, image, width, height, eye, lookat, up, 1 );
    //simple_render_ibl( (ri_bvh_t *)gscene->accel->data, image, width, height, eye, lookat, up );
    //simple_render_beam( (ri_bvh_t *)gscene->accel->data, image, width, height, beam_size, eye, lookat, up );

}
