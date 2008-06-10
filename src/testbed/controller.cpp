#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "render.h"
#include "scene.h"
#include "geom.h"
#include "bvh.h"

#include "glm.h"

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

    int          idx;

    float        v[3];

    ri_geom_t   *geom;


    ri_vector_t    *vertices;
    unsigned int   *indices;

    geom     = ri_geom_new();
    vertices = (ri_vector_t *)ri_mem_alloc(sizeof(ri_vector_t) * grp->numtriangles * 3);
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
