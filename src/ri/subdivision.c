/*
 * Subdivision surface
 *
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "memory.h"
#include "vector.h"
#include "geometric.h"
#include "ri.h"
#include "log.h"
#include "apitable.h"
#include "attribute.h"
#include "geom.h"
#include "render.h"
#include "subdivision.h"

static void read_from_RIB(ri_array_t *vertout, ri_array_t *faceout,
              int nfaces, int indices[], void *vertices, void *sts);

static void calc_vertex_normal(ri_vector_t   *normals,    /* output */
                   ri_vector_t   *vertices,
                       int            nvertices,    
                   unsigned int  *indices,
                       int            nindices);

void
ri_api_subdivision_mesh(RtToken scheme,
                RtInt nfaces, RtInt nvertices[], RtInt vertices[],
                RtInt ntags, RtToken tags[],
                RtInt nargs[], RtInt intargs[], RtFloat floatargs[],
                RtInt n, RtToken tokens[], RtPointer params[])
{
    int        level;
    int        i;

    RtPointer  p_param = NULL;
    RtPointer  st_param = NULL;
    ri_subd_t    *mesh;            /* input mesh */
    ri_subd_t    *subd[MAXSUBDIVLEVEL];    /* Subdivision mesh. */
    ri_subd_face_t    *fp;
    ri_subd_vertex_t  *vp;

    ri_context_t   *ctx;
    ri_attribute_t *attr;
    ri_geom_t      *geom;
    int             rh;
    unsigned int    offset;
    unsigned int    nv;
    unsigned int   *indices;
    unsigned int    nindices;
    ri_matrix_t    *m;
    ri_matrix_t     om;
    ri_matrix_t     orientation;
    int             two_sided;
    ri_vector_t    *vlists;
    ri_vector_t    *nlists;
    ri_vector_t     v;
    unsigned int    npoints;

    (void)ntags;
    (void)tags;
    (void)nargs;
    (void)intargs;
    (void)floatargs;
    
    if (strcmp(scheme, "catmull-clark") != 0) {
        ri_log(LOG_WARN, "Currently supports only Catmull-Clark subdivision scheme");
        return;
    }

    for (i = 0; i < nfaces; i++) {
        if (nvertices[i] != 4) {
            ri_log(LOG_WARN, "Currently supports only quad faces");
            return;
        }
    }

    for (i = 0; i < n; i++) {
        if (strcmp(tokens[i], RI_P) == 0) {
            p_param = params[i];
        } else if (strcmp(tokens[i], RI_ST) == 0) {
            st_param = params[i];
        }
    }

    if (!p_param) {
        ri_log(LOG_WARN, "no RI_P in input");
        return;
    }

    mesh = ri_subd_new();
    read_from_RIB(mesh->vertex, mesh->face, nfaces, vertices,
              p_param, st_param);

    /* refine! */
    for (i = 0; i < MAXSUBDIVLEVEL; i++) {
        subd[i] = ri_subd_new();
        if (i == 0) {
            ri_subd_subdivide(subd[i], mesh);
            ri_subd_delete(mesh);
        } else {
            ri_subd_subdivide(subd[i], subd[i - 1]);
            ri_subd_delete(subd[i - 1]);
        }
    }

    level = MAXSUBDIVLEVEL - 1;

    ctx = ri_render_get()->context;
    attr = (ri_attribute_t *)ri_stack_get(ctx->attr_stack);

    if (attr->sides == 2) two_sided = 1;
    else              two_sided = 0;

    /* TODO: Implement two sided face. */

    /* Get modelview matrix. */
    m = (ri_matrix_t *)ri_stack_get(ctx->trans_stack);

    /* Build orientation matrix. */
    ri_matrix_identity(&orientation);

    if (strcmp(ri_render_get()->context->option->orientation, RI_RH) == 0) {
        rh = 1;
    } else {
        rh = 0;
    }

    if (rh) {
        orientation.f[2][2] = -orientation.f[2][2];
    }

    /* om = orientation . modelview */
    ri_matrix_mul(&om, m, &orientation);

    if (two_sided) {
        nindices = subd[level]->face->nelems * 6 * 2;
    } else {
        nindices = subd[level]->face->nelems * 6;
    }

    indices = (unsigned int *)ri_mem_alloc(sizeof(unsigned int) * nindices);
    
    for (i = 0; i < (int)subd[level]->face->nelems; i++) {
        fp = (ri_subd_face_t *)ri_array_at(subd[level]->face, i);

        if (rh) {
            indices[6 * i + 0] = fp->v_id[2];    
            indices[6 * i + 1] = fp->v_id[1];    
            indices[6 * i + 2] = fp->v_id[0];    
            indices[6 * i + 3] = fp->v_id[3];    
            indices[6 * i + 4] = fp->v_id[2];    
            indices[6 * i + 5] = fp->v_id[0];    

            if (two_sided) {
                offset = subd[level]->face->nelems;
                nv     = subd[level]->vertex->nelems;

                indices[6 * (i+offset) + 0] = fp->v_id[0] + nv;    
                indices[6 * (i+offset) + 1] = fp->v_id[1] + nv;    
                indices[6 * (i+offset) + 2] = fp->v_id[2] + nv;    
                indices[6 * (i+offset) + 3] = fp->v_id[0] + nv;
                indices[6 * (i+offset) + 4] = fp->v_id[2] + nv;    
                indices[6 * (i+offset) + 5] = fp->v_id[3] + nv;    
            }
        } else {
            indices[6 * i + 0] =  fp->v_id[0];    
            indices[6 * i + 1] =  fp->v_id[1];    
            indices[6 * i + 2] =  fp->v_id[2];    
            indices[6 * i + 3] =  fp->v_id[0];    
            indices[6 * i + 4] =  fp->v_id[2];    
            indices[6 * i + 5] =  fp->v_id[3];    

            if (two_sided) {
                offset = subd[level]->face->nelems;
                nv     = subd[level]->vertex->nelems;

                indices[6 * (i+offset) + 0] = fp->v_id[2] + nv;    
                indices[6 * (i+offset) + 1] = fp->v_id[1] + nv;    
                indices[6 * (i+offset) + 2] = fp->v_id[0] + nv;    
                indices[6 * (i+offset) + 3] = fp->v_id[3] + nv;    
                indices[6 * (i+offset) + 4] = fp->v_id[2] + nv;    
                indices[6 * (i+offset) + 5] = fp->v_id[0] + nv;    
            }
        }
    } 

    if (two_sided) {
        npoints = subd[level]->vertex->nelems * 2;
    } else {
        npoints = subd[level]->vertex->nelems;
    }

    vlists = (ri_vector_t *)ri_mem_alloc(sizeof(ri_vector_t) * npoints);

    for (i = 0; i < (int)subd[level]->vertex->nelems; i++) {
        vp = (ri_subd_vertex_t *)ri_array_at(subd[level]->vertex, i);
        v[0] = (ri_float_t)vp->p[0];
        v[1] = (ri_float_t)vp->p[1];
        v[2] = (ri_float_t)vp->p[2];
        v[3] = 1.0;

        /* object space to world space. */
        ri_vector_transform(vlists[i], v, &om);

        if (two_sided) {
            offset = subd[level]->vertex->nelems;
            ri_vector_transform(vlists[i + offset], v, &om);
        }
    }

    nlists = (ri_vector_t *)ri_mem_alloc(sizeof(ri_vector_t) * npoints);

    calc_vertex_normal(nlists, vlists, npoints, indices, nindices);

    geom = ri_geom_new();
    
    ri_geom_add_positions(geom, npoints, (const ri_vector_t *)vlists);
    ri_geom_add_normals(geom, npoints, (const ri_vector_t *)nlists);
    ri_geom_add_indices(geom, nindices, indices);

    if (attr->surface) {
        geom->shadername = strdup(attr->surface);
    }

    if (attr->shader) {
        geom->shader = ri_shader_dup(attr->shader);
    }

    if (attr->material) {
        if (!geom->material) geom->material = ri_material_new();
        ri_material_copy(geom->material, attr->material);
    }

    geom->two_side = two_sided;

    ri_scene_add_geom(ri_render_get()->scene, geom);

    ri_subd_delete(subd[level]);

    ri_mem_free(indices);
    ri_mem_free(vlists);
    ri_mem_free(nlists);
}
              

/* ===========================================================================
 *
 * Private functions
 *
 * ======================================================================== */
static void
read_from_RIB(ri_array_t *vertout, ri_array_t *faceout,
          int nfaces, int indices[], void *vertices, void *sts )
{
    int      i;
    ri_subd_vertex_t v;
    ri_subd_face_t   f;

    int nvertices = 0;

    /* Assume quad polygon as input. */
    for (i = 0; i < nfaces * 4; i++) {
        if (nvertices < indices[i]) nvertices = indices[i];
    }

    /* because vertex index is zero-base in RIB. */
    nvertices++;

    for (i = 0; i < nvertices; i++) {
        v.p[0] = (double)((float *)vertices)[3 * i + 0];
        v.p[1] = (double)((float *)vertices)[3 * i + 1];
        v.p[2] = (double)((float *)vertices)[3 * i + 2];

        if (sts) {        /* Mesh has texture coords */
            v.st[0] = (double)((float *)sts)[2 * i + 0];
            v.st[1] = (double)((float *)sts)[2 * i + 1];
        }

        ri_array_insert(vertout, vertout->nelems, &v);

    }

    for (i = 0; i < nfaces; i++) {
        f.v_id[0] = indices[4 * i + 0];     
        f.v_id[1] = indices[4 * i + 1];     
        f.v_id[2] = indices[4 * i + 2];     
        f.v_id[3] = indices[4 * i + 3]; 

        ri_array_insert(faceout, faceout->nelems, &f);
    }
}

static void
calc_vertex_normal(ri_vector_t  *normals,
           ri_vector_t  *vertices,
           int nvertices,
           unsigned int *indices,
           int nindices)
{
    int     i;
    unsigned int i0, i1, i2;  
    ri_vector_t v0, v1, v2;
    ri_vector_t v01, v02;
    ri_vector_t normal;
    double  area;

    /* calculate vertex normal from polygon mesh.
     * a vertex normal is an aaverage of the face vectors with
     * area weighting.
      */    

    for (i = 0; i < nvertices; i++) {
        ri_vector_setzero(normals[i]);
    }

    for (i = 0; i < nindices / 3; i++) {
        ri_vector_setzero(normal);

        i0 = indices[3 * i + 0];
        i1 = indices[3 * i + 1];
        i2 = indices[3 * i + 2];

        ri_vector_copy(v0, vertices[i0]);
        ri_vector_copy(v1, vertices[i1]);
        ri_vector_copy(v2, vertices[i2]);

        ri_vector_sub(v01, v1, v0);
        ri_vector_sub(v02, v2, v0);
        ri_vector_cross(normal, v01, v02); 
        ri_vector_normalize(normal);

        area = ri_area(v0, v1, v2);

        /* weight by the area of the face. */
        //ri_vector_scale(&normal, (float)area);
        ri_vector_add(normals[i0], normals[i0], normal);
        ri_vector_add(normals[i1], normals[i1], normal);
        ri_vector_add(normals[i2], normals[i2], normal);
    }

    for (i = 0; i < nvertices; i++) {
        ri_vector_normalize(normals[i]);
    }
}

