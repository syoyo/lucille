/*
 * Polygon parser.
 *
 * $Id: polygon.c,v 1.9 2004/06/13 06:44:51 syoyo Exp $
 *
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "vector.h"
#include "geometric.h"
#include "memory.h"
#include "polygon.h"
#include "render.h"
#include "context.h"
#include "attribute.h"
#include "render.h"
#include "apitable.h"
#include "log.h"
#include "array.h"
#include "accel.h"
#include "reflection.h"
#include "geom.h"

static void gen_basis(ri_geom_t *geom);

ri_geom_t *
ri_polygon_parse(RtInt nverts, RtInt n, RtToken tokens[], RtPointer params[])
{
    /* two sided polygon handling bug fix by Duraid Madina Aug 6, 2003 */
    int             i, j;
    int             rh;
    ri_context_t   *ctx;
    ri_attribute_t *attr;
    ri_geom_t      *p;
    RtFloat        *param;
    RtFloat        *texcoords;
    ri_vector_t    *positions;
    ri_vector_t    *normals;
    ri_vector_t    *colors = NULL;
    ri_vector_t    *opacities;
    ri_vector_t     refcol;
    ri_vector_t     v;
    ri_matrix_t    *m;
    ri_matrix_t     om;
    ri_matrix_t     orientation;
    ri_matrix_t     itm;
    unsigned int   *indices;
    unsigned int    nindices;
    int        two_sided;
    int             has_color = 0;
    int             nv;

    p = ri_geom_new();

    if (strcmp(ri_render_get()->context->option->orientation, RI_RH) == 0) {
        rh = 1;
    } else {
        rh = 0;
    }

    ctx = ri_render_get()->context;
    attr = (ri_attribute_t *)ri_stack_get(ctx->attr_stack);

    /* is this a two sided polygon? */
    if( (attr->sides) == 2 )
        two_sided=1;
    else
        two_sided=0;

    /* get modelview matrix */
    m = (ri_matrix_t *)ri_stack_get(ctx->trans_stack);

    /* build orientation matrix */
    ri_matrix_identity(&orientation);
    if (rh) {
        orientation.f[2][2] = -orientation.f[2][2];
    }

    /* om = orientation . modelview */
    ri_matrix_mul(&om, m, &orientation);

    for (i = 0; i < n; i++) {

        param = (RtFloat *)params[i];

        if (strcmp(tokens[i], RI_P) == 0) { /* position */
            if(!two_sided) {
                positions = (ri_vector_t *)ri_mem_alloc(
                        sizeof(ri_vector_t) * nverts);
            } else { /* it is two sided... */
                positions = (ri_vector_t *)ri_mem_alloc(
                        sizeof(ri_vector_t) * nverts * 2);
            }
            for (j = 0; j < nverts; j++) {
                /*
                 * transform vector from object space
                 * to world space
                 */
                ri_vector_set_from_rman(v, &param[3 * j]);
                ri_vector_transform(positions[j], v, &om);
                if(two_sided) {
                    ri_vector_copy(positions[nverts+j],
                                   positions[j]);
                }
            }

            if(!two_sided) {
                ri_geom_add_positions(p,
                                      nverts,
                                      (const ri_vector_t *)positions);
            } else {
                ri_geom_add_positions(p,
                                      nverts * 2,
                                      (const ri_vector_t *)positions);
            }

            
        } else if (strcmp(tokens[i], RI_N) == 0) { /* normal */
            if(!two_sided) {
                normals = (ri_vector_t *)ri_mem_alloc(
                        sizeof(ri_vector_t) * nverts);
            } else {
                normals = (ri_vector_t *)ri_mem_alloc(
                        sizeof(ri_vector_t) * nverts * 2);
            }
            
            /*
             *       -T
             * N' = M   N
             *
             * calculate inverse transpose of the modelview 
             * matrix for normal vector transformation.
             */

            ri_matrix_copy(&itm, &om);

            /* 
             * we need only upper left 3x3 elements, so clear
             * other elements
             */

            itm.f[0][3] = 0.0;
            itm.f[1][3] = 0.0;
            itm.f[2][3] = 0.0;
            itm.f[3][0] = 0.0;
            itm.f[3][1] = 0.0;
            itm.f[3][2] = 0.0;
            itm.f[3][3] = 1.0;
            
            ri_matrix_inverse(&itm);
            ri_matrix_transpose(&itm);

#ifdef DEBUG
            if (j == 0) ri_matrix_print(&itm);
#endif

            for (j = 0; j < nverts; j++) {
                ri_vector_set_from_rman(v, &param[3 * j]);
                ri_vector_transform(normals[j], v, &itm);
                ri_vector_normalize(normals[j]);
                if(two_sided) {
                    ri_vector_copy(normals[nverts+j],
                                   normals[j]);
                    ri_vector_neg(normals[nverts+j]);
                }
            }

            if(!two_sided) {
                ri_geom_add_normals(p,
                                    nverts,
                                    (const ri_vector_t *)normals);
            } else { /* it is two sided, so more normals... */
                ri_geom_add_normals(p,
                                    nverts * 2,
                                    (const ri_vector_t *)normals);
            }
                
        } else if (strcmp(tokens[i], RI_ST) == 0 ||
               strcmp(tokens[i], "st")  == 0) { /* tex coords */
            if(!two_sided) {
                texcoords = (RtFloat *)ri_mem_alloc(
                        sizeof(RtFloat) * nverts * 2);
            } else {
                texcoords = (RtFloat *)ri_mem_alloc(
                        sizeof(RtFloat) * nverts * 4);
            }

            for (j = 0; j < nverts; j++) {
                texcoords[2 * j + 0] = param[2 * j + 0];
                texcoords[2 * j + 1] = param[2 * j + 1];
                if (two_sided) {
                    texcoords[2 * (j + nverts) + 0] =
                        param[2 * j + 0];
                    texcoords[2 * (j + nverts) + 1] =
                        param[2 * j + 1];
                }
            }

            if(!two_sided) {
                ri_geom_add_texcoords(p, nverts, texcoords);
            } else {
                ri_geom_add_texcoords(p, nverts * 2, texcoords);
            }

        } else if (strcmp(tokens[i], RI_CS) == 0) { /* vertex col */ 
            
            if(!two_sided) {
                colors = (ri_vector_t *)ri_mem_alloc(
                        sizeof(ri_vector_t)*nverts);
            } else {
                colors = (ri_vector_t *)ri_mem_alloc(
                        sizeof(ri_vector_t)*nverts * 2);
            }

            for (j = 0; j < nverts; j++) {
                ri_vector_set_from_rman(colors[j], &param[3 * j]);
                if (two_sided) {
                    ri_vector_copy(colors[j + nverts],
                                   colors[j]);
                }
            }

            has_color = 1;
        }
    }
        
    /* create indicies(triangle fan) */
    if(!two_sided)
        nindices = 3 * (nverts - 2);
    else /* a two-sided triangle fan */
        nindices = 6 * (nverts - 2);
        
    indices = (unsigned int *)ri_mem_alloc(sizeof(unsigned int) * nindices);

    if(!two_sided) {
        for (j = 0; j < nverts - 2; j++) {
            if (rh) {
                indices[j * 3 + 0] = j + 2;
                indices[j * 3 + 1] = j + 1;
                indices[j * 3 + 2] = 0;
            } else {
                indices[j * 3 + 0] = 0;
                indices[j * 3 + 1] = j + 1;
                indices[j * 3 + 2] = j + 2;
            }
        }
    } else { /* two sided polygon... */
        nv = nverts;

        for (j = 0; j < nverts - 2; j++) {
            if (rh) {
                indices[j * 3 + 0] = j + 2;
                indices[j * 3 + 1] = j + 1;
                indices[j * 3 + 2] = 0;
            } else {
                indices[j * 3 + 0] = 0;
                indices[j * 3 + 1] = j + 1;
                indices[j * 3 + 2] = j + 2;
            }
        }

        for (j = 0; j < nverts - 2; j++) {
            if (rh) {
                indices[(nindices/2) + j * 3 + 0] = nv + 0;
                indices[(nindices/2) + j * 3 + 1] = nv + j + 1;
                indices[(nindices/2) + j * 3 + 2] = nv + j + 2;
            } else {
                indices[(nindices/2) + j * 3 + 0] = nv + j + 2;
                indices[(nindices/2) + j * 3 + 1] = nv + j + 1;
                indices[(nindices/2) + j * 3 + 2] = nv + j + 0;
            }
        }
    }

    ri_geom_add_indices(p, nindices, indices);

    if(two_sided)
        nverts*=2;
        
    ctx = ri_render_get()->context;
    attr = (ri_attribute_t *)ri_stack_get(ctx->attr_stack);

    /* vertex colors */
    if (!has_color) {
        colors = (ri_vector_t *)ri_mem_alloc(sizeof(ri_vector_t) *
                             nverts);

        ri_vector_copy(refcol, attr->color);

        ri_mem_copy(colors, refcol, sizeof(ri_vector_t) * nverts);
    }

    ri_geom_add_colors(p, nverts, (const ri_vector_t *)colors);

    /* vertex opacities */
    opacities = (ri_vector_t *)ri_mem_alloc(sizeof(ri_vector_t) * nverts);
    
    ri_vector_copy(refcol, attr->opacity);

    ri_mem_copy(opacities, refcol, sizeof(ri_vector_t) * nverts);


    ri_geom_add_opacities(p, nverts, (const ri_vector_t *)opacities);

    /* hack */
    p->kd = 0.75;
    p->ks = 0.0;

    /* surface shader information. */
    if (attr->surface) {
        p->shadername = strdup(attr->surface);
    }

    if (attr->shader) {
        p->shader     = ri_shader_dup(attr->shader);
    }

    /* material */
    if (attr->material) {
        if (!p->material) p->material = ri_material_new();
        ri_material_copy(p->material, attr->material);
    }

    p->two_side = two_sided;
    
    return p;
}

ri_geom_t *
ri_pointspolygons_parse(RtInt npolys, RtInt nverts[], RtInt verts[],
            RtInt n, RtToken tokens[], RtPointer params[])
{
    unsigned int    i, j, k;
    int             rh;
    ri_context_t   *ctx;
    ri_attribute_t *attr;
    ri_geom_t      *p;
    RtFloat        *param;
    RtFloat        *texcoords;
    ri_vector_t    *positions;
    ri_vector_t    *normals;
    ri_vector_t    *colors = NULL;
    ri_vector_t    *opacities;
    ri_vector_t     refcol;
    ri_vector_t     v;
    ri_matrix_t    *m;
    ri_matrix_t     om;
    ri_matrix_t     orientation;
    ri_matrix_t     itm;
    unsigned int   *indices;
    unsigned int    nindices;
    unsigned int    nvertices;
    ri_array_t     *indexarray;
    int             order[6];
    int             has_color = 0;
    int             poly_warn = 0;

    p = ri_geom_new();

    if (strcmp(ri_render_get()->context->option->orientation, RI_RH) == 0) {
        rh = 1;
    } else {
        rh = 0;
    }

    order[0] = 0; order[1] = 1; order[2] = 2;
    order[3] = 0; order[4] = 2; order[5] = 3;

    ctx = ri_render_get()->context;
    attr = (ri_attribute_t *)ri_stack_get(ctx->attr_stack);

    /* get modelview matrix */
    m = (ri_matrix_t *)ri_stack_get(ctx->trans_stack);

    /* build orientation matrix */
    ri_matrix_identity(&orientation);
    if (rh) {
        orientation.f[2][2] = -orientation.f[2][2];
    }

    /* om = orientation . modelview */
    ri_matrix_mul(&om, m, &orientation);

    /* create indicies */
    /* currently only consider triangle polygon */

    indexarray = ri_array_new(sizeof(unsigned int));

    nvertices = 0; nindices = 0; j = 0;
    for (i = 0; i < (unsigned int)npolys; i++) {
        if (nverts[i] > 4 && !poly_warn) {
            ri_log(LOG_WARN,
                   "lucille supports only 3 or 4 face polygon.");
            poly_warn = 1;    /* To display warning only once. */
            continue;
        }

        for (k = 0; k < (unsigned int)nverts[i]; k++) {
            if (nvertices < (unsigned int)verts[j + k]) {
                nvertices = verts[j + k];
            }
        }

        if (nverts[i] == 3) {
            for (k = 0; k < 3; k++) {
                ri_array_insert(indexarray,
                    nindices + k,
                    (unsigned int *)&verts[j + order[k]]);
            }
            nindices += 3;
        } else {        /* nverts[i] = 4 */
            for (k = 0; k < 6; k++) {
                ri_array_insert(indexarray,
                    nindices + k,
                    (unsigned int *)&verts[j + order[k]]);
            }
            nindices += 6;
        }

        j += nverts[i];
    }

    /* because index in RiPointsPolygons()'s verts[] is zero base. */
    nvertices++;

    if (attr->sides == 2) {
        /* Duplicate indiex list with each face index reversed and 
         * offsetted by nvertices.
         * e.g.
         * 
         *    (0, 1, 2, 0, 2, 3)    nvertices = 4.
         *
         * -> (0, 1, 2, 0, 2, 3, 2+4, 1+4, 0+4, 3+4, 2+4, 0+4)
         */
        for (i = 0; i < nindices / 3; i++) {
            for (j = 0; j < 3; j++) {
                k = *(unsigned int *)ri_array_at(indexarray,
                                 i * 3 + 2 - j);
                k += nvertices;
                //verts[i * 3 + order[3 - j]] + nvertices;
                ri_array_insert(indexarray,
                    nindices + i * 3 + j,
                    (unsigned int *)&k);
            }
        }

        nindices *= 2;
    }

    indices = (unsigned int *)ri_mem_alloc(sizeof(unsigned int) * nindices);

    for (i = 0; i < nindices; i++) {
        indices[i] = *(unsigned int *)ri_array_at(indexarray, i);
    }

    ri_geom_add_indices(p, nindices, indices);

    for (i = 0; i < (unsigned int)n; i++) {

        param = (RtFloat *)params[i];

        /*
         * Position
         */
        if (strcmp(tokens[i], RI_P) == 0) {

            if (attr->sides == 2) {
                positions = (ri_vector_t *)ri_mem_alloc(
                        sizeof(ri_vector_t) * nvertices * 2);
            } else {
                positions = (ri_vector_t *)ri_mem_alloc(
                        sizeof(ri_vector_t) * nvertices);
            }

            for (j = 0; j < nvertices; j++) {

                /*
                 * transform vector from object space
                 * to world space
                 */
                ri_vector_set_from_rman(v, &param[j * 3]);

                ri_vector_transform(positions[j], v, &om);

                if (attr->sides == 2) {
                    ri_vector_copy(positions[nvertices+j],
                                   positions[j]);
                }
            }

            if (attr->sides == 2) {
                ri_geom_add_positions(p,
                                      2 * nvertices, 
                                      (const ri_vector_t *)positions);
            } else {
                ri_geom_add_positions(p,
                                      nvertices,
                                      (const ri_vector_t *)positions);
            }

        /*
         * Per vertex normal
         */
        } else if ( (strcmp(tokens[i], RI_N) == 0) ||
                    (strcmp(tokens[i], "vertex normal N") == 0)) {

            if (attr->sides == 2) {
                normals = (ri_vector_t *)ri_mem_alloc(
                        sizeof(ri_vector_t) * 2 * nvertices);
            } else {
                normals = (ri_vector_t *)ri_mem_alloc(
                        sizeof(ri_vector_t) * nvertices);

            }
            /*
             *       -T
             * N' = M   N
             *
             * calculate inverse transpose of the modelview 
             * matrix for normal vector transformation.
             */

            ri_matrix_copy(&itm, &om);

            /* 
             * we need only upper left 3x3 elements, so clear
             * other elements
             */

            itm.f[0][3] = 0.0;
            itm.f[1][3] = 0.0;
            itm.f[2][3] = 0.0;
            itm.f[3][0] = 0.0;
            itm.f[3][1] = 0.0;
            itm.f[3][2] = 0.0;
            itm.f[3][3] = 1.0;
            
            ri_matrix_inverse(&itm);
            ri_matrix_transpose(&itm);

            for (j = 0; j < nvertices; j++) {
                ri_vector_set_from_rman(v, &param[j * 3]);
                ri_vector_transform(normals[j], v, &itm);
                ri_vector_normalize(normals[j]);

                if (attr->sides == 2) {
                    ri_vector_copy(normals[nvertices+j],
                               normals[j]);
                    ri_vector_neg(normals[nvertices+j]);
                }
            }

            if (attr->sides == 2) {
                ri_geom_add_normals(p,
                                    2 * nvertices,
                                    (const ri_vector_t *)normals);
            } else {
                ri_geom_add_normals(p,
                                    nvertices,
                                    (const ri_vector_t *)normals);
            }
        } else if (strcmp(tokens[i], RI_ST) == 0 ||
               strcmp(tokens[i], "st") == 0) { /* tex coords */
            if (attr->sides == 2) {
                texcoords = (RtFloat *)ri_mem_alloc(
                        sizeof(RtFloat) *
                        nvertices * 2 * 2);
            } else {
                texcoords = (RtFloat *)ri_mem_alloc(
                        sizeof(RtFloat) *
                        nvertices * 2);
            }

            for (j = 0; j < nvertices; j++) {
                texcoords[2 * j + 0] = param[2 * j + 0];
                texcoords[2 * j + 1] = param[2 * j + 1];

                if (attr->sides == 2) {
                    texcoords[2 * (nvertices + j) + 0] =
                        param[2 * j + 0];
                    texcoords[2 * (nvertices + j) + 1] =
                        param[2 * j + 1];
                }
            }

            if (attr->sides == 2) {
                ri_geom_add_texcoords(p, nvertices * 2,
                              texcoords);
            } else {
                ri_geom_add_texcoords(p, nvertices,
                              texcoords);
            }

        } else if (strcmp(tokens[i], RI_CS) == 0) { /* vertex col */ 
            
            if (attr->sides == 2) {
                colors = (ri_vector_t *)ri_mem_alloc(
                        sizeof(ri_vector_t)*nvertices);
            } else {
                colors = (ri_vector_t *)ri_mem_alloc(2*
                        sizeof(ri_vector_t)*nvertices);
            }

            for (j = 0; j < nvertices; j++) {
                ri_vector_set_from_rman(colors[j], &param[3 * j]);
                if (attr->sides) {
                    ri_vector_copy(colors[j + nvertices],
                                   colors[j]);
                }
            }

            has_color = 1;
        }

    }

    if (attr->sides == 2) {
        nvertices *= 2;
    }

    /* tangents and binormals */
    if (p->normals) gen_basis(p);

    /* vertex colors */
    if (!has_color) {
        colors = (ri_vector_t *)ri_mem_alloc(sizeof(ri_vector_t) *
                             nvertices);
        
        ri_vector_copy(refcol, attr->color);

        for (i = 0; i < nvertices; i++) {
            ri_mem_copy(colors[i], refcol, sizeof(ri_vector_t));
        }
    }

    ri_geom_add_colors(p, nvertices, (const ri_vector_t *)colors);

    /* vertex opacities */
    opacities = (ri_vector_t *)ri_mem_alloc(sizeof(ri_vector_t) * nvertices);
    
    ri_vector_copy(refcol, attr->opacity);

    for (i = 0; i < nvertices; i++) {
        ri_mem_copy(opacities[i], refcol, sizeof(ri_vector_t));
    }

    ri_geom_add_opacities(p, nvertices, (const ri_vector_t *)opacities);

    /* surface shader information. */
    if (attr->surface) {
        p->shadername = strdup(attr->surface);
    }

    if (attr->shader) {
        p->shader     = ri_shader_dup(attr->shader);
    }

    /* material */
    if (attr->material) {
        if (!p->material) p->material = ri_material_new();
        ri_material_copy(p->material, attr->material);
    }    


    /* hack */
    p->kd = 0.75;
    p->ks = 0.0;

    if (attr->sides == 2) p->two_side = 1;
    else                  p->two_side = 0;

    return p;
}

ri_geom_t *
ri_pointsgeneralpolygons_parse(RtInt npolys, RtInt nloops[],
                   RtInt nverts[], RtInt verts[],
                   RtInt n, RtToken tokens[], RtPointer params[])
{
    /* lucille only consider the polygon with all nloops[] are 1. */
    (void)nloops;

    return ri_pointspolygons_parse(npolys, nverts, verts, n, tokens, params);
}

void
ri_api_polygon(RtInt nverts, RtInt n, RtToken tokens[], RtPointer params[])
{
    ri_geom_t  *geom;
    ri_light_t *arealight;

    ri_timer_start(ri_render_get()->context->timer, "Geom | Polygon");

    geom = ri_polygon_parse(nverts, n, tokens, params);

    ri_timer_end(ri_render_get()->context->timer, "Geom | Polygon");

    if (ri_render_get()->context->arealight_block) {
        arealight = (ri_light_t *)
                ri_list_last(ri_render_get()->scene->light_list)->data;

        if (arealight == NULL) {

            ri_log(LOG_WARN, "Invalid RIB structure?\n");

        } else {

            assert(arealight->geom);

            arealight->geom = geom;
            geom->light = arealight;

        }
    }

    /*
     * Arealight geometry is also added to the scene geometry.
     * Spatial data structure is constructed for mix of light geometries and
     * surface geometries. 
     */
    ri_scene_add_geom(ri_render_get()->scene, geom);
}

void
ri_api_pointspolygons(RtInt npolys, RtInt nverts[], RtInt verts[],
              RtInt n, RtToken tokens[], RtPointer params[])
{
    ri_geom_t *geom;
    ri_light_t *arealight;

    ri_timer_start(ri_render_get()->context->timer, "Geom | PointsPolygons");

    geom = ri_pointspolygons_parse(npolys, nverts, verts,
                       n, tokens, params);

    ri_timer_end(ri_render_get()->context->timer, "Geom | PointsPolygons");

    if (ri_render_get()->context->arealight_block) {
        arealight = (ri_light_t *)
                ri_list_last(ri_render_get()->scene->light_list)->data;

        if (arealight == NULL) {

            ri_log(LOG_WARN, "Invalid RIB structure?\n");

        } else {

            assert(arealight->geom);

            arealight->geom = geom;

        }
    } else {
        ri_scene_add_geom(ri_render_get()->scene, geom);
    }
}

void
ri_api_pointsgeneralpolygons(RtInt npolys, RtInt nloops[],
                 RtInt nverts[], RtInt verts[],
                     RtInt n, RtToken tokens[], RtPointer params[])
{
    ri_geom_t *geom;
    ri_light_t *arealight;

    ri_timer_start(ri_render_get()->context->timer,
               "Geom | PointsGeneralPolygons");

    geom = ri_pointsgeneralpolygons_parse(npolys, nloops, nverts, verts,
                              n, tokens, params);

    ri_timer_end(ri_render_get()->context->timer,
             "Geom | PointsGeneralPolygons");

    if (ri_render_get()->context->arealight_block) {
        arealight = (ri_light_t *)
                ri_list_last(ri_render_get()->scene->light_list)->data;

        if (arealight == NULL) {

            ri_log(LOG_WARN, "Invalid RIB structure?\n");

        } else {

            assert(arealight->geom);

            arealight->geom = geom;

        }

    } else {
        ri_scene_add_geom(ri_render_get()->scene, geom);
    }
}

/* ---------------------------------------------------------------------------
 *
 * Private functions
 *
 * ------------------------------------------------------------------------ */

/*
 * generate tangent vector(dPdu) and binormal vector(dPdv).
 * see:
 * "The Differential Geometry of Texture Mapping and Shading"
 * Ken Turkowski, Apple Compute Technical Report, 1992
 */ 
static void
gen_basis(ri_geom_t *geom)
{
    (void)geom;

#if 0    /* TODO: implement */
    unsigned int i, j;
    ri_vector_t  tmpbasis[3];
    ri_vector_t  dpdu;
    ri_vector_t  dpdv;
    ri_vector_t  tmp;
    //float        scale;
    double       du1, du2;
    double       dv1, dv2;
    double       dx1, dx2;
    double       dy1, dy2;
    double       dz1, dz2;
    double       det, invdet;
    double       area;

    ri_vector_t  *v0, *v1, *v2;
    unsigned int id[3];

    if (!geom->tangents) {
        geom->tangents = (ri_vector_t *)
                 ri_mem_alloc(sizeof(ri_vector_t) *
                          geom->npositions);
        geom->ntangents = geom->npositions;
    }

    if (!geom->binormals) {
        geom->binormals = (ri_vector_t *)
                   ri_mem_alloc(sizeof(ri_vector_t) *
                            geom->npositions);
        geom->nbinormals = geom->npositions;
    }
    
    if (!geom->texcoords) {
        for (i = 0; i < geom->npositions; i++) {
            ri_ortho_basis(tmpbasis, &geom->normals[i]);
            ri_vector_copy(&(geom->tangents[i]), &tmpbasis[0]);
            ri_vector_copy(&(geom->binormals[i]), &tmpbasis[1]);
            
        }

        return;
    } 

    for (i = 0; i < geom->npositions; i++) {
        ri_vector_zero(&(geom->tangents[i]));
        ri_vector_zero(&(geom->binormals[i]));
    }

    for (i = 0; i < geom->nindices / 3; i++) {
        for (j = 0; j < 3; j++) {
            id[j] = geom->indices[3 * i + j];
    }

        v0 = &(geom->positions[id[0]]);
        v1 = &(geom->positions[id[1]]);
        v2 = &(geom->positions[id[2]]);

        du1 = geom->texcoords[2*id[1]+0] - geom->texcoords[2*id[0]+0];
        du2 = geom->texcoords[2*id[2]+0] - geom->texcoords[2*id[0]+0];
        dv1 = geom->texcoords[2*id[1]+1] - geom->texcoords[2*id[0]+1];
        dv2 = geom->texcoords[2*id[2]+1] - geom->texcoords[2*id[0]+1];

        dx1 = v1->f[0] - v0->f[0];
        dx2 = v2->f[0] - v0->f[0];
        dy1 = v1->f[1] - v0->f[1];
        dy2 = v2->f[1] - v0->f[1];
        dz1 = v1->f[2] - v0->f[2];
        dz2 = v2->f[2] - v0->f[2];

        det = du1 * dv2 - dv1 * du2;
        if (det == 0) {
            for (j = 0; j < 3; j++) {
                /* tmp.xyz = N.yzx */
                tmp.f[0] = geom->normals[id[j]].f[1];
                tmp.f[1] = geom->normals[id[j]].f[2];
                tmp.f[2] = geom->normals[id[j]].f[0];

                ri_vector_cross3(&dpdv,
                         &geom->normals[id[j]],
                         &tmp);

                /* average vectors */
                area = ri_area(v0, v1, v2);
                ri_vector_scale(&dpdu, area);

                ri_vector_add(&(geom->tangents[id[j]]),
                          &(geom->tangents[id[j]]),
                          &dpdu);
                
            }

        } else {

            invdet = 1.0 / det;    

            dpdu.f[0] = (dx1 * dv2 - dv1 * dx2) * invdet;
            dpdu.f[1] = (dy1 * dv2 - dv1 * dy2) * invdet;
            dpdu.f[2] = (dz1 * dv2 - dv1 * dz2) * invdet;

            /* average vectors */
            area = ri_area(v0, v1, v2);
            ri_vector_scale(&dpdu, area);

            for (j = 0; j < 3; j++) {
                ri_vector_add(&(geom->tangents[id[j]]),
                          &(geom->tangents[id[j]]),
                          &dpdu);

            }
        }
    }

    for (i = 0; i < geom->npositions; i++) {

        ri_vector_normalize3(&(geom->tangents[i]));

        /* tangent = y axis, binormal = x axis, normal = z axis.
         * (in left-hand coordinates)
         */

#if 0
        printf("n dot t = %f\n", ri_vector_dot3(geom->normals[i],
                            geom->tangents[i]));
#endif

        /* binormal = x = cross(y, z) */
        ri_vector_cross3(&(geom->binormals[i]),
                 geom->tangents[i],
                 geom->normals[i]);
        ri_vector_normalize3(&(geom->binormals[i]));

        /* tangent = cross(z, x) */
        ri_vector_cross3(&(geom->tangents[i]),
                 geom->normals[i],
                 geom->binormals[i]);
        ri_vector_normalize3(&(geom->tangents[i]));

#if 0
        printf("normal[%d] = %f, %f, %f\n",
            i,
            geom->normals[i].f[0],
            geom->normals[i].f[1],
            geom->normals[i].f[2]);


        printf("tangent[%d] = %f, %f, %f\n",
            i,
            geom->tangents[i].f[0],
            geom->tangents[i].f[1],
            geom->tangents[i].f[2]);

        printf("n dot t' = %f\n", ri_vector_dot3(geom->normals[i],
                            geom->tangents[i]));

#endif
    }
#else        /* from nvmeshmener.cpp */


#endif
}
