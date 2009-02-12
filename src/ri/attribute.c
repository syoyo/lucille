/*
 * $Id: attribute.c,v 1.8 2004/06/13 06:44:51 syoyo Exp $
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "attribute.h"
#include "apitable.h"
#include "option.h"
#include "render.h"
#include "context.h"
#include "log.h"
#include "texture.h"
#include "shader.h"
#include "dlload.h"

static ri_shader_t *load_shader(const char *name);

#if 0
static int toInt(RtPointer p)
{
    RtFloat *f;
    f = (RtFloat *)p;
    return (int)(*f);
}
#endif

ri_attribute_t *
ri_attribute_new()
{
    ri_attribute_t *attribute = NULL;
    RtVector one = {1.0, 1.0, 1.0};

    attribute = (ri_attribute_t *)ri_mem_alloc(sizeof(ri_attribute_t));

    /*
     * Set default value.
     */
    ri_vector_set_from_rman(attribute->color, one);
    ri_vector_set_from_rman(attribute->opacity, one);

    attribute->material = ri_material_new();

    attribute->sides = 1;        /* default: one-sided polygon */

    attribute->surface = NULL;
    attribute->shader  = NULL;


    return attribute;
}

void
ri_attribute_free(ri_attribute_t *attribute)
{
    ri_mem_free(attribute->material);
    if (attribute->surface && attribute->shader) {
        ri_param_free(attribute->shader->param);
        ri_mem_free(attribute->shader);
        attribute->shader = NULL;
    }
    ri_mem_free(attribute->surface);
    ri_mem_free(attribute);
}

void
ri_api_attribute_begin()
{
    ri_attribute_t *newattr;
    ri_attribute_t *attr;
    ri_context_t   *ctx;
    ri_matrix_t    *newmat = NULL;
    ri_matrix_t    *mat = NULL;

    ctx = ri_render_get()->context;

    /* AttributeBegin pushes current transformation. */
    mat = (ri_matrix_t *)ri_stack_get(ctx->trans_stack);
    ri_log_and_return_if(mat == NULL);

    newmat = (ri_matrix_t *)ri_mem_alloc(sizeof(ri_matrix_t));
    ri_matrix_copy(newmat, mat);

    ri_stack_push(ctx->trans_stack, (void *)newmat);


    attr = (ri_attribute_t *)ri_stack_get(ctx->attr_stack);
    ri_log_and_return_if(attr == NULL);

    newattr = ri_attribute_new();
    ri_attribute_copy(newattr, attr);

    ri_stack_push(ctx->attr_stack, (void *)newattr);
}

void
ri_attribute_copy(ri_attribute_t *dst, const ri_attribute_t *src)
{
    ri_material_t *tmp;

    tmp = dst->material;        /* save pointer to material */
    ri_mem_copy(dst, src, sizeof(ri_attribute_t));
    
    dst->material = tmp;        /* restore poitner to material */

    /* copy material */
    dst->material = ri_material_new();
    ri_material_copy(dst->material, src->material);

    /* copy surface */
    if (src->surface) dst->surface = strdup(src->surface);

    dst->shader = ri_shader_dup(src->shader);

}

void
ri_api_attribute_end()
{
    ri_attribute_t *attr;
    ri_context_t   *ctx;
    ri_matrix_t *mat = NULL;

    ctx = ri_render_get()->context;

    if (ri_stack_depth(ctx->attr_stack) <= 1) {
        /* stack #1 stores default attribute, so can't pop. */
        ri_log(LOG_WARN, "(Render) There is no corresponding RiAttributeBegin()");    
        return;
    }

    /* AttributeEnd pops current transformation. */
    mat = (ri_matrix_t *)ri_stack_get(
                ri_render_get()->context->trans_stack);

    ri_log_and_return_if(mat == NULL);

    ri_matrix_free(mat);

    ri_stack_pop(ri_render_get()->context->trans_stack);

    attr = (ri_attribute_t *)ri_stack_get(ctx->attr_stack);

    if (attr != NULL) {
        ri_attribute_free(attr);
    } else {    
        ri_log(LOG_WARN, "(Render) attr == NULL");
    }

    ri_stack_pop(ctx->attr_stack);

    ctx->arealight_block = 0;
}

void
ri_api_attribute(RtToken token, RtInt n, RtToken tokens[], RtPointer params[])
{
    int             i, j;
    ri_attribute_t *attr;
    ri_context_t   *ctx;
    RtVector        vec;
    RtToken        *tokp;

    ctx = ri_render_get()->context;

    attr = (ri_attribute_t *)ri_stack_get(ctx->attr_stack);

    if (attr == NULL) {
        ri_log(LOG_WARN, "(Render) attr == NULL");
        return;
    }

    if (strcmp(token, "reflection") == 0) {
        /* surface reflection model parameter */

        for (i = 0; i < n; i++) {
            if (strcmp(tokens[i], "diffuse") == 0) {
                for (j = 0; j < 3; j++) {
                    vec[j] = *((RtFloat *)params[i] + j);
                }
            
                ri_vector_set_from_rman(attr->material->kd, vec);    
            } else if (strcmp(tokens[i], "specular") == 0) {
                for (j = 0; j < 3; j++) {
                    vec[j] = *((RtFloat *)params[i] + j);
                }
            
                ri_vector_set_from_rman(attr->material->ks, vec);    
            } else if (strcmp(tokens[i], "transmittance") == 0) {
                for (j = 0; j < 3; j++) {
                    vec[j] = *((RtFloat *)params[i] + j);
                }
            
                ri_vector_set_from_rman(attr->material->kt, vec);    
            } else if (strcmp(tokens[i], "refraction_index") == 0) {
                vec[0] = *((RtFloat *)params[i]);
            
                attr->material->ior = vec[0];
            } else if (strcmp(tokens[i], "fresnel") == 0) {
                tokp = (RtToken *)params[i];
                if (strcmp(*tokp, "yes") == 0) {
                    attr->material->fresnel = 1;    
                } else {
                    attr->material->fresnel = 0;    
                }
            }
        }
    } else if (strcmp(token, "photon") == 0) {
        /* photon map attributes */

        for (i = 0; i < n; i++) {
            if (strcmp(tokens[i], "causticmap") == 0) {
                tokp = (RtToken *)params[i];
                // pmap->set_causticmap(*tokp);
            } else if (strcmp(tokens[i], "globalmap") == 0) {
                tokp = (RtToken *)params[i];
                // pmap->set_globalmap(*tokp);
            } else if (strcmp(tokens[i], "estimator") == 0) {

                ri_log(LOG_WARN, "(PMap   ) \"estimator\" is not yet supported."); 

            } else if (strcmp(tokens[i], "shadingmodel") == 0 ||
                   strcmp(tokens[i],
                      "string shadingmodel") == 0) {

                ri_log(LOG_WARN, "(PMap   ) \"shadingmodel\" is not yet supported."); 
            }
        }
    }

}

void
ri_api_color(RtColor color)
{
    ri_attribute_t *attr;

    attr = ri_stack_get(ri_render_get()->context->attr_stack);

    ri_vector_set_from_rman(attr->color, color);
}

void
ri_api_opacity(RtColor color)
{
    ri_attribute_t *attr;

    ri_log(LOG_WARN, "(Render) RiOpacity is not yet implemented");

    attr = ri_stack_get(ri_render_get()->context->attr_stack);

    ri_vector_set_from_rman(attr->opacity, color);
}

void
ri_api_texture_coordinates(RtFloat s1, RtFloat t1, RtFloat s2, RtFloat t2,
                           RtFloat s3, RtFloat t3, RtFloat s4, RtFloat t4)
{
    ri_attribute_t *attr;

    ri_log(LOG_WARN, "(Render) RiTextureCoordinates is not yet implemented");

    attr = ri_stack_get(ri_render_get()->context->attr_stack);

    attr->texture_coordinates[0][0] = s1;    
    attr->texture_coordinates[0][1] = t1;    
    attr->texture_coordinates[1][0] = s2;    
    attr->texture_coordinates[1][1] = t2;    
    attr->texture_coordinates[2][0] = s3;    
    attr->texture_coordinates[2][1] = t3;    
    attr->texture_coordinates[3][0] = s4;    
    attr->texture_coordinates[3][1] = t4;    
}

void
ri_api_surface(RtToken name, RtInt n, RtToken tokens[], RtPointer params[])
{
    int            i;
    float          scale = 1.0;
    RtFloat        *valp;
    RtToken        *tokp;
    ri_attribute_t *attr;
    ri_shader_t    *shader = NULL;

    attr = ri_stack_get(ri_render_get()->context->attr_stack);

    attr->material->texture = NULL;

    shader = load_shader(name);
    if (shader) {
        /* Override default shader parameter by ones in RIB call. */
        for (i = 0; i < n; i++) {
            ri_param_override(shader->param, tokens[i], params[i]);
        }

        /* Maintain shader data in attribute stack.
         * This is copied and used for each geometry.
         */
        attr->shader  = shader;

    } else {    /* Fixed shading pipeline. */
        for (i = 0; i < n; i++) {
            ri_log(LOG_DEBUG, "(RI    ) Surface param token = %s", tokens[i]);
            if (strcmp(tokens[i], "texture") == 0) {
                tokp = (RtToken *)params[i];
                attr->material->texture = ri_texture_load(*tokp);

            } else if (strcmp(tokens[i], "iblscale") == 0) {
                valp = (RtFloat *)params[i];
                scale = *valp;
                printf("iblscale = %f\n", scale);
            /* Hack for libMOSAIC */
            } else if (strcmp(tokens[i], "uniform string ColMap") == 0) {
                tokp = (RtToken *)params[i];
                attr->material->texture = ri_texture_load(*tokp);
            }
        }

        if (attr->material->texture) {
            ri_texture_scale(attr->material->texture, scale);
        }

        attr->shader  = NULL;
    }

    /* Discard informations of previously called Surface. */
    ri_mem_free(attr->surface);

    attr->surface = strdup(name);
}

void
ri_api_imager(RtToken name, RtInt n, RtToken tokens[], RtPointer params[])
{
    ri_log(LOG_WARN, "RiImager() will never be implemented.");
    (void)name;
    (void)n;
    (void)tokens;
    (void)params;
}

void
ri_api_sides(RtInt sides)
{
    ri_attribute_t *attr;

    attr = ri_stack_get(ri_render_get()->context->attr_stack);
    
    attr->sides = sides;
}

void
ri_api_hider(RtToken type, RtInt n, RtToken tokens[], RtPointer params[])
{
    ri_log(LOG_WARN, "RiHider() is not yet implemented.");
    (void)type;
    (void)n;
    (void)tokens;
    (void)params;
}

/* --- private functions --- */

static ri_shader_t *
load_shader(const char *name)
{
    ri_parameter_t           *param     = NULL;    /* shader parameter */
    dl_module_t              *module    = NULL;
    ri_shader_t         *s         = NULL;
    char                     *buf       = NULL;
    char                      fullpath[1024];

    /* +strlen("_initparam") for making room for prefix.  */
    buf = (char *)ri_mem_alloc(sizeof(char) *
                   (long)(strlen(name) + strlen("_initparam") + 1));

#ifdef WIN32
    sprintf(buf, "%s.dll", name);
#else
    sprintf(buf, "%s.so", name);
#endif

    if (!ri_option_find_file(fullpath,    /* output */
                 ri_render_get()->context->option,
                 buf)) {
        ri_log(LOG_WARN, "(API   ) Can't find shader \"%s\". Use default shader.", buf);
        ri_mem_free(buf);
        return NULL;
    }

    module = dlload(fullpath);
    if (module == NULL) {
        ri_log(LOG_WARN, "(API   ) Can't load shader \"%s\". Use default shader.", buf);
        ri_mem_free(buf);
        return NULL;
    }

    s   = (ri_shader_t *)ri_mem_alloc(sizeof(ri_shader_t));

    sprintf(buf, "%s_initparam", name);
    s->initparamproc = (ri_shader_initparam_proc)dlgetfunc(module, buf);
    if (!s->initparamproc) {
        fprintf(stderr, "can't get proc: %s\n", buf);
        exit(-1);
    }

    /* Initialize parameter list. */
    param = ri_param_new();
    s->initparamproc(param);
    s->param = param;

    s->shaderproc = (ri_shader_proc)dlgetfunc(module, name);
    if (!s->shaderproc) {
        fprintf(stderr, "can't get proc: %s\n", name);
        exit(-1);
    }

    ri_mem_free(buf);

    return s;
}
