#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <assert.h>

/* for performance analizer */
#if defined(__APPLE__) && defined(__MACH__)
//#include <ppc_intrinsics.h>
#endif

#include "vector.h"
#include "shader.h"
#include "raytrace.h"
#include "noise.h"
#include "memory.h"
#include "random.h"
#include "reflection.h"
#include "thread.h"
#include "texture.h"

#ifndef M_PI
#define M_PI 3.141592
#endif

static int typesize[] = {sizeof(ri_float_t) * 4, sizeof(char), sizeof(ri_float_t)};

typedef struct _lightsource_info_t
{
    ri_light_t       *light;
    int               sample_index;
    int               nsamples;
    ri_lightsource_t  samples[1024];
    ri_vector_t       basis[3];
    ri_vector_t       N;
} lightsource_info_t;

static unsigned int hash    (const char        *str);
static void status_copy     (ri_status_t       *dst,
                             const ri_status_t *src);

static ri_light_t *get_light(ri_render_t *render);

static void init_lightsource(const ri_status_t *status,
                             const ri_vector_t  P,
                             const ri_vector_t  N,
                             ri_float_t         angle);

static lightsource_info_t glightinfo[RI_MAX_THREADS];
static int                glight_initialized[RI_MAX_THREADS];

#ifdef WITH_ALTIVEC
#define vcomp(v, n) (*(((ri_float_t *)&(v)) + n))

/* codes from AltiVec tutorial by Ian Ollman */

static inline vector float vec_div(vector float v)
{
    vector float reciprocal = vec_re(v);
    return vec_madd(reciprocal,
            vec_nmsub(reciprocal, v, vec_ctf(vec_splat_u32(1), 0)),
            reciprocal);
}

//Generate a vector full of -0.0
inline vector float vec_neg_zero(void)
{
    vector unsigned int result = vec_splat_u32(-1);
    return (vector float)vec_sl(result, result);
}

//Calculate the full precision reciprocal square root of v
inline vector float vec_recioprocal_sqrt(vector float v)
{
    const vector float kMinusZero = vec_neg_zero();
    const vector float kOne = vec_ctf(vec_splat_u32(1), 0);
    const vector float kOneHalf = vec_ctf(vec_splat_u32(1), 1);

    //Calculate 1/denomenator using newton rapheson
    //refined reciprocal estimate
    vector float sqrtReciprocalEstimate = vec_rsqrte(v);
    vector float reciprocalEstimate = vec_madd(sqrtReciprocalEstimate,
                           sqrtReciprocalEstimate,
                           kMinusZero);
    vector float halfSqrtReciprocalEst =
            vec_madd(sqrtReciprocalEstimate, kOneHalf, kMinusZero);
    vector float term1 = vec_nmsub(v, reciprocalEstimate, kOne);

    return vec_madd(term1, halfSqrtReciprocalEst, sqrtReciprocalEstimate);
}
#endif

/*
 * Function: ri_shader_dup
 *
 *     Duplicates shader.
 *
 * Parameters:
 *
 *     *src - Src address of shader.
 *
 * Returns:
 *
 *     Duplicated and memory allocated shader.
 *
 */
ri_shader_t *
ri_shader_dup(const ri_shader_t *src)
{
    ri_shader_t *p;

    if (!src) return NULL;

    p = (ri_shader_t *)ri_mem_alloc(sizeof(ri_shader_t));

    p->initparamproc = src->initparamproc;
    p->shaderproc    = src->shaderproc;
    p->param         = ri_param_dup(src->param);

    return p;
}

/*
 * Function: ri_param_new
 *
 *     Allocates memory for shader parameter list and initialize it with zero.
 *
 * Parameters:
 *
 *     None.
 *
 * Returns:
 *
 *     Memory allocated shader parameter list.
 *
 */
ri_parameter_t *
ri_param_new()
{
    ri_parameter_t *p;

    p = (ri_parameter_t *)ri_mem_alloc(sizeof(ri_parameter_t));

    memset(p, 0, sizeof(ri_parameter_t)); 

    return p;
}

/*
 * Function: ri_param_free
 *
 *     Frees memory allocated for shader parameter list.
 *
 * Parameters:
 *
 *     *param - Parameter list it's allocated memory to be freed.
 *
 * Returns:
 *
 *     None.
 *
 */
void
ri_param_free(ri_parameter_t *param)
{
    int          i;
    ri_paramnode_t *p;

    for (i = 0; i < PARAMHASH_SIZE; i++) {
        p = param->paramnodes[i];

        for ( ; p != NULL; p = p->next) {
            ri_mem_free(p->val);
        }
    }

    ri_mem_free(param);
}

/*
 * Function: ri_param_dup
 *
 *     Duplicates shader parameter list.
 *
 * Parameters:
 *
 *     *param - Reference parameter list to be duplicated.
 *
 * Returns:
 *
 *     Memory allocated duplication of input parameter list.
 *
 */
ri_parameter_t *
ri_param_dup(const ri_parameter_t *param)
{
    int             i;
    ri_parameter_t *newparam;
    ri_paramnode_t *p;
    ri_paramnode_t *np;

    if (!param) return NULL;

    newparam = ri_param_new();

    for (i = 0; i < PARAMHASH_SIZE; i++) {
        p = param->paramnodes[i];

        for ( ; p != NULL; p = p->next) {
            np = (ri_paramnode_t *)malloc(sizeof(ri_paramnode_t));
            np->name = strdup((const char *)(p->name));
            np->type = p->type;
            if (p->type == TYPESTRING) {
                np->size = typesize[p->type] * (strlen((char *)(p->val)) + 1);
                np->val = malloc(np->size);
                memcpy(np->val, p->val, p->size);
            } else {
                np->size = typesize[p->type];
                np->val = malloc(p->size);
                memcpy(np->val, p->val, p->size);
            }
            np->next = newparam->paramnodes[i];
            newparam->paramnodes[i] = np;
        }
    }

    return newparam;
}

/*
 * Function: ri_param_eval
 *
 *     Evaluates and returns shader parameter variables specified by name.
 *
 * Parameters:
 *
 *     *data  - Returned value of parameter variable.
 *     *param - Shader parameter list investigated.
 *     *name  - Shader parameter variable name.
 *
 *
 * Returns:
 *
 *     None.
 *
 */
void
ri_param_eval(void *data, const ri_parameter_t *param, const char *name)
{
    unsigned int h;
    int          len;
    ri_paramnode_t *p;

    h = hash(name);
    p = param->paramnodes[h];

    len = strlen(name);

    for ( ;
         p != NULL && p->len != len && strcmp(p->name, name) != 0;
         p = p->next);

    if (p) {
        if (p->type == TYPESTRING) {
            *((char **)data) = (char *)p->val;
        } else {
            memcpy(data, p->val, p->size);
        }
    } else {
        fprintf(stderr, "no entries\n");
        exit(-1);
    }
}

/*
 * Function: ri_param_add
 *
 *     Adds variables to shader parameter list.
 *
 * Parameters:
 *
 *     *param - Shader parameter list.
 *     *name  - Shader parameter variable name.
 *      type  - Type of parameter variable.
 *      val   - Value of parameter variable.
 *
 * Returns:
 *
 *     None.
 *
 */
void
ri_param_add(ri_parameter_t *param, const char *name, int type, const void *val)
{
    unsigned int h;
    int          len;
    ri_paramnode_t *p;

    h = hash(name);
    p = param->paramnodes[h];

    len = strlen(name);

    for ( ;
         p != NULL && p->len != len && strcmp(p->name, name) != 0;
         p = p->next);

    if (p) {
        /* ??? */
        fprintf(stderr, "duplicated declaration.\n");
        exit(-1);
    } else {
        p = (ri_paramnode_t *)malloc(sizeof(ri_paramnode_t));
        p->name = strdup((const char *)name);
        p->type = type;
        if (type == TYPESTRING) {
            p->size = typesize[type] * (strlen((char *)val) + 1);
            p->val = malloc(p->size);
            memcpy(p->val, val, p->size);
        } else {
            p->size = typesize[type];
            p->val = malloc(p->size);
            memcpy(p->val, val, p->size);
        }
        p->next = param->paramnodes[h];
        param->paramnodes[h] = p;
    }
}

/*
 * Function: ri_param_override
 *
 *     Overrides value of shader parameter variable.
 *
 * Parameters:
 *
 *     *param - Shader parameter list.
 *     *name  - Shader parameter variable name overrided.
 *      val   - New value of parameter variable.
 *
 * Returns:
 *
 *     None.
 *
 */
void
ri_param_override(ri_parameter_t *param,
          const char *name, const void *val)
{
    char           **tokp;
    unsigned int     h;
    int              len;
    ri_paramnode_t  *p;
    ri_vector_t      v;

    h = hash(name);
    p = param->paramnodes[h];

    len = strlen(name);

    for ( ;
         p != NULL && p->len != len && strcmp(p->name, name) != 0;
         p = p->next);

    if (!p) {
        /* ??? */
        fprintf(stderr,
            "no variable name [ %s ] in shader parameter list\n",
            name);
        return;
    }

    if (p->type == TYPESTRING) {

        free(p->val);
        tokp = (char **)val;
        p->size = typesize[p->type] * (strlen(*tokp) + 1);
        p->val = malloc(p->size);
        memcpy(p->val, *tokp, p->size);

        printf("%s: override value with %s\n", p->name, *tokp);

    } else {

        if (p->type == TYPEFLOAT) {

            memcpy(p->val, val, typesize[TYPEFLOAT]);

        } else {        /* vector type */

            v[0] = ((ri_float_t *)val)[0];
            v[1] = ((ri_float_t *)val)[1];
            v[2] = ((ri_float_t *)val)[2];
            v[3] = 1.0;

            ri_vector_print(v);
            memcpy(p->val, &v, typesize[TYPEVECTOR]);

        }

    }
}

/*
 * Function: ri_param_type
 *
 *     Returns type of parameter variable.
 *
 * Parameters:
 *
 *     *type  - Type of parameter variable returned.
 *     *param - Parameter lists variable contains.
 *      name  - Name of parameter variable.
 *
 * Returns:
 *
 *     0 if parameter variable is not found in param,
 *     1 if found.
 *
 */
int
ri_param_type(int *type, const ri_parameter_t *param, const char *name)
{
    unsigned int    h;
    int             len;
    ri_paramnode_t *p;

    h = hash(name);
    p = param->paramnodes[h];

    len = strlen(name);

    for ( ;
         p != NULL && p->len != len && strcmp(p->name, name) != 0;
         p = p->next);

    if (!p) {
        /* parameter variable is not found. */
        return 0;
    }

    *type = p->type;
        
    return 1;
}

void
faceforward(
    ri_vector_t       dst,
    const ri_vector_t N,
    const ri_vector_t I)
{
    double dot;
    ri_vector_t eye;

    ri_vector_copy(dst, N);

    ri_vector_copy(eye, I);
    ri_vector_neg(eye);

    dot = ri_vector_dot(N, eye);

    if (dot < -2.0e-1) {        /* TODO: consider machine epsilon */
        dst[0] = -dst[0];
        dst[1] = -dst[1];
        dst[2] = -dst[2];
    }
}

void
normalize(
    ri_vector_t       dst,
    const ri_vector_t N)
{
    ri_vector_copy(dst, N);
    ri_vector_normalize(dst);
}

void
ambient(
    const ri_status_t *status,
    ri_color_t         dst)
{
    ri_vector_t ambcol;

    ambcol[0] = 0.2f;
    ambcol[1] = 0.2f;
    ambcol[2] = 0.2f;
    ambcol[3] = 1.0f;
    ri_vector_copy(dst, ambcol);

    (void)status;
}

void
diffuse(
    const ri_status_t *status,
    ri_color_t         dst,
    const ri_vector_t  N)
{
    double      dot;
    ri_vector_t light;

    ri_vector_copy(light, status->input.L);
    ri_vector_normalize(light);

    dot = ri_vector_dot(N, light);

    if (dot <= 0.0) {
        dst[0] = 0.0f;
        dst[1] = 0.0f;
        dst[2] = 0.0f;
    } else {
        dst[0] = (ri_float_t)dot;
        dst[1] = (ri_float_t)dot;
        dst[2] = (ri_float_t)dot;
    }
}

void
specular(
    const ri_status_t *status,
    ri_color_t         dst,
    const ri_vector_t  N,
    const ri_vector_t  V,
    float              roughness)
{
    double      dot;
    double      coeff;
    ri_vector_t light;
    ri_vector_t half;

    ri_vector_copy(light, status->input.L);
    ri_vector_normalize(light);

    ri_vector_add(half, light, V);
    ri_vector_normalize(half);

    dot = ri_vector_dot(N, half);

    if (dot < 0.0) dot = 0.0;

    if (roughness != 0.0) {
        coeff = pow(dot, 1.0 / roughness);
    } else {
        coeff = dot;
    }

    ri_vector_set1(dst, coeff);
}

void
reflect(
    ri_vector_t       dst,
    const ri_vector_t I,
    const ri_vector_t N)
{
    ri_reflect(dst, I, N);
}

void
refract(
    ri_vector_t       dst,
    const ri_vector_t I,
    const ri_vector_t N,
    float             eta)
{
    ri_refract(dst, I, N, eta);
}

void
transform(
    ri_vector_t        dst,
    const char        *tospace,
    const ri_vector_t  src)
{
    /* TODO: not yet implemented. */
    ri_vector_copy(dst, src);
    
    (void)tospace;
    (void)src;
}

void
vtransform(
    ri_vector_t        dst,
    const char        *from,
    const char        *to,
    const ri_vector_t  src)
{
    /* TODO: not yet implemented. */
    ri_vector_copy(dst, src);

    (void)from;
    (void)to;
    (void)src;
}

void
environment(
    const ri_status_t *status,
    ri_color_t         dst,
    const char        *name,
    const ri_vector_t  coords)
{
    double      m;
    ri_vector_t tex_coords;

    m = 2.0 * sqrt(coords[0] * coords[0] +
                   coords[1] * coords[1] +
                   (coords[2] + 1.0) * (coords[2] + 1.0));

    
    if (m != 0.0) {
        tex_coords[0] = coords[0] / m + 0.5;
        tex_coords[1] = coords[1] / m + 0.5;
    } else {
        tex_coords[0] = 0.5;
        tex_coords[1] = 0.5;
    }

    texture_coords(status, dst, name, tex_coords);
}

void
texture(
    const ri_status_t *status,
    ri_color_t         dst,
    const char *name)
{
    /* TODO cached texture access */

    ri_texture_t *texture = NULL;

    texture = ri_texture_load(name);
    if (!texture) return;

    ri_texture_fetch(dst, texture,
             (double)status->input.s, (double)status->input.t);
}

void
texture_coords(
    const ri_status_t *status,
    ri_color_t         dst,
    const char        *name,
    const ri_vector_t  coords)
{
    const int step = 0x8;
    const int texwidth = 64;
    int u, v;
    int c;

    u = (int)(texwidth * coords[0]) % texwidth;
    v = (int)(texwidth * coords[1]) % texwidth;

    if (u < 0) u = texwidth + u;
    if (v < 0) v = texwidth + v;

    /* checker board pattern */

    c = ((u & step) == 0) ^ ((v & step) == 0);
    dst[0] = (ri_float_t)c;
    dst[1] = (ri_float_t)c;
    dst[2] = (ri_float_t)c;

    (void)name;
    (void)status;
}

float
occlusion(
    const ri_status_t *status,
    const ri_vector_t  P,
    const ri_vector_t  N,
    float              nsamples)
{
    int               i, j, k;
    int               hit;
    int               coverage;
    int               ntheta, nphi;
    double            theta, phi;
    ri_vector_t       dir;
    ri_vector_t       basis[3];
    ri_ray_t          ray;
    ri_intersection_state_t state;

#if defined(__APPLE__) && defined(__MACH__)
    //int               inst;
#endif

#if defined(__APPLE__) && defined(__MACH__)
    //inst = __mfspr(1023);
#endif
    
    coverage = 0;

    /* nphi = 3 * ntheta,
     * nsamples = nphi * ntheta
     */
    ntheta = nsamples / 3.0;
    ntheta = (int)sqrt((double)ntheta);
    if (ntheta < 1) ntheta = 1;
    nphi   = 3 * ntheta;

#if defined(__APPLE__) && defined(__MACH__)
    //inst |= __mfspr(1023);
    //exit(1);
#endif

    /* generate orhonormal basis with its z-axis corresponds to
     * surdace normal.
     */
    ri_ortho_basis(basis, N);

    /* generate samples on hemisphere with
     * Probability DistributionFunction = cos(theta)/Pi
     */
    for (j = 0; j < nphi; j++) {
        for (i = 0; i < (int)ntheta; i++) {
            theta = sqrt((double)i + randomMT()) /
                (double)ntheta    ;
            phi   = 2.0 * M_PI * ((double)j + randomMT()) /
                (double)nphi;

            dir[0] = cos(phi) * theta;
            dir[1] = sin(phi) * theta;
            dir[2] = sqrt(1.0 - theta * theta);

            for (k = 0; k < 3; k++) {
                ray.dir[k] = dir[0] * basis[0][k]
                           + dir[1] * basis[1][k]        
                           + dir[2] * basis[2][k];
            }

            ri_vector_normalize(ray.dir);
            ri_vector_copy(ray.org, P);

            // slightly moves the ray position towards
            // the ray direction
            ray.org[0] += 0.0001 * ray.dir[0]; 
            ray.org[1] += 0.0001 * ray.dir[1]; 
            ray.org[2] += 0.0001 * ray.dir[2]; 

            ray.thread_num = status->thread_num;

            hit = ri_raytrace(ri_render_get(), &ray, &state);

            if (hit) {
                /* there is a occluder. */
                coverage++;
            }
        }
    }    

    if (coverage > (int)nsamples) coverage = (int)nsamples;
    return (ri_float_t)coverage / (ri_float_t)nsamples;
}

/* AltiVec version */
#if 0
#ifdef WITH_ALTIVEC
ri_float_t
occlusion_altivec(const ri_status_t *status,
                  const ri_vector_t P, const ri_vector_t N, ri_float_t nsamples)
{
    int               i, j, k;
    int               hit;
    int               coverage;
    int               ntheta, nphi;
    ri_float_t             inv_ntheta, inv_nphi;
    //double            theta, phi;
    ri_vector_t       dir;
    ri_vector_t       basis[3];
    ri_ray_t          ray;
    ri_surface_info_t surfinfo;
    vector float      theta, phi;
    vector float      vntheta, vnphi;
    vector float      v;

//#if defined(__APPLE__) && defined(__MACH__)
//    int               inst;
//#endif

    coverage = 0;

    /* nphi = 3 * ntheta,
     * nsamples = nphi * ntheta
     */
    
    nsamples = 48;
    ntheta = nsamples / 3.0;
    ntheta = (int)sqrt((double)ntheta);
    if (ntheta < 1) ntheta = 1;
    nphi   = 3 * ntheta;

    vcomp(vntheta, 0) = ntheta;
    vcomp(vntheta, 1) = ntheta;
    vcomp(vntheta, 2) = ntheta;
    vcomp(vntheta, 3) = ntheta;

    inv_ntheta = 1.0 / ntheta;
    inv_nphi   = 1.0 / nphi;

//#if defined(__APPLE__) && defined(__MACH__)
    //inst |= __mfspr(1023);
    //exit(1);
//#endif

    /* generate orhonormal basis with its z-axis corresponds to
     * surdace normal.
     */
    ri_ortho_basis(basis, N);

    /* generate samples on hemisphere with
     * Probability DistributionFunction = cos(theta)/Pi
     */
    for (j = 0; j < nphi; j++) {
        for (i = 0; i < (int)ntheta/4; i++) {

//#if defined(__APPLE__) && defined(__MACH__)
            inst = __mfspr(1023);
//#endif

            vcomp(v, 0) = 4 * i + 0 + randomMT();
            vcomp(v, 1) = 4 * i + 1 + randomMT();
            vcomp(v, 2) = 4 * i + 2 + randomMT();
            vcomp(v, 3) = 4 * i + 3 + randomMT();

            // sqrt(v) = 1.0/sqrt(1/v)
            v = vec_div(v);
            theta = vec_recioprocal_sqrt(v);

            // dummy inst
            theta = vec_mad(theta, vec_div(ntheta), vec_neg_zero());

            vcomp(v, 0) = j + randomMT();
            vcomp(v, 1) = vcomp(v, 0);
            vcomp(v, 2) = vcomp(v, 0);
            vcomp(v, 3) = vcomp(v, 0);

            v = vec_div(v);

            phi = 

//#if defined(__APPLE__) && defined(__MACH__)
            inst |= __mfspr(1023);
//#endif

//#if 1
            phi   = 2.0 * M_PI * ((double)j + randomMT()) /
                (double)nphi;

            dir[0] = cos(phi) * theta;
            dir[1] = sin(phi) * theta;
            dir[2] = sqrt(1.0 - theta * theta);

            for (k = 0; k < 3; k++) {
                ray.dir[k] = dir[0] * basis[0][k]
                         + dir[1] * basis[1][k]        
                         + dir[2] * basis[2][k];
            }

            ri_vector_normalize(ray.dir);
            ri_vector_copy(ray.org, P);


            ray.thread_num = status->thread_num;

            hit = ri_raytrace(ri_render_get(), &ray, &surfinfo);

            if (hit) {
                /* there is a occluder. */
                coverage++;
            }
//#endif
        }
    }    

    if (coverage > (int)nsamples) coverage = (int)nsamples;
    return (ri_float_t)coverage / (ri_float_t)nsamples;
}
#endif
#endif

void
trace(
    const ri_status_t *status,
    ri_vector_t        dst,
    const ri_vector_t  P,
    const ri_vector_t  R)
{
    int          hit;
    ri_vector_t  eye;
    ri_status_t  newstatus;
    ri_shader_t *shader;
    ri_light_t  *light;
    ri_output_t  out;
    //ri_float_t        opa;        /* opacity */

    ri_ray_t          ray;
    ri_intersection_state_t state;

    if (status->ray_depth > 3) {
        ri_vector_setzero(dst);
        return;
    }
        
    ri_vector_copy(ray.dir, R);
    ri_vector_copy(ray.org, P);
    ray.org[0] += 0.0001 * ray.dir[0];
    ray.org[1] += 0.0001 * ray.dir[1];
    ray.org[2] += 0.0001 * ray.dir[2];

    ray.thread_num = status->thread_num;

    hit = ri_raytrace(ri_render_get(), &ray, &state);

    if (!hit) {
        light = get_light(ri_render_get());

        if (light && ((light->type == LIGHTTYPE_IBL) | (light->type == LIGHTTYPE_SUNSKY))) {
            /* Use IBL HDRI */
            ri_texture_ibl_fetch(dst, light->texture, ray.dir);
        } else {
            ri_vector_setzero(dst);
        }

        return; 
    }

    /* ray hits the surface.
     * get a shader at the hit point.
     */

    shader = state.geom->shader;

    if (!shader) {
        ri_vector_setzero(dst);
        return;
    } 

    ri_vector_sub(eye, state.P, P);
    ri_vector_normalize(eye);

    /* Set up the status for evaluating the shader at the hit point. */
    status_copy(&newstatus, status);

    newstatus.ray_depth++;        /* increment ray depth */
    ri_vector_copy(newstatus.input.Cs,   state.color);
    ri_vector_copy(newstatus.input.P,    state.P);
    ri_vector_copy(newstatus.input.N,    state.Ns);
    ri_vector_copy(newstatus.input.Ng,   state.Ng);
    ri_vector_copy(newstatus.input.dPdu, state.tangent);
    ri_vector_copy(newstatus.input.dPdv, state.binormal);
    ri_vector_copy(newstatus.input.I,    eye);
    newstatus.input.s = state.u;
    newstatus.input.t = state.v;

    //opa = state.opacity;
    //ri_vector_set4(&newstatus.input.Os, opa, opa, opa, opa);

    /* call a shader at the hit point */
    shader->shaderproc(&out, &newstatus, shader->param);
    ri_vector_copy(dst, out.Ci);
    //ri_vector_copy(dst, state.color);
}

float
inversesqrt(float x)
{
    if (x < 0.0) return 1.0;

    if (x < 0.001) return 1.0;

    return 1.0f / (float)sqrt(x);
}

float
mod(float a, float b)
{
    int   n;
    float c;

    n = (int)(a / b);

    c = a - n * b;

    if (c < 0.0f) c += b;

    return c;
}

float
minf(float a, float b)
{
    if (a < b) return a;
    
    return b;
}

float
clampf(float a, float min, float max)
{
    if (a < min) return min;
    if (a > max) return max;

    return a;
}

float
noise1d(float f)
{
    ri_float_t val;

    val = noise1(f);

    return ((ri_float_t)val + 1.0) * 0.5;
}

float
noise3d(const ri_vector_t v)
{
    float  arg[3];
    float  val;

    arg[0] = v[0];
    arg[1] = v[1];
    arg[2] = v[2];

    val = noise3(arg);

    return (val + 1.0) * 0.5;
}

float
step(float min, float value)
{
    if (value < min) return 0.0;

    return 1.0;
}

float
smoothstep(float min, float max, float value)
{
    double t;

    if (value < min) {
        t = 0.0;
    } else if (value >= max) {
        t = 1.0;
    } else {
        /* Hermite interpolation */
        t = (value - min) / (max - min);
        t = t * t * (3.0 - 2.0 * t);
    } 
    
    return t;
}

void
mixv(
    ri_vector_t       dst,
    const ri_vector_t x,
    const ri_vector_t y,
    float             alpha)
{
    dst[0] = (1.0 - alpha) * x[0] + alpha * y[0];
    dst[1] = (1.0 - alpha) * x[1] + alpha * y[1];
    dst[2] = (1.0 - alpha) * x[2] + alpha * y[2];
}

float
distant(
    const ri_vector_t p1,
    const ri_vector_t p2)
{
    ri_vector_t d;

    ri_vector_sub(d, p1, p2);

    return length(d);
}

float
area(const ri_vector_t P)
{
    /* TODO: implement this function */
    (void)P;
    return 1.0;
}

/* return the depth of the point P in camera coordinates.
 * the depth is normalized to lie between 0(at the near clipping plane)
 * and 1(aat the far clipping plane).
 */
float
depth(const ri_vector_t P)
{
    /* TODO implement this function */
    (void)P;

    return 1.0;    
}

ri_lightsource_t *
next_lightsource(
    const ri_status_t *status,
    const ri_vector_t  P,
    const ri_vector_t  N,
    ri_float_t         angle)
{
    /* TODO: currently only accounts for IBL light source */

    int                hit;
    int                tid;        /* thread number */
    ri_float_t         ndotl;
    ri_lightsource_t  *l = NULL;
    ri_ray_t           ray;
    ri_intersection_state_t  state;

    tid = status->thread_num;

    if (!glight_initialized[tid]) {
        init_lightsource(status, P, N, angle);

        glight_initialized[tid] = 1;
    }

    ri_vector_copy(ray.org, P);
    ray.org[0] += N[0] * 0.0001;    
    ray.org[1] += N[1] * 0.0001;    
    ray.org[2] += N[2] * 0.0001;    

    ray.thread_num = tid;

    if (glightinfo[tid].light->type == LIGHTTYPE_IBL) {

        while (glightinfo[tid].sample_index < glightinfo[tid].nsamples) {
            l =  &glightinfo[tid].samples[glightinfo[tid].sample_index];
            glightinfo[tid].sample_index++;

            ndotl = ri_vector_dot(l->L, N);
            if (ndotl <= 0.0) continue;

            /* test if the angle between L and N is inside a cone */
            if (acos(ndotl) >= angle) {
                continue;
            }
            
            /* occlusion test */
            ri_vector_copy(ray.dir, l->L);
            ri_vector_normalize(ray.dir);

            hit = ri_raytrace(ri_render_get(), &ray, &state);

            if (hit) {
                /* there is a occluder */
                continue;
            }

            //ri_vector_scale(&l->Cl, (1.0f / ndotl) * M_PI );

            break;
        }

        if (glightinfo[tid].sample_index >= glightinfo[tid].nsamples) {
            /* run out of light sources.
             * prepare for next illuminance() loop
             */
            glight_initialized[tid] = 0;
            return NULL;
        }

    }

    if (l == NULL) {
         /* prepare for next illuminance() loop */
        glight_initialized[tid] = 0;
    }

    return l;
}


/* --- private functions --- */

static unsigned int
hash(const char *str)
{
    const char *p = str;
    unsigned long h = *p;

    if (h) {
        for (p += 1; *p != '\0'; p++) {
            h = (h << 5) - h + *p;
        }
    }

    return h % PARAMHASH_SIZE;
}


static void
status_copy(ri_status_t *dst, const ri_status_t *src)
{
    memcpy(dst, src, sizeof(ri_status_t));
}

static ri_light_t *
get_light(ri_render_t *render)
{
    /* TODO: multiple light source */

    ri_light_t *light;

    if (!ri_list_first(render->scene->light_list)) {
        //ri_log("warning", "no light exist.\n");
        return NULL;
    }

    light = (ri_light_t *)ri_list_first(render->scene->light_list)->data;

    return light;
}

static void
init_lightsource(
    const ri_status_t *status,
    const ri_vector_t  P,
    const ri_vector_t  N,
    ri_float_t         angle)
{
    int          i, j, k;
    int          count = 0;
    int          ntheta, nphi;
    int          nsamples;
    int          tid;            /* thread number */
    ri_float_t   theta, phi;
    ri_vector_t  dir;
    ri_vector_t  ldir;
    ri_option_t *opt;

    (void)P;

    tid = status->thread_num;

    /* TODO: currently only acocunts for IBL light source */
    glightinfo[tid].light = get_light(ri_render_get());
    glightinfo[tid].sample_index = 0;

    opt = ri_render_get()->context->option;

    ri_vector_copy(glightinfo[tid].N, N);
    ri_ortho_basis(glightinfo[tid].basis, N);

    nsamples = opt->narealight_rays;

    ntheta = nsamples / 3.0;
    ntheta = (int)sqrt((double)ntheta);
    if (ntheta < 1) ntheta = 1;
    nphi   = 3 * ntheta;

    glightinfo[tid].nsamples = ntheta * nphi;

    for (j = 0; j < nphi; j++) {
        for (i = 0; i < ntheta; i++) {
            theta = sqrt(((double)i + randomMT()) / (double)ntheta);
            phi   = 2.0 * M_PI * ((double)j + randomMT())
                  / (double)nphi;

            dir[0] = cos(phi) * theta;
            dir[1] = sin(phi) * theta;
            dir[2] = sqrt(1.0 - theta * theta);
        
            //assert(!isnan(dir[0]));
            //assert(!isnan(dir[1]));
            //assert(!isnan(dir[2]));

            for (k = 0; k < 3; k++) {
                ldir[k] = dir[0] * glightinfo[tid].basis[0][k]
                        + dir[1] * glightinfo[tid].basis[1][k]
                        + dir[2] * glightinfo[tid].basis[2][k];
            }
            ldir[3] = 0.0;

            ri_vector_normalize(ldir);

            ri_vector_copy(glightinfo[tid].samples[count].L, ldir);

            /* get light color from IBL image */
            ri_texture_ibl_fetch(
                        glightinfo[tid].samples[count].Cl,
                        glightinfo[tid].light->texture,
                        ldir);

#if defined(LINUX) || defined(__APPLE__)
            if (isnan(glightinfo[tid].samples[count].Cl[0])) {
                printf("ldir = ");
                ri_vector_print(ldir);
            }
#endif

            /* scacle light power */
            ri_vector_scale(glightinfo[tid].samples[count].Cl,
                            glightinfo[tid].samples[count].Cl,
                            1.0f / (ri_float_t)glightinfo[tid].nsamples);

            /* Ol is not yet implemented */
            ri_vector_setzero(glightinfo[tid].samples[count].Ol);

            count++;
        }    
    }
}
