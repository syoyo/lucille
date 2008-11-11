/*
 * Shader interface between renderer and DLL C language shader.
 *
 * $Id: shader.h,v 1.9 2004/06/23 11:13:04 syoyo Exp $
 */
#ifndef SHADER_H
#define SHADER_H

#include <math.h>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "vector.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifdef WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT 
#endif

typedef struct _ri_state_t {

    /*
     * global
     */
    int         version;

    int         qmc_instance;   /* for QMC                  */
    float       x, y;           /* raster pos               */    

    /*
     * ray
     */
    char        face;           /* 'f'ront, 'b'ack,         */

    RtVector    org;            /* ray origin               */
    RtVector    dir;            /* ray dir                  */

    RtVector    n;              /* normal                   */
    RtVector    dndu;           /* tangnet                  */
    RtVector    dndv;           /* binormal                 */

    RtVector    dxdu;           /* first deriv
    RtVector    dxdv;            *
    RtVector    dx2du;           * second deriv                
    RtVector    dx2dv;           *                          */

    RtVector    tex;            /* texture coord            */

    RtFloat     bary[4];        /* barycentric coord        */

    RtVector    motion;         /* motion vector            */
    RtFloat     time;           /* time of ray              */

    /*
     * other 
     */
    unsigned int  thread;       /* thread number            */
    void         *user;         /* user data                */
    int           user_size;    /* size of user data        */
} ri_state_t;

#if 0
typedef enum {riFALSE=0, riTRUE=1}    riBoolean;
typedef int                riInteger;

typedef union {void *p; double d;}    riPointer;    /* 8-byte */
typedef float                riScalar;
typedef struct _riVector;
{
    riScalar x, y, z, w;
} riVector;    
#endif

#define ri_color_t ri_vector_t


/* shader output variables */
typedef struct _ri_output_t
{
    ri_color_t Ci;
    ri_color_t Oi;
} ri_output_t;

/* shader input variables */
typedef struct _ri_input_t
{
    ri_color_t  Os;
    ri_color_t  Cs;
    ri_vector_t P;
    ri_vector_t N;
    ri_vector_t Ng;
    ri_vector_t dPdu;
    ri_vector_t dPdv;
    ri_vector_t I;
    ri_vector_t L;
    ri_vector_t E;
    float       s, t;

    /* TODO: Implement those variables. */
#if 0
    float       u, v;    /* surface parameters            */
    float       du, dv;    /* change in surface parameters        */
    
    ri_vector_t Ng;        /* surface geometric normal        */
    ri_vector_t L;        /* incoming light ray direction        */
    ri_vector_t CI;        /* incoming light ray color        */
    ri_vector_t OI;        /* incoming light ray opacity        */
    float       ncomps;    /* number of color components        */
    float       time;    /* current shutter time            */
    float       dtime;    /* the amount of time covered by this
                 * shading sample            */
    ri_vector_t dPdtime;    /* how the surface position P is
                 * changing per unit time, as described
                 * by motion blur in the scene        */
#endif
} ri_input_t;

/* shader input state */
typedef struct _ri_status_t
{
    /* RenderMan compatible variables */

    ri_input_t    input;    

    /* Global variables */

    int           thread_num;    /* thread number        */

    //ri_render_t  *render;        /* Pointer to the renderer internal */

    unsigned int  qmc_instance;    /* Instance number of low discrepancy
                     * sequence associated to current
                     * ray tree.
                     */ 
                    
    /* Ray variables */

    ri_vector_t   org;        /* Ray origin            */
    ri_vector_t   dir;        /* Ray direction        */

    int           ray_depth;    /* tracing depth        */
    
    /* Intersect variables */
    //ri_geom_t    *geom;        /* Pointer to geometry info    */
} ri_status_t;

/* light source data structure used for illuminance loop. */
typedef struct _ri_lightsource_t
{
    ri_vector_t L;          /* light direction    */
    ri_vector_t Cl;         /* light color        */
    ri_vector_t Ol;         /* light opacity    */
} ri_lightsource_t;

#define PARAMHASH_SIZE 131

typedef struct _ri_paramnode_t
{
    char                   *name;
    int                     len;
    int                     type;
    void                   *val;
    int                     size;

    struct _ri_paramnode_t *next;
} ri_paramnode_t;

/* shader local parameter */
typedef struct _ri_parameter_t
{
    ri_paramnode_t *paramnodes[PARAMHASH_SIZE];
} ri_parameter_t;

typedef void (*ri_shader_initparam_proc)(ri_parameter_t *param);
typedef void (*ri_shader_proc)(ri_output_t *output,
                               ri_status_t *status,
                               ri_parameter_t *param);

/* shader structure. */
typedef struct _ri_shader_t 
{
    ri_shader_initparam_proc  initparamproc;
    ri_shader_proc            shaderproc;        
    ri_parameter_t           *param;
} ri_shader_t;

#define TYPEVECTOR 0
#define TYPESTRING 1 
#define TYPEFLOAT  2 

extern void ri_shader_exec(ri_shader_t *shader);
extern ri_shader_t *ri_shader_dup(const ri_shader_t *src);

//extern void shader_set(shader_initparamproc initparam, shaderproc shader);

extern void ri_status_set(ri_status_t *status);

extern DLLEXPORT ri_parameter_t *ri_param_new ();
extern DLLEXPORT void            ri_param_free(ri_parameter_t *param);
extern DLLEXPORT ri_parameter_t *ri_param_dup (const ri_parameter_t *param);

extern DLLEXPORT void ri_param_eval(
                        void                 *dst,
                        const ri_parameter_t *param,
                        const char           *name);

extern DLLEXPORT void ri_param_add(
                        ri_parameter_t       *param,
                        const char           *name,
                        int                   type,
                        const void           *val);

extern DLLEXPORT void ri_param_override(
                        ri_parameter_t       *param,
                        const char           *name,
                        const void           *val);

/*
 * shader builtin functions.
 */


/* vector and matrix functions */
extern DLLEXPORT void faceforward(
                        ri_vector_t        dst,
                        const ri_vector_t  N,
                        const ri_vector_t  I);

extern DLLEXPORT void normalize(
                        ri_vector_t        dst,
                        const ri_vector_t  N);

extern DLLEXPORT void reflect(
                        ri_vector_t        dst,
                        const ri_vector_t  I,
                        const ri_vector_t  N);

extern DLLEXPORT void refract(
                        ri_vector_t        dst,
                        const ri_vector_t  I,
                        const ri_vector_t  N,
                        float eta);

extern DLLEXPORT void transform(
                        ri_vector_t        dst,
                        const char        *tospace,
                        const ri_vector_t  src);

extern DLLEXPORT void vtransform(
                        ri_vector_t        dst,
                        const char        *from,
                        const char        *to,
                        const ri_vector_t  src);

/* shading functions. all shading function needs ri_status_t argument. */
extern DLLEXPORT void ambient(
                        const ri_status_t *status,
                        ri_color_t         dst);

extern DLLEXPORT void diffuse(
                        const ri_status_t *status,
                        ri_color_t         dst,
                        const ri_vector_t  N);

extern DLLEXPORT void specular(
                        const ri_status_t *status,
                        ri_color_t         dst,
                        const ri_vector_t  N,
                        const ri_vector_t  V,
                        float              roughness);

extern DLLEXPORT void texture(
                        const ri_status_t *status,
                        ri_color_t         dst,
                        const char        *name);

/* texture() with coordinates */
extern DLLEXPORT void texture_coords(
                        const ri_status_t *status,
                        ri_color_t         dst,
                        const char        *name,
                        const ri_vector_t  coords);

extern DLLEXPORT void environment(
                        const ri_status_t *status,
                        ri_color_t         dst,
                        const char        *name,
                        const ri_vector_t  coords);

extern DLLEXPORT float occlusion(
                        const ri_status_t *status,
                        const ri_vector_t  P,
                        const ri_vector_t  N,
                        float              nsamples);

extern DLLEXPORT void trace(
                        const ri_status_t *status,
                        ri_vector_t        dst,
                        const ri_vector_t  P,
                        const ri_vector_t  R);

/* mathematical functions */

#define radians(x) (x) * 3.141592f / 180.0f
#define degrees(x) (x) * 180.0f / 3.141592f

extern DLLEXPORT float inversesqrt(float x);
extern DLLEXPORT float mod(float a, float b);
extern DLLEXPORT float minf(float a, float b);
extern DLLEXPORT float clampf(float a, float min, float max);

extern DLLEXPORT float noise3d(const ri_vector_t v);
extern DLLEXPORT float noise1d(float f);
extern DLLEXPORT float step(float min, float value);
extern DLLEXPORT float smoothstep(float min, float max, float value);
extern DLLEXPORT void  mixv(ri_vector_t       dst,
                            const ri_vector_t x,
                            const ri_vector_t y,
                            float             alpha);

/* geometric functions */
extern DLLEXPORT float area   (const ri_vector_t P);
extern DLLEXPORT float distant(const ri_vector_t p1,
                               const ri_vector_t p2);
extern DLLEXPORT float depth  (const ri_vector_t P);

/* used for implementing illuminance() loop */
//extern DLLEXPORT void              init_lightsource(const ri_vector_t N);
extern DLLEXPORT ri_lightsource_t *next_lightsource(
                                const ri_status_t *status,
                                const ri_vector_t P,
                                const ri_vector_t N,
                                ri_float_t        angle);

#define length(v) ri_vector_length((v))
#define log_base(x, y) log((y)) / log((x))

#define sign(x) ((x) > 0.0) ? 1 : (((x) < 0.0) : -1 : 0)

#if 0
/* Vector component-wise access function is defined in vector.h */
#define xcomp(src) (src).e[0]
#define ycomp(src) (src).e[1]
#define zcomp(src) (src).e[2]
#define wcomp(src) (src).e[3]
#endif

#ifdef __cplusplus
}    /* extern "C" */
#endif

#endif
