/*
 * How to write shader builtin function.
 *
 * 
 * - Types
 *
 *   All vector type is replaced with float4.
 *   Matrix type is replaced with float16.
 *   float -> float.
 *
 * - Naming rule
 * 
 *   float length(vector V) -> length_fv
 *
 *   (void type is ignored)
 *
 * - ABI
 *
 *   Return type is the first argument of the function through pointer.
 *
 *   float length(vector V) -> length_fv(float *ret, float4 V)
 */
 
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "texture.h"
#include "hbuffer.h"

// clang specific type definition.
// You can't comple this source code with other C compilers.
typedef float float4 __attribute__((ext_vector_type(4)));

//texture_t *mytex;

extern texture_t *get_texture();
extern void bora(); // defined in the renderer side

//extern void texture_map(float *out, float u, float v);

extern float noise2(float u, float v);
extern float noise3(float u, float v, float w);

extern float  rsl_gets();
extern float  rsl_gett();
extern float4 rsl_getL();
extern float4 rsl_getCl();

extern void lse_save_cache_iiic(int, int, int, float *);
extern void lse_load_cache_iiic(int, int, int, float *);

#define vdot(a, b) (a[0] * b[0] + a[1] * b[1] + a[2] * b[2])

static void vnormalize(float4 *v)
{
    float4 vv = (*v);

    float len = vv[0] * vv[0] + vv[1] * vv[1] + vv[2] * vv[2];
    float invlen;

    if (len > 1.0e-6f) {
        invlen = 1.0f / sqrtf(len);
        vv[0] *= invlen;
        vv[1] *= invlen;
        vv[2] *= invlen;
    }

    (*v) = vv;
}

void
normalize_vv(float4 *ret, float4 V)
{
    float4 r;

    float len = vdot(V, V);
    float invlen;

    if (len > 1.0e-6f) {
        invlen = 1.0f / sqrtf(len);
        r.x = V.x * invlen;
        r.y = V.y * invlen;
        r.z = V.z * invlen;
    }

    (*ret) = r;

}

void
normalize_vn(float4 *ret, float4 V)
{
    float4 r;

    float len = vdot(V, V);
    float invlen;

    if (len > 1.0e-6f) {
        invlen = 1.0f / sqrtf(len);
        r.x = invlen * V.x;
        r.y = invlen * V.y;
        r.z = invlen * V.z;
    }

    (*ret) = r;

}

void
length_fv(float *ret, float4 V)
{
    float r;

    float l = vdot(V, V);

    r = sqrtf(l);

    (*ret) = r;
}

void
ambient_c(float4 *ret)
{
    float4 amb;

    //printf("ambient\n");
    amb[0] = 0.1f;
    amb[1] = 0.1f;
    amb[2] = 0.1f;
    amb[3] = 0.1f;

    //bora();

    (*ret) = amb;
}

void
diffuse_cn(float4 *ret, float4 N)
{
    float4 L;
    float NdotL;
    float4 rr;

    L = rsl_getL();

    vnormalize(&L);

    NdotL = vdot(N, L);
    if (NdotL < 0.0f) {
        NdotL = 0.0f;
    }

    //printf("diffuse. L = %f, %f, %f\n", L[0], L[1], L[2]);
    //printf("diffuse. N = %f, %f, %f\n", N[0], N[1], N[2]);
    //printf("diffuse. NdotL = %f\n", NdotL);

    rr[0] = NdotL;
    rr[1] = NdotL;
    rr[2] = NdotL;
    rr[3] = NdotL;
    (*ret) = rr;
}

void
reflect_vvv(float4 *ret, float4 I, float4 N)
{
    float IdotN;
    float4 r;

    IdotN = vdot(I, N);

    r = I - 2.0f * IdotN * N;
    
    (*ret) =r;
}

void
faceforward_vvv(float4 *ret, float4 N, float4 I)
{
    float4 r;
    float negIdotN;
    float sign;

    negIdotN = -vdot(I, N);        // FIXME: replace N with Ng

    // FIXME: use copysign()?
    sign = (negIdotN >= 0.0f) ? 1.0f : -1.0f;

    r = sign * N;

    (*ret) = r;

}

void
faceforward_vnv(float4 *ret, float4 N, float4 I)
{
    float4 r;
    float negIdotN;
    float sign;

    negIdotN = -vdot(I, N);        // FIXME: replace N with Ng

    // FIXME: use copysign()?
    sign = (negIdotN >= 0.0f) ? 1.0f : -1.0f;

    r = sign * N;

    (*ret) = r;

}

void
texture_cs(float4 *ret, char *texname)
{
    float s = rsl_gets();
    float t = rsl_gett();

    float texcol[4];
    float4 col4;

    //printf("texname = [%s], s = %f\n", texname, s);

    texture_t *tex = get_texture(); // defined in the renderer side.
    texture_map(texcol, tex, s, t);

    col4.x = texcol[0];
    col4.y = texcol[1];
    col4.z = texcol[2];

    //printf("col = %f, %f, %f, uv = %f, %f\n", texcol[0], texcol[1], texcol[2], s, t);


    (*ret) = col4;
    
}

void
noise_fff(float *ret, float u, float v)
{
    float r;
    r = noise2(u, v);

    (*ret) = r;
}

void
noise_fp(float *ret, float4 p)
{
    float r;

    r = noise3(p.x, p.y, p.z);
    
    (*ret) = r;
}

void
specularbrdf_cvnvf(float4 *ret, float4 L, float4 N, float4 V, float roughness)
{
    float4 r;

    float4 H = L + V;
    vnormalize(&H);

    float NdotH = vdot(N, H);
    if (NdotH < 0.0f) NdotH = 0.0f;

    float val = powf(NdotH, 1.0f / roughness);

    r.x = val;
    r.y = val;
    r.z = val;

    (*ret) = r;

}

void
specular_cnvf(float4 *ret, float4 N, float4 V, float roughness)
{
    // TODO: Calculate contribution from lights in the scene.
  
    float4 Cl = rsl_getCl();
    float4 L  = rsl_getL();
    vnormalize(&L);

    float4 r;
    float4 specular_col;

    specularbrdf_cvnvf(&specular_col, L, N, V, roughness);

    r.x = Cl.x * specular_col.x;
    r.y = Cl.y * specular_col.y;
    r.z = Cl.z * specular_col.z;

    (*ret) = r;
}

/* ---------------------------------------------------------------------------
 *
 * LSE(lucille shader engine) specific functions.
 *
 * ------------------------------------------------------------------------ */

void
save_cache_iiic(int layer, int x, int y, float4 val)
{
    float buf[4];

    buf[0] = val.x;
    buf[1] = val.y;
    buf[2] = val.z;
    buf[3] = val.w;

    lse_save_cache_iiic(layer, x, y, buf);

}

void
load_cache_ciii(float4 *ret, int layer, int x, int y)
{
    float *p = (float *)ret;

    lse_load_cache_iiic(layer, x, y, p);

}
