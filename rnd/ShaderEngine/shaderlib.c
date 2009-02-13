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
#include "noise.h"
#include "hbuffer.h"

// clang specific type definition.
// You can't comple this source code with other C compilers.
typedef float float4 __attribute__((ext_vector_type(4)));

//texture_t *mytex;

extern texture_t *get_texture();
extern void bora(); // defined in the renderer side

//extern void texture_map(float *out, float u, float v);

//extern float noise1(float u);
//extern float noise2(float u, float v);
//extern float noise3(float u, float v, float w);

extern float  rsl_gets();
extern float  rsl_gett();
extern float4 rsl_getL();
extern float4 rsl_getCl();

extern void lse_save_cache_iiic(int, int, int, float *);
extern void lse_load_cache_iiic(int, int, int, float *);

/* ---------------------------------------------------------------------------
 *
 * Constant
 *
 * ------------------------------------------------------------------------- */

float rsl_getPI()
{
    return M_PI;
}

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
diffuse_cp(float4 *ret, float4 P)
{
    diffuse_cn(ret, P);
}

void
diffuse_cv(float4 *ret, float4 v)
{
    diffuse_cn(ret, v);
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
noise_ff(float *ret, float f)
{
    float r;

    printf("noise_ff\n");
    printf("noise1 addr = %p\n", noise1);

    r = noise1(f);

    printf("noise1() call ok\n");
    
    (*ret) = r;
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

void
specular_cppf(float4 *ret, float4 N, float4 V, float roughness)
{
    specular_cnvf(ret, N, V, roughness);
}

void
transform_psp(float4 *ret, char *space, float4 from)
{
    // TODO:
    (*ret) = from;
}

/* ---------------------------------------------------------------------------
 *
 * Math functions
 *
 * ------------------------------------------------------------------------- */

void
abs_ff(float *ret, float f)
{
    float r;

    r = fabsf(f);
    

    (*ret) = r;
}

void
floor_ff(float *ret, float f)
{
    float r;

    r = floorf(f);

    (*ret) = r;
}

void
sqrt_ff(float *ret, float f)
{
    (*ret) = sqrtf(f);
}

void
smoothstep_ffff(float *ret, float minval, float maxval, float value)
{
    // (RI spec 3.2 says)
    // smoothstep returns 0 if value is less than min, 1 if value is greater
    // than or equal to max, and performs a smooth Hermite interpolation
    // between 0 and 1 in the interval min to max.

    float v;
    float f;

    if (value < minval) {

        v = 0.0f; 

    } else if (value > maxval) {

        v = 1.0f;

    } else {

        f = (value - minval) / (maxval - minval);
        v = f * f * (3.0f - 2.0f * f);
    }

    (*ret) = v;

}

// (RI spec 3.2 says)
// mix returns x * (1 - alpha) + y * alpha, that is, it performs a linear blend
// between values x and y . The types of x and y must be identical, but may 
// be any of float, point, vector, normal, or color. The variants that
// operate on colors or point-like objects operate on a component-by-component
// asis (e.g., separately for x, y, and z).
void
mix_cccf(float4 *ret, float4 x, float4 y, float alpha)
{
    float4 r;

    r = x * (1.0f - alpha) + y * alpha;

    (*ret) = r;

}

void
sin_ff(float *ret, float x)
{
    float f;

    f = sinf(x);

    (*ret) = f;

}

void
mod_fff(float *ret, float a, float b)
{
    // (RI spec 3.2 says)
    // mod returns a value greater than 0 and less than or equal to b
    // such that mod(a,b) = a - n*b for some integer n. abs returns
    // the absolute value of its argument and sign returns -1
    // if its argument is negative, 1 if its argument is positive, 
    // and 0 if its argument is zero
    
    float f;

    f = fmodf(a, b);

    (*ret) = f;
}


/* ---------------------------------------------------------------------------
 *
 * Geometric functions
 *
 * ------------------------------------------------------------------------- */
void
area_fp(float* ret, float4 p)
{
    // FIXME
    (*ret) = 1.0f;
}

/* ---------------------------------------------------------------------------
 *
 *
 *
 * ------------------------------------------------------------------------- */

void
xcomp_fp(float *ret, float4 v)
{
    (*ret) = v[0];
}

void
ycomp_fp(float *ret, float4 v)
{
    (*ret) = v[1];
}

void
zcomp_fp(float *ret, float4 v)
{
    (*ret) = v[2];
}

void
wcomp_fp(float *ret, float4 v)
{
    // TODO:
    (*ret) = v[3];
}

// hack
void
turb_cp(float4 *ret, float4 p)
{
    float sum   = 0.0f;
    float octave;
    float a     = 0.0001f;
    float scale = 2.0f;
    float4 r;

    float x, y, z;

    while (a < scale) {
        x = p.x / scale;
        y = p.y / scale;
        z = p.z / scale;
        //octave = scale * noise3(x, y, z);
        octave = noise3(x, y, z);
        sum = sum + octave;
        scale *= 0.5f;
    }

    r.x = sum;
    r.y = sum;
    r.z = sum;

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
