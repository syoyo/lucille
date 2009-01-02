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

extern void bora(); // defined in the renderer side

// clang specific
typedef float float4 __attribute__((ext_vector_type(4)));

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
        r = V * invlen;
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

    L[0] = 1.0f;
    L[1] = 1.0f;
    L[2] = 1.0f;

    vnormalize(&L);

    NdotL = vdot(N, L);

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

    r = sign * negIdotN * N;

    (*ret) = r;

}
