#include <stdio.h>
#include <stdlib.h>
#include <math.h>

extern void bora(); // defined in the renderer side

// clang specific
typedef float float4 __attribute__((ext_vector_type(4)));

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

void ambient_c(float4 *ret)
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

void diffuse_cn(float4 *ret, float4 N)
{
    float4 L;
    float NdotL;
    float4 rr;

    L[0] = 1.0f;
    L[1] = 1.0f;
    L[2] = 1.0f;

    vnormalize(&L);

    NdotL = N[0] * L[0] + N[1] * L[1] + N[2] * L[2];

    //printf("diffuse. L = %f, %f, %f\n", L[0], L[1], L[2]);
    //printf("diffuse. N = %f, %f, %f\n", N[0], N[1], N[2]);
    //printf("diffuse. NdotL = %f\n", NdotL);

    rr[0] = NdotL;
    rr[1] = NdotL;
    rr[2] = NdotL;
    rr[3] = NdotL;
    (*ret) = rr;
}
