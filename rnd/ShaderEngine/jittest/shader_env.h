
#ifndef SHADER_ENV_H
#define SHADER_ENV_H

#ifdef __cplusplus
extern "C" {
#endif

typedef float float4[4];

typedef struct _ri_shader_env_t {
    float4 Ci;
    float4 Oi;
    float4 Cs;
    float4 Os;
    float4 I;
    float4 N;
    float4 Ng;
    float4 E;
    float4 P;
    float u;
    float v;
    float s;
    float t;
    int x;
    int y;
    int z;
    int w;
    float4 L;
    float4 Cl;
    float4 Ol;
} ri_shader_env_t;


#ifdef __cplusplus
}
#endif

#endif

