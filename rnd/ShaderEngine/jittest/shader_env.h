
#ifndef SHADER_ENV_H
#define SHADER_ENV_H

#ifdef __cplusplus
extern "C" {
#endif

typedef float float4[4];

typedef struct _ri_shader_env_t {
    float4 I;
    float4 N;
    float4 Cs;
    float4 Ci;
    float4 Oi;
} ri_shader_env_t;


#ifdef __cplusplus
}
#endif

#endif

