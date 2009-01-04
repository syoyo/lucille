%struct.ri_shader_env_t = type { <4xfloat>, <4xfloat>, <4xfloat>, <4xfloat>, <4xfloat>, <4xfloat>, <4xfloat>, <4xfloat>, float, float, float, float, <4xfloat>, <4xfloat>, <4xfloat> }
@genv = internal global %struct.ri_shader_env_t zeroinitializer, align 32

define <4xfloat> @rsl_getCi() nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 0
    %tmp1 = load <4xfloat>* %tmp
    ret <4xfloat> %tmp1
}

define void @rsl_setCi(<4xfloat> %val) nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 0
    store <4xfloat> %val, <4xfloat>* %tmp
    ret void
}

define <4xfloat> @rsl_getOi() nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 1
    %tmp1 = load <4xfloat>* %tmp
    ret <4xfloat> %tmp1
}

define void @rsl_setOi(<4xfloat> %val) nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 1
    store <4xfloat> %val, <4xfloat>* %tmp
    ret void
}

define <4xfloat> @rsl_getCs() nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 2
    %tmp1 = load <4xfloat>* %tmp
    ret <4xfloat> %tmp1
}

define <4xfloat> @rsl_getI() nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 3
    %tmp1 = load <4xfloat>* %tmp
    ret <4xfloat> %tmp1
}

define <4xfloat> @rsl_getN() nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 4
    %tmp1 = load <4xfloat>* %tmp
    ret <4xfloat> %tmp1
}

define <4xfloat> @rsl_getNg() nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 5
    %tmp1 = load <4xfloat>* %tmp
    ret <4xfloat> %tmp1
}

define <4xfloat> @rsl_getE() nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 6
    %tmp1 = load <4xfloat>* %tmp
    ret <4xfloat> %tmp1
}

define <4xfloat> @rsl_getP() nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 7
    %tmp1 = load <4xfloat>* %tmp
    ret <4xfloat> %tmp1
}

define float @rsl_getu() nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 8
    %tmp1 = load float* %tmp
    ret float %tmp1
}

define float @rsl_getv() nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 9
    %tmp1 = load float* %tmp
    ret float %tmp1
}

define float @rsl_gets() nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 10
    %tmp1 = load float* %tmp
    ret float %tmp1
}

define float @rsl_gett() nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 11
    %tmp1 = load float* %tmp
    ret float %tmp1
}

define <4xfloat> @rsl_getL() nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 12
    %tmp1 = load <4xfloat>* %tmp
    ret <4xfloat> %tmp1
}

define <4xfloat> @rsl_getCl() nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 13
    %tmp1 = load <4xfloat>* %tmp
    ret <4xfloat> %tmp1
}

define <4xfloat> @rsl_getOl() nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 14
    %tmp1 = load <4xfloat>* %tmp
    ret <4xfloat> %tmp1
}

define void @set_shader_env(%struct.ri_shader_env_t* %env) {

    %srcaddr0 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 0
    %tmp0     = load <4xfloat>* %srcaddr0 
    %dstaddr0 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 0
    store <4xfloat> %tmp0, <4xfloat>* %dstaddr0 


    %srcaddr1 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 1
    %tmp1     = load <4xfloat>* %srcaddr1 
    %dstaddr1 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 1
    store <4xfloat> %tmp1, <4xfloat>* %dstaddr1 


    %srcaddr2 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 2
    %tmp2     = load <4xfloat>* %srcaddr2 
    %dstaddr2 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 2
    store <4xfloat> %tmp2, <4xfloat>* %dstaddr2 


    %srcaddr3 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 3
    %tmp3     = load <4xfloat>* %srcaddr3 
    %dstaddr3 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 3
    store <4xfloat> %tmp3, <4xfloat>* %dstaddr3 


    %srcaddr4 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 4
    %tmp4     = load <4xfloat>* %srcaddr4 
    %dstaddr4 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 4
    store <4xfloat> %tmp4, <4xfloat>* %dstaddr4 


    %srcaddr5 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 5
    %tmp5     = load <4xfloat>* %srcaddr5 
    %dstaddr5 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 5
    store <4xfloat> %tmp5, <4xfloat>* %dstaddr5 


    %srcaddr6 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 6
    %tmp6     = load <4xfloat>* %srcaddr6 
    %dstaddr6 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 6
    store <4xfloat> %tmp6, <4xfloat>* %dstaddr6 


    %srcaddr7 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 7
    %tmp7     = load <4xfloat>* %srcaddr7 
    %dstaddr7 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 7
    store <4xfloat> %tmp7, <4xfloat>* %dstaddr7 


    %srcaddr8 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 8
    %tmp8     = load float* %srcaddr8 
    %dstaddr8 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 8
    store float %tmp8, float* %dstaddr8 


    %srcaddr9 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 9
    %tmp9     = load float* %srcaddr9 
    %dstaddr9 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 9
    store float %tmp9, float* %dstaddr9 


    %srcaddr10 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 10
    %tmp10     = load float* %srcaddr10 
    %dstaddr10 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 10
    store float %tmp10, float* %dstaddr10 


    %srcaddr11 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 11
    %tmp11     = load float* %srcaddr11 
    %dstaddr11 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 11
    store float %tmp11, float* %dstaddr11 


    %srcaddr12 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 12
    %tmp12     = load <4xfloat>* %srcaddr12 
    %dstaddr12 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 12
    store <4xfloat> %tmp12, <4xfloat>* %dstaddr12 


    %srcaddr13 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 13
    %tmp13     = load <4xfloat>* %srcaddr13 
    %dstaddr13 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 13
    store <4xfloat> %tmp13, <4xfloat>* %dstaddr13 


    %srcaddr14 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 14
    %tmp14     = load <4xfloat>* %srcaddr14 
    %dstaddr14 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 14
    store <4xfloat> %tmp14, <4xfloat>* %dstaddr14 

    ret void
}

define void @get_shader_env(%struct.ri_shader_env_t* %env) {

    %srcaddr0 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 0
    %tmp0     = load <4xfloat>* %srcaddr0 
    %dstaddr0 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 0
    store <4xfloat> %tmp0, <4xfloat>* %dstaddr0 


    %srcaddr1 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 1
    %tmp1     = load <4xfloat>* %srcaddr1 
    %dstaddr1 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 1
    store <4xfloat> %tmp1, <4xfloat>* %dstaddr1 


    %srcaddr2 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 2
    %tmp2     = load <4xfloat>* %srcaddr2 
    %dstaddr2 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 2
    store <4xfloat> %tmp2, <4xfloat>* %dstaddr2 


    %srcaddr3 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 3
    %tmp3     = load <4xfloat>* %srcaddr3 
    %dstaddr3 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 3
    store <4xfloat> %tmp3, <4xfloat>* %dstaddr3 


    %srcaddr4 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 4
    %tmp4     = load <4xfloat>* %srcaddr4 
    %dstaddr4 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 4
    store <4xfloat> %tmp4, <4xfloat>* %dstaddr4 


    %srcaddr5 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 5
    %tmp5     = load <4xfloat>* %srcaddr5 
    %dstaddr5 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 5
    store <4xfloat> %tmp5, <4xfloat>* %dstaddr5 


    %srcaddr6 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 6
    %tmp6     = load <4xfloat>* %srcaddr6 
    %dstaddr6 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 6
    store <4xfloat> %tmp6, <4xfloat>* %dstaddr6 


    %srcaddr7 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 7
    %tmp7     = load <4xfloat>* %srcaddr7 
    %dstaddr7 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 7
    store <4xfloat> %tmp7, <4xfloat>* %dstaddr7 


    %srcaddr8 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 8
    %tmp8     = load float* %srcaddr8 
    %dstaddr8 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 8
    store float %tmp8, float* %dstaddr8 


    %srcaddr9 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 9
    %tmp9     = load float* %srcaddr9 
    %dstaddr9 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 9
    store float %tmp9, float* %dstaddr9 


    %srcaddr10 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 10
    %tmp10     = load float* %srcaddr10 
    %dstaddr10 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 10
    store float %tmp10, float* %dstaddr10 


    %srcaddr11 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 11
    %tmp11     = load float* %srcaddr11 
    %dstaddr11 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 11
    store float %tmp11, float* %dstaddr11 


    %srcaddr12 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 12
    %tmp12     = load <4xfloat>* %srcaddr12 
    %dstaddr12 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 12
    store <4xfloat> %tmp12, <4xfloat>* %dstaddr12 


    %srcaddr13 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 13
    %tmp13     = load <4xfloat>* %srcaddr13 
    %dstaddr13 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 13
    store <4xfloat> %tmp13, <4xfloat>* %dstaddr13 


    %srcaddr14 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 14
    %tmp14     = load <4xfloat>* %srcaddr14 
    %dstaddr14 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 14
    store <4xfloat> %tmp14, <4xfloat>* %dstaddr14 

    ret void
}

