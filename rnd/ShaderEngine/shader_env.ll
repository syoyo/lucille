%struct.ri_shader_env_t = type { <4 x float>, <4 x float>, <4 x float>, <4 x float>, <4 x float>, <4 x float>, <4 x float>, <4 x float>, float, float, float, float, i32, i32, i32, i32, <4 x float>, <4 x float>, <4 x float> }
@genv = internal global %struct.ri_shader_env_t zeroinitializer, align 32

define <4 x float> @rsl_getCi() nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 0
    %tmp1 = load <4 x float>* %tmp
    ret <4 x float> %tmp1
}

define void @rsl_setCi(<4 x float> %val) nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 0
    store <4 x float> %val, <4 x float>* %tmp
    ret void
}

define <4 x float> @rsl_getOi() nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 1
    %tmp1 = load <4 x float>* %tmp
    ret <4 x float> %tmp1
}

define void @rsl_setOi(<4 x float> %val) nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 1
    store <4 x float> %val, <4 x float>* %tmp
    ret void
}

define <4 x float> @rsl_getCs() nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 2
    %tmp1 = load <4 x float>* %tmp
    ret <4 x float> %tmp1
}

define <4 x float> @rsl_getI() nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 3
    %tmp1 = load <4 x float>* %tmp
    ret <4 x float> %tmp1
}

define <4 x float> @rsl_getN() nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 4
    %tmp1 = load <4 x float>* %tmp
    ret <4 x float> %tmp1
}

define <4 x float> @rsl_getNg() nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 5
    %tmp1 = load <4 x float>* %tmp
    ret <4 x float> %tmp1
}

define <4 x float> @rsl_getE() nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 6
    %tmp1 = load <4 x float>* %tmp
    ret <4 x float> %tmp1
}

define <4 x float> @rsl_getP() nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 7
    %tmp1 = load <4 x float>* %tmp
    ret <4 x float> %tmp1
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

define i32 @rsl_getx() nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 12
    %tmp1 = load i32* %tmp
    ret i32 %tmp1
}

define i32 @rsl_gety() nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 13
    %tmp1 = load i32* %tmp
    ret i32 %tmp1
}

define i32 @rsl_getz() nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 14
    %tmp1 = load i32* %tmp
    ret i32 %tmp1
}

define i32 @rsl_getw() nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 15
    %tmp1 = load i32* %tmp
    ret i32 %tmp1
}

define <4 x float> @rsl_getL() nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 16
    %tmp1 = load <4 x float>* %tmp
    ret <4 x float> %tmp1
}

define <4 x float> @rsl_getCl() nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 17
    %tmp1 = load <4 x float>* %tmp
    ret <4 x float> %tmp1
}

define <4 x float> @rsl_getOl() nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 18
    %tmp1 = load <4 x float>* %tmp
    ret <4 x float> %tmp1
}

define void @set_shader_env(%struct.ri_shader_env_t* %env) {

    %srcaddr0 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 0
    %tmp0     = load <4 x float>* %srcaddr0 
    %dstaddr0 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 0
    store <4 x float> %tmp0, <4 x float>* %dstaddr0 


    %srcaddr1 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 1
    %tmp1     = load <4 x float>* %srcaddr1 
    %dstaddr1 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 1
    store <4 x float> %tmp1, <4 x float>* %dstaddr1 


    %srcaddr2 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 2
    %tmp2     = load <4 x float>* %srcaddr2 
    %dstaddr2 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 2
    store <4 x float> %tmp2, <4 x float>* %dstaddr2 


    %srcaddr3 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 3
    %tmp3     = load <4 x float>* %srcaddr3 
    %dstaddr3 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 3
    store <4 x float> %tmp3, <4 x float>* %dstaddr3 


    %srcaddr4 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 4
    %tmp4     = load <4 x float>* %srcaddr4 
    %dstaddr4 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 4
    store <4 x float> %tmp4, <4 x float>* %dstaddr4 


    %srcaddr5 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 5
    %tmp5     = load <4 x float>* %srcaddr5 
    %dstaddr5 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 5
    store <4 x float> %tmp5, <4 x float>* %dstaddr5 


    %srcaddr6 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 6
    %tmp6     = load <4 x float>* %srcaddr6 
    %dstaddr6 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 6
    store <4 x float> %tmp6, <4 x float>* %dstaddr6 


    %srcaddr7 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 7
    %tmp7     = load <4 x float>* %srcaddr7 
    %dstaddr7 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 7
    store <4 x float> %tmp7, <4 x float>* %dstaddr7 


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
    %tmp12     = load i32* %srcaddr12 
    %dstaddr12 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 12
    store i32 %tmp12, i32* %dstaddr12 


    %srcaddr13 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 13
    %tmp13     = load i32* %srcaddr13 
    %dstaddr13 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 13
    store i32 %tmp13, i32* %dstaddr13 


    %srcaddr14 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 14
    %tmp14     = load i32* %srcaddr14 
    %dstaddr14 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 14
    store i32 %tmp14, i32* %dstaddr14 


    %srcaddr15 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 15
    %tmp15     = load i32* %srcaddr15 
    %dstaddr15 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 15
    store i32 %tmp15, i32* %dstaddr15 


    %srcaddr16 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 16
    %tmp16     = load <4 x float>* %srcaddr16 
    %dstaddr16 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 16
    store <4 x float> %tmp16, <4 x float>* %dstaddr16 


    %srcaddr17 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 17
    %tmp17     = load <4 x float>* %srcaddr17 
    %dstaddr17 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 17
    store <4 x float> %tmp17, <4 x float>* %dstaddr17 


    %srcaddr18 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 18
    %tmp18     = load <4 x float>* %srcaddr18 
    %dstaddr18 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 18
    store <4 x float> %tmp18, <4 x float>* %dstaddr18 

    ret void
}

define void @get_shader_env(%struct.ri_shader_env_t* %env) {

    %srcaddr0 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 0
    %tmp0     = load <4 x float>* %srcaddr0 
    %dstaddr0 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 0
    store <4 x float> %tmp0, <4 x float>* %dstaddr0 


    %srcaddr1 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 1
    %tmp1     = load <4 x float>* %srcaddr1 
    %dstaddr1 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 1
    store <4 x float> %tmp1, <4 x float>* %dstaddr1 


    %srcaddr2 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 2
    %tmp2     = load <4 x float>* %srcaddr2 
    %dstaddr2 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 2
    store <4 x float> %tmp2, <4 x float>* %dstaddr2 


    %srcaddr3 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 3
    %tmp3     = load <4 x float>* %srcaddr3 
    %dstaddr3 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 3
    store <4 x float> %tmp3, <4 x float>* %dstaddr3 


    %srcaddr4 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 4
    %tmp4     = load <4 x float>* %srcaddr4 
    %dstaddr4 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 4
    store <4 x float> %tmp4, <4 x float>* %dstaddr4 


    %srcaddr5 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 5
    %tmp5     = load <4 x float>* %srcaddr5 
    %dstaddr5 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 5
    store <4 x float> %tmp5, <4 x float>* %dstaddr5 


    %srcaddr6 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 6
    %tmp6     = load <4 x float>* %srcaddr6 
    %dstaddr6 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 6
    store <4 x float> %tmp6, <4 x float>* %dstaddr6 


    %srcaddr7 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 7
    %tmp7     = load <4 x float>* %srcaddr7 
    %dstaddr7 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 7
    store <4 x float> %tmp7, <4 x float>* %dstaddr7 


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
    %tmp12     = load i32* %srcaddr12 
    %dstaddr12 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 12
    store i32 %tmp12, i32* %dstaddr12 


    %srcaddr13 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 13
    %tmp13     = load i32* %srcaddr13 
    %dstaddr13 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 13
    store i32 %tmp13, i32* %dstaddr13 


    %srcaddr14 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 14
    %tmp14     = load i32* %srcaddr14 
    %dstaddr14 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 14
    store i32 %tmp14, i32* %dstaddr14 


    %srcaddr15 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 15
    %tmp15     = load i32* %srcaddr15 
    %dstaddr15 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 15
    store i32 %tmp15, i32* %dstaddr15 


    %srcaddr16 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 16
    %tmp16     = load <4 x float>* %srcaddr16 
    %dstaddr16 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 16
    store <4 x float> %tmp16, <4 x float>* %dstaddr16 


    %srcaddr17 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 17
    %tmp17     = load <4 x float>* %srcaddr17 
    %dstaddr17 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 17
    store <4 x float> %tmp17, <4 x float>* %dstaddr17 


    %srcaddr18 = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 18
    %tmp18     = load <4 x float>* %srcaddr18 
    %dstaddr18 = getelementptr %struct.ri_shader_env_t* %env, i32 0, i32 18
    store <4 x float> %tmp18, <4 x float>* %dstaddr18 

    ret void
}

