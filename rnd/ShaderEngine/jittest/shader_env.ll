%struct.ri_shader_env_t = type { <4xfloat>, <4xfloat>, <4xfloat>, <4xfloat>, <4xfloat>, <4xfloat>, <4xfloat>, <4xfloat> }
@genv = internal global %struct.ri_shader_env_t zeroinitializer, align 32

define <4xfloat> @getCi() nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 0
    %tmp1 = load <4xfloat>* %tmp
    ret <4xfloat> %tmp1
}

define void @setCi(<4xfloat> %val) nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 0
    store <4xfloat> %val, <4xfloat>* %tmp
    ret void
}

define <4xfloat> @getOi() nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 1
    %tmp1 = load <4xfloat>* %tmp
    ret <4xfloat> %tmp1
}

define void @setOi(<4xfloat> %val) nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 1
    store <4xfloat> %val, <4xfloat>* %tmp
    ret void
}

define <4xfloat> @getCs() nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 2
    %tmp1 = load <4xfloat>* %tmp
    ret <4xfloat> %tmp1
}

define <4xfloat> @getI() nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 3
    %tmp1 = load <4xfloat>* %tmp
    ret <4xfloat> %tmp1
}

define <4xfloat> @getN() nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 4
    %tmp1 = load <4xfloat>* %tmp
    ret <4xfloat> %tmp1
}

define <4xfloat> @getNg() nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 5
    %tmp1 = load <4xfloat>* %tmp
    ret <4xfloat> %tmp1
}

define <4xfloat> @getE() nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 6
    %tmp1 = load <4xfloat>* %tmp
    ret <4xfloat> %tmp1
}

define <4xfloat> @getP() nounwind {
    %tmp = getelementptr %struct.ri_shader_env_t* @genv, i32 0, i32 7
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

    ret void
}

