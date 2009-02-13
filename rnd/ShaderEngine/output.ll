declare <4 x float> @rsl_getCi()
declare <4 x float> @rsl_getOi()
declare <4 x float> @rsl_getCs()
declare <4 x float> @rsl_getOs()
declare <4 x float> @rsl_getP()
declare <4 x float> @rsl_getI()
declare <4 x float> @rsl_getE()
declare <4 x float> @rsl_getL()
declare <4 x float> @rsl_getN()
declare <4 x float> @rsl_getNg()
declare <4 x float> @rsl_getCl()
declare <4 x float> @rsl_getOl()
declare float @rsl_gets()
declare float @rsl_gett()
declare i32 @rsl_getx()
declare i32 @rsl_gety()
declare i32 @rsl_getz()
declare i32 @rsl_getw()
declare float @rsl_getPI()
declare void @rsl_setCi(<4 x float>)
declare void @rsl_setOi(<4 x float>)
declare void @radians_ff(float*, float);
declare void @degrees_ff(float*, float);
declare void @sin_ff(float*, float);
declare void @asin_ff(float*, float);
declare void @cos_ff(float*, float);
declare void @acos_ff(float*, float);
declare void @tan_ff(float*, float);
declare void @atan_ff(float*, float);
declare void @atan_fff(float*, float, float);
declare void @pow_fff(float*, float, float);
declare void @exp_ff(float*, float);
declare void @sqrt_ff(float*, float);
declare void @inversesqrt_ff(float*, float);
declare void @log_ff(float*, float);
declare void @log_fff(float*, float, float);
declare void @mod_fff(float*, float, float);
declare void @abs_ff(float*, float);
declare void @sign_ff(float*, float);
declare void @min_fff(float*, float, float);
declare void @max_fff(float*, float, float);
declare void @clamp_fff(float*, float, float);
declare void @mix_ffff(float*, float, float, float);
declare void @floor_ff(float*, float);
declare void @ceil_ff(float*, float);
declare void @round_ff(float*, float);
declare void @step_fff(float*, float, float);
declare void @smoothstep_ffff(float*, float, float, float);
declare void @random_f(float*);
declare void @random_c(<4 x float>*);
declare void @random_p(<4 x float>*);
declare void @noise_ff(float*, float);
declare void @noise_fp(float*, <4 x float>);
declare void @noise_fv(float*, <4 x float>);
declare void @noise_fn(float*, <4 x float>);
declare void @noise_fff(float*, float, float);
declare void @xcomp_fv(float*, <4 x float>);
declare void @xcomp_fp(float*, <4 x float>);
declare void @xcomp_fn(float*, <4 x float>);
declare void @ycomp_fv(float*, <4 x float>);
declare void @ycomp_fp(float*, <4 x float>);
declare void @ycomp_fn(float*, <4 x float>);
declare void @zcomp_fv(float*, <4 x float>);
declare void @zcomp_fp(float*, <4 x float>);
declare void @zcomp_fn(float*, <4 x float>);
declare void @length_fv(float*, <4 x float>);
declare void @normalize_vv(<4 x float>*, <4 x float>);
declare void @normalize_vn(<4 x float>*, <4 x float>);
declare void @distance_fpp(float*, <4 x float>, <4 x float>);
declare void @area_fp(float*, <4 x float>);
declare void @faceforward_vvv(<4 x float>*, <4 x float>, <4 x float>);
declare void @faceforward_vnv(<4 x float>*, <4 x float>, <4 x float>);
declare void @faceforward_vnp(<4 x float>*, <4 x float>, <4 x float>);
declare void @faceforward_vvp(<4 x float>*, <4 x float>, <4 x float>);
declare void @reflect_vvv(<4 x float>*, <4 x float>, <4 x float>);
declare void @reflect_vvn(<4 x float>*, <4 x float>, <4 x float>);
declare void @reflect_vvp(<4 x float>*, <4 x float>, <4 x float>);
declare void @refract_vvvf(<4 x float>*, <4 x float>, <4 x float>, float);
declare void @refract_vvnf(<4 x float>*, <4 x float>, <4 x float>, float);
declare void @fresnel_vvvfff(<4 x float>*, <4 x float>, <4 x float>, float, float, float);
declare void @transform_psp(<4 x float>*, i8*, <4 x float>);
declare void @transform_pssp(<4 x float>*, i8*, i8*, <4 x float>);
declare void @transform_pmp(<4 x float>*, <16 x float>, <4 x float>);
declare void @transform_psmp(<4 x float>*, i8*, <16 x float>, <4 x float>);
declare void @transform_vsv(<4 x float>*, i8*, <4 x float>);
declare void @transform_vssv(<4 x float>*, i8*, i8*, <4 x float>);
declare void @transform_vmv(<4 x float>*, <16 x float>, <4 x float>);
declare void @transform_vsmv(<4 x float>*, i8*, <16 x float>, <4 x float>);
declare void @transform_nsn(<4 x float>*, i8*, <4 x float>);
declare void @transform_nssn(<4 x float>*, i8*, i8*, <4 x float>);
declare void @transform_nmn(<4 x float>*, <16 x float>, <4 x float>);
declare void @transform_nsmn(<4 x float>*, i8*, <16 x float>, <4 x float>);
declare void @vtransform_vssv(<4 x float>*, i8*, i8*, <4 x float>);
declare void @depth_fp(float*, <4 x float>);
declare void @depth_fp(float*, <4 x float>);
declare void @comp_fcf(float*, <4 x float>, float);
declare void @mix_cccf(<4 x float>*, <4 x float>, <4 x float>, float);
declare void @ambient_c(<4 x float>*);
declare void @diffuse_cn(<4 x float>*, <4 x float>);
declare void @diffuse_cp(<4 x float>*, <4 x float>);
declare void @diffuse_cv(<4 x float>*, <4 x float>);
declare void @specular_cnvf(<4 x float>*, <4 x float>, <4 x float>, float);
declare void @specular_cppf(<4 x float>*, <4 x float>, <4 x float>, float);
declare void @specularbrdf_cvnvf(<4 x float>*, <4 x float>, <4 x float>, <4 x float>, float);
declare void @phong_cnvf(<4 x float>*, <4 x float>, <4 x float>, float);
declare void @trace_cpp(<4 x float>*, <4 x float>, <4 x float>);
declare void @trace_cpv(<4 x float>*, <4 x float>, <4 x float>);
declare void @texture_cs(<4 x float>*, i8*);
declare void @environment_cs(<4 x float>*, i8*);
declare void @environment_csv(<4 x float>*, i8*, <4 x float>);
declare void @save_cache_iiic(i32, i32, i32, <4 x float>);
declare void @load_cache_ciii(<4 x float>*, i32, i32, i32);
declare void @turb_cp(<4 x float>*, <4 x float>);
declare void @occlusion_fpn(float*, <4 x float>, <4 x float>);
declare void @occlusion_fpnf(float*, <4 x float>, <4 x float>, float);


define void @matte() {
    %roughness = alloca float;
    %tmp1.tmp = alloca float;
    store float 0x3f847ae140000000 , float* %tmp1.tmp;
    %tmp1 = load float* %tmp1.tmp;
    store float %tmp1 , float* %roughness;
    %Kd = alloca float;
    %tmp2.tmp = alloca float;
    store float 0x3fe8000000000000 , float* %tmp2.tmp;
    %tmp2 = load float* %tmp2.tmp;
    store float %tmp2 , float* %Kd;
    %Nf = alloca <4 x float>;
    %tmp3 = call <4 x float> @rsl_getN();
    %tmp4.buf = alloca <4 x float>;
    call void @normalize_vn(<4 x float>* %tmp4.buf, <4 x float> %tmp3);
    %tmp4 = load <4 x float>* %tmp4.buf;
    %tmp5 = call <4 x float> @rsl_getI();
    %tmp6.buf = alloca <4 x float>;
    call void @faceforward_vvv(<4 x float>* %tmp6.buf, <4 x float> %tmp4, <4 x float> %tmp5);
    %tmp6 = load <4 x float>* %tmp6.buf;
    store <4 x float> %tmp6 , <4 x float>* %Nf;
    %V = alloca <4 x float>;
    %tmp7 = call <4 x float> @rsl_getI();
    %tmp8.buf = alloca <4 x float>;
    call void @normalize_vv(<4 x float>* %tmp8.buf, <4 x float> %tmp7);
    %tmp8 = load <4 x float>* %tmp8.buf;
    %tmp9 = sub <4 x float> zeroinitializer , %tmp8;
    store <4 x float> %tmp9 , <4 x float>* %V;
    %tmp10 = load float* %Kd;
    %tmp11.buf = alloca float;
    call void @noise_ff(float* %tmp11.buf, float %tmp10);
    %tmp11 = load float* %tmp11.buf;
    %tmp13.tmp0 = insertelement <4 x float> undef, float %tmp11, i32 0;
    %tmp13.tmp1 = insertelement <4 x float> %tmp13.tmp0, float %tmp11, i32 1;
    %tmp13.tmp2 = insertelement <4 x float> %tmp13.tmp1, float %tmp11, i32 2;
    %tmp13 = insertelement <4 x float> %tmp13.tmp2, float %tmp11, i32 3;
    call void @rsl_setCi( <4 x float> %tmp13 );

    ret void;

}

define void @matte_cache_gen_pass() {
    %roughness = alloca float;
    %tmp1.tmp = alloca float;
    store float 0x3f847ae140000000 , float* %tmp1.tmp;
    %tmp1 = load float* %tmp1.tmp;
    store float %tmp1 , float* %roughness;
    %Kd = alloca float;
    %tmp2.tmp = alloca float;
    store float 0x3fe8000000000000 , float* %tmp2.tmp;
    %tmp2 = load float* %tmp2.tmp;
    store float %tmp2 , float* %Kd;
    %Nf = alloca <4 x float>;
    %tmp3 = call <4 x float> @rsl_getN();
    %tmp4.buf = alloca <4 x float>;
    call void @normalize_vn(<4 x float>* %tmp4.buf, <4 x float> %tmp3);
    %tmp4 = load <4 x float>* %tmp4.buf;
    %tmp5 = call <4 x float> @rsl_getI();
    %tmp6.buf = alloca <4 x float>;
    call void @faceforward_vvv(<4 x float>* %tmp6.buf, <4 x float> %tmp4, <4 x float> %tmp5);
    %tmp6 = load <4 x float>* %tmp6.buf;
    store <4 x float> %tmp6 , <4 x float>* %Nf;
    %V = alloca <4 x float>;
    %tmp7 = call <4 x float> @rsl_getI();
    %tmp8.buf = alloca <4 x float>;
    call void @normalize_vv(<4 x float>* %tmp8.buf, <4 x float> %tmp7);
    %tmp8 = load <4 x float>* %tmp8.buf;
    %tmp9 = sub <4 x float> zeroinitializer , %tmp8;
    store <4 x float> %tmp9 , <4 x float>* %V;
    %tmp10 = load float* %Kd;
    %tmp11.buf = alloca float;
    call void @noise_ff(float* %tmp11.buf, float %tmp10);
    %tmp11 = load float* %tmp11.buf;
    %tmp13.tmp0 = insertelement <4 x float> undef, float %tmp11, i32 0;
    %tmp13.tmp1 = insertelement <4 x float> %tmp13.tmp0, float %tmp11, i32 1;
    %tmp13.tmp2 = insertelement <4 x float> %tmp13.tmp1, float %tmp11, i32 2;
    %tmp13 = insertelement <4 x float> %tmp13.tmp2, float %tmp11, i32 3;
    call void @rsl_setCi( <4 x float> %tmp13 );

    ret void;

}

define void @matte_dynamic_pass() {
    %roughness = alloca float;
    %tmp1.tmp = alloca float;
    store float 0x3f847ae140000000 , float* %tmp1.tmp;
    %tmp1 = load float* %tmp1.tmp;
    store float %tmp1 , float* %roughness;
    %Kd = alloca float;
    %tmp2.tmp = alloca float;
    store float 0x3fe8000000000000 , float* %tmp2.tmp;
    %tmp2 = load float* %tmp2.tmp;
    store float %tmp2 , float* %Kd;
    %Nf = alloca <4 x float>;
    %tmp3 = call <4 x float> @rsl_getN();
    %tmp4.buf = alloca <4 x float>;
    call void @normalize_vn(<4 x float>* %tmp4.buf, <4 x float> %tmp3);
    %tmp4 = load <4 x float>* %tmp4.buf;
    %tmp5 = call <4 x float> @rsl_getI();
    %tmp6.buf = alloca <4 x float>;
    call void @faceforward_vvv(<4 x float>* %tmp6.buf, <4 x float> %tmp4, <4 x float> %tmp5);
    %tmp6 = load <4 x float>* %tmp6.buf;
    store <4 x float> %tmp6 , <4 x float>* %Nf;
    %V = alloca <4 x float>;
    %tmp7 = call <4 x float> @rsl_getI();
    %tmp8.buf = alloca <4 x float>;
    call void @normalize_vv(<4 x float>* %tmp8.buf, <4 x float> %tmp7);
    %tmp8 = load <4 x float>* %tmp8.buf;
    %tmp9 = sub <4 x float> zeroinitializer , %tmp8;
    store <4 x float> %tmp9 , <4 x float>* %V;
    %tmp10 = load float* %Kd;
    %tmp11.buf = alloca float;
    call void @noise_ff(float* %tmp11.buf, float %tmp10);
    %tmp11 = load float* %tmp11.buf;
    %tmp13.tmp0 = insertelement <4 x float> undef, float %tmp11, i32 0;
    %tmp13.tmp1 = insertelement <4 x float> %tmp13.tmp0, float %tmp11, i32 1;
    %tmp13.tmp2 = insertelement <4 x float> %tmp13.tmp1, float %tmp11, i32 2;
    %tmp13 = insertelement <4 x float> %tmp13.tmp2, float %tmp11, i32 3;
    call void @rsl_setCi( <4 x float> %tmp13 );

    ret void;

}
