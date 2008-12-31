; ModuleID = 'shaderlib.c'
target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:128:128"
target triple = "i686-pc-linux-gnu"
@.str = internal constant [10 x i8] c"ambient\0A\00\00"		; <[10 x i8]*> [#uses=1]
@.str1 = internal constant [26 x i8] c"diffuse. N = %f, %f, %f\0A\00\00"		; <[26 x i8]*> [#uses=1]
@.str2 = internal constant [22 x i8] c"diffuse. NdotL = %f\0A\00\00"		; <[22 x i8]*> [#uses=1]

define void @ambient_c(<4 x float> %ret) {
entry:
	%ret.addr = alloca <4 x float>		; <<4 x float>*> [#uses=9]
	store <4 x float> %ret, <4 x float>* %ret.addr
	%call = call i32 (i8*, ...)* @printf( i8* getelementptr ([10 x i8]* @.str, i32 0, i32 0) )		; <i32> [#uses=0]
	%tmp = load <4 x float>* %ret.addr		; <<4 x float>> [#uses=1]
	%vecins = insertelement <4 x float> %tmp, float 0x3FB99999A0000000, i32 0		; <<4 x float>> [#uses=1]
	store <4 x float> %vecins, <4 x float>* %ret.addr
	%tmp1 = load <4 x float>* %ret.addr		; <<4 x float>> [#uses=1]
	%vecins2 = insertelement <4 x float> %tmp1, float 0x3FB99999A0000000, i32 1		; <<4 x float>> [#uses=1]
	store <4 x float> %vecins2, <4 x float>* %ret.addr
	%tmp3 = load <4 x float>* %ret.addr		; <<4 x float>> [#uses=1]
	%vecins4 = insertelement <4 x float> %tmp3, float 0x3FB99999A0000000, i32 2		; <<4 x float>> [#uses=1]
	store <4 x float> %vecins4, <4 x float>* %ret.addr
	%tmp5 = load <4 x float>* %ret.addr		; <<4 x float>> [#uses=1]
	%vecins6 = insertelement <4 x float> %tmp5, float 0x3FB99999A0000000, i32 3		; <<4 x float>> [#uses=1]
	store <4 x float> %vecins6, <4 x float>* %ret.addr
	call void (...)* @bora( )
	ret void
}

declare i32 @printf(i8*, ...)

declare void @bora(...)

define void @diffuse_cn(<4 x float>* %ret, <4 x float> %N) {
entry:
	%ret.addr = alloca <4 x float>*		; <<4 x float>**> [#uses=2]
	%N.addr = alloca <4 x float>		; <<4 x float>*> [#uses=7]
	%L = alloca <4 x float>, align 16		; <<4 x float>*> [#uses=10]
	%NdotL = alloca float, align 4		; <float*> [#uses=6]
	%rr = alloca <4 x float>, align 16		; <<4 x float>*> [#uses=9]
	store <4 x float>* %ret, <4 x float>** %ret.addr
	store <4 x float> %N, <4 x float>* %N.addr
	%tmp = load <4 x float>* %L		; <<4 x float>> [#uses=1]
	%vecins = insertelement <4 x float> %tmp, float 1.000000e+00, i32 0		; <<4 x float>> [#uses=1]
	store <4 x float> %vecins, <4 x float>* %L
	%tmp1 = load <4 x float>* %L		; <<4 x float>> [#uses=1]
	%vecins2 = insertelement <4 x float> %tmp1, float 1.000000e+00, i32 1		; <<4 x float>> [#uses=1]
	store <4 x float> %vecins2, <4 x float>* %L
	%tmp3 = load <4 x float>* %L		; <<4 x float>> [#uses=1]
	%vecins4 = insertelement <4 x float> %tmp3, float 1.000000e+00, i32 2		; <<4 x float>> [#uses=1]
	store <4 x float> %vecins4, <4 x float>* %L
	%tmp5 = load <4 x float>* %L		; <<4 x float>> [#uses=1]
	call void @vnormalize( <4 x float> %tmp5 )
	%tmp6 = load <4 x float>* %N.addr		; <<4 x float>> [#uses=1]
	%vecext = extractelement <4 x float> %tmp6, i32 0		; <float> [#uses=1]
	%tmp7 = load <4 x float>* %L		; <<4 x float>> [#uses=1]
	%vecext8 = extractelement <4 x float> %tmp7, i32 0		; <float> [#uses=1]
	%mul = mul float %vecext, %vecext8		; <float> [#uses=1]
	%tmp9 = load <4 x float>* %N.addr		; <<4 x float>> [#uses=1]
	%vecext10 = extractelement <4 x float> %tmp9, i32 1		; <float> [#uses=1]
	%tmp11 = load <4 x float>* %L		; <<4 x float>> [#uses=1]
	%vecext12 = extractelement <4 x float> %tmp11, i32 1		; <float> [#uses=1]
	%mul13 = mul float %vecext10, %vecext12		; <float> [#uses=1]
	%add = add float %mul, %mul13		; <float> [#uses=1]
	%tmp14 = load <4 x float>* %N.addr		; <<4 x float>> [#uses=1]
	%vecext15 = extractelement <4 x float> %tmp14, i32 2		; <float> [#uses=1]
	%tmp16 = load <4 x float>* %L		; <<4 x float>> [#uses=1]
	%vecext17 = extractelement <4 x float> %tmp16, i32 2		; <float> [#uses=1]
	%mul18 = mul float %vecext15, %vecext17		; <float> [#uses=1]
	%add19 = add float %add, %mul18		; <float> [#uses=1]
	store float %add19, float* %NdotL
	%tmp20 = load <4 x float>* %N.addr		; <<4 x float>> [#uses=1]
	%vecext21 = extractelement <4 x float> %tmp20, i32 0		; <float> [#uses=1]
	%conv = fpext float %vecext21 to double		; <double> [#uses=1]
	%tmp22 = load <4 x float>* %N.addr		; <<4 x float>> [#uses=1]
	%vecext23 = extractelement <4 x float> %tmp22, i32 1		; <float> [#uses=1]
	%conv24 = fpext float %vecext23 to double		; <double> [#uses=1]
	%tmp25 = load <4 x float>* %N.addr		; <<4 x float>> [#uses=1]
	%vecext26 = extractelement <4 x float> %tmp25, i32 2		; <float> [#uses=1]
	%conv27 = fpext float %vecext26 to double		; <double> [#uses=1]
	%call = call i32 (i8*, ...)* @printf( i8* getelementptr ([26 x i8]* @.str1, i32 0, i32 0), double %conv, double %conv24, double %conv27 )		; <i32> [#uses=0]
	%tmp28 = load float* %NdotL		; <float> [#uses=1]
	%conv29 = fpext float %tmp28 to double		; <double> [#uses=1]
	%call30 = call i32 (i8*, ...)* @printf( i8* getelementptr ([22 x i8]* @.str2, i32 0, i32 0), double %conv29 )		; <i32> [#uses=0]
	%tmp31 = load float* %NdotL		; <float> [#uses=1]
	%tmp32 = load <4 x float>* %rr		; <<4 x float>> [#uses=1]
	%vecins33 = insertelement <4 x float> %tmp32, float %tmp31, i32 0		; <<4 x float>> [#uses=1]
	store <4 x float> %vecins33, <4 x float>* %rr
	%tmp34 = load float* %NdotL		; <float> [#uses=1]
	%tmp35 = load <4 x float>* %rr		; <<4 x float>> [#uses=1]
	%vecins36 = insertelement <4 x float> %tmp35, float %tmp34, i32 1		; <<4 x float>> [#uses=1]
	store <4 x float> %vecins36, <4 x float>* %rr
	%tmp37 = load float* %NdotL		; <float> [#uses=1]
	%tmp38 = load <4 x float>* %rr		; <<4 x float>> [#uses=1]
	%vecins39 = insertelement <4 x float> %tmp38, float %tmp37, i32 2		; <<4 x float>> [#uses=1]
	store <4 x float> %vecins39, <4 x float>* %rr
	%tmp40 = load float* %NdotL		; <float> [#uses=1]
	%tmp41 = load <4 x float>* %rr		; <<4 x float>> [#uses=1]
	%vecins42 = insertelement <4 x float> %tmp41, float %tmp40, i32 3		; <<4 x float>> [#uses=1]
	store <4 x float> %vecins42, <4 x float>* %rr
	%tmp43 = load <4 x float>** %ret.addr		; <<4 x float>*> [#uses=1]
	%tmp44 = load <4 x float>* %rr		; <<4 x float>> [#uses=1]
	store <4 x float> %tmp44, <4 x float>* %tmp43
	ret void
}

define internal void @vnormalize(<4 x float> %v) {
entry:
	%v.addr = alloca <4 x float>		; <<4 x float>*> [#uses=16]
	%len = alloca float, align 4		; <float*> [#uses=3]
	%invlen = alloca float, align 4		; <float*> [#uses=4]
	store <4 x float> %v, <4 x float>* %v.addr
	%tmp = load <4 x float>* %v.addr		; <<4 x float>> [#uses=1]
	%vecext = extractelement <4 x float> %tmp, i32 0		; <float> [#uses=1]
	%tmp1 = load <4 x float>* %v.addr		; <<4 x float>> [#uses=1]
	%vecext2 = extractelement <4 x float> %tmp1, i32 0		; <float> [#uses=1]
	%mul = mul float %vecext, %vecext2		; <float> [#uses=1]
	%tmp3 = load <4 x float>* %v.addr		; <<4 x float>> [#uses=1]
	%vecext4 = extractelement <4 x float> %tmp3, i32 1		; <float> [#uses=1]
	%tmp5 = load <4 x float>* %v.addr		; <<4 x float>> [#uses=1]
	%vecext6 = extractelement <4 x float> %tmp5, i32 1		; <float> [#uses=1]
	%mul7 = mul float %vecext4, %vecext6		; <float> [#uses=1]
	%add = add float %mul, %mul7		; <float> [#uses=1]
	%tmp8 = load <4 x float>* %v.addr		; <<4 x float>> [#uses=1]
	%vecext9 = extractelement <4 x float> %tmp8, i32 2		; <float> [#uses=1]
	%tmp10 = load <4 x float>* %v.addr		; <<4 x float>> [#uses=1]
	%vecext11 = extractelement <4 x float> %tmp10, i32 2		; <float> [#uses=1]
	%mul12 = mul float %vecext9, %vecext11		; <float> [#uses=1]
	%add13 = add float %add, %mul12		; <float> [#uses=1]
	store float %add13, float* %len
	%tmp14 = load float* %len		; <float> [#uses=1]
	%cmp = fcmp ogt float %tmp14, 0x3EB0C6F7A0000000		; <i1> [#uses=1]
	br i1 %cmp, label %ifthen, label %ifend

ifthen:		; preds = %entry
	%tmp15 = load float* %len		; <float> [#uses=1]
	%div = fdiv float 1.000000e+00, %tmp15		; <float> [#uses=1]
	store float %div, float* %invlen
	%tmp16 = load <4 x float>* %v.addr		; <<4 x float>> [#uses=1]
	%vecext17 = extractelement <4 x float> %tmp16, i32 0		; <float> [#uses=1]
	%tmp18 = load float* %invlen		; <float> [#uses=1]
	%mul19 = mul float %vecext17, %tmp18		; <float> [#uses=1]
	%tmp20 = load <4 x float>* %v.addr		; <<4 x float>> [#uses=1]
	%vecins = insertelement <4 x float> %tmp20, float %mul19, i32 0		; <<4 x float>> [#uses=1]
	store <4 x float> %vecins, <4 x float>* %v.addr
	%tmp21 = load <4 x float>* %v.addr		; <<4 x float>> [#uses=1]
	%vecext22 = extractelement <4 x float> %tmp21, i32 1		; <float> [#uses=1]
	%tmp23 = load float* %invlen		; <float> [#uses=1]
	%mul24 = mul float %vecext22, %tmp23		; <float> [#uses=1]
	%tmp25 = load <4 x float>* %v.addr		; <<4 x float>> [#uses=1]
	%vecins26 = insertelement <4 x float> %tmp25, float %mul24, i32 1		; <<4 x float>> [#uses=1]
	store <4 x float> %vecins26, <4 x float>* %v.addr
	%tmp27 = load <4 x float>* %v.addr		; <<4 x float>> [#uses=1]
	%vecext28 = extractelement <4 x float> %tmp27, i32 2		; <float> [#uses=1]
	%tmp29 = load float* %invlen		; <float> [#uses=1]
	%mul30 = mul float %vecext28, %tmp29		; <float> [#uses=1]
	%tmp31 = load <4 x float>* %v.addr		; <<4 x float>> [#uses=1]
	%vecins32 = insertelement <4 x float> %tmp31, float %mul30, i32 2		; <<4 x float>> [#uses=1]
	store <4 x float> %vecins32, <4 x float>* %v.addr
	br label %ifend

ifend:		; preds = %ifthen, %entry
	ret void
}
