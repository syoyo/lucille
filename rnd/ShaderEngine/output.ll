declare <4xfloat> @getCi()
declare <4xfloat> @getOi()
declare <4xfloat> @getCs()
declare <4xfloat> @getOs()
declare <4xfloat> @getP()
declare <4xfloat> @getI()
declare <4xfloat> @getE()
declare <4xfloat> @getN()
declare <4xfloat> @getNg()
declare <4xfloat> @getCl()
declare <4xfloat> @getOl()
declare float @gets()
declare float @gett()

define void @matte() {
    %Nf = alloca <4xfloat>;
    %tmp1 = call <4xfloat> @getN();
    %tmp2 = call @normalize();
    store <4xfloat> %tmp2 , <4xfloat>* %Nf;

    ret void;

}
