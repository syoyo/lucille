surface
matte()
{
	normal Nf = faceforward(normalize(N), I);

	//color a = ambient();
	//color d = diffuse(N);
	//Ci = N + a + d;
	//color c = Cs;
	//Ci = Cs + Nf;
	normal Nf = faceforward(normalize(N));
	Ci = Nf;
}
