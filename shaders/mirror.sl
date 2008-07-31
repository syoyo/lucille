surface
mirror(float Ks = 0.5)
{
	color ref;
	normal Nf = faceforward(normalize(N), I);
	vector R = reflect(I, Nf);
	
	ref = trace(P, R);

	Ci = Ks * ref; 
	Oi = Os;
}
