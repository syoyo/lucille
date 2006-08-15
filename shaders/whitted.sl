surface
whitted(
	float eta = 1.5;	/* index of refraction */
	float Kd = .8;		/* diffuse coefficient */
	float Kr = .8;		/* reflective coefficient */
	float Kt = .2;		/* transmissive coefficient */
	float Ks = .2;		/* specular coefficient */
	float Kss = 2)		/* specular exponent */
{
	normal Nn = faceforward(normalize(N), I);

	/* ambient term */
	Ci = Kd * ambient();

	illuminance(P, Nn, PI/2) {
		/* diffuse */
		Ci += Kd * Cl * (L. Nn);
	}

	/* reflection */
	Ci += Ks * trace(P, reflect(I, Nn));

	vector T = refract(I, Nn, (N.I) < 0 ? eta : 1/eta);
	
	if (length(T) != 0.0)
		Ci += Kt * trace(P, T); 
}
