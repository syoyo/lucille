surface
wood(float ringscale = 10;
     color lightwood = color(0.3, 0.12, 0.03),
           darkwood  = color(0.05, 0.01, 0.005);
     float Ka = 0.2,
           Kd = 0.4,
           Ks = 0.6,
           roughness = 0.1)
{
	point NN, V;
	point PP;
	float y, z, r;

	NN = faceforward(normalize(N), I);
	V = -normalize(I);

	PP = transform("shader", P);
	PP += noise(PP);

	y = ycomp(PP);
	z = zcomp(PP);
	r = sqrt(y * y + z * z);

	r *= ringscale;
	r += abs(noise(r));
	r -= floor(r);

	r = smoothstep(0, 0.8, r) - smoothstep(0.83, 1.0, r);
	Ci = mix(lightwood, darkwood, r);

	Oi = Os;
	Ci = Oi * Ci * (Ka * ambient() + Kd * diffuse(NN))
           + (0.3 * r + 0.7) * Ks * specular(NN, V, roughness);
}
