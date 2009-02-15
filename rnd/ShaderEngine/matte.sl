surface
matte()
{
	float roughness = 0.01;
	float Kd = 0.75;

	normal Nf = faceforward(normalize(N), I);
	vector V = -normalize(I);

	Ci = turb(10.0*P) * texture("muda") * (Kd * diffuse(Nf) + ambient() + specular(Nf, V, roughness));

}
