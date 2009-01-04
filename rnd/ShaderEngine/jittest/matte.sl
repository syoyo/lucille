surface
matte()
{
	float roughness = 0.01;
	float Kd = 0.75;
	color cKd = color Kd;

	normal Nf = faceforward(normalize(N), I);
	//Ci = texture("muda") * (diffuse(Nf) + ambient());
	vector V = -normalize(I);
	Ci = texture("muda") * (cKd * diffuse(Nf) + ambient() + specular(Nf, V, roughness));
}
