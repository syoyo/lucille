surface
matte()
{
	float roughness = 0.01;
	float Kd = 0.75;

	normal Nf = faceforward(normalize(N), I);
	vector V = -normalize(I);

	/*
	//color sum;
	//float scale = 1.0;

	//point M = 10.0 * P;

	//sum = scale * color noise(M / scale); 
	//scale = 0.5;
	//sum = sum + scale * color noise(M / scale); 
	//scale = 0.25;
	//sum = sum + scale * color noise(M / scale); 
	//scale = 0.125;
	//sum = sum + scale * color noise(M / scale); 
	
	//Ci = turb(10.0*P) * texture("muda") * (diffuse(Nf) + ambient());
	Ci = (0.25 + 0.75 * turb(10.0*P)) * texture("muda") * (Kd * diffuse(Nf) + ambient() + specular(Nf, V, roughness));
	//Ci = (Kd * diffuse(Nf) + ambient());
	*/
	//Ci = turb(10.0*P) * texture("muda") * (Kd * diffuse(Nf) + ambient() + specular(Nf, V, roughness));
	//Ci = P * texture("muda") * (Kd * diffuse(Nf) + ambient() + specular(Nf, V, roughness));
	//Ci = turb(P);
	Ci = noise(Kd);

}
