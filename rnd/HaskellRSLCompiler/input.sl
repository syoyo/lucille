surface
matte(vector Ka; float Kd = 1;)
{
	//float m = mod(Ka * Kd, 1), k;
	
	//color d = diffuse(N);
	//color a = ambient();
	float f = noise(P);

	color tcol = texture("muda");
	//color c = color noise(P) + d;

	//normal n = ;

	//I = Ka;
	//normal Nf = faceforward(normalize(N), I);
	//Oi = (-Os) - Os;
	//Ci = Os * Cs * (Ka * ambient() + Kd * diffuse(Nf));

	//color c = color(0, 0, 0);

	//normal n = Nf . I;

	//float k = float "bora" mod(Ka);

	//float a = 1.0;

	//if (1) {
	//	k = 2;
	//} else {
	//	k = 3;
	//}
	
}
