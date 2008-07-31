surface
granite(float Kd = 0.8,
              Ka = 0.2)
{
	float sum = 0;
	float i, freq = 1.0;
	point V = normalize(I);

	for (i = 0; i < 6; i = i + 1) {
		sum = sum + abs(0.5 - noise(4 * freq * I))/freq;
		freq *= 2;
	}

	Ci = Cs * sum * (Ka + Kd * diffuse(faceforward(normalize(N), V)));
}
