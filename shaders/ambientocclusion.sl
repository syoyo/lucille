surface
ambientocclusion(float samples = 32)
{
	float occ = occlusion(P, N, samples);

	Ci = (1 - occ) * Cs * Os;
	Oi = Os;
}
