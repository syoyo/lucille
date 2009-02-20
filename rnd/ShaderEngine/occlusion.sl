surface
occlusion()
{
	// hack
	float gain = xcomp(L);
	if (gain < 0.1) gain = 0.1;

	Ci = gain * occlusion(P, N);
}
