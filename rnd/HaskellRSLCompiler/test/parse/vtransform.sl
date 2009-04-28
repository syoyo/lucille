surface
muda()
{
	vector v;
	matrix m;
	vector a = vtransform("tospace", v);
	vector b = vtransform("fromspace", "tospace", v);
	vector c = vtransform(m, v);
	vector d = vtransform("fromspace", m, v);
}

