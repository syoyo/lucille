surface
muda()
{
	normal n;
	matrix m;
	normal a = ntransform("tospace", n);
	normal b = ntransform("fromspace", "tospace", n);
	normal c = ntransform(m, n);
	normal d = ntransform("fromspace", m, n);
}

