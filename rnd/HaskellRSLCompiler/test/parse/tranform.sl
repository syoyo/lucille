surface
muda()
{
	point p;
	matrix m;
	point a = transform("tospace", p);
	point b = transform("fromspace", "tospace", p);
	point b = transform(m, p);
	point b = transform("fromspace", m, p);
}

