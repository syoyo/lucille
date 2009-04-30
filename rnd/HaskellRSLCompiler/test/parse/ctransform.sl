surface
muda()
{
	color col;
	color a = ctransform("tospace", col);
	color b = ctransform("fromspace", "tospace", col);
}

