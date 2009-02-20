surface
mandel()
{
	float window_size = 256.0;

	float xx, yy;
	float x0, y0;

	float max_iter = 255;
	float iter = 0;
	float tmp;

	float zoom = xcomp(L);

	if (zoom < 0.05) {
		zoom = 0.05;
	}

	xx = zoom * x / window_size;
	x0 = zoom * x / window_size;

	yy = zoom * y / window_size;
	y0 = zoom * y / window_size;
 
	while ( ((xx * xx + yy * yy) <= (2.0*2.0)) && (iter < max_iter)) {
		tmp = xx * xx - yy * yy + x0;
		yy = 2.0 * xx * yy + y0;
		xx = tmp;

		iter = iter + 1;
	}

	if (iter == max_iter) {
		Ci = color(0, 0, 0);
	} else {
		Ci = color(iter / 255.0, iter / 255.0, iter / 127.0);	// make result bluish a bit.
	}

}
