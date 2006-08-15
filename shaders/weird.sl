surface
weird( float a=0.5; varying float b=0.25)
{
	Ci = color(mod(s,a), abs(sin(a+b)), mod(b,t));
} 
