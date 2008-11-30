/* Beta-0.3.0: MOSAIC default image shader for Blender integration */

#include "MOSAICfunctions.h"

imager
MOSAICbackground(
	color bgcolor = color(1, 1, 1))
{
	Ci += (1 - alpha) * bgcolor;
	Oi = 1;
	alpha = 1;
}

