/*
 * Perlin noise.
 * Codes from 
 * http://www.mrl.nyu.edu/~perlin/doc/oscar.html
 *
 * $Id: noise.h,v 1.2 2004/01/24 17:11:35 syoyo Exp $
 */

#ifndef NOISE_H
#define NOISE_H

#ifdef __cplusplus
extern "C" {
#endif

//extern double	noise1(double arg);
float           noise1(float arg);
extern float	noise2(float u, float v);
extern float	noise3(float u, float v, float w);

#ifdef __cplusplus
}	/* extern "C" */
#endif

#endif

