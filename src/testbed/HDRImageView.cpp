#include <stdio.h>
#include <stdlib.h>

#include <Fl/Fl.H>
#include <Fl/Fl_Image.H>

#include "HDRImageView.h"

void
HDRImageView::draw()
{

}

static inline float
fclamp( float f, float fmin, float fmax )
{
    if (f < fmin) f = fmin;
    if (f > fmax) f = fmax;

    return f;      
}

static inline unsigned char
clamp( float f )
{
    int i;

    i = f * 255.5f;

    if (i < 0  ) i = 0;
    if (i > 255) i = 255;

    return (unsigned char)i;

}

static void
tonemap( unsigned char *image, float *fimage, int width, int height, float minval, float maxval )
{
    int i;
    float fval;
    float invscale;
    
    invscale = 1.0f / (maxval - minval);
    for (i = 0; i < width * height * 3; i++) {
        fval = (fimage[i] - minval) * invscale;
        image[i] = clamp(fclamp(fval, 0.0f, 1.0f));
    }

}

static void
find_minmaxval( float *minval, float *maxval, float *fimage, int width, int height )
{
    int i;
    float fmax = fimage[0];
    float fmin = fimage[0];
    
    for (i = 1; i < width * height * 3; i++) {
        if (fmax < fimage[i]) fmax = fimage[i];
        if (fmin > fimage[i]) fmin = fimage[i];
    }

    printf("maxval = %f\n", fmax);
    printf("minval = %f\n", fmin);

    (*minval) = fmin;
    (*maxval) = fmax;
}
