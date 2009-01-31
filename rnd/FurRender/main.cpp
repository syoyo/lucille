#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <FL/Fl.H>
#include <FL/Fl_Window.H>
#include <FL/Fl_Box.H>
#include <FL/Fl_Image.H>

#include "curve.h"

#define WINDOW_WIDTH  256
#define WINDOW_HEIGHT 256
#define NSUBSAMPLES   1

unsigned char *image;

unsigned char clamp(float f)
{
    int i = f * 255.5f;
    if (i < 0) i = 0;
    if (i > 255) i = 255;

    return (unsigned char)i;
}

void
render()
{
    ri_bezier_curve_t curve;
    ri_bezier_curve_t curve_rayspace;

    curve.P[0].x = -(float)WINDOW_WIDTH/2 + 30;
    curve.P[0].y = -(float)WINDOW_HEIGHT/2 + 30;
    curve.P[0].z =  1.0f;

    curve.P[1].x = -(float)WINDOW_WIDTH/2 + 60;
    curve.P[1].y =  (float)WINDOW_HEIGHT/2 - 30;
    curve.P[1].z =  1.0f;

    curve.P[2].x =  (float)WINDOW_WIDTH/2 - 60;
    curve.P[2].y = -(float)WINDOW_HEIGHT/2 + 30;
    curve.P[2].z =  1.0f;

    curve.P[3].x =  (float)WINDOW_WIDTH/2 - 30;
    curve.P[3].y =  (float)WINDOW_HEIGHT/2 - 30;
    curve.P[3].z =  1.0f;
    curve.ncontrol_points = 4;

    int depth;
    depth = ri_bezier_curve_calculate_max_recursion_depth(curve, 0.5f / 20.0f);
    depth += 3;
    printf("depth = %d\n", depth);

    curve.bbox.bmin.x = -(float)WINDOW_WIDTH/2;
    curve.bbox.bmin.y = -(float)WINDOW_HEIGHT/2;
    curve.bbox.bmin.z = 0.0f;
    curve.bbox.bmax.x =  (float)WINDOW_WIDTH/2;
    curve.bbox.bmax.y =  (float)WINDOW_HEIGHT/2;
    curve.bbox.bmax.z = 1.0f;

    int     x, y;
    int     u, v;
    int     i;
    float   t;

    point_t rayorg;
    ri_curve_intersect_t isect;

    float   color[3];

    ri_curve_clear_stat();

    for (y = 0; y < WINDOW_HEIGHT; y++) {
        for (x = 0; x < WINDOW_WIDTH; x++) {

            color[0] = 0.0f;
            color[1] = 0.0f;
            color[2] = 0.0f;

            for (v = 0; v < NSUBSAMPLES; v++) {
                for (u = 0; u < NSUBSAMPLES; u++) {

                    rayorg.x = (u / (float)NSUBSAMPLES) + x - (WINDOW_WIDTH / 2);           
                    rayorg.y = (v / (float)NSUBSAMPLES) + y - (WINDOW_HEIGHT / 2);           
                    rayorg.z = 0.0f;           

                    /* Project the curve int ray space. */
                    for (i = 0; i < 4; i++) {
                        curve_rayspace.P[i].x = curve.P[i].x - rayorg.x;
                        curve_rayspace.P[i].y = curve.P[i].y - rayorg.y;
                        curve_rayspace.P[i].z = 1.0f;
                    }
                    curve_rayspace.bbox = curve.bbox;
                    curve_rayspace.bbox.bmin.x -= rayorg.x;
                    curve_rayspace.bbox.bmin.y -= rayorg.y;
                    curve_rayspace.bbox.bmin.z  = 0.0f;
                    curve_rayspace.bbox.bmax.x -= rayorg.x;
                    curve_rayspace.bbox.bmax.y -= rayorg.y;
                    curve_rayspace.bbox.bmax.z  = 1.0f;
                    curve_rayspace.ncontrol_points = 4;

                    ri_bezier_curve_compute_bbox(&curve_rayspace);

                    if (ri_bezier_curve_intersect(&isect,
                                                  curve_rayspace,
                                                  depth)) {
                        
                        color[0] += isect.u;
                        color[1] += isect.v;

                    }           

                }
            }

            color[0] /= (float)(NSUBSAMPLES * NSUBSAMPLES);
            color[1] /= (float)(NSUBSAMPLES * NSUBSAMPLES);
            color[2] /= (float)(NSUBSAMPLES * NSUBSAMPLES);
            image[3*(y*WINDOW_WIDTH+x)+0] = clamp(color[0]);
            image[3*(y*WINDOW_WIDTH+x)+1] = clamp(color[1]);
            image[3*(y*WINDOW_WIDTH+x)+2] = clamp(color[2]);

        }
    }

    ri_curve_show_stat();
    
}

void
render_curve()
{
    int i;

    ri_bezier_curve_t curve;
    ri_bezier_curve_t c0, c1;

    curve.P[0].x = -(float)WINDOW_WIDTH/2;
    curve.P[0].y = -(float)WINDOW_HEIGHT/2;
    curve.P[0].z =  1.0f;

    curve.P[1].x = -(float)WINDOW_WIDTH/2;
    curve.P[1].y =  (float)WINDOW_HEIGHT/2;
    curve.P[1].z =  1.0f;

    curve.P[2].x =  (float)WINDOW_WIDTH/2;
    curve.P[2].y = -(float)WINDOW_HEIGHT/2;
    curve.P[2].z =  1.0f;

    curve.P[3].x =  (float)WINDOW_WIDTH/2;
    curve.P[3].y =  (float)WINDOW_HEIGHT/2;
    curve.P[3].z =  1.0f;
    curve.ncontrol_points = 4;

    int depth;
    depth = ri_bezier_curve_calculate_max_recursion_depth(curve, 1.0f / 20.0f);
    printf("depth = %d\n", depth);

    ri_bezier_curve_split(&c0, &c1, curve);

    float t;
    point_t p;
    
    int   x, y;

    for (i = 0; i < 1000; i++) {
        t = (float)i / 1000.0f;

        ri_bezier_curve_eval3(&p, curve, t);

        x = (int)p.x + WINDOW_WIDTH/2;
        y = (int)p.y + WINDOW_WIDTH/2;
        
        image[3*(y*WINDOW_WIDTH+x)+0] = 255;
        image[3*(y*WINDOW_WIDTH+x)+1] = 255;
        image[3*(y*WINDOW_WIDTH+x)+2] = 255;
    }

}

int
main(int argc, char **argv)
{
    image = (unsigned char *)malloc(WINDOW_WIDTH * WINDOW_HEIGHT * 3);
    memset(image, 0, WINDOW_HEIGHT * WINDOW_WIDTH * 3);

    render();

    Fl_Window *mainWindow = new Fl_Window(100, 100, 400, 400, "curve render viewer");
    mainWindow->begin();

    Fl_Box  *imagePanel = new Fl_Box(10, 10, WINDOW_WIDTH, WINDOW_HEIGHT);
    imagePanel->image(new Fl_RGB_Image(image, WINDOW_WIDTH, WINDOW_HEIGHT, 3));

    mainWindow->end();
    mainWindow->show();

    return Fl::run();
}


